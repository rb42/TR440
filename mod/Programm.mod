
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Programm;

(*	20.06.18	*)

FROM SYSTEM IMPORT
	CAST, ADR ;

FROM WIN32 IMPORT
	GetTickCount ;

IMPORT Terminal, Conversions, Strings, ExStrings, AsmTrace,
	Assembler, FileIO, FileFunc, TerminalEingabe ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM Befehle IMPORT * ;
FROM upOKB IMPORT * ;

FROM DateiVerwaltung IMPORT * ;

FROM DateiBearbeitung IMPORT
	AlleEigenenDateienSchliessen, DateiOeffnen, SetAktSatzIndexGW, SatzLesen, DateiSchliessen ;

FROM Namen IMPORT
	pName,
	AktQuelle,
	AktWoerterbuch,
	quelle0,
	QuellZeile0,
	woerterbuch0,
	AktWB,
	AktSegmWB,
	CzoneWB ;

%IF TTY %THEN
	IMPORT TTY ;
%END

%IF WEB %THEN

IMPORT CGIup ;

%ELSE
FROM Namen IMPORT
	ZeilenText,
	AlleNamenLoeschen ;
FROM Assembler IMPORT
	GetZahl ;
%END

FROM Gebiet IMPORT * ;


FROM Assembler IMPORT
	AblageAdresse, lastDateiname, StartAdresse, PseudoGebiete, AktQuellName ;

IMPORT debug ;

FROM debug IMPORT
	aend48 ;

FROM Menue IMPORT
%IF %NOT WEB %THEN
	SetGrundzustand,
	MenueBefehl,
%END
	MenueBefehlStehtAn ;

FROM Trace IMPORT
	TraceF ;

FROM TR440hp IMPORT
	upShowMemory, cmdAsmVorrang, COManschluss, TTYanschluss, cmdShowInfo,
	cmdEigenerAssembler ;

IMPORT AsmDisasm, AbwicklerUp ;
FROM AsmDisasm IMPORT
	CardToHex1, CardToHex2, CardToHex4, CardToHex6, CardToHex12, tOpcode ;
FROM AbwicklerUp IMPORT
	AktSatz, tSatz, XABgefunden ;


CONST
	TAB = 			CHR (9) ;


VAR
	Programm0 :		pProgramm = NIL ;

	Anzahl :		CARDINAL ;
	StartZeit,
	EndZeit :		CARDINAL ;
	befehl,
	PosTrans :		CARDINAL ;

	merkTakte :		CARDINAL64 ;

	SeitenPuffer :		ARRAY [0..MaxSeitenIndex] OF CARDINAL8 ;

	vRegA,					(* Akkumulator			*)
	vRegQ,					(* Quotientenregister		*)
	vRegD,					(* Multiplikandenregister	*)
	vRegH :			reg48 ;		(* Hilfsregister		*)

	vRegB :			CARDINAL ;	(* Bereitadressregister		24 Bits		*)
	vRegX :			CARDINAL ;	(* Indexbasisregister		24 Bits		*)

	vRegK,
	vRegY,
	vRegU :			CARDINAL8 ;

	vRegM :			BOOLEAN ;

	gezeigt :		BOOLEAN ;

	ChangeAktiv :		BOOLEAN ;
	ChangeAdresse,
	ChangeWert :		CARDINAL ;
	StepOverVon,
	StepOverBis,
	TraceBereichVon,
	TraceBereichBis,
	BRKpoint :		CARDINAL = 0 ;
	BRKopcode :		CARDINAL = 1FFH ;

	TraceBereichAktiv,
	StepOverAktiv,
	BreakPointAktiv :	BOOLEAN = FALSE ;

	gemerktesProgramm :	BOOLEAN = FALSE ;

	lastBreakpoint :	ARRAY [0..31] OF CHAR = '' ;

	GrobZaehler,
	CharIndex :		CARDINAL ;
	CharBuffer :		ARRAY [0..31] OF CHAR ;


PROCEDURE ProgrammSuchen (name : ARRAY OF CHAR) : pProgramm ;
	VAR	p :	pProgramm ;
BEGIN
	p := Programm0 ;
	WHILE p <> NIL DO
		IF ExStrings.EqualI (p ^ .name, name) AND (p ^ .olname [0] <= ' ') AND NOT p ^ .ausOKBgeladen THEN
			RETURN p ;
		END ;
		p := p ^ .next ;
	END ;
	RETURN NIL ;
END ProgrammSuchen ;


PROCEDURE ProgrammlaufSuchen (olname : ARRAY OF CHAR) : pProgramm ;
	VAR	p :	pProgramm ;
BEGIN
	p := Programm0 ;
	WHILE p <> NIL DO
		IF ExStrings.EqualI (p ^ .olname, olname) THEN
			RETURN p ;
		END ;
		p := p ^ .next ;
	END ;
	RETURN NIL ;
END ProgrammlaufSuchen ;


PROCEDURE LastProgramm () : pProgramm ;
	VAR	p :	pProgramm ;
BEGIN
	p := Programm0 ;
	IF p <> NIL THEN
		WHILE p ^ .next <> NIL DO
			p := p ^ .next ;
		END ;
	END ;
	RETURN p ;
END LastProgramm ;


PROCEDURE NewProgramm (name, olname : ARRAY OF CHAR) : pProgramm ;
	VAR	p,
		p2 :	pProgramm ;
BEGIN
	NEW (p) ;
	IF Programm0 = NIL THEN
		Programm0 := p ;
		p ^ .prev := NIL ;
	ELSE
		p2 := LastProgramm () ;
		IF p2 <> NIL THEN
			p2 ^ .next := p ;
			p ^ .prev := p2 ;
		END ;
	END ;
	p ^ .next := NIL ;
	p ^ .name := name ;
	p ^ .olname := olname ;
	p ^ .OLK := 0 ;
	p ^ .leitblock := NIL ;
	p ^ .seite0 := NIL ;
	p ^ .PseudoGeb := '' ;

	p ^ .AktQuelle := NIL ;
	p ^ .AktWoerterbuch := NIL ;
	p ^ .quelle0 := NIL ;
	p ^ .QuellZeile0 := NIL ;
	p ^ .woerterbuch0 := NIL ;
	p ^ .AktWB := NIL ;
	p ^ .AktSegmWB := NIL ;
	p ^ .CzoneWB := NIL ;

	p ^ .ausOKBgeladen := FALSE ;

	RETURN p ;
END NewProgramm ;


PROCEDURE NewSeite (lng : CARDINAL) : pSeitenkopie ;
	VAR	pSeite :	pSeitenkopie ;
BEGIN
	ALLOCATE (pSeite, SIZE(pSeite ^ ) - SIZE(pSeite ^ .inhalt) + lng) ;
	pSeite ^ .next := NIL ;
	pSeite ^ .lnginhalt := lng ;
	pSeite ^ .inhalt [0..lng-1] := SeitenPuffer [0..lng-1] ;
	RETURN pSeite ;
END NewSeite ;


PROCEDURE SeitenGroesse (pSeite : pSeitenkopie) : CARDINAL ;
BEGIN
	RETURN SIZE(pSeite ^ ) - SIZE(pSeite ^ .inhalt) + pSeite ^ .lnginhalt ;
END SeitenGroesse ;


PROCEDURE DeleteSeite (VAR pSeite : pSeitenkopie) ;
BEGIN
	DEALLOCATE (pSeite, SeitenGroesse (pSeite)) ;
END DeleteSeite ;

%IF %NOT WEB %THEN


PROCEDURE AssemblerMeldung ;
BEGIN
	Terminal.WriteLn ;
	Terminal.WriteString ('+++++ Programm war nicht frisch assembliert') ;
END AssemblerMeldung ;


PROCEDURE ProgrammMerken (name : ARRAY OF CHAR) : pProgramm ;
	VAR	p,
		p2 :		pProgramm ;
		s,
		lng :		CARDINAL ;
		gross :		GrossSeitenNummer ;
		pseite,
		vseite :	pSeitenkopie ;
BEGIN
	IF NOT Assembler.frischAssembliert THEN
		AssemblerMeldung ;
		RETURN NIL ;
	END ;
	ProgrammLoeschen (name) ;
	p := NewProgramm (name, ' ') ;
	p ^ .start := StartAdresse ;
	p ^ .alarm := Assembler.AlarmAdresse ;
	RegF := StartAdresse ;
	RegX := Assembler.StartX ;
	RegU := Assembler.StartU ;
	SaveRegister (p ^ .register) ;
	vseite := NIL ;

	p ^ .AktQuelle := AktQuelle ;
	p ^ .AktWoerterbuch := AktWoerterbuch ;
	p ^ .quelle0 := quelle0 ;
	p ^ .QuellZeile0 := QuellZeile0 ;
	p ^ .woerterbuch0 := woerterbuch0 ;
	p ^ .AktWB := AktWB ;
	p ^ .AktSegmWB := AktSegmWB ;
	p ^ .CzoneWB := CzoneWB ;
	p ^ .PseudoGeb := PseudoGebiete ;

	AktQuelle := NIL ;
	AktWoerterbuch := NIL ;
	quelle0 := NIL ;
	QuellZeile0 := NIL ;
	woerterbuch0 := NIL ;
	AktWB := NIL ;
	AktSegmWB := NIL ;
	CzoneWB := NIL ;

	FOR gross := 0 TO MaxGrossSeitenNummer DO		(* Programm-Inhalt im Zusatz-RAM speichern *)
		FOR s := 0 TO MaxSeiteInGrossSeite DO
			WITH SeitenKachelTabelle [gross, s] DO
				IF zugeteilt THEN
					lng := KompressSeite (Hauptspeicher [kachel]) ;	(* Inhalt der Seite aus Operator kopieren *)
					pseite := NewSeite (lng) ;
					pseite ^ .next := NIL ;
					pseite ^ .schreibschutz := NOT beschreibbar ;
					pseite ^ .seitennummer := (gross * (MaxSeiteInGrossSeite+1)) + s ;
					IF p ^ .seite0 = NIL THEN
						p ^ .seite0 := pseite ;
					ELSE
						vseite ^ .next := pseite ;
					END ;
					vseite := pseite ;
				END ;
			END ;
		END ;
	END ;
	RETURN p ;
END ProgrammMerken ;


PROCEDURE ProgNameExt (dbname, name : ARRAY OF CHAR ; VAR ziel : ARRAY OF CHAR) ;
BEGIN
	GetLokalDateinamen (dbname, name, ziel) ;
	Strings.Append ('.440', ziel) ;
END ProgNameExt ;


PROCEDURE ProgrammSpeichern (name : ARRAY OF CHAR) ;
	VAR	p :		pProgramm ;
		anz :		CARDINAL ;
		datei :		FileIO.FILE ;
		zwname :	ARRAY [0..267] OF CHAR ;
		str :		ARRAY [0..15] OF CHAR ;
		pseite :	pSeitenkopie ;
BEGIN
	IF NOT Assembler.frischAssembliert THEN
		AssemblerMeldung ;
		RETURN ;
	END ;
	CreateSTDDB ;
	ProgNameExt (_STDDB, name, zwname) ;
	IF NOT FileIO.CreateFile (datei, zwname) THEN
		Terminal.WriteF ('+++++ Datei %s konnte nicht gespeichert werden', zwname) ;
		RETURN ;
	END ;

	p := ProgrammMerken (name) ;				(* erstmal merken *)

	IF p = NIL THEN
		RETURN ;
	END ;

	AlleNamenLoeschen ;
	p ^ .AktQuelle := NIL ;
	p ^ .AktWoerterbuch := NIL ;
	p ^ .quelle0 := NIL ;
	p ^ .QuellZeile0 := NIL ;
	p ^ .woerterbuch0 := NIL ;
	p ^ .AktWB := NIL ;
	p ^ .AktSegmWB := NIL ;
	p ^ .CzoneWB := NIL ;

	FileIO.WriteF (datei, 'V=1\n') ;			(* Version *)
	FileIO.WriteF (datei, 'S=%h\n', p ^ .start) ;
	FileIO.WriteF (datei, 'A=%h\n', p ^ .alarm) ;
	FileIO.WriteF (datei, 'X=%h\n', p ^ .register.X) ;
	FileIO.WriteF (datei, 'U=%h\n', ORD (p ^ .register.U)) ;
	IF PseudoGebiete [0] > ' ' THEN
		FileIO.WriteF (datei, 'G=%s\n', PseudoGebiete) ;
	END ;

	anz := 0 ;
	pseite := p ^ .seite0 ;
	WHILE pseite <> NIL DO					(* erstmal die Seiten zaehlen *)
		INC (anz) ;
		pseite := pseite ^ .next ;
	END ;

	FileIO.WriteF (datei, "N=%c", anz) ;			(* Seiten-Anzahl *)

	pseite := p ^ .seite0 ;
	WHILE pseite <> NIL DO					(* nun Seiten-Inhalte *)
		IF pseite ^ .schreibschutz THEN
			str := 'S' ;				(* schreibgeschützt *)
		ELSE
			str := 'N' ;				(* normal *)
		END ;
		FileIO.WriteF (datei, "\nI=%'03h.%s.%'04h\n", pseite ^ .seitennummer, str, pseite ^ .lnginhalt) ;
		FileIO.WriteBlock (datei, pseite ^ .inhalt, pseite ^ .lnginhalt) ;
		pseite := pseite ^ .next ;
	END ;

	FileIO.CloseFile (datei) ;
END ProgrammSpeichern ;


PROCEDURE ProgrammLaden (name : ARRAY OF CHAR) : pProgramm ;
	VAR	p :		pProgramm ;
		datei :		FileIO.FILE ;
		version,
		hilf,
		lng,
		anz :		CARDINAL ;
		str,
		zwname :	ARRAY [0..267] OF CHAR ;
		pseite,
		vseite :	pSeitenkopie ;
		buffer :	ARRAY [0..8191] OF CHAR ;
		ok :		BOOLEAN ;

	PROCEDURE zerstoert () : pProgramm ;
	BEGIN
		Terminal.WriteString ('+++++ Programm ist zerstört') ;
		RETURN NIL ;
	END zerstoert ;

BEGIN
	Assembler.frischAssembliert := FALSE ;
	ProgNameExt (_STDDB, name, zwname) ;
	IF NOT FileFunc.FileExists (zwname) THEN
		ProgNameExt (_OEFDB, name, zwname) ;
		IF NOT FileFunc.FileExists (zwname) THEN
			RETURN NIL ;
		END ;
	END ;

	ClearAssocIndex ;

	FileFunc.OpenFileEx (datei, zwname, FileFunc.ReadOnlyDenyWrite, FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF datei.status <> 0 THEN
		RETURN NIL ;
	END ;
	FileFunc.SetFileBuffer (datei, buffer) ;

	ProgrammLoeschen (name) ;
	p := NewProgramm (name, ' ') ;
	vseite := NIL ;
	anz := 0 ;
	ok := TRUE ;
	PseudoGebiete := '' ;

	LOOP
		IF NOT FileIO.ReadLn (datei, str) THEN
			ok := FALSE ;
		ELSIF str [1] <> '=' THEN
			ok := FALSE
		ELSE

			CASE str [0] OF
			'V' :				(* Version *)
				ok := Conversions.StrToCard (str [2..7], version) ;
			|
			'S' :				(* Startadresse *)
				ok := Conversions.StrBaseToCard (str [2..7], 16, hilf) ;
				p ^ .start := hilf ;
			|
			'A' :				(* Alarmadresse initial *)
				ok := Conversions.StrBaseToCard (str [2..7], 16, hilf) ;
				p ^ .alarm := hilf ;
			|
			'X' :				(* Indexbasis initial *)
				ok := Conversions.StrBaseToCard (str [2..7], 16, hilf) ;
				p ^ .register.X := hilf ;
			|
			'U' :				(* Unterprogrammordnungszähler initial *)
				ok := Conversions.StrBaseToCard (str [2..7], 16, hilf) ;
				p ^ .register.U := hilf ;
			|
			'G' :				(* Pseudo-Gebiete *)
				PseudoGebiete := str [2..LENGTH (str)] ;
				p ^ .PseudoGeb := PseudoGebiete ;
			|
			'N' :				(* Anzahl Seiten *)
				ok := Conversions.StrToCard (str [2..7], anz) ;
			|
			'I' :				(* Inhalt einer Seite *)
				IF anz = 0 THEN
					ok := FALSE ;
				ELSE
					DEC (anz) ;
					ok := Conversions.StrBaseToCard (str [2..4], 16, hilf) AND (str [5] = '.')
						AND (str [7] = '.') AND Conversions.StrBaseToCard (str [8..11], 16, lng) ;
					IF lng > HIGH(SeitenPuffer) + 1 THEN
						ok := FALSE ;
						lng := 7 ;
					ELSE
						FileIO.ReadBlock (datei, SeitenPuffer, lng) ;
					END ;
					pseite := NewSeite (lng) ;
					pseite ^ .seitennummer := hilf ;
					IF str [6] = 'N' THEN
						pseite ^ .schreibschutz := FALSE ;
					ELSIF str [6] = 'S' THEN
						pseite ^ .schreibschutz := TRUE ;
					ELSE
						ok := FALSE ;
					END ;
					IF ok THEN

					END ;
					IF p ^ .seite0 = NIL THEN
						p ^ .seite0 := pseite ;
					ELSE
						vseite ^ .next := pseite ;
					END ;
					vseite := pseite ;
					IF anz = 0 THEN			(* die Seiteninhalte waren das Letzte in der Datei *)
						EXIT ;
					END ;
					IF NOT FileIO.ReadLn (datei, str) OR (str [0] <> 0C) THEN
						ok := FALSE ;
					END ;
				END ;
			ELSE
				ok := FALSE ;
			END ;
		END ;

		IF NOT ok THEN
			RETURN zerstoert () ;
		END ;
	END ;

	FileIO.CloseFile (datei) ;

	p ^ .AktQuelle := NIL ;
	p ^ .AktWoerterbuch := NIL ;
	p ^ .quelle0 := NIL ;
	p ^ .QuellZeile0 := NIL ;
	p ^ .woerterbuch0 := NIL ;
	p ^ .AktWB := NIL ;
	p ^ .AktSegmWB := NIL ;
	p ^ .CzoneWB := NIL ;

	RETURN p ;

END ProgrammLaden ;

%END

PROCEDURE InitOLK (name, oln : ARRAY OF CHAR) ;
	VAR	vater,
		entschl :	CARDINAL ;
BEGIN
	WITH Operatoren [AktOperator] DO
		vater := OLK ;
		IF Strings.Equal (OLN, 'PS&ENTSCHL') THEN	(* Der Vater ist wirklich der Entschluessler *)
			entschl := OLK ;
		ELSE
			entschl := OLKE ;
		END ;
	END ;

	INC (OLKpegel) ;
	AktOLK := OLKpegel ;
	INC (AktOperator) ;

	WITH Operatoren [AktOperator] DO
		OLK := AktOLK ;
		OLKV := vater ;
		OLKE := entschl ;
		ON := name ;
		OLN := oln ;
		ProgrammEndeFehler := 0 ;
		AlarmAdresse := 0 ;
		AnzahlBefehle := 0 ;
		AnzahlTakte := 0 ;
		UnterbrechungsAdresse := 0 ;
		MVNR := '' ;
		ZustellungsSperre := FALSE ;
		AlarmSperre := FALSE ;
		ProgrammEnde := FALSE ;
		AlarmGewesen := FALSE ;
	END ;
END InitOLK ;


PROCEDURE ProgrammStarten (name, olname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	p0,
		pol :		pProgramm ;
		pseite :	pSeitenkopie ;
		oln :		ARRAY [0..31] OF CHAR ;
		nr :		ARRAY [0..3] OF CHAR ;
		gross :		GrossSeitenNummer ;
		snr :		CARDINAL ;
BEGIN
	Assembler.frischAssembliert := FALSE ;
	ClearAssocIndex ;

	OLNvergeben := FALSE ;
	OKBaktiv := FALSE ;

	IF cmdAsmVorrang THEN
		p0 := NIL ;
	ELSE
		p0 := ProgrammOKBsuchen (name) ;
	END ;

	IF p0 = NIL THEN				(* keine TR440-OKB gefunden *)
%IF WEB %THEN
		RETURN FALSE				(* Programm nicht gefunden *)
%ELSE
		p0 := ProgrammSuchen (name) ;
		IF p0 = NIL THEN			(* Programm gar nicht gemerkt *)
			p0 := ProgrammLaden (name) ;
			IF p0 = NIL THEN		(* Programm auch nicht montiert *)
				IF cmdAsmVorrang THEN
					p0 := ProgrammOKBsuchen (name) ;
				END ;
				IF p0 = NIL THEN
					IF ProgrammierModus THEN
						Terminal.WriteF ('\n+++++ Programm %s nicht gefunden\n', name) ;
					END ;
					RETURN FALSE ;		(* Programm nicht gefunden *)
				ELSE
					PseudoGebiete := '' ;
				END ;
			ELSE
				PseudoGebiete := p0 ^ .PseudoGeb ;
			END ;
		ELSE
			PseudoGebiete := p0 ^ .PseudoGeb ;
		END ;
%END
	ELSE
		PseudoGebiete := '' ;
	END ;

	IF olname [0] <> 0C THEN		(* expliziter OLNAME gesucht *)
		pol := ProgrammlaufSuchen (olname) ;
		IF pol <> NIL THEN
			OLNvergeben := TRUE ;
			RETURN FALSE ;		(* existiert bereits *)
		END ;
		oln := olname ;
	ELSE
		nr := '' ;
		LOOP
			oln := name ;
			IF nr [0] = 0C THEN
				nr := '1' ;
			ELSE
				nr [0] := CHR(ORD(nr [0]) + 1) ;
				Strings.Append (nr, oln) ;
			END ;
			pol := ProgrammlaufSuchen (oln) ;
			IF pol = NIL THEN			(* der OLNAME existiert noch nicht *)
				EXIT ;
			END ;
		END ;
	END ;
	pol := NewProgramm (name, oln) ;
	pol ^ .start := p0 ^ .start ;
	pol ^ .alarm := p0 ^ .alarm ;
	pol ^ .register := p0 ^ .register ;
	pol ^ .seite0 := NIL ;

%IF %NOT WEB %THEN
	SetGrundzustand (FALSE) ;
%END

	GesamtZahlTakte := 0 ;
	Anzahl := 0 ;

	lastDateiname := name ;

	SchreibschutzAktiv := FALSE ;
	NEW (pol ^ .leitblock) ;
	FOR gross := 0 TO MaxGrossSeitenNummer DO
		FOR snr := 0 TO MaxSeiteInGrossSeite DO
			WITH pol ^ .leitblock ^ [gross, snr] DO
				zugeteilt := FALSE ;
				beschreibbar := FALSE ;
			END ;
		END ;
	END ;

	InitOLK (name, oln) ;

	IF OKBaktiv THEN							(* Programm-Inhalt aus &L1/Gebieten in KSP laden *)
		pol ^ .ausOKBgeladen := TRUE ;
		SeitenKachelTabelle := pol ^ .leitblock ^ ;
		IF NOT GebieteLadenOKB () THEN
			RETURN FALSE ;
		END ;
		pol ^ .leitblock ^ := SeitenKachelTabelle ;
	ELSE
		MV_NR := '' ;
		pseite := p0 ^ .seite0 ;
		WHILE pseite <> NIL DO						(* Programm-Inhalt aus Zusatz-RAM in KSP kopieren *)
			snr := pseite ^ .seitennummer ;
			WITH pol ^ .leitblock ^ [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
				zugeteilt := TRUE ;
				beschreibbar := NOT pseite ^ .schreibschutz ;
				kachel := KachelBelegen () ;
				IF debug.MemoryProtokoll THEN
					TraceF ("  -> Prog Seite %'03h", snr) ;
				END ;
				SeitenPuffer [0..pseite ^ .lnginhalt-1] := pseite ^ .inhalt [0..pseite ^ .lnginhalt-1] ;
				DekompressSeite (KSP [kachel], pseite ^ .lnginhalt) ;
				Hauptspeicher [kachel] := KSP [kachel] ;
			END ;
			pseite := pseite ^ .next ;
		END ;
		SeitenKachelTabelle := pol ^ .leitblock ^ ;
	END ;

	LoadRegister (pol ^ .register) ;
	SchreibschutzAktiv := TRUE ;

	StartAdresse := pol ^ .start ;
	Assembler.AlarmAdresse := pol ^ .alarm ;
	Assembler.StartX := RegX ;
	Assembler.StartU := RegU ;

	AktQuelle := p0 ^ .AktQuelle ;
	AktWoerterbuch := p0 ^ .AktWoerterbuch ;
	quelle0 := p0 ^ .quelle0 ;
	QuellZeile0 := p0 ^ .QuellZeile0 ;
	woerterbuch0 := p0 ^ .woerterbuch0 ;
	AktWB := p0 ^ .AktWB ;
	AktSegmWB := p0 ^ .AktSegmWB ;
	CzoneWB := p0 ^ .CzoneWB ;

	pol ^ .OLK := AktOLK ;
	WITH Operatoren [AktOperator] DO
		AlarmAdresse := pol ^ .alarm ;
		MVNR := MV_NR ;
	END ;

	gemerktesProgramm := TRUE ;

	RETURN TRUE ;
END ProgrammStarten ;


PROCEDURE ProgrammPausieren ;
	VAR	pol :		pProgramm ;
BEGIN

	WITH Operatoren [AktOperator] DO
		TraceF ('* Programm pausieren %s', OLN) ;
		AnzahlTakte := GesamtZahlTakte ;
		AnzahlBefehle := Anzahl ;
		pol := ProgrammlaufSuchen (OLN) ;
		IF pol = NIL THEN
			RETURN ;
		END ;
	END ;

	ClearAssocIndex ;
	pol ^ .leitblock ^ := SeitenKachelTabelle ;
	GesamtZahlTakte := 0 ;
	Anzahl := 0 ;
	SaveRegister (pol ^ .register) ;
	pol ^ .QuellZeile0 := QuellZeile0 ;

	SeitenKachelTabelle := Programm0 ^ .next ^ .leitblock ^  ;
END ProgrammPausieren ;


PROCEDURE ProgrammFortsetzen ;
	VAR	pol :		pProgramm ;
BEGIN

	WITH Operatoren [AktOperator] DO
		TraceF ('* Programm fortsetzen %s', OLN) ;
		pol := ProgrammlaufSuchen (OLN) ;
		IF pol = NIL THEN
			RETURN ;
		END ;
		GesamtZahlTakte := AnzahlTakte ;
		Anzahl := AnzahlBefehle ;
		lastDateiname := ON ;
		AktOLK := OLK ;
	END ;
	QuellZeile0 := pol ^ .QuellZeile0 ;
	ClearAssocIndex ;
	LoadRegister (pol ^ .register) ;
	FolgeAdresse := RegF ;
	SeitenKachelTabelle := pol ^ .leitblock ^   ;

	IF AlleProgrammeAbbrechen THEN
		AlarmGewesen := TRUE ;
		globProgrammEnde := TRUE ;
	ELSE
		AlarmGewesen := FALSE ;
		globProgrammEnde := FALSE ;
	END ;
END ProgrammFortsetzen ;


PROCEDURE DeleteProgramm (VAR p0 : pProgramm) ;
VAR		zw,
		pseite :	pSeitenkopie ;
BEGIN
	TraceF ('* Programm löschen %s', p0 ^ .olname) ;
	pseite := p0 ^ .seite0 ;
	WHILE pseite <> NIL DO
		zw := pseite ;
		pseite := pseite ^ .next ;
		DeleteSeite (zw) ;
	END ;

	IF p0 ^ .next <> NIL THEN
		p0 ^ .next ^ .prev := p0 ^ .prev ;
	END ;
	IF p0 ^ .prev <> NIL THEN
		p0 ^ .prev ^ .next := p0 ^ .next ;		(* Operatorkörper raus aus Kette *)
	END ;

	DISPOSE (p0) ;
END DeleteProgramm ;


PROCEDURE ProgrammBeenden ;
	VAR	p0,
		pol :		pProgramm ;
		zw,
		pseite :	pSeitenkopie ;
		snr :		CARDINAL ;
BEGIN
	Assembler.frischAssembliert := FALSE ;

	WITH Operatoren [AktOperator] DO

		TraceF ('* Programm beenden %s', OLN) ;

		pol := ProgrammlaufSuchen (OLN) ;
		IF pol = NIL THEN
			RETURN ;
		END ;
	END ;

	CheckKSPbelegung ;

	ClearAssocIndex ;
	IF pol ^ .ausOKBgeladen THEN	(*	31.03.18	*)
		p0 := pol ;
	ELSE
		p0 := ProgrammSuchen (pol ^ .name) ;
		IF p0 = NIL THEN
			RETURN ;
		END ;
	END ;
	pseite := p0 ^ .seite0 ;
	WHILE pseite <> NIL DO
		snr := pseite ^ .seitennummer ;
		WITH pol ^ .leitblock ^ [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
			KachelFreigeben (kachel) ;
		END ;
		pseite := pseite ^ .next ;
	END ;
	GesamtZahlTakte := 0 ;
	Anzahl := 0 ;

	CheckKSPbelegung ;

	KillAlleOLgebiete (AktOLK) ;

	AlleEigenenDateienSchliessen ;

	DEC (AktOperator) ;

	AktQuelle := NIL ;
	AktWoerterbuch := NIL ;
	quelle0 := NIL ;
	QuellZeile0 := NIL ;
	woerterbuch0 := NIL ;
	AktWB := NIL ;
	AktSegmWB := NIL ;
	CzoneWB := NIL ;


	IF pol ^ .leitblock <> NIL THEN
		DISPOSE (pol ^ .leitblock) ;
	END ;

	IF pol ^ .next <> NIL THEN
		pol ^ .next ^ .prev := pol ^ .prev ;
	END ;
	IF pol ^ .prev <> NIL THEN
		pol ^ .prev ^ .next := pol ^ .next ;		(* Operatorlauf raus aus Kette *)
	END ;

	IF pol ^ .ausOKBgeladen THEN
		DeleteProgramm (p0) ;
	ELSE
		DISPOSE (pol) ;
	END ;

%IF %NOT WEB %THEN
	SeitenKachelTabelle := Programm0 ^ .next ^ .leitblock ^  ;	(* vom Pseudo-Programm 'TR440' *)
%END
END ProgrammBeenden ;


PROCEDURE ProgrammLoeschen (name : ARRAY OF CHAR) ;
	VAR	p0 :		pProgramm ;
BEGIN
	LOOP
		p0 := ProgrammSuchen (name) ;
		IF p0 = NIL THEN
			RETURN ;
		END ;
		DeleteProgramm (p0) ;
	END ;
END ProgrammLoeschen ;


PROCEDURE KompressSeite (seite : pSeite) (* lng *) : CARDINAL ;
	VAR	lng,
		repl,
		getindex,
		putindex :	CARDINAL ;

	PROCEDURE put (ch : CARDINAL8) ;
	BEGIN
		SeitenPuffer [putindex] := ch ;
		INC (putindex) ;
	END put ;

	PROCEDURE stuelp ;
		VAR	i :	CARDINAL ;
	BEGIN
		WITH seite ^ [getindex-1] DO
			IF repl > 31 THEN
				put (80H + ((repl DIV 256) SHL 2) + ORD (TK) BAND 3) ;
				put (repl MOD 256) ;
			ELSE
				put (repl SHL 2 + ORD (TK) BAND 3) ;
			END ;
			FOR i := 0 TO 5 DO
				put (byte6 [i]) ;
			END ;
		END ;
		repl := 0 ;
	END stuelp ;

BEGIN
	getindex := 1 ;
	putindex := 0 ;
	repl := 0 ;
	REPEAT
		WITH seite ^ [getindex-1] DO
			IF (seite ^ [getindex].TK <> TK)
			OR (seite ^ [getindex].byte6 [0] <> byte6 [0])
			OR (seite ^ [getindex].byte6 [1] <> byte6 [1])
			OR (seite ^ [getindex].byte6 [2] <> byte6 [2])
			OR (seite ^ [getindex].byte6 [3] <> byte6 [3])
			OR (seite ^ [getindex].byte6 [4] <> byte6 [4])
			OR (seite ^ [getindex].byte6 [5] <> byte6 [5]) THEN
				stuelp ;
			ELSE
				INC (repl) ;
			END ;
		END ;

		INC (getindex) ;
	UNTIL getindex > MaxGanzwortInSeite ;
	stuelp ;
	RETURN putindex ;
END KompressSeite ;


PROCEDURE DekompressSeite (seite : pSeite ; lng : CARDINAL) ;
	VAR	repl,
		i,
		getindex,
		putindex :	CARDINAL ;
		tk :		CARDINAL8 ;
BEGIN
	getindex := 0 ;
	putindex := 0 ;
	REPEAT
		repl := ORD (SeitenPuffer [getindex]) SHR 2 ;
		tk := ORD (SeitenPuffer [getindex]) BAND 3 ;
		IF repl > 31 THEN
			INC (getindex) ;
			repl := (repl BAND 31) * 256 + ORD (SeitenPuffer [getindex]) ;
		END ;
		WITH seite ^ [putindex] DO
			TK := tk ;
			byte6 [0..5] := SeitenPuffer [getindex + 1 .. getindex + 6] ;
		END ;
		FOR i := 1 TO repl DO
			seite ^ [putindex + i] := seite ^ [putindex] ;
		END ;
		putindex := putindex + repl + 1 ;
		getindex := getindex + 7 ;
	UNTIL getindex >= lng ;
END DekompressSeite ;


PROCEDURE InitDauer ;
BEGIN
	StartZeit := GetTickCount () ;
	GesamtZahlTakte := 0 ;
END InitDauer ;


PROCEDURE protSekunden (wert : CARDINAL) ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
		l :	CARDINAL ;
BEGIN
	IF wert < 1000 THEN
		Conversions.CardToStr (wert+1000, str) ;
		str [0] := '0' ;
	ELSE
		Conversions.CardToStr (wert, str) ;
	END ;
	l := LENGTH(str) ;
	Terminal.WriteString (str [0..l-4]) ;
	Terminal.WriteString ('.') ;
	Terminal.WriteString (str [l-3..l]) ;
	Terminal.WriteString (' sec.') ;
END protSekunden ;


%IF %NOT WEB %THEN


PROCEDURE ProtTakte ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	Conversions.LongToStr (GesamtZahlTakte, str) ;
	Terminal.WriteString (str) ;
	GesamtZahlTakte := 0 ;
END ProtTakte ;


PROCEDURE ProtKSP ;
	VAR	adr,
		KSP :	CARDINAL ;
BEGIN
	IF cmdShowInfo THEN
		KSP := 0 ;
		FOR adr := 0 TO AktMaxGanzwortNummer BY MaxGanzwortInSeite+1 DO
			IF TestAdresse (adr * 2) THEN
				INC (KSP) ;
			END ;
		END ;
		Terminal.WriteF ('%c K Speicher, ', KSP) ;
	END ;
END ProtKSP ;


PROCEDURE ProtDauer ;
	VAR	str :		ARRAY [0..31] OF CHAR ;
		dauer,
		dauer2 :	CARDINAL ;
BEGIN
	IF ProgrammierModus AND cmdShowInfo THEN

		dauer := GetTickCount () - StartZeit ;
		dauer2 := (GesamtZahlTakte + TakteProMikroSekunde * 500) DIV (TakteProMikroSekunde * 1000) ;		(* 16 Takte ~ 1 Mikrosekunde *)

		IF GesamtZahlTakte <> 0 THEN
			Terminal.WriteLn ;
				ProtKSP ;
				Terminal.WriteF ('%l Takte ~ ', GesamtZahlTakte) ;
				protSekunden (dauer2) ;
		END ;
		Terminal.WriteLn ;

		Terminal.WriteLn ;
			protSekunden (dauer) ;
		Terminal.WriteLn ;
	END ;
END ProtDauer ;


PROCEDURE InitRegZeigen ;
BEGIN
	vRegA := RegA ;
	vRegQ := RegQ ;
	vRegD := RegD ;
	vRegH := RegH ;
	vRegB := RegB ;
	vRegK := RegK ;
	vRegY := RegY ;
	vRegX := RegX ;
	vRegU := RegU ;
	vRegM := RegM ;
END InitRegZeigen ;


PROCEDURE protPos (reg : CHAR) ;
	VAR	x,
		x2,
		y :	CARDINAL ;
BEGIN
	IF debug.BildschirmProtokoll THEN
		Terminal.GetPosition (x, y) ;
		IF NOT gezeigt THEN
			IF x > 80 THEN
				Terminal.WriteLn ;
				Terminal.GetPosition (x, y) ;
			END ;
			gezeigt := TRUE ;
		END ;
		CASE reg OF
		'A' :	x := PosTrans + 32 ;
					x2 := PosTrans + 31 ;
		|
		'Q' :	x := PosTrans + 50 ;
					x2 := PosTrans + 55 ;
		|
		'D' :	x := PosTrans + 68 ;
					x2 := PosTrans + 79 ;
		|
		'H' :	x := PosTrans + 86 ;
					x2 := PosTrans + 103 ;
		|
		'B' :	x := PosTrans + 104 ;
					x2 := PosTrans + 127 ;
		ELSE
			x := x + 3 ;
					x2 := PosTrans + 143 ;
		END ;
		Terminal.Position (x, y) ;
	ELSE
		CASE reg OF
		'A' :			x2 := PosTrans + 31 ;
		|
		'Q' :			x2 := PosTrans + 55 ;
		|
		'D' :			x2 := PosTrans + 79 ;
		|
		'H' :			x2 := PosTrans + 103 ;
		|
		'B' :			x2 := PosTrans + 127 ;
		ELSE
					x2 := PosTrans + 143 ;
		END ;
	END ;
										AsmTrace.SetPos (x2) ;
END protPos ;


PROCEDURE protanf (reg : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	protPos (reg [0]) ;
	IF debug.BildschirmProtokoll THEN
		Terminal.WriteString (reg) ;
		IF reg [0] >= 'A' THEN
			Terminal.WriteString('=') ;
		END ;
	END ;
										AsmTrace.WriteString (reg) ;
										IF reg [0] >= 'A' THEN
											AsmTrace.WriteString ('=') ;
										END ;
	RETURN TRUE ;
END protanf ;


PROCEDURE protLuecke (reg : CHAR) ;
BEGIN
	protPos (reg) ;
END protLuecke ;


PROCEDURE prot48 (reg : ARRAY OF CHAR ; VAR wert : reg48) ;
	VAR 	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	protanf (reg) ;
	CardToHex12 (wert.inh, str) ;
	IF debug.BildschirmProtokoll THEN
		Terminal.WriteCard (wert.TK) ;
		Terminal.Write (' ') ;
		Terminal.WriteString (str) ;
	END ;
										AsmTrace.WriteCard (wert.TK) ;
										AsmTrace.WriteString (' ') ;
										AsmTrace.WriteString (str) ;
END prot48 ;


PROCEDURE prot24 (reg : ARRAY OF CHAR ; wert : CARDINAL) ;
VAR 	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	protanf (reg) ;
	CardToHex4 (wert, str) ;
	IF debug.BildschirmProtokoll THEN
		Terminal.WriteString (str) ;
	END ;									AsmTrace.WriteString (str) ;
END prot24 ;


PROCEDURE prot8 (reg : ARRAY OF CHAR ; wert : CARDINAL) ;
VAR 	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	protanf (reg) ;
	CardToHex2 (wert, str) ;
	IF debug.BildschirmProtokoll THEN
		Terminal.WriteString (str) ;
	END ;									AsmTrace.WriteString (str) ;
END prot8 ;


PROCEDURE prot1 (reg : ARRAY OF CHAR ; wert : CARDINAL) ;
VAR 	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	protanf (reg) ;
	IF reg [0] >= 'A' THEN
		CardToHex1 (wert, str) ;
		IF debug.BildschirmProtokoll THEN
			Terminal.WriteString (str) ;
		END ;								AsmTrace.WriteString (str) ;
	END ;
END prot1 ;


PROCEDURE protDez (reg : ARRAY OF CHAR ; wert : CARDINAL) ;
VAR 	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF protanf (reg) THEN
		IF debug.BildschirmProtokoll THEN
			Terminal.WriteCard (wert) ;
		END ;								AsmTrace.WriteCard (wert) ;
	END ;
END protDez ;


PROCEDURE RegAlleZeigen ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	gezeigt := FALSE ;
	prot48 ('A', RegA) ;
	vRegA := RegA ;
	prot48 ('Q', RegQ) ;
	vRegQ := RegQ ;
	prot48 ('D', RegD) ;
	vRegD := RegD ;
	prot48 ('H', RegH) ;
	vRegH := RegH ;
	prot24 ('B', RegB BAND 0FFFFFFH) ;
	vRegB := RegB ;
	prot24 ('X', RegX BAND 0FFFFFFH) ;
	vRegX := RegX ;
	prot8 ('K', ORD(RegK)) ;
	vRegK := RegK ;
	prot8 ('Y', ORD(RegY)) ;
	vRegY := RegY ;
	prot8 ('U', ORD(RegU)) ;
	vRegU := RegU ;
	prot1 ('M', ORD(RegM)) ;
	vRegM := RegM ;
END RegAlleZeigen ;


PROCEDURE RegZeigen ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	gezeigt := FALSE ;
	IF aend48 (vRegA, RegA) THEN
		prot48 ('A', RegA) ;
		vRegA := RegA ;
	ELSE
		protLuecke ('A') ;
	END ;
	IF aend48 (vRegQ, RegQ) THEN
		prot48 ('Q', RegQ) ;
		vRegQ := RegQ ;
	ELSE
		protLuecke ('Q') ;
	END ;
	IF aend48 (vRegD, RegD) THEN
		prot48 ('D', RegD) ;
		vRegD := RegD ;
	ELSE
		protLuecke ('D') ;
	END ;
	IF aend48 (vRegH, RegH) THEN
		prot48 ('H', RegH) ;
		vRegH := RegH ;
	ELSE
		protLuecke ('H') ;
	END ;
	IF vRegB <> RegB THEN
		prot24 ('B', RegB BAND 0FFFFFFH) ;
		vRegB := RegB ;
	ELSE
		protLuecke ('B') ;
	END ;
	IF vRegX <> RegX THEN
		prot24 ('X', RegX BAND 0FFFFFFH) ;
		vRegX := RegX ;
	END ;
	IF vRegK <> RegK THEN
		prot8 ('K', ORD(RegK)) ;
		vRegK := RegK ;
	END ;
	IF vRegY <> RegY THEN
		prot8 ('Y', ORD(RegY)) ;
		vRegY := RegY ;
	END ;
	IF vRegU <> RegU THEN
		prot8 ('U', ORD(RegU)) ;
		vRegU := RegU ;
	END ;
	IF vRegM <> RegM THEN
		prot1 ('M', ORD(RegM)) ;
		vRegM := RegM ;
	END ;
	IF mod1 <> 0 THEN
		prot24 ('mod1', mod1 BAND 0FFFFFFH) ;
	END ;
	IF mod2 <> 0 THEN
		prot24 ('mod2', mod2 BAND 0FFFFFFH) ;
	END ;
	IF TKalarmErkannt THEN
		prot1 ('*TK-Al.', 1) ;
	END ;
	IF BUEalarmErkannt THEN
		prot1 ('*BÜ-Al.', 1) ;
	END ;
	IF FolgeAdresse <> (RegF + 1) THEN
		prot1 ('!', 1) ;
	END ;
	IF debug.TakteZeigen THEN
		protDez ('T', VAL (CARDINAL, GesamtZahlTakte - merkTakte)) ;
	END ;
END RegZeigen ;


PROCEDURE SprungZeigen ;
BEGIN
	prot24 ('F', RegF BAND 3FFFFFH) ;
END SprungZeigen ;


PROCEDURE ProgrammZeile (befehl : CARDINAL) ;				(* wird auch befehlsweise vom Interpreter aufgerufen *)
BEGIN
	Assembler.frischAssembliert := FALSE ;
	PosTrans := 40 ;
	RegF := AblageAdresse ;
	merkTakte := GesamtZahlTakte ;
	BefehlAusfuehren (befehl) ;
	IF debug.RegDump THEN
		RegZeigen ;
	END ;
END ProgrammZeile ;


PROCEDURE VorrangStart (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgrammStarten (name, '') THEN
		ProgrammLauf (StartAdresse) ;
		ProgrammBeenden ;
	END ;
END VorrangStart ;


PROCEDURE Speichern (zeile : ARRAY OF CHAR) ;
	VAR	i,
		adresse,
		wert :		CARDINAL ;
		parm1,
		parm2 :		ARRAY [0..31] OF CHAR ;
BEGIN
	AsmDisasm.TRIM (zeile) ;
	i := 0 ;
	IF ExStrings.GetNextItem (zeile, i, parm1, ' ,') THEN		(* 1. Adresse *)
		IF GetZahl (parm1, adresse) THEN
			IF ExStrings.GetNextItem (zeile, i, parm2, ' ,') THEN		(* 2. Wert *)
				IF GetZahl (parm2, wert) THEN
					PutHalbwort (adresse, wert) ;
				ELSE
					AsmDisasm.protZeile ('unzul. Wert') ;
				END ;
			ELSE
				AsmDisasm.protZeile ('+++++ Wert fehlt') ;
			END ;
		ELSE
			AsmDisasm.protZeile ('+++++ unzul. Adresse') ;
		END ;
	ELSE
		AsmDisasm.protZeile ('+++++ Adresse fehlt') ;
	END ;
END Speichern ;


PROCEDURE VorrangHilfe ;
BEGIN
	Terminal.WriteLn ;
	AsmDisasm.protZeile ('Vorrang-Befehle :') ;
	AsmDisasm.protZeile2 ('Trace xxx',	'Trace-Bedingungen setzen/löschen') ;
	AsmDisasm.protZeile2 ('MEMORY  ',	'Speicherbelegung zeigen') ;
	AsmDisasm.protZeile2 ('Disasm adr',	'Speicher befehlsweise ab angegebener Adresse disassemblieren') ;
	AsmDisasm.protZeile2 ('Show adr',	'Speicherinhalt ganzwortweise ab Adresse zeigen') ;
	AsmDisasm.protZeile2 ('Show X ',	'Indexzellen-Inhalte zeigen') ;
	AsmDisasm.protZeile2 ('Starte prog',	'gemerktes/montiertes Programm im Vorrang starten') ;
	AsmDisasm.protZeile2 ('Put adr, wert',	'Halbwort-Wert auf Adresse speichern') ;
	AsmDisasm.protZeile2 ('Break adr',	'Programm-Fortsetzung bis zu der Adresse oder dem Befehl') ;
	AsmDisasm.protZeile2 ('Break von:bis',	'Programm-Fortsetzung, Trace auf Adressbereich beschränken') ;
	AsmDisasm.protZeile2 ('Break Cadr',	'Programm-Fortsetzung bis die Adresse ihren Inhalt ändert') ;
	AsmDisasm.protZeile2 ('REG     ',	'alle Register zeigen') ;
	AsmDisasm.protZeile2 ('Esc     ',	'zurück aus Vorrangstufe') ;
	Terminal.WriteLn ;
END VorrangHilfe ;


PROCEDURE Vorrang ;
	VAR	zeile :		ARRAY [0..255] OF CHAR ;
		b,
		gemerkt :	BOOLEAN ;
BEGIN
	gemerkt := gemerktesProgramm ;
	LOOP
		Terminal.WriteLn ;
		Terminal.WriteString ('Vorrang#: ') ;
		LOOP
			b := TerminalEingabe.ReadString (zeile) ;
			IF Terminal.AbbruchWunsch THEN
				Terminal.AbbruchWunsch := FALSE ;
				RETURN ;
			END ;
			IF MenueBefehlStehtAn THEN
				IF MenueBefehl () THEN
					Terminal.WriteLn ;
					Terminal.WriteString ('Vorrang#: ') ;
				END ;
			ELSIF NOT b THEN
				RETURN ;
			ELSIF zeile [0] <> 0C THEN
				EXIT ;
			END ;
		END ;
		AsmDisasm.PutIntoClipboard (zeile) ;
		Strings.Capitalize (zeile) ;
		IF zeile [0] = '?' THEN
			VorrangHilfe ;
		ELSIF Strings.Equal (zeile [0..5], 'DISASM') THEN
			AsmDisasm.BefehleZeigen (zeile [7..255]) ;
		ELSIF Strings.Equal (zeile [0..5], 'MEMORY') THEN
			upShowMemory ;
		ELSIF cmdEigenerAssembler AND Strings.Equal (zeile [0..5], 'STARTE') THEN
			IF gemerkt THEN
				ProgrammPausieren ;
				gemerktesProgramm := FALSE ;
			END ;
			VorrangStart (zeile [7..255]) ;
			IF gemerkt THEN
				gemerktesProgramm := TRUE ;
				ProgrammFortsetzen ;
			END ;
		ELSIF cmdEigenerAssembler AND Strings.Equal (zeile [0..4], 'START') THEN
			IF gemerkt THEN
				ProgrammPausieren ;
				gemerktesProgramm := FALSE ;
			END ;
			VorrangStart (zeile [6..255]) ;
			IF gemerkt THEN
				gemerktesProgramm := TRUE ;
				ProgrammFortsetzen ;
			END ;
		ELSIF Strings.Equal (zeile [0..3], 'SHOW') THEN
			AsmDisasm.Zeigen (zeile [5..255]) ;
		ELSIF Strings.Equal (zeile [0..4], 'BREAK') THEN
			IF WeiterBisBreakpoint (zeile [6..255]) THEN
				EXIT ;
			END ;
		ELSIF Strings.Equal (zeile [0..4], 'TRACE') THEN
			AsmDisasm.TraceBefehl (zeile [6..255]) ;
		ELSIF Strings.Equal (zeile [0..2], 'PUT') THEN
			Speichern (zeile [4..255]) ;
		ELSIF Strings.Equal (zeile [0..2], 'REG') THEN
			RegAlleZeigen ;
		ELSE
			AsmDisasm.protZeile ('+++++ unbekannter Befehl') ;
		END ;
	END ;
END Vorrang ;


PROCEDURE ProgrammlaufHilfe ;
BEGIN
	Terminal.WriteLn ;
	AsmDisasm.protZeile ('Eingabezeichen während des Programmlaufes :') ;
	AsmDisasm.protZeile2 ('#     ',	'Vorrang-Kommandos') ;
	AsmDisasm.protZeile2 ('2 .. 9',	'Anzahl nicht anzuhaltender ASM-Befehle') ;
	AsmDisasm.protZeile2 ('R     ',	'nächsten ASM-Befehl & alle Register zeigen') ;
	AsmDisasm.protZeile2 ('RETURN',	'nächsten ASM-Befehl ausführen') ;
	AsmDisasm.protZeile2 ('O     ',	'Step Over ohne Unterprogramm-Aufruf-Trace') ;
	AsmDisasm.protZeile2 ('1     ',	'wie Trace 1              : Trace komplett ein') ;
	AsmDisasm.protZeile2 ('-     ',	'wie Trace -              : Bildschirmtrace aus') ;
	AsmDisasm.protZeile2 ('X     ',	'wie Trace X              : STD-Trace') ;
	AsmDisasm.protZeile2 ('0     ',	'(Ziffer 0) : wie Trace 0 : Trace komplett aus') ;
	AsmDisasm.protZeile2 ('.     ',	'letzten Break-Befehl noch einmal ausführen') ;
	AsmDisasm.protZeile2 ('ESC   ',	'zurück aus Programmlauf') ;
	Terminal.WriteLn ;
END ProgrammlaufHilfe ;


PROCEDURE WeiterBisBreakpoint (str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	bef,
		pos :	CARDINAL ;
		b :	BOOLEAN ;
BEGIN
	lastBreakpoint := str ;
	IF str [0] = 'C' THEN								(* Memory-Change-Breakpoint *)
		IF GetZahl (str [1..HIGH(str)], ChangeAdresse) THEN
			ChangeWert := GetHalbwort (ChangeAdresse) ;
			debug.Einzelschritt := FALSE ;
			BreakPointAktiv := TRUE ;
			ChangeAktiv := TRUE ;
			RETURN TRUE ;
		END ;
	ELSIF Assembler.enthalten (str, ':') THEN
		Strings.FindNext(':', str, 0, b, pos) ;
		IF GetZahl(str [0..pos-1], TraceBereichVon)
		AND GetZahl (str [pos+1..HIGH(str)], TraceBereichBis) THEN		(* Adress-Bereich *)
			TraceBereichAktiv := TRUE ;
			debug.Einzelschritt := FALSE ;
			debug.Befehle := FALSE ;
			BreakPointAktiv := TRUE ;
			RETURN TRUE ;
		END ;
	ELSIF GetZahl(str, BRKpoint) AND (BRKpoint <> 3EFEFEH) THEN			(* Adress-Breakpoint *)
		debug.Einzelschritt := FALSE ;
		BreakPointAktiv := TRUE ;
		RETURN TRUE ;
	ELSE
		bef :=  AsmDisasm.GetBefehl (str) ;
		IF bef <= 0FFFFFFH THEN
			BRKopcode := bef SHR 16 ;					(* Befehls-Breakpoint *)
			IF debug.an THEN
				TraceF ('Break-Opcode %h', BRKopcode) ;
			END ;
			debug.Einzelschritt := FALSE ;
			BreakPointAktiv := TRUE ;
			RETURN TRUE ;
		END ;
	END ;
	lastBreakpoint := '' ;		(* war fehlerhaft *)
	RETURN FALSE ;
END WeiterBisBreakpoint ;


PROCEDURE Changed () : BOOLEAN ;
BEGIN
	IF ChangeWert <> GetHalbwort (ChangeAdresse) THEN
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END Changed ;


PROCEDURE InitPseudoGebiete () : BOOLEAN ;
	VAR	gebstr,
		parm :		ARRAY [0..63] OF CHAR ;
		ONAME :		ARRAY [0..7] OF CHAR ;
		geb :		pGebiet ;
		index,
		index2,
		anfadr,
		reladr,
		lng,
		gnr,
		i,
		geladen :	CARDINAL ;
		seite :		pSeite ;
		RAM :		pRAMelem ;
BEGIN
	index := 0 ;
	LOOP
		index2 := 0 ;
		gebstr := PseudoGebiete ;
		IF  ExStrings.GetNextItem (PseudoGebiete, index, gebstr, ';') THEN
			 IF ExStrings.GetNextItem (gebstr, index2,			ONAME, ':')
			AND ExStrings.GetNextItem (gebstr, index2, parm, ':')
			AND Conversions.StrBaseToCard (parm, 16,			anfadr)
			AND ExStrings.GetNextItem (gebstr, index2, parm, ':')
			AND Conversions.StrToCard (parm,				lng)
			AND ExStrings.GetNextItem (gebstr, index2, parm, ':')
			AND Conversions.StrToCard (parm,				geladen) THEN
				TraceF ('* OK-Gebiet : %s', gebstr) ;
				gnr := CreateGebiet (ONAME, '', lng, geladen = 0) ;
				geb := GetGebiet (gnr) ;
				RAM := geb ^ .RAM ;
				reladr := anfadr ;
				FOR i := 1 TO lng DO
					seite := GetSeitenAdresse (reladr) ;
					IF seite = NIL THEN				(* Gebiet ist kürzer als deklariert *)
						BREAK ;
					END ;
					RAM ^ .Seite := seite ^ ;			(* Gebietsseite füllen *)
					SpeicherZuordnungWeg (reladr) ;			(* Kachel freigeben *)
					reladr := reladr + (MaxHalbwortInSeite + 1) ;
					RAM := RAM ^ .next ;
				END ;
				geb ^ .Schreibsperre := geladen = 0 ;				(* immer ? *)
				IF (geladen = 1) AND NOT AnmeldeGebiet (gnr, anfadr, '') THEN
					Terminal.WriteF ('+++++ Gebiet nicht anmeldbar : %s', gebstr) ;
					RETURN FALSE ;
				END ;

			ELSE
				Terminal.WriteF ('+++++ Fehler in Gebiet : %s', gebstr) ;
				RETURN FALSE ;
			END ;
		ELSIF gebstr [0] = 0C THEN
			EXIT ;
		END ;
	END ;
	RETURN TRUE ;
END InitPseudoGebiete ;


PROCEDURE CheckXAN ;
	VAR
		pos :		CARDINAL ;
		found :		BOOLEAN ;
		ch :		CHAR ;
BEGIN
%IF TTY %THEN
	IF COManschluss THEN
		TTY.CheckCOM ;
		CharBuffer := TTY.EingabePuffer ;
	ELSIF TTYanschluss THEN
		TTY.CheckTTY ;
		CharBuffer := TTY.EingabePuffer ;
	ELSIF Terminal.EingabeVorhanden THEN
%ELSE
	   IF Terminal.EingabeVorhanden THEN
%END
		WHILE Terminal.CharAvail () DO
			ch := Terminal.ReadChar () ;
			IF (CharIndex = 0) AND (ch <> '#') THEN
					(* irrelevante Eingabe *)
			ELSIF CharIndex < HIGH (CharBuffer) THEN
				CharBuffer [CharIndex] := ch ;
				CharIndex := CharIndex + 1 ;
				CharBuffer [CharIndex] := 0C ;
			END ;
		END ;
	ELSE
		RETURN ;
	END ;

	IF CharBuffer [0] <> 0C THEN

		GrobZaehler := 0 ;

		IF NOT XABgefunden THEN
			ExStrings.FindNextI ('#XAB#.', CharBuffer, 0, found, pos) ;
			IF found THEN
				Terminal.WriteF ('\n#XAB#.\n') ;
				XABgefunden := TRUE ;
				CharIndex := 0 ;
				CharBuffer [0] := 0C ;
%IF TTY %THEN
				TTY.ClearInput ;
%END
				RETURN ;
			END ;
		END ;

		IF AbwicklerUp.ZustandsWahlschalter (AbwicklerUp.ZustWahl4) THEN	(* XAN-Sperre gesetzt *)
					(* #XAN#.  ignorieren *)
			CharIndex := 0 ;
		ELSE
			ExStrings.FindNextI ('#XEN#.', CharBuffer, 0, found, pos) ;
			IF found THEN
				Terminal.WriteF ('\n#XEN#.\n') ;
				AlleProgrammeAbbrechen := TRUE ;
				CharIndex := 0 ;
				CharBuffer [0] := 0C ;
%IF TTY %THEN
				TTY.ClearInput ;
%END
				RETURN ;
			END ;
			ExStrings.FindNextI ('#XAN#.', CharBuffer, 0, found, pos) ;
			IF found THEN
				Terminal.WriteF ('\n#XAN#.\n##ABW#:') ;
				TerminalEingabe.ReadString (CharBuffer) ;
				IF ExStrings.EqualI (CharBuffer [0..5], 'BEENDE') THEN
					globProgrammEnde := TRUE ;
				ELSIF ExStrings.EqualI (CharBuffer [0..3], 'HALT') THEN
					AbwicklerUp.SetZustandsWahlschalter (AbwicklerUp.ZustWahl5, TRUE) ;
				END ;
				CharIndex := 0 ;
				CharBuffer [0] := 0C ;
%IF TTY %THEN
				TTY.ClearInput ;
%END
			END ;
		END ;
		IF CharIndex > 12 THEN
			CharIndex := 0 ;
			CharBuffer [0] := 0C ;
		END ;
	END ;
END CheckXAN ;

%END


PROCEDURE ProgrammLauf (Start : CARDINAL) ;
	VAR	str,
		str2,
		OLname :	ARRAY [0..15] OF CHAR ;
		dateiname,
		zeile :		ARRAY [0..255] OF CHAR ;

		adresse,
		befehl,
		merkFolgeAdresse,
		XANcheckIntervall,
		skipzahl :	CARDINAL ;
		ch :		CHAR ;
		gemerkt :	BOOLEAN ;

	PROCEDURE TraceEinschalten ;
	BEGIN
%IF %NOT WEB %THEN
		IF ProgrammierModus THEN
			debug.Befehle := TRUE ;
			debug.Einzelschritt := TRUE ;
			IF NOT debug.OriginalZeile THEN
				IF ZeilenText (RegF, zeile) THEN
					TraceF ('\t\t\t%s', zeile) ;
				END ;
				debug.OriginalZeile := TRUE ;
			END ;
			debug.RegDump := TRUE ;
			debug.BildschirmProtokoll := TRUE ;
			skipzahl := 0 ;
		END ;
%END
	END TraceEinschalten ;

	PROCEDURE AlarmeNormieren ;
	BEGIN
		AlarmGewesen := FALSE ;
		TKalarmErkannt := FALSE ;
		TKalarmGewesen := FALSE ;
		BUEalarmErkannt := FALSE ;
		BUEalarmGewesen := FALSE ;
		SpeicherschutzAlarmGewesen := FALSE ;
		BefehlsAlarmGewesen := FALSE ;
		UalarmGewesen := FALSE ;
		KBbefehlGewesen := FALSE ;
		MakroAlarm := FALSE ;
		RZSueberlaufGewesen := FALSE ;
	END AlarmeNormieren ;

BEGIN
	AlarmGewesen := FALSE ;
	Assembler.frischAssembliert := FALSE ;

	gemerkt := gemerktesProgramm ;

	IF gemerkt THEN
		OLname := Operatoren [AktOperator].OLN ;
	ELSE
		OLname := lastDateiname ;
	END ;

	InitAssocIndex ;

%IF %NOT WEB %THEN

	SetGrundzustand (FALSE) ;

	TraceF ('*** Programmlauf %s (%s)', lastDateiname, OLname) ;

	IF ProgrammierModus THEN
		IF cmdShowInfo THEN
			Terminal.WriteLn ;
			Terminal.WriteLn ;
			Terminal.WriteString ('Start ') ;
			Terminal.WriteString (OLname) ;
		END ;
	ELSE
		debug.alle (FALSE) ;
	END ;

										AsmTrace.InitTrace (OLname) ;
										AsmTrace.WriteString ('Start ') ;
										AsmTrace.WriteString (OLname) ;
										AsmTrace.WriteLn ;

	IF OKBaktiv THEN
									(* schon alles aus OKB geladen *)
	ELSE
		IF NOT gemerkt THEN
			InitOLK (AktQuellName, AktQuellName) ;		(* Pseudo-OLK installieren *)
		END ;
		IF PseudoGebiete [0] > ' ' THEN
			IF NOT InitPseudoGebiete () THEN		(* Gebiete, die zum Operator gehören, generieren *)
				RETURN ;
			END ;
		END ;
	END ;

	debug.InitRegDump ;

	InitRegZeigen ;

%END

	dateiname := lastDateiname ;

	AlarmeNormieren ;

	globProgrammEnde := FALSE ;
	StepOverAktiv := FALSE ;
	TraceBereichAktiv := FALSE ;

	BRKpoint := 1FFFFFFH ;

	BreakPointAktiv := FALSE ;
	ChangeAktiv := FALSE ;

	IF QuellZeile0 = NIL THEN
		PosTrans := 0 ;
	ELSE
		PosTrans := 50 ;
	END ;

	Anzahl := 0 ;
	GesamtZahlTakte := 0 ;
	InitDauer ;

	GrossSeitenInvarianz := TRUE ;
	RegFbesetzen (Start) ;
	RegX := Assembler.StartX ;
	RegU := Assembler.StartU ;
	skipzahl := 0 ;
	IF NOT debug.BildschirmProtokoll THEN
		debug.Einzelschritt := FALSE ;
	END ;
	GrobZaehler := 0 ;
	zeile := '' ;

	IF Terminal.msPause = 0 THEN
		XANcheckIntervall := 1000 ;
	ELSE
		XANcheckIntervall := 5 ;
	END ;

	LOOP								(* Schleife über alle Befehle *)

%IF %NOT WEB %THEN
		IF debug.OriginalZeile THEN
			IF ZeilenText (RegF, zeile) THEN
				TraceF ('\t\t\t%s', zeile) ;
			ELSE
				zeile := '' ;
			END ;
		ELSE
			zeile := '' ;
		END ;
%END

		befehl := GetBefehlsWort (RegF) ;			(* Befehl aus Speicher holen *)

		adresse := RegF ;

%IF %NOT WEB %THEN
		IF debug.Befehle AND (zeile [0] = 0C) THEN		(* keine Quellzeile da *)
			AsmDisasm.GetMnemo (befehl, zeile) ;
		END ;

		IF Terminal.CtrlBreak OR BreakPointAktiv THEN

			IF StepOverAktiv THEN
				IF (adresse >= StepOverVon) AND (adresse <= StepOverBis) THEN
					TraceEinschalten ;
				END ;
			ELSIF TraceBereichAktiv THEN
				IF (adresse >= TraceBereichVon) AND (adresse <= TraceBereichBis) THEN
					debug.OriginalZeile := FALSE ;
					TraceEinschalten ;
				ELSE
					debug.Befehle := FALSE ;
					debug.Einzelschritt := FALSE ;
					debug.RegDump := FALSE ;
					debug.BildschirmProtokoll := FALSE ;
				END ;
			ELSIF ((adresse = BRKpoint)
				OR ((befehl SHR 16) = BRKopcode))
				OR (ChangeAktiv AND Changed ()) THEN		(* Breakpoint *)
				TraceEinschalten ;
				debug.Einzelschritt := TRUE ;
				Terminal.CtrlBreak := FALSE ;
				BRKpoint := 1FFFFFFH ;
				BRKopcode := 1FFH ;
				ChangeAktiv := FALSE ;
			ELSIF Terminal.CtrlBreak THEN
				TraceEinschalten ;
				Terminal.CtrlBreak := FALSE ;
				BRKpoint := 1FFFFFFH ;
				BRKopcode := 1FFH ;
				ChangeAktiv := FALSE ;
			END ;
		END ;
%END

		INC (Anzahl) ;						(* Anzahl ausgeführter Befehle *)

		IF debug.TakteZeigen THEN
			merkTakte := GesamtZahlTakte ;
		END ;


(***********************************************************************)
		BefehlAusfuehren (befehl) ;
(***********************************************************************)


		IF debug.Befehle OR AlarmGewesen THEN

			IF AlarmGewesen THEN
				IF TKalarmGewesen OR BUEalarmGewesen OR SpeicherschutzAlarmGewesen OR BefehlsAlarmGewesen OR UalarmGewesen THEN
					WITH Operatoren [AktOperator] DO
						IF (AlarmAdresse = 0) OR AlarmSperre THEN
							TraceEinschalten ;
						ELSE
							FolgeAdresse := AlarmAdresse ;
							AlarmSperre := TRUE ;
						END ;
					END ;
				ELSIF KBbefehlGewesen THEN
					TraceEinschalten ;
				ELSIF RZSueberlaufGewesen THEN
					%IF WEB %THEN
						CGIup.WriteF ('\n+++++ Abbruch wegen Rechenzeitschranken-Überlauf +++++\n') ;
					%ELSE
						Terminal.WriteF ('\n+++++ Abbruch wegen Rechenzeitschranken-Überlauf +++++\n') ;
					%END
					globProgrammEnde := TRUE ;
					AlleProgrammeAbbrechen := TRUE ;
				ELSIF AlleProgrammeAbbrechen THEN
					globProgrammEnde := TRUE ;
				END ;


				IF NOT AlleProgrammeAbbrechen THEN
					IF debug.AlarmEin THEN
						TraceEinschalten ;
					END ;
				END ;

				AlarmeNormieren ;

			END ;
%IF %NOT WEB %THEN
			(*
			Terminal.WriteF ("%'04h\t%'06h\t%s", adresse, befehl, zeile) ;
			*)
			CardToHex4 (adresse, str) ;
			CardToHex6 (befehl, str2) ;
			IF debug.BildschirmProtokoll THEN
				Terminal.WriteLn ;
				Terminal.WriteString (str) ;
				Terminal.Write (CHR(9)) ;
				Terminal.WriteString (str2) ;
				Terminal.Write (CHR(9)) ;
                        	Terminal.WriteString (zeile) ;
			END ;
										AsmTrace.WriteLn ;
										AsmTrace.WriteString (str) ;
										AsmTrace.WriteString (CHR(9)) ;
										AsmTrace.WriteString (str2) ;
										AsmTrace.WriteString (CHR(9)) ;
										AsmTrace.WriteString (zeile) ;

			IF debug.RegDump THEN
				RegZeigen ;
			END ;
			IF skipzahl > 0 THEN
				DEC (skipzahl) ;
			ELSIF debug.Einzelschritt THEN

				LOOP
					ch := Terminal.ReadChar () ;
					IF Terminal.AbbruchWunsch THEN
						Terminal.AbbruchWunsch := FALSE ;
						globProgrammEnde := TRUE ;
						EXIT ;
					END ;
					IF MenueBefehlStehtAn THEN
						MenueBefehl ;
					ELSIF ch <> 0C THEN
						EXIT ;
					END ;
				END ;

				Terminal.ClearInput ;		(* nichts vorpuffern *)
				CASE ch OF

				Terminal.Escape :
					EXIT ;
				|
				'?' :	ProgrammlaufHilfe ;
				|
				'2'..'9' :
					skipzahl := ORD(ch) - ORD('0') ;
				|
				'r',
				'R' :	RegAlleZeigen ;
				|
				'o',
				'O' :	CASE VAL (tOpcode, befehl SHR 16) OF
					opSU, opSUE,
					opSFB, opSFBE :
						StepOverAktiv := TRUE ;
						StepOverVon := adresse + 1 ;
						StepOverBis := adresse + 9 ;
						debug.Befehle := FALSE ;
						debug.Einzelschritt := FALSE ;
						debug.OriginalZeile := FALSE ;
						debug.RegDump := FALSE ;
						BreakPointAktiv := TRUE ;
					ELSE
					END ;
				|
				'#' :
					ClearAssocIndex ;
					merkFolgeAdresse := FolgeAdresse ;
					Vorrang ;
					ClearAssocIndex ;
					FolgeAdresse := merkFolgeAdresse ;
					AlarmeNormieren ;
					globProgrammEnde := FALSE ;
				|
				'.' :	IF lastBreakpoint [0] <> 0C THEN
						WeiterBisBreakpoint (lastBreakpoint) ;
					END ;
				|
				'0' :	AsmDisasm.TraceBefehl ('0') ;
				|
				'1' :	AsmDisasm.TraceBefehl ('1') ;
				|
				'-' :	AsmDisasm.TraceBefehl ('-') ;
				|
				'x',
				'X' :	AsmDisasm.TraceBefehl ('X') ;
				ELSE
				END ;
			END ;
		ELSE
			GrobZaehler := GrobZaehler + 1;
			IF GrobZaehler >= XANcheckIntervall THEN
				CheckXAN ;
				GrobZaehler := 0 ;
			END ;

%END

		END ;

		IF globProgrammEnde THEN
			EXIT ;
		END ;

		RegF := FolgeAdresse ;
	END ;

	ClearAssocIndex ;

	lastDateiname := dateiname ;

%IF %NOT WEB %THEN

	IF cmdShowInfo THEN
		Terminal.WriteLn ;							AsmTrace.WriteLn ;
		Terminal.WriteLn ;							AsmTrace.WriteLn ;
			Terminal.WriteString ('Ende ') ;				AsmTrace.WriteString ('Ende ') ;
			Terminal.WriteString (lastDateiname) ;				AsmTrace.WriteString (lastDateiname) ;
		Terminal.WriteLn ;							AsmTrace.WriteLn ;
			Terminal.WriteCard (Anzahl) ;
			Terminal.WriteString (' Befehle') ;

		ProtDauer ;								AsmTrace.ExitTrace ;
	END ;
%END

	KillAlleOLgebiete (AktOLK) ;

	DeleteOperatorlaufDatenbasen ;

	IF NOT gemerkt THEN
		DEC (AktOperator) ;		(* OLK abraeumen *)
	END ;

	TraceF ('*** Programmende %s', OLname) ;

	gemerktesProgramm := FALSE ;

	IF AlleProgrammeAbbrechen THEN
		AlarmGewesen := TRUE ;
	END ;
END ProgrammLauf ;


END Programm.
