
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE DateiBearbeitung;

(*	20.06.18	*)

FROM SYSTEM IMPORT
	ADR, ADDRESS ;

IMPORT FileFunc, FileIO, Conversions, Strings, ExStrings, AbwicklerUp, debug ;

FROM DateiVerwaltung IMPORT
	GENVNRtoZahl, _STDDB, _LFD, _WSP, BKZschreibBerechtigung, DateiSchreibSchutz ;

FROM Struktur IMPORT
	Ganzwort, GetGanzwortU, PutGanzwortU, AlarmGewesen, NullBefehl, AktOLK ;

FROM ZC1 IMPORT
	ANSItoZC1 ;

FROM Trace IMPORT
	TraceF ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;


CONST
	cMagic =	2407 * 4096 ;


TYPE
	tDZ =		(DZundef, DZmaximal, DZgenau, DZungefaehr) ;

	tElement =	(ElemUndef, ElemOktaden, ElemGanz, ElemViertel, ElemAusgabe, ElemGanzOderOktaden, ElemViertelOderOktaden) ;


	tFileBuffer =	POINTER TO ARRAY [0..16383] OF CHAR ;

    	tBuffer =	POINTER TO tBufferRecord ;

	tSatz =		RECORD
				CASE : BOOLEAN OF
				TRUE :
					bytes :		ARRAY [0..65535] OF CHAR ;
				ELSE
					GWe :		ARRAY [0..65535] OF Ganzwort ;
				END ;
			END ;

	tBufferRecord =	RECORD
				Marke :		CARDINAL64 ;
				Laenge :	CARDINAL ;	(* auf Ganzwort-Grenze aufgerundete Bytezahl *)
				next,
				prev :		tBuffer ;
				Inhalt :	tSatz ;		(* dynamische Länge *)
			END ;

	tInfoBlock =	RECORD					(* erster Block für jede vom 'TR440' erzeugte Datei *)
				CASE : BOOLEAN OF
				FALSE :
					block :		ARRAY [0..127] OF CHAR ;
				|
				TRUE :
					Magic :		CARDINAL ;
					Dateilaenge,
					Wortzahl,
					Saetze :	CARDINAL ;
					Dtt :		tDateityp ;
					Dattr :		tDattr ;
					ArtDateilaenge,
					ArtWortzahl :	tDZ ;
					ElementZahl :	CARDINAL ;
					ElementTyp :	tElement ;
					DTB :		ARRAY [0..11] OF CHAR ;
					GENVNR :	CARDINAL ;
					DBN,
					Bkz,
					Exdkz :		ARRAY [0..5] OF CHAR ;
				END ;
			END ;

	tStromRecord =	RECORD
				strom :		CARDINAL ;
				file :		FileFunc.File ;
				filebuffer :	tFileBuffer ;
				AktSatznr :	CARDINAL64 ;
				OLK,					(* Eigentümer *)
				AktSatzIndexGW:	CARDINAL ;
				offen,
				schreibend,
				veraendert :	BOOLEAN ;
				TYP :		tDateityp ;
				BA :		tBetriebsart ;
				firstBuffer,
				lastBuffer,
				aktBuffer :	tBuffer ;
				Info :		tInfoBlock ;
				dateiname :	ARRAY [0..255] OF CHAR ;
			END ;

	tExtName =	ARRAY [0..3] OF CHAR ;


VAR
	initialisiert :	BOOLEAN = FALSE ;

	nextstrmnr :	CARDINAL = 1001 ;		(* wird für jede Dateieröffnung hochgezählt *)

	MaxStromBelegt,
	Numerierung :	CARDINAL ;

	Stroeme :	ARRAY [1..AnzahlStroeme] OF tStromRecord ;

	SatzLaenge :	CARDINAL ;

	MerkInfo :	tInfoBlock ;

	SatzPuffer :	tSatz ;




PROCEDURE NewBuffer (Laenge : CARDINAL) : tBuffer ;
	VAR	buff :	tBuffer ;
BEGIN
	ALLOCATE (buff, SIZE(buff ^ ) - SIZE (buff ^ .Inhalt) + Laenge + 1) ;
	buff ^ .next := NIL ;
	buff ^ .prev := NIL ;
	buff ^ .Laenge := Laenge ;
	RETURN buff ;
END NewBuffer ;


PROCEDURE MakeBuffer (satznr : CARDINAL ; TR440datei : BOOLEAN) : tBuffer ;					(* für das Einfügen / Überschreiben im Speicher *)
	VAR	i, j, k,
		stringlaenge,
		satzlaenge :	CARDINAL ;
		buff :		tBuffer ;
		ch :		CHAR ;
		UTF8 :		BOOLEAN ;

	PROCEDURE put (ch : CHAR) ;
	BEGIN
		SatzPuffer.bytes [k] := ch ;
		k := k + 1 ;
	END put ;

BEGIN
	IF TR440datei THEN											(* Schon Länge in TR440-GW'en vorgegeben *)
		satzlaenge := SatzLaenge * SIZE(Ganzwort) ;
		buff := NewBuffer (satzlaenge) ;
		buff ^ .Inhalt.GWe [0..SatzLaenge-1] := SatzPuffer.GWe [0..SatzLaenge-1] ;
	ELSE													(* direkt aus Windows-Datei gelesen *)
		stringlaenge := LENGTH (SatzPuffer.bytes) ;
		k := 0 ;
		UTF8 := FALSE ;
		FOR i := 1 TO stringlaenge DO
			ch := SatzPuffer.bytes [i-1] ;
			IF UTF8 THEN
				UTF8 := FALSE ;
				CASE ch OF
				0C :
						BREAK ;
				|
				CHR (84H) :	put ('Ä') ;
				|
				CHR (96H) :	put ('Ö') ;
				|
				CHR (9CH) :	put ('Ü') ;
				|
				CHR (0A4H) :	put ('ä') ;
				|
				CHR (0B6H) :	put ('ö') ;
				|
				CHR (0BCH) :	put ('ü') ;
				|
				CHR (9FH) :	put ('ß') ;
				ELSE
						put ('?') ;
				END ;
			ELSE
				CASE ch OF
				0C :
						BREAK ;
				|
				CHR (9) :
						put (' ') ;
				|
				CHR (10),
				CHR (13) :	(* weg *)
				|
				CHR (0C3H) :
						UTF8 := TRUE ;
				ELSE
						put (ch) ;
				END ;
			END ;
		END ;
		stringlaenge := k ;

		FOR i := stringlaenge TO stringlaenge+6 DO
			SatzPuffer.bytes [i] := 0C ;
		END ;

		satzlaenge := (stringlaenge + 6 (* - 1 *)) DIV 6 ;						(* Anzahl GWe auf Ganzwortgrenze aufrunden *)
		buff := NewBuffer (satzlaenge * SIZE(Ganzwort)) ;
		FOR i := 1 TO satzlaenge DO
			WITH buff ^ .Inhalt.GWe [i-1] DO
				TK := 3 ;
				FOR j := 0 TO 5 DO
					ch := SatzPuffer.bytes [(i-1) * 6 + j] ;
					byte6 [j] := ORD (ANSItoZC1 [ch]) ;
				END ;
			END ;
		END ;
		IF (stringlaenge MOD 6) <> 0 THEN
			WITH buff ^ .Inhalt.GWe [satzlaenge-1] DO
				byte6 [5] := stringlaenge MOD 6 ;						(* Oktadenzaehler im letzten GW simulieren *)
			END ;
		END ;
	END ;
	buff ^ .Marke := satznr ;
	RETURN buff ;
END MakeBuffer ;

(*
PROCEDURE MakeSatzPuffer (typ : tDateityp ; buff : tBuffer) ;					(* für das echte Dateischreiben *)
BEGIN
	IF typ >= RANdatei THEN
		Conversions.CardToStr (buff ^ .Marke, SatzPuffer) ;
		Strings.Append (CHR(9), SatzPuffer) ;
		Strings.Append (buff ^ .Inhalt [0 .. buff ^ .Laenge], SatzPuffer) ;
	ELSE
		SatzPuffer := buff ^ .Inhalt [0 .. buff ^ .Laenge] ;
	END ;
END MakeSatzPuffer ;
*)

PROCEDURE DeleteBuffer (VAR buff : tBuffer) ;
BEGIN
	DEALLOCATE (buff, SIZE(buff ^ ) - SIZE (buff ^ .Inhalt) + buff ^ .Laenge + 1) ;
	buff := NIL ;
END DeleteBuffer ;


PROCEDURE ClearBuffers (VAR INOUT buff : tBuffer) ;
	VAR	zwBuff :	tBuffer ;
BEGIN
	WHILE buff <> NIL DO
		zwBuff := buff ;
		buff := buff ^ .next ;
		DeleteBuffer (zwBuff) ;
	END ;
END ClearBuffers ;


PROCEDURE MerkeInfo (DTT : tDateityp ; DL, WZ, D, Z, DATTR, EZ, E : CARDINAL ; bkz, exdkz : ARRAY OF CHAR) ;
BEGIN
	VorbesInfo (MerkInfo) ;
	WITH MerkInfo DO		(* merken für 'SetDateityp' gleich *)
		Magic := cMagic ;
		Dateilaenge := DL ;
		Wortzahl := WZ ;
		Dtt := DTT ;
		Dattr := VAL (tDattr, DATTR) ;
		ArtDateilaenge := VAL (tDZ, D) ;
		ArtWortzahl := VAL (tDZ, Z) ;
		ElementZahl := EZ ;
		ElementTyp := VAL (tElement, E) ;
		Bkz := bkz ;
		Exdkz := exdkz ;
	END ;
END MerkeInfo ;


PROCEDURE GetInfo (VAR dtb : ARRAY OF CHAR ; VAR genvnr, DTT, DL, WZ, D, Z, DATTR, EZ, E, SAETZE : CARDINAL ; VAR bkz, exdkz : ARRAY OF CHAR) ;
BEGIN
	WITH MerkInfo DO		(* kurz vorher mit 'CheckInfoStrmnr' oder 'CheckInfoDatnam' besetzt *)
		IF Magic <> cMagic THEN
			VorbesInfo (MerkInfo) ;
		END ;
		dtb := DTB ;
		genvnr := GENVNR ;
		DL := Dateilaenge ;
		WZ := Wortzahl ;
		DTT := ORD (Dtt) ;
		DATTR := ORD (Dattr) ;
		D := ORD (ArtDateilaenge) ;
		Z := ORD (ArtWortzahl) ;
		EZ := ElementZahl ;
		E := ORD (ElementTyp) ;
		SAETZE := Saetze ;
		bkz := Bkz ;
		exdkz := Exdkz ;
	END ;
END GetInfo ;


PROCEDURE SetMerkDTB (datnam : ARRAY OF CHAR) ;
	VAR	i :		INTEGER ;
		found :		BOOLEAN ;
		pos :		CARDINAL ;
		parts :		FileFunc.FileNameParts ;
BEGIN
	FileFunc.ParseFileName (datnam, parts) ;
	MerkInfo.DTB := parts.name ;

	Strings.FindNext(_LFD, datnam, 0, found, pos) ;
	IF found THEN
		MerkInfo.Dattr := DattrLFD ;
	ELSE
		Strings.FindNext(_WSP, datnam, 0, found, pos) ;
		IF found THEN
			MerkInfo.Dattr := DattrWechselplatte ;
		END ;
	END ;

	MerkInfo.GENVNR := GENVNRtoZahl (parts.extension [1..HIGH(parts.extension)]) ;
END SetMerkDTB ;


PROCEDURE CheckInfoStrmnr (strmnr : CARDINAL) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		VorbesInfo (MerkInfo) ;
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		MerkInfo := Info ;
		SetMerkDTB (dateiname) ;
	END ;
	RETURN TRUE ;
END CheckInfoStrmnr ;


PROCEDURE CheckInfoDatnam (datnam : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i :	CARDINAL ;
		datei :	FileFunc.File ;
BEGIN
	FOR i := 1 TO AnzahlStroeme DO
		WITH Stroeme [i] DO
			IF offen THEN		(* belegtes Element *)
				IF ExStrings.EqualI (datnam, dateiname) THEN	(* hier ist die Datei offen *)
					MerkInfo := Info ;
					SetMerkDTB (datnam) ;
					IF debug.Dateien THEN
						TraceF ('Dateiinfo offen : %s', dateiname) ;
					END ;
					RETURN TRUE ;
				END ;
			END ;
		END ;
	END ;
	FileFunc.OpenFile (datei, datnam, FileFunc.ReadOnlyDenyNone) ;
	IF datei.status = 0 THEN
		FileIO.ReadBytes (datei, MerkInfo) ;
		IF (datei.status = 0) AND (MerkInfo.Magic = cMagic) THEN	(* vom 'TR440' erzeugt *)
			FileFunc.CloseFile (datei) ;
			SetMerkDTB (datnam) ;
			IF debug.Dateien THEN
				TraceF ('Dateiinfo TR440 : %s', datnam) ;
			END ;
			RETURN TRUE ;
		END ;
		FileFunc.CloseFile (datei) ;
		VorbesInfo (MerkInfo) ;						(* sonst Textdatei simulieren *)
		MerkInfo.Saetze := 1 ;		(* auf jeden Fall nicht 0 *)
		SetMerkDTB (datnam) ;
		IF debug.Dateien THEN
			TraceF ('Dateiinfo sonst : %s', datnam) ;
		END ;
		RETURN TRUE ;
	END ;
	VorbesInfo (MerkInfo) ;
	SetMerkDTB (datnam) ;
	IF debug.Dateien THEN
		TraceF ('Dateiinfo fehlt : %s', datnam) ;
	END ;
	RETURN FALSE ;
END CheckInfoDatnam ;



PROCEDURE SetDateityp (VAR datei : FileFunc.File) ;
BEGIN					(* Info vom 'MerkeInfo' schreiben *)
	FileIO.WriteBlock (datei, MerkInfo, SIZE(MerkInfo)) ;
END SetDateityp ;


PROCEDURE VorbesInfo (VAR Info : tInfoBlock) ;
	VAR	i :	CARDINAL ;
BEGIN
	WITH Info DO
		FOR i := 0 TO HIGH(block) DO
			block [i] := 0C ;
		END ;
		Magic := cMagic ;
		Dateilaenge := 128 ;
		Wortzahl := 21 ;
		Dtt := RAMdatei ;
		Dattr := DattrTrommel ;
		ArtDateilaenge := DZungefaehr ;
		ArtWortzahl := DZungefaehr ;
		ElementZahl := 160 ;
		ElementTyp := ElemOktaden ;
		DBN := _STDDB ;
(*
		Saetze := 0 ;
		GENVNR := 0 ;
		FOR i := 0 TO HIGH(DTB) DO
			DTB [i] := 0C ;
		END ;
		FOR i := 0 TO HIGH(DBN) DO
			DBN [i] := 0C ;
			Bkz [i] := 0C ;
			Exdkz [i] := 0C ;
		END ;
*)
	END ;
END VorbesInfo ;


PROCEDURE DateiOeffnen (datnam : ARRAY OF CHAR ; zumSchreiben : BOOLEAN) (*strmnr : *) : CARDINAL ;
	VAR	i :		CARDINAL ;
		TR440datei :	BOOLEAN ;
BEGIN
	INC (nextstrmnr) ;
	FOR i := 1 TO AnzahlStroeme DO
		WITH Stroeme [i] DO
			IF NOT offen THEN		(* freies Element *)
				strom := nextstrmnr ;
				IF i > MaxStromBelegt THEN
					MaxStromBelegt := i ;
				END ;
				veraendert := FALSE ;
				dateiname := datnam ;
				firstBuffer := NIL ;
				lastBuffer := NIL ;
				aktBuffer := NIL ;
				AktSatzIndexGW := 0 ;
				IF zumSchreiben THEN
					IF DateiSchreibSchutz (dateiname) THEN
						RETURN 0 ;
					END ;
					IF debug.Dateien THEN
						TraceF ('schreibend öffnen : %s', dateiname) ;
					END ;
					FileFunc.OpenFile (file, dateiname, FileFunc.ReadWriteDenyAll) ;
					schreibend := TRUE ;
					BA := BAschreibend ;
				ELSE
					FileFunc.OpenFile (file, dateiname, FileFunc.ReadOnlyDenyWrite) ;
					schreibend := FALSE ;
					BA := BAvorwaerts ;
				END ;
				IF file.status = 0 THEN
					offen := TRUE ;		(* belegen *)
					OLK := AktOLK ;		(* Eigentümer merken *)
					IF filebuffer = NIL THEN
						NEW (filebuffer) ;
					END ;
					FileFunc.SetFileBuffer (file, filebuffer ^ ) ;

					FileFunc.SetFilePos (file, 0) ;
					FileIO.ReadBlock (file, Info, SIZE(Info)) ;
					IF (file.status = 0) AND (Info.Magic = cMagic) THEN	(* vom 'TR440' erzeugt *)
						TR440datei := TRUE ;
					ELSIF nurTR440dateien THEN
						offen := FALSE ;
						FileFunc.CloseFile (file) ;
						RETURN 0 ;
					ELSE
						TR440datei := FALSE ;
						VorbesInfo (Info) ;
						Info.Saetze := 1 ;
						FileFunc.SetFilePos (file, 0) ;
					END ;

					TYP := Info.Dtt ;

					CASE TYP OF
					SEQdatei :
							SEQdateiEinlesen (nextstrmnr, TR440datei) ;
					|
					RANdatei :
							RANdateiEinlesen (nextstrmnr, TR440datei) ;
					|
					RAMdatei :
							RAMdateiEinlesen (nextstrmnr, TR440datei) ;
					ELSE
					END ;
					RETURN nextstrmnr ;
				END ;
				RETURN 0 ;
			END ;
		END ;
	END ;
	RETURN 0 ;	(* misslungen *)
END DateiOeffnen ;


PROCEDURE GetStrom (extstrmnr : CARDINAL) : CARDINAL ;
	VAR	s :	CARDINAL ;
BEGIN
	FOR s := 1 TO MaxStromBelegt DO
		WITH Stroeme [s] DO
			IF offen AND (extstrmnr = strom) THEN	(* gefunden *)
				RETURN s ;
			END ;
		END ;
	END ;
	AbwicklerUp.SSRfehler (45H, 'Dateibearbeitung nicht eröffnet') ;
	RETURN 0 ;
END GetStrom ;


PROCEDURE GetDateityp (strmnr : CARDINAL) : tDateityp ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN undefDatei ;
	END ;
	WITH Stroeme [s] DO
		RETURN TYP ;
	END ;
END GetDateityp ;


PROCEDURE GetDATTR (strmnr : CARDINAL) : tDattr ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN DattrMagnetband ;		(* MB als unzul. DATTR ausliefern *)
	END ;
	WITH Stroeme [s] DO
		RETURN Info.Dattr ;
	END ;
END GetDATTR ;


PROCEDURE SatzSuchen (strmnr : CARDINAL ; satznr : CARDINAL64) : BOOLEAN ;		(* aktBuffer zeigt danach auf den Satz oder auf Satz vorher, wenn nicht existent *)
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF aktBuffer = NIL THEN
			aktBuffer := firstBuffer ;
			IF aktBuffer = NIL THEN
				RETURN FALSE ;
			END ;
		END ;
		IF satznr >= lastBuffer ^ .Marke THEN
			aktBuffer := lastBuffer ;
		ELSIF satznr <= firstBuffer ^ .Marke THEN
			aktBuffer := firstBuffer ;
		END ;
		WHILE satznr > aktBuffer ^ .Marke DO			(* vorwärts suchen *)
			aktBuffer := aktBuffer ^ .next ;
			IF aktBuffer = NIL THEN
				aktBuffer := lastBuffer ;
				RETURN FALSE ;
			END ;
		END ;
		WHILE satznr < aktBuffer ^ .Marke DO			(* rückwärts suchen *)
			aktBuffer := aktBuffer ^ .prev ;
			IF aktBuffer = NIL THEN
				aktBuffer := firstBuffer ;
				RETURN FALSE ;
			END ;
		END ;
		IF satznr <> aktBuffer ^ .Marke THEN
			RETURN FALSE ;
		END ;
		RETURN TRUE ;
	END ;
END SatzSuchen ;


PROCEDURE LueckeSuchen (strmnr : CARDINAL ; anzahl : CARDINAL) (* erster freier Satz mit Platz *) : CARDINAL64 ;
	VAR	s :		CARDINAL ;
		freiMarke :	CARDINAL64 ;
		buff :		tBuffer ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	freiMarke := 0 ;
	WITH Stroeme [s] DO
		buff := firstBuffer ;
		WHILE buff <> NIL DO
			freiMarke := buff ^ .Marke + 1 ;
			IF buff ^ .next <> NIL THEN
				IF (buff ^ .next ^ .Marke - freiMarke) >= VAL (CARDINAL64, anzahl) THEN
					RETURN freiMarke ;		(* Lücke gefunden *)
				END ;
			END ;
			buff := buff ^ .next ;
		END ;
	END ;
	RETURN freiMarke ;						(* Lücke erst am Ende der Datei *)
END LueckeSuchen ;


PROCEDURE SatzEinfuegen (strmnr : CARDINAL ; satznr : CARDINAL64 ; buff : tBuffer) ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN ;
	END ;
	WITH Stroeme [s] DO
		IF firstBuffer = NIL THEN				(* 1. Satz für leere Datei *)
			firstBuffer := buff ;
			lastBuffer := buff ;
		ELSIF satznr > lastBuffer ^ .Marke THEN			(* anhängen *)
			lastBuffer ^ .next := buff ;
			buff ^ .prev := lastBuffer ;
			lastBuffer := buff ;
		ELSIF satznr < firstBuffer ^ .Marke THEN		(* vor gesamte Datei hängen *)
			buff ^ .next := firstBuffer ;
			firstBuffer ^ .prev := buff ;
			firstBuffer := buff ;
		ELSE							(* irgendwo dazwischen kleben *)
			IF aktBuffer = NIL THEN
				aktBuffer := lastBuffer ;
			END ;
			WHILE satznr > aktBuffer ^ .Marke DO
				aktBuffer := aktBuffer ^ .next ;
			END ;
			WHILE satznr < aktBuffer ^ .Marke DO
				aktBuffer := aktBuffer ^ .prev ;	(* nun direkt hinter aktBuffer einfügen *)
			END ;
			buff ^ .prev := aktBuffer ;
			buff ^ .next := aktBuffer ^ .next ;
			IF buff ^ .next <> NIL THEN
				buff ^ .next ^ .prev := buff ;
			END ;
			aktBuffer ^ .next := buff ;
		END ;
		aktBuffer := buff ;
		AktSatznr := satznr ;
	END ;
END SatzEinfuegen ;



PROCEDURE SEQdateiEinlesen (strmnr : CARDINAL; TR440datei : BOOLEAN) : BOOLEAN ;
	VAR	satznr :	CARDINAL64 ;
		satzlaenge,
		i :		CARDINAL ;
		buff :		tBuffer ;
		s :		CARDINAL ;
		lng :		CARDINAL16 ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		TYP := SEQdatei ;
		satznr := 0 ;
		LOOP
			IF TR440datei THEN
				IF NOT FileIO.ReadBlock (file,lng, 2) THEN		(* 2 Bytes binäre Satzlänge *)
					EXIT ;
				END ;
				SatzLaenge := ORD (lng) ;
				IF NOT FileIO.ReadBlock (file, SatzPuffer, SatzLaenge * SIZE(Ganzwort)) THEN
					EXIT ;
				END ;
			ELSE
				satzlaenge := FileFunc.ReadLine (file, SatzPuffer.bytes) ;
				IF file.eof THEN
					EXIT ;
				END ;
			END ;
			INC (satznr) ;
			buff := MakeBuffer (satznr, TR440datei) ;
			IF firstBuffer = NIL THEN
				firstBuffer := buff ;
			ELSE
				lastBuffer ^ .next := buff ;
				buff ^ .prev := lastBuffer ;
			END ;
			lastBuffer := buff ;
		END ;
		aktBuffer := firstBuffer ;
		Info.Saetze := satznr ;
	END ;
	RETURN TRUE ;
END SEQdateiEinlesen ;


PROCEDURE upRAMeinlesen (strmnr : CARDINAL; TR440datei : BOOLEAN) : BOOLEAN ;
	VAR	satznr :	CARDINAL64 ;
		satzanzahl,
		satzlaenge :	CARDINAL ;
		buff :		tBuffer ;
		s :		CARDINAL ;
		reserve,
		lng :		CARDINAL16 ;

	PROCEDURE GetMarkeAusSatz ;
		VAR	i :	CARDINAL ;
			nr :	CARDINAL64 ;
	BEGIN
		nr := 0 ;
		i := 0 ;
		WHILE (SatzPuffer.bytes [i] >= '0') AND (SatzPuffer.bytes [i] <= '9') DO
			nr := nr * 10 + VAL (CARDINAL64, ORD (SatzPuffer.bytes [i]) - ORD ('0')) ;
			INC (i) ;
		END ;
		IF (SatzPuffer.bytes [i] = CHR(9)) AND (nr > satznr) THEN
			satznr := nr ;
			SatzPuffer.bytes [0 .. satzlaenge-i-1] := SatzPuffer.bytes [i+1 .. satzlaenge] ;
			satzlaenge := satzlaenge-i-1 ;
			SatzPuffer.bytes [satzlaenge] := 0C ;
		END ;
	END GetMarkeAusSatz ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	satzanzahl := 0 ;
	WITH Stroeme [s] DO
		satznr := 0 ;
		LOOP
			IF TR440datei THEN
				IF NOT FileIO.ReadBlock (file, lng, 2) THEN
					EXIT ;
				END ;
				INC (satzanzahl) ;
				SatzLaenge := ORD (lng) ;
				IF NOT FileIO.ReadBlock (file, satznr , 4) THEN									(* 4 Bytes binäre Satznummer *)
					EXIT ;
				END ;
				IF TYP = RAMdatei THEN
					IF NOT FileIO.ReadBlock (file, reserve, 2) THEN								(* Vorbereitung für 2 weitere Bytes binäre Satznummer bei RAM *)
						EXIT ;
					END ;
				END ;
				IF NOT FileIO.ReadBlock (file, SatzPuffer , SatzLaenge * SIZE(Ganzwort)) THEN					(* binärer Satzinhalt *)
					EXIT ;
				END ;
			ELSE
				INC (satzanzahl) ;
				satzlaenge := FileFunc.ReadLine (file, SatzPuffer.bytes) ;
				INC (satznr, Numerierung) ;
				CASE SatzPuffer.bytes [0] OF
				'0' .. '9' :	GetMarkeAusSatz () ;
				ELSE
				END ;
				IF file.eof THEN
					EXIT ;
				END ;
			END ;
			buff := MakeBuffer (satznr, TR440datei) ;
			SatzEinfuegen (strmnr, satznr, buff) ;
		END ;
		aktBuffer := firstBuffer ;
		Info.Saetze := satzanzahl ;
	END ;
	RETURN TRUE ;
END upRAMeinlesen ;


PROCEDURE RANdateiEinlesen (strmnr : CARDINAL; TR440datei : BOOLEAN) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		TYP := RANdatei ;
		Numerierung := 1 ;
	END ;
	RETURN upRAMeinlesen (strmnr, TR440datei) ;
END RANdateiEinlesen ;


PROCEDURE RAMdateiEinlesen (strmnr : CARDINAL; TR440datei : BOOLEAN) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		TYP := RAMdatei ;
		Numerierung := 1 ;
	END ;
	RETURN upRAMeinlesen (strmnr, TR440datei) ;
END RAMdateiEinlesen ;

(*
PROCEDURE SatzLesenVorwaerts (strmnr : CARDINAL ; VAR INOUT satznr : CARDINAL ; VAR puffer : ARRAY OF CHAR) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		BA := BAvorwaerts ;
		IF NOT SatzSuchen (strmnr, satznr) THEN
			IF aktBuffer = NIL THEN
				RETURN FALSE ;
			END ;
			aktBuffer := aktBuffer ^ .next ;
			IF aktBuffer = NIL THEN
				RETURN FALSE ;
			END ;
			satznr := aktBuffer ^ .Marke ;
		END ;
		puffer := aktBuffer ^ .Inhalt.bytes [0..aktBuffer ^ .Laenge] ;
		AktSatznr := satznr ;
	END ;
	RETURN TRUE ;
END SatzLesenVorwaerts ;


PROCEDURE SatzLesenRueckwaerts (strmnr : CARDINAL ; VAR INOUT satznr : CARDINAL ; VAR puffer : ARRAY OF CHAR) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		BA := BArueckwaerts ;
		IF NOT SatzSuchen (strmnr, satznr) THEN
			IF aktBuffer = NIL THEN
				RETURN FALSE ;
			END ;
			aktBuffer := aktBuffer ^ .prev ;
			IF aktBuffer = NIL THEN
				RETURN FALSE ;
			END ;
			satznr := aktBuffer ^ .Marke ;
		END ;
		puffer := aktBuffer ^ .Inhalt [0..aktBuffer ^ .Laenge] ;
		AktSatznr := satznr ;
	END ;
	RETURN TRUE ;
END SatzLesenRueckwaerts ;
*)

PROCEDURE letzterSatz (strmnr : CARDINAL) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		RETURN AktSatznr >= lastBuffer ^ .Marke ;
	END ;
END letzterSatz ;



PROCEDURE GetDateiBuffer (strmnr : CARDINAL ; VAR SatzAdresse : ADDRESS ; VAR SatzLaenge : CARDINAL ; satznr : CARDINAL64) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF aktBuffer = NIL THEN
			RETURN FALSE ;
		END ;
		SatzAdresse := ADR (aktBuffer ^ .Inhalt) ;
		SatzLaenge := aktBuffer ^ .Laenge ;
		satznr := aktBuffer ^ .Marke ;
	END ;
	RETURN TRUE ;
END GetDateiBuffer ;


PROCEDURE SetPosition (strmnr : CARDINAL ; satznr : CARDINAL64) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		AktSatzIndexGW := 0 ;
		IF satznr = 0 THEN
			aktBuffer := firstBuffer ;
			IF aktBuffer = NIL THEN
				AktSatznr := 1 ;
			ELSE
				AktSatznr := aktBuffer ^ .Marke ;
			END ;
		ELSE
			AktSatznr := satznr ;
			IF NOT SatzSuchen (strmnr, AktSatznr) THEN
				RETURN FALSE ;
			END ;
		END ;
	END ;
	RETURN TRUE ;
END SetPosition ;


PROCEDURE SetPositionRelativ (strmnr : CARDINAL ; relativ : INTEGER) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO

		IF aktBuffer = NIL THEN
			SatzSuchen (strmnr, AktSatznr) ;
		END ;
		IF aktBuffer = NIL THEN
			RETURN FALSE ;
		END ;
		IF (relativ > 0) AND (aktBuffer = firstBuffer) AND (AktSatznr < aktBuffer ^ .Marke) THEN
			DEC (relativ) ;
		END ;
		AktSatzIndexGW := 0 ;
		WHILE relativ > 0 DO
			aktBuffer := aktBuffer ^ .next ;
			IF aktBuffer = NIL THEN
				aktBuffer := lastBuffer ;
				AktSatznr := AktSatznr + 1 ;
				RETURN FALSE ;
			END ;
			DEC (relativ) ;
		END ;
		WHILE relativ < 0 DO
			aktBuffer := aktBuffer ^ .prev ;
			IF aktBuffer = NIL THEN
				aktBuffer := firstBuffer ;
				AktSatznr := 1 ;
				RETURN FALSE ;
			END ;
			INC (relativ) ;
		END ;
		AktSatznr := aktBuffer ^ .Marke ;
	END ;
	RETURN TRUE ;
END SetPositionRelativ ;


PROCEDURE GetErsteSatznr (strmnr : CARDINAL) : CARDINAL64 ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	WITH Stroeme [s] DO
		IF firstBuffer = NIL THEN
			RETURN 0 ;
		END ;
		RETURN firstBuffer ^ .Marke ;
	END ;
END GetErsteSatznr ;


PROCEDURE GetLetzteSatznr (strmnr : CARDINAL) : CARDINAL64 ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	WITH Stroeme [s] DO
		IF lastBuffer = NIL THEN
			RETURN 0 ;
		END ;
		RETURN lastBuffer ^ .Marke ;
	END ;
END GetLetzteSatznr ;


PROCEDURE GetPosition (strmnr : CARDINAL) : CARDINAL64 ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	WITH Stroeme [s] DO
		RETURN AktSatznr ;
	END ;
END GetPosition ;


PROCEDURE SetSchreibend (strmnr : CARDINAL) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF NOT schreibend THEN
			IF NOT AbwicklerUp.checkOEFDB (Info.DBN) THEN
				RETURN FALSE ;
			END ;
			IF DateiSchreibSchutz (dateiname) THEN
				RETURN FALSE ;
			END ;
			IF debug.Dateien THEN
				TraceF ('set schreiben : %s', dateiname) ;
			END ;
			FileFunc.CloseFile (file) ;
			FileFunc.OpenFile (file, dateiname, FileFunc.ReadWriteDenyAll) ;
			IF file.status <> 0 THEN
				RETURN FALSE ;
			END ;
			FileFunc.SetFileBuffer (file, filebuffer ^ ) ;
			schreibend := TRUE ;
		END ;
		veraendert := TRUE ;
		BA := BAschreibend ;
	END ;
	RETURN TRUE ;
END SetSchreibend ;


PROCEDURE SatzLoeschen (strmnr : CARDINAL ; satznr : CARDINAL64) : BOOLEAN ;
	VAR	buff :	tBuffer ;
		s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF NOT SetSchreibend (strmnr) THEN
			RETURN FALSE ;
		END ;
		NullBefehl ;
		IF SatzSuchen (strmnr, satznr) THEN
			buff := aktBuffer ;
			aktBuffer := buff ^ .next ;
			(*
			IF aktBuffer = NIL THEN
				aktBuffer := buff ^ .prev ;
			END ;
			*)
			IF buff ^ .prev = NIL THEN			(* 1. Satz wird gelöscht *)
				firstBuffer := buff ^ .next ;
				IF firstBuffer = NIL THEN
					lastBuffer := NIL ;		(* Datei wird nun ganz leer *)
				ELSE
					firstBuffer ^ .prev := NIL ;
				END ;
			ELSE
				buff ^ .prev ^ .next := buff ^ .next ;
			END ;
			IF buff ^ .next = NIL THEN			(* letzter Satz wird gelöscht *)
				lastBuffer := buff ^ .prev ;
				IF lastBuffer <> NIL THEN
					lastBuffer ^ .next := NIL ;
				END ;
			ELSE
				buff ^ .next ^ .prev := buff ^ .prev ;
			END ;
			DeleteBuffer (buff) ;
		END ;
		IF aktBuffer = NIL THEN
			AktSatznr := 0 ;
		ELSE
			AktSatznr := aktBuffer ^ .Marke ;
		END ;
	END ;
	RETURN TRUE ;
END SatzLoeschen ;


PROCEDURE SatzLesen (strmnr : CARDINAL ; satznr : CARDINAL64 ; VAR puffer : ARRAY OF CHAR ; VAR Laenge : CARDINAL) : BOOLEAN ;		(* exakt oder gar keinen Satz	*)
	VAR	s,
		trans :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF satznr = 0 THEN
			satznr := AktSatznr ;
		END ;
		IF NOT SatzSuchen (strmnr, satznr) THEN
			RETURN FALSE ;
		END ;
		Laenge := aktBuffer ^ .Laenge ;
		IF AktSatzIndexGW = 0 THEN
			IF Laenge > HIGH(puffer) THEN
				Laenge := HIGH(puffer)+1 ;
			END ;
			puffer [0..Laenge-1] := aktBuffer ^ .Inhalt.bytes [0..Laenge-1] ;
		ELSE
			trans := AktSatzIndexGW * SIZE(Ganzwort) ;
			Laenge := Laenge - trans ;
			IF Laenge > HIGH(puffer) THEN
				Laenge := HIGH(puffer)+1 ;
			END ;
			puffer [0..Laenge-1] := aktBuffer ^ .Inhalt.bytes [trans..Laenge-1 + trans] ;
		END ;
		AktSatznr := satznr ;
		AktSatzIndexGW := AktSatzIndexGW + Laenge DIV SIZE(Ganzwort) ;
	END ;
	RETURN TRUE ;
END SatzLesen ;


PROCEDURE SatzSchreiben (strmnr : CARDINAL ; satznr : CARDINAL64 ; puffer : ARRAY OF CHAR) : BOOLEAN ;	(* nur im Speicher *)
	VAR	buff :	tBuffer ;
		laenge,
		s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF NOT SetSchreibend (strmnr) THEN
			RETURN FALSE ;
		END ;
		SatzLaenge := (HIGH(puffer)+1+5) DIV SIZE(Ganzwort) ;
		IF (aktBuffer = NIL) OR (AktSatzIndexGW = 0) THEN
			SatzPuffer.bytes [0 .. SatzLaenge * SIZE(Ganzwort) - 1] := puffer [0..SatzLaenge * SIZE(Ganzwort) - 1] ;
		ELSE										(* Satz fortschreiben *)
			SatzPuffer.bytes [0 .. (AktSatzIndexGW-1)*SIZE(Ganzwort)] := aktBuffer ^ .Inhalt.bytes [0..(AktSatzIndexGW-1)*SIZE(Ganzwort)] ;
			SatzPuffer.bytes [AktSatzIndexGW*SIZE(Ganzwort) .. AktSatzIndexGW*SIZE(Ganzwort) + SatzLaenge * SIZE(Ganzwort) - 1] := puffer [0..SatzLaenge * SIZE(Ganzwort) - 1] ;
			SatzLaenge := SatzLaenge + AktSatzIndexGW ;
		END ;
		buff := MakeBuffer (satznr, TRUE) ;
		SatzLoeschen (strmnr, satznr) ;				(* alten Satz löschen, falls schon existent *)
		SatzEinfuegen (strmnr, satznr, buff) ;
		AktSatzIndexGW := aktBuffer ^ .Laenge DIV SIZE(Ganzwort) ;
	END ;
	RETURN TRUE ;
END SatzSchreiben ;


PROCEDURE GetAktSatzIndexGW (strmnr : CARDINAL) : CARDINAL ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	WITH Stroeme [s] DO
		RETURN AktSatzIndexGW ;
	END ;
END GetAktSatzIndexGW ;


PROCEDURE SetAktSatzIndexGW (strmnr, satzindex : CARDINAL) ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN  ;
	END ;
	WITH Stroeme [s] DO
		AktSatzIndexGW := satzindex ;
	END ;
END SetAktSatzIndexGW ;


PROCEDURE GetAktSatzLaengeGW (strmnr : CARDINAL) : CARDINAL ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN 0 ;
	END ;
	WITH Stroeme [s] DO
		IF aktBuffer <> NIL THEN
			RETURN aktBuffer ^ .Laenge DIV SIZE(Ganzwort) ;
		END ;
	END ;
	RETURN 0 ;
END GetAktSatzLaengeGW ;


PROCEDURE upDateiSchreiben (strmnr : CARDINAL ; typ : tDateityp) : BOOLEAN ;			(* echtes Schreiben *)
	VAR	ok :		BOOLEAN ;
		s :		CARDINAL ;
		reserve,
		lng :		CARDINAL16 ;
		saetze :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		reserve := 0 ;
		saetze := 0 ;
		aktBuffer := firstBuffer ;
		WHILE aktBuffer <> NIL DO
			saetze := saetze + 1 ;
			aktBuffer := aktBuffer ^ .next ;
		END ;
		Info.Saetze := saetze ;

		aktBuffer := firstBuffer ;
		ok := TRUE ;
		FileFunc.SetFilePos (file, 0) ;
		Info.Magic := cMagic ;
		FileIO.WriteBlock (file, Info , SIZE(Info)) ;
		WHILE aktBuffer <> NIL DO
			lng := aktBuffer ^ .Laenge DIV SIZE(Ganzwort) ;
			FileIO.WriteBlock (file, lng, 2) ;
			IF file.status <> 0 THEN
				ok := FALSE ;
				BREAK ;
			END ;
			IF typ <> SEQdatei THEN
				FileIO.WriteBlock (file,aktBuffer ^ .Marke, 4) ;						(* 4 Bytes binäre Satzmarke *)
				IF file.status <> 0 THEN
					ok := FALSE ;
					BREAK ;
				END ;
				IF typ = RAMdatei THEN
					FileIO.WriteBlock (file, reserve , 2) ;					(* Vorbereitung für 6 Bytes Satzmarke bei RAM *)
					IF file.status <> 0 THEN
						ok := FALSE ;
						BREAK ;
					END ;
				END ;
			END ;
			FileIO.WriteBlock (file, aktBuffer ^ .Inhalt, aktBuffer ^ .Laenge) ;
			IF file.status <> 0 THEN
				ok := FALSE ;
				BREAK ;
			END ;
			aktBuffer := aktBuffer ^ .next ;
		END ;
		FileFunc.TruncateFile (file) ;
	END ;
	RETURN ok ;
END upDateiSchreiben ;


PROCEDURE GetDateiBetriebsart (strmnr : CARDINAL) : tBetriebsart ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN BAundef ;
	END ;
	WITH Stroeme [s] DO
		RETURN BA ;
	END ;
END GetDateiBetriebsart ;


PROCEDURE SetDateiBetriebsart (strmnr : CARDINAL ; Betriebsart : tBetriebsart) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF Betriebsart = BAschreibend THEN
			RETURN SetSchreibend (strmnr) ;
		END ;
		BA := Betriebsart ;
	END ;
	RETURN TRUE ;
END SetDateiBetriebsart ;


PROCEDURE SetDBN (strmnr : CARDINAL ; DBN : ARRAY OF CHAR) ;
	VAR	s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s <> 0 THEN
		Stroeme [s].Info.DBN := DBN ;
	END ;
END SetDBN ;


PROCEDURE PHYSlesen (strmnr, blocknr, AAOP, ANZ : CARDINAL) : BOOLEAN ;
	VAR	s,
		i,
		j :	CARDINAL ;
		merk :	BOOLEAN ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	merk := debug.Speichern ;
	debug.Speichern := FALSE ;
	WITH Stroeme [s] DO
		FileFunc.SetFilePos (file, blocknr * 128 * SIZE(Ganzwort) + SIZE (tInfoBlock)) ;
		FOR i := 1 TO ANZ DO
			IF NOT FileIO.ReadBlock (file, SatzPuffer, 128 * SIZE (Ganzwort)) THEN
				RETURN FALSE ;
			END ;
			FOR j := 0 TO 127 DO
				PutGanzwortU (AAOP, SatzPuffer.GWe [j]) ;
				IF AlarmGewesen THEN
					debug.Speichern := merk ;
					RETURN FALSE ;
				END ;
				AAOP := AAOP + 2 ;
			END ;
		END ;
	END ;
	debug.Speichern := merk ;
	RETURN TRUE ;
END PHYSlesen ;


PROCEDURE PHYSschreiben (strmnr, blocknr, AAOP, ANZ : CARDINAL) : BOOLEAN ;
	VAR	s,
		i,
		j :	CARDINAL ;
		merk :	BOOLEAN ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	WITH Stroeme [s] DO
		FileFunc.SetFilePos (file, blocknr * 128 * SIZE(Ganzwort) + SIZE (tInfoBlock)) ;
		FOR i := 1 TO ANZ DO
			FOR j := 0 TO 127 DO
				GetGanzwortU (AAOP, SatzPuffer.GWe [j]) ;
				IF AlarmGewesen THEN
					debug.Lesen := merk ;
					RETURN FALSE ;
				END ;
				AAOP := AAOP + 2 ;
			END ;
			FileIO.WriteBlock (file, SatzPuffer, 128 * SIZE (Ganzwort)) ;
		END ;
	END ;
	debug.Lesen := merk ;
	RETURN TRUE ;
END PHYSschreiben ;


PROCEDURE SEQdateiSchreiben (strmnr : CARDINAL) : BOOLEAN ;
BEGIN
	RETURN upDateiSchreiben (strmnr, SEQdatei) ;
END SEQdateiSchreiben ;


PROCEDURE RANdateiSchreiben (strmnr : CARDINAL) : BOOLEAN ;
BEGIN
	RETURN upDateiSchreiben (strmnr, RANdatei) ;
END RANdateiSchreiben ;


PROCEDURE RAMdateiSchreiben (strmnr : CARDINAL) : BOOLEAN ;
BEGIN
	RETURN upDateiSchreiben (strmnr, RAMdatei) ;
END RAMdateiSchreiben ;


PROCEDURE DateiSchliessen (strmnr : CARDINAL) : BOOLEAN ;
	VAR	ok :	BOOLEAN ;
		i,
		s :	CARDINAL ;
BEGIN
	s := GetStrom (strmnr) ;
	IF s = 0 THEN
		RETURN FALSE ;
	END ;
	WITH Stroeme [s] DO
		IF veraendert THEN
			CASE TYP OF
			SEQdatei :	ok := SEQdateiSchreiben (strmnr) ;
			|
			RANdatei :	ok := RANdateiSchreiben (strmnr) ;
			|
			RAMdatei :	ok := RAMdateiSchreiben (strmnr) ;
			ELSE
			END ;
		END ;
		FileFunc.CloseFile (file) ;
		ClearBuffers (firstBuffer) ;
		offen := FALSE ;
	END ;
	IF s = MaxStromBelegt THEN
		REPEAT
			DEC (MaxStromBelegt) ;
		UNTIL (MaxStromBelegt < 1) OR Stroeme [MaxStromBelegt].offen ;
	END ;
	RETURN ok ;
END DateiSchliessen ;


PROCEDURE AlleEigenenDateienSchliessen ;
	VAR	i :	CARDINAL ;
BEGIN
	IF AktOLK <> 0 THEN
		FOR i := 1 TO MaxStromBelegt DO
			WITH Stroeme [i] DO
				IF offen AND (OLK = AktOLK) THEN
					DateiSchliessen (strom) ;
				END ;
			END ;
		END ;
	END ;
END AlleEigenenDateienSchliessen ;


PROCEDURE AlleDateienSchliessen ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO MaxStromBelegt DO
		WITH Stroeme [i] DO
			IF offen THEN
				DateiSchliessen (strom) ;
			END ;
		END ;
	END ;
	MaxStromBelegt := 0 ;
END AlleDateienSchliessen ;


PROCEDURE Init ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO AnzahlStroeme DO
		WITH Stroeme [i] DO
			offen := FALSE ;
			filebuffer := NIL ;
		END ;
	END ;
	MaxStromBelegt := 0 ;
END Init ;



BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;

FINALLY

	AlleDateienSchliessen ;

END DateiBearbeitung.
