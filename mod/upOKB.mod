
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upOKB;

(*	03.04.18	*)


IMPORT Terminal, Conversions, Strings, ExStrings, AsmTrace, Assembler, FileIO, FileFunc ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM Programm IMPORT
	pProgramm,
	NewProgramm,
	ProgrammLoeschen ;

FROM Assembler IMPORT
	StartAdresse ;

FROM DateiVerwaltung IMPORT * ;

FROM DateiBearbeitung IMPORT
	AlleEigenenDateienSchliessen, DateiOeffnen, SetAktSatzIndexGW, SatzLesen, DateiSchliessen, SatzSchreiben, GetLetzteSatznr,
	MerkeInfo, RANdatei, DattrPlatte ;

FROM Trace IMPORT
	TraceF ;

FROM Namen IMPORT
	pName ;

IMPORT debug ;

FROM AbwicklerUp IMPORT
	AktSatz, tSatz ;


FROM Gebiet IMPORT * ;


VAR

	OKB :			POINTER TO ARRAY [0..1279] OF Ganzwort = NIL ;		(* OKB max. 1280 GW'e *)
	OKBlaenge :		CARDINAL = 0 ;
	aktSSP :		BOOLEAN = FALSE ;
	_L1strom :		CARDINAL = 0 ;
	anfDauerGebiete,
	anfLaufzeitGebiete,
	anfInitialGebiete,
	anzDauerGebiete,
	anzLaufzeitGebiete,
	anzInitialGebiete,
	OKBlng :		CARDINAL ;

	aktOKBDBN :		ARRAY [0..7] OF CHAR ;


PROCEDURE CheckOKBnamen (stromnr, satz : CARDINAL ; progGW1, progGW2 : CARDINAL64) : CARDINAL ;
	VAR	i,
		lng :		CARDINAL ;
BEGIN
	IF AktSatz = NIL THEN
		NEW (AktSatz) ;
	END ;
	SetAktSatzIndexGW (stromnr, 0) ;
	IF NOT SatzLesen (stromnr, satz, AktSatz ^ .bytes [0..256 * SIZE(Ganzwort)], lng) THEN		(* Kopfsatz 1 oder 2 in der &L *)
		RETURN 0 ;
	END ;
	lng := lng DIV SIZE (Ganzwort) ;
	FOR i := 0 TO lng BY 4 DO
		IF (extLiesGanzwort (AktSatz ^ .GWe [i].byte6) = progGW1) AND (extLiesGanzwort (AktSatz ^ .GWe [i+1].byte6) = progGW2) THEN
			RETURN (ORD (extLiesGanzwort (AktSatz ^ .GWe [i+3].byte6)) SHR 12) BAND 0FFFH ;	(* Satzposition der OKB in der &L *)
    		END ;
	END ;
	RETURN 0 ;
END CheckOKBnamen ;


PROCEDURE _L1_Oeffnen (schreibend : BOOLEAN) : BOOLEAN ;
	VAR	dateiname :	ARRAY [0..276] OF CHAR ;
		GENVNR :	CARDINAL ;
BEGIN
	IF _L1strom = 0 THEN
		IF GetEchtenDateinamen (aktOKBDBN, '&L1', dateiname) = TreeOK THEN
			GENVNR := 999999 ;
			IF DateiExistiert (dateiname, GENVNR) THEN
				AppendExtension (GENVNR, dateiname) ;
			ELSE
				IF NOT schreibend THEN
					RETURN FALSE ;
				END ;
				MerkeInfo (		RANdatei,
					(* DL = *)	116,
					(* WZ = *)	4096,
					(* D = *)	3,
					(* Z= *)	3,
					(* DATTR = *)	ORD(DattrPlatte),
					(* EZ= *)	4096,
					(* E= *)	2,
							'',
							''
					) ;

				IF DateiKreieren (dateiname, GENVNR) THEN
					AppendExtension (GENVNR, dateiname) ;
				END ;
			END ;
			_L1strom := DateiOeffnen (dateiname, schreibend) ;
			IF _L1strom = 0 THEN
				RETURN FALSE;
			END ;
		END ;
	END ;
	RETURN TRUE ;
END _L1_Oeffnen ;


PROCEDURE _L1_Satzlesen (satznr : CARDINAL ; GNR : CARDINAL) : BOOLEAN ;
	VAR	i,
		lng :		CARDINAL ;
		dateiname :	ARRAY [0..267] OF CHAR ;
		AchtelSeite :	tAchtelSeite ;
BEGIN
	IF NOT _L1_Oeffnen (FALSE) THEN
		RETURN FALSE ;
	END ;
	SetAktSatzIndexGW (_L1strom, 0) ;
	IF debug.MemoryProtokoll THEN
		TraceF (" Gebiet %c aus &L1(%c) lesen", GNR, satznr) ;
	END ;
	IF SatzLesen (_L1strom, satznr, AktSatz ^ .bytes [0..HIGH(AktSatz ^ .bytes)], lng) THEN		(* Satz mit DGB-/IGB- Inhalt in der &L1 *)
		FOR i := 0 TO lng DIV SIZE(Ganzwort) - 1 BY 128 DO
			AchtelSeite [0..127] := AktSatz ^ . GWe [i .. i+127] ;

			 IF NOT SchreibGebiet (GNR, i DIV 128, AchtelSeite) THEN
				TraceF ("Gebiet %c nicht beschreibbar auf %'05h", GNR, i) ;
			 	RETURN FALSE ;
			END ;
		END ;
		RETURN TRUE ;
	END ;


	RETURN FALSE ;
END _L1_Satzlesen ;


PROCEDURE _L1_FreierSatz () : CARDINAL ;
BEGIN
	IF NOT _L1_Oeffnen (TRUE) THEN
		RETURN 0 ;
	END ;
	RETURN GetLetzteSatznr (_L1strom) + 1 ;
END _L1_FreierSatz ;


PROCEDURE OKBlesen (stromnr, satz : CARDINAL ; progGW1, progGW2 : CARDINAL64) : CARDINAL ;
	VAR	i,
		lng :	CARDINAL ;

	PROCEDURE GetRelAdr (elem : CARDINAL ; VAR anf, anz : CARDINAL) ;
		VAR	wort :	CARDINAL64 ;
	BEGIN
		wort := extLiesGanzwort (OKB ^ [elem].byte6) ;
		anf := wort SHR 25 ;									(* Ganzwort-Nr. in OKB ^ *)
		anz := ORD (wort) BAND 0FFH ;								(* Anzahl Elemente in OKB ^ *)
	END GetRelAdr ;

BEGIN
	IF AktSatz = NIL THEN
		NEW (AktSatz) ;
	END ;
	IF OKB = NIL THEN
		NEW (OKB) ;
	END ;
	SetAktSatzIndexGW (stromnr, 0) ;
	IF NOT SatzLesen (stromnr, satz, AktSatz ^ .bytes [0..HIGH(AktSatz ^ .bytes)], lng) THEN	(* OKB-Satz in der &L *)
		RETURN 0 ;
	END ;
	lng := lng DIV SIZE (Ganzwort) ;
	IF lng > HIGH (OKB ^ ) THEN
		RETURN 0 ;										(* OKB ist zu lang *)
	END ;
	OKB ^ [0..lng] := AktSatz ^ .GWe [0..lng] ;
	OKBlng := lng ;

	IF progGW1 <> 0 THEN										(* Satzinhalt prüfen *)
		IF (extLiesGanzwort (OKB ^ [0].byte6) <> progGW1)
		OR (extLiesGanzwort (OKB ^ [1].byte6) <> progGW2)
		OR ((ORD (extLiesGanzwort (OKB ^ [3].byte6)) BAND 0FFFFH) <> lng) THEN			(* &L ist zerstört *)
			TraceF ('&L zerstört Satz %c', satz) ;
			RETURN 0 ;
		END ;
	END ;

	GetRelAdr (8, anfDauerGebiete, anzDauerGebiete) ;
	GetRelAdr (9, anfLaufzeitGebiete, anzLaufzeitGebiete) ;
	GetRelAdr (11, anfInitialGebiete, anzInitialGebiete) ;
	GetStringAusGanzwort (OKB ^ [12], MV_NR) ;
	RETURN satz ;
END OKBlesen ;


PROCEDURE OKBschreiben (stromnr, satz : CARDINAL) : CARDINAL ;
BEGIN
	SetAktSatzIndexGW (stromnr, 0) ;
	AktSatz ^ .GWe [0..OKBlng] := OKB ^ [0..OKBlng] ;
	IF NOT SatzSchreiben (stromnr, satz, AktSatz ^ .bytes [0..OKBlng * SIZE(Ganzwort) - 1]) THEN	(* OKB-Satz in der &L *)
		RETURN 0 ;
	END ;
	RETURN satz ;
END OKBschreiben ;


PROCEDURE GebietBearbeiten (GNRindex, relLng : CARDINAL) (* geändert *) : BOOLEAN ;
	VAR	GNR,
		lng,
		satznr,
		j :		CARDINAL ;
		Achtelseite :	tAchtelSeite ;
BEGIN
	IF OKB ^ [GNRindex].TK  <> 1 THEN								(* keine Gebietslage *)
		RETURN FALSE ;
	END ;
	GNR := extLiesGanzwort (OKB ^ [GNRindex].byte6) ;
	lng := ORD (extLiesGanzwort (OKB ^ [GNRindex + relLng].byte6)) BAND 0FFFFH ;			(* Länge in KW *)

	satznr := _L1_FreierSatz () ;

	SetAktSatzIndexGW (_L1strom, 0) ;
	FOR j := 1 TO lng * 8 DO
		IF NOT LiesGebiet (GNR, j-1, Achtelseite) THEN
			RETURN FALSE ;
		END ;
		AktSatz ^ .GWe [0..HIGH(Achtelseite)] := Achtelseite ;
		IF NOT SatzSchreiben (_L1strom, satznr, AktSatz ^ .bytes [0..SIZE(Achtelseite) - 1]) THEN
			RETURN FALSE ;
		END ;
	END ;

	DeleteGebiet (GNR) ;

	ZuwGanzwort (OKB ^ [GNRindex], 3, satznr) ;

	RETURN TRUE ;
END GebietBearbeiten ;


PROCEDURE OKBbearbeiten (stromnr, satz : CARDINAL) ;
	VAR	i,
		GNR :		CARDINAL ;
		OKBgeaendert :	BOOLEAN ;
BEGIN
	IF OKBlesen (stromnr, satz, 0, 0) <> 0 THEN
		FOR i := 1 TO anzDauerGebiete DO
			IF GebietBearbeiten (anfDauerGebiete + (i-1) * 6 + 2, 3) THEN
				OKBgeaendert := TRUE ;
			END ;
		END ;
		FOR i := 1 TO anzInitialGebiete DO
			IF GebietBearbeiten (anfInitialGebiete + (i-1) * 4 + 1, 2) THEN
				OKBgeaendert := TRUE ;
			END ;
		END ;
	END ;
	IF OKBgeaendert THEN
		OKBschreiben (stromnr, satz) ;
	END ;

END OKBbearbeiten ;


PROCEDURE OKBsBearbeiten (stromnr, satz : CARDINAL) ;
	VAR
		i,
		lng :		CARDINAL ;
		KopfSatz :	tSatz ;
BEGIN
	NEW (KopfSatz) ;
	SetAktSatzIndexGW (stromnr, 0) ;
	IF NOT SatzLesen (stromnr, satz, KopfSatz ^ .bytes [0..256 * SIZE(Ganzwort)], lng) THEN		(* Kopfsatz 1 oder 2 in der &L *)
		RETURN ;
	END ;
	lng := lng DIV SIZE (Ganzwort) ;
	FOR i := 0 TO lng BY 4 DO
		IF KopfSatz ^ .GWe [i].byte6 [0] <> 0 THEN						(* Element im Kopfsatz ist belegt *)
			OKBbearbeiten (stromnr, ORD (extLiesGanzwort (KopfSatz ^ .GWe [i+3].byte6) SHR 12) BAND 0FFFH) ;	(* Satzposition der OKB in der &L *)
    		END ;
	END ;
	DISPOSE (KopfSatz) ;
END OKBsBearbeiten	 ;


PROCEDURE upGebietslageInDateilage ;
	VAR	GENVNR,
		stromnr :	CARDINAL ;
		datnam :	ARRAY [0..267] OF CHAR ;
BEGIN
	aktOKBDBN := _STDDB ;
	IF GetEchtenDateinamen (_STDDB, '&L', datnam) = TreeOK THEN
		GENVNR := 999999 ;
		IF DateiExistiert (datnam, GENVNR) THEN
			AppendExtension (GENVNR, datnam) ;
			stromnr := DateiOeffnen (datnam, FALSE) ;	(* &L zum Lesen oeffnen *)
			IF stromnr <> 0 THEN
				OKBsBearbeiten (stromnr, 1) ;		(* OKBs in Satz 1 bearbeiten *)
				OKBsBearbeiten (stromnr, 2) ;		(* OKBs in Satz 2 bearbeiten *)
				DateiSchliessen (stromnr) ;		(* &L  schließen *)
				stromnr := 0 ;
				DateiSchliessen (_L1strom) ;		(* &L1 schließen *)
				_L1strom := 0 ;
			END ;
		END ;
	END ;
END upGebietslageInDateilage ;


PROCEDURE DauerGebietLaden (indanf : CARDINAL) : BOOLEAN ;
	VAR	wort :		CARDINAL64 ;
		GNR,
		SNR_L1,
		AA,
		VK,
		BEA,
		LNG :		CARDINAL ;
		OGNM :		ARRAY [0..7] OF CHAR ;
		SSP :		BOOLEAN ;
BEGIN
	GetStringAusGanzwort (OKB ^ [indanf], OGNM) ;
	wort := extLiesGanzwort (OKB ^ [indanf+4].byte6) ;
	AA := ORD (wort SHR 24) BAND Cardinal24Bit ;
	VK := (ORD (wort) SHR 12) BAND 0FFFH ;
	SSP := OKB ^ [indanf+5].TK = 3 ;
	wort := extLiesGanzwort (OKB ^ [indanf+5].byte6) ;
	BEA := ORD (wort SHR 24) BAND Cardinal24Bit ;
	LNG := ORD (wort) BAND 0FFFFH ;

	IF VK > 10H THEN
		AA := AA BOR 8000000H ;		(* damits nicht Adresse 0 ist *)
	ELSE
		AA := 0 ;
	END ;

	IF OKB ^ [indanf+2].TK = 1 THEN					(* Gebietslage *)
		GNR := extLiesGanzwort (OKB ^ [indanf+2].byte6) ;
	ELSE								(* Dateilage *)
		SNR_L1 := extLiesGanzwort (OKB ^ [indanf+2].byte6) ;

		GNR :=  CreateGebiet (OGNM, '', LNG, FALSE) ;
		IF GNR = 0 THEN
			TraceF ('&L1-GB %s nicht kreierbar', OGNM) ;
			RETURN FALSE ;
		END ;
		_L1_Satzlesen (SNR_L1, GNR) ;
	END ;
	IF NOT AnmeldeGebiet (GNR, AA, OGNM) THEN
		RETURN FALSE ;
	END ;

	IF SSP THEN
		SetSchreibSperre (GNR, TRUE) ;
	END ;

	RETURN TRUE ;
END DauerGebietLaden ;


PROCEDURE LaufzeitGebietKreieren (indanf : CARDINAL) : CARDINAL ;
	VAR	wort :		CARDINAL64 ;
		VORBES :	Ganzwort ;
		GNR,
		AA,
		VK,
		BEA,
		LNG,
		i,
		j :		CARDINAL ;
		OGNM :		ARRAY [0..7] OF CHAR ;
		VORBESrelevant :BOOLEAN ;
		Achtelseite :	tAchtelSeite ;
BEGIN
	GetStringAusGanzwort (OKB ^ [indanf], OGNM) ;
	VORBES := OKB ^ [indanf+1] ;
	wort := extLiesGanzwort (OKB ^ [indanf+2].byte6) ;
	AA := (wort SHR 24) BAND Cardinal24Bit ;
	VK := (wort SHR 12) BAND 0FFFH ;
	VORBESrelevant := ODD (wort) ;
	aktSSP := OKB ^ [indanf+3].TK = 3 ;
	wort := extLiesGanzwort (OKB ^ [indanf+3].byte6) ;
	BEA := (wort SHR 24) BAND Cardinal24Bit ;
	LNG := wort BAND 0FFFFH ;

	GNR :=  CreateGebiet (OGNM, '', LNG, FALSE) ;
	IF GNR = 0 THEN
		TraceF ('LGB %s nicht kreierbar', OGNM) ;
	END ;

	IF VK > 10H THEN
		IF NOT AnmeldeGebiet (GNR, AA BOR 8000000H, '') THEN
			TraceF ('LGB %s nicht anmeldbarbar auf %h', OGNM, AA) ;
			RETURN 0 ;
		END ;
	END ;

	IF VORBESrelevant THEN
		FOR i := 0 TO HIGH(Achtelseite) DO
			Achtelseite [i] := VORBES ;
		END ;
		FOR i := 0 TO LNG*8 - 1 DO
			 IF NOT SchreibGebiet (GNR, i, Achtelseite) THEN
				TraceF ('LGB %s nicht vorloeschbar auf %h', OGNM, AA) ;
			 	RETURN 0 ;
			END ;
		END ;
	END ;

	RETURN GNR ;
END LaufzeitGebietKreieren ;


PROCEDURE InitialGebietKopieren (LGEB, reladr : CARDINAL) : BOOLEAN ;
	VAR	wort :		CARDINAL64 ;
		i, j,
		indanf,
		GNR,
		SNR_L1,
		LNG :		CARDINAL ;
		Achtelseite :	tAchtelSeite ;
BEGIN
	FOR i := 1 TO anzInitialGebiete DO
		indanf := anfInitialGebiete + (i-1) * 4 ;
		wort := extLiesGanzwort (OKB ^ [indanf+3].byte6) ;
		IF ORD (wort SHR 24) = reladr THEN			(* das ist das zum Laufzeitgebiet 'reladr' gehörende Initialgebiet *)
			LNG := wort BAND 0FFFFH ;

			IF OKB ^ [indanf+1].TK = 1 THEN
				GNR := extLiesGanzwort (OKB ^ [indanf+1].byte6) ;
				FOR j := 1 TO LNG*8 DO
					IF NOT LiesGebiet (GNR, j-1, Achtelseite) THEN
						RETURN FALSE ;
					END ;
					IF NOT SchreibGebiet (LGEB, j-1, Achtelseite) THEN
						RETURN FALSE ;
					END ;
				END ;
			ELSE
				SNR_L1 := extLiesGanzwort (OKB ^ [indanf+1].byte6) ;
				_L1_Satzlesen (SNR_L1, LGEB) ;
			END ;
			RETURN TRUE ;
		END ;
	END ;
	RETURN TRUE ;
END InitialGebietKopieren ;


PROCEDURE GebieteLadenOKB () : BOOLEAN ;
	VAR	i,
		GNR :	CARDINAL ;
BEGIN
	_L1strom := 0 ;
	FOR i := 1 TO anzDauerGebiete DO
		IF NOT DauerGebietLaden (anfDauerGebiete + (i-1) * 6) THEN
			RETURN FALSE ;
		END ;
	END ;
	FOR i := 1 TO anzLaufzeitGebiete DO
		GNR := LaufzeitGebietKreieren (anfLaufzeitGebiete + (i-1) * 4) ;
		IF GNR = 0 THEN
			RETURN FALSE ;
		END ;
		IF NOT InitialGebietKopieren (GNR, (i-1) * 8) THEN
			RETURN FALSE ;
		END ;
		IF aktSSP THEN
			SetSchreibSperre (GNR, TRUE) ;
		END ;
	END ;
	DateiSchliessen (_L1strom) ;
	_L1strom := 0 ;

	RETURN TRUE ;
END GebieteLadenOKB ;


PROCEDURE OKBda (DBN : ARRAY OF CHAR; progGW1, progGW2 : CARDINAL64) : BOOLEAN ;
	VAR	GENVNR,
		stromnr,
		satz :		CARDINAL ;
		datnam :	ARRAY [0..267] OF CHAR ;
BEGIN
	IF GetEchtenDateinamen (DBN, '&L', datnam) = TreeOK THEN
		GENVNR := 999999 ;
		IF DateiExistiert (datnam, GENVNR) THEN
			AppendExtension (GENVNR, datnam) ;
			stromnr := DateiOeffnen (datnam, FALSE) ;	(* &L zum Lesen oeffnen *)
			IF stromnr <> 0 THEN
				satz := CheckOKBnamen (stromnr, 1, progGW1, progGW2) ;
				IF satz = 0 THEN
					satz := CheckOKBnamen (stromnr, 2, progGW1, progGW2) ;
				END ;
				IF satz <> 0 THEN
					satz := OKBlesen (stromnr, satz, progGW1, progGW2) ;
				END ;
				DateiSchliessen (stromnr) ;		(* &L schließen *)
				RETURN satz <> 0 ;
			END ;
		END ;
	END ;
	RETURN FALSE ;
END OKBda ;


PROCEDURE ProgrammOKBsuchen (name : ARRAY OF CHAR) : pProgramm ;
	VAR	i :		CARDINAL ;
		p0 :		pProgramm ;
		reg :		reg48 ;
		DBN :		ARRAY [0..7] OF CHAR ;
		progGW1,
		progGW2 :	CARDINAL64 ;
BEGIN
	OKBaktiv := FALSE ;
	p0 := NIL ;
	reg.TK := 3 ;
	IF LENGTH (name) > 6 THEN
		PutStringInRegister (name [0..5], reg) ;
		progGW1 := reg.inh ;
		PutStringInRegister (name [6..LENGTH(name)], reg) ;
		progGW2 := reg.inh ;
	ELSE
		PutStringInRegister (name [0..LENGTH(name)], reg) ;
		progGW1 := reg.inh ;
		progGW2 := 0 ;
	END ;
	FOR i := 0 TO 7 DO
		IF AktAuftrag.Hierarchie [i] = 0 THEN		(* Ende der Hierarchie : nix gefunden *)
			RETURN NIL ;
		END ;
		reg.inh := AktAuftrag.Hierarchie [i] ;
		reg.TK := 3 ;
		GetStringAusRegister (reg, DBN) ;
		IF OKBda (DBN, progGW1, progGW2) THEN
			BREAK ;
		END ;
	END ;
	OKBaktiv := TRUE ;
	aktOKBDBN := DBN ;
	ProgrammLoeschen (name) ;
	TraceF ('* Prog %s (%s)', name, aktOKBDBN) ;
	p0 := NewProgramm (name, ' ') ;
	RegU := ORD (extLiesGanzwort (OKB ^ [3].byte6) SHR 24) BAND 0FFH ;
	RegX := extLiesGanzwort (OKB ^ [5].byte6) SHR 24 ;
	StartAdresse := extLiesGanzwort (OKB ^ [7].byte6) SHR 24 ;
	p0 ^ .alarm := ORD (extLiesGanzwort (OKB ^ [7].byte6)) BAND 0FFFFFFH ;
	p0 ^ .start := StartAdresse ;
	p0 ^ .ausOKBgeladen := TRUE ;
	RegF := StartAdresse ;
	SaveRegister (p0 ^ .register) ;
	RETURN p0 ;
END ProgrammOKBsuchen ;


END upOKB.
