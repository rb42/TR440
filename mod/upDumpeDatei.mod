
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upDumpeDatei;

(*	25.03.18	*)


FROM SYSTEM IMPORT
	SWAPENDIAN ;

IMPORT FileFunc, RunProg, Strings, Registry ;

FROM COMMCTRL IMPORT
	InitCommonControls ;

FROM Conversions IMPORT
	CardToStr ;

FROM Strings IMPORT
	Append ;

%IF %NOT WEB %THEN
FROM Menue IMPORT
	Editor ;
%END

FROM Struktur IMPORT Halbwort, Ganzwort, extLiesHalbwort, tAchtelSeite ;

FROM ZC1 IMPORT * ;

FROM DateiBearbeitung IMPORT * ;

FROM DateiVerwaltung IMPORT
	_STDDB, GetLokalDateinamen, TreeOK ;

IMPORT FileIO ;

FROM FileIO IMPORT
	FILE,
	CreateFile ;

FROM Parameter IMPORT
	InitParameter,
	NextParameter ;

FROM Terminal IMPORT
	WriteF,
	ReadString,
	AbbruchWunsch ;

FROM OFNup IMPORT
	OFNtr440,
	AktString ;

FROM TR440hp IMPORT
	DefaultPfad,
	EditorKey,
	DatenKey ;

FROM Gebiet IMPORT
	pGebiet,
	GetGebiet,
	LiesGebiet ;

VAR
	satznr :	CARDINAL64 ;

	strmnr,
	laenge :	CARDINAL ;

	str :		ARRAY [0..255] OF CHAR ;

	satz :		RECORD
				CASE : BOOLEAN OF
				TRUE :		bytes :		ARRAY [0..65536*7-1] OF CHAR ;
				ELSE		GWe :		ARRAY [0..65535] OF Ganzwort ;
				END ;
			END ;

	datei :		FileFunc.File ;



PROCEDURE KonvertSatzNachExtern (VAR ZC1buffer, buffer : ARRAY OF CHAR ; laenge : CARDINAL) ;
	VAR	i, j :		CARDINAL ;
		str :		ARRAY [0..7] OF CHAR ;
		ch :		CHAR ;

BEGIN
	satz.bytes := ZC1buffer ;
	buffer := '' ;
	IF laenge <> 0 THEN
		str [6] := 0C ;
		FOR i := 1 TO (laenge + SIZE(Ganzwort) - 1) DIV SIZE(Ganzwort) DO
			FOR j := 0 TO 5 DO
				ch := ZC1toANSI [CHR(satz.GWe [i-1].byte6 [j])] ;
				IF ch < ' ' THEN
					ch := ' ' ;
				END ;
				str [j] := ch ;
			END ;
			Append (str, buffer) ;
		END ;
	END ;
END KonvertSatzNachExtern ;



PROCEDURE KonvertTR440datei (quelle : ARRAY OF CHAR ; VAR dateiname : ARRAY OF CHAR ; alsText : BOOLEAN) : BOOLEAN ;
	VAR	i,
		j,
		hi,
		lo,
		rel :		CARDINAL ;
		dtb,
		str :		ARRAY [0..15] OF CHAR ;
		genvnr,
		DTT, DL, WZ, D, Z,
		DATTR, EZ, E,
		Saetze :	CARDINAL ;
		BKZ,
		EXDKZ :		ARRAY [0..7] OF CHAR ;

		ch :		CHAR ;


	PROCEDURE protDTB ;
	BEGIN
		FileIO.WriteF (datei, "%s(%c.%'02c)", dtb, genvnr DIV 100, genvnr MOD 100) ;
	END protDTB ;

	PROCEDURE protDTT ;
	BEGIN
		CASE VAL (tDateityp, DTT) OF
		SEQdatei :	str := 'SEQ' ;
		|
		RANdatei :	str := 'RAN' ;
		|
		RAMdatei :	str := 'RAM' ;
		|
		RASdatei :	str := 'RAS' ;
		|
		PHYSdatei :	str := 'PHYS' ;
		ELSE
			RETURN ;
		END ;
		FileIO.WriteF (datei, ', %s', str) ;
	END protDTT ;

	PROCEDURE protDLWZ ;
		CONST	DZ =	'-MGU' ;
	BEGIN
		IF (D > 0) AND (D < 4) THEN
			str [0] := DZ [D] ;
			str [1] := 0C ;
			FileIO.WriteF (datei, ', %s%c', str, DL) ;
		END ;
		IF (Z > 0) AND (Z < 4) THEN
			str [0] := DZ [Z] ;
			str [1] := 0C ;
			FileIO.WriteF (datei, ', %s%cW', str, WZ) ;
		END ;
	END protDLWZ ;

	PROCEDURE protDATTR ;
	BEGIN
		CASE DATTR OF
		0 :	str := 'KSP' ;
		|
		1 :	str := 'Platte' ;
		|
		2:	str := 'Trommel' ;
		|
		3:	str := 'MB' ;
		|
		4:	str := 'WSP' ;
		|
		5:	str := 'LFD' ;
		ELSE
			RETURN ;
		END ;
		FileIO.WriteF (datei, ', %s', str) ;
	END protDATTR ;

	PROCEDURE protEZ ;
	BEGIN
		IF E <> 0 THEN
			CASE E OF
			1 :	str := 'OKT' ;
			|
			2 :	str := 'GW' ;
			|
			3 :	str := 'VW' ;
			|
			4 :	str := 'AZ' ;
			|
			5 :	str := 'GW/OKT' ;
			|
			6 :	str := 'VW/OKT' ;
			ELSE
				RETURN ;
			END ;
			FileIO.WriteF (datei, ', %c %s pro Satz', EZ, str) ;
		END ;
	END protEZ ;

	PROCEDURE protSaetze ;
	BEGIN
		IF Saetze = 0 THEN
			FileIO.WriteF (datei, ', leere Datei') ;
		ELSIF Saetze = 1 THEN
			FileIO.WriteF (datei, ', 1 Satz') ;
		ELSE
			FileIO.WriteF (datei, ', %c Sätze', Saetze) ;
		END ;
	END protSaetze ;

BEGIN
	nurTR440dateien := TRUE ;
	strmnr := DateiOeffnen (quelle, FALSE) ;
	nurTR440dateien := FALSE ;

	IF strmnr = 0 THEN
		WriteF ('\n+++++ Datei  %s  konnte nicht geöffnet werden', quelle) ;
		dateiname := quelle ;
		RETURN FALSE ;
	END ;
	IF alsText THEN
		FileFunc.ConstructFileName ('.txt', quelle, dateiname) ;
	ELSE
		FileFunc.ConstructFileName ('.bin440', quelle, dateiname) ;
	END ;
	IF NOT CreateFile (datei, dateiname) THEN
		WriteF ('\n+++++ Datei  %s  nicht kreierbar', dateiname) ;
		DateiSchliessen (strmnr) ;
		RETURN FALSE  ;
	END ;


	IF CheckInfoStrmnr (strmnr) THEN
		GetInfo (dtb, genvnr, DTT, DL, WZ, D, Z, DATTR, EZ, E, Saetze, BKZ, EXDKZ) ;
		IF NOT alsText THEN
			FileIO.WriteF (datei, '\n\n\t') ;
			protDTB ;
			protDTT ;
			protDLWZ ;
			protDATTR ;
			protEZ ;
			protSaetze ;
		END ;
	END ;

	SetPosition (strmnr, 0) ;
	satznr := GetPosition (strmnr) ;
	WHILE SatzLesen (strmnr, satznr, satz.bytes, laenge) DO
		satznr := GetPosition (strmnr) ;
		IF alsText THEN
			IF laenge <> 0 THEN
				FOR i := 1 TO (laenge + SIZE(Ganzwort) - 1) DIV SIZE(Ganzwort) DO
					str [6] := 0C ;
					FOR j := 0 TO 5 DO
						ch := ZC1toANSI [CHR(satz.GWe [i-1].byte6 [j])] ;
						IF ch < ' ' THEN
							ch := ' ' ;
						END ;
						str [j] := ch ;
					END ;
					FileIO.WriteString (datei, str) ;
				END ;
			END ;
			FileIO.WriteLn (datei, '') ;
		ELSE
			FileIO.WriteF (datei, '\n') ;
			IF laenge = 0 THEN
				FileIO.WriteF (datei, "\n%' 6c\t\t*Leersatz*", satznr) ;
			ELSE
				FOR i := 1 TO (laenge + SIZE(Ganzwort) - 1) DIV SIZE(Ganzwort) DO
					IF (i-1) MOD 4 = 0 THEN
						rel := (i-1) * 2 ;
						FileIO.WriteF (datei, "\n%' 4c-%'04h", ORD(satznr), rel) ;
					END ;
					hi := extLiesHalbwort (satz.GWe [i-1].hi) ;
					lo := extLiesHalbwort (satz.GWe [i-1].lo) ;
					str [6] := 0C ;
					FOR j := 0 TO 5 DO
						ch := ZC1toANSI [CHR(satz.GWe [i-1].byte6 [j])] ;
						IF ch < ' ' THEN
							ch := '.' ;
						END ;
						str [j] := ch ;
					END ;
					IF ORD (satz.GWe [i-1].TK) > 3 THEN	(* kaputte Struktur *)
						str := '??????' ;
					END ;
					FileIO.WriteF (datei, "\t%c %'06h %'06h   %s", ORD(satz.GWe [i-1].TK), hi, lo, str) ;
				END ;
			END ;
		END ;
		IF NOT SetPositionRelativ (strmnr, 1) THEN
			BREAK ;
		END ;
		satznr := GetPosition (strmnr) ;
		SetAktSatzIndexGW (strmnr, 0) ;
	END ;
	DateiSchliessen (strmnr) ;
	FileIO.CloseFile (datei) ;
	RETURN TRUE ;
END KonvertTR440datei ;


PROCEDURE KonvertTR440gebiet (gebietsnummer : CARDINAL ; VAR dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	geb :		pGebiet ;
		i, j, k,
		hi, lo :	CARDINAL ;
		ch :		CHAR ;
		str,
		str1,
		ziel :	ARRAY [0..267] OF CHAR ;
		puffer :	tAchtelSeite ;
BEGIN
	geb := GetGebiet (gebietsnummer) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;
	CardToStr (gebietsnummer, str1) ;
	str := 'gebiet-' ;
	Append (str1, str) ;
	IF GetLokalDateinamen (_STDDB, str, dateiname) <> TreeOK THEN
		RETURN FALSE ;
	END ;

	FileFunc.ConstructFileName ('.bin440', dateiname, ziel) ;

	IF NOT CreateFile (datei, ziel) THEN
		WriteF ('\n+++++ Datei  %s  nicht kreierbar\n', ziel) ;
		RETURN FALSE  ;
	END ;

	dateiname := ziel ;

	FileIO.WriteF (datei, '\n\t\tGebiet %c, OLK = %c, %c KW', gebietsnummer, geb ^ .OLK, geb ^ .Laenge) ;
	IF geb ^ .OGNM [0] <> 0C THEN
		FileIO.WriteF (datei, ', OGNM = "%s"', geb ^ .OGNM) ;
	END ;
	IF geb ^ .PGNM [0] <> 0C THEN
		FileIO.WriteF (datei, ', PGNM = "%s"', geb ^ .PGNM) ;
	END ;
	IF geb ^ .Dauergebiet THEN
		FileIO.WriteF (datei, ', Dauergebiet') ;
	END ;
	IF geb ^ .Schreibsperre THEN
		FileIO.WriteF (datei, ', Schreibsperre') ;
	END ;

	FileIO.WriteF (datei, '\n') ;
	IF geb ^ .Laenge = 0 THEN
		FileIO.WriteF (datei, "\nleeres Gebiet") ;
	ELSE
		FOR i := 0 TO geb ^ .Laenge * 8 -1 DO		(* pro Achtelseite *)
			FileIO.WriteF (datei, '\n') ;
			IF NOT LiesGebiet (gebietsnummer, i, puffer) THEN
				BREAK ;
			END ;
			FOR j := 0 TO 127 DO			(* pro Ganzwort *)
				IF j MOD 4 = 0 THEN
					FileIO.WriteF (datei, "\n%' 3c-%'04h", i, i*256 + j*2) ;
				END ;
				hi := extLiesHalbwort (puffer [j].hi) ;
				lo := extLiesHalbwort (puffer [j].lo) ;
				str [6] := 0C ;
				FOR k := 0 TO 5 DO
					ch := ZC1toANSI [CHR(puffer [j].byte6 [k])] ;
					IF ch < ' ' THEN
						ch := '.' ;
					END ;
					str [k] := ch ;
				END ;
				IF ORD (puffer [j].TK) > 3 THEN	(* kaputte Struktur *)
					str := '??????' ;
				END ;
				FileIO.WriteF (datei, "\t%c %'06h %'06h   %s", ORD(puffer [j].TK), hi, lo, str) ;
			END ;
		END ;
	END ;
	FileIO.CloseFile (datei) ;
	RETURN TRUE ;
END KonvertTR440gebiet ;


END upDumpeDatei.
