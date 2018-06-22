
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE EingabeGebiet;

(*	09.04.18	*)

FROM ZC1 IMPORT
	ANSItoZC1 ;

FROM ASCII IMPORT
	em ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM Gebiet IMPORT * ;


VAR
	EinGBK,
	ASnr :		CARDINAL ;
	puffer,
	fluli :		tAchtelSeite ;
	GW :		Ganzwort ;
	gebietsindex,
	pufferindex,
	wortindex,
	fluliindex :	CARDINAL ;
	imFremdstring :	BOOLEAN ;


PROCEDURE InitGW ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 0 TO 5 DO
		GW.byte6 [i] := 0 ;
	END ;
END InitGW ;


PROCEDURE InitPuffer ;
	VAR	i :	CARDINAL ;
BEGIN
	GW.TK := 3 ;
	InitGW ;
	FOR i := 0 TO HIGH (puffer) DO
		puffer [i] := GW ;
	END ;
	pufferindex := 0 ;
	wortindex := 0 ;
END InitPuffer ;


PROCEDURE InitEingabeGebiet ;
BEGIN
	EinGBK := 0 ;
	ASnr := 0 ;
	gebietsindex := 0 ;
	fluliindex := 0 ;
	imFremdstring := FALSE ;
	InitPuffer ;
	fluli := puffer ;
	RETURN ;
END InitEingabeGebiet ;


PROCEDURE StuelpPuffer ;
BEGIN
	IF ASnr MOD 8 = 0 THEN
		IF EinGBK = 0 THEN
			EinGBK := CreateGebiet ('', '', 1, TRUE) ;
			RegA.inh := EinGBK ;	(* Eingabe-Gebietskennz. *)
			RegA.TK := 1 ;
		ELSE
			VeraendereGebiet (EinGBK, ASnr DIV 8 + 1) ;
		END ;
	END ;
	SchreibGebiet (EinGBK, ASnr, puffer) ;
	ASnr := ASnr + 1 ;
	InitPuffer ;
END StuelpPuffer ;


PROCEDURE PutEingabe (ch : CHAR) ;
BEGIN
	GW.byte6 [wortindex] := ORD (ANSItoZC1 [ch]) ;
	wortindex := wortindex + 1 ;
	IF wortindex = 6 THEN
		puffer [pufferindex] := GW ;
		InitGW ;
		wortindex := 0 ;
		pufferindex := pufferindex + 1 ;
		IF pufferindex > HIGH (puffer) THEN
			StuelpPuffer ;
		END ;
	END ;
	gebietsindex := gebietsindex + 1 ;
END PutEingabe ;


PROCEDURE PutFluliHW (HW : CARDINAL) ;
BEGIN
	IF (fluliindex DIV 2) <= HIGH(fluli) THEN		(* max. 256 Fluli-Eintr‰ge mˆglich *)
		IF ODD (fluliindex) THEN
			ZuwHalbwort (fluli [fluliindex DIV 2].lo, HW) ;
		ELSE
			ZuwHalbwort (fluli [fluliindex DIV 2].hi, HW) ;
		END ;
		fluliindex := fluliindex + 1 ;
	END ;
END PutFluliHW ;


PROCEDURE PutFluliElement ;
	VAR	HW :	CARDINAL ;
BEGIN
	HW := (gebietsindex MOD 3) SHL 22 + gebietsindex DIV 3 ;
	PutFluliHW (HW) ;
END PutFluliElement ;


PROCEDURE PutEingabeGebiet (str : ARRAY OF CHAR) ;
	VAR	i,
		lng :		CARDINAL ;
		ch :		CHAR ;
		gleichvorher :	BOOLEAN ;
BEGIN
	gleichvorher := FALSE ;
	lng := LENGTH (str) ;
	IF (lng > 1) AND (str [lng-2] = '#') AND (str [lng-1] = '.') THEN
		lng := lng - 2 ;			(*  #.  am Zeilenende weg *)
	END ;
	FOR i := 1 TO lng DO
		ch := str [i-1] ;
		CASE ch OF
		'#' :		imFremdstring := FALSE ;
				PutFluliElement ;
				PutEingabe ('#') ;
				gleichvorher := FALSE ;
		|
		'=' :		PutEingabe ('=') ;
				gleichvorher := TRUE ;
		|
		'/' :		IF gleichvorher THEN
					imFremdstring := TRUE ;
				END ;
				PutEingabe ('/') ;
				gleichvorher := FALSE ;
		|
		em :		PutFluliElement ;
				PutEingabe (em) ;
				gleichvorher := FALSE ;
		|
		' ',
		CHR(9) :	PutEingabe (' ') ;
		|
		'a'..'z' :	IF imFremdstring THEN
					PutEingabe (ch) ;
				ELSE
					PutEingabe (CHR (ORD (ch) - 32)) ;	(* -> Groﬂbuchstabe *)
				END ;
				gleichvorher := FALSE ;
		ELSE
				PutEingabe (ch) ;
				gleichvorher := FALSE ;
		END ;
	END ;
END PutEingabeGebiet ;


PROCEDURE PutPuffer (puf : tAchtelSeite ; anzGW : CARDINAL ; reladr : CARDINAL) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO anzGW DO
		PutGanzwortU (reladr + (i - 1) * 2, puf [i-1]) ;
	END ;
END PutPuffer ;


PROCEDURE ExitEingabeGebiet (AAE, LNGE : CARDINAL) (* FluLi *) : CARDINAL ;
BEGIN
	PutEingabeGebiet (em) ;
	WHILE wortindex <> 0 DO
		PutEingabe (0C) ;	(* letztes Wort raus *)
	END ;
	IF fluliindex <= 1 THEN		(* kein Flusy da *)
		IF (ASnr =0) AND (pufferindex < LNGE) THEN
			PutPuffer (puffer, pufferindex, AAE) ;
			RETURN 0 ;
		END ;
	ELSE
		PutFluliHW (0FFFFFFH) ;
		PutFluliHW (0FFFFFFH) ;
	END ;
	IF (ASnr = 0) AND ((pufferindex * 6 + fluliindex * 3 + 2) < LNGE * 6) THEN		(* Gebiet unnoetig *)
		PutPuffer (puffer, pufferindex, AAE) ;
		PutPuffer (fluli, (fluliindex + 1) DIV 2, AAE + pufferindex * 2) ;
		IF fluliindex <= 1 THEN
			RETURN 0 ;
		ELSE
			RETURN pufferindex * 2 ;
		END ;

	ELSE
		RegA.TK := 1 ;			(* merke : Gebiet ist da *)
		RegA.inh := EinGBK ;
		IF pufferindex <> 0 THEN
			StuelpPuffer ;		(* letzte 1/8-Seite raus *)
		END ;
		IF fluliindex <= 1 THEN
			RETURN 0 ;
		END ;
		IF ASnr MOD 8 = 0 THEN
			VeraendereGebiet (EinGBK, ASnr DIV 8 + 1) ;
		END ;
		SchreibGebiet (EinGBK, ASnr, fluli) ;
		RETURN ASnr ;
	END ;
END ExitEingabeGebiet ;


END EingabeGebiet.
