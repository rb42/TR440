
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_3;

(*	13.02.16	*)


FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Gebiet IMPORT * ;

IMPORT Strings, Terminal, FileFunc, FileIO, AsmTrace ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM AbwicklerUp IMPORT * ;



PROCEDURE _SSR_3_0 ;
	VAR	OGNM,
		PGNM :		ARRAY [0..7] OF CHAR ;
		wort :		CARDINAL64 ;
		adresse,
		LNG,
		GNR :		CARDINAL ;
		Schreibsperre,
		Dauergebiet :	BOOLEAN ;
BEGIN
	SSRaufruf ('Gebiet erstellen', 7) ;
	GetStringAusSpeicher (OGNM, VBL + 2, 6) ;
	GetStringAusSpeicher (PGNM, VBL + 4, 6) ;
	wort := GetGanzwort (VBL + 6) ;
	Schreibsperre := AktTypenkennung = 3 ;				(* SSP *)
	LNG := wort BAND 0FFFFH ;
	wort := GetGanzwort (VBL + 12) ;
	Dauergebiet :=  (wort BAND 0C00000H) <> 0 ;		(* GA *)
	IF (wort BAND 800000000000H) = 0 THEN			(* A *)
		adresse := (wort SHR 24) BAND 3FFFFFH ;		(* AA *)
	ELSE
		adresse := 0 ;
	END ;
	GNR := CreateGebiet (OGNM, PGNM, LNG, Dauergebiet) ;
	IF GNR = 0 THEN
		SSRfehler (7, 'Gebietsname schon vergeben') ;
		RETURN ;
	END ;
	SetSchreibSperre (GNR, Schreibsperre) ;
	IF adresse <> 0 THEN
		IF NOT AnmeldeGebiet (GNR, adresse, '') THEN
			SSRfehler (11, 'Überschreitung des Operatorlauf-Adressraums') ;
			RETURN ;
		END ;
	END ;
	RegA.inh := GNR ;
	RegA.TK := 1 ;
END _SSR_3_0 ;


PROCEDURE _SSR_3_4 ;
	VAR	GNR :	CARDINAL ;
BEGIN
	SSRaufruf ('Gebiet löschen', 2) ;
	GNR := GNRausGBK (2) ;
	IF GNR <> 0 THEN
		IF NOT DeleteGebiet (GNR) THEN
			SSRfehler (8, 'Gebietsname ist unbekannt') ;
		END ;
	END ;
END _SSR_3_4 ;


PROCEDURE _SSR_3_8 ;
	VAR	GNR,
		adresse :	CARDINAL ;
		wort : 		 CARDINAL64 ;
BEGIN
	SSRaufruf ('Gebiet aufrufen zur Verarbeitung', 3) ;
	GNR := GNRausGBK (2) ;
	IF GNR = 0 THEN
		RETURN ;
	END ;
	wort := GetGanzwort (VBL + 4) ;
	IF (wort BAND 800000000000H) = 0 THEN			(* A *)
		adresse := (wort SHR 24) BAND 3FFFFFH ;		(* AA *)
		IF NOT AnmeldeGebiet (GNR, adresse, '') THEN
			SSRfehler (11, 'Überschreitung des Operatorlauf-Adressraums') ;
			RETURN ;
		END ;
	END ;
END _SSR_3_8 ;


PROCEDURE _SSR_3_12 ;
BEGIN
	SSRnimpl ('Gemeinschaftsgebiet einrichten') ;
END _SSR_3_12 ;


PROCEDURE _SSR_3_16 ;
	VAR	GNR :	CARDINAL ;
BEGIN
	SSRaufruf ('Gebiet abmelden von der Verarbeitung', 2) ;
	GNR := GNRausGBK (2) ;
	IF GNR <> 0 THEN
		AbmeldeGebiet (GNR) ;
	END ;
END _SSR_3_16 ;


PROCEDURE _SSR_3_20 ;
BEGIN
	SSRnimpl ('Gemeinschaftsgebietssperre verändern') ;
END _SSR_3_20 ;


PROCEDURE _SSR_3_24 ;
	VAR	GNR :	CARDINAL ;
BEGIN
	SSRaufruf ('Schreibsperre setzen', 2) ;
	GNR := GNRausGBK (2) ;
	IF GNR = 0 THEN
		RETURN ;
	END ;
	SetSchreibSperre (GNR, TRUE) ;
END _SSR_3_24 ;


PROCEDURE _SSR_3_28 ;
	VAR	GNR :	CARDINAL ;
BEGIN
	SSRaufruf ('Schreibsperre löschen', 2) ;
	GNR := GNRausGBK (2) ;
	IF GNR = 0 THEN
		RETURN ;
	END ;
	SetSchreibSperre (GNR, FALSE) ;
END _SSR_3_28 ;


PROCEDURE _SSR_3_36 ;
	VAR	GNR,
		LNG :	CARDINAL ;
BEGIN
	SSRaufruf ('Verändern der Länge', 2) ;
	GNR := GNRausGBK  (2) ;
	IF GNR = 0 THEN
		RETURN ;
	END ;
	LNG := GetHalbwort (VBL + 1) ;
	IF NOT VeraendereGebiet (GNR, LNG) THEN
		SSRfehler (18, 'Standardgebiet nicht veränderbar') ;
	END ;
END _SSR_3_36 ;


PROCEDURE _SSR_3_56 ;
	VAR	GNR,
		VK :	CARDINAL ;
BEGIN
	SSRaufruf ('Verändern der Verarbeitungsklasse', 2) ;
	GNR := GNRausGBK (2) ;
	IF GNR = 0 THEN
		RETURN ;
	END ;
	VK := (GetHalbwort (VBL + 1) SHR 12) BAND 1FH ;
	IF VK >= 1CH THEN			(* Kernspeicher *)
		IF NOT LadeGebiet (GNR) THEN
			SSRfehler (0CH, 'Gebiet nicht zur Bearbeitung aufrufbar');
		END ;
	ELSE
		EntladeGebiet (GNR) ;
	END ;
END _SSR_3_56 ;


PROCEDURE _SSR_3_68 ;
	VAR
		AAA,
		AAE,
		ANZ,
		P,
		wort,
		asnr,
		GNRabsender,
		GNRempfaenger :	CARDINAL ;
		puffer :	tAchtelSeite ;
BEGIN
	SSRaufruf ('Achtelseitentransport', 5) ;
	P := GetHalbwort (VBL + 1) SHR 8 BAND 1 ;
	wort := GetGanzwort (VBL + 2) ;
	IF AktTypenkennung = 0 THEN
		GNRabsender := 0 ;
	ELSE
		GNRabsender := GNRausGBK (2) ;
		IF GNRabsender = 0 THEN
			RETURN ;
		END ;
	END ;
	wort := GetGanzwort (VBL + 6) ;
	IF AktTypenkennung = 0 THEN
		GNRempfaenger := 0 ;
	ELSE
		GNRempfaenger := GNRausGBK (6) ;
		IF GNRempfaenger = 0 THEN
			RETURN ;
		END ;
	END ;
	AAA := GetHalbwort (VBL + 4) ;
	ANZ := GetHalbwort (VBL + 5) ;
	AAE := GetHalbwort (VBL + 8) ;
	IF P <> 0 THEN
		ANZ := ANZ DIV 128 ;				(* Ganzwort-Anzahl -> Achtelseitenanzahl *)
		IF GNRabsender <> 0 THEN
			AAA := AAA DIV 256 ;			(* Absender-Gebietsrel.Adresse -> Achtelseitennummer *)
		END ;
		IF GNRempfaenger <> 0 THEN
			AAE := AAE DIV 256 ;			(* Empfaenger-Gebietsrel.Adresse -> Achtelseitennummer *)
		END ;
	END ;
	FOR asnr := 0 TO ANZ-1 DO
		IF GNRabsender = 0 THEN				(* Quelle ist Hauptspeicher *)
			GetAchtelSeite (AAA + asnr*256, puffer) ;
		ELSE						(* Quelle ist Gebiet *)
			LiesGebiet (GNRabsender, AAA + asnr, puffer) ;
		END ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
		IF GNRempfaenger = 0 THEN			(* Ziel ist Hauptspeicher *)
			PutAchtelSeite (AAE + asnr*256, puffer) ;
		ELSE						(* Ziel ist Gebiet *)
			SchreibGebiet (GNRempfaenger, AAE + asnr, puffer) ;
		END ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
	END ;
END _SSR_3_68 ;


END SSR_3.
