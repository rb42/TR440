
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_1;

(*	12.06.18	*)


FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM AbwicklerUp IMPORT * ;


PROCEDURE _SSR_1_0 ;
	VAR	AA,
		TYP,
		LNG,
		i :		CARDINAL ;
		wort :	CARDINAL64 ;
		OLKnr :		OperatorNummer ;
BEGIN
	SSRaufruf ('Steuerinformation eintragen', 2) ;
	AA := GetHalbwort (VBL + 1) ;
	TYP := GetHalbwort (VBL + 3) ;
	LNG := TYP SHR 16 BAND 0FFH ;
	TYP := TYP BAND 0FFFFH ;

	wort := GetGanzwort (AA) ;
	OLKnr := SuchOperator (wort SHR 24) ;
	IF OLKnr <> 0 THEN
		WITH Operatoren [OLKnr].SteuerInformation DO
			Kopf.AbsOLK := AktOLK ;
			Kopf.LNG := LNG ;
			Kopf.TYP := TYP ;
			FOR i := 1 TO LNG DO
				GetGanzwortU (AA + (i - 1) * 2, Inhalt [i]) ;
				IF AlarmGewesen THEN
					BREAK ;
				END ;
			END ;
		END ;
	END ;
END _SSR_1_0 ;


PROCEDURE _SSR_1_4 ;
	VAR	AA,
		LNG,
		anz,
		i :		CARDINAL ;
BEGIN
	SSRaufruf ('Lies Steuerinformation', 2) ;
	AA := GetHalbwort (VBL + 2) ;
	LNG := GetHalbwort (VBL + 3) ;
	anz := LNG - 1 ;
	WITH Operatoren [AktOperator].SteuerInformation DO
		IF anz > Kopf.LNG THEN
			anz := Kopf.LNG ;
		END ;
		PutGanzwort (AA, 3, (VAL (CARDINAL64, Kopf.AbsOLK) SHL 24) BOR VAL (CARDINAL64, (Kopf.LNG) SHL 16 BOR Kopf.TYP)) ;
		FOR i := 1 TO anz DO
			PutGanzwortU (AA + i * 2, Inhalt [i]) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
		END ;
		IF (LNG < Kopf.LNG + 1) AND (LNG <> 1) THEN
			RegQ.inh := Kopf.LNG + 1 ;
			RegQ.TK := 1 ;
			SSRfehler (3, 'Puffer zu klein') ;
		END ;
	END ;
END _SSR_1_4 ;


PROCEDURE _SSR_1_8 ;
	VAR	WS :	CARDINAL ;
BEGIN
	SSRaufruf ('Wahlschalter setzen', 2) ;
	WS := GetHalbwort (VBL + 3) BAND WahlschalterVeraenderbar ;
	Wahlschalter := Wahlschalter BOR WS ;
	RegA.inh := Wahlschalter ;
	RegA.TK := 3 ;
END _SSR_1_8 ;


PROCEDURE _SSR_1_12 ;
	VAR	WS :	CARDINAL ;
BEGIN
	SSRaufruf ('Wahlschalter löschen', 2) ;
	WS := GetHalbwort (VBL + 3) BAND WahlschalterVeraenderbar ;
	Wahlschalter := Wahlschalter BAND (BNOT WS) ;
	RegA.inh := Wahlschalter ;
	RegA.TK := 3 ;
END _SSR_1_12 ;


PROCEDURE _SSR_1_16 ;
BEGIN
	SSRnimpl ('SSR 1 16') ;
END _SSR_1_16 ;


PROCEDURE _SSR_1_20 ;
BEGIN
	SSRaufruf ('Gespräch in Abschnitt umwandeln', 1) ;
	Wahlschalter := Wahlschalter	BAND 0FF7FFFH		(* Z1 = GSP ausschalten *)
					BOR     4000H ;		(* Z2 = Ablaufprotokoll für SSR 6 16 einschalten *)
	AbschnittsModus := TRUE ;
END _SSR_1_20 ;


END SSR_1.
