
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE IndexArithmetik;

(*	08.01.16	*)

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM upFestkomma IMPORT * ;


PROCEDURE _HBC ;
BEGIN
	mod2Check ;
	RegB := trAdd24 (RegB, GetHalbwort (AktAdressteil)) ;
	Takte (6) ;
END _HBC;


PROCEDURE _VBC ;
BEGIN
	mod2Check ;
	RegB := trSub24 (RegB, GetHalbwort (AktAdressteil)) ;
	Takte (9) ;
END _VBC;


PROCEDURE _HBA ;
BEGIN
	mod2Ohne ;
	RegB := trAdd24 (RegB, AktAdressteil) ;
	Takte (7) ;
END _HBA;


PROCEDURE _VBA ;
BEGIN
	mod2Ohne ;
	RegB := trSub24 (RegB, AktAdressteil) ;
	Takte (9) ;
END _VBA;


PROCEDURE _HBPX ;
	VAR	p2 :		CARDINAL8 ;
		faktor :	INTEGER ;
BEGIN
	mod2Ohne ;
	faktor := GetP8 () ;
	p2 := AktAdressteil ;
	IF faktor = 1 THEN
		RegB := trAdd24 (RegB, GetIndexZellenWert(p2)) ;
	ELSIF tr24toInt32 (faktor) = -1 THEN
		RegB := trSub24 (RegB, GetIndexZellenWert(p2)) ;
	ELSE
		RegB := trAdd24 (RegB, trMult (tr24to48 (GetIndexZellenWert(p2)), tr24to48 (faktor))) ;
	END ;
	Takte (trAbs24(faktor) * 6 + 3) ;
END _HBPX;


PROCEDURE _HXP ;
BEGIN
	mod2Ohne ;
	RegB := trAdd24 (GetIndexZellenWert (AktAdressteil), GetP8 ()) ;
	PutIndexZellenWert (AktAdressteil, RegB) ;
	Takte (10) ;
END _HXP;


PROCEDURE _HXX ;
	VAR	p2 :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	p2 := AktAdressteil ;
	RegB := trAdd24 (GetIndexZellenWert (p2), GetIndexZellenWert (AktAdressteil SHR 8)) ;
	PutIndexZellenWert (p2, RegB) ;
	Takte (15) ;
END _HXX;


PROCEDURE _VXX ;
	VAR	p2 :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	p2 := AktAdressteil ;
	RegB := trSub24 (GetIndexZellenWert (p2), GetIndexZellenWert (AktAdressteil SHR 8)) ;
	PutIndexZellenWert (p2, RegB) ;
	Takte (14) ;
END _VXX;


PROCEDURE _RX ;
	VAR	wert :		INTEGER ;
		p1,
		p2 :		CARDINAL8 ;
		negativ,
		speichern :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	p1 := AktAdressteil SHR 8 ;
	p2 := AktAdressteil ;
	negativ := (ORD (p1) BAND 4) <> 0 ;
	speichern := ODD (ORD (p1)) ;
	CASE p1 BAND 0F8H OF
	80H :	wert := RegA.lo ;
	|
	40H :	wert := RegQ.lo ;
	|
	20H :	wert := RegD.lo ;
	|
	10H :	wert := RegH.lo ;
	|
	08H :	wert := RegB ;
	ELSE
		BefehlsAlarm ;
	END ;
	IF negativ THEN
		RegB := trSub24 (GetIndexZellenWert (p2), wert) ;
	ELSE
		RegB := trAdd24 (GetIndexZellenWert (p2), wert) ;
	END ;
	IF speichern THEN
		PutIndexZellenWert (p2, RegB) ;
	END ;
	Takte (12) ;
END _RX;



END IndexArithmetik.
