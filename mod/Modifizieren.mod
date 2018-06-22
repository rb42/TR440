
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Modifizieren;

(*	09.02.16	*)

IMPORT Terminal ;

FROM IndexArithmetik IMPORT
	_RX, _HXP, _HXX ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM upFestkomma IMPORT * ;


PROCEDURE _MF ;
BEGIN
	_MFU ;
	RegB := mod1 ;
END _MF;


PROCEDURE _MFU ;
BEGIN
	mod1 := trAdd24 (GetIndexZellenWert (AktAdressteil), mod2) ;
	mod2 := 0 ;
	Takte (1) ;
END _MFU;


PROCEDURE _MCF ;
BEGIN
	_MCFU ;
	RegB := mod1 ;
	Takte (1) ;
END _MCF;


PROCEDURE _MCFU ;
BEGIN
	mod1 := trAdd24 (GetHalbwort (AktAdressteil), mod2) ;
	mod2 := 0 ;
	Takte (1) ;
END _MCFU;


PROCEDURE _MD ;
BEGIN
	mod1 := trAdd24 (GetIndexZellenWert (AktAdressteil), mod2) ;
	mod2 := GetIndexZellenWert (AktAdressteil SHR 8) ;
	RegB := mod2 ;
	Takte (5) ;
END _MD;


PROCEDURE _M ;
BEGIN
	RegB := trAdd24 (GetIndexZellenWert (AktAdressteil), mod2) ;
	mod2 := RegB ;
	Takte (1) ;
END _M;


PROCEDURE _MH ;
	VAR	p :	CARDINAL ;
BEGIN
	_HXP ;
	mod2 := RegB ;
	Takte (1) ;
END _MH;


PROCEDURE _MHX ;
BEGIN
	_HXX ;
	mod2 := RegB ;
END _MHX;


PROCEDURE _MRX ;
BEGIN
	_RX ;
	mod2 := RegB ;
END _MRX;


PROCEDURE _MC ;
BEGIN
	RegB := trAdd24 (GetHalbwort (AktAdressteil), mod2) ;
	mod2 := RegB ;
	Takte (2) ;
END _MC;


PROCEDURE _MCE ;
	VAR	wert :	CARDINAL ;
		x :	CARDINAL ;
BEGIN
	wert := AktAdressteil ;
	x := 0 ;
	REPEAT
		INC (x) ;
		wert := GetHalbwort (wert BAND 3FFFFFH) ;
		IF Terminal.CtrlBreak OR AlarmGewesen THEN
			BREAK ;
		END ;
	UNTIL wert BAND 800000H <> 0 ;
	IF wert BAND 400000H = 0 THEN
		wert := wert BAND 7FFFFFH ;
	END ;
	RegB := trAdd24 (wert, mod2) ;
	mod2 := RegB ;
	Takte (x * 13 - 9) ;
END _MCE;


PROCEDURE _MA ;
BEGIN
	RegB := trAdd24 (AktAdressteil, mod2) ;
	mod2 := RegB ;
	Takte (1) ;
END _MA;


PROCEDURE _MNA ;
BEGIN
	RegB := trSub24 (mod2, AktAdressteil) ;
	mod2 := RegB ;
	Takte (2) ;
END _MNA;



END Modifizieren.
