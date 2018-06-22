
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SetzenLoeschen;

(*	11.05.16	*)

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM upFestkomma IMPORT * ;


PROCEDURE _BA ;
BEGIN
	mod2Check ;
	RegA.inh := tr24to48 (AktAdressteil) ;
	RegA.TK := 1 ;
	Takte (2) ;
END _BA;


PROCEDURE _BAR ;
BEGIN
	RegH := RegA ;
	_BA ;
	Takte (1) ;
END _BAR;


PROCEDURE _BAN ;
BEGIN
	_BA ;
	RegA.inh := trInvert (RegA.inh) ;
	Takte (3) ;
END _BAN;


PROCEDURE _BANR ;
BEGIN
	RegH := RegA ;
	_BAN ;
END _BANR;


PROCEDURE _LR ;
	VAR	tk :	CARDINAL ;
BEGIN
	mod2Check ;
	tk := AktAdressteil SHR 6 BAND 3 ;
	IF AktAdressteil BAND 20H <> 0 THEN
		RegA.inh := 0 ;
		RegA.TK := tk ;
	END ;
	IF AktAdressteil BAND 10H <> 0 THEN
		RegQ.inh := 0 ;
		RegQ.TK := tk ;
	END ;
	IF AktAdressteil BAND 8H <> 0 THEN
		RegD.inh := 0 ;
		RegD.TK := tk ;
	END ;
	IF AktAdressteil BAND 4H <> 0 THEN
		RegH.inh := 0 ;
		RegH.TK := tk ;
	END ;
	Takte (3) ;
END _LR;


PROCEDURE _LA ;
BEGIN			(* F2E3HTVM *)
	mod2Check ;
	IF ODD (AktAdressteil) THEN			(* M *)
		RegM := FALSE ;
	END ;
	IF AktAdressteil BAND 8 <> 0 THEN		(* H *)
		RegA.inh := RegA.inh BAND 000000000003FH ;
	END ;
	IF AktAdressteil BAND 4 <> 0 THEN		(* T *)
		RegA.inh := RegA.inh BAND 000000000000FH ;
	END ;

	IF AktAdressteil BAND 80H <> 0 THEN		(* F *)
		RegA.inh := RegA.inh BAND 00000000000FFH ;
	END ;
	IF AktAdressteil BAND 40H <> 0 THEN		(* 2 *)
		RegA.inh := RegA.inh BAND 0000000FFFFFFH ;
	END ;
	IF AktAdressteil BAND 20H <> 0 THEN		(* E *)
		RegA.inh := RegA.inh BAND 0FFFFFFFFFF00H ;
	END ;
	IF AktAdressteil BAND 10H <> 0 THEN		(* 3 *)
		RegA.inh := RegA.inh BAND 0FFFFFFFF0000H ;
	END ;
	IF AktAdressteil BAND 2H <> 0 THEN		(* V *)
		RegA.inh := RegA.inh BAND 03FFFFFFFFFFFH ;
	END ;
	Takte (4) ;
END _LA;


PROCEDURE _XBA ;
BEGIN
	mod2Ohne ;
	RegB := AktAdressteil ;
	Takte (1) ;
END _XBA;


PROCEDURE _XBAN ;
BEGIN
	_XBA ;
	RegB := trInvert24 (RegB) ;
	Takte (1) ;
END _XBAN;


PROCEDURE _ZX ;
	VAR	wert :	CARDINAL ;
BEGIN
	mod2Ohne ;
	wert := GetP8 () ;
	PutIndexZellenWert (AktAdressteil BAND 0FFH, wert) ;
	RegB := wert ;
	Takte (6) ;
END _ZX;


PROCEDURE _ZU ;
BEGIN
	mod2Check ;
	RegU := AktAdressteil BAND 0FFH ;
	Takte (1) ;
END _ZU;


PROCEDURE _ZTR ;
	VAR	tk :	CARDINAL ;
BEGIN
	mod2Check ;
	tk := AktAdressteil SHR 6 BAND 3 ;
	IF (AktAdressteil BAND 2) <> 0 THEN
		RegM :=  TRUE ;
	END ;
	CASE AktAdressteil BAND 03CH OF
	0 :
	|
	20H :	RegA.TK := tk ;
	|
	10H :	RegQ.TK := tk ;
	|
	8H :	RegD.TK := tk ;
	|
	4H :	RegH.TK := tk ;
	ELSE
		BefehlsAlarm ;
	END ;
	Takte (5) ;
END _ZTR;


PROCEDURE _ZT0 ;
BEGIN
	mod2Check ;
	ManipGanzwort (AktAdressteil, zt0) ;
	Takte (13) ;
END _ZT0;


PROCEDURE _ZT1 ;
BEGIN
	mod2Check ;
	ManipGanzwort (AktAdressteil, zt1) ;
	Takte (13) ;
END _ZT1;


PROCEDURE _ZT2 ;
BEGIN
	mod2Check ;
	ManipGanzwort (AktAdressteil, zt2) ;
	Takte (13) ;
END _ZT2;


PROCEDURE _ZT3 ;
BEGIN
	mod2Check ;
	ManipGanzwort (AktAdressteil, zt3) ;
	Takte (13) ;
END _ZT3;


PROCEDURE _LC ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Check ;
	wert := GetGanzwort (AktAdressteil) BAND MarkenBit ;
	PutGanzwort (AktAdressteil, AktTypenkennung, 0) ;
	Takte (13) ;
END _LC;


PROCEDURE _LMT ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Check ;
	wert := GetGanzwort (AktAdressteil) ;
	PutGanzwort (AktAdressteil, AktTypenkennung, MarkenBit) ;
	IF ORD(AktTypenkennung) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (13) ;
END _LMT;


PROCEDURE _LMC ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Check ;
	wert := GetGanzwort (AktAdressteil) BAND MarkenBitWeg ;
	PutGanzwort (AktAdressteil, AktTypenkennung, wert) ;
	IF ORD(AktTypenkennung) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (13) ;
END _LMC;


PROCEDURE _ZMC ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Check ;
	wert := GetGanzwort (AktAdressteil) BOR MarkenBit ;
	PutGanzwort (AktAdressteil, AktTypenkennung, wert) ;
	IF ORD(AktTypenkennung) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (13) ;
END _ZMC;


PROCEDURE _LZL ;
	VAR	sl,
		sr :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	sl := AktAdressteil SHR 8 ;
	sr := AktAdressteil BAND 0FFH ;
	RegK := (RegK BOR sl) BAND (BNOT sr) ;
	Takte (3) ;
END _LZL;


PROCEDURE _NL ;
	VAR	sr :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	sr := AktAdressteil BAND 0FFH ;
	RegK := RegK BXOR sr ;
	Takte (3) ;
END _NL;


PROCEDURE _ZI ;
BEGIN
	mod2Ohne ;
	ClearAssocIndex ;
	RegX := GetHalbwort (AktAdressteil) BAND 3FFFFFH ;
	XbasisPruefen ;
	Takte (58) ;
END _ZI;



END SetzenLoeschen.
