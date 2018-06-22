
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE GleitkommaArithmetik;

(*	07.02.16	*)


FROM Struktur IMPORT * ;

FROM Register IMPORT * ;

FROM upFestkomma IMPORT * ;


FROM upGleitkomma IMPORT
	gkTR440toIntel,
	gkIntelToTR440,
	gkIntelToTR440runden ;

VAR
	opA,
	opD,
	opH,
	opQ,
	opAQ,
	op1,
	op2 :	LONGREAL ;

	wort,
	wort2 :	CARDINAL64 ;



PROCEDURE op_get_A ;
BEGIN
	mod2Check ;

	RegA.inh := GetGanzwort (AktAdressteil) ;
	RegA.TK := AktTypenkennung ;
	CheckMarke (RegA) ;
END op_get_A ;


PROCEDURE op_get_D ;
BEGIN
	mod2Check ;

	RegD.inh := GetGanzwort (AktAdressteil) ;
	RegD.TK := AktTypenkennung ;
	CheckMarke (RegD) ;
END op_get_D ;


PROCEDURE op_get_1 ;
BEGIN
	mod2Ohne ;

	wort := GetGanzwort (AktAdressteil) ;
	IF AktTypenkennung <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	IF NOT gkTR440toIntel (wort, 0, op1) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	RegD.inh := 0 ;
	RegD.TK := 1 ;
END op_get_1 ;


PROCEDURE op_get_2 ;
BEGIN
	mod2Ohne ;

	wort := GetGanzwort (AktAdressteil) ;
	IF AktTypenkennung <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	wort2 := GetGanzwort (AktAdressteil + 2) ;
	IF AktTypenkennung <> 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	IF NOT gkTR440toIntel (wort, wort2, op2) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	RegD.inh := 0 ;
	RegD.TK := 1 ;
END op_get_2 ;


PROCEDURE op_conv_A ;
BEGIN
	IF NOT gkTR440toIntel (RegA.inh, 0, opA) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	IF RegA.TK <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
END op_conv_A ;


PROCEDURE op_conv_H ;
BEGIN
	IF NOT gkTR440toIntel (RegH.inh, 0, opH) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	IF RegH.TK <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
END op_conv_H ;


PROCEDURE op_conv_D ;
BEGIN
	IF NOT gkTR440toIntel (RegD.inh, 0, opD) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	IF RegD.TK <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
END op_conv_D ;


PROCEDURE op_conv_AQ ;
BEGIN
	IF NOT gkTR440toIntel (RegA.inh, RegQ.inh, opAQ) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	IF RegA.TK <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	IF RegQ.TK <> 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
END op_conv_AQ ;


PROCEDURE op_conv_A_D ;
BEGIN
	op_conv_A ;
	op_conv_D ;
END op_conv_A_D ;


PROCEDURE op_A_D ;
BEGIN
	op_get_D ;
	op_conv_A_D ;
END op_A_D ;


PROCEDURE end_A (op : LONGREAL ; takte : CARDINAL) ;
BEGIN
	IF NOT gkIntelToTR440runden (op, RegA.inh) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	RegQ.inh := 0 ;
	RegQ.TK := 0 ;
	Takte (takte) ;
END end_A ;


PROCEDURE end_AQ (op : LONGREAL ; takte : CARDINAL) ;
BEGIN
	IF NOT gkIntelToTR440 (op, RegA.inh, RegQ.inh) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	Takte (takte) ;
END end_AQ ;


PROCEDURE _GA ;
BEGIN
	op_A_D ;
	end_A (opA + opD, 28) ;
END _GA;


PROCEDURE _GAB ;
BEGIN
	op_A_D ;
	end_A (opA + ABS(opD), 28) ;
END _GAB;


PROCEDURE _GAC ;
BEGIN
	op_A_D ;
	end_A (opA + opD, 37) ;
	RegD := RegA ;
	PutGanzwort (AktAdressteil, 0, RegA.inh) ;
END _GAC;


PROCEDURE _GSB ;
BEGIN
	op_A_D ;
	end_A (opA - opD, 28) ;
END _GSB;


PROCEDURE _GSBB ;
BEGIN
	op_A_D ;
	end_A (opA - ABS(opD), 28) ;
END _GSBB;


PROCEDURE _GSBC ;
	VAR	gw1,
		gw2 :	CARDINAL64 ;
		gk :	LONGREAL ;
BEGIN
	op_get_D ;
	op_conv_A_D ;
	opD := opD - opA ;
	IF NOT gkIntelToTR440runden (opD , RegD.inh) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	RegQ.inh := 0 ;
	RegQ.TK := 0 ;
	PutGanzwort (AktAdressteil, 0, RegD.inh) ;
	Takte (37) ;
END _GSBC;


PROCEDURE _GSBI ;
BEGIN
	op_A_D ;
	end_A (opD - opA, 28) ;
END _GSBI;


PROCEDURE _GSBD ;
BEGIN
	op_get_A ;
	op_conv_A_D ;
	end_A (opD - opA, 30) ;
END _GSBD;


PROCEDURE _GML ;
BEGIN
	op_A_D ;
	end_A (opA * opD, 54) ;
END _GML;


PROCEDURE _GMLN ;
BEGIN
	op_A_D ;
	end_A ((- opA) * opD, 54) ;
END _GMLN;


PROCEDURE _GMLA ;
BEGIN
	op_A_D ;
	op_conv_H ;
	end_A (opA * opD + opH, 97) ;
END _GMLA;


PROCEDURE _GMAN ;
BEGIN
	op_A_D ;
	end_A ((- opA) * opD + opH, 97) ;
	op_conv_H ;
END _GMAN;


PROCEDURE _GDV ;
BEGIN
	op_A_D ;
	end_A (opA / opD, 213) ;
	RegD.inh := 0 ;
END _GDV;


PROCEDURE _GDVI ;
BEGIN
	op_A_D ;
	end_A (opD / opA, 216) ;
	RegD.inh := 0 ;
END _GDVI;


PROCEDURE angleichen_A_D ;
	VAR	expA,
		expD :	INTEGER ;
BEGIN
	expA := tr8to24 (RegA.inh) ;
	expD := tr8to24 (RegD.inh) ;
	wort := RegD.inh ;
	WHILE expA > expD DO
		wort := wort SHR 4 ;
		INC (expD) ;
	END ;
	WHILE expA < expD DO
		RegA.inh := RegA.inh SHR 4 ;
		INC (expA) ;
	END ;
	RegA.inh := (RegA.inh BAND 0FFFFFFFFFF00H) BOR VAL(CARDINAL64, expA BAND 0FFH) ;
END angleichen_A_D ;


PROCEDURE _AU ;
BEGIN
	op_get_D ;
	IF (RegD.TK <> 0) OR (RegA.TK <> 0) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	angleichen_A_D ;
	RegA.inh := VAL (CARDINAL64, trAdd (2, RegA.inh BAND 0FFFFFFFFFF00H, wort BAND 0FFFFFFFFFF00H)) BOR (RegA.inh BAND 0FFH) ;
	Takte (26) ;
END _AU;


PROCEDURE _SBU ;
BEGIN
	op_get_D ;
	IF (RegD.TK <> 0) OR (RegA.TK <> 0) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	angleichen_A_D ;
	RegA.inh := VAL (CARDINAL64, trSub (2, RegA.inh BAND 0FFFFFFFFFF00H, wort BAND 0FFFFFFFFFF00H)) BOR (RegA.inh BAND 0FFH) ;
	Takte (26) ;
END _SBU;


PROCEDURE _REZ ;
BEGIN
	op_get_D ;
	op_conv_D ;
	end_A (1.0E0 / opD, 213) ;
	RegD.inh := 0 ;
END _REZ;


PROCEDURE _DA ;
BEGIN
	op_get_2 ;
	op_conv_AQ ;
	end_AQ (opAQ + op2, 106) ;
END _DA;


PROCEDURE _DSB ;
BEGIN
	op_get_2 ;
	op_conv_AQ ;
	end_AQ (opAQ - op2, 106) ;
END _DSB;


PROCEDURE _DML ;
BEGIN
	op_get_2 ;
	op_conv_AQ ;
	end_AQ (opAQ * op2, 243) ;
END _DML;


PROCEDURE _MLD ;
BEGIN
	op_get_1 ;
	op_conv_A ;
	end_AQ (opA * op1, 64) ;
END _MLD;


END GleitkommaArithmetik.
