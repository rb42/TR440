
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Ersetzen;

(*	30.05.16	*)

FROM Befehle IMPORT
	internBefehlAusfuehren ;

FROM AsmDisasm IMPORT
	tOpcode, RzweitCodeErlaubt ;

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;
FROM upFestkomma IMPORT * ;



PROCEDURE _E ;
	VAR	ind :	CARDINAL8 ;
BEGIN
	AktBefehlscode := AktAdressteil SHR 8 ;
	ind := AktAdressteil ;
	AktAdressteil := GetIndexZellenWert (ind) ;
	internBefehlAusfuehren ;
	Takte (4) ;
END _E;


PROCEDURE _EZ ;
	VAR	ind :	CARDINAL8 ;
BEGIN
	AktBefehlscode := AktAdressteil SHR 8 ;
	ind := AktAdressteil ;
	RegB := trAdd24 (GetIndexZellenWert (ind), 2) ;
	AktAdressteil := trAdd24 (RegB, mod2) ;
	PutIndexZellenWert (ind, RegB) ;
	mod2 := 0 ;
	internBefehlAusfuehren ;
	Takte (5) ;
END _EZ;


PROCEDURE _ENZ ;
	VAR	ind :	CARDINAL8 ;
BEGIN
	AktBefehlscode := AktAdressteil SHR 8 ;
	ind := AktAdressteil ;
	AktAdressteil := trAdd24 (GetIndexZellenWert (ind), mod2) ;
	RegB := trSub24 (GetIndexZellenWert (ind), 2) ;
	PutIndexZellenWert (ind, RegB) ;
	mod2 := 0 ;
	internBefehlAusfuehren ;
	Takte (8) ;
END _ENZ;

PROCEDURE _EMB ;
	VAR	ind :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	AktBefehlscode := AktAdressteil SHR 8 ;
	ind := AktAdressteil ;
	AktAdressteil := GetIndexZellenWert (ind) ;
	mod2 := RegB ;
	internBefehlAusfuehren ;
	Takte (4) ;
END _EMB;


PROCEDURE _MAB ;
BEGIN
	mod2Ohne ;
	AktBefehlscode := AktAdressteil SHR 8 ;
	RegB := trAdd24 (RegB, tr8to48 (AktAdressteil)) ;
	AktAdressteil := RegB ;
	internBefehlAusfuehren ;
	Takte (9) ;
END _MAB;


PROCEDURE _MABI ;
BEGIN
	GrossSeitenInvarianz := TRUE ;
	_MAB ;
	Takte (1) ;
END _MABI;


PROCEDURE _MU ;
BEGIN
	mod2Ohne ;
	GrossSeitenInvarianz := TRUE ;
	MUmodifizierung := TRUE ;
	AktBefehlscode := AktAdressteil SHR 8 ;
	AktAdressteil := trAdd24 (GetIndexZellenWert (RegU), tr8to24 (AktAdressteil)) ;
	internBefehlAusfuehren ;
	Takte (15) ;
END _MU;


PROCEDURE _EMU ;
BEGIN
	GrossSeitenInvarianz := TRUE ;
	(*
	MUmodifizierung := TRUE ;
	*)
	AktBefehlscode := AktAdressteil SHR 8 ;
	AktAdressteil := GetHalbwort (trAdd24 (GetIndexZellenWert (RegU), tr8to24 (AktAdressteil))) ;
	internBefehlAusfuehren ;
	Takte (29) ;
END _EMU;


PROCEDURE _RLR ;
	VAR	wert :		CARDINAL ;
BEGIN
	AktBefehlscode := AktAdressteil SHR 8 ;
	wert := GetRegisterWertHW (AktAdressteil) ;
	AktAdressteil := trAdd24 (RegF, wert) ;
	internBefehlAusfuehren ;
	Takte (10) ;
END _RLR;


PROCEDURE _R ;
BEGIN
	mod2Check ;
	AktBefehlscode := AktAdressteil SHR 8 ;
	IF NOT (VAL (tOpcode, AktBefehlscode) IN RzweitCodeErlaubt) THEN
		BefehlsAlarm ;
	ELSE
		AktAdressteil := AktAdressteil BAND 0FFH ;
		R_Befehl := TRUE ;
		internBefehlAusfuehren ;
	END ;
	Takte (5) ;
END _R;


PROCEDURE _T ;
	VAR	bef :	CARDINAL ;
BEGIN
	bef := GetHalbwort (AktAdressteil) ;
	AktBefehlscode := bef SHR 16 ;
	AktAdressteil := bef BAND 0FFFFH ;
	internBefehlAusfuehren ;
	Takte (5) ;
END _T;



END Ersetzen.
