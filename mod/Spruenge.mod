
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Spruenge;

(*	27.01.16	*)

FROM WIN32 IMPORT
	Sleep ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM upFestkomma IMPORT * ;
FROM upGleitkomma IMPORT
	gkVgl ;

PROCEDURE springeX (bedingung : BOOLEAN) ;
BEGIN
	mod2Ohne ;
	IF bedingung THEN
		RegFbesetzen (AktAdressteil) ;
	END ;
END springeX ;


PROCEDURE springe (bedingung : BOOLEAN) ;
BEGIN
	mod2Check ;
	IF bedingung THEN
		RegFbesetzen (AktAdressteil) ;
	END ;
END springe ;


PROCEDURE springeRelativ (bedingung : BOOLEAN );
BEGIN
	IF bedingung THEN
		RegFbesetzen (trAdd24 (RegF, GetP8 ())) ;
	END ;
END springeRelativ ;


PROCEDURE _NULL ;
BEGIN
	mod2Ohne ;
	(* nix tun *)
	Takte (1) ;
END _NULL;


PROCEDURE _WB ;		(* (10 * Adressteil - 5)  Mikrosekunden pausieren *)
BEGIN
	mod2Ohne ;
	IF AktAdressteil = 0 THEN
		Takte (3) ;
	ELSE
		Takte ((tr24toInt32(AktAdressteil) * 10 - 5) * TakteProMikroSekunde) ;
		Sleep ((tr24toInt32(AktAdressteil) + 50) DIV 100) ;		(* Anzahl zu wartender Millisekunden *)
	END ;
END _WB;


PROCEDURE _S ;
BEGIN
	springe (TRUE) ;
	Takte (1) ;
END _S;


PROCEDURE _SE ;
BEGIN
	GrossSeitenInvarianz := TRUE ;
	RegFbesetzen (trAdd24 (GetHalbwort (AktAdressteil), mod2)) ;
	mod2 := 0 ;
	Takte (2) ;
END _SE;


PROCEDURE _SFB ;
BEGIN
	RegB := RegF + 1 ;
	_S ; ;
END _SFB;


PROCEDURE _SFBE ;
BEGIN
	RegB := RegF + 1 ;
	_SE ;
	Takte (13) ;
END _SFBE;


PROCEDURE _SU ;
BEGIN
	mod2Check ;
	IF RegU = 254 THEN
		U_Alarm ;
	END ;
	RegU := trAdd24 (RegU, 1) ;
	PutIndexZellenWert (RegU, RegF + 1) ;
	_S ;
	Takte (7) ;
END _SU;


PROCEDURE _SUE ;
BEGIN
	IF RegU = 254 THEN
		U_Alarm ;
	END ;
	RegU := trAdd24 (RegU, 1) ;
	PutIndexZellenWert (RegU, RegF + 1) ;
	_SE ;
	Takte (6) ;
END _SUE;


PROCEDURE txtVgl (a, b : CARDINAL64) : tVgl ;
BEGIN
	IF a < b THEN
		RETURN kleiner ;
	ELSIF a > b THEN
		RETURN groesser ;
	ELSE
		RETURN gleich ;
	END ;
END txtVgl ;


PROCEDURE chkVgl (a, b : CARDINAL64 ; TKa, TKb : CARDINAL8) : tVgl ;
BEGIN
	IF TKb > TKa THEN
		TKa := TKb ;
	END ;
	CASE TKa OF
	0 :	RETURN gkVgl (a, b) ;
	|
	1 :	RETURN trVgl (a, b) ;
	ELSE
		RETURN txtVgl (a, b) ;
	END ;
END chkVgl ;


PROCEDURE _SI0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) = gleich) ;
	Takte (1) ;
END _SI0;


PROCEDURE _SN0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) <> gleich) ;
	Takte (1) ;
END _SN0;


PROCEDURE _SGG0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) <> kleiner) ;
	Takte (1) ;
END _SGG0;


PROCEDURE _SG0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) = groesser) ;
	Takte (1) ;
END _SG0;


PROCEDURE _SKG0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) <> groesser) ;
	Takte (1) ;
END _SKG0;


PROCEDURE _SK0 ;
BEGIN
	springe (chkVgl (RegA.inh, 0, RegA.TK, 0) = kleiner) ;
	Takte (1) ;
END _SK0;


PROCEDURE _SR ;
BEGIN
	springe (ODD(RegA.inh)) ;
	Takte (1) ;
END _SR;


PROCEDURE _SRN ;
BEGIN
	springe (NOT ODD(RegA.inh)) ;
	Takte (1) ;
END _SRN;


PROCEDURE _SI ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) = gleich) ;
	Takte (6) ;
END _SI;


PROCEDURE _SN ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) <> gleich) ;
	Takte (6) ;
END _SN;


PROCEDURE _SGG ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) <> kleiner) ;
	Takte (6) ;
END _SGG;


PROCEDURE _SG ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) = groesser) ;
	Takte (6) ;
END _SG;


PROCEDURE _SKG ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) <> groesser) ;
	Takte (6) ;
END _SKG;


PROCEDURE _SK ;
BEGIN
	springe (chkVgl (RegA.inh, RegH.inh, RegA.TK, RegH.TK) = kleiner) ;
	Takte (6) ;
END _SK;


PROCEDURE _SXI ;
BEGIN
	springeX (tr24Gleich0 (RegB)) ;
	Takte (1) ;
END _SXI;


PROCEDURE _SXN ;
BEGIN
	springeX (tr24Ungleich0 (RegB)) ;
	Takte (1) ;
END _SXN;


PROCEDURE _SXGG ;
BEGIN
	springeX (tr24GroesserGleich0 (RegB)) ;
	Takte (1) ;
END _SXGG;


PROCEDURE _SXG ;
BEGIN
	springeX (tr24Groesser0 (RegB)) ;
	Takte (1) ;
END _SXG;


PROCEDURE _SXKG ;
BEGIN
	springeX (tr24KleinerGleich0 (RegB)) ;
	Takte (1) ;
END _SXKG;


PROCEDURE _SXK ;
BEGIN
	springeX (tr24Kleiner0 (RegB)) ;
	Takte (1) ;
END _SXK;


PROCEDURE _SXR ;
BEGIN
	springeX (ODD(RegB)) ;
	Takte (1) ;
END _SXR;


PROCEDURE _SXRN ;
BEGIN
	springeX (NOT ODD(RegB)) ;
	Takte (1) ;
END _SXRN;


PROCEDURE _SZX ;
	VAR	wert :	INTEGER ;
BEGIN
	wert := GetIndexZellenWert (AktAdressteil) ;
	IF tr24Kleiner0 (wert) THEN
		PutIndexZellenWert (AktAdressteil, trAdd24 (wert, 1)) ;
		springeRelativ (TRUE) ;
	END ;
	Takte (11) ;
END _SZX;


PROCEDURE _SM ;
BEGIN
	springe (RegM) ;
	RegM := FALSE ;
	Takte (1) ;
END _SM;


PROCEDURE _SMN ;
BEGIN
	springe (NOT RegM) ;
	RegM := FALSE ;
	Takte (1) ;
END _SMN;


PROCEDURE _SAA ;
BEGIN
	mod2Check ;
	springe (BUEalarmErkannt) ;
	BUEalarmErkannt := FALSE ;
	Takte (1) ;
END _SAA;


PROCEDURE _SAT ;
BEGIN
	mod2Check ;
	springe (TKalarmErkannt) ;
	TKalarmErkannt := FALSE ;
	Takte (1) ;
END _SAT;


PROCEDURE st (ja : BOOLEAN) ;
	VAR	tk :	CARDINAL8 ;
BEGIN
	mod2Check ;
	IF AktAdressteil BAND 2 <> 0 THEN
		springeRelativ (TRUE) ;
	ELSE
		tk := (AktAdressteil SHR 6) BAND 3 ;
		CASE AktAdressteil SHR 2 BAND 0FH OF
		1 :	springeRelativ ((RegH.TK = tk) = ja) ;
		|
		2 :	springeRelativ ((RegD.TK = tk) = ja) ;
		|
		4 :	springeRelativ ((RegQ.TK = tk) = ja) ;
		|
		8 :	springeRelativ ((RegA.TK = tk) = ja) ;
		ELSE
			BefehlsAlarm ;
		END ;
	END ;
END st ;


PROCEDURE _ST ;
BEGIN
	st (TRUE) ;
	Takte (10) ;
END _ST;


PROCEDURE _STN ;
BEGIN
	st (FALSE) ;
	Takte (10) ;
END _STN;


PROCEDURE _SBIT ;
	VAR	bitnr,
		i :	INTEGER ;
		wert :	CARDINAL64 ;
BEGIN
	mod2Ohne ;
	bitnr := AktAdressteil BAND 3FH ;
	CASE AktAdressteil SHR 6 BAND 3 OF
	0 :	wert := RegA.inh ;
	|
	1 :	wert := RegQ.inh ;
	|
	2 :	wert := RegD.inh ;
	|
	3 :	wert := RegH.inh ;
	ELSE
	END ;
	IF bitnr > 48 THEN
		BefehlsAlarm ;
	END ;
	springeRelativ (wert BAND Bitmasken48 [bitnr] <> 0) ;
	Takte (16 - ((bitnr-1) MOD 16)) ;
END _SBIT;


PROCEDURE _SEGG ;
BEGIN
	mod2Check ;
	RegD := RegA ;
	springeRelativ (trVgl (tr8to48 (RegA.lo), tr8to48(AktAdressteil)) <> kleiner) ;
	IF RegA.TK <> 0 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (10) ;
END _SEGG;


PROCEDURE sl (ja : BOOLEAN) : BOOLEAN ;
BEGIN
	mod2Ohne ;
	IF (((ORD(RegK) BAND AktAdressteil) <> 0) = ja) THEN
		springeRelativ (TRUE) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END sl ;


PROCEDURE _SL ;
BEGIN
	sl (TRUE) ; ;
	Takte (1) ;
END _SL;


PROCEDURE _SLL ;
BEGIN
	IF sl (TRUE) THEN
		RegK := ORD(RegK) BAND (AktAdressteil BXOR 0FFH) ;
	END ;
	Takte (1) ;
END _SLL;


PROCEDURE _SLN ;
BEGIN
	sl (FALSE) ;
	Takte (1) ;
END _SLN;


PROCEDURE _SNL ;
BEGIN
	IF NOT sl (FALSE) THEN
		RegK := ORD(RegK) BAND (AktAdressteil BXOR 0FFH) ;
	END ;
	Takte (1) ;
END _SNL;


PROCEDURE _PDP ;
BEGIN
	mod2Check ;
	RegA.inh := GetGanzwort (AktAdressteil) ;
	RegA.TK := AktTypenkennung ;
	RegY := AktTypenkennung ;
	RegFbesetzen (RegF+2) ;
	Takte (21) ;
END _PDP;


END Spruenge.
