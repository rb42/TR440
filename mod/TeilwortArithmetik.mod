
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE TeilwortArithmetik;

(*	26.08.16	*)

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM upVLI IMPORT * ;
FROM upFestkomma IMPORT * ;

FROM VLIkern IMPORT
	Add,
	Subtract,
	Multiply,
	Divide ;

FROM TransportBefehle IMPORT
	_BD ;


PROCEDURE RegZus (wert : CARDINAL64) ;
BEGIN
	IF ORD (RegD.TK) > ORD (RegA.TK) THEN
		RegA.TK := RegD.TK ;
	END ;
	IF RegA.TK < 2 THEN
		RegA.inh := tr48Norm (wert) ;
	ELSE
		RegA.inh := wert BAND Cardinal48Bit ;
	END ;
	Takte (2) ;
END RegZus ;


PROCEDURE _VEL ;
BEGIN
	_BD ;
	RegZus (RegA.inh BOR RegD.inh) ;
END _VEL;


PROCEDURE _AUT ;
BEGIN
	_BD ;
	RegZus (RegA.inh BXOR RegD.inh) ;
END _AUT;


PROCEDURE _ET ;
BEGIN
	_BD ;
	RegZus (RegA.inh BAND RegD.inh) ;
END _ET;


PROCEDURE _ZUS ;
BEGIN
	_BD ;
	RegZus ((RegA.inh BAND (BNOT RegH.inh)) BOR (RegD.inh BAND RegH.inh)) ;
	Takte (3) ;
END _ZUS;


PROCEDURE _VLA ;
BEGIN
	mod2Check ;
	RegD.inh := trHW (AktAdressteil) ;
	RegD.TK := 1 ;
	RegA.inh := RegH.inh BOR RegD.inh ;
	RegA.TK := RegH.TK ;
	Takte (8) ;
END _VLA;


PROCEDURE _ATA ;
BEGIN
	mod2Check ;
	RegD.inh := trHW (AktAdressteil) ;
	RegD.TK := 1 ;
	RegA.inh := RegH.inh BXOR RegD.inh ;
	RegA.TK := RegH.TK ;
	Takte (8) ;
END _ATA;


PROCEDURE _ETA ;
BEGIN
	mod2Check ;
	RegD.inh := trHW (AktAdressteil) ;
	RegD.TK := 1 ;
	RegA.inh := RegH.inh BAND RegD.inh ;
	RegA.TK := RegH.TK ;
	Takte (8) ;
END _ETA;


PROCEDURE _A2 ;
BEGIN
	mod2Check ;
	RegA.inh := tr24to48 (trAdd24 (RegA.inh, GetHalbwort (AktAdressteil))) ;
	RegD.TK := RegA.TK ;
	RegD.inh := 0 ;
	CheckUeberlauf (RegA) ;
	Takte (14) ;
END _A2;


PROCEDURE _SB2 ;
BEGIN
	mod2Check ;
	RegA.inh := tr24to48 (trSub24 (RegA.inh, GetHalbwort (AktAdressteil))) ;
	RegD.TK := RegA.TK ;
	RegD.inh := 0 ;
	CheckUeberlauf (RegA) ;
	Takte (14) ;
END _SB2;


PROCEDURE _M2 ;
BEGIN
	mod2Check ;
	RegA.inh := trMult (RegA.inh, tr24to48 (GetHalbwort (AktAdressteil))) ;
	RegD.TK := RegA.TK ;
	RegD.inh := 0 ;
	RegQ := RegD ;
	RegY := 0 ;
	CheckUeberlauf (RegA) ;
	Takte (50) ;
END _M2;


PROCEDURE _M2N ;
BEGIN
	mod2Check ;
	RegA.inh := trMult (trInvert (RegA.inh), tr24to48 (GetHalbwort (AktAdressteil))) ;
	RegD.TK := RegA.TK ;
	RegD.inh := 0 ;
	RegQ := RegD ;
	RegY := 0 ;
	CheckUeberlauf (RegA) ;
	Takte (50) ;
END _M2N;


PROCEDURE _M2R ;
BEGIN
	mod2Check ;
	InitVLI ;
	EinfachToVLI (RegA, VLIa) ;
	wert48ToVLI (tr24to48(GetHalbwort (AktAdressteil)), VLIb) ;
	Multiply (VLIa, VLIb, VLIc) ;
	wert48ToVLI (400000H, VLId) ;
	Divide (VLIc, VLId, VLIa, VLIb) ;
	VLItoEinfach (VLIa, RegA) ;
	VLItoEinfach (VLIb, RegQ) ;
	IF trNegativ (RegA.inh) THEN
		IF (RegQ.inh BAND 200000H) = 0 THEN
			RegA.inh := trSub (2, RegA.inh, 1) ;
		END ;
	ELSE
		IF (RegQ.inh BAND 200000H) <> 0 THEN
			RegA.inh := trAdd (2, RegA.inh, 1) ;
		END ;
	END ;
	IF (RegA.TK <> 1) OR (AktTypenkennung <> 1) THEN
		TypenkennungsAlarm (RegF) ;
	END ;
	RegD.TK := RegA.TK ;
	RegD.inh := 0 ;
	RegQ := RegD ;
	RegY := 0 ;
	Takte (37) ;
END _M2R;


PROCEDURE _M2NR ;
BEGIN
	trInvert (RegA.inh) ;
	_M2R ;
END _M2NR;


PROCEDURE upATSBT (wert : CARDINAL64) ;
	VAR	maske :	CARDINAL64 ;
		p :	CARDINAL ;
BEGIN
	maske := RegQ.inh ;
	RegD := RegQ ;
	p := 0 ;
	IF (maske BAND 0FFFFFFFFFFFFH) <> 0FFFFFFFFFFFFH THEN
		WHILE ODD (maske) DO
			INC (p) ;
			maske := maske SHR 1 ;
			wert := wert SHR 1 ;
		END ;
	END ;
	wert := (wert BOR maske) + (RegA.inh BAND BNOT maske) ;
	RegA.inh := wert BAND maske ;
	Takte (p * 2 + 21) ;
END upATSBT ;


PROCEDURE _AT ;
BEGIN
	mod2Check ;
	upATSBT (GetGanzwort(AktAdressteil)) ;
END _AT;


PROCEDURE _SBT ;
BEGIN
	mod2Check ;
	upATSBT (BNOT GetGanzwort(AktAdressteil)) ;
END _SBT;


END TeilwortArithmetik.
