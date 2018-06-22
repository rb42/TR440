
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE FestkommaArithmetik;

(*	15.02.16	*)

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;
FROM upFestkomma IMPORT * ;

FROM TransportBefehle IMPORT
	_B, _BD ;

FROM upVLI IMPORT * ;

FROM VLIkern IMPORT
	Multiply,
	Divide,
	Add,
	Subtract ;

PROCEDURE _A ;
BEGIN
	_BD ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trAdd (RegA.TK, RegA.inh, RegD.inh) ;
	CheckUeberlauf (RegA) ;
	Takte (5) ;
END _A;


PROCEDURE _AB ;
BEGIN
	_BD ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trAdd (RegA.TK, RegA.inh, trAbs (RegD.inh)) ;
	CheckUeberlauf (RegA) ;
	Takte (5) ;
END _AB;


PROCEDURE _AC ;
BEGIN
	_BD ;
	IF RegA.TK > RegD.TK THEN
		RegD.TK := RegA.TK ;
	END ;
	RegD.inh := trAdd (RegD.TK, RegA.inh, RegD.inh) ;
	CheckUeberlauf (RegD) ;
	PutGanzwort (AktAdressteil, RegD.TK, AktWertFuerSpeicher) ;
	Takte (14) ;
END _AC;


PROCEDURE _SB ;
BEGIN
	_BD ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trSub (RegA.TK, RegA.inh, RegD.inh) ;
	CheckUeberlauf (RegA) ;
	Takte (5) ;
END _SB;


PROCEDURE _SBB ;
BEGIN
	_BD ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trSub (RegA.TK, RegA.inh, trAbs (RegD.inh)) ;
	CheckUeberlauf (RegA) ;
	Takte (5) ;
END _SBB;


PROCEDURE _SBC ;
BEGIN
	_BD ;
	IF RegA.TK > RegD.TK THEN
		RegD.TK := RegA.TK ;
	END ;
	RegD.inh := trSub (RegD.TK, RegD.inh, RegA.inh) ;
	CheckUeberlauf (RegD) ;
	PutGanzwort (AktAdressteil, RegD.TK, AktWertFuerSpeicher) ;
	Takte (14) ;
END _SBC;


PROCEDURE _SBI ;
BEGIN
	_BD ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trSub (RegA.TK, RegD.inh, RegA.inh) ;
	CheckUeberlauf (RegA) ;
	Takte (5) ;
END _SBI;


PROCEDURE _SBD ;
BEGIN
	_B ;
	IF RegD.TK > RegA.TK THEN
		RegA.TK := RegD.TK ;
	END ;
	RegA.inh := trSub (RegA.TK, RegD.inh, RegA.inh) ;
	CheckUeberlauf (RegA) ;
	Takte (8) ;
END _SBD;


PROCEDURE _MLR ;
	VAR	hilfsreg :	reg48 ;
BEGIN
	_BD ;
	InitVLI ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	EinfachToVLI (RegA, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;
	Multiply (VLIa, VLIb, VLIc) ;

	VLItoDoppelt (VLIc, RegA, hilfsreg) ;
	IF trAbs (hilfsreg.inh) >= 200000000000H THEN	(* Rundung *)
		IF trNegativ (RegA.inh) THEN
			RegA.inh := trSub (1, RegA.inh, 1) ;
		ELSE
			RegA.inh := trAdd (1, RegA.inh, 1) ;
		END ;
	END ;

	RegQ.TK := 1 ;
	RegQ.inh := 0 ;

	CheckUeberlauf (RegA) ;
	Takte (57) ;
END _MLR;


PROCEDURE _MNR ;
BEGIN
	RegA.inh := trInvert (RegA.inh) ;
	_MLR ;
END _MNR;


PROCEDURE _MAR ;
BEGIN
	_MLR ;
	RegA.inh := trAdd (1, RegA.inh, RegH.inh) ;
	RegA.TK := 1 ;
	Takte (10) ;
END _MAR;


PROCEDURE _MANR ;
BEGIN
	RegA.inh := trInvert (RegA.inh) ;
	_MAR ;
END _MANR;


PROCEDURE Check32bitVerschiebung (VAR wert : CARDINAL64) ;
BEGIN
	IF trNegativ (wert) THEN
		IF (wert BAND 0FFFFFFFFH) = 0FFFFFFFFH THEN
			wert := trInvert (trInvert (wert) SHR 32) ;
			wert48ToVLI (4000H, VLId) ;
		ELSE
			wert48ToVLI (400000000000H, VLId) ;
		END ;
	ELSIF (wert BAND 0FFFFFFFFH) = 0 THEN
		wert := wert SHR 32 ;
		wert48ToVLI (4000H, VLId) ;
	ELSE
		wert48ToVLI (400000000000H, VLId) ;
	END ;
END Check32bitVerschiebung ;


PROCEDURE _DV ;
	VAR	wert :	CARDINAL64 ;
		ok :	BOOLEAN ;
BEGIN
	InitVLI ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	IF NOT (trVglAbs (RegA.inh, RegD.inh) = kleiner) THEN
		RegA.TK := 0 ;
		ArithmetischerAlarm ;
		RETURN ;
	END ;

	wert := RegD.inh ;
	Check32bitVerschiebung (wert) ;
	wert48ToVLI (wert, VLIb) ;

	EinfachToVLI (RegA, VLIc) ;
	Multiply (VLIc, VLId, VLIa) ;

	Divide (VLIa, VLIb, VLIc, VLId) ;

	ok := TRUE ;
	IF NOT VLItoEinfach (VLIc, RegA) THEN
		ok := FALSE ;
	END ;
	IF NOT VLItoEinfach (VLId, RegQ) THEN
		ok := FALSE ;
	END ;

	IF NOT ok THEN
		ArithmetischerAlarm ;
	END ;

	RegD.inh := 0 ;
	RegD.TK := 1 ;
	Takte (220) ;
END _DV;


PROCEDURE _DVI ;
	VAR	wert :	CARDINAL64 ;
		ok :	BOOLEAN ;
BEGIN
	InitVLI ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	IF NOT (trVglAbs (RegD.inh, RegA.inh) = kleiner) THEN
		RegA.TK := 0 ;
		ArithmetischerAlarm ;
		RETURN ;
	END ;

	wert := RegA.inh ;
	Check32bitVerschiebung (wert) ;
	wert48ToVLI (wert, VLIa) ;

	EinfachToVLI (RegD, VLIc) ;
	Multiply (VLIc, VLId, VLIb) ;

	Divide (VLIb, VLIa, VLIc, VLId) ;

	ok := TRUE ;
	IF NOT VLItoEinfach (VLIc, RegA) THEN
		ok := FALSE ;
	END ;
	IF NOT VLItoEinfach (VLId, RegQ) THEN
		ok := FALSE ;
	END ;

	IF NOT ok THEN
		ArithmetischerAlarm ;
	END ;

	RegD.inh := 0 ;
	RegD.TK := 1 ;
	Takte (220) ;
END _DVI;


PROCEDURE _DVD ;
	VAR	ok :	BOOLEAN ;
BEGIN
	InitVLI ;
	_BD ;

	DoppeltToVLI (RegA, RegQ, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	IF (RegA.TK <> 1) OR (RegQ.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	IF NOT (trVglAbs (RegA.inh, RegD.inh) = kleiner) THEN
		RegA.TK := 0 ;
		ArithmetischerAlarm ;
		RETURN ;
	END ;

	Divide (VLIa, VLIb, VLIc, VLId) ;

	ok := TRUE ;
	IF NOT VLItoEinfach (VLIc, RegA) THEN
		ok := FALSE ;
	END ;
	IF NOT VLItoEinfach (VLId, RegQ) THEN
		ok := FALSE ;
	END ;

	IF NOT ok THEN
		ArithmetischerAlarm ;
	END ;

	RegD.inh := 0 ;
	RegD.TK := 1 ;
	Takte (228) ;
END _DVD;


PROCEDURE _AQ ;
BEGIN
	(*
	InitVLI ;
	_BD ;
	DoppeltToVLI (RegA, RegQ, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	IF (RegA.TK <> 1) OR (RegQ.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;

	Add (VLIa, VLIb, VLIc) ;
	VLItoDoppelt (VLIc, RegA, RegQ) ;
	*)

	_BD ;

	IF (RegA.TK <> 1) OR (RegQ.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	RegQ.inh := trAdd (1, RegQ.inh, RegD.inh) ;
	Takte (14) ;
END _AQ;


PROCEDURE _SBQ ;
BEGIN
	(*
	InitVLI ;
	_BD ;
	DoppeltToVLI (RegA, RegQ, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	IF (RegA.TK <> 1) OR (RegQ.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;

	Subtract (VLIa, VLIb, VLIc) ;
	VLItoDoppelt (VLIc, RegA, RegQ) ;
	*)

	_BD ;

	IF (RegA.TK <> 1) OR (RegQ.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;
	RegQ.inh := trSub (1, RegQ.inh, RegD.inh) ;
	Takte (14) ;
END _SBQ;


PROCEDURE _ML ;
BEGIN
	InitVLI ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	EinfachToVLI (RegA, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	Multiply (VLIa, VLIb, VLIc) ;
	VLItoDoppelt (VLIc, RegA, RegQ) ;
	Takte (52) ;
END _ML;


PROCEDURE _MLN ;
BEGIN
	InitVLI ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	wert48ToVLI (trInvert (RegA.inh), VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	Multiply (VLIa, VLIb, VLIc) ;
	VLItoDoppelt (VLIc, RegA, RegQ) ;
	Takte (52) ;
END _MLN;


PROCEDURE _MLA ;
BEGIN
	InitVLI ;
	DoppeltToVLI (RegH, RegQ, VLId) ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	EinfachToVLI (RegA, VLIa) ;
	EinfachToVLI (RegD, VLIb) ;

	Multiply (VLIa, VLIb, VLIc) ;
	Add (VLIc, VLId, VLIa) ;
	VLItoDoppelt (VLIa, RegA, RegQ) ;
	Takte (63) ;
END _MLA;


PROCEDURE _MAN ;
BEGIN
	InitVLI ;
	_BD ;

	IF (RegA.TK <> 1) OR (RegD.TK <> 1) THEN
		TypenkennungsAlarm (AktAdressteil) ;
		RETURN ;
	END ;

	DoppeltToVLI (RegH, RegQ, VLId) ;
	wert48ToVLI (trInvert (RegA.inh), VLIa) ;
	EinfachToVLI (RegD, VLIb) ;
	Multiply (VLIa, VLIb, VLIc) ;
	Add (VLIc, VLId, VLIa) ;
	VLItoDoppelt (VLIa, RegA, RegQ) ;
	Takte (63) ;
END _MAN;


PROCEDURE _AA ;
	VAR	zw :	CARDINAL64 ;
BEGIN
	mod2Check ;
	CASE RegA.TK OF
	0 :	IF trAbs24 (AktAdressteil) > 127 THEN
			ArithmetischerAlarm ;
		END ;
		zw := trAdd (1, tr8to48(RegA.inh),  tr8to48 (AktAdressteil)) ;
		IF trAbs (zw) > 127 THEN
			ArithmetischerAlarm ;
		END ;
		RegA.inh := (RegA.inh BAND MantissenMaske) BOR (zw BAND ExponentenMaske) ;
		RegD.inh := AktAdressteil BAND 0FFH ;
	|
	1 :	RegD.inh := tr24to48 (AktAdressteil) ;
		RegA.inh := trAdd (1, RegA.inh, RegD.inh) ;
		CheckUeberlauf (RegA) ;
	|
	2 :	IF (AktAdressteil BAND Cardinal24Bit) > 0FFFFH THEN
			ArithmetischerAlarm ;
		END ;
		RegD.inh := AktAdressteil BAND 0FFFFH ;
		RegA.inh := (RegA.inh BAND 0FFFFFFFF0000H) BOR VAL (CARDINAL64, (tr48Norm (trAdd (2, RegA.inh BAND 0FFFFH, RegD.inh)) BAND 0FFFFH)) ;
	ELSE
		RegD.inh := tr24to48 (AktAdressteil) ;
		RegA.inh := trAdd (3, RegA.inh, RegD.inh) ;
	END ;
	RegD.TK := 1 ;
	Takte (13) ;
END _AA;


PROCEDURE _SBA ;
	VAR	zw :	CARDINAL64 ;
BEGIN
	mod2Check ;
	CASE RegA.TK OF
	0 :	IF trAbs24 (AktAdressteil) > 127 THEN
			ArithmetischerAlarm ;
		END ;
		zw := trSub (1, tr8to48(RegA.inh),  tr8to48 (AktAdressteil)) ;
		IF trAbs (zw) > 127 THEN
			ArithmetischerAlarm ;
		END ;
		RegA.inh := (RegA.inh BAND MantissenMaske) BOR (zw BAND ExponentenMaske) ;
		RegD.inh := AktAdressteil BAND 0FFH ;
	|
	1 :	RegD.inh := tr24to48 (AktAdressteil) ;
		RegA.inh := trSub (1, RegA.inh, RegD.inh) ;
		CheckUeberlauf (RegA) ;
	|
	2 :	IF (AktAdressteil BAND Cardinal24Bit) > 0FFFFH THEN
			ArithmetischerAlarm ;
		END ;
		RegD.inh := AktAdressteil BAND 0FFFFH ;
		RegA.inh := (RegA.inh BAND 0FFFFFFFF0000H) BOR VAL (CARDINAL64, (tr48Norm (trSub (2, RegA.inh BAND 0FFFFH, RegD.inh)) BAND 0FFFFH)) ;
	ELSE
		RegD.inh := tr24to48 (AktAdressteil) ;
		RegA.inh := tr48Norm (trSub (3, RegA.inh, RegD.inh)) ;
	END ;
	RegD.TK := 1 ;
	Takte (13) ;
END _SBA;


END FestkommaArithmetik.
