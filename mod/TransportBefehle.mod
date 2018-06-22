
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE TransportBefehle;

(*	04.04.18	*)

FROM Befehle IMPORT
	nimpl ;

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;
FROM OktadenTransport IMPORT * ;
FROM upFestkomma IMPORT * ;

IMPORT debug ;

FROM Trace IMPORT
	TraceF ;


PROCEDURE _B ;
BEGIN
	mod2Check ;
	RegA.inh := GetGanzwort (AktAdressteil) ;
	RegA.TK := AktTypenkennung ;
	CheckMarke (RegA) ;
	Takte (2) ;
END _B ;


PROCEDURE _BU ;
BEGIN
	mod2Check ;
	RegA.inh := GetGanzwort (AktAdressteil) ;
	RegA.TK := AktTypenkennung ;
	Takte (2) ;
END _BU ;


PROCEDURE _BR ;
BEGIN
	RegH := RegA ;
	_B ;
	Takte (3) ;
END _BR ;


PROCEDURE _BN ;
BEGIN
	_B ;
	CASE RegA.TK OF
	0 :
		RegA.inh := (RegA.inh BAND ExponentenMaske) BOR (VAL (CARDINAL64, trInvert (RegA.inh)) BAND MantissenMaske) ;
	ELSE
		RegA.inh := trInvert (RegA.inh) ;
	END ;
	Takte (3) ;
END _BN ;


PROCEDURE _BNR ;
BEGIN
	RegH := RegA ;
	_BN ;
END _BNR ;


PROCEDURE _BB ;
BEGIN
	_B ;
	CASE RegA.TK OF
	0 :
		RegA.inh := (RegA.inh BAND ExponentenMaske) BOR (VAL (CARDINAL64, trAbs (RegA.inh)) BAND MantissenMaske) ;
	|
	1 :	RegA.inh := trAbs (RegA.inh) ;
	ELSE
	END ;
	Takte (3) ;
END _BB ;


PROCEDURE _BQ ;
BEGIN
	mod2Check ;
	RegQ.inh := GetGanzwort (AktAdressteil) ;
	RegQ.TK := AktTypenkennung ;
	CheckMarke (RegQ) ;
	Takte (3) ;
END _BQ ;


PROCEDURE _BD ;
BEGIN
	mod2Check ;
	RegD.inh := GetGanzwort (AktAdressteil) ;
	RegD.TK := AktTypenkennung ;
	CheckMarke (RegD) ;
	Takte (3) ;
END _BD ;


PROCEDURE _BH ;
BEGIN
	mod2Check ;
	RegH.inh := GetGanzwort (AktAdressteil) ;
	RegH.TK := AktTypenkennung ;
	CheckMarke (RegH) ;
	Takte (3) ;
END _BH ;


PROCEDURE _BQB ;
BEGIN
	_B ;
	RegQ := RegA ;
END _BQB ;


PROCEDURE _BZ ;
BEGIN
	_B ;
	AktAdressteil := AktAdressteil + 2 ;
	_BQ ;
	Takte (10) ;
END _BZ ;


PROCEDURE _BZN ;
BEGIN
	_BN ;
	HilfsReg := RegA ;
	AktAdressteil := AktAdressteil + 2 ;
	_BN ;
	RegQ := RegA ;
	RegA := HilfsReg ;
	Takte (5) ;
END _BZN ;


PROCEDURE _BT ;
	VAR	wert,
		maske :	CARDINAL64 ;
		p :	CARDINAL ;
BEGIN
	mod2Check ;
	wert := GetGanzwort(AktAdressteil) ;
	maske := RegQ.inh ;
	p := 0 ;
	IF (maske BAND 0FFFFFFFFFFFFH) <> 0FFFFFFFFFFFFH THEN
		WHILE ODD (maske) DO
			INC (p) ;
			maske := maske SHR 1 ;
			wert := wert SHR 1 ;
		END ;
	END ;
	RegA.inh := wert BAND (BNOT maske) ;
	RegA.TK := AktTypenkennung ;
	Takte (p * 2 + 7) ;
END _BT ;


PROCEDURE BnzCnz (bnz : BOOLEAN) ;
	VAR	a :	INTEGER ;
		i,
		il,
		ir,
		b,
		f,
		d :	CARDINAL ;
		gw,
		maske :	CARDINAL64 ;
BEGIN
	ir := GetIndexZellenWert (AktAdressteil) ;
	a := trAdd24 (ir, mod2) ;					(* Adresse Ganzwort *)
	il := GetIndexZellenWert (AktAdressteil SHR 8) ;
	RegB := il ;
	b := il BAND 0FFH ;						(* Zeichennummer 0,1, ... *)
	f := (il SHR 12) BAND 0FH ;					(* Zeichenlänge 4,6,8 oder 12 *)
	IF (f < 4) OR (f > 12) THEN
		d := 5 ;
		f := 8 ;
		BefehlsAlarm ;
	ELSE
		d := (48 DIV f) - 1 ;					(* max. Zeichennummer *)
	END ;

	CASE f OF
	4 :		maske := 0FH ;
	|
	6 :		maske := 3FH ;
	|
	8 :		maske := 0FFH ;
	|
	12 :		maske := 0FFFH ;
	ELSE
			maske := 0FFH ;
			d := 5 ;
			f := 8 ;
			BefehlsAlarm ;
	END ;

	gw := GetGanzwort (a) ;

	IF bnz THEN
		RegA.TK := AktTypenkennung ;
		IF (b BAND 0FH) < d THEN
			gw := gw SHR VAL (CARDINAL8, (d - (b BAND 0FH)) * f) ;

		END ;
		RegA.inh := gw BAND maske ;
	ELSE
		RegQ.inh := RegA.inh BAND maske ;
		RegQ.TK := RegA.TK ;
		RegA.TK := AktTypenkennung ;

		IF (b BAND 0FH) < d THEN
			RegQ.inh := RegQ.inh SHL VAL (CARDINAL8, (d - (b BAND 0FH)) * f) ;
			maske := maske SHL VAL (CARDINAL8, (d - (b BAND 0FH)) * f) ;
		END ;

		RegA.inh := (gw BAND (BNOT maske)) BOR RegQ.inh ;
		PutGanzwort (a, 4, RegA.inh) ;
	END ;

	IF (b BAND 0FH) < d THEN
		PutIndexZellenWert (AktAdressteil SHR 8, (il BAND 0FFFF00H) BOR (b + 1)) ;
	ELSE
		PutIndexZellenWert (AktAdressteil SHR 8,  il BAND 0FFFF00H) ;
		PutIndexZellenWert (AktAdressteil, ir + 2) ;
	END ;

	mod2 := 0 ;
END BnzCnz ;



PROCEDURE _BNZ ;
BEGIN
	BnzCnz (TRUE);
	Takte (41) ;
END _BNZ ;


PROCEDURE _BZ2 ;
BEGIN
	mod2Check ;
	IF ODD (AktAdressteil) THEN
		RegA.inh := VAL (CARDINAL64, GetHalbwort (AktAdressteil)) SHL 24 ;
		RegA.TK := AktTypenkennung ;
		RegA.inh := RegA.inh BOR VAL (CARDINAL64, GetHalbwort (AktAdressteil + 1)) ;
		Takte (10) ;
	ELSE
		_BU ;
		Takte (8) ;
	END ;
END _BZ2 ;


PROCEDURE _B2 ;
BEGIN
	mod2Check ;
	RegA.inh := GetHalbwort (AktAdressteil) ;
	RegA.TK := AktTypenkennung ;
	Takte (9) ;
END _B2 ;


PROCEDURE _B2V ;
BEGIN
	mod2Check ;
	RegA.inh := tr24to48 (GetHalbwort (AktAdressteil)) ;
	RegA.TK := 1 ;
	Takte (10) ;
END _B2V ;


PROCEDURE _B2VN ;
BEGIN
	_B2V ;
	RegA.inh := trInvert (tr24to48 (RegA.inh)) ;
	Takte (2) ;
END _B2VN ;


PROCEDURE _B3 ;
BEGIN
	mod2Check ;
	RegA.inh := GetDrittelwort (AktAdressteil) ;
	RegA.TK := 1 ;
	Takte (9) ;
END _B3 ;


PROCEDURE _B3V ;
BEGIN
	_B3 ;
	RegA.inh := tr16to48 (RegA.inh) ;
	Takte (1) ;
END _B3V ;


PROCEDURE _BLEI ;
BEGIN
	mod2Check ;
	RegA.TK := 2 ;
	IF AktAdressteil = 4 THEN
		RegA.inh := VAL (CARDINAL64, RegX) SHL 24 ;
	ELSE
		RegA.inh := 0 ;
	END ;
	Takte (13) ;
END _BLEI ;


PROCEDURE _BSS ;
BEGIN
	nimpl ;
	Takte (3) ;
END _BSS ;


PROCEDURE _BL ;
BEGIN
	_BU ;
	IF TestAdresse (AktAdressteil) AND AktSchreibschutz THEN
		TraceF ("** BL löscht schreibgeschütztes Wort : %'06h", AktAdressteil) ;
		SchreibschutzAktiv := FALSE ;			(* wg. altem TR440-Fehler beim BL *)
	END ;
	PutGanzwort (AktAdressteil, 0, 0) ;
	SchreibschutzAktiv := TRUE ;
	Takte (15) ;
END _BL ;


PROCEDURE _BC ;
	VAR	HilfsMarke :	CARDINAL64 ;
BEGIN
	mod2Check ;
	HilfsReg.inh := GetGanzwort (AktAdressteil) ;
	HilfsReg.TK := AktTypenkennung ;
	HilfsMarke := HilfsReg.inh BAND MarkenBit ;

	CheckUeberlauf (RegA) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher BOR HilfsMarke) ;

	RegA := HilfsReg ;
	CheckMarke (RegA) ;
	Takte (13) ;
END _BC ;


PROCEDURE _BCI ;
	VAR	wort :	CARDINAL64 ;
BEGIN
	mod2Ohne ;
	ClearAssocIndex ;
	wort := GetGanzwort (AktAdressteil) ;
	IF AlarmGewesen THEN
		RETURN ;
	END ;
	PutGanzwort (AktAdressteil, 3, VAL(CARDINAL64, RegX) SHL 24 BOR VAL(CARDINAL64, RegU)) ;
	RegX := wort SHR 24 BAND 3FFFFFH ;
	RegU := wort BAND 0FFH ;
	XbasisPruefen ;
	Takte (66) ;
END _BCI ;


PROCEDURE _C ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegA) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher) ;
	Takte (8) ;
END _C ;


PROCEDURE _CU ;
BEGIN
	mod2Check ;
	PutGanzwort (AktAdressteil, RegA.TK, RegA.inh) ;
	Takte (8) ;
END _CU ;


PROCEDURE _CR ;
BEGIN
	_C ;
	RegA := RegH ;
	Takte (1) ;
END _CR ;


PROCEDURE _CN ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegA) ;
	CASE RegA.TK OF
	0 :	HilfsReg.inh := (VAL (CARDINAL64, trInvert (RegA.inh)) BAND MantissenMaske) BOR (RegA.inh BAND ExponentenMaske) ;
	ELSE
		HilfsReg.inh := trInvert (RegA.inh) ;
	END ;
	CheckUeberlauf (HilfsReg) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher) ;
	Takte (9) ;
END _CN ;


PROCEDURE _CB ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegA) ;
	CASE RegA.TK OF
	0 :	HilfsReg.inh := (VAL (CARDINAL64, trAbs (RegA.inh)) BAND MantissenMaske) BOR (RegA.inh BAND ExponentenMaske) ;
	|
	1 :	HilfsReg.inh := trAbs (RegA.inh) ;
	ELSE
	END ;
	CheckUeberlauf (HilfsReg) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher) ;
	Takte (9) ;
END _CB ;


PROCEDURE _CMT ;
BEGIN
	mod2Check ;
	HilfsReg := RegA ;
	CheckUeberlauf (HilfsReg) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher BOR MarkenBit) ;
	IF ORD(RegA.TK) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (8) ;
END _CMT ;


PROCEDURE _CMR ;
	VAR	marke :	CARDINAL64 ;
BEGIN
	mod2Check ;
	IF RegM THEN
		marke := MarkenBit ;
	ELSE
		marke := 0 ;
	END ;
	HilfsReg := RegA ;
	CheckUeberlauf (HilfsReg) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher BOR marke) ;
	IF ORD(RegA.TK) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (9) ;
END _CMR ;


PROCEDURE _CMC ;
	VAR	marke :	CARDINAL64 ;
BEGIN
	mod2Check ;
	HilfsReg.inh := GetGanzwort (AktAdressteil) ;
	marke := HilfsReg.inh BAND MarkenBit ;
	HilfsReg := RegA ;
	CheckUeberlauf (HilfsReg) ;
	PutGanzwort (AktAdressteil, RegA.TK, AktWertFuerSpeicher BOR marke) ;
	IF ORD(RegA.TK) > 1 THEN
		TypenkennungsAlarm (AktAdressteil) ;
	END ;
	Takte (13) ;
END _CMC ;


PROCEDURE _CQ ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegQ) ;
	PutGanzwort (AktAdressteil, RegQ.TK, AktWertFuerSpeicher) ;
	Takte (9) ;
END _CQ ;


PROCEDURE _CD ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegD) ;
	PutGanzwort (AktAdressteil, RegD.TK, AktWertFuerSpeicher) ;
	Takte (9) ;
END _CD ;


PROCEDURE _CH ;
BEGIN
	mod2Check ;
	CheckUeberlauf (RegH) ;
	PutGanzwort (AktAdressteil, RegH.TK, AktWertFuerSpeicher) ;
	Takte (9) ;
END _CH ;


PROCEDURE _CZ ;
BEGIN
	_C ;
	CheckUeberlauf (RegQ) ;
	PutGanzwort (AktAdressteil + 2, RegQ.TK, AktWertFuerSpeicher) ;
	Takte (8) ;
END _CZ ;


PROCEDURE _CT ;
	VAR	wert,
		hilfA,
		hilfQ :	CARDINAL64 ;
		p :	CARDINAL ;
BEGIN
	mod2Check ;
	wert := GetGanzwort(AktAdressteil) ;
	RegD.TK := AktTypenkennung ;
	hilfA := RegA.inh ;
	hilfQ := RegQ.inh ;
	p := 0 ;
	IF (hilfQ BAND 0FFFFFFFFFFFFH) <> 0FFFFFFFFFFFFH THEN
		WHILE ODD (hilfQ) DO
			INC (p) ;
			hilfQ := hilfQ SHR 1 ;
			hilfA := hilfA SHL 1 ;
		END ;
	END ;
	wert := (wert BAND RegQ.inh) BOR (hilfA BAND (BNOT RegQ.inh)) ;
	PutGanzwort (AktAdressteil, RegD.TK, wert) ;
	RegD.inh := wert ;
	Takte (p * 2 + 22) ;
END _CT ;


PROCEDURE _CNZ ;
BEGIN
	BnzCnz (FALSE) ;
	Takte (55) ;
END _CNZ ;


PROCEDURE _C2 ;
BEGIN
	mod2Check ;
	PutHalbwort (AktAdressteil, RegA.lo) ;
	Takte (15) ;
END _C2 ;

PROCEDURE _C3 ;
BEGIN
	mod2Check ;
	PutDrittelwort (AktAdressteil, RegA.lo) ;
	Takte (15) ;
END _C3 ;


PROCEDURE tauschen (VAR Reg1, Reg2 : reg48) ;
BEGIN
	HilfsReg := Reg1 ;
	Reg1 := Reg2 ;
	Reg2 := HilfsReg ;
END tauschen ;


PROCEDURE _RT ;
BEGIN
	mod2Check ;
	CASE (AktAdressteil SHR 4) BAND 0FH OF
	3 :	tauschen (RegD, RegH) ;
	|
	5 :	tauschen (RegQ, RegH) ;
	|
	9 :	tauschen (RegA, RegH) ;
	|
	6 :	tauschen (RegQ, RegD) ;
	|
	0AH :	tauschen (RegA, RegD) ;
	|
	0CH :	tauschen (RegA, RegQ) ;
	ELSE
		BefehlsAlarm ;
	END ;
	Takte (4) ;
END _RT ;

PROCEDURE _XB ;
BEGIN
	mod2Ohne ;
	RegB := GetIndexZellenWert (AktAdressteil) ;
	Takte (1) ;
END _XB ;


PROCEDURE _XC ;
BEGIN
	mod2Ohne ;
	PutIndexZellenWert (AktAdressteil, RegB) ;
	Takte (3) ;
END _XC ;


PROCEDURE _XCN ;
BEGIN
	mod2Ohne ;
	PutIndexZellenWert (AktAdressteil, trInvert24 (RegB)) ;
	Takte (5) ;
END _XCN ;


PROCEDURE _TTX ;
	VAR	il,
		ir :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	il := (AktAdressteil SHR 8) BAND 0FFH ;
	ir := AktAdressteil BAND 0FFH ;
	RegB := GetIndexZellenWert (il) ;
	PutIndexZellenWert (il, GetIndexZellenWert (ir)) ;
	PutIndexZellenWert (ir, RegB) ;
	Takte (11) ;
END _TTX ;


PROCEDURE _TXX ;
	VAR	il,
		ir :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	il := (AktAdressteil SHR 8) BAND 0FFH ;
	ir := AktAdressteil BAND 0FFH ;
	RegB := GetIndexZellenWert (ir) ;
	PutIndexZellenWert (il, RegB) ;
	Takte (4) ;
END _TXX ;


PROCEDURE _TXR ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Ohne ;
	wert := tr24to48 (GetIndexZellenWert (AktAdressteil)) ;
	IF (AktAdressteil BAND 800H) <> 0 THEN		(* negativ *)
		wert := trInvert (wert) ;
	END ;
	IF (AktAdressteil SHR 8) BAND 80H <> 0 THEN
		RegA.inh := wert ;
		RegA.TK := 1 ;
	END ;
	IF (AktAdressteil SHR 8) BAND 40H <> 0 THEN
		RegQ.inh := wert ;
		RegQ.TK := 1 ;
	END ;
	IF (AktAdressteil SHR 8) BAND 20H <> 0 THEN
		RegD.inh := wert ;
		RegD.TK := 1 ;
	END ;
	IF (AktAdressteil SHR 8) BAND 10H <> 0 THEN
		RegH.inh := wert ;
		RegH.TK := 1 ;
	END ;
	RegB := trHW (wert) ;
	Takte (5) ;
END _TXR ;


PROCEDURE _TRX ;
	VAR	negativ :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	negativ := (AktAdressteil BAND 800H) <> 0 ;		(* negativ *)
	CASE (AktAdressteil SHR 8) BAND 0F0H OF
	80H :	RegB := trHW(RegA.inh) ;
	|
	40H :	RegB := trHW(RegQ.inh) ;
	|
	20H :	RegB := trHW(RegD.inh) ;
	|
	10H :	RegB := trHW(RegH.inh) ;
	ELSE
		BefehlsAlarm ;
	END ;
	IF negativ THEN
		RegB := trInvert24 (RegB) ;
	END ;
	PutIndexZellenWert (AktAdressteil, RegB) ;
	Takte (6) ;
END _TRX ;


PROCEDURE _TCB ;
BEGIN
	mod2Check ;
	RegB := GetHalbwort (AktAdressteil) ;
	Takte (2) ;
END _TCB ;


PROCEDURE _TBC ;
BEGIN
	mod2Ohne ;
	PutHalbwort (AktAdressteil, RegB) ;
	Takte (7) ;
END _TBC ;


PROCEDURE _BCL ;
	VAR	hilf :	CARDINAL8 ;
BEGIN
	mod2Ohne ;
	hilf := GetSechstelwort (AktAdressteil) ;
	PutSechstelwort (AktAdressteil, RegK);
	RegK := hilf ;
	Takte (18) ;
END _BCL ;


PROCEDURE _WTV ;
	VAR	i,
		anzGW :	INTEGER ;
		wert :	CARDINAL64 ;
		p1,
		p2 :	CARDINAL8 ;
		quelle,
		ziel :	CARDINAL ;
BEGIN
	mod2Ohne ;
	anzGW := tr24toInt32 (RegB) ;
	p1 := AktAdressteil SHR 8 ;
	p2 := AktAdressteil ;
	ziel := GetIndexZellenWert (p1) ;
	quelle := GetIndexZellenWert (p2) ;
	IF anzGW <> 0 THEN
		FOR i := 1 TO anzGW DO
			wert := GetGanzwort (quelle) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
			PutGanzwort (ziel, AktTypenkennung, wert) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
			INC (quelle, 2) ;
			INC (ziel, 2);
		END ;
		RegB := ziel - 2 ;
	ELSE
		RegB := ziel ;
	END ;
	Takte (anzGW * 21 + 10) ;
END _WTV ;


PROCEDURE _WTR ;
	VAR	i,
		anzGW :	INTEGER ;
		wert :	CARDINAL64 ;
		p1,
		p2 :	CARDINAL8 ;
		quelle,
		ziel :	CARDINAL ;
BEGIN
	mod2Ohne ;
	anzGW := tr24toInt32 (RegB) ;
	p1 := AktAdressteil SHR 8 ;
	p2 := AktAdressteil ;
	ziel := GetIndexZellenWert (p1) ;
	quelle := GetIndexZellenWert (p2) ;
	IF anzGW <> 0 THEN
		FOR i := 1 TO anzGW DO
			wert := GetGanzwort (quelle) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
			PutGanzwort (ziel, AktTypenkennung, wert) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
			DEC (quelle, 2) ;
			DEC (ziel, 2);
		END ;
		RegB := ziel + 2 ;
	ELSE
		RegB := ziel ;
	END ;
	Takte (anzGW * 21 + 10) ;
END _WTR ;



PROCEDURE upQCR (reladr : CARDINAL) ;
BEGIN
	PutGanzwort (reladr, 2 + ORD(RegM), (VAL (CARDINAL64, RegB) SHL 24) BOR  (VAL (CARDINAL64, RegK) SHL 16) BOR (VAL (CARDINAL64, RegY) SHL 8) BOR VAL (CARDINAL64, RegU)) ;
	IF AlarmGewesen THEN
		RETURN ;
	END ;
	PutGanzwort (reladr + 2, RegA.TK, RegA.inh) ;
	PutGanzwort (reladr + 4, RegQ.TK, RegQ.inh) ;
	PutGanzwort (reladr + 6, RegD.TK, RegD.inh) ;
	PutGanzwort (reladr + 8, RegH.TK, RegH.inh) ;
	PutGanzwort (reladr + 10, 2, RegT SHL 24) ;
END upQCR ;


PROCEDURE _QBR ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	mod2Ohne ;
	RegT := GetHalbwort (AktAdressteil) ;
	IF AlarmGewesen THEN
		RETURN ;
	END ;
	RegH.inh := GetGanzwort (AktAdressteil - 2) ; RegH.TK := AktTypenkennung ;
	RegD.inh := GetGanzwort (AktAdressteil - 4) ; RegD.TK := AktTypenkennung ;
	RegQ.inh := GetGanzwort (AktAdressteil - 6) ; RegQ.TK := AktTypenkennung ;
	RegA.inh := GetGanzwort (AktAdressteil - 8) ; RegA.TK := AktTypenkennung ;
	wert := GetGanzwort (AktAdressteil - 10) ;
	IF ODD (AktTypenkennung) THEN
		RegM := TRUE ;
	ELSE
		RegM := FALSE ;
	END ;
	RegB := wert SHR 24 ;
	RegK := (wert SHR 16) BAND 0FFH ;
	RegY := (wert SHR 8) BAND 0FFH ;
	RegU := wert BAND 0FFH ;
	Takte (84) ;
END _QBR ;


PROCEDURE _QCR ;
BEGIN
	mod2Ohne ;
	upQCR (AktAdressteil) ;
	Takte (55) ;
END _QCR ;


PROCEDURE _ZK ;
	VAR	hilf :		CARDINAL64 ;
		OADR :		CARDINAL ;
		bytezahl,
		anz,
		i, p :		CARDINAL ;
		oktade :	CARDINAL8 ;
		B,I,V :		BOOLEAN ;
		speicher :	ARRAY [0..31] OF CARDINAL8 ;
BEGIN
	mod2Ohne ;
	p := (AktAdressteil SHR 8) BAND 0FH ;
	IF p = 0 THEN
		RETURN ;
	END ;
	B := (AktAdressteil BAND 8000H) <> 0 ;
	I := (AktAdressteil BAND 4000H) <> 0 ;
	V := (AktAdressteil BAND 2000H) <> 0 ;
	IF (AktAdressteil BAND 1000H) <> 0 THEN		(* Register *)
		CASE AktAdressteil BAND 38H OF
		08H :	OADR := RegB ;
			IF I THEN
				INC (RegB, p) ;
			END ;
		|
		10H :	OADR := RegH.inh BAND 0FFFFFFH ;
			IF I THEN
				INC (RegH.inh, p) ;
			END ;
		|
		20H :	OADR := RegD.inh BAND 0FFFFFFH ;
			IF I THEN
				INC (RegD.inh, p) ;
			END ;
		ELSE
			BefehlsAlarm ;
			RETURN ;
		END ;
	ELSE						(* Indexzelle *)
		OADR := GetIndexZellenWert (AktAdressteil) ;
		IF I THEN
			PutIndexZellenWert (AktAdressteil, OADR + p) ;
		END ;
	END ;

	InitOktaden ;

	IF B THEN				(* <A,Q> := Oktaden (evtl. verkürzt zu Tetraden) aus Speicher  *)
		IF V THEN
			bytezahl := (p + 1) DIV 2 ;
			FOR i := 0 TO p-1 DO
				IF ODD (i) THEN
					speicher [i DIV 2] := oktade BOR (GetOktade (OADR + i) BAND 0FH) ;
				ELSE
					oktade := ORD (GetOktade (OADR + i)) SHL 4 ;
				END ;
			END ;
			IF ODD (p) THEN
				speicher [(p-1) DIV 2] := oktade ;
			END ;
		ELSE
			bytezahl := p ;
			FOR i := 0 TO p-1 DO
				speicher [i] := ORD (GetOktade (OADR + i)) ;
			END ;
		END ;
		RegA.inh := 0 ;
		RegA.TK := 3 ;
		IF bytezahl > 6 THEN
			anz := 6 ;
		ELSE
			anz := bytezahl ;
		END ;
		FOR i := 0 TO anz - 1 DO
			RegA.inh := RegA.inh BOR (VAL (CARDINAL64, speicher [i]) SHL (40 - i * 8)) ;
		END ;
		IF bytezahl > 6 THEN
			RegQ.inh := 0 ;
			RegQ.TK := 3 ;
			FOR i := 6 TO bytezahl - 1 DO
				RegQ.inh := RegQ.inh BOR (VAL (CARDINAL64, speicher [i]) SHL (40 - (i - 6) * 8)) ;
			END ;
		END ;

	ELSE					(* Speicher := (evtl. aus Tetraden verlängerte) Oktaden aus <A,Q> *)
		hilf := RegA.inh ;
		bytezahl := p  ;
		IF V THEN
			FOR i := 0 TO p * 2 - 1 DO
				speicher [i] := (hilf SHR 44 BAND 0FH) BOR 0B0H ;	(* -> Ziffer im ZC1 *)
				IF i = 11 THEN
					hilf := RegQ.inh ;
				ELSE
					hilf := hilf SHL 4 ;
				END ;
			END ;
		ELSE
			FOR i := 0 TO p DO
				speicher [i] := hilf SHR 44 BAND 0FFH ;
				IF i = 5 THEN
					hilf := RegQ.inh ;
				ELSE
					hilf := hilf SHL 8 ;
				END ;
			END ;
		END ;
		FOR i := 0 TO bytezahl - 1 DO
			PutOktade (OADR + i, speicher [i]) ;
		END ;
		AbschlussOktaden ;
	END ;

	Takte (122) ;
END _ZK ;


PROCEDURE _TOK ;
	VAR	qwort,
		zwort :		CARDINAL64 ;
		i,
		anz,
		quelle,
		quellNr,
		ziel,
		zielNr :	CARDINAL ;
		ch :		CARDINAL8 ;
		erstes :	BOOLEAN ;
		qhalbwort,
		zhalbwort :	CARDINAL ;
BEGIN
	mod2Ohne ;
	anz := AktAdressteil ;
	quelle := (RegH.inh BAND 0FFFFFFH) DIV 3 ;
	quellNr := (RegH.inh BAND 0FFFFFFH) MOD 3 ;
	RegQ.inh := 0 ;
	RegQ.TK := 3 ;
		erstes := TRUE ;

	IF RegH.TK = 3 THEN
		ziel := (RegH.inh SHR 24) DIV 3 ;
		zielNr := (RegH.inh SHR 24) MOD 3 ;
		qhalbwort := GetHalbwort (quelle) ;
		CASE quellNr OF
		  0 :	zhalbwort := 0 ;
		| 1 :	zhalbwort := GetHalbwort (ziel) BAND 0FF00FFH ;
		| 2 :	zhalbwort := GetHalbwort (ziel) BAND 0FFFF00H ;
		ELSE
		END ;

		FOR i := 1 TO anz DO

			CASE quellNr OF
			  0 :	IF NOT erstes THEN
					INC (quelle) ;
					qhalbwort := GetHalbwort (quelle) ;
				END ;
				ch := qhalbwort SHR 16 ;
				quellNr := 1 ;
			| 1 :	ch := qhalbwort SHR 8 ;
				quellNr := 2 ;
			| 2 :	ch := qhalbwort ;
				quellNr := 0 ;
				erstes := FALSE ;
			ELSE
			END ;

			CASE zielNr OF
			  0 :	zhalbwort := ORD(ch) SHL 16 ;
			  	zielNr := 1 ;
			| 1 :	zhalbwort := zhalbwort BOR (ORD(ch) SHL 8) ;
				zielNr := 2 ;
			| 2 :	zhalbwort := zhalbwort BOR ORD (ch) ;
				NeueTypenkennung := 3 ;
				PutHalbwort (ziel, zhalbwort) ;
				zhalbwort := 0 ;
				INC (ziel) ;
				zielNr := 0 ;
			ELSE
			END ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
		END ;
		CASE zielNr OF
		  0 :	RegB := ziel - 1 ;
		| 1 :
			NeueTypenkennung := 3 ;
			PutHalbwort (ziel, (GetHalbwort (ziel) BAND 0FFFFH) BOR zhalbwort) ;
			RegB := ziel ;
		| 2 :
		ELSE
			NeueTypenkennung := 3 ;
			PutHalbwort (ziel, (GetHalbwort (ziel) BAND 0FFH) BOR zhalbwort) ;
			RegB := ziel ;
		END ;
		RegA.inh := GetGanzwort (RegB) ;
		RegA.TK := 3 ;
		RegD := RegA ;
		RegH := RegA ;

	ELSIF RegH.TK = 2 THEN
		ziel := (RegH.inh SHR 24) DIV 6 * 2 ;	(* Ganzwortgrenze *)
		zielNr := (RegH.inh SHR 24) MOD 6 ;
(*	bis vor 04.04.18 :
		ziel := RegH.hi DIV 6 * 2 ;
		zielNr := RegH.hi MOD 6 ;
*)
		qwort := GetGanzwort (quelle) ;
		quellNr := 0 ;
		zwort := 0 ;

		zwort := GetGanzwort (ziel) ;

		FOR i := 1 TO anz DO

			CASE quellNr OF
			  0 :	ch := qwort SHR 40 ;
			  	quellNr := 1 ;
			| 1 :	ch := qwort SHR 32 ;
				quellNr := 2 ;
			| 2 :	ch := qwort SHR 24 ;
				quellNr := 3 ;
			| 3 :	ch := qwort SHR 16 ;
				quellNr := 4 ;
			| 4 :	ch := qwort SHR 8 ;
				quellNr := 5 ;
			| 5 :	ch := qwort ;
				quellNr := 0 ;
			ELSE
			END ;

			CASE zielNr OF
			  0 :	IF NOT erstes THEN
					zwort := GetGanzwort (ziel) ;
				END ;
			  	zwort := (zwort BAND  00FFFFFFFFFFH) BOR (VAL(CARDINAL64, ch) SHL 40) ;
			| 1 :	zwort := (zwort BAND 0FF00FFFFFFFFH) BOR (VAL(CARDINAL64, ch) SHL 32) ;
			| 2 :	zwort := (zwort BAND 0FFFF00FFFFFFH) BOR (VAL(CARDINAL64, ch) SHL 24) ;
			| 3 :	zwort := (zwort BAND 0FFFFFF00FFFFH) BOR (VAL(CARDINAL64, ch) SHL 16) ;
			| 4 :	zwort := (zwort BAND 0FFFFFFFF00FFH) BOR (VAL(CARDINAL64, ch) SHL 8) ;
			| 5 :	zwort := (zwort BAND 0FFFFFFFFFF00H) BOR VAL (CARDINAL64, ch) ;
				PutGanzwort (ziel, 3, zwort) ;
				INC (ziel, 2) ;
				zwort := GetGanzwort (ziel) ;
				erstes := FALSE ;
				zielNr := 0 ;
			ELSE
			END ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
		END ;
		IF zielNr = 0 THEN
			RegF := ziel - 1 ;
			zwort := GetGanzwort (ziel-1) ;
		ELSE
			RegF := ziel ;
			PutGanzwort (ziel, 3, zwort) ;
		END ;
		RegA.inh := zwort ;
		RegA.TK := 3 ;
		RegD := RegA ;
		RegH.hi := RegH.hi + anz ;
	ELSE
		BefehlsAlarm ;
	END ;
	Takte (200) ;
END _TOK ;


END TransportBefehle.
