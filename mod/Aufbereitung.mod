
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Aufbereitung;

(*	27.04.16	*)

FROM Befehle IMPORT
	nimpl ;

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;
FROM upFestkomma IMPORT * ;


PROCEDURE _US ;
	VAR	zlng,
		tabelle :	CARDINAL ;
		s :		CARDINAL8 ;
		alleZeichen :	BOOLEAN ;

	PROCEDURE Maskieren (z : CARDINAL) : CARDINAL ;
	BEGIN
		IF zlng = 6 THEN
			RETURN z BAND 3FH ;
		ELSIF zlng = 8 THEN
			RETURN z BAND 0FFH ;
		END ;
		RETURN z BAND 0FFFH ;
	END Maskieren ;

	PROCEDURE GetUSelement (z : CARDINAL) : CARDINAL ;
		VAR	zelle :	CARDINAL ;
	BEGIN
		zelle := GetHalbwort (tabelle + Maskieren (z) DIV 2) ;
		RegA.TK := AktTypenkennung ;
		IF ODD (z) THEN
			RETURN zelle BAND 0FFFH ;
		ELSE
			RETURN (zelle SHR 12) BAND 0FFFH ;
		END ;
	END GetUSelement ;

	PROCEDURE USalle ;
		VAR	wert,
			maske :	CARDINAL64 ;
			i :	CARDINAL ;
	BEGIN
		wert := RegA.inh ;
		IF zlng = 6 THEN
			maske := 0FFFFFFFFFFFFFFC0H ;
			FOR i := 1 TO 8 DO
				wert := ((wert BAND maske) BOR VAL (CARDINAL64, GetUSelement (wert) BAND 3FH)) ROR 6 ;
			END ;
		ELSIF zlng = 8 THEN
			maske := 0FFFFFFFFFFFFFF00H ;
			FOR i := 1 TO 6 DO
				wert := ((wert BAND maske) BOR VAL (CARDINAL64, GetUSelement (wert) BAND 0FFH)) ROR 8 ;
			END ;
		ELSE
			maske := 0FFFFFFFFFFFFF000H ;
			FOR i := 1 TO 4 DO
				wert := ((wert BAND maske) BOR VAL (CARDINAL64, GetUSelement (wert) BAND 0FFFH)) ROR 12 ;
			END ;
		END ;
		RegA.inh := wert ROL 48 ;
	END USalle ;

BEGIN
	s := (AktAdressteil SHR 12) BAND 0FH ;
	alleZeichen :=  ODD (s) ;

	CASE s BAND 0EH OF
	8 :	zlng := 6 ;
	|
	4 :	zlng := 8 ;
	|
	2 :	zlng := 12 ;
	ELSE
		BefehlsAlarm ;
		RETURN ;
	END ;
	tabelle := trAdd24 (GetIndexZellenWert (AktAdressteil), mod2) ;
	mod2 := 0 ;
	RegB := tabelle ;

	IF alleZeichen THEN
		USalle ;
		RegQ := RegA ;
		IF zlng = 6 THEN
			Takte (176) ;
			RegY := 8 ;
		ELSIF zlng = 8 THEN
			Takte (136) ;
			RegY := 6 ;
		ELSE
			Takte (96) ;
			RegY := 4 ;
		END ;
	ELSE
		RegA.inh := GetUSelement (RegA.inh) ;
		Takte (32) ;
	END ;
END _US;



PROCEDURE upIR (VAR reg : reg48) ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	CASE reg.TK OF
	0 :	IF AktAdressteil BAND 8 <> 0 THEN		(* bilde Betrag *)
			wert := trAbs (reg.inh BAND MantissenMaske) ;
			reg.inh := (reg.inh BAND ExponentenMaske) BOR (wert BAND MantissenMaske) ;
		ELSE
			wert := trInvert (reg.inh BAND MantissenMaske) ;
			reg.inh := (reg.inh BAND ExponentenMaske) BOR (wert BAND MantissenMaske) ;
		END ;
	|
	1 :	IF AktAdressteil BAND 8 <> 0 THEN		(* bilde Betrag *)
	   		reg.inh := trAbs (reg.inh) ;
		ELSE
			reg.inh := trInvert (reg.inh) ;
		END ;
	ELSE	IF AktAdressteil BAND 8 = 0 THEN			(* nicht bilde Betrag *)
			reg.inh := trInvert (reg.inh) ;
		END ;
	END ;
END upIR ;



PROCEDURE _IR ;			(* A Q D H B *)
	VAR	p :	CARDINAL ;
BEGIN
	mod2Ohne ;
	p := 0 ;
	IF AktAdressteil BAND 80H <> 0 THEN
		INC (p) ;
		upIR (RegA) ;
	END ;
	IF AktAdressteil BAND 40H <> 0 THEN
		INC (p) ;
		upIR (RegQ) ;
	END ;
	IF AktAdressteil BAND 20H <> 0 THEN
		INC (p) ;
		upIR (RegD) ;
	END ;
	IF AktAdressteil BAND 10H <> 0 THEN
		INC (p) ;
		upIR (RegH) ;
	END ;
	Takte (p * 4 +1) ;
END _IR;


PROCEDURE _SH ;			(* A Q L K Z R U B *)
	VAR	p :		INTEGER ;
		anz :		CARDINAL ;
		merkwert :	CARDINAL64 ;
		s :		CARDINAL8 ;
		rundung,
		links,
		imKreis,
		unabhVonTK,
		zaehlen :	BOOLEAN ;

	PROCEDURE shift (VAR reg : reg48) ;
		VAR	wert :		CARDINAL64 ;
			negativ :	BOOLEAN ;
	BEGIN
		IF unabhVonTK OR (reg.TK > 1) OR imKreis THEN
			wert := reg.inh BAND Cardinal48Bit ;

			IF links THEN
				IF imKreis THEN
					wert := (wert SHL anz) BOR (wert SHR (48-anz)) ;
				ELSE
					wert := wert SHL anz ;
				END ;
			ELSE
				IF imKreis THEN
					wert := (wert SHR anz) BOR (wert SHL (48-anz)) ;
				ELSE
					wert := wert SHR anz ;
				END ;
			END ;

			reg.inh := wert BAND Cardinal48Bit ;
		ELSE
			wert := reg.inh BAND Cardinal48Bit ;

			IF links THEN
				IF wert > Max48bitWertPositiv THEN	(* negative Zahl *)
					wert := (wert SHL anz) BOR (VAL (CARDINAL64, 0FFFFFFFFFFFFH) SHR (48-anz)) ;
				ELSE
					wert := wert SHL anz ;
				END ;
			ELSE
				IF wert > Max48bitWertPositiv THEN	(* negative Zahl *)
					wert := (wert BOR Negativ48Bit) SAR anz ;
				ELSE
					wert := wert SHR anz ;
				END ;
			END ;

			reg.inh := wert BAND Cardinal48Bit ;

		END ;
	END shift ;

	PROCEDURE ShiftZaehlen ;
		VAR	wert :	CARDINAL64 ;
			i :	CARDINAL ;
	BEGIN
		wert := RegA.inh ;
		IF links THEN
			FOR i := 1 TO anz DO
				IF wert BAND MarkenBit <> 0 THEN
					INC (RegY) ;
				END ;
				wert := wert SHL 1 ;
			END ;
		ELSE
			FOR i := 1 TO anz DO
				IF ODD(wert) THEN
					INC (RegY) ;
				END ;
				wert := wert SHR 1 ;
			END ;
		END ;
	END ShiftZaehlen ;

	PROCEDURE ShiftZaehlenZ ;
		VAR	wertA,
			wertQ :	CARDINAL64 ;
			i :	CARDINAL ;
	BEGIN
		wertA := (RegA.inh BAND Cardinal48Bit) ;
		wertQ := (RegQ.inh BAND Cardinal48Bit) ;
		IF links THEN
			IF unabhVonTK OR (RegA.TK > 1) OR (RegQ.TK > 1) OR imKreis THEN
				FOR i := 1 TO anz DO
					IF (wertA BAND MarkenBit) <> 0 THEN
						INC (RegY) ;
					END ;
					wertA := wertA SHL 1 ;
					IF (wertQ BAND MarkenBit) <> 0 THEN
						wertA := wertA BOR 1 ;
					END ;
				END ;
			ELSE
				FOR i := 1 TO anz DO
					IF (wertA BAND MarkenBit) <> 0 THEN
						INC (RegY) ;
					END ;
					wertA := wertA SHL 1 ;
					IF (wertQ BAND 200000000000H) <> 0 THEN
						wertA := wertA BOR 1 ;
					END ;
				END ;
			END ;
			wertQ := wertQ SHL 1 ;
		ELSIF unabhVonTK OR (RegA.TK > 1) OR (RegQ.TK > 1) OR imKreis THEN
			FOR i := 1 TO anz DO
				IF ODD (wertA) THEN
					INC (RegY) ;
				END ;
				wertA := wertA SHR 1 ;
				IF ODD (wertQ) THEN
					wertA := wertA BOR MarkenBit ;
				END ;
				wertQ := wertQ SHR 1 ;
			END ;
		ELSE
			FOR i := 1 TO anz DO
				IF ODD(wertA) THEN
					INC (RegY) ;
				END ;
				wertA := wertA SHR 1 ;
			END ;
		END ;
	END ShiftZaehlenZ ;

	PROCEDURE ShiftZ ;
		VAR	wertA,
			wertQ,
			hilf :		CARDINAL64 ;
			negativ :	BOOLEAN ;
	BEGIN

		IF unabhVonTK OR (RegA.TK > 1) OR (RegQ.TK > 1) OR imKreis THEN

			wertA := RegA.inh BAND Cardinal48Bit ;
			wertQ := RegQ.inh BAND Cardinal48Bit ;

			IF links THEN
				IF imKreis THEN
					WHILE anz >= 48 DO
						hilf := wertA ;
						wertA := wertQ ;
						wertQ := hilf ;
						DEC (anz, 48) ;
					END ;
					IF anz <> 0 THEN
						hilf := wertA ;
						wertA := (wertA SHL anz) BOR (wertQ SHR (48-anz)) ;
						wertQ := (wertQ SHL anz) BOR (hilf SHR (48-anz)) ;
					END ;
				ELSE
					WHILE anz >= 48 DO
						wertA := wertQ ;
						wertQ := 0 ;
						DEC (anz, 48) ;
					END ;
					IF anz <> 0 THEN
						wertA := (wertA SHL anz) BOR (wertQ SHR (48-anz)) ;
						wertQ := wertQ SHL anz ;
					END ;
				END ;
			ELSE
				IF imKreis THEN
					WHILE anz >= 48 DO
						hilf := wertA ;
						wertA := wertQ ;
						wertQ := hilf ;
						DEC (anz, 48) ;
					END ;
					IF anz <> 0 THEN
						hilf := wertA ;
						wertA := (wertA SHR anz) BOR (wertQ SHL (48-anz)) ;
						wertQ := (wertQ SHR anz) BOR (hilf SHL (48-anz)) ;
					END ;
				ELSE
					WHILE anz >= 48 DO
						wertQ := wertA ;
						wertA := 0 ;
						DEC (anz, 48) ;
					END ;
					IF anz <> 0 THEN
						wertQ := (wertQ SHR anz) BOR (wertA SHL (48-anz)) ;
						wertA := wertA SHR anz ;
					END ;
				END ;
			END ;
		ELSE

			wertA := RegA.inh BAND Max48bitWertPositiv ;
			wertQ := RegQ.inh BAND Max48bitWertPositiv ;

			IF links THEN
				negativ := (RegQ.inh BAND MarkenBit) <> 0 ;
				IF (RegA.inh BAND MarkenBit) <> (RegQ.inh BAND MarkenBit) THEN
					wertA := BNOT wertA ;
				END ;
				wertA := (wertA SHL anz) BOR (wertQ SHR (46-anz)) ;
				wertQ := (wertQ SHL anz) BAND Max48bitWertPositiv ;
				IF negativ THEN
					wertQ := wertQ BOR (Max48bitWertPositiv SHR (46-anz)) ;
				END ;
			ELSE
				negativ := (RegA.inh BAND MarkenBit) <> 0 ;
				IF (RegA.inh BAND MarkenBit) <> (RegQ.inh BAND MarkenBit) THEN
					wertQ := BNOT wertQ ;
				END ;
				wertQ := (wertQ SHR anz) BOR ((wertA SHL (46-anz)) BAND Max48bitWertPositiv) ;
				wertA := wertA SHR anz ;
				IF negativ THEN
					wertA := wertA BOR (Max48bitWertPositiv SHL (46-anz)) ;
				END ;
			END ;
			IF negativ THEN
				wertA := (wertA BAND Max48bitWertPositiv) BOR Vorzeichen48Bit ;
				wertQ := (wertQ BAND Max48bitWertPositiv) BOR Vorzeichen48Bit ;
			END ;
			IF rundung THEN
				IF (wertQ BAND 200000000000H) <> 0 THEN		(* Bit 3 in Q gesetzt *)
					INC (wertA) ;
				END ;
				wertQ := 0 ;
			END ;
		END ;

		RegA.inh := wertA BAND Cardinal48Bit ;
		RegQ.inh := wertQ BAND Cardinal48Bit ;

	END ShiftZ ;

BEGIN
	mod2Check ;
	p := tr8to48 (AktAdressteil) ;
	s := AktAdressteil SHR 8 ;

	links := s BAND 20H <> 0 ;		(* L *)
	imKreis := s BAND 10H <> 0 ;		(* K *)
	rundung := s BAND 4 <> 0 ;		(* R *)
	unabhVonTK := s BAND 2 <> 0 ;		(* U *)
	zaehlen := s BAND 1 <> 0 ;		(* B *)

	IF zaehlen THEN
		RegY := 0 ;
	END ;

	IF unabhVonTK THEN
		rundung := FALSE ;
	END ;

	IF ((p = 0 (* nix zu shiften *) ) AND NOT rundung) OR (s BAND 0C8H = 0 (* kein Register angegeben *) ) THEN
		RETURN ;
	END ;
	IF p < 0 THEN
		anz := VAL (CARDINAL8, 0 - p) ;
		links := NOT links ;
	ELSE
		anz := VAL (CARDINAL8, p) ;
	END ;

	IF s BAND 8H <> 0 THEN			(* Z *)
		IF imKreis THEN
			rundung := FALSE ;
		END ;
		IF zaehlen THEN
			ShiftZaehlenZ ;
		END ;
		ShiftZ ;
		IF rundung THEN
			IF trNegativ (RegA.inh) THEN
				IF (RegQ.inh BAND 200000000000H) = 0 THEN
					RegA.inh := trSub (RegA.TK, RegA.inh, 1) ;
				END ;
			ELSE
				IF (RegQ.inh BAND 200000000000H) <> 0 THEN
					RegA.inh := trAdd (RegA.TK, RegA.inh, 1) ;
				END ;
			END ;
			RegQ.inh := 0 ;
		END ;
	ELSE
		anz := anz MOD 48 ;
		IF s BAND 80H <> 0 THEN		(* A *)
			IF links OR imKreis THEN
				rundung := FALSE ;
			END ;
			merkwert := RegA.inh ;
			IF zaehlen THEN
				ShiftZaehlen ;
			END ;
			shift (RegA) ;
			IF rundung THEN
				IF trNegativ (RegA.inh) THEN
					IF NOT ODD (merkwert SHR (anz-1)) THEN
						RegA.inh := trSub (RegA.TK, RegA.inh, 1) ;
					END ;
				ELSE
					IF ODD (merkwert SHR (anz-1)) THEN
						RegA.inh := trAdd (RegA.TK, RegA.inh, 1) ;
					END ;
				END ;
			END ;

		END ;
		IF s BAND 40H <> 0 THEN		(* Q *)
			shift (RegQ) ;
		END ;
	END ;
	Takte ((p DIV 4 + p MOD 4) * 2 + 5) ;
END _SH;


PROCEDURE _SHB ;
	VAR	p :	CARDINAL8 ;
BEGIN
	mod2Ohne;
	p := AktAdressteil ;
	IF AktAdressteil BAND 8000H <> 0 THEN		(* links *)
		RegB := (RegB SHL p) BAND Cardinal24Bit ;
	ELSE
		RegB := (RegB BAND Cardinal24Bit) SHR p ;
	END ;
	Takte (p * 4 + 6) ;
END _SHB;


PROCEDURE _VAQ ;
BEGIN
	mod2Check ;
	IF (RegA.TK <> 1) (* OR (RegQ.TK <> 1) *) THEN
		TypenkennungsAlarm (RegF) ;
	END ;

	IF (RegA.inh BAND MarkenBit) <> (RegQ.inh BAND MarkenBit) THEN
		IF (trVgl (RegA.inh, 0) = gleich) AND (trVgl (RegQ.inh, 0) <> gleich) THEN
			(*
			IF trNegativ (RegQ.inh) THEN
				RegQ.inh := trSub (RegQ.inh, 1) ;
			ELSE
				RegQ.inh := trAdd (RegQ.inh, 1) ;
			END ;
			*)
			RegA.inh := 0FFFFFFFFFFFFH ;
		ELSIF trVgl (RegQ.inh, 0) = gleich THEN
			RegQ.inh := 0FFFFFFFFFFFFH ;
		ELSE
			IF trNegativ (RegQ.inh) THEN
				RegA.inh := trSub (1, RegA.inh, 1) ;
			ELSE
				RegA.inh := trAdd (1, RegA.inh, 1) ;
				IF trVgl (RegA.inh, 0) = gleich THEN
					RegA.inh := 0FFFFFFFFFFFFH ;
				END ;
			END ;
			RegQ.inh := RegQ.inh BXOR Vorzeichen48Bit ;		(* trInvert (RegQ.inh) ; *)
		END ;
		Takte (17) ;
	ELSE
		Takte (3) ;
	END ;
END _VAQ;


PROCEDURE _NRM ;
	VAR	mantisse,
		mantisse2 :	CARDINAL64 ;
		anz,
		exponent :	INTEGER ;
		negativ :	BOOLEAN ;
BEGIN
	mod2Check ;
	CASE (AktAdressteil SHR 5) BAND 7 OF
	0 :	(* N *)
			IF (RegA.inh BAND Cardinal48Bit) = 0 THEN
				RegY := 48 ;
			ELSE
				RegY := 0 ;
				WHILE (RegA.inh BAND MarkenBit) = 0 DO
					RegA.inh := RegA.inh SHL 1 ;
					INC (RegY) ;
				END ;
				RegA.inh := RegA.inh BAND Cardinal48Bit ;
				Takte (ORD (RegY) * 2 + 3) ;
			END ;
	|
	1 :	(* L *)
			RegY := 0 ;
			WHILE (RegA.inh BAND MarkenBit) <> 0 DO
				RegA.inh := RegA.inh SHL 1 ;
				INC (RegY) ;
			END ;
			RegA.inh := RegA.inh BAND Cardinal48Bit ;
			Takte (ORD (RegY) * 2 + 3) ;
	|
	2 :	(* F *)
			IF (RegA.TK <> 1) OR (RegQ.TK <> 1) THEN
				TypenkennungsAlarm (RegF) ;
			ELSIF (RegA.inh BAND Vorzeichen48Bit) <> (RegQ.inh BAND Vorzeichen48Bit) THEN
				ArithmetischerAlarm ;
			ELSE
						(* <A,Q> := <A,Q> normalisiert *)
				RegY := 0 ;
				negativ := (RegA.inh BAND Vorzeichen48Bit) <> 0 ;
				mantisse := RegA.inh BAND 3FFFFFFFFFFFH ;
				mantisse2 := RegQ.inh BAND 3FFFFFFFFFFFH ;
				IF negativ THEN					(* negative Zahl normalisieren *)
					IF (mantisse <> 3FFFFFFFFFFFH) OR (mantisse2 <> 3FFFFFFFFFFFH) THEN
						WHILE (mantisse BAND 3C0000000000H) = 3C0000000000H DO
							mantisse := (mantisse SHL 4) BOR ((mantisse2 SHR 42) BAND 0FH) ;
							mantisse2 := (mantisse2 SHL 4) BOR 0FH ;
							INC (RegY, 4) ;
						END ;
					END ;
				ELSIF (mantisse <> 0) OR (mantisse2 <> 0) THEN
					WHILE (mantisse BAND 3C0000000000H) = 0 DO
						mantisse := mantisse SHL 4 BOR ((mantisse2 SHR 42) BAND 0FH) ;
						mantisse2 := mantisse2 SHL 4 ;
						INC (RegY, 4) ;
					END ;
				END ;
				RegA.inh := mantisse BAND 3FFFFFFFFFFFH ;
				IF negativ THEN
					RegA.inh := RegA.inh BOR Negativ48Bit  ;
				END ;
				Takte (ORD (RegY) DIV 2 + 7) ;
			END ;
	|
	3 :	(* F4 *)
			IF (RegA.TK <> 1) OR (RegQ.TK <> 1) THEN
				TypenkennungsAlarm (RegF) ;
			ELSIF (RegA.inh BAND MarkenBit) <> (RegQ.inh BAND MarkenBit) THEN
				ArithmetischerAlarm ;
			ELSE
						(* <A,Q> := <A,Q> normalisiert *)
				RegY := 0 ;
				negativ := (RegA.inh BAND Vorzeichen48Bit) <> 0 ;
				mantisse := RegA.inh BAND 3FFFFFFFFFFFH ;
				mantisse2 := RegQ.inh BAND 3FFFFFFFFFFFH ;
				IF negativ THEN					(* negative Zahl normalisieren *)
					IF (mantisse <> 3FFFFFFFFFFFH) OR (mantisse2 <> 3FFFFFFFFFFFH) THEN
						WHILE (mantisse BAND 3C0000000000H) = 3C0000000000H DO
							mantisse := (mantisse SHL 4) BOR ((mantisse2 SHR 42) BAND 0FH) ;
							mantisse2 := (mantisse2 SHL 4) BOR 0FH ;
							INC (RegY) ;
						END ;
					END ;
				ELSIF (mantisse <> 0) OR (mantisse2 <> 0) THEN
					WHILE (mantisse BAND 3C0000000000H) = 0 DO
						mantisse := mantisse SHL 4 BOR ((mantisse2 SHR 42) BAND 0FH) ;
						mantisse2 := mantisse2 SHL 4 ;
						INC (RegY) ;
					END ;
				END ;
				RegA.inh := mantisse BAND 3FFFFFFFFFFFH ;
				IF negativ THEN
					RegA.inh := RegA.inh BOR Negativ48Bit  ;
				END ;
				Takte (RegY * 2 + 7) ;
			END ;
	|
	4 :	(* G *)
			IF RegA.TK <> 0 THEN
				TypenkennungsAlarm (RegF) ;
			ELSE
				RegY := 0 ;
				negativ := (RegA.inh BAND Vorzeichen48Bit) <> 0 ;
				mantisse := RegA.inh BAND 3FFFFFFFFF00H ;
				exponent := tr8to48 (RegA.inh) ;
				IF negativ THEN					(* negative Zahl normalisieren *)
					IF mantisse <> 3FFFFFFFFF00H THEN
						WHILE (mantisse BAND 3C0000000000H) = 3C0000000000H DO
							mantisse := (mantisse SHL 4) BOR 0F00H ;
							INC (RegY, 4) ;
							DEC (exponent) ;
						END ;
					END ;
				ELSIF mantisse <> 0 THEN
					WHILE (mantisse BAND 3C0000000000H) = 0 DO
						mantisse := mantisse SHL 4 ;
						INC (RegY, 4) ;
						DEC (exponent) ;
					END ;
				END ;
				RegA.inh := (mantisse BAND Max48bitWertPositiv) BOR VAL (CARDINAL64, (exponent BAND 0FFH)) ;
				IF negativ THEN
					RegA.inh := RegA.inh BOR Negativ48Bit  ;
				END ;
				Takte (ORD (RegY) DIV 2 + 3) ;
			END ;
	|
	6 :	(* FG *)
			IF (RegA.TK <> 1) OR (RegQ.TK <> 1) THEN
				TypenkennungsAlarm (RegF) ;
			ELSIF (RegA.inh BAND MarkenBit) <> (RegQ.inh BAND MarkenBit) THEN
				ArithmetischerAlarm ;
			ELSE
						(* A gleitkomma := <A,Q> festkomma ; Q := 0 *)
				negativ := (RegA.inh BAND MarkenBit) <> 0 ;
				mantisse := RegA.inh BAND 3FFFFFFFFFFFH ;
				mantisse2 := RegQ.inh BAND 3FFFFFFFFFFFH ;
				exponent := 0 ;
				anz := 0 ;
				IF negativ THEN					(* negative Zahl normalisieren *)
					IF (mantisse <> 3FFFFFFFFFFFH) OR (mantisse2 <> 3FFFFFFFFFFFH) THEN
						WHILE (mantisse BAND 3C0000000000H) = 3C0000000000H DO
							mantisse := (mantisse SHL 4) BOR ((mantisse2 SHR 42) BAND 0FH) ;
							mantisse2 := (mantisse2 SHL 4) BOR 0FH ;
							INC (anz) ;
							DEC (exponent) ;
						END ;
					END ;
				ELSIF (mantisse <> 0) OR (mantisse2 <> 0) THEN
					WHILE (mantisse BAND 3C0000000000H) = 0 DO
						mantisse := mantisse SHL 4 BOR ((mantisse2 SHR 42) BAND 0FH) ;
						mantisse2 := mantisse2 SHL 4 ;
						INC (anz) ;
						DEC (exponent) ;
					END ;
				END ;
				IF exponent < 0 THEN
					DEC (exponent) ;		(* B-1 *)
				END ;
				RegA.inh := (mantisse BAND 3FFFFFFFFF00H) BOR VAL (CARDINAL64, (exponent BAND 0FFH)) ;
				IF negativ THEN
					RegA.inh := RegA.inh BOR Negativ48Bit  ;
				END ;
				RegQ.inh := 0 ;
				RegQ.TK := 1 ;
				RegA.TK := 0 ;
				RegY := 0 ;
				Takte (anz * 2 + 10) ;
			END ;
	ELSE
		BefehlsAlarm ;
	END ;

END _NRM;


PROCEDURE _KDFR ;
	VAR	zahl,
		wert :	CARDINAL64 ;
		i,
		p,
		anz :	CARDINAL ;
BEGIN
	mod2Check ;
	p := AktAdressteil BAND 0FH ;
	IF p > 13 THEN
		BefehlsAlarm ;
		RETURN ;
	END ;
	anz := p ;
	wert := RegQ.inh SHL 16 ;
	IF anz > 12 THEN
		zahl := RegA.inh BAND 0FH ;
		anz := 12 ;
	ELSE
		zahl := 0 ;
		IF anz < 12 THEN
			wert := wert SHL ((12 - anz) * 4) ;
		END ;
	END ;

	FOR i := 1 TO anz DO
		wert := wert ROL 4 ;
		zahl := zahl * 10 + (wert BAND 0FH) ;
	END ;

	RegA.inh := zahl ;
	RegA.TK := 1 ;

	RegQ.TK := 1 ;
	RegQ.inh := 0 ;
	RegD := RegQ ;
	RegH := RegQ ;

	Takte (p * 7 + 22) ;
END _KDFR;


PROCEDURE _KFLD ;
	VAR	zahl,
		wert :	CARDINAL64 ;
		p,
		i,
		anz :	CARDINAL ;
BEGIN
	mod2Check ;
	p := AktAdressteil BAND 0FH ;
	IF (p= 0 ) OR (p > 13) THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	zahl := RegA.inh ;
	anz := p ;
	wert := 0 ;
	IF RegA.TK > 1 THEN
		TypenkennungsAlarm (RegF) ;
		RETURN ;
	END ;
	IF (RegA.inh BAND Vorzeichen48Bit) <> 0 THEN
		ArithmetischerAlarm ;
		RETURN ;
	END ;
	FOR i := 1 TO anz DO
		zahl := zahl BAND 3FFFFFFFFFFFH ;
		wert := wert SHL 4 ;
		zahl := zahl * 10 ;
		wert := wert BOR (zahl SHR 46) ;
	END ;
	RegQ.inh := wert BAND VAL (CARDINAL64, 0FFFFFFFFFFFFH) ;
	RegA.inh := wert SHR 48 ;
	RegD.inh := (zahl SHR 2) BAND 3FFFFFFFFFFFH ;
	RegA.TK := 1 ;
	RegQ.TK := 1 ;
	Takte (p * 9 + 5) ;
END _KFLD;



END Aufbereitung.
