
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upFestkomma;

(*	17.07.16	*)


FROM Register IMPORT
	AktAdressteil, tVgl ;

FROM Struktur IMPORT * ;

CONST
	vz24 =		0FFFFFF000000H ;
	vz16 =		0FFFFFFFF0000H ;
	vz8 =		0FFFFFFFFFF00H ;

	mask48 =	0FFFFFFFFFFFFH ;
	mask24 =	      0FFFFFFH ;
	mask16 =		0FFFFH ;
	mask8 =			  0FFH ;

	bit48 =		 800000000000H ;
	bit47 =		 400000000000H ;

	bit24 =		       800000H ;
	bit23 =		       400000H ;

	bit16 =			 8000H ;

	bit8 =			   80H ;




PROCEDURE negativ48 (x : B1_INT48) : BOOLEAN [INLINE] ;
BEGIN
	RETURN (x BAND bit48) <> 0 ;
END negativ48 ;


PROCEDURE negativ24 (x : B1_INT24) : BOOLEAN [INLINE] ;
BEGIN
	RETURN (x BAND bit24) <> 0 ;
END negativ24 ;


PROCEDURE negativ16 (x : B1_INT24) : BOOLEAN [INLINE] ;
BEGIN
	RETURN (x BAND bit16) <> 0 ;
END negativ16 ;


PROCEDURE negativ8 (x : B1_INT24) : BOOLEAN [INLINE] ;
BEGIN
	RETURN (x BAND bit8) <> 0 ;
END negativ8 ;


PROCEDURE invert (x : B1_INT48) : B1_INT48 [INLINE] ;
BEGIN
	RETURN (BNOT x) BAND mask48 ;
END invert ;


PROCEDURE z24 (x : B1_INT48) : B1_INT48 [INLINE] ;
BEGIN
	RETURN x BAND mask24 ;
END z24 ;


PROCEDURE invert24 (x : B1_INT24) : B1_INT24 [INLINE] ;
BEGIN
	RETURN z24 (BNOT x) ;
END invert24 ;


PROCEDURE uptr24toInt32 (a : B1_INT24) : INTEGER32 [INLINE] ;
BEGIN
	IF negativ24 (a) THEN
		RETURN 0 - invert24 (a) ;
	ELSE
		RETURN z24 (a) ;
	END ;
END uptr24toInt32 ;


PROCEDURE trAdd (TK : CARDINAL8; a, b : B1_INT48) : B1_INT48 ;
	VAR	erg :	CARDINAL64 ;
		aneg,
		bneg,
		inv :	BOOLEAN ;
BEGIN
	(*
	IF TK > 1 THEN
		RETURN a + b ;
	END ;
        *)

	inv := FALSE ;

	IF negativ48 (a) THEN
		a := invert (a) ;
		aneg := TRUE ;
	ELSE
		a := a BAND mask48 ;
		aneg := FALSE ;
	END ;

	IF negativ48 (b) THEN
		b := invert (b) ;
		bneg := TRUE ;
	ELSE
		b := b BAND mask48 ;
		bneg := FALSE ;
	END ;

	IF aneg = bneg THEN
		erg := a + b ;
		inv := aneg ;
	ELSIF a > b THEN
		erg := a - b ;
		inv := aneg ;
	ELSE
		erg := b - a ;
		inv := bneg ;
	END ;

	IF (TK < 2) AND (erg > Max48bitWertPositiv) THEN
		ArithmetischerAlarm ;
	END ;

	IF inv THEN
		RETURN invert (erg) ;
	ELSE
		RETURN erg BAND mask48 ;
	END ;
END trAdd ;


PROCEDURE trAdd24 (a, b : B1_INT24) : B1_INT24 ;
	VAR	erg :	CARDINAL64 ;
		aneg,
		bneg,
		inv :	BOOLEAN ;
BEGIN
	inv := FALSE ;

	IF negativ24 (a) THEN
		a := invert24 (a) ;
		aneg := TRUE ;
	ELSE
		a := z24 (a) ;
		aneg := FALSE ;
	END ;

	IF negativ24 (b) THEN
		b := invert24 (b) ;
		bneg := TRUE ;
	ELSE
		b := z24 (b) ;
		bneg := FALSE ;
	END ;

	IF aneg = bneg THEN
		erg := a + b ;
		inv := aneg ;
	ELSIF a > b THEN
		erg := a - b ;
		inv := aneg ;
	ELSE
		erg := b - a ;
		inv := bneg ;
	END ;

	IF inv THEN
		RETURN invert24 (erg) ;
	ELSE
		RETURN z24 (erg) ;
	END ;
END trAdd24 ;


PROCEDURE trSub (TK : CARDINAL8; a, b : B1_INT48) : B1_INT48 ;
BEGIN
	RETURN trAdd (TK, a, invert (b)) ;
END trSub ;


PROCEDURE trSub24 (a, b : B1_INT24) : B1_INT24 ;
BEGIN
	RETURN trAdd24 (a, invert24 (b)) ;
END trSub24 ;


PROCEDURE trInvert (a : B1_INT48) : B1_INT48 ;
BEGIN
	RETURN invert (a) ;
END trInvert ;


PROCEDURE trAbs (a : B1_INT48) : B1_INT48 ;
BEGIN
	IF negativ48 (a) THEN
		RETURN invert (a) ;
	ELSE
		RETURN a BAND mask48 ;
	END ;
END trAbs ;


PROCEDURE trAbs24 (a : B1_INT24) : B1_INT24 ;
BEGIN
	IF negativ24 (a) THEN
		RETURN invert24 (a) ;
	ELSE
		RETURN z24 (a) ;
	END ;
END trAbs24 ;


PROCEDURE trInvert24 (a : B1_INT24) : B1_INT24 ;
BEGIN
	RETURN invert24 (a) ;
END trInvert24 ;


PROCEDURE trMult (a, b : B1_INT48) : B1_INT48 ;
	VAR	inv :	BOOLEAN ;
		erg :	CARDINAL64 ;
BEGIN
	inv := FALSE ;
	IF negativ48 (a) THEN
		inv := TRUE ;
		a := invert (a) ;
	ELSE
		a := a BAND mask48 ;
	END ;
	IF negativ48 (b) THEN
		inv := NOT inv ;
		b := invert (b) ;
	ELSE
		b := b BAND mask48 ;
	END ;
	erg := a * b ;
	IF erg > Max48bitWertPositiv THEN
		ArithmetischerAlarm ;
	END ;
	IF inv THEN
		RETURN invert (erg) ;
	ELSE
		RETURN erg BAND mask48 ;
	END ;
END trMult ;


PROCEDURE trNegativ (a : B1_INT48) : BOOLEAN ;
BEGIN
	RETURN negativ48(a) ;
END trNegativ ;


PROCEDURE tr48Norm (a : B1_INT48) : B1_INT48 ;
BEGIN
	IF negativ48 (a) THEN
		IF (a BAND mask48) = mask48 THEN
			RETURN 0 ;		(* immer positive 0 *)
		END ;
	END ;
	RETURN a BAND mask48 ;
END tr48Norm ;


PROCEDURE tr8to48 (a : B1_INT24) : B1_INT48 ;
BEGIN
	IF negativ8 (a) THEN
		RETURN (VAL (B1_INT48, a) BAND mask8) BOR vz8 ;
	END ;
	RETURN VAL (B1_INT48, a) BAND mask8 ;
END tr8to48 ;


PROCEDURE tr8to24 (a : B1_INT24) : B1_INT24 ;
BEGIN
	RETURN z24 (tr8to48 (a)) ;
END tr8to24 ;


PROCEDURE tr16to48 (a : B1_INT24) : B1_INT48 ;
BEGIN
	IF negativ16 (a) THEN
		IF (a BAND mask16) = mask16 THEN
			RETURN 0 ;		(* immer positive 0 *)
		END ;
		RETURN (VAL (B1_INT48, a) BAND mask16) BOR vz16 ;
	END ;
	RETURN VAL (B1_INT48, a) BAND mask16 ;
END tr16to48 ;


PROCEDURE tr24to48 (a : B1_INT24) : B1_INT48 ;
BEGIN
	IF negativ24 (a) THEN
		IF (a BAND mask24) = mask24 THEN
			RETURN 0 ;		(* immer positive 0 *)
		END ;
		RETURN (VAL (B1_INT48, a) BAND mask24) BOR vz24 ;
	END ;
	RETURN VAL (B1_INT48, a) BAND mask24 ;
END tr24to48 ;


PROCEDURE trCard32to48 (a : CARDINAL) : B1_INT48 ;
	VAR	wert :	B1_INT48 ;
BEGIN
	wert := VAL (B1_INT48, a) ;
	RETURN wert ;
END trCard32to48 ;


PROCEDURE trCard64to48 (a : CARDINAL64) : B1_INT48 ;
BEGIN
	RETURN a BAND mask48 ;
END trCard64to48 ;


PROCEDURE trInt32to48 (a : INTEGER32) : B1_INT48 ;
	VAR	wert :	B1_INT48 ;
BEGIN
	IF a < 0 THEN
		wert := invert (0 - VAL (INTEGER64, a)) ;
	ELSE
		wert := VAL (CARDINAL64, a) ;
	END ;
	RETURN wert ;
END trInt32to48 ;


PROCEDURE trInt64to48 (a : INTEGER64) : B1_INT48 ;
BEGIN
	IF a < 0 THEN
		RETURN invert (0 - a) ;
	ELSE
		RETURN VAL (CARDINAL64, a) ;
	END ;
END trInt64to48 ;


PROCEDURE tr48toInt32 (a : B1_INT48) : INTEGER32 ;
BEGIN
	IF negativ48 (a) THEN
		RETURN 0 - VAL (INTEGER32, invert (a)) ;
	ELSE
		RETURN a ;
	END ;
END tr48toInt32 ;


PROCEDURE tr48toInt64 (a : B1_INT48) : INTEGER64 ;
BEGIN
	IF negativ48 (a) THEN
		RETURN 0 - VAL (INTEGER64, invert (a)) ;
	ELSE
		RETURN a ;
	END ;
END tr48toInt64 ;


PROCEDURE tr24toInt32 (a : B1_INT24) : INTEGER32 ;
BEGIN
	RETURN uptr24toInt32 (a) ;
END tr24toInt32 ;


PROCEDURE trHW (a : B1_INT48) : B1_INT24 ;
BEGIN
	RETURN z24 (a) ;
END trHW ;


PROCEDURE GetP8 () : B1_INT24 ;
BEGIN
	RETURN tr8to24 (AktAdressteil SHR 8) ;
END GetP8 ;


PROCEDURE trUeberlauf48 (a : B1_INT48) : BOOLEAN ;
BEGIN
	RETURN ((a BAND bit48) = 0) <> ((a BAND bit47) = 0) ;
END trUeberlauf48 ;


PROCEDURE trUeberlauf24 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN ((a BAND bit24) = 0) <> ((a BAND bit23) = 0) ;
END trUeberlauf24 ;


PROCEDURE trVglAbs (a, b : B1_INT48) : tVgl ;
BEGIN
	IF negativ48 (a) THEN
		a := invert (a) ;
	END ;
	IF negativ48 (b) THEN
		b := invert (b) ;
	END ;
	IF a < b THEN
		RETURN kleiner ;
	ELSIF a > b THEN
		RETURN groesser ;
	ELSE
		RETURN gleich ;
	END ;
END trVglAbs ;


PROCEDURE trVgl (a, b : B1_INT48) : tVgl ;
	VAR	aneg,
		bneg :	BOOLEAN ;
BEGIN
	IF negativ48 (a) THEN
		a := invert (a) ;
		IF a = 0 THEN		(* war negative 0 *)
			aneg := FALSE ;	(* wie positive 0 behandeln *)
		ELSE
			aneg := TRUE ;
		END ;
	ELSE
		a := a BAND mask48 ;
		aneg := FALSE ;
	END ;

	IF negativ48 (b) THEN
		b := invert (b) ;
		IF b = 0 THEN		(* war negative 0 *)
			bneg := FALSE ;	(* wie positive 0 behandeln *)
		ELSE
			bneg := TRUE ;
		END ;
	ELSE
		b := b BAND mask48 ;
		bneg := FALSE ;
	END ;

	IF aneg THEN
		IF bneg THEN
			   IF a < b THEN
				RETURN groesser ;
			ELSIF a > b THEN
				RETURN kleiner ;
			ELSE
				RETURN gleich ;
			END ;
		ELSE
			RETURN kleiner ;
		END ;
	ELSE
		IF bneg THEN
			RETURN groesser ;
		ELSE
			IF a < b THEN
				RETURN kleiner ;
			ELSIF a > b THEN
				RETURN groesser ;
			ELSE
				RETURN gleich ;
			END ;
		END ;
	END ;
END trVgl ;


PROCEDURE tr24Kleiner0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) < 0 ;
END tr24Kleiner0 ;


PROCEDURE tr24KleinerGleich0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) <= 0 ;
END tr24KleinerGleich0 ;


PROCEDURE tr24Gleich0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) = 0 ;
END tr24Gleich0 ;


PROCEDURE tr24Ungleich0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) <> 0 ;
END tr24Ungleich0 ;


PROCEDURE tr24GroesserGleich0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) >= 0 ;
END tr24GroesserGleich0 ;


PROCEDURE tr24Groesser0 (a : B1_INT24) : BOOLEAN ;
BEGIN
	RETURN uptr24toInt32 (a) > 0 ;
END tr24Groesser0 ;



END upFestkomma.
