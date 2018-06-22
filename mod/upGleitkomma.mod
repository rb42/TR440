
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upGleitkomma;

(*	09.01.16	*)

FROM SYSTEM IMPORT
	CAST ;

FROM Struktur IMPORT * ;

FROM upFestkomma IMPORT * ;

FROM Register IMPORT
	tVgl ;


CONST
	maskVz1 =		0000C00000000000H ;
	maskMant1 =		00003FFFFFFFFF00H ;
	maskVzExp =		0000000000000080H ;
	maskExp =		000000000000007FH ;

	maskVz2 =		0000C00000000000H ;
	maskMant2 =		00003FFFFFFFFFFFH ;

	verstecktesBit =	0000200000000000H ;

	IntelBIAS =		1023 ;


PROCEDURE gkTR440toIntel (gw1, gw2 : CARDINAL64 ; VAR lr : LONGREAL) : BOOLEAN ;
	VAR	mant,
		mant2,
		mant3,
		zw :	CARDINAL64 ;
		exp2,
		exp16 :	INTEGER ;
		neg :	BOOLEAN ;
BEGIN
	mant := ((gw1 BAND maskMant1) SHL 18) BOR ((gw2 BAND maskMant2) SHR 20) ;	(* Mantisse linksbündig in 64-Bit *)
	mant3 := gw2 ROR 20 ;								(* Mantissenrest linksbündig *)

	exp16 := tr24toInt32 (tr8to24 (gw1)) ;
	exp2 := exp16 * 4 ;
	neg := (gw1 BAND maskVz1) <> 0 ;
	IF neg THEN		(* Mantisse negativ *)
		mant := BNOT mant ;
		mant3 := BNOT mant3 ;
	END ;

	IF mant = 0 THEN
		lr := 0.0 ;
		RETURN TRUE ;
	END ;

	WHILE mant <= 7FFFFFFFFFFFFFFFH DO	(* führende 0-Bits rausshiften *)
		mant := mant SHL 1 ;
		exp2 := exp2 - 1 ;
		mant3 := mant3 ROL 1 ;
		IF ODD (mant3) THEN
			INC (mant) ;
		END ;
	END ;

	mant := mant SHL 1 ;			(* zu versteckendes 1. L-Bit wegshiften *)
	exp2 := exp2 - 1 ;
	mant3 := mant3 ROL 1 ;
	IF ODD (mant3) THEN			(* aufrunden *)
		INC (mant) ;
	END ;

	exp2 := exp2 + IntelBIAS ;			(* BIAS zum Exponenten *)

	mant2 := (mant SHR 12) ;
	IF (mant BAND 0FFFH) > 800H THEN		(* Mantissenrest mit Rundung *)
		INC (mant2) ;
	END ;
	zw := (VAL (CARDINAL64, exp2 BAND 7FFH) SHL 52 ) BOR mant2 ;
	IF neg THEN
		zw := zw BOR VAL (CARDINAL64, 8000000000000000H) ;
	END ;
	lr := CAST (LONGREAL, zw) ;
	RETURN TRUE ;
END gkTR440toIntel ;


PROCEDURE gkIntelToTR440 (lr : LONGREAL ; VAR gw1, gw2 : CARDINAL64) : BOOLEAN ;
	VAR	mant,
		mant2 :	CARDINAL64 ;
		exp2,
		exp16 :	INTEGER ;
BEGIN
	IF lr = 0.0 THEN
		gw1 := 0 ;
		gw2 := 0 ;
		RETURN TRUE ;
	END ;
	exp2 := (CAST (CARDINAL64, lr) SHR 52) BAND 7FFH ;
	exp2 := exp2 - IntelBIAS + 1 ;
	mant := (CAST (CARDINAL64, lr) BOR 10000000000000H) SHL 11 ;		(*	Mantisse linksbündig in 64-Bit incl. verstecktem IEEE 754 - Bit davor	*)

	mant := mant SHR (4 - exp2 MOD 4) ;						(* auf exp16-Grenze geshiftet *)

	IF exp2 < 0 THEN
		exp16 := exp2 DIV 4 ;
	ELSE
		exp16 := exp2 DIV 4 + 1 ;						(* exp2 -> exp16 *)
	END ;

	IF lr < 0.0 THEN
		mant := BNOT (mant SHR 2) ;					(* negativ mit 2 passenden Vorzeichenbits davor *)
		WHILE (mant BAND 3C00000000000000H) = 3C00000000000000H DO
			mant := (mant SHL 4) BOR 0FH ;				(* negative Zahl normalisieren *)
			exp16 := exp16 - 1 ;
		END ;
		mant := mant BOR VAL (CARDINAL64, 0C000000000000000H) ;			(* negatives Vorzeichen dazu *)
	ELSE
		mant := mant SHR 2 ;						(* positiv mit 2 0-Vorzeichenbits davor *)
		WHILE (mant BAND 3C00000000000000H) = 0 DO
			mant := mant SHL 4 ;					(* positive Zahl normalisieren *)
			exp16 := exp16 - 1 ;
		END ;
	END ;
	IF exp16 < 0 THEN
		IF exp16 < -128 THEN						(* zahl zu klein für TR440 => -0.0 *)
			gw1 := 0FFFFFFFFFF80H ;
			gw2 := 0FFFFFFFFFFFFH ;
			RETURN TRUE ;
		END ;
	END ;

	gw1 := ((mant SHR 16) BAND 0FFFFFFFFFF00H) BOR VAL (CARDINAL64, exp16 BAND 0FFH) ;	(* 1. 46-Bit-Wort rechtsbündig mit führendem VZ *)
	gw2 := ((mant SHL 22) BAND  3FFFFFFFFFFFH) BOR (gw1 BAND  0C00000000000H) ;		(* 2. 46-Bit-Wort rechtsbündig mit führendem VZ *)

	IF exp16 > 127 THEN							(* zahl zu groß für TR440 => Überlauf *)
		RETURN FALSE ;
	END ;

	RETURN TRUE ;
END gkIntelToTR440 ;


PROCEDURE gkIntelToTR440runden (lr : LONGREAL ; VAR gw1 : CARDINAL64) : BOOLEAN ;
	VAR	gw2 :	CARDINAL64 ;
BEGIN
	IF NOT gkIntelToTR440 (lr, gw1, gw2) THEN
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END gkIntelToTR440runden ;


PROCEDURE gkVgl (a, b : CARDINAL64) : tVgl ;
	VAR	r1,
		r2 :	LONGREAL ;
BEGIN
	gkTR440toIntel (a, 0, r1) ;
	gkTR440toIntel (b, 0, r2) ;
	IF r1 < r2 THEN
		RETURN kleiner ;
	ELSIF r1 > r2 THEN
		RETURN groesser ;
	ELSE
		RETURN gleich ;
	END ;
END gkVgl ;



END upGleitkomma.
