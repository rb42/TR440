
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upVLI;

(* 10.01.16 *)

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;
FROM upFestkomma IMPORT * ;

IMPORT VLIkern ;


VAR
	VLIinitialisiert :	BOOLEAN = FALSE ;


PROCEDURE InitVLI ;
BEGIN
	IF NOT VLIinitialisiert THEN
		VLIinitialisiert := TRUE ;
		VLIa := VLIkern.Create () ;
		VLIb := VLIkern.Create () ;
		VLIc := VLIkern.Create () ;
		VLId := VLIkern.Create () ;
		VLIhilf1 := VLIkern.Create () ;
		VLIhilf2 := VLIkern.Create () ;
		VLIhilf3 := VLIkern.Create () ;
	END ;
END InitVLI ;


PROCEDURE wert48ToVLI (wert : CARDINAL64 ; VAR INOUT vli : VLIkern.VLI) ;
BEGIN
	VLIkern.SetValue (vli, tr48toInt64 (wert)) ;
END wert48ToVLI ;


PROCEDURE EinfachToVLI (reg : reg48 ; VAR INOUT vli : VLIkern.VLI) ;
BEGIN
	wert48ToVLI (reg.inh, vli) ;
END EinfachToVLI ;


PROCEDURE VLItoEinfach (vli : VLIkern.VLI ; VAR reg : reg48) : BOOLEAN ;
	VAR	wert :	INTEGER64 ;
BEGIN
	reg.TK := 1 ;
	IF VLIkern.GetValue (vli, wert) THEN
		reg.inh := trInt64to48 (wert) ;
		IF VLIkern.IsNegative (vli) THEN
			IF (reg.inh BAND Vorzeichen48Bit) <> Vorzeichen48Bit THEN
				RETURN FALSE ;				(* Unterlauf *)
			END ;
		ELSE
			IF reg.inh > Max48bitWertPositiv THEN
				RETURN FALSE ;				(* Überlauf *)
			END ;
		END ;
		IF ((reg.inh BAND MarkenBit) = 0) <> ((reg.inh BAND VorzeichenBit) = 0) THEN
			RETURN FALSE ;					(* Überlauf / Unterlauf *)
		END ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END VLItoEinfach ;


PROCEDURE DoppeltToVLI (regHi, regLo : reg48 ; VAR INOUT vli : VLIkern.VLI) ;
BEGIN
	EinfachToVLI (regHi, VLIhilf1) ;					(* Vorzeichen berücksichtigen		*)
	VLIkern.SetValue (VLIhilf2, 400000000000H) ;
	VLIkern.Multiply (VLIhilf1, VLIhilf2, VLIhilf3) ;			(* regHi SHL 46				*)
	VLIkern.SetValue (VLIhilf2, regLo.inh (* BAND 3FFFFFFFFFFFH *)) ;	(* nur rechte 46 Bits berücksichtigen ?	*)
	VLIkern.Add (VLIhilf3, VLIhilf2, vli) ;					(* (regHi SHL 46) + regLo		*)
END DoppeltToVLI ;


PROCEDURE VLItoDoppelt (vli : VLIkern.VLI ; VAR regHi, regLo : reg48) : BOOLEAN ;
	VAR	wertHi,
		wertLo :	INTEGER64 ;
		anz :		CARDINAL ;
		digits :	ARRAY [0..15] OF VLIkern.Digit ;
		neg,
		ok :		BOOLEAN ;
BEGIN

	anz := 15 ;
	IF NOT VLIkern.GetDigits(vli, digits, anz, neg) THEN
		RETURN FALSE ;
	END ;
	ok := TRUE ;
	wertLo := digits [0] ;
	wertHi := 0 ;
	IF anz > 1 THEN
		wertLo := wertLo + (VAL (INTEGER64, digits [1] BAND 3FFFH) SHL 32) ;	(* untere 46 Bits *)
		wertHi := digits [1] SHR 14 ;
		IF anz > 2 THEN
			wertHi := wertHi BOR (VAL (INTEGER64, digits [2]) SHL 18) ;
			IF anz > 3 THEN
				ok := FALSE ;				(* Zahl zu groß *)
			END ;
		END ;
	END ;
	IF neg THEN
		regHi.inh := trInvert (wertHi) ;
		regLo.inh := trInt64to48 (0 - wertLo) ;
	ELSE
		regHi.inh := wertHi ;
		regLo.inh := wertLo ;
	END ;
	regHi.TK := 1 ;
	regLo.TK := 1 ;
	IF VAL (CARDINAL64, wertHi) > Max48bitWertPositiv THEN
		RETURN FALSE ;						(* Überlauf *)
	END ;
	RETURN ok ;
END VLItoDoppelt ;


END upVLI.
