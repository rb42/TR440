
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE debug;

(*	20.06.18	*)

FROM Register IMPORT * ;

FROM Trace IMPORT
	TraceF ;

FROM AsmDisasm IMPORT
	CardToHex2, CardToHex4, CardToHex6, CardToHex12 ;


VAR
	vRegA,					(* Akkumulator			*)
	vRegQ,					(* Quotientenregister		*)
	vRegD,					(* Multiplikandenregister	*)
	vRegH :		reg48 ;			(* Hilfsregister		*)

	vRegY :		CARDINAL8 ;			(* Shiftzähler			 8 Bits		*)

	vRegM :		BOOLEAN ;		(* Markenregister		 1  Bit		*)


		(* Befehlswerk : *)
	vRegB,					(* Bereitadressregister		24 Bits		*)
	vRegF,					(* Befehlsfolgeregister		24 Bits		*)
	vRegX :					(* Indexbasisregister		22 Bits		*)
			CARDINAL ;

	vRegK,					(* Merlichterregister		 8 Bits		*)
	vRegU :		CARDINAL8 ;		(* Unterprogrammregister	 8 Bits		*)


	initialisiert :	BOOLEAN = FALSE ;


PROCEDURE alle (ja : BOOLEAN) ;
BEGIN
	RegDump := ja ;
	Befehle := ja ;
	Einzelschritt := ja ;
	AlarmEin := ja ;
	OriginalZeile := ja ;
	SSRprotokoll := ja ;
	BildschirmProtokoll := ja ;
	EinBeiProgStart := ja ;
	TakteZeigen := ja ;

	an := ja ;
	Lesen := ja ;
	Speichern := ja ;
	BefehlLesen := ja ;
	IndexWert := ja ;
	ZweitBefehle := ja ;
	AssoziativProtokoll := ja ;
	Dateien := ja ;
	AssemblerProtokoll := ja ;
	MemoryProtokoll := ja ;
END alle ;


PROCEDURE InitRegDump ;
BEGIN
	RegA.TK := 3 ;
	RegA.inh := 0 ;
	RegQ := RegA ;
	RegD := RegA ;
	RegH := RegA ;
	RegB := 0 ;
	RegM := FALSE ;
	RegY := 0 ;
	RegK := 0 ;
	vRegA := RegA ;
	vRegQ := RegQ ;
	vRegD := RegD ;
	vRegH := RegH ;
	vRegY := RegY ;
	vRegM := RegM ;
	vRegB := RegB ;
	vRegF := RegF ;
	vRegX := RegX ;
	vRegK := RegK ;
	vRegU := RegU ;
END InitRegDump ;


PROCEDURE aend48 (VAR vReg, Reg : reg48) : BOOLEAN ;
BEGIN
	RETURN (vReg.TK <> Reg.TK) OR (vReg.inh <> Reg.inh) ;
END aend48 ;


PROCEDURE RegisterDumpen ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF aend48 (vRegA, RegA) THEN
		CardToHex12 (RegA.inh, str) ;
		TraceF ('RA <- %c %s', ORD(RegA.TK), str) ;
		vRegA := RegA ;
	END ;
	IF aend48 (vRegH, RegH) THEN
		CardToHex12 (RegH.inh, str) ;
		TraceF ('RH <- %c %s', ORD(RegH.TK), str) ;
		vRegH := RegH ;
	END ;
	IF aend48 (vRegQ, RegQ) THEN
		CardToHex12 (RegQ.inh, str) ;
		TraceF ('RQ <- %c %s', ORD(RegQ.TK), str) ;
		vRegQ := RegQ ;
	END ;
	IF aend48 (vRegD, RegD) THEN
		CardToHex12 (RegD.inh, str) ;
		TraceF ('RD <- %c %s', ORD(RegD.TK), str) ;
		vRegD := RegD ;
	END ;
	IF vRegY <> RegY THEN
		CardToHex2 (ORD(RegY), str) ;
		TraceF ('RY <- %s', str) ;
		vRegY := RegY ;
	END ;
	IF vRegM <> RegM THEN
		TraceF ('RM <- %h', ORD(RegM)) ;
		vRegM := RegM ;
	END ;
	IF vRegB <> RegB THEN
		CardToHex4 (RegB BAND 0FFFFFFH, str) ;
		TraceF ('BB <- %s', str) ;
		vRegB := RegB ;
	END ;
(*
	IF vRegF <> RegF THEN
		CardToHex4 (RegF BAND 0FFFFFFH, str) ;
		TraceF ('BF <- %s', str) ;
		vRegF := RegF ;
	END ;
*)
	IF vRegX <> RegX THEN
		CardToHex4 (RegX BAND 0FFFFFFH, str) ;
		TraceF ('BX <- %s', str) ;
		vRegX := RegX ;
	END ;
	IF vRegK <> RegK THEN
		CardToHex2 (ORD(RegK), str) ;
		TraceF ('BK <- %s', str) ;
		vRegK := RegK ;
	END ;
	IF vRegU <> RegU THEN
		CardToHex2 (ORD(RegU), str) ;
		TraceF ('BU <- %s', str) ;
		vRegU := RegU ;
	END ;
END RegisterDumpen ;


PROCEDURE Rmod1 ;
BEGIN
	IF RegDump THEN
		IF mod1 <> 0 THEN
			TraceF ("mod1 <- %'06h", mod1) ;
		END ;
	END ;
END Rmod1 ;


PROCEDURE Rmod2 ;
BEGIN
	IF RegDump THEN
		IF mod2 <> 0 THEN
			TraceF ("mod2 <- %'06h", mod2) ;
		END ;
	END ;
END Rmod2 ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		InitRegDump ;
	END ;
END debug.
