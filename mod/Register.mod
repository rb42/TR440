
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Register;

(*	20.12.15	*)


VAR
	initialisiert :	BOOLEAN = FALSE ;


PROCEDURE SaveRegister (VAR reg : tRegBlock) ;
BEGIN
	reg.A := RegA ;
	reg.Q := RegQ ;
	reg.D := RegD ;
	reg.H := RegH ;

	reg.B := RegB ;
	reg.F := RegF ;
	reg.X := RegX ;
	reg.T := RegT ;

	reg.Y := RegY ;
	reg.K := RegK ;
	reg.U := RegU ;

	reg.M := RegM ;
END SaveRegister ;


PROCEDURE LoadRegister (VAR reg : tRegBlock) ;
BEGIN
	RegA := reg.A ;
	RegQ := reg.Q ;
	RegD := reg.D ;
	RegH := reg.H ;

	RegB := reg.B ;
	RegF := reg.F ;
	RegX := reg.X ;
	RegT := reg.T ;

	RegY := reg.Y ;
	RegK := reg.K ;
	RegU := reg.U ;

	RegM := reg.M ;
END LoadRegister ;


PROCEDURE InitReg ;
BEGIN
	mod1 := 0 ;
	mod2 := 0 ;
END InitReg ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		InitReg ;
	END ;
END Register.
