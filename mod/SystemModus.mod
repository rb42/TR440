
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SystemModus;

(*	12.01.16	*)

IMPORT Terminal ;

FROM Befehle IMPORT
	nimpl ;

FROM Struktur IMPORT * ;

FROM Trace IMPORT
	TraceF ;


PROCEDURE _VPU ;
BEGIN
	nimpl ;
END _VPU;

PROCEDURE _ZDP ;
BEGIN
	nimpl ;
END _ZDP;

PROCEDURE _Y ;
BEGIN
	nimpl ;
END _Y;

PROCEDURE _LEI ;
BEGIN
	nimpl ;
END _LEI;

PROCEDURE _VSS ;
BEGIN
	nimpl ;
END _VSS;

PROCEDURE _SW ;
BEGIN
	nimpl ;
END _SW;

PROCEDURE _HALT ;
BEGIN
	AlarmGewesen := TRUE ;
	globProgrammEnde := TRUE ;
END _HALT;

PROCEDURE _KB ;
	VAR	X, Y :	CARDINAL ;
BEGIN
	AlarmGewesen := TRUE ;
	KBbefehlGewesen := TRUE ;
	Terminal.GetPosition (X, Y) ;
	IF X <> 0 THEN
		Terminal.WriteLn ;
	END ;
	Terminal.WriteString ('*BRK*') ;
END _KB ;

PROCEDURE _VMO ;
BEGIN
	nimpl ;
END _VMO;


END SystemModus.
