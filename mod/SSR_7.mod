
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_7;


(*	28.01.16	*)


FROM AbwicklerUp IMPORT * ;


PROCEDURE _SSR_7_0 ;
BEGIN
	SSRnimpl ('Verteiler-SSR durchreichen') ;
END _SSR_7_0 ;


PROCEDURE _SSR_7_4 ;
BEGIN
	SSRnimpl ('Modusabhängiger Rerun') ;
END _SSR_7_4 ;


PROCEDURE _SSR_7_8 ;
BEGIN
	SSRnimpl ('Ausliefern Systemdaten') ;
END _SSR_7_8 ;


PROCEDURE _SSR_7_12 ;
BEGIN
	SSRnimpl ('Statistikinformationen eintragen') ;
END _SSR_7_12 ;


PROCEDURE _SSR_7_16 ;
BEGIN
	SSRnimpl ('Botschaften durchreichen') ;
END _SSR_7_16 ;


PROCEDURE _SSR_7_20 ;
BEGIN
	SSRnimpl ('Steuerung von Messungen') ;
END _SSR_7_20 ;




END SSR_7.
