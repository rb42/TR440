
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_2;

(*	29.03.18	*)


FROM AbwicklerUp IMPORT * ;


PROCEDURE _SSR_2_0 ;
BEGIN
	SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
END _SSR_2_0 ;


END SSR_2.
