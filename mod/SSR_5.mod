
(****************************************************************************************)
(*											*)
(*	Copyright � 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_5;

(*	28.01.16	*)


FROM AbwicklerUp IMPORT * ;


PROCEDURE _SSR_5_0 ;
BEGIN
	SSRnimpl ('Warteschlange kreieren') ;
END _SSR_5_0 ;


PROCEDURE _SSR_5_4 ;
BEGIN
	SSRnimpl ('Warteschlange l�schen') ;
END _SSR_5_4 ;


PROCEDURE _SSR_5_8 ;
BEGIN
	SSRnimpl ('Sendung absenden an Warteschlange') ;
END _SSR_5_8 ;


PROCEDURE _SSR_5_12 ;
BEGIN
	SSRnimpl ('Informieren �ber Sendungen') ;
END _SSR_5_12 ;


PROCEDURE _SSR_5_16 ;
BEGIN
	SSRnimpl ('Warten auf Sendungen') ;
END _SSR_5_16 ;


PROCEDURE _SSR_5_20 ;
BEGIN
	SSRnimpl ('Sendung �bernehmen') ;
END _SSR_5_20 ;


PROCEDURE _SSR_5_24 ;
BEGIN
	SSRnimpl ('Sendung l�schen') ;
END _SSR_5_24 ;


PROCEDURE _SSR_5_28 ;
BEGIN
	SSRnimpl ('Sendung zur�cksenden') ;
END _SSR_5_28 ;


PROCEDURE _SSR_5_32 ;
BEGIN
	SSRnimpl ('Auftrag kreieren') ;
END _SSR_5_32 ;


PROCEDURE _SSR_5_40 ;
BEGIN
	SSRnimpl ('Ger�t belegen') ;
END _SSR_5_40 ;


PROCEDURE _SSR_5_44 ;
BEGIN
	SSRnimpl ('Ger�t freigeben') ;
END _SSR_5_44 ;


PROCEDURE _SSR_5_48 ;
BEGIN
	SSRnimpl ('Sendung absenden an Ger�t') ;
END _SSR_5_48 ;


END SSR_5.
