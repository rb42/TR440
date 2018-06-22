
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE OktadenTransport;

(*	12.12.15	*)

FROM Struktur IMPORT * ;

VAR
	qreladrVorher :		CARDINAL = 1000000H ;
	qOktadenHalbwort :	CARDINAL ;

	zreladrVorher :		CARDINAL = 1000000H ;
	zOktadenHalbwort :	CARDINAL ;
	gespeichertOktaden :	BOOLEAN = FALSE ;


PROCEDURE InitOktaden ;
BEGIN
	qreladrVorher := 1FFFFFFH ;
	zreladrVorher := 1FFFFFFH ;
	gespeichertOktaden := FALSE ;
END InitOktaden ;


PROCEDURE GetOktade (oadr : CARDINAL) : CARDINAL8 ;
	VAR	reladr :	SpeicherAdresse ;
BEGIN
	reladr := oadr DIV 3 ;
	IF reladr <> qreladrVorher THEN
		qreladrVorher := reladr ;
		qOktadenHalbwort := GetHalbwort (reladr) ;
	END ;
	CASE oadr MOD 3 OF
	0 :		RETURN qOktadenHalbwort SHR 16 ;
	|
	1 :		RETURN qOktadenHalbwort SHR 8 ;
	|
	2 :		qreladrVorher := 1FFFFFFH ;
			RETURN qOktadenHalbwort ;
	ELSE
	END ;
	RETURN 0 ;
END GetOktade ;


PROCEDURE AbschlussOktaden ;
BEGIN
	IF gespeichertOktaden THEN
		NeueTypenkennung := 4 ;
		PutHalbwort (zreladrVorher, zOktadenHalbwort) ;
		zreladrVorher := 1FFFFFFH ;
		gespeichertOktaden := FALSE ;
	END ;
END AbschlussOktaden ;


PROCEDURE PutOktade (oadr : SpeicherAdresse ; oktade : CARDINAL8) ;
	VAR	reladr :	SpeicherAdresse ;
BEGIN
	reladr := oadr DIV 3 ;
	IF reladr <> zreladrVorher THEN
		zreladrVorher := reladr ;
		zOktadenHalbwort := GetHalbwort (reladr) ;
	END ;
	CASE oadr MOD 3 OF
	0 :		zOktadenHalbwort := zOktadenHalbwort BAND 000FFFFH BOR ORD(oktade) SHL 16 ;
			gespeichertOktaden := TRUE ;
	|
	1 :		zOktadenHalbwort := zOktadenHalbwort BAND 0FF00FFH BOR ORD(oktade) SHL 8 ;
			gespeichertOktaden := TRUE ;
	|
	2 :		zOktadenHalbwort := zOktadenHalbwort BAND 0FFFF00H BOR ORD(oktade) ;
			AbschlussOktaden ;
	ELSE
	END ;
END PutOktade ;


END OktadenTransport.
