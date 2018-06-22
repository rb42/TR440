
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE ZC1;

(*	11.05.16	*)

FROM ASCII IMPORT
	cr, lf, ff, eot, em, sub, si, so ;

CONST
	cZC1toANSI : ARRAY CHAR OF CHAR =

			{
			0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,
			lf,	lf,	lf,	lf,	lf,	lf,	cr,	ff,	lf,	lf,	lf,	lf,	lf,	lf,	lf,	lf,
			sub,	em,	0C,	si,	so,	eot,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,
			0C,	0C,	0C,	0C,	0C,	'#',	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,
			0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,
			0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,	0C,
			'"',	"'",	'´',	'`',	0C,	'^',	'°',	'~',	0C,	0C,	0C,	0C,	'´',	'`',	'_',	0C,
			'%',	'§',	'¤',	'$',	'¢',	0C,	'@',	'&',	'*',	0C,	0C,	0C,	'¤',	0C,	0C,	0C,
			0C,	0C,	'¬',	0C,	0C,	0C,	'¦',	0C,	0C,	0C,	0C,	'|',	0C,	0C,	0C,	0C,
			'+',	'-',	0C,	'/',	0C,	0C,	0C,	'=',	0C,	0C,	0C,	'<',	'>',	'«',	'»',	0C,
			'(',	')',	'[',	']',	'{',	'}',	'<',	'>',	0C,	'.',	',',	':',	';',	'!',	'?',	' ',
			'0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',	'8',	'9',	0C,	0C,	0C,	0C,	0C,	0C,
			'A',	'B',	'C',	'D',	'E',	'F',	'G',	'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',	'P',
			'Q',	'R',	'S',	'T',	'U',	'V',	'W',	'X',	'Y',	'Z',	'Ä',	'Ö',	'Ü',	0C,	0C,	0C,
			'a',	'b',	'c',	'd',	'e',	'f',	'g',	'h',	'i',	'j',	'k',	'l',	'm',	'n',	'o',	'p',
			'q',	'r',	's',	't',	'u',	'v',	'w',	'x',	'y',	'z',	'ä',	'ö',	'ü',	'ß',	0C,	0C
			} ;


VAR
	initialisiert :	BOOLEAN = FALSE ;


PROCEDURE Init ;
	VAR	ch,
		ch2 :	CHAR ;
BEGIN
	FOR ch := 0C TO CHR(255) DO
		ZC1toANSI [ch] := cZC1toANSI [ch] ;
		ANSItoZC1 [ch] := 0C ;
	END ;
	ANSItoZC1 [lf] := CHR(21) ;
	FOR ch := 1C TO CHR(255) DO
		ch2 := cZC1toANSI [ch] ;
		IF (ch2 <> 0C) AND (ANSItoZC1 [ch2] = 0C) THEN
			ANSItoZC1 [ch2] := ch ;
		END ;
	END ;
	FOR ch := 1C TO CHR(255) DO
		IF ZC1toANSI [ch] = 0C THEN
			ZC1toANSI [ch] := 1C ;
		END ;
	END ;
	FOR ch := 1C TO CHR(255) DO
		IF ANSItoZC1 [ch] = 0C THEN
			ANSItoZC1 [ch] := 2C ;
		END ;
	END ;
	ANSItoZC1 ['\'] := ANSItoZC1 ['/'] ;
	ANSItoZC1 ['÷'] := ANSItoZC1 ['='] ;
	ANSItoZC1 ['¸'] := ANSItoZC1 [','] ;
	ANSItoZC1 ['“'] := ANSItoZC1 ['"'] ;
	ANSItoZC1 ['{'] := ANSItoZC1 ['('] ;
	ANSItoZC1 ['}'] := ANSItoZC1 [')'] ;

	ZC1toANSI [CHR(80H)] := ' ' ;
END Init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
END ZC1.
