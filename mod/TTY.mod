IMPLEMENTATION MODULE TTY;


(*	06.04.18	*)


IMPORT Terminal, Conversions, Strings, Environment, FileFunc, TerminalEingabe ;

FROM RS232 IMPORT
	tPort,
	InitV24,
	GetV24Char,
	PutV24Char,
	CloseV24 ;

FROM Trace IMPORT
	TraceF ;

FROM WIN32 IMPORT
	Sleep ;


CONST
	modultest = 	FALSE ;		(* TRUE, wenn Simulation TTY über Tastatur/Trace *)

	cr =		CHR (13) ;
	lf =		CHR (10) ;
	si =		CHR (31) ;
	so =		CHR (27) ;

	codetabSI :	ARRAY OF CHAR =
					{
					'&', 'T',  cr, 'O', ' ', 'H', 'N', 'M',
					 lf, 'L', 'R', 'G', 'I', 'P', 'C', 'V',
					'E', 'Z', 'D', 'B', 'S', 'Y', 'F', 'X',
					'A', 'W', 'J',  so, 'U', 'Q', 'K', si
					} ;

	codetabSO :	ARRAY OF CHAR =
			 		{
					'#', '5',  cr, '9', ' ', '!', ',', '.',
					 lf, ')', '4', ']', '8', '0', ':', '=',
					'3', '+', '_', '*', '_', '6', '[', '/',
					'-', '2', ';',  so, '7', '1', '(', si
					} ;

VAR
	portnr :		tPort ;

	initialisiert :		BOOLEAN = FALSE ;

	imSI :			BOOLEAN = TRUE ;

	inEingabe :		CARDINAL = 0 ;

	austabSI,
	austabSO :		ARRAY CHAR OF CHAR ;



PROCEDURE put (ch : CHAR) ;						(* ch = TTY-5-Bit-Code *)
	VAR	str :	ARRAY [0..7] OF CHAR ;
BEGIN
	IF modultest THEN
		CASE ch OF
		cr :		TraceF ("\t*TTYout 'cr'") ;
		|
		lf :		TraceF ("\t*TTYout 'lf'") ;
		|
		si :		TraceF ("\t*TTYout 'si'") ;
		|
		so :		TraceF ("\t*TTYout 'so'") ;
		ELSE
				TraceF ('\t*TTYout %c', ORD (ch)) ;
		END ;
	ELSE
		PutV24Char (portnr, ch) ;
	END ;
END put ;


PROCEDURE get (VAR ch : CHAR) : BOOLEAN ;				(* ch := TTY-5-Bit-Code *)
	VAR	i :	CARDINAL ;
		str :	ARRAY [0..2] OF CHAR ;
BEGIN
	IF modultest THEN
		TerminalEingabe.ReadString (str) ;
		Terminal.WriteLn ;
		IF str [0] = 0C THEN
			RETURN FALSE ;
		END ;
		IF Conversions.StrToCard (str, i) AND (i < 32) THEN
			ch := CHR (i) ;
		ELSE
			ch := CHR (4) ;	(* Blank *)
		END ;
		TraceF ('\t*TTYin %c', ORD (ch)) ;
		RETURN TRUE ;
	END ;
	RETURN GetV24Char (portnr, ch) ;
END get ;


PROCEDURE Write (ch : CHAR) ;
	VAR	str :	ARRAY [0..1] OF CHAR ;
BEGIN
	IF initialisiert THEN
		CASE ch OF
		'ä', 'Ä' :
			Write ('A') ; ch := 'E' ;
		|
		'ö', 'Ö' :
			Write ('O') ; ch := 'E' ;
		|
		'ü', 'Ü' :
			Write ('U') ; ch := 'E' ;
		|
		'ß' :
			Write ('S') ; ch := 'S' ;
		|
		'a'..'z' :
			ch := CHR (ORD (ch) - 20H) ;
		|
		'_' :	ch := '-' ;
		ELSE
		END ;

		IF modultest THEN
			str [0] := ch ;
			str [1] := 0C ;
			TraceF ('*TTYwrite %s', str) ;
		END ;

		CASE ch OF
			cr, lf, ' ' :							(* unabhängig vom SI/SO - Zustand *)
								put (austabSI [ch]) ;
			|
			'&', 'T',      'O',      'H', 'N', 'M',
			     'L', 'R', 'G', 'I', 'P', 'C', 'V',
			'E', 'Z', 'D', 'B', 'S', 'Y', 'F', 'X',
			'A', 'W', 'J',      'U', 'Q', 'K' :				(* muss im SI - Zustand sein *)
								IF NOT imSI THEN
									put (si) ;
									imSI := TRUE ;
								END ;
								put (austabSI [ch]) ;
			|
			'#', '5',      '9',      '!', ',', '.',
			     ')', '4', ']', '8', '0', ':', '=',
			'3', '+',      '*',      '6', '[', '/',
			'-', '2', ';',      '7', '1', '(' :				(* muss im SO - Zustand sein *)
								IF imSI THEN
									put (so) ;
									imSI := FALSE ;
								END ;
								put (austabSO [ch]) ;
			ELSE
								put (austabSI [' ']) ;
		END ;
	END ;
END Write ;


PROCEDURE upGetPortINI (name : ARRAY OF CHAR ; port : CARDINAL ; VAR str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	zwinidatei,
		prog :		ARRAY [0..267] OF CHAR ;
BEGIN
	Environment.GetProgramName (prog) ;
	zwinidatei := name ;
	Strings.Append ('.INI', zwinidatei) ;
	FileFunc.ConstructFileName (zwinidatei, prog, str) ;	(* INI-Datei im Programm-Verzeichnis *)
	RETURN FileFunc.FileExists (str) ;
END upGetPortINI ;


PROCEDURE GetPortINI (name : ARRAY OF CHAR ; port : CARDINAL ; VAR str : ARRAY OF CHAR) ;
	VAR	zwinidatei :	ARRAY [0..267] OF CHAR ;
BEGIN
	IF upGetPortINI (name, port, str) THEN		(* INI-Datei ohne Portnr-Erweiterung im Namen existiert *)

	ELSE						(* INI-Datei mit Portnr-Erweiterung im Namen versuchen *)
		zwinidatei := name ;
		Conversions.CardToStr (port, str) ;
		Strings.Append (str, zwinidatei) ;
		upGetPortINI (zwinidatei, port, str) ;
	END ;
END GetPortINI ;


PROCEDURE InitTTY (port : CARDINAL) : BOOLEAN ;
	VAR	i :	CARDINAL ;
		str :	ARRAY [0..267] OF CHAR ;
BEGIN
	GetPortINI ('TTY', port, str) ;		(* TTYxx.INI im Programm-Verzeichnis oder TTY.INI *)

	portnr := VAL (tPort, port) ;

	IF NOT modultest THEN
		IF NOT InitV24 (portnr, str) THEN
			RETURN FALSE ;
		END ;
	END ;

	initialisiert := TRUE ;
	Terminal.WriteExt := Write ;
	Terminal.WriteExtAngemeldet := TRUE ;
	imSI := TRUE ;
	inEingabe := 0 ;
	EingabePuffer [0] := 0C ;
	FOR i := 0 TO 255 DO
		austabSI [CHR(i)] := CHR (255) ;
		austabSO [CHR(i)] := CHR (255) ;
	END ;
	FOR i := 0 TO 31 DO
		austabSI [codetabSI [i]] := CHR(i) ;
		austabSO [codetabSO [i]] := CHR(i) ;
	END ;
	RETURN TRUE ;
END InitTTY ;



PROCEDURE merkeInput (ch : CHAR) ;
	VAR	str :	ARRAY [0..1] OF CHAR ;
BEGIN
	EingabePuffer [inEingabe] := ch ;
	IF inEingabe < HIGH (EingabePuffer) THEN
		INC (inEingabe) ;
		EingabePuffer [inEingabe] := 0C ;
		(*
		IF modultest THEN
			str [0] := ch ;
			str [1] := 0C ;
			TraceF ('\t*TTYread %s', str) ;
		END ;
		*)
	END ;
END merkeInput ;


PROCEDURE GetTTY (VAR ch : CHAR) : BOOLEAN ;
BEGIN
	IF initialisiert THEN
		IF get (ch) THEN
			CASE ch OF
			si :
				imSI := TRUE ;
			|
			so :
				imSI := FALSE ;
			|
			cr :
				(* ignorieren *)
			|
			CHR (32) .. CHR(255) :
				(* unzul. *)
			ELSE
				IF imSI THEN
					ch := codetabSI [ORD(ch)] ;
				ELSE
					ch := codetabSO [ORD(ch)] ;
				END ;
				merkeInput (ch) ;
			END ;
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END GetTTY ;


PROCEDURE CheckTTY ;
	VAR	ch :	CHAR ;
BEGIN
	IF initialisiert THEN
		WHILE GetTTY (ch) DO			(* alle Eingaben abholen *)
		END ;
		Terminal.EingabeVorhanden := inEingabe > 0 ;
	END ;
END CheckTTY ;


PROCEDURE EingabeLesen (VAR Zeile : ARRAY OF CHAR) ;
	VAR	ch :		CHAR ;
		flusyda :	BOOLEAN ;
BEGIN
	flusyda := FALSE ;
	IF initialisiert THEN
		LOOP
			IF GetTTY (ch) THEN
				CASE ch OF
				'#' :
					flusyda := TRUE ;
				|
				'.' :
					IF flusyda THEN
						EXIT ;
					END ;
				|
				cr :
							(* ignorieren *)
				|
				lf :
					EXIT ;		(* Zeile zu Ende *)
				ELSE
					flusyda := FALSE ;
				END ;
			ELSE
				Sleep (200) ;
			END ;
		END ;
		Zeile := EingabePuffer ;

		TraceF ('*TTY-Eingabe %s', Zeile) ;

		inEingabe := 0 ;
		EingabePuffer [0] := 0C ;
	ELSE
		Zeile := '' ;
	END ;
END EingabeLesen ;


PROCEDURE ClearInput ;
BEGIN
	inEingabe := 0 ;
	EingabePuffer [0] := 0C ;
END ClearInput ;



(**************************************** ab hier Versionen für generelle V24-Terminals : ***************************************************)


PROCEDURE COMwrite (ch : CHAR) ;
	VAR	str :	ARRAY [0..1] OF CHAR ;
BEGIN
	IF initialisiert THEN
		IF modultest THEN
			str [0] := ch ;
			str [1] := 0C ;
			TraceF ('*COMout : %s', str) ;
		ELSE
			PutV24Char (portnr, ch) ;
		END ;
	END ;
END COMwrite ;


PROCEDURE InitCOM (port : CARDINAL) : BOOLEAN ;
	VAR	i :	CARDINAL ;
		str,
		str2 :	ARRAY [0..15] OF CHAR ;
BEGIN
	portnr := VAL (tPort, port) ;

	GetPortINI ('COM', port, str) ;		(* COMxx.INI im Programm-Verzeichnis oder COM.INI *)

	IF NOT modultest THEN
		IF NOT InitV24 (portnr, str) THEN
			RETURN FALSE ;
		END ;
	END ;

	initialisiert := TRUE ;
	Terminal.WriteExt := COMwrite ;
	Terminal.WriteExtAngemeldet := TRUE ;
	inEingabe := 0 ;
	EingabePuffer [0] := 0C ;
	RETURN TRUE ;
END InitCOM ;


PROCEDURE GetCOM (VAR ch : CHAR) : BOOLEAN ;
BEGIN
	IF initialisiert THEN
		IF modultest THEN
			IF Terminal.CharAvail () THEN
				Terminal.Read (ch) ;
				Terminal.Write (ch) ;
				merkeInput (ch) ;
				RETURN TRUE ;
			END ;
		ELSE
			IF GetV24Char (portnr, ch) THEN
				merkeInput (ch) ;
				RETURN TRUE ;
			END ;
		END ;
	END ;
	RETURN FALSE ;
END GetCOM ;


PROCEDURE CheckCOM ;
	VAR	ch :	CHAR ;
BEGIN
	IF initialisiert THEN
		WHILE GetCOM (ch) DO			(* alle Eingaben abholen *)
		END ;
		Terminal.EingabeVorhanden := inEingabe > 0 ;
	END ;
END CheckCOM ;


PROCEDURE COMeingabeLesen (VAR Zeile : ARRAY OF CHAR) ;
	VAR	ch :		CHAR ;
		flusyda :	BOOLEAN ;
BEGIN
	flusyda := FALSE ;
	IF initialisiert THEN
		LOOP
			IF GetCOM (ch) THEN
				CASE ch OF
				'#' :
					flusyda := TRUE ;
				|
				'.' :
					IF flusyda THEN
						EXIT ;
					END ;
				|
				Terminal.Enter :
					EXIT ;		(* Zeile zu Ende *)
				ELSE
					flusyda := FALSE ;
				END ;
			ELSE
				Sleep (100) ;
			END ;
		END ;
		Zeile := EingabePuffer ;

		TraceF ('*COM-Eingabe : %s', Zeile) ;

		inEingabe := 0 ;
		EingabePuffer [0] := 0C ;
	ELSE
		Zeile := '' ;
	END ;
END COMeingabeLesen ;



BEGIN
FINALLY
	IF initialisiert THEN
		initialisiert := FALSE ;
		IF NOT modultest THEN
			CloseV24 (portnr) ;
		END ;
		Terminal.WriteExtAngemeldet := FALSE ;
	END ;
END TTY.
