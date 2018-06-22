
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE TASformat;

(*	11.05.16	*)

IMPORT
	FileIO,
	FileFunc ;

FROM Terminal IMPORT
	WriteF ;

FROM Struktur IMPORT
	NullBefehl ;

FROM ASCII IMPORT
	ht ;


CONST
	MaxTeilZeile =	4 ;


TYPE
	teilzeile =	ARRAY [0..255] OF CHAR ;


VAR
	q,
	z :		FileIO.FILE ;

	input,
	output :	ARRAY [0..2047] OF CHAR ;

	teilnr :	CARDINAL ;
	NameDa :	BOOLEAN ;


PROCEDURE ZeileBearbeiten ;
	VAR
		i,
		ioutput,
		j,
		k :		CARDINAL ;
		teile :		ARRAY [0..MaxTeilZeile] OF teilzeile ;
		jteil :		ARRAY [0..MaxTeilZeile] OF CARDINAL ;
		ch :		CHAR ;
		KommaFehlt,
		GRBU :		BOOLEAN ;


	PROCEDURE get ;
	BEGIN
		ch := input [i] ;
		IF GRBU THEN
			IF (ch >= 'a') AND (ch <= 'z') THEN
				ch := CHR (ORD (ch) - 32) ;	(* -> Groﬂbuchstabe *)
			END ;
		END ;
		IF ch <> 0C THEN
			INC (i) ;
		END ;
	END get ;


	PROCEDURE put (ch : CHAR) ;
	BEGIN
		teile [teilnr] [jteil [teilnr]] := ch ;
		INC (jteil [teilnr]) ;
		teile [teilnr] [jteil [teilnr]] := 0C ;
	END put ;


	PROCEDURE NextTeil ;
	BEGIN
		IF jteil [teilnr] > 0 THEN
			IF (teilnr < MaxTeilZeile) THEN
				put (0C) ;
				INC (teilnr) ;
				IF ch > ' ' THEN
					put (ch) ;
				END ;
			ELSE
				put (ch) ;
			END ;
		ELSIF ch > ' ' THEN
			put (ch) ;
		END ;
	END NextTeil ;

	PROCEDURE Kommentar ;
	BEGIN
		IF teilnr < (MaxTeilZeile - ORD(NOT NameDa)) THEN
			put (0C) ;
			teilnr := MaxTeilZeile - ORD(NOT NameDa) ;
		END ;
	END Kommentar ;

	PROCEDURE String (bis : CHAR) ;
	BEGIN
		GRBU := FALSE ;
		put (bis) ;
		LOOP
			get ;
			IF ch = 0C THEN
				EXIT ;
			END ;
			put (ch) ;
			IF (ch = '+') AND (input [i] = '(') THEN
				get ;
				put ('(') ;
				GRBU := TRUE ;
				Klammer ;
				GRBU := FALSE ;
			ELSIF ch = bis THEN
				IF bis = '"' THEN
					EXIT ;
				END ;
				get ;
				put (ch) ;
				IF ch = bis THEN
					EXIT ;
				END ;
			END ;
		END ;
		GRBU := TRUE ;
	END String ;

	PROCEDURE Hexzahl ;
	BEGIN
		put ("'") ;
		REPEAT
			get ;
			put (ch) ;
		UNTIL (ch = "'") OR (ch = 0C) ;
	END Hexzahl ;

	PROCEDURE Klammer ;
		VAR	klanz :	CARDINAL ;
	BEGIN
		klanz := 1 ;
		LOOP
			get ;
			CASE ch OF
			0C :		EXIT ;
			|
			')' :		DEC (klanz) ;
					put (ch) ;
					IF klanz = 0 THEN
						EXIT ;
					END ;
			|
			'(' :		INC (klanz) ;
					put (ch) ;
			|
			"'" :		IF input [i] = "'" THEN
						put (ch) ;
						get ;
						String ("'") ;
					ELSE
						Hexzahl ;
					END ;
			|
			'"' :		String ('"') ;
			ELSE
					put (ch) ;
			END ;
		END ;
	END Klammer ;

	PROCEDURE putoutput (ch : CHAR) ;
	BEGIN
		output [ioutput] := ch ;
		INC (ioutput) ;
	END putoutput ;


BEGIN
	i := 0 ;
	NameDa := FALSE ;
	KommaFehlt := FALSE ;
	GRBU := TRUE ;
	teilnr := 0 ;
	FOR j := 0 TO MaxTeilZeile DO
		jteil [j] := 0 ;
		teile [j, 0] := 0C ;
	END ;

	LOOP
		get ;

		CASE ch OF
(*
		'A'..'Z',
		'*', '_', '&' :		put (ch) ;
		|
*)
		0C :			KommaFehlt := TRUE ;		(* fehlender Statement-Abschluss *)
					EXIT ;
		|
		1C .. ' ' :		NextTeil ;
		|
		"'" :			IF input [i] = "'" THEN
						put (ch) ;
						get ;
						String ("'") ;
					ELSE
						Hexzahl ;
					END ;
		|
		':' :			Kommentar ;
					EXIT ;
		|
		'-' :			IF input [i] = '-' THEN		(* Kommentar *)
						Kommentar ;
						EXIT ;
					END ;
					put (ch) ;
		|
		'"' :			String ('"') ;
		|
		'(' :			NextTeil ;
					Klammer ;
		|
		'=' :			put (ch) ;
					IF teilnr = 0 THEN
						ch := ' ' ;
						NextTeil ;
						NameDa := TRUE ;
					END ;
		|
		',' :			EXIT ;
		|
		'+' :			IF input [i] = '(' THEN
						put (ch) ;
						get ;
						put (ch) ;
						Klammer ;
					ELSE
						put (ch) ;
					END ;
		ELSE
					put (ch) ;
		END ;
	END ;

	WHILE ch <> 0C DO		(* Restzeile nur noch kopieren *)
		put (ch) ;
		get ;
	END ;
	put (0C) ;

	ioutput := 0 ;

	IF (teilnr > 0) OR (teile [0, 0] <> 0C ) THEN	(* keine Leerzeile *)
		IF NOT NameDa THEN
			putoutput (ht) ;
		END ;

		FOR j := 0 TO teilnr DO
			FOR k := 0 TO jteil [j] DO
				ch := teile [j, k] ;
				IF ch <> 0C THEN
					putoutput (ch) ;
				END ;
			END ;
			IF NameDa AND (j = 0) THEN
				putoutput (ht) ;
			ELSIF j < teilnr THEN
				IF jteil [j] < 9 THEN
					putoutput (ht) ;
				ELSE
					putoutput (' ') ;
				END ;
			END ;
		END ;

		IF KommaFehlt AND (ioutput > 0) AND (output [ioutput-1] <> ',') THEN
			putoutput (',') ;
		END ;
	END ;

	putoutput (0C) ;

END ZeileBearbeiten ;


PROCEDURE Bearbeiten (quelle, ziel : ARRAY OF CHAR) ;
BEGIN
	IF FileIO.OpenReadFile (q, quelle) THEN
		IF FileIO.CreateFile (z, ziel) THEN
			WHILE	FileIO.ReadLn (q, input) DO
				ZeileBearbeiten ;
				FileIO.WriteLn (z, output) ;
			END ;
			FileIO.CloseFile (q) ;
			FileIO.CloseFile (z) ;
		ELSE
			WriteF ('\n+++++ %s nicht erzeugbar', ziel) ;
		END ;
	ELSE
		WriteF ('\n+++++ %s nicht lesbar', quelle) ;
	END ;
END Bearbeiten ;



PROCEDURE TASformatierer (quelle : ARRAY OF CHAR) ;
	VAR	alt :	ARRAY [0..267] OF CHAR ;
BEGIN
	IF FileFunc.FileExists (quelle) THEN
		FileFunc.ConstructFileName ('.bak', quelle, alt) ;
		FileFunc.DeleteFile (alt) ;
		IF FileFunc.RenameFile (quelle, alt) THEN
			Bearbeiten (alt, quelle) ;
		ELSE
			WriteF ('\n+++++ umbenennen misslungen -> %s', alt) ;
		END ;
	ELSE
		WriteF ('\n+++++ Datei %s nicht gefunden', quelle) ;
	END ;
END TASformatierer ;



END TASformat.
