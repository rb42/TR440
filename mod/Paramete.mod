
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Parameter ;


(*	20.06.18	*)


IMPORT Environment ;
IMPORT FileFunc ;
IMPORT FileIO ;


FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;


VAR
	cmd :		ARRAY [0..255] OF CHAR ;
        cmdIndex :	CARDINAL ;

	datParam :	BOOLEAN = FALSE ;
	ersterParam :	BOOLEAN = TRUE ;

	parmdatei :	FileFunc.File ;

	readbuffer :	POINTER TO ARRAY [0..4095] OF CHAR ;

	initialisiert :	BOOLEAN = FALSE ;



PROCEDURE InitParameter () : BOOLEAN ;
BEGIN
	IF NOT initialisiert THEN
		Environment.GetCommandLine (cmd) ;
	END ;
	cmdIndex := 0 ;
	WHILE cmd [cmdIndex] <= ' ' DO
		IF cmd [cmdIndex] = 0C THEN
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END InitParameter ;


PROCEDURE SetParameter (cmdparm : ARRAY OF CHAR) ;
BEGIN
	initialisiert := TRUE ;
	cmd := cmdparm ;
	cmdIndex := 0 ;
	datParam := FALSE ;
	ersterParam := TRUE ;
END SetParameter ;


PROCEDURE NextParameter (VAR str : ARRAY OF CHAR) ;
	VAR	ZielIndex :	CARDINAL ;
		imString :	BOOLEAN ;
		ch :		CHAR ;
BEGIN
	IF datParam THEN	(* Parameter zeilenweise aus Datei lesen *)
		LOOP
			IF NOT FileIO.ReadLn (parmdatei, str) THEN
				datParam := FALSE ;
				FileFunc.CloseFile (parmdatei) ;
				DISPOSE (readbuffer) ;
				str := '' ;
				RETURN ;
			END ;
			IF str [0] > ' ' THEN
				IF (str [0] <> '-') AND (str [0] <> '/') THEN
					str [1..HIGH(str)] := str [0..HIGH(str)-1] ;
					str [0] := '-' ;
				END ;
				RETURN ;
			END ;
		END ;
	END ;

	WHILE cmd [cmdIndex] <= ' ' DO		(* führende Blanks skippen *)
		IF cmd [cmdIndex] = 0C THEN
			str := '' ;
			RETURN ;
		END ;
		INC (cmdIndex) ;
	END ;

	imString := FALSE ;
	ZielIndex := 0 ;
	WHILE (cmd [cmdIndex] > ' ') OR imString DO
		ch := cmd [cmdIndex] ;
		IF ch = '"' THEN		(* in " ... " - Folgen sind
							Blanks erlaubt *)
			imString := NOT imString ;
		ELSE
			IF ZielIndex < HIGH (str) THEN
				str [ZielIndex] := ch ;
				INC (ZielIndex) ;
			END ;
		END ;
		INC (cmdIndex) ;
	END ;
	str [ZielIndex] := 0C ;
	IF ersterParam THEN
		ersterParam := FALSE ;
		IF str [0] = '@' THEN		(* Parameter aus Datei lesen *)
			FileFunc.OpenFile (parmdatei, str [1..ZielIndex], FileFunc.ReadOnlyDenyWrite) ;
			IF parmdatei.status = 0 THEN
				NEW (readbuffer) ;
				FileFunc.SetFileBuffer (parmdatei, readbuffer ^ ) ;
				datParam := TRUE ;
			END ;
			NextParameter (str) ;
		END ;
	END ;
END NextParameter ;


END Parameter.
