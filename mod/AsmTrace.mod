
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE AsmTrace;

(*	12.06.18	*)

FROM ASCII IMPORT
	cr, lf, ht ;

FROM SYSTEM IMPORT
    ADR, CAST, VA_START, %IF ADW %THEN VA_LIST, ADRINT, %ELSE ADDRESS, %END IsThread ;

FROM FormatString IMPORT
	FormatStringEx ;

FROM Struktur IMPORT
	TasTraceExtension ;

IMPORT FileIO, Conversions, FileFunc, Terminal ;

IMPORT debug ;


CONST
	crlf : ARRAY OF CHAR = {CHR(13), CHR(10)} ;

%IF %NOT WEB %THEN

VAR
	initialisiert :	BOOLEAN = FALSE ;

	aktSpalte,
	aktZeile :	CARDINAL ;

	datei,
	hilf :		FileIO.FILE ;

	zwStr :		ARRAY [0..2047] OF CHAR ;
	buffer :	ARRAY [0..16383] OF CHAR ;

	dnam :		ARRAY [0..255] OF CHAR ;
%END


PROCEDURE InitTrace (datnam : ARRAY OF CHAR) ;
BEGIN
%IF %NOT WEB %THEN
	IF NOT debug.an OR NOT debug.Befehle OR ((BacktraceAnzahl < 100) AND (BacktraceAnzahl <> 0)) THEN	(* Dateitrace ist ausgeschaltet *)
		RETURN ;
	END ;
	FileFunc.ConstructFileName (TraceDirectory, datnam, dnam) ;
	FileFunc.ConstructFileName (TasTraceExtension, dnam, dnam) ;
	IF FileIO.CreateFileBuf (datei, dnam, buffer) THEN
		initialisiert := TRUE ;
		aktZeile := 1 ;
	ELSE
		Terminal.WriteF ('\n+++++ Tracedatei %s nicht erzeugbar\n', dnam) ;
	END ;
%END
END InitTrace ;


PROCEDURE RestoreTrace (datnam : ARRAY OF CHAR) ;
BEGIN
%IF %NOT WEB %THEN
	IF NOT debug.an OR NOT debug.Befehle OR ((BacktraceAnzahl < 100) AND (BacktraceAnzahl <> 0)) THEN	(* Dateitrace ist ausgeschaltet *)
		RETURN ;
	END ;
	IF datnam [0] <> 0C THEN
		FileFunc.ConstructFileName (TraceDirectory, datnam, dnam) ;
		FileFunc.ConstructFileName (TasTraceExtension, dnam, dnam) ;
	END ;
	IF FileIO.OpenAppendFileBuf (datei, dnam, buffer) THEN
		initialisiert := TRUE ;
		aktZeile := 1 ;
	ELSE
		Terminal.WriteF ('\n+++++ Tracedatei %s nicht fortschreibbar\n', dnam) ;
	END ;
%END
END RestoreTrace ;


PROCEDURE AnfangLoeschen ;
	VAR	i :		CARDINAL ;
		helpnam :	ARRAY [0..255] OF CHAR ;
		readbuff :	ARRAY [0..16383] OF CHAR ;
BEGIN
%IF %NOT WEB %THEN
	FileIO.CloseFile (datei) ;						(* Tracedatei schließen *)
	FileFunc.ConstructFileName ('.hlp', dnam, helpnam) ;
	FileFunc.DeleteFile (helpnam) ;

	IF NOT FileFunc.RenameFile (dnam, helpnam) THEN				(* nach  .HLP  umbenennen *)
		initialisiert := FALSE ;
		Terminal.WriteF ('\n+++++ Zwischendatei %s nicht erzeugbar\n', helpnam) ;
		RETURN ;
	END ;

	IF NOT FileIO.CreateFileBuf (datei, dnam, buffer) THEN			(* Tracedatei neu kreieren *)
		initialisiert := FALSE ;
		Terminal.WriteF ('\n+++++ Tracedatei %s nicht neu erzeugbar\n', dnam) ;
		RETURN ;
	END ;

	FileFunc.OpenFileEx (hilf, helpnam, FileFunc.ReadOnlyDenyWrite,
				FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF hilf.status <> 0 THEN
		initialisiert := FALSE ;
		Terminal.WriteF ('\n+++++ Zwischendatei %s nicht lesbar\n', helpnam) ;
		RETURN ;
	END ;
	FileFunc.SetFileBuffer (hilf, readbuff) ;
	FOR i := 1 TO BacktraceAnzahl DIV 2 DO					(* erste Hälfte der  .HLP  Datei skippen *)
		IF NOT FileIO.ReadLn (hilf, zwStr) THEN
			BREAK ;
		END ;
	END ;

	aktZeile := 0 ;

	WHILE FileIO.ReadLn (hilf, zwStr) DO					(* zweite Hälfte in die neue Tracedatei kopieren *)
		FileIO.WriteLn (datei, zwStr) ;
		INC (aktZeile) ;
	END ;

	FileIO.CloseFile (hilf) ;
	FileFunc.DeleteFile (helpnam) ;						(*  .HLP  Datei löschen und nun neue Tracedatei fortschreiben *)
%END
END AnfangLoeschen ;


PROCEDURE WriteString (str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
%IF %NOT WEB %THEN
	IF initialisiert THEN

		FileIO.WriteString (datei, str) ;

		FOR i := 1 TO LENGTH(str) DO
			CASE str [i-1] OF
			cr :	aktSpalte := 1 ;
			|
			lf :	aktSpalte := 1 ;
				INC (aktZeile) ;
			|
			ht :	aktSpalte := ((aktSpalte - 1) DIV 8 + 1) * 8 + 1 ;
			ELSE
				INC (aktSpalte) ;
			END ;
		END ;
		IF (aktZeile > BacktraceAnzahl) AND (BacktraceAnzahl <> 0) THEN
			AnfangLoeschen ;
		END ;
	END ;
%END
END WriteString ;


PROCEDURE WriteLn ;
BEGIN
%IF %NOT WEB %THEN
	IF initialisiert THEN
		WriteString (crlf) ;
	END ;
%END
END WriteLn ;


PROCEDURE WriteCard (zahl : CARDINAL) ;
	VAR	str :	ARRAY [0..31] OF CHAR ;
BEGIN
%IF %NOT WEB %THEN
	IF initialisiert THEN
		Conversions.CardToStr (zahl, str) ;
		WriteString (str) ;
	END ;
%END
END WriteCard ;


PROCEDURE SetPos (spalte : CARDINAL) ;
BEGIN
%IF %NOT WEB %THEN
	IF initialisiert THEN
		REPEAT
			WriteString (ht) ;
		UNTIL aktSpalte >= spalte ;
	END ;
%END
END SetPos ;


PROCEDURE WriteF (formatStr : ARRAY OF CHAR	(* %s für String %c für CARDINAL %h für Hex-CARDINAL %i für INTEGER %l für LONGINT \t für TAB *)

		) %IF %NOT DLL %THEN [RightToLeft, LEAVES, VARIABLE] %END ;	(* beliebige Anzahl Parameter dahinter *)
    VAR addr :  %IF ADW %THEN VA_LIST %ELSE ADDRESS %END ;
BEGIN
%IF %NOT WEB %THEN
	IF NOT initialisiert THEN
		RETURN ;
	END ;

	VA_START (addr) ;
	IF NOT FormatStringEx (formatStr, zwStr, addr) THEN
		WriteString ('WriteF-Formatfehler : ') ;
		WriteString (formatStr) ;
	END ;

	WriteString (zwStr);

%END
END WriteF ;


PROCEDURE ExitTrace ;
BEGIN
%IF %NOT WEB %THEN
	IF initialisiert THEN
		initialisiert := FALSE ;
		FileIO.CloseFile (datei) ;
	END ;
%END
END ExitTrace ;


END AsmTrace.
