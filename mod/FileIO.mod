
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE FileIO;


(*	20.06.18	*)


IMPORT
	FileFunc ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Strings IMPORT
	Append ;

FROM SYSTEM IMPORT
%IF ADW %THEN
	ADR, VA_LIST,
%END
	FUNC, ADDRESS, BYTE, VA_START ;

FROM FormatString IMPORT
	FormatStringEx ;


VAR
	ReadBuffer,
	WriteBuffer :	POINTER TO ARRAY [0..32767] OF CHAR ;

	zwStr	: ARRAY [0..2047] OF CHAR ;


PROCEDURE OpenReadFile (VAR datei : FILE ; name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	FileFunc.OpenFileEx (datei, name, FileFunc.ReadOnlyDenyWrite,
				FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF datei.status = 0 THEN
		IF ReadBuffer = NIL THEN
			NEW (ReadBuffer) ;
		END ;
		FileFunc.SetFileBuffer (datei, ReadBuffer ^ ) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END OpenReadFile ;


PROCEDURE ReadBlock (VAR datei : FILE ; VAR str : ARRAY OF BYTE ; len : CARDINAL) : BOOLEAN ;
BEGIN
	FileFunc.ReadBlock (datei, %IF ADW %THEN ADR(str) %ELSE str %END, len) ;
	RETURN (datei.status = 0) AND (datei.count = len) ;
END ReadBlock ;


PROCEDURE ReadByte (VAR datei : FILE ; VAR ch : CHAR) : BOOLEAN ;
BEGIN
	RETURN ReadBlock (datei, ch, 1) ;
END ReadByte ;


PROCEDURE ReadBytes (VAR datei : FILE ; VAR str : ARRAY OF BYTE) : BOOLEAN ;
BEGIN
	RETURN ReadBlock (datei, str, HIGH(str)+1) ;
END ReadBytes ;


PROCEDURE ReadChar (VAR datei : FILE ; VAR ch : CHAR) : BOOLEAN ;
BEGIN
	ch := FileFunc.ReadChar (datei) ;
	RETURN (datei.status = 0) AND (datei.count = 1) ;
END ReadChar ;


PROCEDURE ReadLn (VAR datei : FILE ; VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
	VAR	ch :	CHAR ;
		i :	CARDINAL ;
BEGIN
	i := 0 ;
	LOOP
		ch := FileFunc.ReadChar (datei) ;
		IF ch = FileFunc.EOL THEN
			EXIT ;
		END ;
		IF (datei.status <> 0) OR (ch = 0C) THEN
			datei.eof := TRUE ;
			EXIT ;
		END ;
		IF i < HIGH (zeile) THEN
			zeile [i] := ch ;
			INC (i) ;
		END ;
	END ;
	zeile [i] := 0C ;
	RETURN NOT datei.eof OR (i > 0) ;
END ReadLn ;



PROCEDURE Eof (VAR datei : FILE) : BOOLEAN ;
BEGIN
	RETURN (datei.status <> 0) OR datei.eof ;
END Eof ;


PROCEDURE DateiKomplettLesen (name : ARRAY OF CHAR ; VAR DateiLaenge : CARDINAL) : ADDRESS ;
	VAR	datei :		FILE ;
		lng :		CARDINAL ;
		buf :		POINTER TO ARRAY [0..1000000] OF CHAR ;
BEGIN
	FileFunc.OpenFileEx (datei, name, FileFunc.ReadOnlyDenyNone,
				FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF datei.status <> 0 THEN
		RETURN NIL ;
	END ;
	lng := FileFunc.FileLength (datei) ;
	ALLOCATE (buf, lng+1) ;
	DateiLaenge := lng ;
	IF buf = NIL THEN
		RETURN NIL ;
	END ;
	IF NOT ReadBlock (datei, buf ^ , lng) THEN
		DEALLOCATE (buf, lng) ;
		FileFunc.CloseFile (datei) ;
		RETURN NIL ;
	END ;
	buf ^ [lng] := 0C ;
	FileFunc.CloseFile (datei) ;
	RETURN buf ;
END DateiKomplettLesen ;


PROCEDURE Rewind (VAR datei : FILE) : BOOLEAN ;
BEGIN
	FileFunc.SetFilePos (datei, 0) ;
	RETURN datei.status = 0 ;
END Rewind ;


PROCEDURE CreateFileBuf (VAR datei : FILE ; name : ARRAY OF CHAR ; VAR Buffer : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	FileFunc.CreateFileEx (datei, name, FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF datei.status = 0 THEN
		FileFunc.SetFileBuffer (datei, Buffer) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END CreateFileBuf ;


PROCEDURE OpenAppendFileBuf (VAR datei : FILE ; name : ARRAY OF CHAR ; VAR Buffer : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	FileFunc.OpenFileEx (datei, name, FileFunc.ReadWriteDenyAll,
				FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	IF datei.status = 0 THEN
		FileFunc.SetFilePos (datei, FileFunc.FileLength (datei)) ;
		FileFunc.SetFileBuffer (datei, Buffer) ;
		RETURN TRUE ;
	ELSIF FileFunc.TranslateFileError (datei) = FileFunc.FileErrFileNotFound THEN
		FileFunc.CreateFileEx (datei, name, FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
		IF datei.status = 0 THEN
			FileFunc.SetFileBuffer (datei, Buffer) ;
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END OpenAppendFileBuf ;


PROCEDURE CreateFile (VAR datei : FILE ; name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF WriteBuffer = NIL THEN
		NEW (WriteBuffer) ;
	END ;
	RETURN CreateFileBuf (datei, name, WriteBuffer ^ ) ;
END CreateFile ;


PROCEDURE WriteBlock (VAR datei : FILE ; VAR block : ARRAY OF BYTE ; len : CARDINAL) : BOOLEAN ;
BEGIN
	FileFunc.WriteBlock (datei, %IF ADW %THEN ADR (block) %ELSE block[0..len-1] %END, len) ;
	RETURN datei.status = 0 ;
END WriteBlock ;


PROCEDURE WriteByte (VAR datei : FILE ; VAR ch : CHAR) : BOOLEAN ;
BEGIN
	FileFunc.WriteChar (datei, ch) ;
	RETURN datei.status = 0 ;
END WriteByte ;


PROCEDURE WriteString (VAR datei : FILE ; string : ARRAY OF CHAR) : BOOLEAN ;
	VAR	lng :	CARDINAL ;
BEGIN
	lng := LENGTH(string) ;
	IF lng = 0 THEN
		RETURN TRUE;
	END ;
	RETURN WriteBlock (datei, string[0..lng-1], lng) ;
END WriteString ;


PROCEDURE WriteLn (VAR datei : FILE ; zeile : ARRAY OF CHAR) : BOOLEAN ;
	VAR	lng :	CARDINAL ;
BEGIN
	lng := LENGTH(zeile) ;
	IF lng <> 0 THEN
		IF NOT WriteBlock (datei, zeile [0..lng-1], lng) THEN
			RETURN FALSE ;
		END ;
	END ;
	FileFunc.WriteChar (datei, FileFunc.EOL) ;
	RETURN datei.status = 0 ;
END WriteLn ;


PROCEDURE WriteBytes (VAR datei : FILE ; VAR inhalt : ARRAY OF BYTE) : BOOLEAN ;
BEGIN
	RETURN WriteBlock (datei,inhalt, HIGH(inhalt) + 1) ;
END WriteBytes ;



PROCEDURE CloseFile (VAR datei : FILE) ;
BEGIN
	FileFunc.RemoveFileBuffer (datei) ;
	FileFunc.CloseFile (datei) ;
END CloseFile ;


PROCEDURE WriteF (VAR datei : FILE ; formatStr : ARRAY OF CHAR	(* %s für String %c für CARDINAL %h für Hex-CARDINAL %i für INTEGER %l für LONGINT \t für TAB *)

		) %IF %NOT DLL %THEN [RightToLeft, LEAVES, VARIABLE] %END ;	(* beliebige Anzahl Parameter dahinter *)
	VAR	addr :	%IF ADW %THEN VA_LIST %ELSE ADDRESS %END ;
BEGIN

	VA_START (addr) ;
	IF NOT FormatStringEx (formatStr, zwStr, addr) THEN
		zwStr := 'WriteF-Formatfehler : ' ;
		Append (formatStr, zwStr) ;
	END ;

	FUNC WriteString (datei, zwStr);
END WriteF ;



BEGIN
	ReadBuffer := NIL ;
	WriteBuffer := NIL ;
END FileIO.
