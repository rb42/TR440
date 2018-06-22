
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE OFNup;

(*	04.05.16	*)


FROM COMMDLG IMPORT
	LPOFNHOOKPROC,
	GetOpenFileName,
	GetSaveFileName,
	OPENFILENAME,
	CommDlgExtendedError,
	OFN_FILEMUSTEXIST,
	OFN_OVERWRITEPROMPT ;

IMPORT Terminal, Strings ;

FROM SYSTEM IMPORT
	ADR, CAST ;

FROM WIN32 IMPORT
	HWND ;

FROM WINX IMPORT
	Instance ;


VAR
	name :		ARRAY [0..267] OF CHAR = '' ;
	of :		OPENFILENAME ;
	err :		CARDINAL ;


PROCEDURE OFNvorbes (VAR pname : ARRAY OF CHAR) ;
BEGIN
	of.lStructSize		:= SIZE (of) ;
	of.hwndOwner		:= CAST (HWND, Terminal.WND) ;
        of.hInstance		:= Instance ;
	of.lpstrCustomFilter	:= NIL ;
	of.nMaxCustFilter	:= 0 ;
	of.nFilterIndex		:= 0 ;
	of.lpstrFile		:= ADR(pname) ;
	of.nMaxFile		:= SIZE(pname) ;
	of.lpstrFileTitle	:= NIL ;
	of.nMaxFileTitle	:= 0 ;
	of.lpstrInitialDir	:= ADR (pname) ;
	of.Flags		:= 0 ;
	of.nFileOffset		:= 0 ;
	of.nFileExtension	:= 0 ;
	of.lpstrDefExt		:= NIL ;
	of.lCustData		:= 0 ;
	of.lpfnHook		:= CAST (LPOFNHOOKPROC, 0) ;
	of.lpTemplateName	:= NIL ; (* ADR (pname) ; *)
END OFNvorbes ;


PROCEDURE OFNsuche (initdir : ARRAY OF CHAR) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'alle Dateiarten', 0C, '*.*', 0C, 0C} ;
BEGIN
	IF initdir [0] > ' ' THEN
		name := initdir ;
	END ;
	OFNvorbes (name) ;
	of.lpstrTitle		:= ADR ('Datei auswählen') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF GetOpenFileName (of) THEN
		AktString := name ;
		RETURN TRUE ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNsuche ;


PROCEDURE OFNtas (vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'TAS-Datei', 0C, '*.TAS', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR ('TR440-Assembler-Datei') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNtas ;


PROCEDURE OFNlst (vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'TAS-Protokoll', 0C, '*.TLST', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR ('TR440-Assembler-Protokoll') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNlst ;


PROCEDURE OFNexe (titel, vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'Programm', 0C, '*.exe', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR (titel) ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNexe ;


PROCEDURE OFNtrac (vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'Trace-Datei', 0C, '*.TRAC', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR ('TR440-Trace-Datei') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNtrac ;


PROCEDURE OFNkdo (vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'KDO-Datei', 0C, '*.KDO', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR ('TR440-Kommando-Datei') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNkdo ;


PROCEDURE OFNtr440 (vbname : ARRAY OF CHAR ; speichern : BOOLEAN) : BOOLEAN ;
	CONST	txt :	ARRAY OF CHAR =	{'TR440-Datei', 0C, '*.1*;*.2*;*.3*;*.4*;*.5*;*.6*;*.7*;*.8*;*.9*', 0C, 0C} ;
BEGIN
	OFNvorbes (vbname) ;
	of.lpstrTitle		:= ADR ('TR440-Binärdatei') ;
	of.lpstrFilter		:= ADR (txt) ;
	IF speichern THEN
		(*
		of.Flags := OFN_OVERWRITEPROMPT ;
		*)
		IF GetSaveFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		END ;
	ELSE
		of.Flags := OFN_FILEMUSTEXIST ;
		IF GetOpenFileName (of) THEN
			AktString := vbname ;
			RETURN TRUE ;
		ELSE
			err := CommDlgExtendedError() ;
		END ;
	END ;
	AktString := 0C ;
	RETURN FALSE ;
END OFNtr440 ;


END OFNup.
