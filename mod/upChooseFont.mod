
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upChooseFont;

(*	06.04.18	*)

FROM SYSTEM IMPORT
	BYTE, ADR ;

FROM WIN32 IMPORT
	HWND ;

FROM WINX IMPORT
	NULL_HDC ;

FROM COMMDLG IMPORT
	CHOOSEFONT, ChooseFont,
	SCREEN_FONTTYPE,
	CF_FIXEDPITCHONLY, CF_SCREENFONTS,
	CF_INITTOLOGFONTSTRUCT, CF_NOSTYLESEL,
	REGULAR_FONTTYPE ;

FROM WINGDI IMPORT
	LOGFONT,
	FIXED_PITCH,
	ANSI_CHARSET ;


PROCEDURE FillBytes(VAR a : ARRAY OF BYTE);
VAR
    i           : CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(a) DO
        a[i] := 0;
    END;
END FillBytes;


PROCEDURE PromptChooseFont(wnd : HWND ; VAR INOUT fontname : ARRAY OF CHAR) : BOOLEAN;

VAR
	cf          : CHOOSEFONT;
	lf          : LOGFONT;
BEGIN

	FillBytes(lf);
	lf.lfFaceName := fontname;
	lf.lfCharSet := ANSI_CHARSET;

	FillBytes(cf);
	cf.lStructSize := SIZE(CHOOSEFONT);
	cf.hwndOwner := wnd;
	cf.hDC := NULL_HDC;
	cf.lpLogFont := ADR(lf);
	cf.nFontType := SCREEN_FONTTYPE BOR REGULAR_FONTTYPE ;
	cf.Flags := CF_SCREENFONTS BOR CF_INITTOLOGFONTSTRUCT BOR CF_FIXEDPITCHONLY ;

	IF ChooseFont(cf) THEN
		fontname := lf.lfFaceName;
		RETURN TRUE;
	END;

	RETURN FALSE;
END PromptChooseFont;

END upChooseFont.
