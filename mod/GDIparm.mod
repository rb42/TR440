
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE GDIparm;

(*	20.06.18	*)


FROM SYSTEM IMPORT
	ADR, CAST, BYTE ;

FROM Terminal IMPORT
	GetMainWindow ;

FROM COMMDLG IMPORT
	CHOOSEFONT,
	ChooseFont,
	CF_PRINTERFONTS,
	CF_SCREENFONTS,
	CF_INITTOLOGFONTSTRUCT,
	CF_ANSIONLY,
	CF_FORCEFONTEXIST,
	CF_SCRIPTSONLY,
	CF_FIXEDPITCHONLY,
	CF_NOSTYLESEL,
	DEVNAMES, LPDEVNAMES,
	PAGESETUPDLG,
	PageSetupDlg,
	PRINTDLG,
	PrintDlg,
	PRINTER_FONTTYPE,
	REGULAR_FONTTYPE,
	PD_NOPAGENUMS, PD_USEDEVMODECOPIESANDCOLLATE, PD_PRINTSETUP,
	LPCFHOOKPROC,
	CommDlgExtendedError ;

FROM MemUtils IMPORT
	MoveMem ;

FROM WIN32 IMPORT
	GlobalLock, GlobalFree ;

FROM WINGDI IMPORT
	DEVMODE, LPDEVMODE,
	LOGFONT, FW_DONTCARE, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE ;

FROM WINX IMPORT
	NULL_HWND, NULL_HDC, NULL_HGLOBAL ;

FROM Conversions IMPORT
	CardToStr ;

FROM Strings IMPORT
	Append ;


VAR
	vFONT :		CHOOSEFONT ;

	(*
	vPAGE :		PAGESETUPDLG ;
	*)

	vPAGE :		PRINTDLG ;

	lf :		LOGFONT ;

	dm :		DEVMODE ;

	dn :		DEVNAMES ;

	err :		CARDINAL ;

	geaendert :	BOOLEAN ;

	PrinterName :	ARRAY [0..255] OF CHAR ;



PROCEDURE FillBytes(VAR a : ARRAY OF BYTE);
VAR
    i           : CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(a) DO
        a[i] := 0;
    END;
END FillBytes;


PROCEDURE ZusatzWeg (VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO LENGTH(str) DO
		IF str [i-1] = '|' THEN
			str [i-1] := 0C ;
			RETURN ;
		END ;
	END ;
END ZusatzWeg ;


PROCEDURE GDIdruckerAuswahl (VAR str : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	ZusatzWeg (str) ;
	IF DruckerDialog (str) THEN
		str := PrinterName ;
		CheckDruckerZusatz (str) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END GDIdruckerAuswahl ;


PROCEDURE GDIfontAuswahl (VAR str : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	ZusatzWeg (str) ;
	FillBytes(lf);
	lf.lfFaceName := str;
	lf.lfCharSet := ANSI_CHARSET;

	lf.lfHeight := 0 ;
	lf.lfWidth := 0 ;
	lf.lfEscapement := 0 ;
	lf.lfOrientation := 0 ;
	lf.lfWeight := FW_DONTCARE ;
	lf.lfItalic := FALSE ;
	lf.lfUnderline := FALSE ;
	lf.lfStrikeOut := FALSE ;
	lf.lfCharSet := ANSI_CHARSET ;
	lf.lfOutPrecision := OUT_DEFAULT_PRECIS ;
	lf.lfClipPrecision := CLIP_DEFAULT_PRECIS ;
	lf.lfQuality := DEFAULT_QUALITY ;
	lf.lfPitchAndFamily := FF_DONTCARE ;


	vFONT.lStructSize := SIZE (vFONT) ;
	vFONT.hwndOwner := GetMainWindow () ;
	vFONT.hDC := NULL_HDC ;
	vFONT.rgbColors := 0 ;
	vFONT.lCustData := 0 ;
	vFONT.lpfnHook := CAST (LPCFHOOKPROC, NIL) ;
	vFONT.lpLogFont := ADR (lf) ;
	vFONT.lpTemplateName := NIL  ;
	vFONT.lpszStyle := NIL ;
	vFONT.nFontType := PRINTER_FONTTYPE BOR REGULAR_FONTTYPE ;
	vFONT.nSizeMin := 0 ;
	vFONT.nSizeMax := 0 ;

	vFONT.Flags := 0 BOR CF_PRINTERFONTS BOR CF_ANSIONLY BOR CF_FORCEFONTEXIST BOR CF_FIXEDPITCHONLY BOR CF_INITTOLOGFONTSTRUCT ;
	IF ChooseFont (vFONT) THEN
		str := lf.lfFaceName ;
		CheckFontZusatz (str) ;
		RETURN TRUE ;
	ELSE
		err := CommDlgExtendedError() ;
	END ;
	RETURN FALSE ;
END GDIfontAuswahl ;


PROCEDURE NewZusatz (VAR str : ARRAY OF CHAR ; bef : ARRAY OF CHAR ; zahl : CARDINAL) ;
	VAR	str3 :	ARRAY [0..63] OF CHAR ;
BEGIN
	IF NOT geaendert THEN
		Append ('|', str) ;
		geaendert := TRUE ;
	END ;
	CardToStr (zahl, str3) ;
	Append (bef, str) ;
	Append (str3, str) ;
END NewZusatz ;


PROCEDURE DruckerDialog (VAR str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	lpDevMode :	LPDEVMODE ;
		lpDevNames :	POINTER TO RECORD
					CASE : BOOLEAN OF
					TRUE :		off :	DEVNAMES ;
					ELSE
							puff :	ARRAY [0..1023] OF CHAR ;
					END ;
				END ;
BEGIN
	vPAGE.lStructSize := SIZE (vPAGE) ;
	vPAGE.hwndOwner := GetMainWindow () ;
	vPAGE.hDevMode := NULL_HGLOBAL ;
	vPAGE.hDevNames := NULL_HGLOBAL ;
	vPAGE.Flags := PD_NOPAGENUMS BOR PD_USEDEVMODECOPIESANDCOLLATE BOR PD_PRINTSETUP ;
	IF PrintDlg (vPAGE) THEN
		IF vPAGE.hDevNames <> NULL_HGLOBAL THEN
			lpDevNames := GlobalLock (vPAGE.hDevNames) ;
			PrinterName := lpDevNames ^ .puff [lpDevNames ^ .off.wDeviceOffset .. lpDevNames ^ .off.wOutputOffset];
			GlobalFree (lpDevNames) ;
		END ;
		IF vPAGE.hDevMode <> NULL_HGLOBAL THEN
			lpDevMode := GlobalLock (vPAGE.hDevMode) ;
			dm := lpDevMode ^ ;
			GlobalFree (lpDevMode) ;
			RETURN TRUE ;
		END ;
	ELSE
		err := CommDlgExtendedError() ;
	END ;
	RETURN FALSE ;
END DruckerDialog ;


PROCEDURE CheckFontZusatz (VAR str : ARRAY OF CHAR) ;
BEGIN
	geaendert := FALSE ;
	IF vFONT.iPointSize <> 100 THEN
		NewZusatz (str, '/.', vFONT.iPointSize DIV 10) ;
	END ;
	IF vFONT.lpLogFont <> NIL THEN
		IF vFONT.lpLogFont ^ .lfItalic THEN
			NewZusatz (str, '/i', 1) ;
		END ;
		IF vFONT.lpLogFont ^ .lfUnderline THEN
			NewZusatz (str, '/u', 1) ;
		END ;
		IF vFONT.lpLogFont ^ .lfStrikeOut THEN
			NewZusatz (str, '/s', 1) ;
		END ;
		IF vFONT.lpLogFont ^  .lfWeight <> 400 THEN
			NewZusatz (str, '/w', vFONT.lpLogFont ^ .lfWeight) ;
		END ;
		IF vFONT.lpLogFont ^  .lfCharSet <> 0 THEN
			NewZusatz (str, '/c', vFONT.lpLogFont ^ .lfCharSet) ;
		END ;
		IF vFONT.lpLogFont ^ .lfPitchAndFamily <> 0 THEN
			NewZusatz (str, '/f', vFONT.lpLogFont ^ .lfPitchAndFamily) ;
		END ;

	END ;
END CheckFontZusatz ;


PROCEDURE CheckDruckerZusatz (VAR str : ARRAY OF CHAR) ;
BEGIN
	geaendert := FALSE ;
	NewZusatz (str, '/O', dm.dmOrientation) ;
	IF dm.dmDuplex > 1 THEN
		NewZusatz (str, '/D', dm.dmDuplex) ;
	END ;
	IF dm.dmPaperSize <> 9 THEN
		NewZusatz (str, '/P', dm.dmPaperSize) ;
	END ;
	IF dm.dmCopies > 1 THEN
		NewZusatz (str, '/K', dm.dmCopies) ;
	END ;
	IF dm.dmDefaultSource <> 0 THEN
		NewZusatz (str, '/S', dm.dmDefaultSource) ;
	END ;
END CheckDruckerZusatz ;


END GDIparm.
