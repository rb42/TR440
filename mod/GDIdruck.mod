
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE GDIdruck;


(*	27.03.18	*)


FROM SYSTEM IMPORT
	ADDRESS, ADR, CAST, UNREFERENCED_PARAMETER, FUNC, DWORD ;

FROM ASCII IMPORT
	si, so, esc ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Trace IMPORT
	TraceF ;

FROM Strings IMPORT
	Append,
	Length;

FROM ExStrings IMPORT
	EqualI;

FROM SysClock IMPORT
	DateTime;

IMPORT Trace ;

FROM WIN32 IMPORT
	HDC, HANDLE, HFONT, BOOL, WSIZE, COLORREF, HPEN, HBRUSH, HGDIOBJ,
	GetProfileString, POINT, RECT,
	GetLastError ;

FROM WINX IMPORT
	NULL_HDC, NULL_HWND, NULL_HFONT, NIL_POINT, NIL_STR, NULL_HPEN, NIL_DWORD,
	SelectFont, DeleteFont, SelectPen, DeletePen, SelectBrush, DeleteBrush ;

FROM WINGDI IMPORT
	DEVMODE, LOGFONT, TEXTMETRIC, DOCINFO,
	DM_MODIFY, DM_COPY,
	DM_ORIENTATION, DM_PAPERSIZE,
	DMDUP_HORIZONTAL, DMDUP_VERTICAL,
	DM_COLOR, DM_DEFAULTSOURCE, DM_YRESOLUTION,
	DM_PRINTQUALITY, DM_COPIES, DM_DUPLEX, DM_COLLATE,
	DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE,
	DMPAPER_LETTER, DMPAPER_A4,
	MM_TWIPS, TMPF_TRUETYPE,
	GetDeviceCaps, LOGPIXELSY,
	FW_LIGHT, FW_NORMAL, FW_DEMIBOLD, FW_BOLD, FW_HEAVY,
	ANSI_CHARSET, EASTEUROPE_CHARSET, OEM_CHARSET,
	OUT_TT_PRECIS, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	FIXED_PITCH, VARIABLE_PITCH, FF_MODERN, FF_DONTCARE,
	SP_APPABORT, SP_USERABORT, SP_OUTOFDISK, SP_OUTOFMEMORY,
	CreateFontIndirect, CreateDC, DeleteDC, SetMapMode,
	TextOut, GetTextMetrics, GetTextExtentPoint32,
	MoveToEx, LineTo, Rectangle,
	StartDoc, EndDoc, AbortDoc, StartPage, EndPage,
	SetAbortProc,
	DMBIN_UPPER, DMBIN_LOWER, DMBIN_MANUAL,
	SetTextColor, SetBkColor, GetTextColor, GetBkColor,
	SetBkMode, TRANSPARENT, OPAQUE,
	CreatePen, PS_SOLID,
	CreateBrushIndirect, LOGBRUSH, BS_SOLID,
	Polygon,
	GetObject,
	ResetDC,
	SetROP2, R2_WHITE, R2_BLACK, R2_COPYPEN ;

FROM WINUSER IMPORT
	FillRect ;

FROM WINSPOOL IMPORT
	OpenPrinter, ClosePrinter, DocumentProperties ;

FROM Conversions IMPORT
	CardToString,
	StrToCard ;

FROM FileFunc IMPORT
	File, EOL, FileNameParts, FileExists,
	OpenFile, CloseFile, DeleteFile, SetFileBuffer, ReadOnlyDenyWrite,
	ReadChar, ParseFileName, SetFilePos, GetFilePos, FileSpecString,
	GetFileDateTime;


TYPE

	PrintDriverInfo			= POINTER TO DEVMODE ;

	tFontAttribut			= (
						fProportional,
						fFett,
						fUnterstrichen,
						fHinterlegt,
						fInvers
					  ) ;

	sFontAttribute			= SET OF tFontAttribut ;

	tFontName			= ARRAY [0..31] OF CHAR ;

	pFont				= POINTER TO tFont ;

	tFont				= RECORD
						Next			: pFont ;
						Attr			: sFontAttribute ;
						pointSize		: INTEGER ;
						PenLinie		: HPEN ;
						Handle			: HFONT ;
						Name			: tFontName ;
					  END ;

	tCharFont			= RECORD
						pointSize		: INTEGER ;
						Name			: tFontName ;
					  END ;



CONST

	mitResetDC			= FALSE ;

	MaxCharsPerLine			= 256;

	StringsStart			= 60000;
	SpoolerOutOfDiskSpace		= StringsStart + 0;
	SpoolerOutOfMemory		= StringsStart + 1;
	PrintingError			= StringsStart + 2;
	ErrorCreatingPage		= StringsStart + 3;
	ErrorStartingDocument		= StringsStart + 4;
	(*ErrorOnlyTrueType		= StringsStart + 5;*)
	ErrorSelectingFont		= StringsStart + 6;
	ErrorInitializing		= StringsStart + 7;
	ErrorOpeningFile		= StringsStart + 8;

	schwarz				= 0 ;
	weiss				= 0FFFFFFH ;

TYPE
	PrintResults			= (PrintOk, PrintEof, PrintAbort, PrintError);


VAR
	initialisiert :		BOOLEAN = FALSE ;

	AutoRestart,
	AutoEndePruefen,
	ersterAuftrag,
	AuftragAktiv :		BOOLEAN ;
	BlockSatz,
	Zentrieren,
	mitAbsolutPositionierung,
	DPpositionenZaehlen,
	ProportionalDruck,
	ManuellFrageUnterdruecken,
	SchonTextAufSeite,
	DruckUnterdrueckung,
	RandMarkierung,
	ImKopfText,
	ImVerborgenenText,
	OhneFFamEnde,
	AbsPosBlank3 :		BOOLEAN ;

	EndeNachAuftrag :	BOOLEAN ;

	HeftRand,
	ZielX,
	ZielY,
	ZielPixelX,
	ZielPixelY,

	AktZeilenAbstand,
	AktuelleKopie,
	AnzahlKopien,
	AbsPosX,
	AbsPosY,
	Heinheit,
	Veinheit,
	EinheitRechteck,
	OffsetLinksPitch,
	HfaktorRand,
	HfaktorText,
	Vfaktor,
	AktDpSpalte,
	PunktFaktor,
	PunktDivisor,
	AktPunktGroesse,
	GraphikAufloesung,
	AbsolutBlankAnzahl,
	MittlereBreite,
	ProportionalOffset,
	FontPunktGroesse,
	HfaktorTextKandidat,
	StdFontPunktGroesse,
	ZeilenAbstandGanz,
	ZeilenAbstandViertel,
	AktViertelZeile,
	AutoEnde,
	Hmin,
	Vmin,
	Hmax,
	Vmax :			CARDINAL ;

	outMode		: PrintDriverInfo ;

	keinDrucker	: BOOLEAN = TRUE ;

	ausgedruckt	: BOOLEAN = FALSE ;

	PageBedruckt	: BOOLEAN = FALSE ;

	inInitialisierung :	BOOLEAN = TRUE ;

	GDIdruckerOffen :	BOOLEAN = FALSE ;

	GDIdruckerName	: ARRAY [0..255] OF CHAR ;

	GDIjobName	: ARRAY [0..127] OF CHAR ;

	linesPerPage	: CARDINAL;

	Puffer		: POINTER TO ARRAY [0..1023] OF CHAR = NIL ;

	xKoordinatePufferAnfang,
	yKoordinatePufferAnfang
			: CARDINAL ;

	PufferIndex	: CARDINAL = 0 ;

	printRes	: PrintResults ;

	hDC		: HDC ;

	hPrn		: HANDLE;

	hgAkt,
	vgAkt		: CARDINAL ;

	di		: DOCINFO ;

	lf		: LOGFONT;

	tm		: TEXTMETRIC;

	Font0		: pFont = NIL ;

	pAktFont	: pFont = NIL ;

	xKoordinate,
	yKoordinate,
	xMerkCursor,
	yMerkCursor,
	LeftMargin,
	TopMargin,
	ZeilenAbstand,
	BasisZeichenBreite,
	BasisZeilenAbstand,
	TransVertikal	: INTEGER ;

	yResolution	: CARDINAL ;


	AnfangsPunktgroesse,
	AnfangsHeight,
	AnfangsWeight,
	AnfangsItalic,
	AnfangsUnderline,
	AnfangsStrikeout,
	AnfangsOutPrecision,
	AnfangsPitchFamily,
	AnfangsPitch,
	AnfangsPenDicke,
	AnfangsOrientationFont,
	AnfangsEscapement,
	AnfangsQuality,
	AnfangsCharSet,

	AnfangsOrientation,
	AnfangsPrintQuality,
	AnfangsDuplex,
	AnfangsPapiergroesse,
	AnfangsKopien,
	AnfangsSource,
	AnfangsYresolution,
	AnfangsCollate,
	AnfangsTT,
	AnfangsMonochrome :	CARDINAL ;



PROCEDURE SetDefaultFontAktuell (fontname : ARRAY OF CHAR) ;
BEGIN
	SetNewFont (fontname, 0, sFontAttribute { } ) ;
	BasisZeichenBreite := tm.tmAveCharWidth ;
	BasisZeilenAbstand := ZeilenAbstand ;
END SetDefaultFontAktuell ;


PROCEDURE CheckError (erg : DWORD; bez : ARRAY OF CHAR) : DWORD ;
	VAR	zw :	CARDINAL ;
BEGIN
	IF erg:CARDINAL = 0 THEN
		zw := GetLastError () ;
		TraceF ('SysError %h bei %s', zw, bez) ;
	ELSIF Trace.SEBtrace BAND 32 <> 0 THEN
		TraceF ('GDI %s', bez) ;
	END ;
	RETURN erg ;
END CheckError ;


PROCEDURE CardVal (str : ARRAY OF CHAR) : CARDINAL ;
	VAR	i :	CARDINAL ;
BEGIN
	IF StrToCard (str, i) THEN
		RETURN i ;
	ELSE
		RETURN 0 ;
	END ;
END CardVal ;


PROCEDURE ZusatzAuswerten (zus : ARRAY OF CHAR ; inDruckerDefinition : BOOLEAN) ;
	VAR	i,
		len :	CARDINAL ;

	PROCEDURE GetZahl () : CARDINAL ;
		VAR	j,
			z :	CARDINAL ;
	BEGIN
		j := i + 1 ;
		WHILE (zus [j] >= '0') AND (zus [j] <= '9') DO
			j := j+1 ;
		END ;
		IF j = i + 1 THEN
			z := 0 ;
		ELSE
			z := CardVal (zus [i+1 .. j-1]) ;
		END ;
		i := j - 1 ;
		RETURN z ;
	END GetZahl ;

BEGIN
	TraceF ('zus=%s', zus) ;


	MaxDruckZeilen := 68 ;
	len := LENGTH (zus) ;
	i := 0 ;
	LOOP
		IF i >= len THEN
			EXIT ;
		END ;
		IF zus [i] = '/' THEN
			i := i + 1 ;
			CASE zus [i] OF
		(* Kleinbuchstaben für Font-Definition : *)
			  '.' :			(* Punktgrößen-Definition *)
				AnfangsPunktgroesse := GetZahl () ;
			| 'h' :			(* Height-Angabe *)
				AnfangsHeight := GetZahl () ;		(* height = PointSize * DruckerPixelProInchVertikal / 72 *)
			| 'w' :			(* Weight-Angabe *)
				AnfangsWeight := GetZahl () ;		(* 0..1000 : 100=dünn, 400=normal, 700=bold, 800=Ultrabold, 900=Heavy *)
			| 'i' :			(* Italic *)
				AnfangsItalic := GetZahl () ;		(* 1=Italic *)
			| 'u' :			(* Underline *)
				AnfangsUnderline := GetZahl () ;	(* 1=Underline *)
			| 's' :			(* StrikeOut *)
				AnfangsStrikeout := GetZahl () ;	(* 1=StrikeOut *)
			| 't' :			(* Typ=OutPrecision *)
				AnfangsOutPrecision := GetZahl () ;	(* 3=normal, 7=nur TrueType, 4=vorz. TrueType, 5=vorz. DeviceFont, 6=vorz. Raster, 8=vorz. Outline *)
			| 'p' :			(* Pitch *)
				AnfangsPitch := GetZahl () ;		(* 10 / 12 / 15 / 18 *)
			| 'o' :			(* Orientation *)
				AnfangsOrientationFont := GetZahl () ;	(* Winkel zur x-Achse in 1/10 Grad, normal=0 *)
			| 'f' :			(* Pitch And Family *)
				AnfangsPitchFamily := GetZahl () ;	(* 1=variablePitch, 2=fixedPitch, 16=Roman, 32=Modern, 64=Script, 128=Decorative *)
			| 'l' :			(* Linienmodus - Dicke *)
				AnfangsPenDicke := GetZahl () ;		(* 1, 2, 3 etc. *)

			| 'e' :			(* Escapement *)
				AnfangsEscapement := GetZahl () ;	(* ähnlich Orientation, normal=0 *)
			| 'c' :			(* CharSet *)
				AnfangsCharSet := GetZahl () ;		(* normal-ANSI=0, default=1, Symbol=2 *)
			| 'q' :			(* Quality *)
				AnfangsQuality := GetZahl () ;		(* default=0, Draft=1, Proof=2, NonAntialiased=3, Antialiased=4 *)

		(* Großbuchstaben für Drucker-Definition : *)
			| 'O' :			(* Orientation *)
				AnfangsOrientation := GetZahl () ;	(* 1=Portrait, 2=Landscape *)
				IF AnfangsOrientation = 2 THEN
					MaxDruckZeilen := 48 ;
				ELSE
					MaxDruckZeilen := 68 ;
				END ;
			| 'Q' :			(* Quality *)
				AnfangsPrintQuality := GetZahl () ;	(* 1=Draft, 2=Low, 3=Medium, 4=High, >10 => DPI *)
			| 'D' :			(* Duplex *)
				AnfangsDuplex := GetZahl () ;		(* 1=kein Duplex, 2=lange Seite, 3=kurze Seite *)
			| 'A' :			(* Ax - Papier *)
				CASE GetZahl() OF
				  2 :	AnfangsPapiergroesse := 66 ;
				| 3 :	AnfangsPapiergroesse := 8 ;
				| 4 :	AnfangsPapiergroesse := 9 ;
				| 5 :	AnfangsPapiergroesse := 11 ;
				| 6 :	AnfangsPapiergroesse := 70 ;
				ELSE
				END ;
			| 'B' :			(* Bx - Papier *)
				CASE GetZahl() OF
				  4 :	AnfangsPapiergroesse := 12 ;
				| 5 :	AnfangsPapiergroesse := 13 ;
				ELSE
				END ;
			| 'P' :			(* Papier *)
				AnfangsPapiergroesse := GetZahl () ;	(* A2=66, A3=8, A4=9, A5=11, A6=70 *)
			| 'K' :			(* Kopien *)
				AnfangsKopien := GetZahl () ;
			| 'S' :			(* Source *)
				AnfangsSource := GetZahl () ;
			| 'Y' :			(* Yresolution *)
				AnfangsYresolution := GetZahl () ;	(* dann muss /Qnnn angegeben sein *)
			| 'C' :			(* Collate *)
				AnfangsCollate := GetZahl () ;		(* 0=kein Collate, 1=mit Collate *)
			| 'T' :			(* TToption *)
				AnfangsTT := GetZahl () ;		(* 1=Bitmap, 2=Download, 3=Subdev, 4=Download/Outline *)
			| 'M' :			(* Monochrome *)
				AnfangsMonochrome := GetZahl () ;	(* 1=Monochrome, 2=Color *)
			ELSE
			END ;
		END ;
		i := i + 1 ;
	END ;
END ZusatzAuswerten ;


PROCEDURE upPufferAusstuelpen (aufJedenFall : BOOLEAN) ;
	VAR	siz :	WSIZE ;
		i :	INTEGER ;
BEGIN
	IF inInitialisierung OR NOT GDIdruckerOffen THEN
		PufferIndex := 0 ;
		PageBedruckt := FALSE ;
		ausgedruckt := FALSE ;
		RETURN ;
	END ;

	IF (PufferIndex <> 0) OR aufJedenFall THEN
		IF NOT PageBedruckt THEN
			i := StartPage(hDC) ;
			IF i <= 0 THEN
				TraceF ('StartPageFail %c', 0 - i) ;
			ELSE
				TraceF ('StartPage') ;
			END ;
			PageBedruckt := TRUE ;
		END ;
	END ;
	IF PufferIndex <> 0 THEN
		Puffer ^ [PufferIndex]:= 0C ;
		CheckError (TextOut (hDC, xKoordinatePufferAnfang, yKoordinatePufferAnfang, Puffer ^ [0..PufferIndex], PufferIndex), 'TextOut') ;
		IF Trace.SEBtrace BAND 32 <> 0 THEN
			TraceF ('TextOut [%i,%i] %s', xKoordinatePufferAnfang, yKoordinatePufferAnfang, Puffer ^ ) ;
		END ;
		CheckError (GetTextExtentPoint32 (hDC, Puffer ^ [0..PufferIndex] , PufferIndex, siz), 'GetTextExtentPoint32') ;
		xKoordinate := xKoordinatePufferAnfang + ORD(siz.cx) ;
		IF Trace.SEBtrace BAND 32 <> 0 THEN
			TraceF ('  X=%i', xKoordinate) ;
		END ;
		PufferIndex := 0 ;
		ausgedruckt := TRUE ;
	END ;
END upPufferAusstuelpen ;


PROCEDURE PufferAusstuelpen ;
BEGIN
	upPufferAusstuelpen (FALSE) ;
END PufferAusstuelpen ;


PROCEDURE MakeFont (font : pFont) ;
	VAR	name :	ARRAY [0..127] OF CHAR ;
		i :	CARDINAL ;
		dvc,
		dvc2 :	INTEGER ;
BEGIN
	AnfangsPunktgroesse := 0 ;
	AnfangsHeight := 0 ;
	AnfangsWeight := 0 ;
	AnfangsItalic := 0 ;
	AnfangsUnderline := 0 ;
	AnfangsStrikeout := 0 ;
	AnfangsOutPrecision := 0 ;
	AnfangsOrientationFont := 0 ;
	AnfangsPitchFamily := 0 ;
	AnfangsPitch := 0 ;
	AnfangsPenDicke := 2 ;
	AnfangsCharSet := 0 ;
	AnfangsEscapement := 0 ;
	AnfangsQuality := 0 ;

	name := font ^ .Name ;
	IF name [0] > ' ' THEN
		FOR i := 1 TO LENGTH(name) DO
			IF name [i-1] = '|' THEN
				ZusatzAuswerten (name [i .. LENGTH(name)] , FALSE) ;
				name [i-1] := 0C ;
				BREAK ;
			END ;
		END ;
	END ;

	IF fFett IN font ^ .Attr THEN
		lf.lfWeight := FW_BOLD ;
	ELSIF AnfangsWeight <> 0 THEN
		lf.lfWeight := AnfangsWeight ;
	ELSE
		lf.lfWeight := FW_NORMAL ;
	END ;

	dvc := GetDeviceCaps(hDC, LOGPIXELSY) ;
	dvc2 := 600 ;

	IF font ^ .pointSize <> 0 THEN
		lf.lfHeight := - (font ^ .pointSize * 254 * dvc2 DIV 7200) ;
	ELSIF AnfangsHeight <> 0 THEN
		lf.lfHeight := AnfangsHeight ;
	ELSIF AnfangsPunktgroesse <> 0 THEN
		lf.lfHeight :=  - (VAL(INTEGER, AnfangsPunktgroesse) * 254 * dvc2 DIV 7200) ;
		font ^ .pointSize := AnfangsPunktgroesse ;
	ELSE
		lf.lfHeight :=  - (10 * 254 * dvc2 DIV 7200) ;
	END ;

	TraceF ('MakeFont %s => LOGPIXELSY=%c, lfHeight=%i', name, dvc, lf.lfHeight) ;

	IF AnfangsPitch = 0 THEN
		lf.lfWidth := 0 ;
	ELSE
		lf.lfWidth := - (254 * 120 DIV VAL(INTEGER, AnfangsPitch) * dvc2 DIV 7200) ;
	END ;

	lf.lfEscapement := AnfangsEscapement ;

	lf.lfOrientation := AnfangsOrientationFont ;

	IF AnfangsItalic <> 0 THEN
		lf.lfItalic := TRUE ;
	ELSE
		lf.lfItalic := FALSE;
	END ;

	IF fUnterstrichen IN font ^ .Attr THEN
		lf.lfUnderline := TRUE ;
	ELSIF AnfangsUnderline <> 0 THEN
		lf.lfUnderline := TRUE ;
	ELSE
		lf.lfUnderline := FALSE ;
	END ;

	IF AnfangsStrikeout <> 0 THEN
		lf.lfStrikeOut := TRUE ;
	ELSE
		lf.lfStrikeOut := FALSE;
	END ;

	lf.lfCharSet := AnfangsCharSet ;

	IF AnfangsOutPrecision <> 0 THEN
		lf.lfOutPrecision := AnfangsOutPrecision ;
	ELSE
		lf.lfOutPrecision := OUT_DEFAULT_PRECIS ;
	END ;

	lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;

	lf.lfQuality := AnfangsQuality ;

	IF (fProportional IN font ^ .Attr)
	OR (AnfangsPitchFamily BAND VARIABLE_PITCH = VARIABLE_PITCH) THEN
		lf.lfPitchAndFamily := VARIABLE_PITCH ;
		mitAbsolutPositionierung := TRUE ;
	ELSE
		lf.lfPitchAndFamily := FIXED_PITCH ;
	END ;
	IF AnfangsPitchFamily <> 0 THEN
		lf.lfPitchAndFamily := ORD (lf.lfPitchAndFamily) BOR AnfangsPitchFamily ;
	END ;

	font ^ .PenLinie := CheckError (CreatePen (PS_SOLID, AnfangsPenDicke * 6, 0), 'CreatePen') ;	(* schwarz, 2 Pixel breit *)
	TraceF ('PenLinie=%h, Dicke=%c', font ^ .PenLinie, AnfangsPenDicke) ;

	lf.lfFaceName := name;

	font ^ .Handle := CheckError (CreateFontIndirect(lf), 'CreateFontIndirect') ;

	TraceF ('FontHandle=%h', font ^ .Handle) ;
END MakeFont ;


PROCEDURE SetFontZeilenAbstand ;
	VAR	PapierLaenge :	CARDINAL ;
BEGIN
	ZeilenAbstand := lf.lfHeight ;
	IF outMode ^ .dmOrientation = DMORIENT_PORTRAIT THEN
		PapierLaenge := ORD (outMode ^ .dmPaperLength) ;
	ELSE
		PapierLaenge := ORD (outMode ^ .dmPaperWidth) ;
	END ;

	MaxDruckZeilen :=	(
				  PapierLaenge				(* Papierlänge in 1/10 mm *)
(*
				- ORD (0 - TopMargin)			(* abzgl. oberer Rand *)
*)
				)
					* 16
					* 100
				DIV (	ORD(0 - lf.lfHeight)		(* Fonthöhe in negativer yKoordinate *)
					* 3
					* ORD(outMode ^ .dmScale)	(* Skalierung in 1/100 *)
				    ) ;					(* aus Papierlänge in 1/10 mm und Fonthöhe Zeilenanzahl berechnen *)
END SetFontZeilenAbstand ;


PROCEDURE SetFontAktuell (font : pFont) ;
	VAR	oldFont :	HFONT ;
BEGIN
	IF font = NIL THEN
		TraceF ('Font=NIL') ;
	ELSE
		pAktFont := font ;
		oldFont := CheckError (SelectFont (hDC, font ^ .Handle), 'SelectFont') ;
		CheckError (GetTextMetrics (hDC, tm), 'GetTextMetrics') ;
		TraceF ('FontMetrics: Handle=%h, W=%c, H=%c, O=%h', font ^ .Handle, tm.tmAveCharWidth, tm.tmHeight, oldFont) ;
	END ;
END SetFontAktuell ;


PROCEDURE SetNewFont (name : ARRAY OF CHAR ; pointSize : INTEGER ; Attr : sFontAttribute) ;
	VAR	i :		CARDINAL ;
		merkFont,
		lastFont :	pFont ;
BEGIN
	PufferAusstuelpen ;

	lastFont := Font0 ;

	NEW (merkFont) ;
	merkFont ^ .Next := NIL ;
	IF Font0 = NIL THEN
		Font0 := merkFont ;
	END ;
	IF lastFont <> NIL THEN
		lastFont ^ .Next := merkFont ;
	END ;
	merkFont ^ .Name := name ;
	merkFont ^ .pointSize := pointSize ;
	merkFont ^ .Attr := Attr ;

	TraceF ('NewFont %s, Pt=%c, A=%h', name, pointSize, Attr) ;

	MakeFont (merkFont) ;

	SetFontAktuell (merkFont) ;

	IF Trace.SEBtrace BAND 32 <> 0 THEN
		CheckError (GetObject (CAST (HGDIOBJ, merkFont ^ .Handle), SIZE(lf), ADR (lf)), 'GetObject') ;
		TraceF ('Font %s : %iH %iW %iC %iQ %iP', lf.lfFaceName, ORD (lf.lfHeight), ORD (lf.lfWidth), ORD (lf.lfCharSet), ORD (lf.lfQuality), ORD (lf.lfPitchAndFamily)) ;
	END ;

END SetNewFont ;



PROCEDURE GetPrinterDC(DruckerName : ARRAY OF CHAR) : HDC;
VAR
	inMode		: PrintDriverInfo ;
	size,
	fields		: CARDINAL ;
	printres	: INTEGER ;
BEGIN

	hDC := NULL_HDC;

	IF OpenPrinter(DruckerName, hPrn, NIL) THEN

		TraceF ('OpenPrinter %s', DruckerName) ;

		inMode := NIL ;

		size := DocumentProperties(NULL_HWND,	hPrn,
							DruckerName,			(* Drucker-Name			*)
							outMode ^ ,			(* hier unbenutzt		*)
							outMode ^ ,			(* dto.				*)
							0);				(* = 0 => Größe zurückliefern	*)

		IF size > 0 THEN

			IF outMode = NIL THEN
				ALLOCATE(outMode, size) ;
			END ;

			(*
			ALLOCATE(inMode, size);
			*)

			(* hier Werte in inMode besetzen *)

			IF DocumentProperties(NULL_HWND,hPrn,
							DruckerName,			(* Drucker-Name			*)
							outMode ^ ,			(* initialisieren von outMode	*)
							inMode ^ ,			(* und einmischen von inMode	*)
							DM_COPY BOR DM_MODIFY) = 1
			THEN

				printres := outMode ^ .dmPrintQuality ;
				yResolution := outMode ^ .dmYResolution ;
				Trace.TraceF ('PrintQuality %i yResolution %i', printres, yResolution) ;
				IF printres > 0 THEN
					yResolution := printres ;
				END ;
				IF (yResolution = 0) OR (yResolution > 10000) THEN
					yResolution := 600 ;
				END ;

				fields := DM_ORIENTATION BOR DM_PAPERSIZE ;		(* die Felder sind immer besetzt *)

				outMode ^ .dmOrientation := AnfangsOrientation ;
				outMode ^ .dmPaperSize := AnfangsPapiergroesse ;

				IF AnfangsDuplex <> 0 THEN
					outMode ^ .dmDuplex := AnfangsDuplex ;
					fields := fields BOR DM_DUPLEX ;
				END ;

				IF AnfangsKopien <> 0 THEN
					outMode ^ .dmCopies := AnfangsKopien ;
					fields := fields BOR DM_COPIES ;
				END ;

				IF AnfangsPrintQuality <> 0 THEN
					IF AnfangsPrintQuality > 10 THEN	(* DPI-Angabe *)
						outMode ^ .dmPrintQuality := AnfangsPrintQuality ;
					ELSE
						outMode ^ .dmPrintQuality := 0 - AnfangsPrintQuality ;
					END ;
					fields := fields BOR DM_PRINTQUALITY ;
				END ;

				IF AnfangsYresolution <> 0 THEN
					outMode ^ .dmYResolution := AnfangsYresolution ;
					fields := fields BOR DM_YRESOLUTION ;
				END ;

				IF AnfangsSource <> 0 THEN
					outMode ^ .dmDefaultSource := AnfangsSource ;
					fields := fields BOR DM_DEFAULTSOURCE ;
				END ;

				IF AnfangsCollate <> 0 THEN
					outMode ^ .dmCollate := AnfangsCollate ;
					fields := fields BOR DM_COLLATE ;
				END ;

				IF AnfangsMonochrome <> 0 THEN
					outMode ^ .dmColor := AnfangsMonochrome ;
					fields := fields BOR DM_COLOR ;
				END ;

				outMode ^ .dmFields := outMode ^ .dmFields BOR fields ;

				IF DocumentProperties(NULL_HWND,	hPrn,
									DruckerName,
									outMode ^ ,
									outMode ^ ,
									DM_COPY BOR DM_MODIFY) = 1
				THEN
					hDC := CreateDC(NIL_STR,	DruckerName,
								   	NIL_STR,
									outMode ^ );
					TraceF ('DC=%h', hDC) ;
					SetBkMode (hDC, TRANSPARENT) ;
				END;
			ELSE
				CheckError (0, 'DocumentProperties') ;
			END;

			(*
			IF outMode <> NIL THEN
				DEALLOCATE(outMode, size);
			END;
			*)
		ELSE
			CheckError (0, 'DocumentProperties') ;
		END;

	ELSE
		CheckError (0, 'OpenPrinter') ;
	END;

	RETURN hDC ;
END GetPrinterDC;


PROCEDURE DruckerBinaerAusgabe (ch : CHAR) ;
BEGIN
	IF GDIdruckerOffen THEN
		IF  PufferIndex = 0 THEN
			xKoordinatePufferAnfang := xKoordinate ;
			yKoordinatePufferAnfang := yKoordinate + TransVertikal ;	(*	29.04.13	*)
		END ;
		Puffer ^ [PufferIndex] := ch ;
		INC (PufferIndex) ;
		IF PufferIndex >= HIGH (Puffer ^ ) THEN
			PufferAusstuelpen ;
		END ;
	END ;
END DruckerBinaerAusgabe ;


PROCEDURE ZeichenAusgeben (zeile : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO Length (zeile) DO
		DruckerBinaerAusgabe (zeile [i-1]) ;
	END ;
END ZeichenAusgeben ;


PROCEDURE upOpenGDIdrucker () : BOOLEAN ;
	VAR
		i,j :		CARDINAL ;
		name :		ARRAY [0..255] OF CHAR ;
		zwname,
		strDevice :	ARRAY [0..255] OF CHAR ;
		strZusatz :	ARRAY [0..255] OF CHAR ;
		pDriver,
		pDevice,
		pOutput		: ARRAY [0..255] OF CHAR ;
BEGIN
	name := GDIdruckerName ;

	inInitialisierung := TRUE ;

	ausgedruckt := FALSE ;

	GDIdruckerOffen := TRUE ;
	PufferIndex := 0 ;

	TraceF ('OpenGDIdrucker %s', name) ;


	IF name [0] <= ' ' THEN
		IF name [0] = '|' THEN
			ZusatzAuswerten (name [1..HIGH(name)], TRUE) ;
		END ;
		GetProfileString ("windows", "device", "", strDevice, HIGH(strDevice)) ;
	ELSE
		FOR i := 1 TO LENGTH (name) DO
			IF name [i-1] = '|' THEN
				strZusatz := name [i .. HIGH(name)] ;
				name [i-1] := 0C ;
				ZusatzAuswerten (strZusatz, TRUE) ;
				BREAK ;
			END ;
		END ;
		GetProfileString ("Devices", name, "", zwname, HIGH(zwname)) ;
		strDevice := name ;
		Append (',', strDevice) ;
		Append (zwname, strDevice) ;
	END ;

	TraceF ('Devices %s', strDevice) ;

	i := 0 ;
	pDriver [0] := 0C ;
	pOutput [0] := 0C ;
	LOOP
		INC (i) ;
		IF strDevice [i] = 0C THEN
			pDevice := strDevice ;
			DEC (i) ;
			EXIT ;
		END ;
		IF strDevice [i] = "," THEN
			pDevice := strDevice [0..i-1] ;
			EXIT ;
		END ;
	END ;
	j := i ;
	LOOP
		INC (j) ;
		IF strDevice [j] = 0C THEN
			DEC (j) ;
			EXIT ;
		END ;
		IF strDevice [j] = "," THEN
			IF i+1 = j THEN
				pDriver := "" ;
			ELSE
				pDriver := strDevice [i+1..j-1] ;
			END ;
			pOutput := strDevice [j+1..HIGH(strDevice)] ;
			EXIT ;
		END ;
	END ;

	TraceF ('Device %s', pDevice) ;

	hDC := CheckError (GetPrinterDC (pDevice), 'GetPrinterDC') ;

	IF hDC <> NULL_HDC THEN
		keinDrucker := FALSE ;

		RETURN TRUE ;
	END ;

	RETURN FALSE ;
END upOpenGDIdrucker ;


PROCEDURE VorbesGDIdrucker ;
BEGIN
	AnfangsPunktgroesse := 0 ;			(* keine Angabe				*)
	AnfangsOrientation := DMORIENT_LANDSCAPE ;	(* Querdruck				*)
	AnfangsDuplex := 0 ;				(* kein Duplex				*)
	AnfangsPapiergroesse := DMPAPER_A4 ;		(* A4					*)
	AnfangsKopien := 0 ;				(* keine Drucker-Kopien			*)
	AnfangsPrintQuality := 0 ;			(* Default-Qualität			*)
	AnfangsYresolution := 0 ;			(* keine Angabe				*)
	AnfangsSource := 0 ;				(* Default-Source			*)
	AnfangsCollate := 0 ;				(* kein Collate				*)
	AnfangsTT := 0 ;				(* keine TrueType-Sonderbehandlung	*)
	AnfangsMonochrome := 1 ;			(* expl. S/W auf Farbdrucker		*)

END VorbesGDIdrucker ;


PROCEDURE OpenGDIdrucker (name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN

	GdiInit ;

	GDIdruckerName := name ;

	VorbesGDIdrucker ;

	RETURN upOpenGDIdrucker () ;
END OpenGDIdrucker ;


PROCEDURE CloseGDIdrucker ;
BEGIN
	IF GDIdruckerOffen THEN

		DeleteFonts ;
		TraceF ('CloseGDIdrucker') ;
		IF hDC <> NULL_HDC THEN

			TraceF ('ClosePrinter') ;
			ClosePrinter(hPrn);

			DeleteDC (hDC) ;
			hDC := NULL_HDC ;
		END ;
		keinDrucker := TRUE ;
		GDIdruckerOffen := FALSE ;
	END ;

END CloseGDIdrucker ;


PROCEDURE GroesseEinstellen (PunktGroesse : CARDINAL) ;
BEGIN
	SetNewFont ('', PunktGroesse,  sFontAttribute { } ) ;
END GroesseEinstellen ;


PROCEDURE ZeilenAnfang ;
BEGIN
	xKoordinate := LeftMargin ;
END ZeilenAnfang ;


PROCEDURE ZeilenEndeAusgeben ;
BEGIN
	PufferAusstuelpen ;
	yKoordinate := yKoordinate + ZeilenAbstand ;
END ZeilenEndeAusgeben ;


PROCEDURE Vorschub (Zeilenzahl : INTEGER) ;
BEGIN
	PufferAusstuelpen ;
	yKoordinate := yKoordinate + ZeilenAbstand * Zeilenzahl ;
END Vorschub ;


PROCEDURE Pitch10ausgeben ;
BEGIN
	IF pAktFont <> NIL THEN
		SetNewFont ('', 127 DIV 10, pAktFont ^ .Attr) ;
	END ;
END Pitch10ausgeben ;

PROCEDURE Pitch12ausgeben ;
BEGIN
	IF pAktFont <> NIL THEN
		SetNewFont ('', 127 DIV 12, pAktFont ^ .Attr) ;
	END ;
END Pitch12ausgeben ;


PROCEDURE Pitch15ausgeben ;
BEGIN
	IF pAktFont <> NIL THEN
		SetNewFont ('', 127 DIV 15, pAktFont ^ .Attr) ;
	END ;
END Pitch15ausgeben ;


PROCEDURE ProportionalAusschalten ;
BEGIN
	IF pAktFont <> NIL THEN
		SetNewFont ('', pAktFont ^ .pointSize, pAktFont ^ .Attr - sFontAttribute { fProportional }) ;
	END ;
END ProportionalAusschalten ;


PROCEDURE Ausstuelpen ;
BEGIN
	PufferAusstuelpen ;
END Ausstuelpen ;


PROCEDURE SeitenWechselAusgeben ;
	VAR	pageRes :	INTEGER ;
BEGIN
	PufferAusstuelpen ;
	pageRes := EndPage(hDC);
	TraceF ('EndPage %I', pageRes) ;
	IF pageRes <= 0 THEN	(* Fehler *)
		(*
		IF pageRes < 0 THEN
			CASE pageRes OF
			SP_APPABORT, SP_USERABORT:
				RETURN PrintAbort;
			|
			SP_OUTOFDISK:
				MessageBoxId(SpoolerOutOfDiskSpace, MsgWarning);
			|
			SP_OUTOFMEMORY:
				MessageBoxId(SpoolerOutOfMemory, MsgWarning);
			ELSE
				IF Abort THEN
					RETURN PrintAbort;
				ELSE
					MessageBoxId(PrintingError, MsgWarning);
				END;
			END;
			RETURN PrintError;
		ELSE
			MessageBoxId(ErrorCreatingPage, MsgWarning);
			RETURN PrintError;
		END ;
		*)
	END;
	PageBedruckt := FALSE ;
END SeitenWechselAusgeben ;


PROCEDURE SeitenAnfangAusgeben ;
BEGIN
	PufferAusstuelpen ;
	PageBedruckt := FALSE ;
	CheckError (SetMapMode(hDC, MM_TWIPS), 'SetMapMode') ;
	xKoordinate := LeftMargin ;
	yKoordinate := TopMargin ;
	TransVertikal := 0 ;
END SeitenAnfangAusgeben ;



PROCEDURE SetZeilenAbstand (Abstand : CARDINAL) ;
BEGIN
	AktZeilenAbstand := Abstand ;

	CASE Abstand OF
	  1 :	(* 1 *)
		ZeilenAbstand := - 240 ;
		ZeilenAbstandGanz := 1 ;
		ZeilenAbstandViertel := 0 ;
		Vfaktor := Veinheit DIV 6 ;
	| 2 :	(* 1 1/2 *)
		ZeilenAbstand := - 360 ;
		ZeilenAbstandGanz := 1 ;
		ZeilenAbstandViertel := 2 ;
		Vfaktor := Veinheit DIV 4 ;
	| 3 :	(* 2 *)
		ZeilenAbstand := - 480 ;
		ZeilenAbstandGanz := 2 ;
		ZeilenAbstandViertel := 0 ;
		Vfaktor := Veinheit DIV 3 ;
	| 4 :	(* 3 *)
		ZeilenAbstand := - 720 ;
		ZeilenAbstandGanz := 3 ;
		ZeilenAbstandViertel := 0 ;
		Vfaktor := Veinheit DIV 2 ;

	ELSE	(* 1/2 *)
		ZeilenAbstand := - 120 ;
		ZeilenAbstandGanz := 0 ;
		ZeilenAbstandViertel := 2 ;
		AktZeilenAbstand := 0 ;
	  	Vfaktor := Veinheit DIV 12 ;
	END ;
END SetZeilenAbstand ;


PROCEDURE GesamtInitialisierung ;
	VAR	i :	CARDINAL ;
BEGIN
	TraceF ('GesamtInit') ;
	IF Puffer = NIL THEN
		NEW (Puffer) ;
	END ;
	PufferIndex := 0 ;

END GesamtInitialisierung ;


PROCEDURE TeilInitialisierung (mitInitString : BOOLEAN) ;
BEGIN
	TraceF ('TeilInit') ;
	vgAkt := weiss ;
	hgAkt := schwarz ;
	SeitenAnfangAusgeben ;
END TeilInitialisierung ;


PROCEDURE TeilExit ;
BEGIN
	TraceF ('TeilExit') ;
	SeitenWechselAusgeben ;
END TeilExit ;


PROCEDURE JobInit (dateiname : ARRAY OF CHAR) ;
	VAR	i :	INTEGER ;
		j :	CARDINAL;
		ch :	CHAR ;
BEGIN

	IF Puffer = NIL THEN
		NEW (Puffer) ;
	END ;
	PufferIndex := 0 ;

	IF dateiname [0] > ' ' THEN
		FOR j := 0 TO LENGTH (dateiname) DO
			ch := dateiname [j] ;
			CASE ch OF
			  '<', '>', '?', '"', ':', '/', '*', '.' :
			  	ch := '_' ;
			| '[' :	ch := 'Ä' ;
			| '\' :	ch := 'Ö' ;
			| ']' :	ch := 'Ü' ;
			| '{' :	ch := 'ä' ;
			| '|' :	ch := 'ö' ;
			| '}' :	ch := 'ü' ;
			| '~' :	ch := 'ß' ;
			ELSE
			END ;
			GDIjobName [j] := ch ;
		END ;
		TraceF ('JobInit %s -> %s', dateiname, GDIjobName) ;
	END ;

	CheckError (SetMapMode(hDC, MM_TWIPS), 'SetMapMode') ;

	LeftMargin := 480 ;
	TopMargin := -120 ;
	TransVertikal := 0 ;

	di.cbSize := SIZE(di);
	di.lpszDocName := ADR(GDIjobName) ;
	di.lpszOutput := NIL;
	di.lpszDatatype := NIL;
	di.fwType := 0;

			(*
			SetAbortProc(shInfo.dc, AbortProc);
                        *)
	i := StartDoc(hDC, di) ;
	TraceF ('StartDoc %s, ID=%i', GDIjobName, i) ;
	IF i > 0 THEN
                                (*
				Abort := FALSE;
				*)
		printRes := PrintOk ;
		inInitialisierung := FALSE ;
	ELSE
		keinDrucker := TRUE ;
		printRes := PrintAbort ;
	END ;

END JobInit ;


PROCEDURE JobExit ;
BEGIN
	TraceF ('JobExit') ;
	DeleteFonts ;

	IF (printRes = PrintError) OR (printRes = PrintAbort) THEN
		AbortDoc(hDC);
		TraceF ('AbortDoc') ;
	ELSE
		EndDoc(hDC);
		TraceF ('EndDoc') ;
	END;
END JobExit ;


PROCEDURE CRausgeben ;
BEGIN
	PufferAusstuelpen ;
	xKoordinate := LeftMargin ;
END CRausgeben ;


PROCEDURE DeleteFonts ;
	VAR	zw :	pFont ;
BEGIN
	WHILE Font0 <> NIL DO
		IF ORD(CheckError (DeleteFont (Font0 ^ .Handle), 'DeleteFont')) <> 0 THEN
			IF Trace.SEBtrace BAND 8 <> 0 THEN
				Trace.TraceF ('DeleteFont %h', Font0 ^ .Handle) ;
			END ;
		END ;
		IF ORD(CheckError (DeletePen (Font0 ^ .PenLinie), 'DeletePen2')) <> 0 THEN
			IF Trace.SEBtrace BAND 8 <> 0 THEN
				Trace.TraceF ('DeletePen2 %h', Font0 ^ .PenLinie) ;
			END ;
		END ;
		zw := Font0 ;
		Font0 := Font0 ^ .Next ;
		DISPOSE (zw) ;
	END ;
	pAktFont := NIL ;
END DeleteFonts ;


PROCEDURE GdiInit ;
BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		TraceF ('GdiInit') ;
		hDC := NULL_HDC ;
	END ;
END GdiInit ;


PROCEDURE GdiExit ;
BEGIN
	IF initialisiert THEN
		CloseGDIdrucker ;
		initialisiert := FALSE ;
		TraceF ('GdiExit') ;
		IF hDC <> NULL_HDC THEN
			EndPage (hDC) ;
			EndDoc (hDC) ;
			DeleteFonts ;
			DeleteDC (hDC) ;
			hDC := NULL_HDC ;
		END ;
	END ;
END GdiExit ;


BEGIN
FINALLY
	GdiExit ;
END GDIdruck.
