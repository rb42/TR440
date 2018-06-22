
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)
(*											*)
(*	This Module is base on the Work of Stony Brook Modula-2 from 2002		*)
(*											*)
(****************************************************************************************)


IMPLEMENTATION MODULE Terminal;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADDRESS, ADR, CAST, UNREFERENCED_PARAMETER, IsThread,
%IF ADW %THEN
	VA_LIST,
%END
    VA_START ;

FROM ASCII IMPORT
    bs, ht, cr, lf;

IMPORT WIN32, WINX, WINUSER ;

FROM WINUSER IMPORT
	WNDCLASS, RegisterClass, CreateWindowEx, SetCaretPos,
	TranslateMessage, DispatchMessage, DefWindowProc,
	WS_OVERLAPPEDWINDOW, MSG, MINMAXINFO, SetWindowPos,
	CS_VREDRAW, CS_HREDRAW, MessageBox, ShowWindow, GetDC,
	PAINTSTRUCT, BeginPaint, EndPaint, WM_PAINT, UpdateWindow,
	GetMessage, MessageBeep, IsIconic, DestroyWindow,
	ReleaseDC,
	InvalidateRect,
	WM_CHAR, WM_KEYDOWN, WM_GETMINMAXINFO, WM_APP, WM_COMMAND,
	ScrollWindow, IntersectRect, IDC_ARROW, LoadCursor,
	WM_SETFOCUS, WM_KILLFOCUS, CreateCaret, ShowCaret,
	DestroyCaret, CW_USEDEFAULT, GetWindowRect, GetClientRect,
	SW_SHOW, SW_SHOWMAXIMIZED,
	PostQuitMessage, WM_DESTROY, SWP_NOMOVE, SWP_NOZORDER,
	SendMessage, HideCaret, LoadIcon, IDI_APPLICATION,
	GetKeyState, VK_CONTROL, VK_MENU, VK_SHIFT, VK_DELETE,
	OpenClipboard, GetClipboardData, CF_TEXT, CloseClipboard,
	SIZE_RESTORED, SIZE_MAXIMIZED, WM_CLOSE,
	SetWindowText, WM_SIZE,
	WS_MAXIMIZE,
	VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT,
	VK_CANCEL, VK_HOME, VK_END,
	GetSystemMetrics, SM_CXSCREEN, SM_CYSCREEN, SM_CXMAXIMIZED, SM_CYMAXIMIZED ;

FROM WIN32 IMPORT
	HWND, LPTSTR, LPARAM, WPARAM, UINT, LRESULT, RECT, HDC, HANDLE,
	HFONT, HGDIOBJ,
	GlobalLock, GlobalSize, GlobalUnlock,
	Sleep ;

FROM WINX IMPORT
	NIL_RECT, NULL_HWND, NULL_HMENU, NULL_HINSTANCE, NULL_HANDLE,
	NULL_HBITMAP, Instance, GetStockBrush;

FROM WINGDI IMPORT
	TextOut, WHITE_BRUSH, SelectObject, CreateFont,
	FW_NORMAL, FF_DONTCARE, FIXED_PITCH, ANSI_CHARSET,
	OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	TEXTMETRIC, GetTextMetrics ;

FROM FormatString IMPORT
	FormatStringEx ;

IMPORT Threads, Strings ;

TYPE
    PMINMAXINFO         = POINTER TO MINMAXINFO;

CONST
	MAX_CHARS   	= 2047;

	WM_CARETMESSAGE	= WM_APP + 1;
	WM_TERMINATE	= WM_APP + 2;

	SHOW_CARET	= 0;
	HIDE_CARET	= 1;

	cMaxZeile	= 127 ;
	cMaxSpalte	= 511 ;

	StrgV =		CHR(16H) ;

	EinTrenner =	cr ;

VAR
	Class       	: WNDCLASS;
	MainWindow  	: HWND;
	TM          	: TEXTMETRIC;
	DC          	: HDC;
	CharHeight  	: CARDINAL;
	CharWidth   	: CARDINAL;
	Leading     	: CARDINAL;
	X,Y         	: CARDINAL;
	DX, DY      	: CARDINAL;
	Screen      	: ARRAY [0..cMaxZeile], [0..cMaxSpalte] OF CHAR ;
	KeyBuf      	: ARRAY [0..MAX_CHARS-1] OF CHAR;
	kBegin      	: CARDINAL;
	kEnd        	: CARDINAL;
	HaveFocus   	: BOOLEAN;
	client      	: RECT;
	wind        	: RECT;

	Font		: HFONT ;

	BufferLock  	: Threads.CriticalSection;
	WriteLock   	: Threads.CriticalSection;
	MessageThread 	: Threads.Thread;
	CharInBuf   	: Threads.SignalSem;

	InitDone    	: Threads.EventSem;

	zusatz		: CARDINAL = 0 ;

	WartenAufEingabe,
	TermInited  	: BOOLEAN;

	WindowBreite,
	WindowHoehe 	: CARDINAL ;

	zwStr		: ARRAY [0..8191] OF CHAR ;

	lastInput	: ARRAY [0..cMaxSpalte] OF CHAR ;

	EinMerker	: ARRAY [0..2047] OF CHAR ;
	LastEinIndex	: CARDINAL = 0 ;

	AktEinIndex	: INTEGER ;
	AktEinAnfang	: CARDINAL ;



PROCEDURE InitInputBuffer ;
BEGIN
	EinMerker := '' ;
	LastEinIndex := 0 ;
	AktEinIndex := 1 ;
	AktEinAnfang := 0 ;
END InitInputBuffer ;


PROCEDURE SetEinElement (nr : INTEGER) : BOOLEAN ;
	VAR	i,
		lng :	CARDINAL ;
BEGIN
	AktEinIndex := 1 ;
	AktEinAnfang := 0 ;
	IF nr = 1 THEN
		RETURN EinMerker [0] <> 0C ;
	END ;
	lng := LENGTH (EinMerker) ;
	FOR i := 1 TO lng DO
		IF EinMerker [i-1] = EinTrenner THEN
			INC (AktEinIndex) ;
			IF nr = AktEinIndex THEN
				AktEinAnfang := i ;
				RETURN EinMerker [i] <> 0C ;
			END ;
		END ;
	END ;
	RETURN FALSE ;
END SetEinElement ;


PROCEDURE EingabeMerken (buf : ARRAY OF CHAR) ;
	VAR	ind,
		anf,
		end,
		lng,
		lnggesamt :	CARDINAL ;
BEGIN
	lng := LENGTH (buf) ;
	lnggesamt := LENGTH (EinMerker) ;
	ind := 1 ;
	WHILE SetEinElement (ind) DO
		anf := AktEinAnfang ;
		SetEinElement (ind+1) ;
		end := AktEinAnfang-2 ;
		IF Strings.Equal (buf, EinMerker [anf .. end]) THEN			(* Inhalt schon enthalten *)
			EinMerker [anf] := 0C ;
			Strings.Append (EinMerker [end+2 .. lnggesamt], EinMerker) ;	(* altes Element löschen, um es hinten dran zu hängen *)
		END ;
		ind := ind + 1 ;
	END ;
	WHILE lnggesamt + lng + 1 > HIGH (EinMerker) DO					(* ältestes Element löschen *)
		SetEinElement (2) ;
		EinMerker := EinMerker [AktEinAnfang .. lnggesamt] ;
		lnggesamt := lnggesamt-AktEinAnfang ;
	END ;
	Strings.Append (buf, EinMerker) ;
	Strings.Append (cr, EinMerker) ;
	SetEinElement (1000000000) ;
END EingabeMerken ;


PROCEDURE EingabeSuchen (richtung : INTEGER ; VAR buf : ARRAY OF CHAR) ;
	VAR	ind,
		anf,
		end :	INTEGER ;
BEGIN
	buf := '' ;
	ind := AktEinIndex + richtung ;
	IF ind < 1 THEN
		ind := 1000000000 ;
	END ;
	IF NOT SetEinElement (ind) THEN
		IF richtung < 0 THEN
			ind := AktEinIndex-1 ;
		ELSE
			ind := 1 ;
		END ;
		IF NOT SetEinElement (ind) THEN
			RETURN ;
		END ;
	END ;
	anf := AktEinAnfang ;
	SetEinElement (ind+1) ;
	end := AktEinAnfang-2 ;
	buf := EinMerker [anf..end] ;
	AktEinIndex := ind ;
END EingabeSuchen ;


PROCEDURE CaretMessage(msg : WPARAM);
BEGIN
	IF HaveFocus AND WartenAufEingabe THEN
		SendMessage(MainWindow, WM_CARETMESSAGE, msg, 0);
	END;
END CaretMessage;


PROCEDURE WaitForChar();
BEGIN
	WartenAufEingabe := TRUE ;
	CaretMessage(SHOW_CARET);
	SetCaretPos(X*CharWidth, Y*CharHeight);
	Threads.WaitForSignalSem(CharInBuf, Threads.SemWaitForever);
	CaretMessage(HIDE_CARET);
	WartenAufEingabe := FALSE ;
END WaitForChar;


PROCEDURE CursorZeigen ;
BEGIN
	WartenAufEingabe := TRUE ;
	CaretMessage(SHOW_CARET);
	SetCaretPos(X*CharWidth, Y*CharHeight);
END CursorZeigen ;


PROCEDURE CursorVerbergen ;
BEGIN
	CaretMessage(HIDE_CARET);
	WartenAufEingabe := FALSE ;
END CursorVerbergen ;


PROCEDURE AddChar(ch : CHAR);
BEGIN
	Threads.EnterCriticalSection(BufferLock);

	EingabeVorhanden := TRUE ;

	IF kEnd = kBegin THEN
		MessageBeep(0ffffh);
	ELSE
		Threads.SendSignalSem(CharInBuf, 1);

		KeyBuf[kEnd - 1] := ch;
		INC(kEnd);
		IF kEnd = MAX_CHARS + 1 THEN
			kEnd := 1;
		END;
	END;

	Threads.LeaveCriticalSection(BufferLock);
END AddChar;



PROCEDURE SimulateInput (str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO LENGTH (str) DO
		AddChar (str [i-1]) ;
	END ;
END SimulateInput ;


PROCEDURE GetChar() : CHAR;
VAR
    ch  : CHAR;
BEGIN
	IF Unterdruecken THEN
		RETURN 0C ;
	END ;

	Threads.EnterCriticalSection(BufferLock);

	IF (kEnd > kBegin) AND (kEnd - 1 = kBegin) THEN
		EingabeVorhanden := FALSE ;
		Threads.LeaveCriticalSection(BufferLock);
		RETURN 0c;
	ELSE
		ch := KeyBuf[kBegin];
		INC(kBegin);
		IF kBegin = MAX_CHARS THEN
			kBegin := 0;
		END;
		Threads.LeaveCriticalSection(BufferLock);
		RETURN ch;
	END;
END GetChar;


PROCEDURE ClearInput ;
BEGIN
	Threads.EnterCriticalSection(BufferLock);
	kBegin := 0 ;
        kEnd := 1 ;
	EingabeVorhanden := FALSE ;
	Threads.LeaveCriticalSection(BufferLock);
END ClearInput ;


PROCEDURE SaveZeile (zeile : CARDINAL) ;
	VAR	maxSpalt,
		i, j :	CARDINAL ;
		puf,
		puf2 :	ARRAY [0..cMaxSpalte+1] OF CHAR ;
BEGIN
	IF ProtokollAngemeldet THEN
		maxSpalt := 3FFFH ;
		FOR i := 0 TO MaxSpalte DO			(* erstmal definiertes Zeilenende suchen *)
			CASE Screen [zeile, i] OF
			0C,
			' ',
			cr,
			lf,
			ht :
				puf [i] := ' ' ;
			ELSE
				puf [i] := Screen [zeile, i] ;
				maxSpalt := i ;
			END ;
		END ;
		IF maxSpalt <> 3FFFH THEN	(* keine Leerzeile *)
			FOR i := 7 TO maxSpalt BY 8 DO			(* dann Tabulatoren einsetzen, wenn möglich *)
				IF (puf [i] = ' ') AND (puf [i-1] <= ' ') THEN
					puf [i] := ht ;
					j := i-1 ;
					WHILE puf [j] = ' ' DO
						puf [j] := 0C ;
						IF j > 0 THEN
							j := j - 1 ;
						ELSE
							BREAK ;
						END ;
					END ;
				END ;
			END ;
		END ;
		j := 0 ;
		IF maxSpalt <> 3FFFH THEN			(* keine Leerzeile *)
			FOR i := 0 TO maxSpalt DO		(* nun komprimieren *)
				IF puf [i] <> 0C THEN
					puf2 [j] := puf [i] ;
					j := j + 1 ;
				END ;
			END ;
		END ;
		puf2 [j] := 0C ;
		Protokoll (puf2) ;				(* ins Protokoll ausgeben *)
	END ;
END SaveZeile ;


(* Write to the window, teletype style *)

PROCEDURE OutPutCh(ch : CHAR);
TYPE
    AOC     = ARRAY [0..0] OF CHAR;
VAR
    DC  : HDC;
    i   : CARDINAL;
BEGIN
    IF MainWindow <> NULL_HWND THEN
        IF ch = cr THEN
            X := 0;
        ELSIF ch = lf THEN
            Y := Y + 1;
            IF Y > MaxZeile THEN
	    	SaveZeile (MaxZeile) ;
                FOR i := 0 TO MaxZeile-1 DO
                    Screen[i] := Screen[i+1];
                END;
                FOR i := 0 TO MaxSpalte DO
                    Screen[MaxZeile][i] := ' ';
                END;
                ScrollWindow(MainWindow, 0,
                    -INT(CharHeight),
                    NIL_RECT, NIL_RECT);
                UpdateWindow(MainWindow);
                Y := MaxZeile;
            END;
        ELSIF ch = bs THEN
            IF X > 0 THEN
                X := X - 1;
            END;
        ELSIF ch = ht THEN
            REPEAT
                X := X + 1;
            UNTIL X REM 8 = 0;
            IF X > MaxSpalte THEN
                X := MaxSpalte;
            END;
        ELSIF X <= MaxSpalte THEN
            DC := GetDC(MainWindow);
        SelectObject(DC, CAST (HGDIOBJ, Font));
	    TextOut(DC, X*CharWidth, Y*CharHeight+Leading,
                        CAST(AOC, ch), 1);
            ReleaseDC(MainWindow, DC);
            Screen[Y, X] := ch;
            X := X + 1;
        ELSIF X >= MaxSpalte THEN
            X := 0;
            Y := Y + 1;
            IF Y > MaxZeile THEN
	    	SaveZeile (MaxZeile) ;
                FOR i := 0 TO MaxZeile-1 DO
                    Screen[i] := Screen[i+1];
                END;
                FOR i := 0 TO MaxSpalte DO
                    Screen[MaxZeile][i] := ' ';
                END;
                ScrollWindow(MainWindow, 0,
                    -INT(CharHeight),
                    NIL_RECT, NIL_RECT);
                UpdateWindow(MainWindow);
                Y := MaxZeile;
            END;
            DC := GetDC(MainWindow);
        SelectObject(DC, CAST (HGDIOBJ, Font));
            TextOut(DC, X*CharWidth, Y*CharHeight+Leading,
	                        CAST(AOC, ch), 1);
            ReleaseDC(MainWindow, DC);
            Screen[Y, X] := ch;
            X := X + 1;
        END;
    END;
END OutPutCh;


PROCEDURE Write(ch : CHAR);
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;

    IF NOT TermInited THEN
        TermInit;
    END;

	IF msPause <> 0 THEN
		Sleep (msPause) ;
	END ;

    Threads.EnterCriticalSection(WriteLock);

    OutPutCh(ch);

    Threads.LeaveCriticalSection(WriteLock);

	IF WriteExtAngemeldet THEN
		WriteExt (ch) ;
	END ;

END Write;


PROCEDURE OutPutString(str : ARRAY OF CHAR);
TYPE
    AOC     = ARRAY [0..0] OF CHAR;
VAR
    DC          : HDC;
    ch          : CHAR;
    i           : CARDINAL;
    j           : CARDINAL;
    l           : CARDINAL;
    ctrlChars   : BOOLEAN;
BEGIN
    IF MainWindow = NULL_HWND THEN
        RETURN;
    END;

    l := LENGTH(str);
    IF l = 0 THEN
        RETURN;
    END;

    DC := GetDC(MainWindow);
        SelectObject(DC, CAST (HGDIOBJ, Font));

    ctrlChars := FALSE;
    i := 0;
    WHILE i < l DO
        IF (str[i] = cr) OR
           (str[i] = lf) OR
           (str[i] = bs) OR
           (str[i] = ht)
        THEN
            ctrlChars := TRUE;
            i := l;
        END;
        INC(i);
    END;

	IF Y > MaxZeile THEN
	    FOR j := 0 TO MaxZeile-1 DO
		Screen[j] := Screen[j+1];
	    END;
	    FOR j := 0 TO MaxSpalte DO
		Screen[MaxZeile][j] := ' ';
	    END;
	    ScrollWindow(MainWindow, 0, -INT(CharHeight),
			 NIL_RECT, NIL_RECT);
	    Y := MaxZeile;
	    MakeRepaintWindow(MainWindow) ;
    ELSIF (NOT ctrlChars) AND (X + l <= MaxSpalte) THEN
        Screen[Y][X..X+l-1] := str;
       	TextOut(DC, X*CharWidth, Y*CharHeight+Leading, str, l);
        X := X + l;
    ELSE
        i := 0;
        WHILE (i <= HIGH(str)) AND (str[i] <> CHR(0)) DO
            ch := str[i];

            IF ch = cr THEN
                X := 0;
            ELSIF ch = lf THEN
                Y := Y + 1;
                IF Y > MaxZeile THEN
		    SaveZeile (MaxZeile) ;
                    FOR j := 0 TO MaxZeile-1 DO
                        Screen[j] := Screen[j+1];
                    END;
                    FOR j := 0 TO MaxSpalte DO
                        Screen[MaxZeile][j] := ' ';
                    END;
                    ScrollWindow(MainWindow, 0, -INT(CharHeight),
                                 NIL_RECT, NIL_RECT);
                    UpdateWindow(MainWindow);
                    Y := MaxZeile;
        	END;
            ELSIF ch = bs THEN
                IF X > 0 THEN
                    X := X - 1;
                END;
            ELSIF ch = ht THEN
                REPEAT
                    X := X + 1;
                UNTIL X REM 8 = 0;
                IF X > MaxSpalte THEN
                    X := MaxSpalte;
                END;
            ELSIF X <= MaxSpalte THEN
               	TextOut(DC, X*CharWidth, Y*CharHeight+Leading,
                            CAST(AOC, ch), 1);
                Screen[Y, X] := ch;
                X := X + 1;
            ELSIF X > MaxSpalte THEN
                X := 0;
                Y := Y + 1;
                IF Y > MaxZeile THEN
		    SaveZeile (MaxZeile) ;
                    FOR j := 0 TO MaxZeile-1 DO
                        Screen[j] := Screen[j+1];
                    END;
                    FOR j := 0 TO MaxSpalte DO
                        Screen[MaxZeile][j] := ' ';
                    END;
                    ScrollWindow(MainWindow, 0,
                        -INT(CharHeight),
                        NIL_RECT, NIL_RECT);
                    UpdateWindow(MainWindow);
                    Y := MaxZeile;
                END;
                TextOut(DC, X*CharWidth, Y*CharHeight+Leading,
                            CAST(AOC, ch), 1);
                Screen[Y, X] := ch;
                X := X + 1;
            END;

            INC(i);
        END;
    END;

    ReleaseDC(MainWindow, DC);
END OutPutString;


PROCEDURE WriteCard (n : CARDINAL) ;
BEGIN
	IF n > 9 THEN
		WriteCard (n DIV 10) ;
		n := n MOD 10 ;
	END ;
	Write (CHR(n + ORD('0'))) ;
END WriteCard ;

PROCEDURE WriteLongCard (n : LONGCARD) ;
BEGIN
	IF n > 9 THEN
		WriteLongCard (n DIV 10) ;
		n := n MOD 10 ;
	END ;
	Write (CHR(ORD(n) + ORD('0'))) ;
END WriteLongCard ;

PROCEDURE WriteLn;
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;

    IF NOT TermInited THEN
        TermInit;
    END;

    Write(cr);
    Write(lf);

END WriteLn;

PROCEDURE WriteString(s : ARRAY OF CHAR);
	VAR	i :	CARDINAL ;
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;

    IF NOT TermInited THEN
        TermInit;
    END;

	IF (msPause <> 0) OR WriteExtAngemeldet THEN
		FOR i := 1 TO LENGTH (s) DO
			Write (s [i-1]) ;
		END ;
		RETURN ;
	END ;

    Threads.EnterCriticalSection(WriteLock);

	IF (X + LENGTH(s)) > MaxSpalte THEN
		OutPutCh (cr);
		OutPutCh (lf);
	END ;

    (*
    i := 0;
    WHILE (i <= HIGH(s)) AND (s[i] <> CHR(0)) DO
        OutPutCh(s[i]);
        INC(i);
    END;
    *)
    OutPutString(s);
    Threads.LeaveCriticalSection(WriteLock);
END WriteString;

PROCEDURE Position(NewX, NewY : CARDINAL);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    X := NewX;
    Y := NewY;

    Threads.LeaveCriticalSection(WriteLock);
END Position;

PROCEDURE GetPosition(VAR getX, getY : CARDINAL);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    getX := X;
    getY := Y;

    Threads.LeaveCriticalSection(WriteLock);
END GetPosition;

PROCEDURE ClearScreen ;
VAR
    lX, lY        : CARDINAL;
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;

    IF TermInited THEN

    	FOR lX := X+1 TO cMaxSpalte DO
		Screen[Y,lX] := ' ' ;
	END ;
        FOR lY := Y+1 TO cMaxZeile DO
            FOR lX := 0 TO cMaxSpalte DO
                Screen[lY,lX] := ' ';
            END;
        END;
	MakeRepaintWindow (MainWindow);

    END;
END ClearScreen;


PROCEDURE CharAvail() : BOOLEAN;
BEGIN
	IF Unterdruecken THEN
		RETURN FALSE ;
	END ;

	IF NOT TermInited THEN
		TermInit;
	END;

	Threads.EnterCriticalSection(BufferLock);

	IF (kEnd > kBegin) AND (kEnd - 1 = kBegin) THEN
		Threads.LeaveCriticalSection(BufferLock);
		RETURN FALSE;
	ELSE
		Threads.LeaveCriticalSection(BufferLock);
		RETURN TRUE;
	END;
END CharAvail;


PROCEDURE ReadChar() : CHAR;
	VAR	WarteMerk :	BOOLEAN ;
BEGIN
	IF Unterdruecken THEN
		RETURN 0C ;
	END ;

	IF NOT TermInited THEN
		TermInit;
	END;

	IF MainWindow <> NULL_HWND THEN
		IF NOT CharAvail () THEN
			WarteMerk := imWarteZustand ;

			imWarteZustand := TRUE ;
			WaitForChar();
			imWarteZustand := WarteMerk ;
		END ;

		RETURN GetChar();
	ELSE
		RETURN 0c;
	END;
END ReadChar;


PROCEDURE Read(VAR OUT ch : CHAR);
BEGIN
	IF NOT TermInited THEN
		TermInit;
	END;

	ch := ReadChar();
END Read;


PROCEDURE Reset;
VAR
    X, Y        : CARDINAL;
BEGIN
    IF TermInited THEN
        FOR Y := 0 TO cMaxZeile DO
            FOR X := 0 TO cMaxSpalte DO
                Screen[Y,X] := ' ';
            END;
        END;
	MakeRepaintWindow (MainWindow);
        Position(0, 0);
    END;
END Reset;


(* Repaint the window *)

PROCEDURE RepaintWindow;
    VAR
        DC              : HDC;
        PS              : PAINTSTRUCT;
        i               : CARDINAL;
        LineRect        : RECT;
        Intersect       : RECT;
BEGIN
        DC := BeginPaint(MainWindow, PS);
        SelectObject(DC, CAST (HGDIOBJ, Font));
        LineRect.left := 0;
        LineRect.right := (MaxSpalte+1) * CharWidth-1;
        LineRect.top := Leading;
	LineRect.bottom := LineRect.top + INT(CharHeight) - 1 ;
        FOR i := 0 TO Y DO
	    IF IntersectRect(Intersect, PS.rcPaint, LineRect) THEN
	        TextOut(DC, 0, i*CharHeight+Leading, Screen[i], MaxSpalte);
	    END;
	    LineRect.top := LineRect.top + INT(CharHeight);
            LineRect.bottom := LineRect.bottom + INT(CharHeight);
        END;
        EndPaint(MainWindow, PS);
END RepaintWindow;


PROCEDURE DoKeyDown(wParam : WPARAM) : BOOLEAN;
VAR
    normal : BOOLEAN;
BEGIN
    normal := TRUE;

    IF (ORD(GetKeyState(VK_SHIFT)) BAND 08000h) = 08000h THEN
        normal := FALSE;
    END;

    IF (ORD(GetKeyState(VK_CONTROL)) BAND 08000h) = 08000h THEN
        normal := FALSE;
	IF wParam = VK_CANCEL THEN
		CtrlBreak := TRUE ;
	END ;
    END;

    IF (ORD(GetKeyState(VK_MENU)) BAND 08000h) = 08000h THEN
        normal := FALSE;
    END;

    IF normal THEN
        CASE wParam OF
        VK_UP:
            AddChar(CursorUp);
            RETURN TRUE;
        |
        VK_DOWN:
            AddChar(CursorDown);
            RETURN TRUE;
        |
        VK_LEFT:
            AddChar(CursorLeft);
            RETURN TRUE;
	|
	VK_DELETE:
	    AddChar(Delete);
        |
        VK_RIGHT:
            AddChar(CursorRight);
            RETURN TRUE;
        |
        VK_PRIOR:
            AddChar(PageUp);
            RETURN TRUE;
        |
        VK_NEXT:
            AddChar(PageDown);
            RETURN TRUE;
	|
	VK_HOME :
	    AddChar(Pos1) ;
	    RETURN TRUE ;
	|
	VK_END :
	    AddChar(Ende) ;
	    RETURN TRUE ;
        ELSE
        END;
    END;
    RETURN FALSE;
END DoKeyDown;


PROCEDURE WindowGroesse (hWnd : HWND ; lParam : LPARAM) ;
	VAR	WindowSizeX,
		WindowSizeY,
		zwX,
		zwY :		CARDINAL ;
		wind,
		client :	RECT ;
BEGIN
	GetWindowRect(hWnd, wind);
	GetClientRect(hWnd, client);
	DX := (wind.right - wind.left) - (client.right - client.left);
	DY := (wind.bottom - wind.top) - (client.bottom - client.top);
	WindowSizeY := ORD (lParam DIV 10000H) (* DIV modY * modY *) ;
	WindowSizeX := ORD (lParam MOD 10000H) (* DIV modX * modX *) ;


	IF CharWidth <> 0 THEN
		zwX := (GetSystemMetrics (SM_CXMAXIMIZED) (* - VAL (INTEGER, DX) *) ) DIV VAL (INTEGER, CharWidth) ;
		zwY := (GetSystemMetrics (SM_CYMAXIMIZED) (* - VAL (INTEGER, DY) *) ) DIV VAL (INTEGER, CharHeight) ;
		IF WindowSizeX > zwX * CharWidth THEN
			WindowSizeX := zwX * CharWidth ;
		END ;
		IF WindowSizeY > zwY * CharHeight THEN
			WindowSizeY := zwY * CharHeight ;
		END ;
		IF WindowSizeX > (cMaxSpalte+1) * CharWidth THEN
			WindowSizeX := (cMaxSpalte+1) * CharWidth ;
		END ;
		IF WindowSizeY > (cMaxZeile+1) * CharHeight THEN
			WindowSizeY := (cMaxZeile+1) * CharHeight ;
		END ;
		WindowHoehe := (WindowSizeY (* - DY *)) DIV CharHeight ;
		WindowBreite := (WindowSizeX (* - DX *)) DIV CharWidth ;
		MaxZeile := WindowHoehe-1 ;
		IF MaxZeile > cMaxZeile THEN
			MaxZeile := cMaxZeile ;
		END ;
		MaxSpalte := WindowBreite-1 ;
		IF Y > MaxZeile THEN
			Y := MaxZeile ;
		END ;
		IF X > MaxSpalte THEN
			X := MaxSpalte ;
		END ;
		WindowSizeX := (MaxSpalte + 1) * CharWidth ;
		WindowSizeY := (MaxZeile + 1) * CharHeight ;
	END ;
	SetWindowPos(hWnd, NULL_HWND, 0,0,WindowSizeX + DX,WindowSizeY + DY,
		     SWP_NOMOVE BOR SWP_NOZORDER);
	MakeRepaintWindow (MainWindow) ;
END WindowGroesse ;


PROCEDURE MakeRepaintWindow (hWnd : HWND) ;
BEGIN
	InvalidateRect (hWnd, NIL_RECT, TRUE) ;
	UpdateWindow (hWnd) ;
END MakeRepaintWindow ;



PROCEDURE MakeLineRepaint (hwnd : HWND ; von, bis : CARDINAL) ;
(*
	VAR	r :	RECT ;
*)
BEGIN
	DC := GetDC(hwnd);
	SelectObject(DC, CAST (HGDIOBJ, Font));
	TextOut(DC, X*CharWidth, Y*CharHeight+Leading, Screen [Y, von..bis], bis-von+1) ;
	ReleaseDC(hwnd, DC);
(*
	r.left := (von-1)*CharWidth ;
	r.top := (Y-1) * CharHeight ;
	r.right := (bis+1) * CharWidth ;
	r.bottom := (Y * CharHeight) ;
	InvalidateRect (hwnd, r, TRUE) ;
*)
	UpdateWindow (hwnd) ;
END MakeLineRepaint ;


(* interpret commands *)
PROCEDURE Commands(window:   WIN32.HWND;
                   wParam:   WIN32.WPARAM);
BEGIN
	CASE wParam DIV 10000H OF
	  0 :		IF MenueAngemeldet THEN
	  			MenueAufruf (window, wParam MOD 10000H) ;
			END ;
	| 1 :		(* Accelerator *)
	| 2 :		(* Control *)
	ELSE
	END; (* case *)
END Commands;


(* Our window proc.  Everything except paint messages are passed on *)

PROCEDURE TerminalWindowProc(hWnd : HWND;
                             wMsg : UINT;
                             wParam : WPARAM;
                             lParam : LPARAM) : LRESULT [EXPORT, %IF ADW %THEN WINDOWS %ELSE WIN32SYSTEM %END];
VAR
    pmmi        : PMINMAXINFO;
BEGIN
	CASE wMsg OF
	WM_PAINT:
		IF NOT IsIconic(hWnd) THEN
		    RepaintWindow;
		ELSE
		    RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
		END;
	|
	WM_CLOSE:
		AbbruchWunsch := TRUE ;
		IF Abbrechbar THEN
			PostQuitMessage(0);
			AddChar(0C);
		ELSE
			AddChar (Escape) ;
		END ;
	|
	WM_DESTROY:
		AbbruchWunsch := TRUE ;
		PostQuitMessage(0);

		AddChar(0C);
	|
	WM_COMMAND:
		Commands (hWnd, wParam) ;
		RETURN WINUSER.DefWindowProc(hWnd, wMsg, wParam, lParam);
	|
	WM_SETFOCUS:
		HaveFocus := TRUE;
		IF CharWidth <> 0 THEN
		    CreateCaret(hWnd, NULL_HBITMAP, CharWidth, CharHeight);
		    IF WartenAufEingabe THEN
			ShowCaret(hWnd);
			SetCaretPos(X*CharWidth, Y*CharHeight);
		    END ;
		END;
	|
	WM_KILLFOCUS:
		HaveFocus := FALSE;
		DestroyCaret;
	|
	WM_CHAR:
		AddChar(CHR(wParam));
	|
	WM_KEYDOWN:
		IF NOT DoKeyDown(wParam) THEN
		    RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
		END;
	|
	WM_GETMINMAXINFO:
		IF MainWindow <> NULL_HWND THEN
		    pmmi := CAST(PMINMAXINFO, lParam);
		    pmmi^.ptMaxSize.x := (cMaxSpalte+1) * CharWidth + DX;
		    pmmi^.ptMaxSize.y := (cMaxZeile+1) * CharHeight + DY;
		    pmmi^.ptMaxTrackSize.x := (cMaxSpalte+1) * CharWidth + DX;
		    pmmi^.ptMaxTrackSize.y := (cMaxZeile+1) * CharHeight + DY;
		    pmmi^.ptMinTrackSize.x := 32 * CharWidth + DX;
		    pmmi^.ptMinTrackSize.y := 4 * CharHeight + DY;
		END;
		RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
	|
	WM_CARETMESSAGE:
		IF wParam = SHOW_CARET THEN
		    ShowCaret(hWnd);
		    SetCaretPos(X*CharWidth, Y*CharHeight);
		ELSE
		    HideCaret(hWnd);
		END;
	|
	WM_SIZE :
		CASE wParam OF
		  SIZE_RESTORED,
		  SIZE_MAXIMIZED :
			WindowGroesse (hWnd, lParam) ;
		ELSE
			RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
		END ;

	|
	WM_TERMINATE:
		AbbruchWunsch := TRUE ;
		IF Abbrechbar THEN
			DestroyWindow(hWnd);
		END ;
	ELSE
		RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
	END;
	RETURN 0;
END TerminalWindowProc;


PROCEDURE StartWindow() : BOOLEAN;
VAR
	WindowSizeX,
	WindowSizeY :	CARDINAL ;
	ptr :		LPTSTR;
	ok  :		BOOLEAN;
BEGIN
	IF Unterdruecken THEN
		RETURN FALSE ;
	END ;

    ok := TRUE;

    (* Create an Terminal window *)

	IF Maximal THEN
		zusatz := WS_MAXIMIZE ;
	END ;

    Class.style := CS_VREDRAW + CS_HREDRAW;
    Class.lpfnWndProc := TerminalWindowProc ;
    Class.cbClsExtra := 0;
    Class.cbWndExtra := 0;
    Class.hInstance := Instance;
    ptr := WindowIcon;
    Class.hIcon := LoadIcon(NULL_HINSTANCE, ptr^);
	IF Class.hIcon = NIL THEN
	    ptr := IDI_APPLICATION;
	    Class.hIcon := LoadIcon(NULL_HINSTANCE, ptr^);
	END ;
    ptr := IDC_ARROW;
    Class.hCursor := LoadCursor(NULL_HINSTANCE, ptr^);
    Class.hbrBackground := GetStockBrush(WHITE_BRUSH);
    Class.lpszMenuName := NIL;
    Class.lpszClassName := ADR(WindowClass);

    RegisterClass(Class);

    Font := CreateFont (ZeichenHoehe, 0,
                                0, 0,
                                FW_NORMAL,
                                0, 0, 0,
                                ANSI_CHARSET,
                                OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                                FIXED_PITCH BOR FF_DONTCARE,
                                FontName) ;
    HaveFocus := FALSE;
    CharWidth := 0;
    MainWindow := CreateWindowEx(
           0,
           WindowClass,
           WindowTitle,
           WS_OVERLAPPEDWINDOW,
           CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
           NULL_HWND, NULL_HMENU,
           Instance,
           CAST(ADDRESS, 0));

	WND := MainWindow:CARDINAL ;

    IF MainWindow <> NULL_HWND THEN

        GetWindowRect(MainWindow, wind);
        GetClientRect(MainWindow, client);

        (* Get the text info *)

        DC := GetDC(MainWindow);

        SelectObject(DC, CAST (HGDIOBJ, Font));
        GetTextMetrics(DC, TM);
        CharHeight := TM.tmHeight + TM.tmExternalLeading;
        CharWidth := TM.tmAveCharWidth;

        ReleaseDC(MainWindow, DC);

	X := (GetSystemMetrics (SM_CXSCREEN) - 50) DIV VAL (INTEGER, CharWidth) ;
	Y := (GetSystemMetrics (SM_CYSCREEN) - 50) DIV VAL (INTEGER, CharHeight) ;
	IF MaxSpalte > X THEN
		MaxSpalte := X ;
	END ;
	IF MaxZeile > Y THEN
		MaxZeile := Y ;
	END ;

        DX := (wind.right - wind.left) - (client.right - client.left);
        DY := (wind.bottom - wind.top) - (client.bottom - client.top);
	IF CharWidth <> 0 THEN
		WindowSizeX := (MaxSpalte+1) * CharWidth ;
		WindowSizeY := (MaxZeile+1) * CharHeight ;
	END ;

        X := 0;
        Y := 0;
        Leading := TM.tmExternalLeading;

        SetWindowPos(MainWindow, NULL_HWND, 0, 0, WindowSizeX, WindowSizeY,
                     SWP_NOMOVE BOR SWP_NOZORDER);

	IF Maximal THEN
        	ShowWindow(MainWindow, SW_SHOWMAXIMIZED);
	ELSE
        	ShowWindow(MainWindow, SW_SHOW);
	END ;
        UpdateWindow(MainWindow);

        IF HaveFocus THEN
            SendMessage(MainWindow, WM_SETFOCUS, 0, 0);
        END;

    ELSE
        MessageBox(NULL_HWND, "CreateWindow failed", "sebag - Terminal-Fehler", 0);
        ok := FALSE;
    END;

	EingabeVorhanden := FALSE ;

    Threads.SetEventSem(InitDone);
    RETURN ok;
END StartWindow;


PROCEDURE UpdateWindowTitle ;
BEGIN
	SetWindowText (MainWindow, WindowTitle) ;
END UpdateWindowTitle ;


PROCEDURE FlushMessage(dummy : ADDRESS) : CARDINAL;
VAR
    msg : MSG;
BEGIN
    UNREFERENCED_PARAMETER(dummy);

    IF NOT StartWindow() THEN
        TermInited := FALSE;
        RETURN 1;
    END;

    WHILE GetMessage(msg, NULL_HWND, 0, 0) DO
        TranslateMessage(msg);
        DispatchMessage(msg);
    END;

    TermInited := FALSE;

    RETURN 0;
END FlushMessage;

PROCEDURE TermInit;
VAR
    X, Y        : CARDINAL;
    created     : BOOLEAN;
BEGIN
    (* Erase screen before doing the create *)

	IF Unterdruecken THEN
		RETURN ;
	END ;

    FOR Y := 0 TO cMaxZeile DO
        FOR X := 0 TO cMaxSpalte DO
            Screen[Y,X] := ' ';
        END;
    END;

    Threads.ResetSignalSem(CharInBuf);

    Threads.CreateEventSem(InitDone, "", created);
    Threads.CreateThread(MessageThread, FlushMessage, NIL, 0, FALSE);
    Threads.WaitForEventSem(InitDone, Threads.SemWaitForever);
    Threads.CloseEventSem(InitDone);
    TermInited := TRUE;
END TermInit;


PROCEDURE HoleClipboard (VAR ziel : ARRAY OF CHAR) ;
        VAR     lng :   CARDINAL ;
                mem :   HANDLE ;
                ptr :   POINTER TO ARRAY [0..2047] OF CHAR ;
BEGIN
	ziel := '' ;
        IF NOT OpenClipboard (MainWindow) THEN
                RETURN ;
        END ;
        mem := GetClipboardData (CF_TEXT) ;
        IF mem = NULL_HANDLE THEN
                CloseClipboard ;
                RETURN ;
        END ;
        ptr := GlobalLock (mem) ;
        lng := GlobalSize (mem) ;
        lng := LENGTH (ptr ^ [0..lng-1]) ;
	IF lng > HIGH(ziel) THEN
		lng := HIGH(ziel) ;
	END ;

	ziel := ptr ^ [0..lng-1] ;
	ziel [lng] := 0C ;
        GlobalUnlock (mem) ;
        CloseClipboard ;
END HoleClipboard ;


PROCEDURE WriteF (formatStr : ARRAY OF CHAR	(* %s für String %c für CARDINAL %h für Hex-CARDINAL %i für INTEGER %l für LONGINT \t für TAB *)

		) %IF %NOT DLL %THEN [RightToLeft, LEAVES, VARIABLE] %END ;	(* beliebige Anzahl Parameter dahinter *)
	VAR	addr :	%IF ADW %THEN VA_LIST %ELSE ADDRESS %END ;
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;

	IF NOT TermInited THEN
		TermInit;
	END;

	Threads.EnterCriticalSection(WriteLock);

	VA_START (addr) ;
	IF NOT FormatStringEx (formatStr, zwStr, addr) THEN
		zwStr := 'WriteF-Formatfehler : ' ;
		Strings.Append (formatStr, zwStr) ;
	END ;

	IF msPause <> 0 THEN

		Threads.LeaveCriticalSection(WriteLock);

		WriteString (zwStr) ;
		RETURN ;
	END ;

	IF (X + LENGTH(zwStr)) > MaxSpalte THEN
		OutPutCh (cr);
		OutPutCh (lf);
	END ;

	OutPutString(zwStr);

	Threads.LeaveCriticalSection(WriteLock);
END WriteF ;


PROCEDURE ClearRestLine ;
	VAR	i :	CARDINAL ;
BEGIN
	IF Unterdruecken THEN
		RETURN ;
	END ;
	FOR i := X TO MaxSpalte DO
		Screen [Y, i] := ' ' ;
	END ;
	DC := GetDC(MainWindow);
	SelectObject(DC, CAST (HGDIOBJ, Font));
	TextOut(DC, X*CharWidth, Y*CharHeight+Leading, Screen [Y, X..MaxSpalte], MaxSpalte-X+1) ;
	ReleaseDC(MainWindow, DC);
END ClearRestLine ;


PROCEDURE ReadString (VAR str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	xanf,
		xmax,
		i :		CARDINAL ;
		ch :		CHAR ;
		ReadEnde :	BOOLEAN ;
		buff :		ARRAY [0..8191] OF CHAR ;

	PROCEDURE GetInput () : BOOLEAN ;
		VAR	l :	CARDINAL ;
	BEGIN
		str := Screen [Y, xanf..xmax-1] ;
		lastInput := str ;
		l := LENGTH (lastInput) ;
		IF (ch <> Enter) THEN
			IF l > 2 THEN		(* irreguläres Eingabeendezeichen nicht mit speichern *)
				lastInput [l-2] := 0C ;
				l := l-2 ;
			ELSIF l > 1 THEN	(* irreguläres Eingabeendezeichen nicht mit speichern *)
				lastInput [l-1] := 0C ;
				l := l-1 ;
			END ;
		END ;
		IF l > 2 THEN
			EingabeMerken (lastInput) ;
		END ;
		RETURN xmax > xanf ;
	END GetInput ;

BEGIN
	CtrlBreak := FALSE ;
	buff [0] := 0C ;
	i := 0 ;
	str [0] := 0C ;
	IF Unterdruecken THEN
		RETURN FALSE ;
	END ;
	xanf := X ;
	xmax := X ;
	imWarteZustand := TRUE ;
	ReadEnde := FALSE ;
	LOOP
		IF buff [0] = 0C THEN
			Read (ch );
		ELSE
			ch := buff [0] ;
			buff := buff [1..HIGH(buff)] ;
			buff [HIGH(buff)] := 0C ;
		END ;
		IF AbbruchWunsch THEN
			imWarteZustand := FALSE ;
			RETURN FALSE ;
		END ;
		IF CtrlBreak THEN
			imWarteZustand := FALSE ;
			RETURN FALSE ;
		END ;
		IF ReadExtAngemeldet THEN
			ReadEnde := ReadExt (ch) ;
		END ;
		CASE ch OF
			  CursorLeft :	IF X > xanf THEN
						X := X - 1 ;
					END ;

			| CursorRight :	IF X < xmax THEN
						X := X + 1 ;
					END ;

			| CursorUp :	X := xanf ;
					xmax := X ;
					ClearRestLine ;
					EingabeSuchen (-1, buff) ;

			| Pos1 :	X := xanf ;

			| Ende :	X := xmax ;

			| CursorDown :	X := xanf ;
					xmax := X ;
					ClearRestLine ;
					EingabeSuchen (1, buff) ;

			| Delete :	Screen [Y, X..MaxSpalte-1] := Screen [Y, X+1..MaxSpalte] ;
					Screen [Y, MaxSpalte] := ' ' ;
					MakeLineRepaint (MainWindow, X, MaxSpalte) ;
					IF xmax > X THEN
						xmax := xmax - 1 ;
					END ;
			| BackSpace :	IF X > xanf THEN
						X := X - 1 ;
						IF xmax > X THEN
							xmax := xmax - 1 ;
						END ;
						Screen [Y, X..MaxSpalte-1] := Screen [Y, X+1..MaxSpalte] ;
						Screen [Y, MaxSpalte] := ' ' ;
						MakeLineRepaint (MainWindow, X, MaxSpalte) ;
					END ;
			| Escape :	GetInput ;
					imWarteZustand := FALSE ;
					RETURN FALSE ;
			| Enter :	ReadEnde := TRUE ;
			| StrgV :	HoleClipboard (buff) ;
			ELSE
				IF (ch >= ' ') AND (X < MaxSpalte-1) THEN
					Screen [Y, X+1..MaxSpalte] := Screen [Y, X..MaxSpalte-1] ;
					IF xmax > X THEN
						xmax := xmax+1 ;
					END ;
					Screen [Y, X] := ch ;
					MakeLineRepaint (MainWindow, X, MaxSpalte) ;
					X := X + 1 ;
					IF X > xmax THEN
						xmax := X ;
					END ;
				END ;
		END ;
		IF ReadEnde THEN
			IF buff [0] <> 0C THEN
				FOR i := 1 TO LENGTH(buff) DO
					AddChar (buff [i-1]) ;
				END ;
			END ;
			imWarteZustand := FALSE ;
			RETURN GetInput () ;
		END ;

	END ;
END ReadString ;


PROCEDURE GetMainWindow () : HWND ;
BEGIN
	RETURN WND:HWND ;
END GetMainWindow ;


PROCEDURE SendOSMessage(wnd : WIN32.HWND;
                        msg : WIN32.DWORD;
                        wp : WIN32.WPARAM;
                        lp : WIN32.LPARAM) : WIN32.LRESULT [INLINE];
BEGIN
    RETURN WINUSER.SendMessage(wnd, msg, wp, lp);
END SendOSMessage;


PROCEDURE SetWindowIconOS(w : WIN32.HWND ; smallicon, bigicon : WIN32.HICON);
BEGIN
	SendOSMessage(w, WINUSER.WM_SETICON, 0, CAST(WIN32.LPARAM, smallicon));
	SendOSMessage(w, WINUSER.WM_SETICON, 1, CAST(WIN32.LPARAM, bigicon));
END SetWindowIconOS;


PROCEDURE InitWindowIcons (small, big : ARRAY OF CHAR) ;
	VAR
		smallicon,
		bigicon :		WIN32.HICON ;
BEGIN
	smallicon := WINUSER.LoadIcon (WINX.Instance, small) ;
	bigicon := WINUSER.LoadIcon (WINX.Instance, big) ;
	SetWindowIconOS (MainWindow, smallicon, bigicon) ;
END InitWindowIcons ;



VAR
    created     : BOOLEAN;
BEGIN
    IF NOT IsThread THEN
        TermInited :=  FALSE;
	WartenAufEingabe := FALSE ;
	lastInput := '' ;
	EinMerker := '' ;
        MainWindow := NULL_HWND;
        kBegin := 0;
        kEnd := 1;
        X := 0;
        Y := 0;
        Threads.CreateCriticalSectionEx(BufferLock, 1000, TRUE);
        Threads.CreateCriticalSection(WriteLock);
        Threads.CreateSignalSem(CharInBuf, 1024, 0, "", created);
    END;

FINALLY
    IF NOT IsThread THEN
        IF TermInited THEN
	    Abbrechbar := TRUE ;
            SendMessage(MainWindow, WM_TERMINATE, 0, 0);
            Threads.SleepThread(100);
            Threads.CloseCriticalSection(BufferLock);
            Threads.CloseCriticalSection(WriteLock);
            Threads.CloseSignalSem(CharInBuf);
        END;
    END;
END Terminal.
