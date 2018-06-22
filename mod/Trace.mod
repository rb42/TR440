
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Trace;

(*  20.06.18    *)


FROM SYSTEM IMPORT
    ADR, CAST, FUNC, VA_START, 	UNREFERENCED_PARAMETER,
    %IF ADW %THEN VA_LIST, %ELSE ADDRESS, %END IsThread ;

FROM Strings IMPORT
    Append, Concat ;

FROM Conversions IMPORT
    StrToCard ;

FROM Environment IMPORT
    GetProgramName,
    GetSymbol ;

FROM FileFunc IMPORT
    ParseFileName,
    FileNameParts,
    File,
    OpenCreateFile,
    ReadWriteDenyWrite,
    FileLength,
    SetFilePos,
    WriteBlock,
    CloseFile ;

FROM FormatString IMPORT
    FormatStringEx ;

FROM Registry IMPORT
    LocalMachine,
    CurrentUser,
    InitRegistry,
    ReadRegCard,
    ReadRegString ;

FROM SysClock IMPORT
    DateTime, GetClock ;

FROM WIN32 IMPORT
    HWND, LPARAM, BOOL,
    GetCurrentThreadId,
    GetTickCount,
    CRITICAL_SECTION, InitializeCriticalSection, DeleteCriticalSection,
    EnterCriticalSection, LeaveCriticalSection,
%IF Bits64 %THEN
    DWORD_PTR,
%END
    OutputDebugString ;

FROM WINUSER IMPORT
    WM_COPYDATA,
    COPYDATASTRUCT,
    EnumWindows,
    GetProp,
    SendMessageTimeout,
    SMTO_NORMAL,
    MessageBox, MB_OK, MB_ICONINFORMATION ;

FROM WINX IMPORT
%IF %NOT Bits64 %THEN
    NULL_HWND,
    NULL_HANDLE,
%END
    Instance
     ;


CONST
    TraceWindowProp =	'SEBtracer_TRACEWIN' ;

    cTraceDatei =       'C:\log\SEBtrace.log' ;

    EnvPfad =		'Software\SEB\SEBtrace' ;
    EnvSymbol =		'SEBtrace' ;
    EnvTraceDatei =	'SEBtraceFile' ;

    TAB : ARRAY OF CHAR =   { CHR(9), 0C } ;

    Trenner =       ': ' ;

    crlf : ARRAY OF CHAR =  {CHR(13), CHR(10), 0C} ;


TYPE
    string =    ARRAY [0..2047] OF CHAR ;


VAR
    Initialisiert :	BOOLEAN = FALSE ;

    TraceSem :		CRITICAL_SECTION ;
    SemInitialisiert :	BOOLEAN = FALSE ;

    mitAnhalten :	BOOLEAN = FALSE ;

    TraceWindow :	HWND = %IF Bits64 %THEN NIL %ELSE NULL_HWND %END ;

    ProgramName :	ARRAY [0..63] OF CHAR ;

    zwStr :		string ;

    datei :		File ;



PROCEDURE mitTrace() : BOOLEAN [INLINE] ;
BEGIN
    IF NOT Initialisiert THEN
        Initialisieren ;
    END ;
    RETURN TraceAktiv ;
END mitTrace ;


PROCEDURE TraceF (formatStr : ARRAY OF CHAR (* %s für String %c für CARDINAL %h für Hex-CARDINAL %i für INTEGER %l für LONGINT \t für TAB *)

        ) %IF %NOT DLL %THEN [RightToLeft, LEAVES, VARIABLE] %END ; (* beliebige Anzahl Parameter dahinter *)
    VAR addr :  %IF ADW %THEN VA_LIST %ELSE ADDRESS %END ;
BEGIN
    IF mitTrace() THEN
        IF SemInitialisiert THEN
            EnterCriticalSection (TraceSem) ;
        END ;
        VA_START (addr) ;
        IF NOT FormatStringEx (formatStr, zwStr, addr) THEN
            zwStr := 'TraceF-Fehler : ' ;
            Append (formatStr, zwStr) ;
        END ;
        SendTraceMessage (zwStr) ;
        IF SemInitialisiert THEN
            LeaveCriticalSection (TraceSem) ;
        END ;
    END ;
END TraceF ;


PROCEDURE Vorbereiten ;
    VAR name :  ARRAY [0..255] OF CHAR ;
        parts : FileNameParts ;
BEGIN
    IF NOT SemInitialisiert THEN
            InitializeCriticalSection (TraceSem) ;
        SemInitialisiert := TRUE ;
    END ;
    EnterCriticalSection (TraceSem) ;
    GetProgramName (name) ;
    ParseFileName (name, parts) ;
    ProgramName := parts.name ;
    LeaveCriticalSection (TraceSem) ;
END Vorbereiten ;


PROCEDURE FindTraceWindow ;
BEGIN
    FUNC EnumWindows (EnumProc, 0) ;
END FindTraceWindow ;


PROCEDURE DateiTrace (txt : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
    OpenCreateFile (datei, TraceDatei, ReadWriteDenyWrite) ;
    IF datei.status = 0 THEN
        SetFilePos (datei, FileLength(datei)) ;
        WriteBlock (datei, %IF ADW %THEN ADR( %END txt %IF ADW %THEN ) %END , LENGTH(txt)) ;
        IF datei.status = 0 THEN
            CloseFile (datei) ;
            RETURN TRUE ;
        END ;
        CloseFile (datei) ;
    END ;
    RETURN FALSE ;
END DateiTrace ;


PROCEDURE Initialisieren ;
    VAR str :   ARRAY [0..31] OF CHAR ;
        i : CARDINAL ;
BEGIN
    Initialisiert := TRUE ;
    Vorbereiten ;       (* hier nach ist TraceSem erst verlaesslich da *)
    EnterCriticalSection (TraceSem) ;
    IF GetSymbol (EnvSymbol, str) THEN
        IF StrToCard (str, i) THEN
            IF i <> 0 THEN
                SEBtrace := SEBtrace BOR i ;
                TraceErlaubt := TRUE ;
            END ;
        END ;
    END ;
    IF TraceErlaubt THEN
        IF SEBtrace BAND 1024 <> 0 THEN
            TraceFuerDebug := TRUE ;
        ELSIF SEBtrace BAND 512 <> 0 THEN
            TraceInDatei := TRUE ;
        END ;
        IF TraceInDatei THEN
            IF TraceDatei [0] <= ' ' THEN
                TraceDatei := cTraceDatei ;
            END ;
            IF DateiTrace (crlf) THEN
                TraceAktiv := TRUE ;
            END ;
        ELSIF TraceFuerDebug THEN
            TraceAktiv := TRUE ;
        ELSE
            FindTraceWindow ;   (* hier wird TraceAktiv=TRUE, wenn Trace-Window gefunden *)
        END ;
    END ;
    LeaveCriticalSection (TraceSem) ;
END Initialisieren ;


PROCEDURE InitTrace ;
BEGIN
    TraceErlaubt := TRUE ;
    Initialisiert := FALSE ;
    TraceF ('Init Trace %c', SEBtrace) ;
    IF TraceAktiv THEN
        IF SEBtrace = 0 THEN
            SEBtrace := 1 ;
        ELSIF SEBtrace BAND 256 <> 0 THEN
            mitAnhalten := TRUE ;
        END ;
    ELSE        (* Aktivierung misslungen *)
        SEBtrace := 0 ;
    END ;
END InitTrace ;


PROCEDURE ExitTrace ;
BEGIN
    TraceAktiv := FALSE ;
END ExitTrace ;


PROCEDURE AppendZahl (zahl, AnzahlZiffern : CARDINAL ; VAR INOUT str : ARRAY OF CHAR) ;
    VAR index : CARDINAL ;
BEGIN
    index := LENGTH (str) + AnzahlZiffern ;
    IF index <= HIGH (str) THEN
        str [index] := 0C ;
        DEC (index) ;
        WHILE AnzahlZiffern > 0 DO
            str [index] := CHR (zahl MOD 10 + ORD ('0')) ;
            zahl := zahl DIV 10 ;
            DEC (index) ;
            DEC (AnzahlZiffern) ;
        END ;
    END ;
END AppendZahl ;


PROCEDURE AppendZif2 (zahl : CARDINAL ; VAR INOUT str : ARRAY OF CHAR) ;
BEGIN
    AppendZahl (zahl, 2, str) ;
END AppendZif2 ;


PROCEDURE SendTraceMessage (str : ARRAY OF CHAR) ;
    VAR tCDS :  COPYDATASTRUCT ;
        msg :   string ;
%IF Bits64 %THEN
	lr :	DWORD_PTR ;
%ELSE
        lr :    CARDINAL ;
%END
        now :   DateTime ;
BEGIN
    Concat (ProgramName, Trenner, msg) ;            (* EXEname *)

    IF NOT TraceFuerDebug THEN

        AppendZahl (Instance:CARDINAL32, 5, msg) ;  (* Instance *)

        Append (Trenner, msg) ;
        AppendZahl (GetCurrentThreadId(), 5, msg) ; (* ThreadID *)

        Append (Trenner, msg) ;
        GetClock (now) ;
        AppendZif2 (now.year, msg) ;
        AppendZif2 (now.month, msg) ;
        AppendZif2 (now.day, msg) ;         (* Date *)

        Append (Trenner, msg) ;
        AppendZif2 (now.hour, msg) ;
        AppendZif2 (now.minute, msg) ;
        AppendZif2 (now.second, msg) ;          (* Time *)

        Append (Trenner, msg) ;
        AppendZahl (GetTickCount(), 3, msg) ;       (* MilliSekunden *)

        Append (Trenner, msg) ;
    END ;

    Append (TAB, msg) ;
    Append (str, msg) ;                 (* Text *)

    IF TraceInDatei THEN
        Append (crlf, msg) ;
        FUNC DateiTrace (msg) ;
    ELSIF TraceFuerDebug THEN
        Append (crlf, msg) ;
        OutputDebugString (msg) ;
    ELSE
        tCDS.dwData := 0 ;
        tCDS.cbData := LENGTH (msg) ;
        tCDS.lpData := ADR(msg) ;

        lr := SendMessageTimeout (TraceWindow, WM_COPYDATA, 0, CAST(LPARAM,ADR(tCDS)), SMTO_NORMAL, 5000, lr) ;
    END ;

    IF mitAnhalten THEN
        FUNC MessageBox (hWnd:HWND, 'Programm angehalten - weiter mit OK', 'SEBtrace', MB_OK BOR MB_ICONINFORMATION) ;
    END ;
END SendTraceMessage ;


PROCEDURE IsTraceWindow (hWnd : HWND) : BOOLEAN ;
BEGIN
    RETURN GetProp(hWnd, TraceWindowProp) <> %IF Bits64 %THEN NIL %ELSE NULL_HANDLE %END ;
END IsTraceWindow ;



PROCEDURE EnumProc (hWnd : HWND ; lParam : LPARAM) : BOOL [EXPORT, %IF ADW %THEN WINDOWS %ELSE WIN32SYSTEM %END ] ;
BEGIN
    UNREFERENCED_PARAMETER(lParam);
    IF IsTraceWindow (hWnd) THEN
        TraceWindow := hWnd ;
        TraceAktiv := TRUE ;
        RETURN FALSE ;
    END ;
    RETURN TRUE ;   (* weiter suchen in Enumeration *)
END EnumProc ;


PROCEDURE Init ;
    VAR i : CARDINAL ;
BEGIN
    IF InitRegistry (LocalMachine, EnvPfad, FALSE)
    OR InitRegistry (CurrentUser, EnvPfad, FALSE) THEN
        IF ReadRegCard (EnvSymbol, i) THEN
            SEBtrace := SEBtrace BOR i ;
            IF SEBtrace <> 0 THEN
                IF (TraceDatei [0] <= ' ') AND NOT ReadRegString (EnvTraceDatei, TraceDatei) THEN
                    TraceDatei := cTraceDatei ;
                END ;
                InitTrace ;
            END ;
        END ;
    END ;
END Init ;


PROCEDURE Term ;
BEGIN
    (*
    ExitTrace ;
    *)
    IF SemInitialisiert THEN
        SemInitialisiert := FALSE ;
        DeleteCriticalSection (TraceSem) ;
    END ;
END Term ;



BEGIN
    IF NOT IsThread THEN
        Init ;
    END ;
FINALLY
    IF NOT IsThread THEN
        Term;
    END;

END Trace.
