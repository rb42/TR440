
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Menue ;


(*	30.06.18	*)


%IF %NOT WEB %THEN

IMPORT SYSTEM ;
IMPORT WIN32 ;
IMPORT WINUSER ;
IMPORT WINX ;

IMPORT FileFunc, FileIO, Strings, RunProg, Registry, Environment ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

IMPORT upTextFrage, debug ;

FROM OFNup IMPORT * ;

FROM upChooseFont IMPORT * ;

IMPORT SSR_6 ;

FROM ASCII IMPORT
	cr ;

FROM Conversions IMPORT
	StrToCard ;

FROM Environment IMPORT
	GetProgramName ;

FROM Strings IMPORT
	Append,
	Equal ;

IMPORT Terminal ;
FROM Terminal IMPORT
	WND,
	FontName,
	MenueAngemeldet,
	MenueAufruf,
	SimulateInput,
	WriteF ;

FROM Struktur IMPORT * ;

FROM TR440hp IMPORT * ;

FROM GDIparm IMPORT * ;

FROM TASformat IMPORT
	TASformatierer ;

FROM Gebiet IMPORT
	upShowGebiete ;

IMPORT AsmDisasm ;
FROM AsmDisasm IMPORT
	TraceBefehl,
	TRIM ;

FROM Assembler IMPORT
	ListingDirectory,
	GetZahl ;

FROM AsmTrace IMPORT
	ExitTrace,
	BacktraceAnzahl,
	TraceDirectory ;

FROM Abwickler IMPORT
	SSRzaehlerZeigen ;

FROM AbwicklerUp IMPORT
	XABgefunden ;

FROM Programm IMPORT
	ProgNameExt,
	RegAlleZeigen,
	ProgrammPausieren,
	ProgrammFortsetzen ;

FROM upOKB IMPORT
	upGebietslageInDateilage ;

FROM DateiVerwaltung IMPORT
	Datentraeger,
	_STDDB,
	_OEFDB,
	UserTemporaer ;

FROM upDumpeDatei IMPORT
	KonvertTR440datei,
	KonvertTR440gebiet ;

FROM Trace IMPORT
	TraceF,
	InitTrace ;


CONST
	mnuSetFont			=  1 ;
	mnuSetBenutzer			=  2 ;
	mnuSetBKZ			=  3 ;
	mnuSetFKZ			=  4 ;
	mnuSetDatentraeger		=  5 ;
	mnuSetQuellbasis		=  6 ;
	mnuSetEditor			=  7 ;
	mnuSetDrucker			=  8 ;
	mnuSetDruckerFont		=  9 ;

	mnuZeigenQuelle			= 11 ;
	mnuZeigenProtokoll		= 12 ;
	mnuZeigenTrace			= 13 ;
	mnuZeigenKommando		= 14 ;
	mnuZeigenTR440datei		= 15 ;
	mnuZeigenTerminal		= 16 ;
	mnuZeigenAblaufProtokoll	= 17 ;
	mnuZeigenAblaufProtokollLoe	= 18 ;
	mnuZeigenTR440textDatei		= 19 ;
	mnuZeigenTR440gebiet		= 20 ;

	mnuTAS				= 21 ;
	mnuMerke			= 22 ;
	mnuMontiere			= 23 ;
	mnuMontiere2			= 24 ;
	mnuLoesche			= 25 ;
	mnuStarte			= 26 ;

	mnuTraceE			= 31 ;
	mnuTraceB			= 32 ;
	mnuTraceQ			= 33 ;
	mnuTraceR			= 34 ;
	mnuTraceY			= 35 ;
	mnuTraceF			= 36 ;
	mnuTraceT			= 37 ;
	mnuTraceL			= 38 ;
	mnuTraceS			= 39 ;
	mnuTraceA			= 40 ;
	mnuTraceI			= 41 ;
	mnuTraceC			= 42 ;
	mnuTraceZ			= 43 ;
	mnuTraceD			= 44 ;
	mnuTraceP			= 45 ;
	mnuTraceM			= 46 ;
	mnuTraceProgStart		= 47 ;

	mnuTraceAus			= 61 ;
	mnuTraceEin			= 62 ;
	mnuTraceNormal			= 63 ;
	mnuTraceNurDatei		= 64 ;

	mnuTraceDateiEin		= 71 ;
	mnuTraceDateiAus		= 72 ;
	mnuTraceDateiKomplett		= 73 ;
	mnuBacktraceNormal		= 74 ;
	mnuBacktraceErweitert		= 75 ;

	mnuXBG				= 81 ;
	mnuXEN				= 82 ;
	mnuXAN				= 83 ;
	mnuBeende			= 84 ;
	mnuDateiEingabe			= 85 ;
	mnuEmulatorEnde			= 86 ;
	mnuXAB				= 87 ;

	mnuSetZeichenHoehe14		= 91 ;
	mnuSetZeichenHoehe15		= 92 ;
	mnuSetZeichenHoehe16		= 93 ;
	mnuSetZeichenHoehe17		= 94 ;
	mnuSetZeichenHoehe18		= 95 ;
	mnuSetZeichenHoehe19		= 96 ;
	mnuSetZeichenHoehe20		= 97 ;

	mnuInterpreter			= 101 ;
	mnuDisassembler			= 102 ;
	mnuShow				= 103 ;
	mnuRegister			= 104 ;
	mnuMemory			= 105 ;
	mnuSEBtrace			= 106 ;
	mnuTASformatierer		= 107 ;
	mnuGebiete			= 108 ;
	mnuDateilage			= 109 ;
	mnuSSRzaehlerZeigen		= 110 ;

	mnuHilfe			= 901 ;
	mnuDokuBefehlsliste		= 902 ;
	mnuDokuAssembler		= 903 ;
	mnuDokuSystemdienste		= 904 ;
	mnuDokuEntschlKommandos		= 905 ;
	mnuDokuKdoTaschenbuch		= 906 ;

	mnuUeber			= 999 ;


	lf =				CHR (10) ;

	CaptionText =			'TR440-Emulator' ;

	InfoText : ARRAY OF CHAR =	{
					'TR440  Emulator  Vs.  ', g1, g2, v1, v2, v3, lf, lf,
					'http://www.TR440.info', lf, lf,
					'© 2018 Software Entwicklung Buchmann, Solingen', lf, lf,
					'http://www.sebag.de'
					, 0C
					} ;

VAR
	AktMenue :			WIN32.HMENU ;

	MainWindow :			WIN32.HWND ;

	zw :				ARRAY [0..255] OF CHAR ;


	MenueDialog,
	MenueTrace,
	MenueTraceDatei,
	MenueTraceZusatz,
	MenueHilfsprogramme,
	MenueProgramm,
	MenueBearbeiten,
	MenueEinstellungen,
	MenueZeichenHoehe,
	MenueInfo,
	MenueDoku :			WIN32.HMENU ;

	MenueBefehlGemerkt,
	NoTestGray :			CARDINAL ;

	MenueBusy :			BOOLEAN = FALSE ;


PROCEDURE cmdIntern (str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	status :	CARDINAL ;
BEGIN
	TraceF ('PerformCommand %s', str) ;
	RETURN RunProg.PerformCommand (str, RunProg.AsyncExec, status) ;
END cmdIntern ;


PROCEDURE cmd (dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	str :		ARRAY [0..255] OF CHAR ;
		i :		CARDINAL ;
		found :		BOOLEAN ;
BEGIN
	IF FileFunc.FileExists (dateiname) THEN
		Strings.FindNext (' ', dateiname, 0, found, i) ;
		IF found THEN
			str := '"' ;
			Strings.Append (dateiname, str) ;
			Strings.Append ('"', str) ;
		ELSE
			str := 'start ' ;
			Strings.Append (dateiname, str) ;
		END ;
		RETURN cmdIntern (str) ;
	END ;
	RETURN FALSE ;
END cmd ;


PROCEDURE upCMD (name, ext : ARRAY OF CHAR) : BOOLEAN ;
	VAR	progname,
		dateiname,
		str :		ARRAY [0..255] OF CHAR ;
BEGIN
	dateiname := Datentraeger ;
	Strings.Append ('Sonstiges\', dateiname) ;
	Strings.Append (name, dateiname) ;
	Strings.Append (ext, dateiname) ;
	RETURN cmd (dateiname) ;
END upCMD ;


PROCEDURE HTMLshow (name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	RETURN upCMD (name, '.html') ;
END HTMLshow ;


PROCEDURE PDFshow (name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	RETURN upCMD (name, '.pdf') ;
END PDFshow ;


PROCEDURE Editor (dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	status :	CARDINAL ;
		quelle :	FileFunc.File ;
		i :		CARDINAL ;
		gef :		BOOLEAN ;
BEGIN
	IF FileIO.OpenReadFile (quelle, dateiname) THEN
		FileIO.CloseFile (quelle) ;
		IF Editorname [0] > ' ' THEN
			TraceF ('RunProgram %s %s', Editorname, dateiname) ;
			RETURN RunProg.RunProgram (Editorname, dateiname, '', RunProg.AsyncExec, status) ;
		ELSE
			RETURN cmd (dateiname) ;
		END ;
	END ;
	RETURN FALSE ;
END Editor ;


PROCEDURE EditorUp (ausgewaehlt : BOOLEAN ; key : ARRAY OF CHAR ; VAR default : ARRAY OF CHAR) ;
BEGIN
	IF SetRegistryWert (ausgewaehlt, key, AktString) THEN
		default := AktString ;
		Editor (default) ;
	END ;
END EditorUp ;


PROCEDURE SetRegistryWert (ausfuehren : BOOLEAN ; key, wert : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF ausfuehren THEN
		IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE) THEN
			IF Registry.WriteRegString (key, wert) THEN
				TraceF ('* \\Current User\\%s\\%s = %s', DefaultPfad, key, wert) ;
			ELSE
				TraceF ('+++++ \\Current User\\%s\\%s nicht setzbar', DefaultPfad, key) ;
			END ;
			RETURN TRUE ;
		ELSE
			TraceF ('+++++ \\Current User\\%s nicht einstellbar', DefaultPfad) ;
		END ;
	END ;
	RETURN FALSE ;
END SetRegistryWert ;


PROCEDURE PromptString (Bez : ARRAY OF CHAR ; VAR INOUT text : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	upTextFrage.Bezeichnung := Bez ;
	RETURN upTextFrage.TextHolen (MainWindow, text) ;
END PromptString ;


PROCEDURE PromptWert (str : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	zw := str ;
	RETURN TRUE ;
END PromptWert ;


PROCEDURE MenueAufgerufen (window : WIN32.HWND; id : CARDINAL) ;
BEGIN
	IF MenueBusy THEN
		RETURN ;
	END ;

	MenueBusy := TRUE ;

	IF MenueSofort (id) THEN
					(* schon alles erledigt *)
	ELSIF NOT MenueBefehlStehtAn THEN
		MenueBefehlStehtAn := TRUE ;
		MenueBefehlGemerkt := id ;
		IF Terminal.imWarteZustand THEN
			SimulateInput (cr) ;
		END ;

	END ;

	MenueBusy := FALSE ;

END MenueAufgerufen ;


PROCEDURE VerschiebeInOefdb (name : ARRAY OF CHAR) ;
	VAR	zwname1,
		zwname2 :	ARRAY [0..255] OF CHAR ;
BEGIN
	ProgNameExt (_STDDB, name, zwname1) ;
	ProgNameExt (_OEFDB, name, zwname2) ;
	IF FileFunc.FileExists (zwname2) THEN
		IF NOT FileFunc.DeleteFile (zwname2) THEN
			WriteF ('\n+++++ Datei %s konnte nicht gelöscht werden', zwname2) ;
		END ;
	END ;
	IF NOT FileFunc.CopyFile (zwname1, zwname2) THEN
		WriteF ('\n+++++ Datei %s konnte nicht kopiert werden', zwname2) ;
	END ;
END VerschiebeInOefdb ;


PROCEDURE GetQuelle (VAR name : ARRAY OF CHAR) : BOOLEAN ;
	VAR	parts :	FileFunc.FileNameParts ;
BEGIN
	IF SetRegistryWert (OFNtas (DefaultQuelle, FALSE), TASkey, AktString) THEN
		DefaultQuelle := AktString ;
		FileFunc.ParseFileName (DefaultQuelle, parts) ;
		name := parts.name ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END GetQuelle ;


PROCEDURE MenueSofort (id : CARDINAL) : BOOLEAN ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN

	CASE id OF				(* diese Befehle werden immer sofort ausgeführt *)

	mnuTraceE :			TraceBefehl ('E') ;
					RETURN TRUE ;
	|
	mnuTraceB :			TraceBefehl ('B') ;
					RETURN TRUE ;
	|
	mnuTraceQ :			TraceBefehl ('Q') ;
					RETURN TRUE ;
	|
	mnuTraceR :			TraceBefehl ('R') ;
					RETURN TRUE ;
	|
	mnuTraceY :			TraceBefehl ('Y') ;
					RETURN TRUE ;
	|
	mnuTraceF :			TraceBefehl ('F') ;
					RETURN TRUE ;
	|
	mnuTraceT :			TraceBefehl ('T') ;
					RETURN TRUE ;
	|
	mnuTraceL :			TraceBefehl ('L') ;
					RETURN TRUE ;
	|
	mnuTraceS :			TraceBefehl ('S') ;
					RETURN TRUE ;
	|
	mnuTraceA :			TraceBefehl ('A') ;
					RETURN TRUE ;
	|
	mnuTraceI :			TraceBefehl ('I') ;
					RETURN TRUE ;
	|
	mnuTraceC :			TraceBefehl ('C') ;
					RETURN TRUE ;
	|
	mnuTraceZ :			TraceBefehl ('Z') ;
					RETURN TRUE ;
	|
	mnuTraceD :			TraceBefehl ('D') ;
					RETURN TRUE ;
	|
	mnuTraceP :			TraceBefehl ('P') ;
					RETURN TRUE ;
	|
	mnuTraceM :			TraceBefehl ('M') ;
					RETURN TRUE ;

	|
	mnuTraceAus :			TraceBefehl ('0') ;
					RETURN TRUE ;
	|
	mnuTraceEin :			TraceBefehl ('1') ;
					RETURN TRUE ;
	|
	mnuTraceNormal :		TraceBefehl ('X') ;
					RETURN TRUE ;
	|
	mnuTraceProgStart :		debug.EinBeiProgStart := TRUE ;
					RETURN TRUE ;
	|
	mnuTraceNurDatei :		TraceBefehl ('-') ;
					RETURN TRUE ;

	|
	mnuTraceDateiEin :		BacktraceAnzahl := 1000000 ;
					RETURN TRUE ;
	|
	mnuTraceDateiAus :		ExitTrace ;
					BacktraceAnzahl := 1 ;
					RETURN TRUE ;
	|
	mnuTraceDateiKomplett :		BacktraceAnzahl := 0 ;
					RETURN TRUE ;
	|
	mnuBacktraceNormal :		BacktraceAnzahl := 1000000 ;
					RETURN TRUE ;
	|
	mnuBacktraceErweitert :		BacktraceAnzahl := 10000000 ;
					RETURN TRUE ;
	|
	mnuXEN :			AlarmGewesen := TRUE ;
					AlleProgrammeAbbrechen := TRUE ;
	|
	mnuEmulatorEnde :		HALT ;
	|
	mnuXAN :			IF NOT Grundzustand AND NOT Terminal.imWarteZustand THEN
						SimulateInput ('#XAN#.') ;
					END ;
					RETURN TRUE ;
	|
	mnuXAB :			IF NOT Grundzustand AND NOT Terminal.imWarteZustand THEN
						XABgefunden := TRUE ;
					END ;
					RETURN TRUE ;
	|
	mnuBeende :			AlarmGewesen := TRUE ;
					globProgrammEnde := TRUE ;
	|
	mnuDateiEingabe :		IF ProgrammEingabeBeginn THEN		(* in SSR 6 16 - Eingabeanforderung *)
						SimulateInput ('@') ;
						SimulateInput (cr) ;		(* dadurch Eingabedatei-Anfrage *)
					END ;
					RETURN TRUE ;
	|
	mnuSEBtrace :			InitTrace ;
					RETURN TRUE ;
	ELSE
	END ;

	RETURN FALSE ;
END MenueSofort ;


PROCEDURE CheckVerzeichnis (VAR INOUT verz : ARRAY OF CHAR) ;
BEGIN
	TRIM (verz) ;
	IF verz [0] <= ''  THEN
		verz := 'C:\TR440' ;
	END ;
	IF verz [LENGTH(verz)-1] <> '\' THEN
		Strings.Append ('\', verz) ;
	END ;
	FileFunc.CreateDirTree (verz) ;
END CheckVerzeichnis ;


PROCEDURE MenueBefehl () (* es hat sich was getan *) : BOOLEAN ;
	VAR	id,
		index,
		status,
		GNR :		CARDINAL ;
		name :		ARRAY [0..255] OF CHAR ;
		lbl :		pLeitblock ;
		b :		BOOLEAN ;

	PROCEDURE ProgPause ;
	BEGIN
		ProgrammPausieren ;
		INC (AktOperator) ;
		WITH Operatoren [AktOperator] DO
			OLKV := AktOLK ;
			INC (OLKpegel) ;
			AktOLK := OLKpegel ;
			OLK := AktOLK ;
		END ;
		zwKachelBelegt := KachelBelegt ;
		StdLeitblock ;
	END ProgPause ;

	PROCEDURE ProgWeiter ;
	BEGIN
		LeitblockFreigeben ;
		KachelBelegt := zwKachelBelegt ;
		DEC (AktOperator) ;
		ProgrammFortsetzen ;
	END ProgWeiter ;


BEGIN

	id := MenueBefehlGemerkt ;
	MenueBefehlStehtAn := FALSE ;
	name := '' ;

	IF Grundzustand THEN
		CASE id OF

		mnuSetFont :			SetRegistryWert (PromptChooseFont (MainWindow, FontName), FontKey , FontName) ;
						RETURN FALSE ;
		|
		mnuSetBenutzer :		SetRegistryWert (PromptString ('Benutzer-Name', AktAuftrag.BEN), BenKey , AktAuftrag.BEN) ;
						RETURN FALSE ;
		|
		mnuSetBKZ :			SetRegistryWert (PromptString ('Benutzer-BKZ', AktAuftrag.BKZ), BKZkey, AktAuftrag.BKZ) ;
						RETURN FALSE ;
		|
		mnuSetFKZ :			SetRegistryWert (PromptString ('Auftrags-FKZ', AktAuftrag.FKZ), FKZkey, AktAuftrag.FKZ) ;
						RETURN FALSE ;
		|
		mnuSetDatentraeger :		IF PromptString ('TR440-Daten-Ort', Datentraeger) THEN
							CheckVerzeichnis (Datentraeger) ;
							SetRegistryWert (TRUE, DatenKey, Datentraeger) ;

							AblaufProtokoll := Datentraeger ;
							Strings.Append (UserTemporaer, AblaufProtokoll) ;
							Strings.Append ('&PROTO\ABLAUFPROTOK.TXT', AblaufProtokoll) ;

							TerminalProtokoll := Datentraeger ;
							Strings.Append (UserTemporaer, TerminalProtokoll) ;
							Strings.Append ('&PROTO\TERMINALPROT.TXT', TerminalProtokoll) ;
						END ;
						RETURN FALSE ;
		|
		mnuSetQuellbasis :		IF PromptString ('Quell-Basis-Ort', Quellbasis) THEN
							CheckVerzeichnis (Quellbasis) ;
							SetRegistryWert (TRUE, QuellbasisKey, Quellbasis) ;

							DefaultQuelle := Quellbasis ;
							Strings.Append ('Quellen\', DefaultQuelle) ;

							DefaultLST := DefaultQuelle ;
							Strings.Append ('Protokolle\vorlage.TLST', DefaultLST) ;
							SetRegistryWert (TRUE, LSTkey, DefaultLST) ;

							DefaultTRAC := DefaultQuelle ;
							Strings.Append ('Trace\vorlage.TRAC', DefaultTRAC) ;
							SetRegistryWert (TRUE, TRACkey, DefaultTRAC) ;

							Strings.Append ('vorlage.TAS', DefaultQuelle) ;
							SetRegistryWert (TRUE, TASkey, DefaultQuelle) ;


							DefaultKDO := Quellbasis ;
							Strings.Append ('Kommandos\proz.KDO', DefaultKDO) ;
							SetRegistryWert (TRUE, KDOkey, DefaultKDO) ;
						END ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe14 :		SetRegistryWert (PromptWert ('14'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe15 :		SetRegistryWert (PromptWert ('15'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe16 :		SetRegistryWert (PromptWert ('16'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe17 :		SetRegistryWert (PromptWert ('17'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe18 :		SetRegistryWert (PromptWert ('18'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe19 :		SetRegistryWert (PromptWert ('19'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuSetZeichenHoehe20 :		SetRegistryWert (PromptWert ('20'), ZhoeheKey, zw) ;
						RETURN FALSE ;
		|
		mnuXBG :			SimulateInput ('#XBG,BEN=') ;
						SimulateInput (AktAuftrag.BEN) ;
						IF AktAuftrag.BKZ [0] <> 0C THEN
							SimulateInput (',BKZ=') ;
							SimulateInput (AktAuftrag.BKZ) ;
						END ;
						IF AktAuftrag.FKZ [0] <> 0C THEN
							SimulateInput (',FKZ=') ;
							SimulateInput (AktAuftrag.FKZ) ;
						END ;
						SimulateInput ('#.') ;
						SimulateInput (cr) ;
						id := 0 ;

		|
		mnuInterpreter :		Interpreter ;
						id := 0 ;
		|
		mnuTAS :			IF GetQuelle (name) THEN
							StdLeitblock ;
							upCallAssembler (DefaultQuelle, FALSE) ;
							id := 0 ;
						END ;
		|
		mnuMerke :			IF GetQuelle (name) THEN
							StdLeitblock ;
							IF upCallAssembler (DefaultQuelle, FALSE) THEN
								upProgMerke (name) ;
							END ;
							id := 0 ;
						END ;
		|
		mnuMontiere :			IF GetQuelle (name) THEN
							StdLeitblock ;
							IF upCallAssembler (DefaultQuelle, FALSE) THEN
								upProgMontiere (name) ;
							END ;
							id := 0 ;
						END ;

		|
		mnuMontiere2 :			IF GetQuelle (name) THEN
							StdLeitblock ;
							IF upCallAssembler (DefaultQuelle, FALSE) THEN
								upProgMontiere (name) ;
								VerschiebeInOefdb (name) ;
							END ;
							id := 0 ;
						END ;

		|
		mnuStarte :			IF GetQuelle (name) THEN
							StdLeitblock ;
							upProgProg (name) ;
							id := 0 ;
						END ;

		ELSE
		END ;
	END ;


	CASE id OF
	0 :				(* wurde schon ausgeführt *)
	|
	mnuZeigenQuelle :		EditorUp (OFNtas (DefaultQuelle, FALSE), TASkey, DefaultQuelle) ;
					RETURN FALSE ;
	|
	mnuZeigenProtokoll :		EditorUp (OFNlst (DefaultLST, FALSE), LSTkey, DefaultLST) ;
					RETURN FALSE ;
	|
	mnuZeigenTrace :		EditorUp (OFNtrac (DefaultTRAC, FALSE), TRACkey, DefaultTRAC) ;
					RETURN FALSE ;
	|
	mnuZeigenKommando :		EditorUp (OFNkdo (DefaultKDO, FALSE), KDOkey, DefaultKDO) ;
					RETURN FALSE ;
	|
	mnuZeigenTR440datei,
	mnuZeigenTR440textDatei :	DefaultTR440 := Datentraeger ;
					Strings.Append (UserTemporaer, DefaultTR440) ;
					Strings.Append (_STDDB, DefaultTR440) ;
					Strings.Append ('\TEST.100', DefaultTR440) ;
					IF OFNtr440 (DefaultTR440, FALSE) THEN
						DefaultTR440 := AktString ;
						IF KonvertTR440datei (AktString, zw, id = mnuZeigenTR440textDatei) THEN
							Editor (zw) ;
						END ;

					END ;
					RETURN FALSE ;
	|
	mnuZeigenTR440gebiet :		IF PromptString ('Gebietsnummer', AktString) AND GetZahl (AktString, GNR) THEN
						IF KonvertTR440gebiet (GNR, zw) THEN
							Editor (zw) ;
						END ;
					END ;
					RETURN FALSE ;

	|
	mnuZeigenTerminal :		TermProtAus ;
					FileFunc.ConstructFileName ('.COP', TerminalProtokoll, zw) ;
					IF FileFunc.CopyFile (TerminalProtokoll, zw) THEN
						TermProtEin (FALSE) ;
						Editor (zw) ;
					ELSE
						TermProtEin (FALSE) ;
					END ;
					RETURN FALSE ;
	|
	mnuZeigenAblaufProtokoll,
	mnuZeigenAblaufProtokollLoe :	AblaufProtAus ;
					FileFunc.ConstructFileName ('.COP', AblaufProtokoll, zw) ;
					IF FileFunc.CopyFile (AblaufProtokoll, zw) THEN
						AblaufProtEin (id = mnuZeigenAblaufProtokollLoe) ;
						Editor (zw) ;
					ELSE
						AblaufProtEin (FALSE) ;
					END ;
					RETURN FALSE ;
	|
	mnuXEN :			SimulateInput ('#XEN#.') ;
	       				SimulateInput (cr) ;
	|
	mnuTASformatierer :		IF GetQuelle (name) THEN
						TASformatierer (DefaultQuelle) ;
						Editor (DefaultQuelle) ;
						id := 0 ;
					END ;
	|
	mnuTAS :			IF GetQuelle (name) THEN
						ProgPause ;
						upCallAssembler (DefaultQuelle, FALSE) ;
						ProgWeiter ;
					END ;
	|
	mnuMerke :			IF GetQuelle (name) THEN
						ProgPause ;
						IF upCallAssembler (DefaultQuelle, FALSE) THEN
							upProgMerke (name) ;
						END ;
						ProgWeiter ;
					END ;
	|
	mnuMontiere :			IF GetQuelle (name) THEN
						ProgPause ;
						IF upCallAssembler (DefaultQuelle, FALSE) THEN
							upProgMontiere (name) ;
						END ;
						ProgWeiter ;
					END ;
	|
	mnuMontiere2 :			IF GetQuelle (name) THEN
						ProgPause ;
						IF upCallAssembler (DefaultQuelle, FALSE) THEN
							upProgMontiere (name) ;
							VerschiebeInOefdb (name) ;
						END ;
						ProgWeiter ;
					END ;
	|
	mnuDisassembler :		IF PromptString ('Adresse', name) THEN
						AsmDisasm.BefehleZeigen (name) ;
					END ;
	|
	mnuShow :			IF PromptString ('Adresse', name) THEN
						AsmDisasm.Zeigen (name) ;
					END ;
	|
	mnuRegister :			b := debug.BildschirmProtokoll ;
					debug.BildschirmProtokoll := TRUE ;
					RegAlleZeigen ;
					debug.BildschirmProtokoll := b ;
	|
	mnuMemory :			upShowMemory ;
	|
	mnuGebiete :			upShowGebiete ;
	|
	mnuDateilage :			upGebietslageInDateilage ;
	|
	mnuSSRzaehlerZeigen :		SSRzaehlerZeigen ;
	|
	mnuUeber :			WINUSER.MessageBox (MainWindow, InfoText, CaptionText, (* WINUSER.MB_HELP BOR *) WINUSER.MB_OK) ;
					RETURN FALSE ;
	|
	mnuHilfe :			cmdIntern ('start http://www.tr440.info/hilfe.html') ;
					RETURN FALSE ;
	|
	mnuDokuEntschlKommandos :	HTMLshow ('Kommandoliste') ;
					RETURN FALSE ;
	|
	mnuDokuBefehlsliste :		PDFshow ('GrosseBefehlsliste') ;
					RETURN FALSE ;
	|
	mnuDokuAssembler :		PDFshow ('Assemblersprache') ;
					RETURN FALSE ;
	|
	mnuDokuSystemdienste :		PDFshow ('Systemdienste') ;
					RETURN FALSE ;
	|
	mnuDokuKdoTaschenbuch :		PDFshow ('KdoTaschenbuch') ;
					RETURN FALSE ;
	|
	mnuSetEditor :			SetRegistryWert (OFNexe ('Text-Editor', Editorname, FALSE), EditorKey, AktString) ;
					Editorname := AktString ;
					RETURN FALSE ;
	|
	mnuSetDrucker :			SetRegistryWert (GDIdruckerAuswahl(Drucker), DruckerKey, Drucker) ;
					RETURN FALSE ;
	|
	mnuSetDruckerFont :		SetRegistryWert (GDIfontAuswahl(DruckerFont), DruckerFontKey, DruckerFont) ;
					RETURN FALSE ;

	ELSE
					RETURN FALSE ;
	END ;
	RETURN TRUE ;
END MenueBefehl ;


PROCEDURE  SubMenueZeichenhoehe () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe14,				'Zeichenhöhe 1&4') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe15,				'Zeichenhöhe 1&5') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe16,				'Zeichenhöhe 1&6') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe17,				'Zeichenhöhe 1&7') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe18,				'Zeichenhöhe 1&8') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe19,				'Zeichenhöhe 1&9') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetZeichenHoehe20,				'Zeichenhöhe &20') ;
	MenueZeichenHoehe := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueZeichenhoehe ;


PROCEDURE SubMenueEinstellungen () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetFont,					'Fontname') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP, 			SubMenueZeichenhoehe (),	'Zeichen&hoehe') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetBenutzer,					'&BEN=') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetBKZ,						'B&KZ=') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetFKZ,						'&FKZ=') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetDatentraeger,				'&Datentraeger-Verzeichnis') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetQuellbasis,					'&Quellbasis-Verzeichnis') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetEditor,					'&Editor für Textdateien') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetDrucker,					'D&rucker einstellen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSetDruckerFont,					'Druckerf&ont einstellen') ;
	MenueEinstellungen := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueEinstellungen ;


PROCEDURE SubMenueBearbeiten () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	IF ProgrammierModus THEN
		WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenQuelle,				'TAS - &Quelle') ;
		WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenProtokoll,			'TAS - &Protokolldatei') ;
		WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenTrace,				'TAS - T&racedatei') ;
	END ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenKommando,					'&Kommandodatei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenTR440datei,				'TR&440 - Datei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenTR440textDatei,				'TR440 - Te&xtdatei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenTR440gebiet,				'TR440 - &Gebiet') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenTerminal,					'&Terminal-Protokolldatei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenAblaufProtokoll,				'&Ablauf-Protokolldatei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuZeigenAblaufProtokollLoe,			'&Ablauf-Protokolldatei mit Löschen') ;
	MenueBearbeiten := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueBearbeiten ;


PROCEDURE SubMenueProgramm () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTAS,						'Programm &Assemblieren') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING , mnuMerke,						'     und ausführbar &Merken') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuMontiere,					'     und Spei&chern in STDDB') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuMontiere2,					'     und Speichern in &OEFDB') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuStarte,						'Programm assemblieren und &Starten') ;
	MenueProgramm := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueProgramm ;


PROCEDURE SubMenueHilfsprogramme () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuInterpreter,					'&Interpreter für TAS') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDisassembler,					'&Disassembler') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTASformatierer,					'TAS-&Formatierer') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuShow,						'&Speicher dumpen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuRegister,					'&Register zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuGebiete,					'&Gebiete auflisten') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDateilage,					'alle &Programme in STDDB in Dateilage bringen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuMemory,						'&Memory-Aufbau zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSEBtrace,					'SEB&trace ankoppeln') ;
	IF SSRsZaehlen THEN
		WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuSSRzaehlerZeigen,			'SSR-Anzahlen &zeigen') ;
	END ;
	MenueHilfsprogramme := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueHilfsprogramme ;


PROCEDURE SubMenueTraceZusatz () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceE,						'&E Einzelschritt an') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceB,						'&B Befehle tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceQ,						'&Q Quellzeilen zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceR,						'&R Registeränderungen tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceY,						'&Y SSR-Protokoll mit VBL zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceF,						'&F Einzelschritt ein bei Alarm/SSR-Fehlern') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceT,						'&T Takt-Anzahlen zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceL,						'&L Lesen tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceS,						'&S Speichern tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceA,						'&A Befehlsauslese tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceI,						'&I Indexzugriffe tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceC,						'&C Assoziativ-Register tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceZ,						'&Z Zweitbefehle tracen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceD,						'&D Dateizugriffe zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceP,						'&P Assembler-Protokoll zeigen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceM,						'&M Memory-Alloc/Dealloc zeigen') ;
	MenueTraceZusatz := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueTraceZusatz ;


PROCEDURE SubMenueTraceDatei () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceDateiEin,					'Datei-Trace &einschalten') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceDateiAus,					'Datei-Trace &ausschalten') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceDateiKomplett,				'&ohne Backtrace') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuBacktraceNormal,				'&normaler Backtrace ~ 1 Mio. Befehle') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuBacktraceErweitert,				'er&weiterter Backtrace ~ 10 Mio. Befehle') ;
	MenueTraceDatei := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueTraceDatei ;


PROCEDURE SubMenueTrace () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceAus,					'&0 allen Trace ausschalten') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceEin,					'&1 allen Trace einschalten') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceNormal,					'&X Normal-Trace ~ 0EBQRY') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceNurDatei,					'&- Trace nicht auf Bildschirm') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceProgStart,					'Trace ein bei &Programmstart') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuTraceF,						'Trace ein bei Alarm/SSR-&Fehlern') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,			SubMenueTraceZusatz(),		'&Zusatz-Steuerungen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,			SubMenueTraceDatei(),		'&Datei-Trace') ;
	MenueTrace := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueTrace ;


PROCEDURE SubMenueDialog () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	IF ProgrammierModus THEN
		WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuXBG,					'#X&BG,BEN= beginne Dialog') ;
		WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	END ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuXEN,						'#X&EN#. beende Dialog') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuXAN,						'#X&AN#. anhalten Dialog') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuXAB,						'#&XAB#. Restausgabe abbrechen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuBeende,						'&Programm abbrechen') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDateiEingabe,					'Eingabe&simulation aus Datei') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuEmulatorEnde,					'Emulator bee&nden') ;
	MenueDialog := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueDialog ;



PROCEDURE SubMenueDoku () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDokuBefehlsliste,				'große &Befehlsliste TAS') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDokuSystemdienste,				'&Systemdienste TR440') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDokuAssembler,					'&Assemblersprache TAS') ;
	MenueDoku := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueDoku ;



PROCEDURE SubMenueInfo () : WIN32.UINT ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	menu := WINUSER.CreateMenu () ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuHilfe,						'&Hilfe') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDokuEntschlKommandos,				'benutzbare &Kommandos') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuDokuKdoTaschenbuch,				'Kommando-&Taschenbuch') ;
	IF ProgrammierModus THEN
		WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,		SubMenueDoku(),			'&Dokumentationen') ;
	END ;
	WINUSER.AppendMenu (menu, WINUSER.MF_SEPARATOR, 0, '') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_STRING, mnuUeber,						'&Über den TR440-Emulator') ;
	MenueInfo := menu ;
	RETURN menu:WIN32.UINT ;
END SubMenueInfo ;


PROCEDURE InitMenue ;
	VAR	menu :	WIN32.HMENU ;
BEGIN
	IF WND = 0 THEN
		RETURN ;
	END ;
	MainWindow := SYSTEM.CAST (WIN32.HWND, WND) ;

	IF ProgrammierModus THEN
		NoTestGray := 0 ;
	ELSE
		NoTestGray := WINUSER.MF_GRAYED ;
	END ;

	AktMenue := WINUSER.CreateMenu () ;
	menu := AktMenue ;
	IF menu = WINX.NULL_HMENU THEN
		RETURN ;
	END ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,			SubMenueDialog(),		'&Dialog') ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,			SubMenueBearbeiten(),		'&Bearbeiten') ;
	IF ProgrammierModus THEN
		WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,		SubMenueTrace(),		'&Trace') ;
		IF cmdEigenerAssembler THEN
			WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,	SubMenueProgramm(),		'&Programmierung') ;
		END ;
		WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,		SubMenueHilfsprogramme(),	'&Hilfsprogramme') ;
		WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,		SubMenueEinstellungen(),	'&Einstellungen (ab nächstem Start)') ;
	END ;
	WINUSER.AppendMenu (menu, WINUSER.MF_POPUP,			SubMenueInfo(),			'&Info') ;

	WINUSER.SetMenu (MainWindow, AktMenue) ;

	MenueAufruf := MenueAufgerufen ;
	MenueAngemeldet := TRUE ;

	IF Terminal.ZeichenHoehe >= 14 THEN
		WINUSER.CheckMenuItem (MenueZeichenHoehe, Terminal.ZeichenHoehe - 14, WINUSER.MF_CHECKED BOR WINUSER.MF_BYPOSITION) ;
	END ;

END InitMenue ;

%END


PROCEDURE SetGrundzustand (wert : BOOLEAN) ;
BEGIN
	IF Grundzustand <> wert THEN
		Grundzustand := wert ;
	END ;
END SetGrundzustand ;

END Menue.
