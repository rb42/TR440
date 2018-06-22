
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

MODULE TR440;

(*	20.06.18	*)


<*/RESOURCE:TR440.res *>


IMPORT	Terminal, Registry, Conversions, Strings, TerminalEingabe ;

%IF TTY %THEN
IMPORT TTY ;
%END

FROM COMMCTRL IMPORT
	InitCommonControls ;

FROM Parameter IMPORT
	InitParameter,
	NextParameter ;

FROM Menue IMPORT
	InitMenue,
	Editor ;

IMPORT Struktur, DateiVerwaltung, debug ;

FROM TR440hp IMPORT * ;

FROM AsmTrace IMPORT
	ExitTrace ;

FROM Trace IMPORT
	SEBtrace,
	InitTrace,
	TraceF ;


CONST
	Ueberschrift : ARRAY OF CHAR  =	{ 'TR440-Emulator ', g1, g2, v1, v2, v3 } ;



VAR
	termPause :	CARDINAL = 0 ;

	TTYport :	CARDINAL = 0 ;
	COMport :	CARDINAL = 0 ;


PROCEDURE CheckReg (key : ARRAY OF CHAR) ;
	VAR	str :		ARRAY [0..511] OF CHAR ;
BEGIN
	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, TRUE) THEN
		IF NOT Registry.ReadRegString (key, str) THEN								(* existiert noch nicht im CurrentUser *)
			IF Registry.InitRegistry (Registry.LocalMachine, DefaultPfad, FALSE) THEN
				IF Registry.ReadRegString (key, str) THEN						(* aber in LocalMachine *)
					TraceF ('* \\Local Machine\\%s\\%s : %s', DefaultPfad, key, str) ;
					IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE) THEN
						IF Registry.WriteRegString (key, str) THEN				(* => übernehmen als Default in CurrentUser *)
							TraceF ('* \\CurrentUser\\%s\\%s = %s', DefaultPfad, key, str) ;
						ELSE
							TraceF ('+++++ \\CurrentUser\\%s\\%s nicht setzbar', DefaultPfad, key) ;
						END ;
					ELSE
						TraceF ('+++++ \\CurrentUser\\%s\\ nicht einstellbar', DefaultPfad) ;
					END ;
				ELSE
					TraceF ('+++++ \\LocalMachine\\%s\\%s nicht vorhanden', DefaultPfad, key) ;
				END ;
			ELSE
				TraceF ('+++++ \\LocalMachine\\%s\\ nicht einstellbar', DefaultPfad) ;
			END ;
		ELSE
			TraceF ('* \\CurrentUser\\%s\\%s : %s', DefaultPfad, key, str) ;
		END ;
	ELSE
		TraceF ('+++++ \\CurrentUser\\%s\\ nicht einstellbar', DefaultPfad) ;
	END ;
END CheckReg ;


PROCEDURE CheckDefaultRegistry ;
BEGIN
	CheckReg (DatenKey) ;
	CheckReg (QuellbasisKey) ;
	CheckReg (EditorKey) ;
	CheckReg (BenKey) ;
	CheckReg (BKZkey) ;
	CheckReg (FKZkey) ;
	CheckReg (ZhoeheKey) ;
	CheckReg (MaxSpalteKey) ;
	CheckReg (MaxZeileKey) ;
	CheckReg (MaximalKey) ;
	CheckReg (FontKey) ;
	CheckReg (TASkey) ;
	CheckReg (LSTkey) ;
	CheckReg (TRACkey) ;
	CheckReg (KDOkey) ;
	CheckReg (DruckerKey) ;
	CheckReg (DruckerFontKey) ;
END CheckDefaultRegistry ;


PROCEDURE GetRegZahl (key : ARRAY OF CHAR ; default : CARDINAL) : CARDINAL ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
		i :	CARDINAL ;
BEGIN
	IF Registry.ReadRegString (key, str) THEN
		IF (str [0] <> 0C) AND Conversions.StrToCard (str, i) THEN
			RETURN i ;
		END ;
	END ;
	RETURN default ;
END GetRegZahl ;


PROCEDURE InitWindow ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	Terminal.ZeichenHoehe := cmdZeichenhoehe ;
	Terminal.FontName := cmdFontName ;
	Terminal.MaxSpalte := cmdMaxSpalte ;
	Terminal.MaxZeile := cmdMaxZeile ;
	Terminal.Maximal := cmdMaximal ;

	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE) THEN
		IF cmdZeichenhoehe = 0 THEN
			Terminal.ZeichenHoehe := GetRegZahl (ZhoeheKey, 16) ;
		END ;

		IF cmdFontName [0] = 0C THEN
			IF Registry.ReadRegString (FontKey, str) THEN
				Terminal.FontName := str ;
			ELSE
				Terminal.FontName := 'Courier New' ;
			END ;
		END ;

		IF cmdMaxSpalte = 0 THEN
			Terminal.MaxSpalte := GetRegZahl (MaxSpalteKey, 200) ;
		END ;

		IF cmdMaxZeile = 0 THEN
			Terminal.MaxZeile := GetRegZahl (MaxZeileKey, 72) ;
		END ;

		IF NOT cmdMaximal THEN
			IF GetRegZahl (MaximalKey, 0) = 1 THEN
				Terminal.Maximal := TRUE ;
			END ;
		END ;
	ELSE
		IF cmdZeichenhoehe = 0 THEN
			Terminal.ZeichenHoehe := 16 ;
		END ;
		IF cmdMaxSpalte = 0 THEN
			Terminal.MaxSpalte := 200 ;
		END ;
		IF cmdMaxZeile = 0 THEN
			Terminal.MaxZeile := 72 ;
		END ;
		Terminal.Maximal := TRUE ;
	END ;

	Terminal.Abbrechbar := FALSE ;
	Terminal.WindowTitle := '  seb   TR440-Emulator' ;

	Terminal.WriteLn ;

	Terminal.InitWindowIcons ('Icon16', 'Icon32') ;
END InitWindow ;


PROCEDURE InitReg (key : ARRAY OF CHAR ; VAR wert : ARRAY OF CHAR ; vorbes : ARRAY OF CHAR) ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF Registry.ReadRegString (key, str) AND (str [0] > ' ')THEN
		wert := str [0..LENGTH(str)] ;
	ELSE
		wert := vorbes ;
		IF NOT Registry.WriteRegString (key, vorbes) THEN
			TraceF ('+++++ Fehler beim Registry-Schreiben') ;
		END ;
	END ;
END InitReg ;



PROCEDURE Init ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
		i :	CARDINAL ;
BEGIN
	CheckDefaultRegistry ;


(*	IF NOT Struktur.ProgrammierModus THEN
*)
		debug.alle (FALSE) ;
(*
	END ;
*)

	Struktur.InitSpeicher ;

	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE) THEN
		IF Registry.ReadRegString (DatenKey, str) THEN
			IF str [LENGTH(str)-1] <> '\' THEN
				Strings.Append ('\', str) ;
			END ;
			DateiVerwaltung.Datentraeger := str ;
		ELSE
			Registry.WriteRegString (DatenKey, DateiVerwaltung.Datentraeger) ;	(* damit man's leichter ändern kann *)
		END ;

		TerminalProtokoll := DateiVerwaltung.Datentraeger ;
		Strings.Append (DateiVerwaltung.UserTemporaer, TerminalProtokoll) ;
		Strings.Append (DateiVerwaltung._PROTO, TerminalProtokoll) ;
		AblaufProtokoll := TerminalProtokoll ;
		Strings.Append ('\TERMINALPROT.txt', TerminalProtokoll) ;
		Strings.Append ('\ABLAUFPROTOK.txt', AblaufProtokoll) ;

		InitReg (EditorKey, Editorname, '') ;
		InitReg (BenKey, Struktur.AktAuftrag.BEN, 'Rainard Buchmann') ;
		InitReg (BKZkey, Struktur.AktAuftrag.BKZ, 'RB') ;
		InitReg (FKZkey, Struktur.AktAuftrag.FKZ, 'EMUL') ;
		InitReg (DruckerKey, Drucker, '') ;
		InitReg (DruckerFontKey, DruckerFont, 'Courier New') ;

		GetDefaultQuelle ;
	END ;

	Struktur.AlleProgrammeAbbrechen := FALSE ;

	IF DateiVerwaltung.CheckDatentraeger () THEN

		IF NOT InitFehlermeldung THEN
			Terminal.ClearScreen ;
		END ;

		IF TerminalProtokoll [0] > ' ' THEN
			TermProtEin (TRUE) ;
		END ;

		IF NOT InitFehlermeldung AND NOT Struktur.AbschnittsModus THEN
			FOR i := 1 TO Terminal.MaxZeile DO
				Terminal.WriteLn ;
			END ;
		END ;
        END ;
	Terminal.WriteString (Ueberschrift) ;
	IF NOT InitFehlermeldung AND NOT Struktur.AbschnittsModus THEN
		FOR i := 1 TO 6 DO
			Terminal.WriteLn ;
		END ;
	END ;

END Init ;


PROCEDURE GetParameter ;
	VAR	parm :	ARRAY [0..127] OF CHAR ;

	PROCEDURE GetZahlParameter () : CARDINAL ;
		VAR	zahl :	CARDINAL ;
	BEGIN
		IF Conversions.StrToCard (parm [2..HIGH(parm)], zahl) THEN
			RETURN zahl ;
		END ;
		RETURN 0 ;
	END GetZahlParameter ;

BEGIN
	DateiVerwaltung.UserTemporaer := DateiVerwaltung._USER ;
	Strings.Append ('\', DateiVerwaltung.UserTemporaer) ;

	IF InitParameter () THEN
		LOOP
			NextParameter (parm) ;
			CASE parm [0] OF
			  0C :
				EXIT ;
			| '/',
			  '-' :
				CASE CAP (parm [1]) OF
				  'P' :									(* Programmierer-Modus *)
						IF termPause = 0 THEN
							Struktur.ProgrammierModus := TRUE ;
						END ;
				| 'H' :									(* Zeichenhöhe *)
						cmdZeichenhoehe := GetZahlParameter () ;
				| 'Z' :									(* Anzahl Bildschirm-Zeilen *)
						cmdMaxZeile := GetZahlParameter () ;
				| 'S' :									(* Anzahl Bildschirm-Spalten *)
						cmdMaxSpalte := GetZahlParameter () ;
				| 'M' :									(* Vollbildschirm *)
						cmdMaximal := GetZahlParameter () > 0 ;
				| 'F' :									(* Fontname *)
						cmdFontName := parm [2..HIGH(parm)] ;
				| 'A' :									(* Abschnitts-Modus *)
						IF parm [2] > ' ' THEN
							Struktur.AbschnittsModus := TRUE ;
							Terminal.Unterdruecken := TRUE ;
							InitDefaults ;
							TerminalEingabe.InitSimulation (parm [2..HIGH(parm)]) ;
						END ;
				| 'V' :									(* interner ASM-Programm - Vorrang vor OKB *)
						cmdAsmVorrang := TRUE ;
				| 'E' :									(* eigener Assembler benutzbar *)
						cmdEigenerAssembler := TRUE ;
				| 'W' :									(* WEB - Interface CGI *)
						WebInterface := TRUE ;
						Struktur.AbschnittsModus := TRUE ;
						Terminal.Unterdruecken := TRUE ;
				| 'U' :									(* User-Vorgabe *)
						DateiVerwaltung.UserTemporaer := DateiVerwaltung._USER ;
						Strings.Append (parm [2..HIGH(parm)], DateiVerwaltung.UserTemporaer) ;
						IF DateiVerwaltung.UserTemporaer [LENGTH(DateiVerwaltung.UserTemporaer)-1] <> '\' THEN
							Strings.Append ('\', DateiVerwaltung.UserTemporaer) ;
						END ;
				| 'I' :
						cmdShowInfo := TRUE ;					(* Takte und KSP-Bedarf zeigen *)
				| 'Y' :
						SSRsZaehlen := TRUE ;					(* alle SSRs mitzählen *)
%IF TTY %THEN
				| 'T' :
						TTYport := 0 ;
						IF parm [2] <= ' ' THEN
							termPause := 140 ;				(* TTY-FS-Simulation : nur 7 Zeichen / sec *)
							GLOBmsPause := termPause ;
							Struktur.ProgrammierModus := FALSE ;		(* schließt sich mit Programmiermodus aus *)
						ELSE
							COMport := 0 ;
							TTYport := GetZahlParameter () ;		(* echter TTY - Anschluss über COMxx gewünscht *)
							IF (TTYport = 0) OR (TTYport > 100) THEN
								InitWindow ;
								InitFehlermeldung := TRUE ;
								Terminal.WriteF ('\n+++++ TTY-Port %s unzulässig', parm) ;
								TTYport := 0 ;
							END ;
						END ;
				| 'C' :
						TTYport := 0 ;
						COMport := 0 ;
						IF parm [2] > ' ' THEN
							COMport := GetZahlParameter () ;		(* echter TTY - Anschluss über COMxx gewünscht *)
							IF (COMport = 0) OR (COMport > 100) THEN
								InitWindow ;
								InitFehlermeldung := TRUE ;
								Terminal.WriteF ('\n+++++ TTY-Port %s unzulässig', parm) ;
								COMport := 0 ;
							END ;
						END ;
%END
				| '@' :									(* Eingabe - Simulation *)
						InitDefaults ;
						TerminalEingabe.InitSimulation (parm [2..HIGH(parm)]) ;

				ELSE
						InitWindow ;
						InitFehlermeldung := TRUE ;
						Terminal.WriteF ('\n+++++ unbekannter Startparameter : %s', parm) ;
				END ;
			ELSE
					InitWindow ;
					InitFehlermeldung := TRUE ;
					Terminal.WriteF ('\n+++++ falscher Startparameter : %s', parm) ;
			END ;
		END ;
	END ;
END GetParameter ;



BEGIN
	InitCommonControls ;

	SEBtrace := 1 ;
	InitTrace ;

	GetParameter ;

	InitWindow ;

	TraceF ('Terminal-Font : %s', Terminal.FontName) ;
	TraceF ('max. Zeile = %c', Terminal.MaxZeile) ;
	TraceF ('max. Spalte = %c', Terminal.MaxSpalte) ;

	DefaultQuelle := '' ;

	Init ;

%IF TTY %THEN
	IF TTYport <> 0 THEN
		IF TTY.InitTTY (TTYport) THEN
			TTYanschluss := TRUE ;
			Struktur.ProgrammierModus := FALSE ;	(* geht nicht bei TTY *)
		ELSE
			Terminal.WriteLn ;
			Terminal.WriteF ('+++++ TTY-Anschluss %c funktioniert nicht', TTYport) ;
		END ;
	END ;

	IF COMport <> 0 THEN
		IF TTY.InitCOM (COMport) THEN
			COManschluss := TRUE ;
			Struktur.ProgrammierModus := FALSE ;	(* geht nicht bei TTY *)
		ELSE
			Terminal.WriteLn ;
			Terminal.WriteF ('+++++ COM-Anschluss %c funktioniert nicht', TTYport) ;
		END ;
	END ;
%END

	IF termPause > Terminal.msPause THEN
		Terminal.msPause := termPause ;
	END ;

	InitMenue ;

	Eingabe ;

FINALLY
	DateiVerwaltung.DeleteDatenbasis ('') ;		(* alle selber kreierten löschen *)

	TermProtAus ;
	AblaufProtAus ;

	ExitTrace ;

	IF Struktur.AbschnittsModus THEN
		Editor (AblaufProtokoll) ;
	END ;
END TR440.
