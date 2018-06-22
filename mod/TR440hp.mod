
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE TR440hp;

(*	12.06.18	*)

IMPORT
	WIN32,
	WINUSER,
	WINX ;

FROM SYSTEM IMPORT
	CAST ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Terminal IMPORT
	PageDown,
	InitInputBuffer ;

FROM ExStorage IMPORT
	MemoryInUse,
	MaxMemoryUsed,
	CombineHeap ;

FROM Menue IMPORT
	Grundzustand,
	SetGrundzustand,
%IF %NOT WEB %THEN
	MenueBefehl,
%END
	MenueBefehlStehtAn ;

IMPORT Struktur, Befehle, AsmDisasm, Terminal, AsmTrace, TerminalEingabe ;
IMPORT Strings, ExStrings, Conversions, Environment, FileFunc, Registry ;
IMPORT Assembler, Namen, Register, Programm, debug, Trace, DateiVerwaltung, DateiBearbeitung, Gebiet, SSR_6 ;

FROM AsmDisasm IMPORT
	protZeile, protZeile2, TRIM ;


%IF WEB %THEN
	IMPORT CGIup ;
%ELSE

IMPORT OFNup ;

	%IF TTY %THEN
		IMPORT TTY ;
	%END

FROM AsmDisasm IMPORT
	ShowMemory ;
%END

FROM AbwicklerUp IMPORT
	 ZustandsWahlschalter,
	 ZustWahl2 ;

FROM Trace IMPORT
	TraceF ;


VAR
	bef,
	item,
	parm1,
	parm2 :			ARRAY [0..255] OF CHAR ;

	TermProtPuffer,
	AblaufProtPuffer :	POINTER TO ARRAY [0..8191] OF CHAR = NIL ;

	befehl,
	p1,
	p2,
	i :			CARDINAL ;


%IF %NOT WEB %THEN

PROCEDURE GetWert (str : ARRAY OF CHAR; VAR erg : CARDINAL) : BOOLEAN ;
	VAR	j,
		k :	CARDINAL ;
BEGIN
	j := 0 ;
	WHILE (str [j] = ' ') OR (str [j] = ',') DO
		INC (j) ;
	END ;
	k := LENGTH (str) - 1 ;
	WHILE (k > j) AND (str [k] = ' ') DO
		DEC (k) ;
	END ;
	IF str [j] = "'" THEN
		IF str [k] = "'" THEN
			IF Conversions.StrBaseToCard (str[j+1..k-1], 16, erg) THEN
				RETURN TRUE ;
			END ;
		ELSIF Conversions.StrBaseToCard (str[j+1..k], 16, erg) THEN
			RETURN TRUE ;
		END ;
	END ;

	IF Conversions.StrToCard (str [j..k], erg) THEN
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END GetWert ;


PROCEDURE Speichern ;
	VAR	adresse,
		wert :		CARDINAL ;
BEGIN
	IF ExStrings.GetNextItem (bef, i, parm1, ' ,') THEN		(* 1. Adresse *)
		IF GetWert (parm1, adresse) THEN
			IF ExStrings.GetNextItem (bef, i, parm2, ' ,') THEN		(* 2. Wert *)
				IF GetWert (parm2, wert) THEN
					Struktur.PutHalbwort (adresse, wert) ;
				ELSE
					protZeile ('+++++ unzul. Wert') ;
				END ;
			ELSE
				protZeile ('+++++ Wert fehlt') ;
			END ;
		ELSE
			protZeile ('+++++ unzul. Adresse') ;
		END ;
	END ;
END Speichern ;


PROCEDURE Zeigen ;
	VAR	adresse,
		wert,
		j :		CARDINAL ;
		inh :		CARDINAL64 ;
		str :		ARRAY [0..31] OF CHAR ;
BEGIN
	IF ExStrings.GetNextItem (bef, i, parm1, ' ,') THEN		(* 1. Adresse *)
		AsmDisasm.Zeigen (parm1) ;
	END ;
	Terminal.WriteLn ;
END Zeigen ;


PROCEDURE BefehleZeigen ;
	VAR	adresse,
		wert,
		j :		CARDINAL ;
		str :		ARRAY [0..31] OF CHAR ;
BEGIN
	IF ExStrings.GetNextItem (bef, i, parm1, ' ,') THEN		(* 1. Adresse *)
		AsmDisasm.BefehleZeigen (parm1) ;
	ELSE
		protZeile ('+++++ Adresse fehlt') ;
	END ;
	Terminal.WriteLn ;
END BefehleZeigen ;


PROCEDURE AsmBefehl (VAR befehl : CARDINAL) : BOOLEAN ;
BEGIN
	Assembler.imLauf2 := TRUE ;
	Assembler.AktZeile := parm1 ;
	befehl := AsmDisasm.GetBefehl (item) ;
	IF befehl < 1FFFFFFH THEN		(* Befehlsnamen erkannt *)
		IF ExStrings.GetNextItem (bef, i, parm1, ' ,') THEN		(* 1. Parameter *)
			IF GetWert (parm1, p1) THEN
				IF ExStrings.GetNextItem (bef, i, parm2, ' ,') THEN		(* 2. Parameter *)
					IF GetWert (parm2, p2) THEN
						befehl := befehl BOR ((p1 BAND 0FFH) SHL 8) BOR (p2 BAND 0FFH) ;
						RETURN TRUE ;
					ELSE
						protZeile ('+++++ unzul. 2. Parameter') ;
					END ;
				ELSE
					befehl := befehl BOR (p1 BAND 0FFFFH) ;
					RETURN TRUE ;
				END ;
			ELSE
				protZeile ('+++++ unzul. 1. Parameter') ;
			END ;
		ELSE
			protZeile ('+++++ Befehlsparameter fehlen') ;
		END ;
	ELSE
		protZeile ('+++++ unbekannter Befehlsname') ;
	END ;
	RETURN FALSE ;
END AsmBefehl ;

PROCEDURE PutDefaultQuelle ;
BEGIN
	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, TRUE) THEN
		IF NOT Registry.WriteRegString (TASkey, DefaultQuelle) THEN
			protZeile ('++++ Registry-Update misslungen') ;
		END ;
	ELSE
		protZeile ('++++ Registry-Zugriff misslungen') ;
	END ;
	CheckDefaultQuelle ;
END PutDefaultQuelle ;


PROCEDURE ProgNameGueltig (name : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	TRIM (name) ;
	IF name [0] <= ' ' THEN
		protZeile ('+++++ kein Programmname angegeben') ;
		RETURN FALSE ;
	ELSIF NOT DateiVerwaltung.CheckDateinamen (name) THEN
		protZeile ('+++++ ungültiger Programmname angegeben') ;
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END ProgNameGueltig ;



PROCEDURE upProgMerke (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgNameGueltig (name) THEN
		Programm.ProgrammMerken (name) ;
	END ;
END upProgMerke ;


PROCEDURE upProgStarte (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgNameGueltig (name) THEN
		Gebiet.InitGebiete ;		(* Notnagel mit evtl. Speicher-Leak *)
		IF Programm.ProgrammStarten (name, name) THEN
			Programm.ProgrammLauf (Assembler.StartAdresse) ;
			Programm.ProgrammBeenden ;
		END ;
	END ;
END upProgStarte ;


PROCEDURE upProgMontiere (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgNameGueltig (name) THEN
		Programm.ProgrammSpeichern (name) ;
	END ;
END upProgMontiere ;


PROCEDURE upProgProg (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgNameGueltig (name) THEN
		bef := name ;
		i := 0 ;
		IF CallAssembler (FALSE) THEN
			Programm.ProgrammMerken (name) ;
			IF Programm.ProgrammStarten (name, name) THEN
				Programm.ProgrammLauf (Assembler.StartAdresse) ;
				Programm.ProgrammBeenden ;
			END ;
		END ;
	END ;
END upProgProg ;


PROCEDURE upProgLoesche (name : ARRAY OF CHAR) ;
BEGIN
	IF ProgNameGueltig (name) THEN
		Programm.ProgrammLoeschen (name) ;
	END ;
END upProgLoesche ;


PROCEDURE ProgMerke ;
BEGIN
	ExStrings.GetNextItem (bef, i, parm1, ' ,') ;		(* Parameter = Programmname *)
	upProgMerke (parm1) ;
END ProgMerke ;


PROCEDURE ProgStarte ;
BEGIN
	ExStrings.GetNextItem (bef, i, parm1, ' ,') ;		(* Parameter = Programmname *)
	upProgStarte (parm1) ;
END ProgStarte ;


PROCEDURE ProgMontiere ;
BEGIN
	ExStrings.GetNextItem (bef, i, parm1, ' ,') ;		(* Parameter = Programmname *)
	upProgMontiere (parm1) ;
END ProgMontiere ;


PROCEDURE ProgProg ;
BEGIN
	upProgProg (bef [i..255]) ;
END ProgProg ;


PROCEDURE ProgLoesche ;
BEGIN
	ExStrings.GetNextItem (bef, i, parm1, ' ,') ;		(* Parameter = Programmname *)
	upProgLoesche (parm1) ;
END ProgLoesche ;


PROCEDURE GetDateiNamen (neu : BOOLEAN) : BOOLEAN ;
	VAR	prog :	ARRAY [0..255] OF CHAR ;
BEGIN
	GetDefaultQuelle ;
	IF ExStrings.GetNextItem (bef, i, parm1, ' ,') THEN		(* 1. Parameter = Dateiname *)
		IF neu OR Strings.Equal (parm1, '?') THEN
			parm1 := '' ;
			Assembler.lastDateiname := '' ;
		ELSE
			IF DefaultQuelle [0] = 0C THEN
				Environment.GetProgramName (prog) ;
				FileFunc.ConstructFileName (Struktur.TasQuellExtension, prog, DefaultQuelle) ;
			END ;
			FileFunc.ConstructFileName (parm1, DefaultQuelle, parm2) ;
			DefaultQuelle := parm2 ;
			IF FileFunc.FileExists (parm2) THEN
				PutDefaultQuelle ;
				RETURN TRUE ;
			ELSIF NOT neu THEN
				Terminal.WriteLn ;
				Terminal.WriteString ('+++++ Datei ') ;
				Terminal.WriteString (parm2) ;
				Terminal.WriteString (' nicht gefunden') ;
				Terminal.WriteLn ;
				RETURN FALSE ;
			END ;
		END ;
	END ;
	IF Assembler.lastDateiname [0] = 0C THEN
		IF OFNup.OFNtas (DefaultQuelle, neu) THEN
			parm2 := OFNup.AktString ;
			DefaultQuelle := parm2 ;
			PutDefaultQuelle ;
			RETURN TRUE ;
		ELSE
			RETURN FALSE ;
		END ;
	ELSE
		parm2 := '' ;
	END ;
	RETURN TRUE ;
END GetDateiNamen ;


PROCEDURE upCallAssembler (name : ARRAY OF CHAR ; mitLauf : BOOLEAN) : BOOLEAN ;
	VAR	ok :		BOOLEAN ;
		parts :		FileFunc.FileNameParts ;
BEGIN
	ok := FALSE ;

	FileFunc.ParseFileName (name, parts) ;
	Assembler.AktQuellName := parts.name ;

	Programm.InitDauer ;
	IF Assembler.Lauf1 (name) THEN
		IF Assembler.Lauf2 () THEN
			Terminal.WriteLn ;
			Terminal.WriteString ('Ende Assembler') ;
			Struktur.GesamtZahlTakte := 0 ;
			Programm.ProtDauer ;

			IF mitLauf THEN
				Gebiet.InitGebiete ;		(* auch wenn dabei Verschnitt entsteht *)
				Programm.ProgrammLauf (Assembler.StartAdresse) ;
			END ;

			ok := TRUE ;
		ELSE
			Terminal.WriteLn ;
			Terminal.WriteString ('Ende Assembler') ;
			Programm.ProtDauer ;
		END ;
	END ;

	Assembler.ExitProtokoll ;

	RETURN ok ;
END upCallAssembler ;


PROCEDURE CallAssembler (mitLauf : BOOLEAN) : BOOLEAN ;
BEGIN
	(*
	IF mitLauf AND Strings.Equal (bef [i..255], '*') THEN
		Programm.InitDauer ;
		Struktur.GesamtZahlTakte := 0 ;
		Programm.ProgrammLauf (Assembler.StartAdresse) ;
		RETURN TRUE ;
	END ;
	*)

	Assembler.AktQuellName := parm2 ;

	IF NOT GetDateiNamen (FALSE) THEN
		RETURN FALSE ;
	END ;

	Struktur.StdLeitblock ;

	RETURN upCallAssembler (parm2, mitLauf) ;

END CallAssembler ;


PROCEDURE InterpreterSpeichern ;
BEGIN
	IF GetDateiNamen (TRUE) THEN
		protZeile2 ('Speichern', parm2) ;
		IF NOT Assembler.Abspeichern (parm2) THEN
			protZeile2 ('+++++ Fehler beim Speichern', parm2) ;
		ELSE
			PutDefaultQuelle ;
		END ;
	END ;
END InterpreterSpeichern ;


PROCEDURE EditorAufrufen ;
BEGIN
	IF GetDateiNamen (FALSE) THEN
		protZeile2 ('Editorstart', parm2) ;
		Assembler.Editor (parm2) ;
	END ;
END EditorAufrufen ;


PROCEDURE DirektBefehl ;
	VAR	l,
		befehl :	CARDINAL ;
		str,
		str2 :		ARRAY [0..31] OF CHAR ;
BEGIN
	parm1 := bef [i..LENGTH(bef)] ;
	AsmDisasm.TRIM (parm1) ;
	Assembler.imLauf2 := TRUE ;
	Assembler.AktZeile := parm1 ;
	IF parm1 [0] = "'" THEN
		l := LENGTH(parm1)-1 ;
		IF parm1 [l] = "'" THEN
			DEC (l) ;
		END ;
		Conversions.StrBaseToCard (parm1 [1..l], 16, befehl) ;
		AsmDisasm.GetMnemo (befehl, str2) ;
		AsmDisasm.CardToHex6 (befehl, str) ;
		Strings.Append ('  ', str) ;
		Strings.Append (str2, str) ;
	ELSIF Assembler.GetBefehlsZeile (parm1) THEN
		AsmDisasm.GetMnemo (Assembler.AktBefehlswort, str2) ;
		AsmDisasm.CardToHex6 (Assembler.AktBefehlswort, str) ;
		Strings.Append ('  ', str) ;
		Strings.Append (str2, str) ;
	ELSE
		str := '++++ Syntax-Fehler' ;
	END ;
	Terminal.Write (TAB) ;
	Terminal.WriteString (str) ;
END DirektBefehl ;


PROCEDURE Tracebefehl ;
	VAR	j :	CARDINAL ;
BEGIN
	AsmDisasm.TRIM (bef) ;
	IF ExStrings.GetNextItem (bef, i, parm1, ' ') THEN		(* 1. Parameter = Trace-Spezifikationen *)

		AsmDisasm.TraceBefehl (parm1) ;
	ELSE
		parm1 := '?' ;
		AsmDisasm.TraceBefehl (parm1) ;
	END ;
END Tracebefehl ;


PROCEDURE BackTrace ;
	VAR	anz :	CARDINAL ;
BEGIN
	AsmDisasm.TRIM (bef) ;
	IF ExStrings.GetNextItem (bef, i, parm1, ' ') THEN		(* 1. Parameter = Anzahl Backtrace-Zeilen *)
		IF GetWert (parm1, anz) THEN
			IF (anz = 0) OR (anz > 99) THEN
				AsmTrace.BacktraceAnzahl := anz ;
				RETURN ;
			END ;
		END ;
	END ;
	Terminal.WriteString ('++++ Syntax-Fehler : ') ;
	Terminal.WriteString (bef) ;
END BackTrace ;


PROCEDURE upShowMemory ;
BEGIN
	AsmDisasm.ShowMemory ;
	CombineHeap ;
	Struktur.CheckKSPbelegung ;
	Terminal.WriteF ("\n\nMemory : %cKB / %cKB benutzt", (MemoryInUse () + 512) DIV 1024, (MaxMemoryUsed () + 512) DIV 1024) ;
	Terminal.WriteF (  "\nv. KSP : %cKW / %cKW belegt\n", Struktur.KSPanz, Struktur.KSPmax) ;
END upShowMemory ;



PROCEDURE MemoryReservieren ;
	VAR	anz,
		adr,
		gs :	CARDINAL ;
BEGIN
	AsmDisasm.TRIM (bef) ;
	IF ExStrings.GetNextItem (bef, i, parm1, ' ') THEN		(* 1. Parameter = Anzahl Großseiten *)
		IF GetWert (parm1, anz) THEN
			IF (anz > 0) AND (anz <= 64) THEN
				Struktur.SpeicherReservieren (anz) ;
				RETURN ;
			ELSE
				protZeile ('+++++ unzul. Wert') ;
			END ;
		END ;
		protZeile ('+++++ unzulässig') ;
	ELSE
		upShowMemory ;
	END ;
END MemoryReservieren ;

%END

PROCEDURE insAblaufProtokoll (ch : CHAR) ;
BEGIN
	IF AblaufProtEingeschaltet THEN
		FileFunc.WriteChar (AblaufProtDatei, ch) ;
	END ;
%IF WEB %THEN
	CASE ORD(ch) OF
	13 :					(* CR *)
		(* ignorieren *)
	|
	10 :					(* LF *)
		CGIup.PREwriteLn ;
	|
	12 :					(* FF *)
		CGIup.WriteF ('<p style="page-break-before:always">') ;
	ELSE
		CGIup.PREwriteString (ch) ;
	END ;
%END
END insAblaufProtokoll ;


PROCEDURE ProtokollAusgabe (zeile : ARRAY OF CHAR) ;		(* wird von Terminal aufgerufen *)
BEGIN
	FileFunc.WriteLine (TermProtDatei, zeile) ;
	(*
	IF ZustandsWahlschalter (ZustWahl2) THEN
		IF AblaufProtEingeschaltet THEN
			FileFunc.WriteLine (AblaufProtDatei, zeile) ;
		END ;
	END ;
	*)
END ProtokollAusgabe ;


PROCEDURE TermProtEin (neu : BOOLEAN) ;
BEGIN
	IF NOT TermProtEingeschaltet AND (TerminalProtokoll [0] > ' ') THEN
		DateiVerwaltung.CreatePROTO ;
		IF TermProtPuffer = NIL THEN
			NEW (TermProtPuffer) ;
		END ;
		IF neu THEN
			FileFunc.CreateFile (TermProtDatei, TerminalProtokoll) ;
		ELSE
			FileFunc.OpenCreateFile (TermProtDatei, TerminalProtokoll, FileFunc.ReadWriteDenyAll) ;
		END ;
		IF TermProtDatei.status = 0 THEN
			FileFunc.SetFileBuffer (TermProtDatei, TermProtPuffer ^ ) ;
			IF NOT neu THEN
				FileFunc.SetFilePos (TermProtDatei, FileFunc.FileLength (TermProtDatei)) ;
			END ;
		END ;
		IF TermProtDatei.status = 0 THEN
			TermProtEingeschaltet := TRUE ;
			Terminal.Protokoll := ProtokollAusgabe ;
			Terminal.ProtokollAngemeldet := TRUE ;
		ELSE
			Terminal.WriteF ('\n+++++ Terminal-Protokolldatei %s nicht erzeugbar\n', TerminalProtokoll) ;
		END ;
	END ;
END TermProtEin ;



PROCEDURE TermProtAus ;
BEGIN
	IF TermProtEingeschaltet THEN
		TermProtEingeschaltet := FALSE ;
		Terminal.ProtokollAngemeldet := FALSE ;
		FileFunc.CloseFile (TermProtDatei) ;
	END ;
END TermProtAus ;


PROCEDURE AblaufProtEin (neu : BOOLEAN) ;
BEGIN
	IF NOT AblaufProtEingeschaltet AND (AblaufProtokoll [0] > ' ') THEN
		DateiVerwaltung.CreatePROTO ;
		IF AblaufProtPuffer = NIL THEN
			NEW (AblaufProtPuffer) ;
		END ;
		IF neu THEN
			FileFunc.CreateFile (AblaufProtDatei, AblaufProtokoll) ;
		ELSE
			FileFunc.OpenCreateFile (AblaufProtDatei, AblaufProtokoll, FileFunc.ReadWriteDenyAll) ;
		END ;
		IF AblaufProtDatei.status = 0 THEN
			FileFunc.SetFileBuffer (AblaufProtDatei, AblaufProtPuffer ^ ) ;
			IF NOT neu THEN
				FileFunc.SetFilePos (AblaufProtDatei, FileFunc.FileLength (AblaufProtDatei)) ;
			END ;
		END ;
		IF AblaufProtDatei.status = 0 THEN
			AblaufProtEingeschaltet := TRUE ;
		ELSE
			Terminal.WriteF ('\n+++++ AblaufProtokolldatei %s nicht erzeugbar\n', AblaufProtokoll) ;
		END ;
	END ;
END AblaufProtEin ;



PROCEDURE AblaufProtAus ;
BEGIN
	IF AblaufProtEingeschaltet THEN
		AblaufProtEingeschaltet := FALSE ;
		FileFunc.CloseFile (AblaufProtDatei) ;
	END ;
END AblaufProtAus ;


PROCEDURE CheckDefaultQuelle ;
	VAR	parts :	FileFunc.FileNameParts ;
		str :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF DefaultQuelle [0] > ' ' THEN
		FileFunc.ParseFileName (DefaultQuelle, parts) ;
		parts.name := '' ;
		parts.extension := '' ;

		FileFunc.AssembleParts (parts, Assembler.ListingDirectory) ;
		Strings.Append ('Protokolle\', Assembler.ListingDirectory) ;
		FileFunc.CreateDirTree (Assembler.ListingDirectory) ;

		FileFunc.AssembleParts (parts, AsmTrace.TraceDirectory) ;
		Strings.Append ('Trace\', AsmTrace.TraceDirectory) ;
		FileFunc.CreateDirTree (AsmTrace.TraceDirectory) ;

		FileFunc.AssembleParts (parts, str) ;
		str [LENGTH(str)-1] := 0C ;
		FileFunc.ParseFileName (str, parts) ;
		parts.name := '' ;
		FileFunc.AssembleParts (parts, KdoDirectory) ;
		Strings.Append ('Kommandos\', KdoDirectory) ;
		FileFunc.CreateDirTree (KdoDirectory) ;

	END ;
END CheckDefaultQuelle ;


PROCEDURE GetDefaultQuelle ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE)
	OR Registry.InitRegistry (Registry.LocalMachine, DefaultPfad, FALSE) THEN
		IF Registry.ReadRegString (TASkey, str) THEN
			DefaultQuelle := str ;
			CheckDefaultQuelle ;
		END ;
	END ;
END GetDefaultQuelle ;


PROCEDURE EntschluesslerStart ;		(* in Grundstufe *)
BEGIN
	(*
	debug.alle (FALSE) ;
	*)
	IF NOT DateiVerwaltung.DatentraegerOk THEN
		RETURN ;
	END ;
	Gebiet.AlleGebieteLoeschen ;
	DateiVerwaltung.DeleteSTDDB ;
	DateiVerwaltung.CreateBKZ ;
	IF Programm.ProgrammStarten ('PS&ENTSCHL', 'PS&ENTSCHL') THEN
		Programm.ProgrammLauf (Assembler.StartAdresse) ;
		Programm.ProgrammBeenden ;
		DateiBearbeitung.AlleDateienSchliessen ;
	END ;
END EntschluesslerStart ;


PROCEDURE XBGunzul ;
VAR
	bef : ARRAY [0..7] OF CHAR ;
BEGIN
	IF Struktur.AbschnittsModus THEN
		bef := 'XBA' ;
	ELSE
		bef := 'XBG' ;
	END ;
	Terminal.WriteF ('\n+++++ unzul. %s :  #%s,BEN=...,BKZ=...,FKZ=...#.  erwartet\n', bef) ;
END XBGunzul ;


PROCEDURE XBG ;
	VAR	spezname,
		spezwert,
		befnam :	ARRAY [0..63] OF CHAR ;
		j, l,
		wert :		CARDINAL ;
		fertig :	BOOLEAN ;
BEGIN ;
	IF NOT DateiVerwaltung.DatentraegerOk THEN
		RETURN ;
	END ;
	i := 0 ;
	fertig := FALSE ;
	ExStrings.GetNextItem (bef, i, parm1, ',') ;
	TRIM (parm1) ;
	IF NOT Strings.Equal (parm1, '#XBG') AND NOT Strings.Equal (parm1, '#XBA') THEN
		XBGunzul ;
		RETURN ;
	END ;

	InitInputBuffer ;

	WHILE ExStrings.GetNextItem (bef, i, parm1, ',') DO
		TRIM (parm1) ;
		l := LENGTH (parm1) ;
		IF (l > 1) AND Strings.Equal (parm1 [l-2..l-1], '#.') THEN
			parm1 [l-2] := 0C ;
			fertig := TRUE ;
		END ;
		j := 0 ;
		IF  ExStrings.GetNextItem (parm1, j, spezname, '=')
		AND ExStrings.GetNextItem (parm1, j, spezwert, ',') THEN
			TRIM (spezname) ;
			TRIM (spezwert) ;
			IF    Strings.Equal (spezname, 'BEN') THEN
				Struktur.AktAuftrag.BEN := spezwert ;
				IF Struktur.AktAuftrag.BKZ [0] = 0C THEN
					Struktur.AktAuftrag.BKZ := Struktur.AktAuftrag.BEN [0..5] ;
				END ;
			ELSIF Strings.Equal (spezname, 'BKZ') THEN
				Struktur.AktAuftrag.BKZ := spezwert ;
			ELSIF Strings.Equal (spezname, 'FKZ') THEN
				Struktur.AktAuftrag.FKZ := spezwert ;
			ELSIF Strings.Equal (spezname, 'SBG') THEN
			ELSIF Strings.Equal (spezname, 'KSB') THEN
			ELSIF Strings.Equal (spezname, 'TSB') THEN
			ELSIF Strings.Equal (spezname, 'PSB') THEN
			ELSIF Strings.Equal (spezname, 'RZS') THEN
				IF Conversions.StrToCard (spezwert, wert) THEN
					Struktur.MaximalZahlTakte := VAL (CARDINAL64, wert) * Struktur.TakteProMikroSekunde * 1000000 ;
				END ;
			ELSIF Strings.Equal (spezname, 'DRS') THEN
			ELSE
				BREAK ;
			END ;
		ELSE
			XBGunzul ;
			BREAK ;
		END ;
		IF fertig THEN
			IF (Struktur.AktAuftrag.BEN [0] <= ' ') THEN
				BREAK ;
			END ;

 			IF Struktur.AktAuftrag.BKZ [0] < ' ' THEN
				Struktur.AktAuftrag.BKZ := Struktur.AktAuftrag.BEN [0..5] ;
			END ;

			SetGrundzustand (FALSE) ;

			AblaufProtEin (TRUE) ;		(* neues AblaufProtokoll erzeugen *)

			IF NOT Struktur.AbschnittsModus THEN
				FOR j := 1 TO 3 DO
					Terminal.WriteLn ;
				END ;
				Terminal.WriteString ('** Beginn TR440 - Gespräch') ;
			END ;

			EntschluesslerStart ;

			AblaufProtAus ;

			SetGrundzustand (TRUE) ;
			Struktur.StdLeitblock ;

			FOR j := 1 TO 3 DO
				Terminal.WriteLn ;
			END ;

			IF NOT Struktur.AbschnittsModus THEN
				Terminal.WriteString ('** Ende TR440 - Gespräch') ;

				FOR j := 1 TO 6 DO
					Terminal.WriteLn ;
				END ;
			END ;

			RETURN ;
		END ;
	END ;
	XBGunzul ;
END XBG ;

%IF %NOT WEB %THEN

PROCEDURE InterpreterHilfe ;
BEGIN
	Terminal.WriteLn ;
	protZeile ('Interpreter-Befehle :') ;
	protZeile2 ('Trace xxx',	'Trace-Bedingungen setzen/löschen') ;
	protZeile2 ('Disasm adr',	'Speicher befehlsweise ab angegebener Adresse disassemblieren') ;
	protZeile2 ('Show adr',		'Speicherinhalt ganzwortweise ab Adresse zeigen') ;
	protZeile2 ('Put adr, wert',	'Halbwort-Wert auf Adresse speichern') ;
	protZeile2 ('Save dateiname',	'alle Interpreter-Befehle in Datei schreiben') ;
	protZeile2 ('START adr',	'Ablageadresse festlegen') ;
	protZeile2 ('XBASIS adr',	'Indexbasis auf Wert setzen') ;
	protZeile2 ('UNTPR xx',		'U-Register setzen') ;
	protZeile2 ('REG     ',		'alle Register zeigen') ;
	protZeile2 ('Esc    ',		'zurück aus Interpreter') ;
	Terminal.WriteLn ;
END InterpreterHilfe ;


PROCEDURE Interpreter ;
	VAR	zeile :	ARRAY [0..255] OF CHAR ;
		str,
		str2 :	ARRAY [0..31] OF CHAR ;
		Adresse,
		zw,
		x,
		y :	CARDINAL ;
BEGIN
	Struktur.StdLeitblock ;				(* Schreibschütze vom vorigen Programm weg *)

	Assembler.StartX := 0F000H ;
	Assembler.StartU := 240 ;
	Assembler.StartAdresse := 10000H ;

	Assembler.InitAsm ;				(* immer von 0 an beginnen *)
	Assembler.InitProgrammlauf ;

	Assembler.imInterpreter := TRUE ;
	Assembler.imLauf2 := TRUE ;
	Programm.InitRegZeigen ;
	Programm.RegAlleZeigen ;
	Struktur.GesamtZahlTakte := 0 ;
	Struktur.InitAssocIndex ;

	Namen.AktQuelle := Namen.QuelleEintragen ('Interpreter') ;
	Namen.AktWB := Namen.WoerterbuchEintragen ('Interpreter') ;

	LOOP
		Terminal.WriteLn ;
		Terminal.WriteString ('Inter#: ') ;
		IF Terminal.AbbruchWunsch THEN
			EXIT ;
		END ;
		IF NOT TerminalEingabe.ReadString (zeile) THEN
			EXIT ;
		END ;
		AsmDisasm.PutIntoClipboard (zeile) ;
		Strings.Capitalize (zeile) ;
		AsmDisasm.TRIM (zeile) ;

		   IF Strings.Equal (zeile, '?') THEN
		   	InterpreterHilfe ;
		ELSIF Strings.Equal (zeile [0..4], 'TRACE') THEN
			bef := zeile ;
			i := 6 ;
			Tracebefehl ;
		ELSIF Strings.Equal (zeile [0..3], 'SHOW') THEN
			bef := zeile ;
			i := 5 ;
			Zeigen ;
		ELSIF Strings.Equal (zeile [0..2], 'PUT') THEN
			bef := zeile ;
			i := 4 ;
			Speichern ;
		ELSIF Strings.Equal (zeile [0..5], 'DISASM') THEN
			bef := zeile ;
			i := 7 ;
			BefehleZeigen ;
		ELSIF Strings.Equal (zeile [0..3], 'SAVE') THEN
			bef := zeile ;
			i := 5 ;
			InterpreterSpeichern ;
		ELSIF Strings.Equal (zeile [0..2], 'REG') THEN
			Programm.RegAlleZeigen ;
		ELSIF Strings.Equal (zeile [0..4], 'START') THEN
			IF Assembler.GetZahl (zeile [6..255], Assembler.AblageAdresse) THEN
			END ;
		ELSIF Strings.Equal (zeile [0..5], 'XBASIS') THEN
			IF Assembler.GetZahl (zeile [7..255], Register.RegX) THEN
			END ;
		ELSIF Strings.Equal (zeile [0..4], 'UNTPR') THEN
			IF Assembler.GetZahl (zeile [6..255], zw) THEN
				Register.RegU := zw ;
			END ;
		ELSE
			Terminal.GetPosition (x, y) ;
			IF x < 30 THEN
				Terminal.Position (32, y) ;
			ELSE
				Terminal.Write (TAB) ;
			END ;
			IF zeile [0] > ' '  THEN
				Assembler.AktZeile := zeile ;
				IF Assembler.GetBefehlsZeile (zeile) THEN

					Namen.ZeileEintragen (Assembler.AblageAdresse, zeile) ;			(* Zeile merken für später *)
					TraceF ('\t\t\t%s', zeile) ;

					AsmDisasm.CardToHex4 (Assembler.AblageAdresse, str) ;
					Terminal.WriteString (str) ;
					Terminal.WriteString ('  ') ;
					AsmDisasm.GetMnemo (Assembler.AktBefehlswort, str2) ;
					AsmDisasm.CardToHex6 (Assembler.AktBefehlswort, str) ;
					Strings.Append ('  ', str) ;
					Strings.Append (str2, str) ;
					Terminal.WriteString (str) ;

					Programm.ProgrammZeile (Assembler.AktBefehlswort) ;
					IF Register.RegF <> Assembler.AblageAdresse THEN
						Programm.SprungZeigen ;
					END ;
				END ;
			END ;
		END ;
	END ;
END Interpreter ;


PROCEDURE InitInterpreter ;
BEGIN
	Assembler.InitProgrammlauf ;
	Register.RegX := 0F000H ;
	Register.RegU := 240 ;
END InitInterpreter ;


PROCEDURE Hilfe ;
BEGIN
	protZeile ('Befehle :') ;
	protZeile2 ('#XBG,BEN=...#.',	'Gespräch beginnen') ;
	protZeile2 ('#XEN#.     ',	'Gespräch beenden') ;
	protZeile2 ("             ",	' ') ;
	protZeile2 ('Memory nn',	'Memory auf Anzahl Großseiten setzen (à 32 K Worte)') ;
	protZeile2 ('Trace xxx',	'Trace-Bedingungen setzen/löschen') ;
	protZeile2 ("             ",	' ') ;
	IF cmdEigenerAssembler THEN
		protZeile2 ('Tas datei',	'.TAS - Datei assemblieren und im Speicher ablegen') ;
		protZeile2 ('Run datei',	'.TAS - Datei assemblieren und als Programm starten') ;
		protZeile2 ('Edit datei',	'.TAS - Datei mit Standard-Editor bearbeiten') ;
		protZeile2 ("             ",	' ') ;
	END ;
	protZeile2 ('Disasm adr',	'Speicher befehlsweise ab angegebener Adresse disassemblieren') ;
	protZeile2 ('BACKTRACE nnnn',	'Anzahl Zeilen für Dateitrace-Puffer setzen') ;
	protZeile2 ('Show adr',		'Speicherinhalt ganzwortweise ab Adresse zeigen') ;
	protZeile2 ('Put adr, wert',	'Halbwort-Wert auf Adresse speichern') ;
	protZeile2 ('Inter   ',		'Assembler-Interpreter starten') ;
	protZeile2 ('Dir befehl',	'angegebenen Befehl assemblieren') ;
	protZeile2 ("Dir 'hexwort'",	'angegebenes Wort als Befehl disassemblieren') ;
	protZeile2 ("             ",	' ') ;
	IF cmdEigenerAssembler THEN
		protZeile2 ("Merke name",	'assembliertes Programm zum Start unter dem Namen merken') ;
		protZeile2 ("Prog name",	'angegebenes Programm assemblieren, merken und starten') ;
		protZeile2 ("Loesche name",	'angegebenes gemerktes Programm löschen') ;
		protZeile2 ("             ",	' ') ;
		protZeile2 ("Montiere name",	'aktuelles Programm in &STDDB speichern') ;
		protZeile2 ("Starte name",	'angegebenes gemerktes/montiertes Programm starten') ;
	END ;
END Hilfe ;


PROCEDURE TestBefehle () : BOOLEAN ;
BEGIN

	   IF Strings.Equal (item, '?') OR Strings.Equal (item, 'HILFE') OR Strings.Equal (item, 'HELP') THEN
		Hilfe ;

	ELSIF Strings.Equal (item, 'DIR') THEN
		DirektBefehl ;

	ELSIF Strings.Equal (item, 'PUT') THEN
		Speichern ;

	ELSIF Strings.Equal (item, 'SHOW') THEN
		Zeigen ;

	ELSIF Strings.Equal (item, 'DISASM') THEN
		BefehleZeigen ;

	ELSIF Strings.Equal (item, 'TRACE') THEN
		Tracebefehl ;

	ELSIF Strings.Equal (item, 'BACKTRACE') THEN
		BackTrace ;

	ELSIF Strings.Equal (item, 'MEMORY') THEN
		MemoryReservieren ;

	ELSIF Strings.Equal (item, 'INTER') THEN
		Interpreter ;

	ELSIF cmdEigenerAssembler THEN
		   IF Strings.Equal (item, 'RUN') THEN
			CallAssembler (TRUE) ;

		ELSIF Strings.Equal (item, 'ASM') OR Strings.Equal (item, 'TAS') THEN
			CallAssembler (FALSE) ;

		ELSIF Strings.Equal (item, 'EDIT') THEN
			EditorAufrufen ;

		ELSIF Strings.Equal (item, 'MERKE') THEN
			ProgMerke ;

		ELSIF Strings.Equal (item, 'STARTE') OR Strings.Equal (item, 'START') THEN
			ProgStarte ;

		ELSIF Strings.Equal (item, 'PROG') THEN
			ProgProg ;

		ELSIF Strings.Equal (item, 'MONTIERE') THEN
			ProgMontiere ;

		ELSIF Strings.Equal (item, 'LOESCHE') THEN
			ProgLoesche ;
		ELSE
			protZeile ('+++ Befehl unbekannt') ;
		END ;
	ELSE
		protZeile ('+++ unbekannter Befehl') ;
	END ;

	RETURN TRUE ;

END TestBefehle ;

%END


PROCEDURE InitDefaults ;
BEGIN
	IF Registry.InitRegistry (Registry.CurrentUser, DefaultPfad, FALSE)
	OR Registry.InitRegistry (Registry.LocalMachine, DefaultPfad, FALSE) THEN
		Registry.ReadRegString (TASkey, DefaultQuelle) ;
		Registry.ReadRegString (LSTkey, DefaultLST) ;
		Registry.ReadRegString (TRACkey, DefaultTRAC) ;
		Registry.ReadRegString (KDOkey, DefaultKDO) ;
		Registry.ReadRegString (QuellbasisKey, Quellbasis) ;
	END ;
END InitDefaults ;


VAR
	ReadFlusyGewesen :	BOOLEAN ;


PROCEDURE ReadExtension (VAR ch : CHAR) : BOOLEAN ;	(* wird in 'Terminal.ReadString' nach jedem gelesenen Zeichen aufgerufen *)
BEGIN
	IF NOT Grundzustand OR NOT Struktur.ProgrammierModus THEN
		CASE ch OF
		  0C :	(* ignorieren *)
		| 'ä' :
			ch := '#' ;
			ReadFlusyGewesen := TRUE ;
		| '#' :
			ReadFlusyGewesen := TRUE ;
		| '.' :
			IF ReadFlusyGewesen THEN
				EingabeAbschluss := TRUE ;
				ReadFlusyGewesen := FALSE ;
				RETURN TRUE ;		(*   #.   als Eingabeende *)
			END ;
		| PageDown :
			ReadFlusyGewesen := FALSE ;
			EingabeAbschluss := TRUE ;
			ch := Terminal.Enter ;
			RETURN TRUE ;			(*   Bild Unten   auch   als Eingabeende *)
		ELSE
			ReadFlusyGewesen := FALSE ;
		END ;
	END ;
	RETURN FALSE ;
END ReadExtension ;


PROCEDURE Eingabe ;
	VAR	b :	BOOLEAN ;
BEGIN

%IF %NOT WEB %THEN
	Assembler.frischAssembliert := TRUE ;
	Programm.ProgrammMerken ('TR440') ;
	Programm.ProgrammStarten ('TR440', 'TR440') ;
	Assembler.frischAssembliert := FALSE ;

	Terminal.ReadExt := ReadExtension ;
	Terminal.ReadExtAngemeldet := TRUE ;
%END

	Struktur.StdLeitblock ;							(* Leitblock normieren *)

%IF %NOT WEB %THEN
	InitInterpreter ;
%END

	InitDefaults ;

	LOOP
		SetGrundzustand (TRUE) ;

		Terminal.AbbruchWunsch := FALSE ;

		Struktur.AlleProgrammeAbbrechen := FALSE ;
%IF %NOT WEB %THEN
		IF MenueBefehlStehtAn THEN
			MenueBefehl ;
		END ;
		IF NOT Struktur.AbschnittsModus THEN
	    		Terminal.WriteLn ; Terminal.WriteString (' : ') ;

			IF Terminal.AbbruchWunsch THEN
				EXIT ;
			END ;
		END ;
%END

		LOOP
			SetGrundzustand (TRUE) ;

%IF WEB %THEN
				b := TerminalEingabe.ReadString (bef) ;
				IF NOT b THEN
					EXIT ;
				END ;
%ELSE

	%IF TTY %THEN
			IF COManschluss THEN
				TTY.COMeingabeLesen (bef) ;
				b := TRUE ;
			ELSIF TTYanschluss THEN
				TTY.EingabeLesen (bef) ;
				b := TRUE ;
			ELSE
				b := TerminalEingabe.ReadString (bef) ;
			END ;
	%ELSE
			b := TerminalEingabe.ReadString (bef) ;
	%END
%END
			IF Terminal.AbbruchWunsch THEN
				RETURN ;
			END ;
			TRIM (bef) ;
			IF MenueBefehlStehtAn THEN
%IF %NOT WEB %THEN
				IF MenueBefehl () THEN
			    		Terminal.WriteLn ; Terminal.WriteString (' : ') ;
				END ;
%END
			ELSIF bef [0] <> 0C THEN
				EXIT ;
			END ;
		END ;

		IF b THEN
			IF NOT Struktur.AbschnittsModus THEN
				AsmDisasm.PutIntoClipboard (bef) ;
			END ;
			Strings.Capitalize (bef) ;
			IF Struktur.AbschnittsModus THEN
				IF Strings.Equal (bef [0..3], '#XBA') THEN
					Terminal.WriteLn ;
					XBG ;
					bef := '' ;
				END ;
			ELSE
				IF Strings.Equal (bef [0..3], '#XBG') THEN
					Terminal.WriteLn ;
					XBG ;
					bef := '' ;
				END ;
			END ;
		END ;

		IF Struktur.AbschnittsModus THEN
			EXIT ;
		END ;

		Terminal.WriteLn ;

		i := 0 ;
		IF b AND ExStrings.GetNextItem (bef, i, item, ' ') THEN
			TRIM (item) ;
			IF item [0] = 0C THEN
				(* nix tun *)
			ELSIF Strings.Equal (item, 'EXIT') OR Strings.Equal (item, '#XEN#.') THEN
				EXIT ;
%IF %NOT WEB %THEN
			ELSIF Struktur.ProgrammierModus THEN
				IF NOT TestBefehle () THEN
					EXIT ;
				END ;
%END
			END ;
		END ;
	END ;

END Eingabe ;


END TR440hp.
