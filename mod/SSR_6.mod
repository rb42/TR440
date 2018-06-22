
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_6;

(*	20.06.18	*)


FROM ASCII IMPORT
	eot, em, lf ;

FROM ZC1 IMPORT
	ZC1toANSI ;

IMPORT Strings, ExStrings, Terminal, FileIO, FileFunc,
	AsmTrace, Programm, debug, Assembler, TerminalEingabe ;

%IF %NOT WEB %THEN
IMPORT OFNup ;
	%IF TTY %THEN
	IMPORT TTY ;
	%END
%END

%IF WEB %THEN
IMPORT CGIup ;
%END

FROM AsmDisasm IMPORT
	CardToHex12 ;

FROM Trace IMPORT
	TraceF ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM AbwicklerUp IMPORT * ;

FROM EingabeGebiet IMPORT * ;

FROM TR440hp IMPORT
	AblaufProtEingeschaltet,
	AblaufProtDatei,
	DefaultQuelle,
	DefaultKDO,
	EingabeAbschluss,
	TTYanschluss,
	COManschluss ;

FROM Menue IMPORT
	ProgrammEingabeBeginn,
%IF %NOT WEB %THEN
	MenueBefehl,
%END
	MenueBefehlStehtAn ;




PROCEDURE _SSR_6_0 ;
	VAR	D,
		i,
		reladr,
		AA,
		EA :		CARDINAL ;
		wort :		CARDINAL64 ;
		extForm :	ARRAY [0..7] OF CHAR ;
		ch :		CHAR ;
BEGIN
	IF NOT SSRaufruf ('Eintragen Dumpbereich', 2) THEN
		RETURN ;
	END ;
	D := GetHalbwort (VBL + 1) ;
	IF D = 3 THEN
		(* alle adressierten KSP-Laufzeitgebiete des Operatorlaufs dumpen *)
	ELSIF ZustandsWahlschalter (ZustWahl2) AND AblaufProtEingeschaltet THEN	(* DPROTOKOLL,EIN *)
		wort := GetGanzwort (VBL + 2) ;
		AA := wort SHR 24 ;
		EA := wort BAND Cardinal24Bit ;
		extForm [6] := 0C ;
		FOR reladr := AA TO EA BY 2 DO
			IF (AA - reladr) MOD 16 = 0 THEN
				FileIO.WriteF (AblaufProtDatei, "\n%'05h", reladr) ;
				%IF WEB %THEN
					CGIup.WriteF ("\n%'05h", reladr) ;
				%END
			END ;
			wort := GetGanzwort (reladr) ;
			IF AktTypenkennung < 2 THEN
				extForm := '      ' ;
			ELSE
				FOR i := 0 TO 5 DO
					ch := ZC1toANSI [CHR (wort SHR (40 - i * 8))] ;
					IF ch < ' ' THEN
						ch := ' ' ;
					END ;
					extForm [i] := ch ;
				END ;
			END ;
			FileIO.WriteF (AblaufProtDatei, "  %c %'06h %'06h %s", ORD(AktTypenkennung), ORD(wort SHR 24), ORD(wort BAND Cardinal24Bit), extForm) ;
			%IF WEB %THEN
				CGIup.WriteF ("  %c %'06h %'06h %s", ORD(AktTypenkennung), ORD(wort SHR 24), ORD(wort BAND Cardinal24Bit), extForm) ;
			%END
		END ;
	END ;
END _SSR_6_0 ;


PROCEDURE _SSR_6_4 ;
BEGIN
	SSRaufruf ('Verändern Ablaufprotokoll', 1) ;
			(* ignorieren *)
END _SSR_6_4 ;


PROCEDURE _SSR_6_8 ;
BEGIN
	SSRaufruf ('Eintragen Kopftexterweiterung', 2) ;
			(* ignorieren *)
END _SSR_6_8 ;


PROCEDURE _SSR_6_12 ;
BEGIN
	IF SSRaufruf ('Eintragen Zeilen', 2) THEN

		IF ZustandsWahlschalter (ZustWahl2) THEN
			AusgabeListe ((* Konsole = *) FALSE, TRUE) ;
		END ;
	END ;
END _SSR_6_12 ;


PROCEDURE VorrangEntschluessler (Fluli, AAE, LNGE : CARDINAL) ;
	VAR	dbg :		BOOLEAN ;
		OLKnr,
		LNG,
		i :		CARDINAL ;
		wort :		CARDINAL64 ;
		ON,
		OLN :		ARRAY [0..12] OF CHAR ;
		tracedat :	ARRAY [0..255] OF CHAR ;
BEGIN
	OLKnr := AktOperator + 1 ;
	IF OLKnr > HIGH (Operatoren) THEN
		TraceF ('zu viele verschachtelte Operatorläufe') ;
		RETURN ;
	END ;
	ON := 'PS&ENTSCHL' ;
	OLN := 'PS&ENTSCH2' ;
	LNG := 131 ;

	TraceF ('Start Stufe %c : %s als %s', OLKnr, ON, OLN) ;
										AsmTrace.WriteF ("\n\t* Start Stufe %c : Programm %s als %s", OLKnr, ON, OLN) ;
	WITH Operatoren [OLKnr].SteuerInformation DO
		Kopf.AbsOLK := AktOLK ;
		Kopf.LNG := LNG ;
		Kopf.TYP := 1 ;			(* TYP = Startsatz *)
		wort := VAL (CARDINAL64, AktOLK) SHL 24 BOR VAL(CARDINAL64, (LNG SHL 16) + 1) ;
		ZuwGanzwort (Inhalt [0], 3, wort) ;
		ZuwGanzwort (Inhalt [1], 3, 52004H) ;	(* PM-Kopf *)
		ZuwGanzwort (Inhalt [2], 1, RegA.inh) ;
		ZuwGanzwort (Inhalt [3], 3, Fluli) ;	(* linkes HW Steuerbits :	46 :	nicht fragen nach Vorrang-Ende
											48 :	kein Standard-Protokoll
											47 :		Konsol-Protokoll wenn 48	*)
		IF RegA.inh = 0 THEN
			FOR i := 1 TO LNG-4 DO
				GetGanzwortU (AAE + (i - 1) * 2, Inhalt [i + 3]) ;
				IF AlarmGewesen THEN
					BREAK ;
				END ;
			END ;
		END ;
	END ;

	Programm.ProgrammPausieren ;


	dbg := debug.Befehle ;
	tracedat := Assembler.lastDateiname ;
	AsmTrace.ExitTrace ;

	IF Programm.ProgrammStarten (ON, OLN) THEN
		Programm.ProgrammLauf (Assembler.StartAdresse) ;
		Programm.ProgrammBeenden ;
	ELSE
		Programm.ProgrammFortsetzen ;
		TraceF ('+++ Entschlüssler nicht startbar') ;
		RETURN ;
	END ;

	AsmTrace.ExitTrace ;
	IF dbg THEN
		debug.an := TRUE ;
		debug.Befehle := TRUE ;
		AsmTrace.RestoreTrace (tracedat) ;
	END ;
	Assembler.lastDateiname := tracedat ;

	Programm.ProgrammFortsetzen ;
END VorrangEntschluessler ;


PROCEDURE _SSR_6_16 ;
	VAR	i,
		FLanz,
		Fluli,
		AAE,
		LNGE,
		TEKA,
		pos :		CARDINAL ;
		str :		ARRAY [0..771] OF CHAR ;		(* bei mehr als 768 Zeichen auf jeden Fall -> Gebiet *)
		str2 :		ARRAY [0..31] OF CHAR ;
		Eingabe,
		Konsole,
		Ablauf,
		found :		BOOLEAN ;

		datei :		ARRAY [1..9] OF FileIO.FILE ;
		dateiindex :	CARDINAL ;


	PROCEDURE PutFluli (ind : CARDINAL) ;
	BEGIN
		PutHalbwort (AAE + Fluli, (ind MOD 3) SHL 22 + ind DIV 3) ;
		Fluli := Fluli + 1 ;
	END PutFluli ;

BEGIN
	IF NOT SSRaufruf ('Eintragen Zeilen für Konsolprotokoll', 3) THEN
		RETURN ;
	END ;

	dateiindex := 0 ;

	TEKA := GetHalbwort (VBL + 1) BAND 0FH ;

	Konsole := ((TEKA BAND 2) <> 0) ;
	Eingabe := ((TEKA BAND 4) <> 0) AND Konsole ;
	Ablauf := (TEKA BAND 1) <> 0 ;

	RegA.inh := 0 ;				 			(* GBK :  kein Gebiet *)
	RegQ.TK := 2 ;
	RegQ.inh := 0 ;							(* EM / FLULI : Eingabe leer *)

	IF Konsole OR Ablauf THEN
		IF NOT AusgabeListe (Konsole AND NOT ZustandsWahlschalter (ZustWahl3) AND NOT AbschnittsModus , Ablauf AND ZustandsWahlschalter (ZustWahl2)) THEN
			RETURN ;
		END ;
	END ;

	IF Eingabe THEN
		IF NOT AbschnittsModus THEN
			Terminal.WriteString ('#:') ;
		END ;

		LOOP
			FS := 0 ;
			EingabeAbschluss := FALSE ;
			XABgefunden := FALSE ;				(* Wirkung von #XAB#. beenden *)

			ProgrammEingabeBeginn := TRUE ;
%IF WEB %OR %NOT TTY %THEN
				TerminalEingabe.ReadString (str) ;
%ELSE
			IF COManschluss THEN
				TTY.COMeingabeLesen (str) ;
			ELSIF TTYanschluss THEN
				TTY.EingabeLesen (str) ;
			ELSE
				TerminalEingabe.ReadString (str) ;
			END ;
%END
			ProgrammEingabeBeginn := FALSE ;

			IF Terminal.AbbruchWunsch THEN
				Terminal.AbbruchWunsch := FALSE ;
				AlleProgrammeAbbrechen := TRUE ;
				AlarmGewesen := TRUE ;
				EingabeAbschluss := FALSE ;
				RETURN ;
			END ;

			IF MenueBefehlStehtAn THEN
%IF %NOT WEB %THEN
				IF MenueBefehl () THEN
					Terminal.WriteLn ;
					Terminal.WriteString (' #: ') ;
				END ;
%END
			ELSIF str [0] <> 0C THEN
				EXIT ;
			ELSIF EingabeAbschluss THEN
				EingabeAbschluss := FALSE ;
				RETURN ;				(* leere Eingabe mit Bild Unten *)
			END ;
		END ;

		AAE := GetHalbwort (VBL + 4) ;
		LNGE := GetHalbwort (VBL + 5) ;

		IF Strings.Equal (str, '#.') AND NOT AbschnittsModus THEN	(* leere Eingabe *)
			EingabeAbschluss := FALSE ;
			RETURN ;
		ELSIF str [0] = '@' THEN					(* Eingabe aus Windows-Textdatei *)

			IF str [1] >= 'A' THEN
				FileFunc.ConstructFileName (str [1..HIGH(str)], DefaultKDO, DefaultKDO) ;
%IF %NOT WEB %THEN
			ELSIF OFNup.OFNkdo (DefaultKDO, FALSE) THEN
				DefaultKDO := OFNup.AktString ;
%END
			ELSE
				EingabeAbschluss := FALSE ;
				RETURN ;					(* keine Datei : leere Eingabe *)
			END ;
			DatNam := DefaultKDO ;
			dateiindex := 1 ;
			FileFunc.OpenFileEx (datei [dateiindex], DatNam, FileFunc.ReadOnlyDenyWrite,
						FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
			IF datei [dateiindex].status = 0 THEN
				InitEingabeGebiet ;
				FS := 0 ;
				REPEAT
					WHILE FileIO.ReadLn (datei [dateiindex], str) DO
						IF (str [0] = '@') AND (str [1] = '*') THEN		(* zu ignorierende Zeile in Eingabesimulations-Datei *)

						ELSIF (str [0] = '@') AND (str [1] >= 'A') THEN		(* geschachtelte Datei *)
							dateiindex := dateiindex+1 ;
							FileFunc.ConstructFileName (str [1..HIGH(str)], DefaultKDO, DatNam) ;
							FileFunc.OpenFileEx (datei [dateiindex], DatNam, FileFunc.ReadOnlyDenyWrite,
														FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
							IF datei [dateiindex].status <> 0 THEN
								TraceF ('+++ eingeschachtelte Datei nicht da : %s', DatNam) ;
				 				dateiindex := dateiindex-1 ;		(* geschachtelte Datei nicht gefunden *)
							END ;
						ELSE
							PutEingabeGebiet (str) ;
							PutEingabeGebiet (lf) ;
						END ;
					END ;
					FileIO.CloseFile (datei [dateiindex]) ;
					dateiindex := dateiindex-1 ;
				UNTIL dateiindex < 1 ;
			ELSE
				TraceF ('+++ Datei nicht da : %s', DatNam) ;
				EingabeAbschluss := FALSE ;
				RETURN ;
			END ;
		ELSIF ExStrings.EqualI (str, '#XEN#.') THEN
			AlleProgrammeAbbrechen := TRUE ;
			AlarmGewesen := TRUE ;
			EingabeAbschluss := FALSE ;
			RETURN ;						(* keine Datei : leere Eingabe *)
		ELSE
			InitEingabeGebiet ;
			LOOP
				Strings.FindNext ('#.', str, 0, found, pos) ;
				IF found THEN
					IF pos <> 0 THEN
						PutEingabeGebiet (str [0..pos-1]) ;
						str := '' ;
					END ;
					IF NOT AbschnittsModus THEN
						EXIT ;
					END ;
				END ;
				IF EingabeAbschluss THEN
					PutEingabeGebiet (str) ;
					EXIT ;
				END ;
				PutEingabeGebiet (str) ;
				PutEingabeGebiet (lf) ;
				Terminal.WriteLn ;
				LOOP
					EingabeAbschluss := FALSE ;
					FS := 0 ;

%IF WEB %OR %NOT TTY %THEN
						IF NOT TerminalEingabe.ReadString (str) THEN
							EingabeAbschluss := TRUE ;
						END ;
%ELSE
					IF COManschluss THEN
						TTY.COMeingabeLesen (str) ;
					ELSIF TTYanschluss THEN
						TTY.EingabeLesen (str) ;
					ELSE
						IF NOT TerminalEingabe.ReadString (str) THEN
							EingabeAbschluss := TRUE ;
						END ;
					END ;
%END
					IF MenueBefehlStehtAn THEN
%IF %NOT WEB %THEN
						IF MenueBefehl () THEN
							Terminal.WriteLn ;
							Terminal.WriteString (' #: ') ;
						END ;
%END
					ELSE
						EXIT ;
					END ;
				END ;
			END ;
		END ;

		IF AbschnittsModus THEN
			Wahlschalter := Wahlschalter BAND 0FF7FFFH ;		(* Z1 = GSP ausschalten *)
		END ;

		EingabeAbschluss := FALSE ;
		Fluli := ExitEingabeGebiet (AAE, LNGE) ;

		IF Fluli = 0 THEN
			RegQ.inh := 1000000H ;	 				(* keine FLULI da *)
			IF ZustandsWahlschalter (ZustWahl7) THEN
(*
				RegQ.inh := 3000000H ; 				(* alter ABW-Fehler : keine FLULI da bei gesetztem Z7 *)
*)
				SetZustandsWahlschalter (ZustWahl7, FALSE) ;	(* zusätzlich Z7 löschen *)
			END ;

		ELSIF ZustandsWahlschalter (ZustWahl7) THEN			(* Z7 gesetzt : keinen Entschlüssler einschachteln *)
			SetZustandsWahlschalter (ZustWahl7, FALSE) ;		(* aber dafür Z7 löschen *)
			RegQ.inh := VAL (CARDINAL64, Fluli) BOR 2000000H ; 	(* Eingabe + FLULI da *)

		ELSE
										(* Vorrang-Entschlüssler starten *)
			SetZustandsWahlschalter (ZustWahl6, TRUE) ;		(* Z6 : Entschlüssler wird auf 2. Stufe laufen *)
			VorrangEntschluessler (Fluli, AAE, LNGE) ;
			RegQ.inh := 3000000H ;					(* Eingabe wurde vom eingeschachtelten Entschlüssler verarbeitet *)
		END ;
	END ;
END _SSR_6_16 ;


PROCEDURE _SSR_6_20 ;
BEGIN
	SSRaufruf ('Eintragen Zeilen auf der KSM', 3) ;

	AusgabeListe (FALSE, TRUE) ;					(* vorläufig alles ins Ablaufprotokoll *)
END _SSR_6_20 ;


END SSR_6.
