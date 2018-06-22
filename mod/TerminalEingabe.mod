
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE TerminalEingabe;

(*	12.06.18	*)


(*	@*kdodatei	Eingabe aus kdodatei beginnen								*)
(*				Steuerungen in kdodatei :							*)
(*			@kdodatei2	einschachteln der kdodatei2 als Eingabe-Simulations-Datei		*)
(*			@*kdodatei2	umschalten auf kdodatei2 als Eingabe-Simulations-Datei (Nachfolger)	*)
(*			@*		aktuelle Eingabedatei von vorne an wiederholen				*)
(*			@*=		Kommentarzeile								*)
(*			@**nnn		nnn Millisekunden Pause zwischen je 2 Eingabezeichen			*)
(*					wenn > 0, auch bei jedem SSR 6 16 je nach verbrauchten Takten warten	*)
(*			@**Wnnn		auf der Stelle nnn Millisekunden warten					*)
(*			@**Innn		vor jeder ersten Eingabe nach Ausgaben nnn Millisekunden warten		*)
(*			@**Ann		vor jedem ausgegebenen Zeichen nn Millisekunden warten			*)
(*														*)
(*	@*>kdodatei2	Beginn Tastatur-Aufzeichnung in Datei kdodatei2						*)
(*	@*>		Ende Tastatur-Aufzeichnung								*)



IMPORT Terminal, FileFunc, FileIO, Conversions ;

FROM WIN32 IMPORT
	Sleep ;

FROM RandomNumbers IMPORT
	Random ;

FROM TR440hp IMPORT
	DefaultKDO, GLOBmsPause ;

FROM Struktur IMPORT
	GesamtZahlTakte, TakteProMikroSekunde, AbschnittsModus ;

FROM Storage IMPORT
	ALLOCATE ;


CONST
	MaxEingabeStack =	10 ;


VAR
	SimulationAktiv :	BOOLEAN = FALSE ;
	AusgabeGewesen :	BOOLEAN = FALSE ;
	inAufzeichnung :	BOOLEAN = FALSE ;

	ImsPauseInput :		CARDINAL =	100 ;		(* 100 ms Pause zwischen je 2 Eingabezeichen	Random 1/2 ... 4/1		*)
	ImsPauseVorInput :	CARDINAL =	2000 ;		(* 2000 ms vor erster Eingabe nach Ausgabe warten : Benutzer - Denkpause	*)
	ImsPause :		CARDINAL =	10 ;		(* 10 ms warten zwischen je 2 Ausgabezeichen					*)

	msPauseInput :		CARDINAL = 0 ;
	msPauseVorInput :	CARDINAL = 0 ;
(*	msPause	:		CARDINAL = 0 ; ist im Modul Terminal *)

	EingabeDatei,
	AusgabeDatei :		FileIO.FILE ;
	TakteMerker :		CARDINAL64 ;

	EingabeStackIndex :	CARDINAL = 0 ;
	EingabeStack :		ARRAY [1..MaxEingabeStack] OF FileIO.FILE ;

	AusgabePuffer :		POINTER TO ARRAY [0..1023] OF CHAR = NIL ;



PROCEDURE ReadString (VAR str : ARRAY OF CHAR) : BOOLEAN ;
	VAR	warten :	CARDINAL ;
		b :		BOOLEAN ;
BEGIN
	IF AusgabeGewesen THEN
		AusgabeGewesen := FALSE ;
		IF (msPauseVorInput > 0) AND NOT AbschnittsModus THEN
			IF checkBreak () THEN
				RETURN FALSE ;
			END ;
			Terminal.CursorZeigen ;
			Sleep (Random (msPauseVorInput DIV 2, msPauseVorInput * 2)) ;		(* künstliche Eingabe  - Denkpause *)
			Terminal.CursorVerbergen ;
			IF checkBreak () THEN
				RETURN FALSE ;
			END ;
		END ;
	END ;
	LOOP
		IF SimulationAktiv THEN
			IF checkBreak () THEN
				RETURN FALSE ;
			END ;
			IF FileIO.ReadLn (EingabeDatei, str) THEN
				IF (str [0] = '@') THEN
					IF (str [1] = '*') THEN					(* angehängte Datei *)
						IF str [2] <= ' ' THEN
							IF NOT AbschnittsModus THEN
								FileIO.Rewind (EingabeDatei) ;	(* im Gespräch Eingabesimulations-Datei von vorne wiederholen *)
								Sleep (4000) ;			(* aber erst kurze Pause *)
							END ;
						ELSIF str [2] = '=' THEN			(* Kommentar *)

						ELSIF str [2] = '*' THEN			(* Eingabesimulations-Protokoll steuern *)
							IF NOT AbschnittsModus THEN		(* im Gespräch *)
								CASE str [3] OF
								'W' :
									IF Conversions.StrToCard (str [4..HIGH(str)], warten) THEN
										Sleep (warten) ;
									END ;
								|
								'I' :
									IF Conversions.StrToCard (str [4..HIGH(str)], warten) THEN
										ImsPauseVorInput := warten ;
										msPauseVorInput := ImsPauseVorInput ;
									END ;
								|
								'A' :
									IF Conversions.StrToCard (str [4..HIGH(str)], warten) THEN
										ImsPause := warten ;
										IF ImsPause > GLOBmsPause THEN
											Terminal.msPause := ImsPause ;
										END ;
									END ;
								|
								'1'..'9' :
									IF Conversions.StrToCard (str [3..HIGH(str)], warten) THEN
										ImsPauseInput := warten ;
										msPauseInput := ImsPauseInput ;
									END ;
								ELSE
									msPauseInput := 100 ;
								END ;
							END ;
						ELSIF str [2] >= 'A' THEN
							InitSimulation (str [2..HIGH(str)]) ;	(* angehängte nächste Datei *)
						ELSE
							fehler (str) ;
						END ;
					ELSIF str [1] >= 'A' THEN				(* eingeschachtelte Eingabe-Simulations-Datei *)
						NewSimulation (str [1..HIGH(str)]) ;		(* eingeschachtelte nächste Datei *)
					ELSE
						fehler (str) ;
					END ;
				ELSE
					prot (str) ;						(* Eingabesimulation zeigen *)
					RETURN TRUE ;						(* gelesene Zeile ausliefern *)
				END ;
			ELSE
				close ;
			END ;
		ELSIF AbschnittsModus THEN
			RETURN FALSE ;
		ELSE
			b := Terminal.ReadString (str) ;
			IF (str [0] = '@') AND (str [1] = '*') THEN
				IF str [2] = '>' THEN						(* Beginn oder Ende Tastatur-Aufzeichnung *)
					Terminal.WriteLn ;
					IF str [3] <= ' ' THEN
						endAufzeichnung ;
						Terminal.WriteString ('Ende Tastatur-Aufzeichnung') ;
					ELSIF str [3] >= 'A' THEN
						beginAufzeichnung (str [3..HIGH(str)]) ;
						IF inAufzeichnung THEN
							Terminal.WriteString ('Beginn Tastatur-Aufzeichnung') ;
							FileIO.WriteLn (AusgabeDatei, '@*= Start Aufzeichnung') ;
						END ;
					ELSE
						fehler (str) ; ;
					END ;
					Terminal.WriteLn ;
					Terminal.WriteLn ;
				ELSIF str [2] >= 'A' THEN					(* Beginn 1.  @*kdodatei   Eingabe-Simulations-Datei *)
					Terminal.WriteLn ;
					Terminal.WriteLn ;
					Terminal.WriteLn ;
					IF inAufzeichnung THEN
						str [1] := '@' ;
						FileIO.WriteLn (AusgabeDatei, str [1..HIGH(str)]) ;	(* 1. Befehl als  @kdodatei  mit in Tastatur-Aufzeichnung schreiben *)
					END ;
					InitSimulation (str [2..HIGH(str)]) ;
				ELSE
					fehler (str) ;
				END ;
			ELSE
				IF inAufzeichnung THEN
					FileIO.WriteLn (AusgabeDatei, str) ;			(* normale Tastatur-Eingabe in Tastatur-Aufzeichnung *)
				END ;
				RETURN b ;
			END ;
		END ;
	END ;
END ReadString ;


PROCEDURE checkBreak () : BOOLEAN ;
	VAR	ch :	CHAR ;
BEGIN
	IF NOT AbschnittsModus THEN
		IF Terminal.CtrlBreak THEN
			closeAll ;
			RETURN TRUE ;
		END ;
		WHILE Terminal.CharAvail () DO
			ch := Terminal.ReadChar () ;
			IF ch = Terminal.Escape THEN
				closeAll ;
				RETURN TRUE ;
			END ;
		END ;
	END ;
	RETURN FALSE ;
END checkBreak ;


PROCEDURE InitSimulation (dateiname : ARRAY OF CHAR) ;
BEGIN
	IF EingabeStackIndex = 0 THEN
		IF AbschnittsModus THEN
			ImsPauseInput :=		0 ;
			ImsPauseVorInput :=		0 ;
			ImsPause :=			0 ;
			msPauseInput :=			0 ;
			msPauseVorInput :=		0 ;
			Terminal.msPause :=		0 ;
		ELSE
			msPauseInput :=			ImsPauseInput ;
			msPauseVorInput :=		ImsPauseVorInput ;

			IF GLOBmsPause > ImsPause THEN
				Terminal.msPause :=	GLOBmsPause ;
			ELSE
				Terminal.msPause :=	ImsPause ;
			END ;
		END ;
	ELSE
		close ;		(* evtl. vorherige Simulationsdatei schließen *)
	END ;
	NewSimulation (dateiname) ;
END InitSimulation ;


PROCEDURE NewSimulation (dateiname : ARRAY OF CHAR) ;
	VAR	DatNam :	ARRAY [0..267] OF CHAR ;
BEGIN
	TakteMerker := GesamtZahlTakte ;
	FileFunc.ConstructFileName (dateiname, DefaultKDO, DefaultKDO) ;
	DatNam := DefaultKDO ;
	IF EingabeStackIndex < MaxEingabeStack THEN
		FileFunc.OpenFileEx (EingabeDatei, DatNam, FileFunc.ReadOnlyDenyWrite,
					FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
		IF EingabeDatei.status = 0 THEN
			SimulationAktiv := TRUE ;

			EingabeStackIndex := EingabeStackIndex + 1 ;
			EingabeStack [EingabeStackIndex] := EingabeDatei ;
		ELSE
			fehler (DatNam) ;
		END ;
	END ;
END NewSimulation ;


PROCEDURE fehler (datnam : ARRAY OF CHAR) ;
BEGIN
	IF NOT AbschnittsModus THEN
		Terminal.WriteLn ;
		Terminal.WriteString ('+++++ nicht gefunden : ') ;
		Terminal.WriteString (datnam) ;
		Terminal.WriteLn ;
	END ;
	closeAll ;
END fehler ;


PROCEDURE AusgabeBeginn ;
	VAR	takte :		CARDINAL ;
		wartems :	CARDINAL ;
BEGIN
	AusgabeGewesen := TRUE ;
	IF msPauseInput > 0 THEN
		IF checkBreak () THEN
			RETURN ;
		END ;
		IF GesamtZahlTakte > TakteMerker THEN				(* verstrichene Takte abwarten *)
			takte := ORD (GesamtZahlTakte-TakteMerker) ;
			wartems := (takte + TakteProMikroSekunde * 500) DIV (TakteProMikroSekunde * 1000) ;
			IF wartems > 0 THEN
				Sleep (wartems) ;				(* 16 Takte ~ 1 Mikrosekunde *)
			END ;
		END ;
		TakteMerker := GesamtZahlTakte ;
	END ;
END AusgabeBeginn ;



PROCEDURE prot (str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	IF NOT AbschnittsModus THEN
		Terminal.ClearInput ;
		IF msPauseInput > 0 THEN
			FOR i := 1 TO LENGTH (str) DO
				IF checkBreak () THEN
					RETURN ;
				END ;
				Terminal.Write (str [i-1]) ;
				Terminal.CursorZeigen ;
				Sleep (Random (msPauseInput DIV 2, msPauseInput * 4)) ;
				Terminal.CursorVerbergen ;
				IF checkBreak () THEN
					RETURN ;
				END ;
			END ;
		ELSE
			Terminal.WriteString (str) ;
			IF checkBreak () THEN
				RETURN ;
			END ;
		END ;
	END ;
END prot ;


PROCEDURE close ;
BEGIN
	IF SimulationAktiv THEN
		FileIO.CloseFile (EingabeDatei) ;
		IF EingabeStackIndex > 0 THEN
			EingabeStackIndex := EingabeStackIndex - 1 ;
			IF EingabeStackIndex = 0 THEN
				msPauseInput := 0 ;
				msPauseVorInput := 0 ;
				Terminal.msPause := GLOBmsPause ;

				Terminal.CtrlBreak := FALSE ;
				SimulationAktiv := FALSE ;
			ELSE
				EingabeDatei := EingabeStack [EingabeStackIndex] ;
			END ;
		END ;
	END ;
END close ;


PROCEDURE closeAll ;
BEGIN
	WHILE EingabeStackIndex > 0 DO
		close ;
	END ;
END closeAll ;


PROCEDURE beginAufzeichnung (dateiname : ARRAY OF CHAR) ;
	VAR	DatNam :	ARRAY [0..267] OF CHAR ;
BEGIN
	endAufzeichnung ;
	FileFunc.ConstructFileName (dateiname, DefaultKDO, DefaultKDO) ;
	DatNam := DefaultKDO ;
	IF AusgabePuffer = NIL THEN
		NEW (AusgabePuffer) ;
	END ;
	IF FileIO.OpenAppendFileBuf (AusgabeDatei, DatNam, AusgabePuffer ^ ) THEN
		inAufzeichnung := TRUE ;
	END ;
END beginAufzeichnung ;


PROCEDURE endAufzeichnung ;
BEGIN
	IF inAufzeichnung THEN
		inAufzeichnung := FALSE ;
		FileIO.CloseFile (AusgabeDatei) ;
	END ;
END endAufzeichnung ;


BEGIN
FINALLY
	closeAll ;
	endAufzeichnung ;
END TerminalEingabe.
