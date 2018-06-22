
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_0;

(*	04.08.16	*)

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM AbwicklerUp IMPORT * ;

IMPORT Programm, Assembler, AsmTrace, debug, AsmDisasm ;


PROCEDURE _SSR_0_4 ;
	VAR	ON,
		OLN :	ARRAY [0..15] OF CHAR ;
		OLK :	CARDINAL ;
		dbg :	BOOLEAN ;

	PROCEDURE TraceFortsetzung ;
	BEGIN
%IF %NOT WEB %THEN
		AsmTrace.ExitTrace ;
		IF dbg THEN
			debug.an := TRUE ;
			debug.Befehle := TRUE ;
				AsmTrace.RestoreTrace (OLN) ;
		END ;
%END
	END TraceFortsetzung ;

BEGIN
	IF NOT SSRaufruf ('Starte Operatorlauf', 7) THEN
		RETURN ;
	END ;

	upStart (ON, OLN) ;

	Programm.ProgrammPausieren ;

%IF %NOT WEB %THEN

	dbg := debug.Befehle ;
	AsmTrace.ExitTrace ;

	IF debug.EinBeiProgStart THEN
		AsmDisasm.TraceBefehl ('1') ;
	END ;
%END

	IF Programm.ProgrammStarten (ON, OLN) THEN
		OLK := AktOLK ;
		Programm.ProgrammLauf (Assembler.StartAdresse) ;
		Programm.ProgrammBeenden ;
	ELSE
		TraceFortsetzung ;

		Programm.ProgrammFortsetzen ;

		IF Programm.OLNvergeben THEN
			SSRfehler(24H, 'Operatorlauf bereits vorhanden') ;
		ELSE
			SSRfehler(1BH, 'Operator nicht vorhanden') ;
		END ;
		RETURN ;
	END ;

	TraceFortsetzung ;

	Programm.ProgrammFortsetzen ;
	IF Operatoren [AktOperator].ProgrammEndeFehler <> 0 THEN
		SSRfehler (1EH, 'Operator mit Fehler beendet') ;
		RegQ.inh := VAL (CARDINAL64, OLK SHL 24) BOR VAL(CARDINAL64, Operatoren [AktOperator].ProgrammEndeFehler) ;
		RegQ.TK := 1 ;
	ELSE
		RegQ.inh := VAL (CARDINAL64, OLK SHL 24) ;
	END ;
	RegQ.TK := 1 ;
	RegH.inh := 0 ;
END _SSR_0_4 ;


PROCEDURE _SSR_0_8 ;
	VAR	ON,
		OLN :		ARRAY [0..15] OF CHAR ;

BEGIN
	IF NOT SSRaufruf ('Beende und starte Operatorlauf', 7) THEN
		RETURN ;
	END ;

%IF %NOT WEB %THEN

	AsmTrace.ExitTrace ;

	IF debug.EinBeiProgStart THEN
		AsmDisasm.TraceBefehl ('1') ;
	END ;
%END

	upStart (ON, OLN) ;
	Programm.ProgrammBeenden ;
	IF Programm.ProgrammStarten (ON, OLN) THEN
		Programm.ProgrammLauf (Assembler.StartAdresse) ;
		Programm.ProgrammBeenden ;
	END ;
	globProgrammEnde := TRUE ;
END _SSR_0_8 ;


PROCEDURE _SSR_0_12 ;
BEGIN
	SSRaufruf ('Beende Operatorlauf', 0) ;
	AsmTrace.ExitTrace ;
	globProgrammEnde := TRUE ;
	Operatoren [AktOperator].ProgrammEnde := TRUE ;
	Operatoren [AktOperator].ProgrammEndeFehler := 0 ;
END _SSR_0_12 ;


PROCEDURE _SSR_0_16 ;
BEGIN
	SSRaufruf ('Beende Operatorlauf mit Fehlermeldung', 2) ;
	AsmTrace.ExitTrace ;
	globProgrammEnde := TRUE ;
	Operatoren [AktOperator].ProgrammEnde := TRUE ;
	Operatoren [AktOperator].ProgrammEndeFehler := GetHalbwort (VBL + 3) ;
END _SSR_0_16 ;


PROCEDURE _SSR_0_20 ;
BEGIN
	SSRaufruf ('Alarmadresse anmelden', 1) ;
	RegA.TK := 2 ;
	WITH Operatoren [AktOperator] DO
		RegA.inh := AlarmAdresse ;
		AlarmAdresse := GetHalbwort (VBL + 1) ;
		AlarmSperre := FALSE ;
		ZustellungsSperre := FALSE ;
		AlarmGewesen := FALSE ;
	END ;
END _SSR_0_20 ;


PROCEDURE _SSR_0_22 ;
BEGIN
	SSRaufruf ('Zustellungssperre verändern', 1) ;
	Operatoren [AktOperator].ZustellungsSperre := ODD (GetHalbwort(VBL + 1)) ;
END _SSR_0_22 ;


PROCEDURE _SSR_0_26 ;
	VAR	AA :	SpeicherAdresse ;
		wort :	CARDINAL64 ;
BEGIN
	IF NOT SSRaufruf ('Normierter Start', 1) THEN
		RETURN ;
	END ;

	AA := GetHalbwort (VBL+1) ;
	wort := GetGanzwort (AA) ;
	RegM := AktTypenkennung > 1 ;
	RegB := (wort SHR 24) BAND Cardinal24Bit ;
	RegK := (wort SHR 16) BAND 0FFH ;
	RegY := (wort SHR 8) BAND 0FFH ;
	RegU := wort BAND 0FFH ;
	RegA.inh := GetGanzwort (AA+2) ; RegA.TK := AktTypenkennung ;
	RegQ.inh := GetGanzwort (AA+4) ; RegQ.TK := AktTypenkennung ;
	RegD.inh := GetGanzwort (AA+6) ; RegD.TK := AktTypenkennung ;
	RegH.inh := GetGanzwort (AA+8) ; RegH.TK := AktTypenkennung ;
	RegT := GetHalbwort (AA+10) ;
	RegF := GetHalbwort (AA+18) ; FolgeAdresse := RegF ;
	(* STB1 / BH ? *)
END _SSR_0_26 ;


PROCEDURE _SSR_0_28 ;
BEGIN
	SSRnimpl ('Alarmkeller erzeugen') ;
END _SSR_0_28 ;


PROCEDURE _SSR_0_30 ;
	VAR	AA :	SpeicherAdresse ;
		wort :	CARDINAL64 ;
BEGIN
	IF NOT SSRaufruf ('Weiterstart nach Alarm', 1) THEN
		RETURN ;
	END ;

	AA := GetHalbwort (VBL + 1) ;
	IF (AA BAND 800000H) = 0 THEN
		Operatoren [AktOperator].AlarmSperre := FALSE ;
		Operatoren [AktOperator].ZustellungsSperre := FALSE ;
	END ;
	IF (AA BAND 400000H) = 0 THEN
		AA := AA BAND 3FFFFFH ;
		wort := GetGanzwort (AA) ;
		RegM := AktTypenkennung > 1 ;
		RegK := (wort SHR 16) BAND 0FFH ;
		RegY := (wort SHR 8) BAND 0FFH ;
		RegU := wort BAND 0FFH ;
		RegA.inh := GetGanzwort (AA+2) ; RegA.TK := AktTypenkennung ;
		RegQ.inh := GetGanzwort (AA+4) ; RegQ.TK := AktTypenkennung ;
		RegD.inh := GetGanzwort (AA+6) ; RegD.TK := AktTypenkennung ;
		RegH.inh := GetGanzwort (AA+8) ; RegH.TK := AktTypenkennung ;
		RegT := GetHalbwort (AA+10) ;
	END ;
	RegF := Operatoren [AktOperator].UnterbrechungsAdresse ; FolgeAdresse := RegF ;
	Operatoren [AktOperator].AlarmSperre := FALSE ;
	Operatoren [AktOperator].ZustellungsSperre := FALSE ;
END _SSR_0_30 ;


PROCEDURE _SSR_0_32 ;
BEGIN
	IF SSRaufruf ('Alarmsperren löschen', 1) THEN
		Operatoren [AktOperator].AlarmSperre := FALSE ;
	END ;
END _SSR_0_32 ;


PROCEDURE _SSR_0_34 ;
BEGIN
	SSRnimpl ('SSR-Fehler umleiten auf SW-Alarm') ;
END _SSR_0_34 ;


PROCEDURE _SSR_0_36 ;
	VAR	i :	CARDINAL ;
		rest0 :	BOOLEAN ;
BEGIN
	IF SSRaufruf ('Hierarchie der Bibliotheken', 10) THEN
		IF ODD (GetHalbwort (VBL + 1)) THEN			(* neue Hierarchie anliefern *)
			rest0 := FALSE ;
			FOR i := 0 TO 7 DO
				IF rest0 THEN
					AktAuftrag.Hierarchie [i] := 0 ;
				ELSE
					AktAuftrag.Hierarchie [i] := GetGanzwort (VBL + i*2 + 2) ;
					IF AktTypenkennung <> 3 THEN
						rest0 := TRUE ;
						AktAuftrag.Hierarchie [i] := 0 ;
					END ;
				END ;
			END ;
		ELSE							(* Hierarchie ausliefern *)
			FOR i := 0 TO 7 DO
				PutGanzwort (VBL + i*2 + 2, 3 * ORD (AktAuftrag.Hierarchie [i] <> 0), AktAuftrag.Hierarchie [i]) ;
			END ;
		END ;
	END ;
END _SSR_0_36 ;


PROCEDURE _SSR_0_40 ;
BEGIN
	SSRnimpl ('Freiwillig selber verdrängen') ;
END _SSR_0_40 ;


END SSR_0.
