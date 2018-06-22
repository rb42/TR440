
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)


IMPLEMENTATION MODULE Abwickler;

(*	13.06.18	*)

FROM Trace IMPORT
	TraceF ;

IMPORT debug ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM SSR_0 IMPORT * ;
FROM SSR_1 IMPORT * ;
FROM SSR_2 IMPORT * ;
FROM SSR_3 IMPORT * ;
FROM SSR_4 IMPORT * ;
FROM SSR_5 IMPORT * ;
FROM SSR_6 IMPORT * ;
FROM SSR_7 IMPORT * ;
FROM SSR_47 IMPORT * ;
FROM SSR_250 IMPORT * ;
FROM SSR_253 IMPORT * ;
FROM SSR_255 IMPORT * ;
FROM AbwicklerUp IMPORT * ;

FROM DateiVerwaltung IMPORT
	FSdateiverwaltung ;

FROM TR440hp IMPORT
	SSRsZaehlen ;



TYPE
	pSSRelem =	POINTER TO tSSRelem ;

	tSSRelem =	RECORD
				next :		pSSRelem ;
				adressteil :	CARDINAL ;
				anzahl :	CARDINAL ;
			END ;
VAR
	SSRelem0 :	pSSRelem = NIL ;



PROCEDURE _SSR ;
VAR	FortsetzAdr,
	FehlerAdr,
	AufAdr :	CARDINAL ;
BEGIN
	VBL := RegB ;				(* Adresse Versorgungsblock in B *)
	FS := 0 ;
	AufrufAdresse := RegF + 1 ;

	FortsetzAdr := AufrufAdresse ;		(* normale Fortsetzung nach SSR *)

	AufAdr := AufrufAdresse ;

	SSRl := AktAdressteil SHR 8 ;
	SSRr := AktAdressteil ;

	IF SSRsZaehlen THEN
		SSRzaehlen (AktAdressteil) ;
	END ;


	Takte (3000) ;				(* mittlere geschätzte Taktzahl pro SSR *)


	IF AktAdressteil = 12 THEN		(* Sonderbehandlung SSR 0 12 : VBL irrelevant, Fehler gibts nicht *)
		_SSR_0_12 ;
		RETURN ;
	END ;

	IF NOT TestAdresse (VBL) THEN
		SpeicherschutzAlarm ('bei SSR-Versorgungsblock', VBL) ;
		RegA.inh := 2 ;		(* Versorgungsalarm *)
		RETURN ;
	END ;

	FehlerAdr :=  GetHalbwort (VBL) ;	(* Fehler-Rücksprungsadresse *)

	IF ODD (VBL) THEN
		formaleFehler ;
	ELSE
		CASE SSRl OF

		0 :	_SSR_0 ;		(* Regiedienste				*)
		|
		1 :	_SSR_1 ;		(* Steuerinformationsdienste		*)
		|
		2 :	_SSR_2 ;		(* Magnetbanddienste			*)
		|
		3 :	_SSR_3 ;		(* Gebietsdienste			*)
		|
		4 :	_SSR_4 ;		(* Informationsdienste			*)
		|
		5 :	_SSR_5 ;		(* KOMSYS-Dienste			*)
		|
		6 :	_SSR_6 ;		(* Protokolldienste			*)
		|
		7 :	_SSR_7 ;		(* Allgemeine Dienste			*)
		|
		47 :	_SSR_47 ;		(* Testhilfen				*)
		|
		250 :	_SSR_250 ;		(* DB-Dienste TR4 ?			*)
		|
		253 :
			FSdateiverwaltung := 0 ;

			_SSR_253 ;		(* Dateidienste				*)

			IF FSdateiverwaltung <> 0 THEN
				SSRfehler (FSdateiverwaltung, 'Fehler bei Dateizugriff') ;
			END ;
			IF FS <> 0 THEN
				FS := FS BOR 0C00000H ;	(* Kennzeichen : Fehler der Datenorganisation *)
			END ;
		|
		255 :	_SSR_255 ;		(* Messungen				*)


		ELSE	SSRunzulaessig ;
		END ;
	END ;

	IF FS <> 0 THEN
		IF ProgrammierModus THEN
			AlarmGewesen := TRUE ;	(* Debug-Halt nach SSR-Fehler *)
		END ;
		FortsetzAdr := FehlerAdr ;
		RegA.inh := (VAL(CARDINAL64, AufAdr) SHL 24) BOR VAL(CARDINAL64, FS) ;
	(*
		RegA.inh := VAL(CARDINAL64, FS) BOR (VAL (CARDINAL64, AufAdr) SHL 24) ;
	*)
		RegA.TK := 1 ;
	END ;
	GrossSeitenInvarianz := TRUE ;
	MUmodifizierung := FALSE ;
	RegFbesetzen (FortsetzAdr) ;
END _SSR ;


PROCEDURE _SSR_0 ;
BEGIN
	CASE SSRr OF
	4 :	_SSR_0_4 ;		(* Starte Operatorlauf					*)
	|
	8 :	_SSR_0_8 ;		(* Beende und starte Operatorlauf			*)
	|
	12 :	_SSR_0_12 ;		(* Beende Operatorlauf					*)
	|
	16 :	_SSR_0_16 ;		(* Beende Operatorlauf mit Fehlermeldung		*)
	|
	20 :	_SSR_0_20 ;		(* Alarmadresse anmelden				*)
	|
	22 :	_SSR_0_22 ;		(* Zustellungssperre verändern				*)
	|
	26 :	_SSR_0_26 ;		(* Normierter Start					*)
	|
	28 :	_SSR_0_28 ;		(* Alarmkeller erzeugen					*)
	|
	30 :	_SSR_0_30 ;		(* Weiterstart nach Alarm				*)
	|
	32 :	_SSR_0_32 ;		(* Alarmsperren löschen					*)
	|
	34 :	_SSR_0_34 ;		(* SSR-Fehler auf SW-Alarm umleiten			*)
	|
	36 :	_SSR_0_36 ;		(* Hierarchie der Bibliotheken				*)
	|
	40 :	_SSR_0_40 ;		(* freiwillig selber verdrängen				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_0 ;


PROCEDURE _SSR_1 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_1_0 ;		(* Steuerinformation eintragen				*)
	|
	4 :	_SSR_1_4 ;		(* Lies Steuerinformation				*)
	|
	8 :	_SSR_1_8 ;		(* Wahlschalter setzen					*)
	|
	12 :	_SSR_1_12 ;		(* Wahlschalter setzen					*)
	|
	16 :	_SSR_1_16 ;		(* ??							*)
	|
	20 :	_SSR_1_20 ;		(* ??							*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_1 ;


PROCEDURE _SSR_2 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_2_0 ;		(* Modusabhängiger Dienst				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_2 ;


PROCEDURE _SSR_3 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_3_0 ;		(* Gebiet erstellen					*)
	|
	4 :	_SSR_3_4 ;		(* Gebiet löschen					*)
	|
	8 :	_SSR_3_8 ;		(* Gebiet aufrufen zur Verarbeitung			*)
	|
	12 :	_SSR_3_12 ;		(* Gemeinschaftsgebiet einrichten			*)
	|
	16 :	_SSR_3_16 ;		(* Gebiet abmelden vonb der Verarbeitung		*)
	|
	20 :	_SSR_3_20 ;		(* Gemeinschaftsgebietssperre verändern			*)
	|
	24 :	_SSR_3_24 ;		(* Schreibsperre setzen					*)
	|
	28 :	_SSR_3_28 ;		(* Schreibsperre löschen				*)
	|
	36 :	_SSR_3_36 ;		(* Verändern der Länge					*)
	|
	56 :	_SSR_3_56 ;		(* Verändern der Verarbeitungsklasse			*)
	|
	68 :	_SSR_3_68 ;		(* Achtelseitentransport				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_3 ;


PROCEDURE _SSR_4 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_4_0 ;		(* Informiere über Kenndaten				*)
	|
	4 :	_SSR_4_4 ;		(* Informiere über aktuelle Gebiete			*)
	|
	8 :	_SSR_4_8 ;		(* Informiere über Alarmursache				*)
	|
	20 :	_SSR_4_20 ;		(* Informiere über Gebietsnamen				*)
	|
	24 :	_SSR_4_24 ;		(* Informiere über Gebietslänge				*)
	|
	28 :	_SSR_4_28 ;		(* Informiere über Speicherberechtigungen		*)
	|
	32 :	_SSR_4_32 ;		(* Informiere über Zeit					*)
	|
	36 :	_SSR_4_36 ;		(* Informiere über Signale				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_4 ;


PROCEDURE _SSR_5 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_5_0 ;		(* Warteschlange kreieren				*)
	|
	4 :	_SSR_5_4 ;		(* Warteschlange löschen				*)
	|
	8 :	_SSR_5_8 ;		(* Sendung absenden an Warteschlange			*)
	|
	12 :	_SSR_5_12 ;		(* Informieren über Sendungen				*)
	|
	16 :	_SSR_5_16 ;		(* Warten auf Sendungen					*)
	|
	20 :	_SSR_5_20 ;		(* Sendung übernehmen					*)
	|
	24 :	_SSR_5_24 ;		(* Sendung löschen					*)
	|
	28 :	_SSR_5_28 ;		(* Sendung zurücksenden					*)
	|
	32 :	_SSR_5_32 ;		(* Auftrag kreieren					*)
	|
	40 :	_SSR_5_40 ;		(* Gerät belegen					*)
	|
	44 :	_SSR_5_44 ;		(* Gerät freigeben					*)
	|
	48 :	_SSR_5_48 ;		(* Sendung absenden an Gerät				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_5 ;


PROCEDURE _SSR_6 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_6_0 ;		(* Eintragen Dumpbereich				*)
	|
	4 :	_SSR_6_4 ;		(* Verändern Ablaufprotokoll				*)
	|
	8 :	_SSR_6_8 ;		(* Eintragen Kopftexterweiterung			*)
	|
	12 :	_SSR_6_12 ;		(* Eintragen Zeilen					*)
	|
	16 :	_SSR_6_16 ;		(* Eintragen Zeilen für Konsolprotokoll			*)
	|
	20 :	_SSR_6_20 ;		(* Eintragen Zeilen für KSM - Ausgabe			*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_6 ;


PROCEDURE _SSR_7 ;
BEGIN
	CASE SSRr OF
	0 :	_SSR_7_0 ;		(* Verteiler-SSR durchreichen				*)
	|
	4 :	_SSR_7_4 ;		(* Modusabhämgiger Rerun				*)
	|
	8 :	_SSR_7_8 ;		(* Ausliefern Systemdaten				*)
	|
	12 :	_SSR_7_12 ;		(* Statistikinformation eintragen			*)
	|
	16 :	_SSR_7_16 ;		(* Botschaften durchreichen				*)
	|
	20 :	_SSR_7_20 ;		(* Steuerung von Messungen				*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_7 ;


PROCEDURE _SSR_47 ;
BEGIN
	CASE SSRr OF
	11 :	_SSR_47_11 ;		(* Abwickler-Testhilden					*)
	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_47 ;


PROCEDURE _SSR_250 ;
BEGIN
	CASE SSRr OF
	2 :	_SSR_250_2 ;		(* ??							*)
	|
	4 :	_SSR_250_4 ;		(* ??							*)
	|
	6 :	_SSR_250_6 ;		(* ??							*)
	|
	8 :	_SSR_250_8 ;		(* ??							*)
	|
	10 :	_SSR_250_10 ;		(* ??							*)
	|
	12 :	_SSR_250_12 ;		(* ??							*)
	|
	20 :	_SSR_250_20 ;		(* ??							*)
	|
	22 :	_SSR_250_22 ;		(* ??							*)
	|
	24 :	_SSR_250_24 ;		(* ??							*)
	|
	30 :	_SSR_250_30 ;		(* ??							*)
	|
	32 :	_SSR_250_32 ;		(* ??							*)
	|
	34 :	_SSR_250_34 ;		(* ??							*)
	|
	40 :	_SSR_250_40 ;		(* ??							*)
	|
	42 :	_SSR_250_42 ;		(* ??							*)
	|
	44 :	_SSR_250_44 ;		(* ??							*)
	|
	46 :	_SSR_250_46 ;		(* ??							*)

	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_250 ;


PROCEDURE _SSR_253 ;
BEGIN
	CASE SSRr OF
	1 :	_SSR_253_1 ;		(* Kreation einer Datenbasis				*)
	|
	2 :	_SSR_253_2 ;		(* Aufgabe einer Datenbasis				*)
	|
	3 :	_SSR_253_3 ;		(* Kreation einer Datei					*)
	|
	4 :	_SSR_253_4 ;		(* Löschen einer Datei					*)
	|
	5 :	_SSR_253_5 ;		(* Kenndaten verändern					*)
	|
	6 :	_SSR_253_6 ;		(* Schreibsperre setzen					*)
	|
	7 :	_SSR_253_7 ;		(* Schreibsperre lösen					*)
	|
	8 :	_SSR_253_8 ;		(* Einschleusen einer Datei				*)
	|
	9 :	_SSR_253_9 ;		(* Bearbeitung eröffnen					*)
	|
	10 :	_SSR_253_10 ;		(* Satztransport					*)
	|
	11 :	_SSR_253_11 ;		(* Satzposition einstellen				*)
	|
	12 :	_SSR_253_12 ;		(* Betriebsart einstellen				*)
	|
	13 :	_SSR_253_13 ;		(* Sätze löschen					*)
	|
	14 :	_SSR_253_14 ;		(* Bearbeitung beenden					*)
	|
	15 :	_SSR_253_15 ;		(* Kenndsatz schreiben					*)
	|
	16 :	_SSR_253_16 ;		(* Kenndsatz lesen					*)
	|
	17 :	_SSR_253_17 ;		(* Freie Satzpositionen ausliefern			*)
	|
	18 :	_SSR_253_18 ;		(* Belegte Satzpositionen ausliefern			*)
	|
	19 :	_SSR_253_19 ;		(* Kenndaten ausliefern					*)
	|
	20 :	_SSR_253_20 ;		(* Dateibezeichnung verändern				*)
	|
	21 :	_SSR_253_21 ;		(* Bandwechsel						*)
	|
	22 :	_SSR_253_22 ;		(* Sätze überschreiben					*)
	|
	23 :	_SSR_253_23 ;		(* Transport eines Blockes				*)
	|
	24 :	_SSR_253_24 ;		(* Abmelden einer Datei					*)
	|
	25 :	_SSR_253_25 ;		(* Abspannauftrag					*)
	|
	26 :	_SSR_253_26 ;		(* Platzreservierung					*)
	|
	27 :	_SSR_253_27 ;		(* Datei gebietsweise kopieren				*)
	|
	32 :	_SSR_253_32 ;		(* Dateibezeichnungen ausliefern			*)
	|
	39 :	_SSR_253_39 ;		(* Datei in Gebiet wandeln				*)
	|
	40 :	_SSR_253_40 ;		(* Ausgabe einer Datei					*)

	ELSE
		SSRunzulaessig ;
	END ;
	IF FS <> 0 THEN
		FS := FS BOR 0C00000H ;	(* Kennzeichen : FM Datenorganisation *)
	END ;
END _SSR_253 ;


PROCEDURE _SSR_255 ;
BEGIN
	CASE SSRr OF
	255 :	_SSR_255_255 ;		(* Messungen durchführen				*)
	ELSE
		SSRunzulaessig ;
	END ;
END _SSR_255 ;



PROCEDURE SSRzaehlen (adr : CARDINAL) ;
	VAR	elem :	pSSRelem ;
BEGIN
	IF SSRelem0 = NIL THEN						(* 1. Zählelement erzeugen *)
		NEW (SSRelem0) ;
		SSRelem0 ^ .next := NIL ;
		SSRelem0 ^ .adressteil := adr ;
		SSRelem0 ^ .anzahl := 0 ;
	END ;
	elem := SSRelem0 ;
	WHILE elem  <> NIL DO
		IF elem ^ .adressteil = adr THEN			(* Zählelement gefunden *)
			INC (elem ^ .anzahl) ;
			RETURN ;
		END ;
		IF elem ^ .next = NIL THEN				(* neues Zählelement erzeugen *)
			NEW (elem ^ .next) ;
			elem := elem ^ .next ;
			elem ^ .next := NIL ;
			elem ^ .adressteil := adr ;
			elem ^ .anzahl := 1 ;
			RETURN ;
		END ;
		elem := elem ^ .next ;					(* richtiges Zählelement weitersuchen *)
	END ;
END SSRzaehlen ;


PROCEDURE SSRzaehlerZeigen ;
	VAR	elem,
		velem :		pSSRelem ;
BEGIN
	elem := SSRelem0 ;
	WHILE elem <> NIL DO
		TraceF ('SSR %c %c \t%c-mal', elem ^ .adressteil DIV 256, elem ^ .adressteil MOD 256, elem ^ .anzahl) ;
		elem ^ .anzahl := 0 ;
		velem := elem ;
		elem := elem ^ .next ;
		DISPOSE (velem) ;
	END ;
	SSRelem0 := NIL ;
END SSRzaehlerZeigen ;


END Abwickler.

