
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE AbwicklerUp;

(*	13.06.18	*)


FROM SYSTEM IMPORT
	CAST ;

FROM ASCII IMPORT
	eot, cr, lf, ff ;

FROM Trace IMPORT
	TraceF ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM ZC1 IMPORT * ;

IMPORT ExStrings, debug ;

IMPORT Strings, Conversions, Terminal, FileFunc, FileIO, AsmTrace, TerminalEingabe ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM TR440hp IMPORT
	insAblaufProtokoll,
	Drucker,
	DruckerFont ;

FROM DateiVerwaltung IMPORT * ;

FROM DateiBearbeitung IMPORT * ;

FROM Gebiet IMPORT * ;

%IF %NOT WEB %THEN

FROM upDumpeDatei IMPORT
	KonvertSatzNachExtern ;

FROM GDIdruck IMPORT * ;
%END


%IF WEB %THEN
IMPORT CGIup ;
%END


PROCEDURE upDateiAnf (VAR DBN, Dateiname, BKZ, EXDKZ : ARRAY OF CHAR ; VAR GENVNR : CARDINAL ; VAR lfd : BOOLEAN) : BOOLEAN ;
	VAR	i,
		adr :		CARDINAL ;
		zw :		ARRAY [0..255] OF CHAR ;
		DTT :		tDateityp ;
		DATTR :		CARDINAL8 ;
		GB :		BOOLEAN ;
BEGIN

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN FALSE ;
	END ;

	IF GetEchtenDatenbasisNamen (DBN, zw) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN FALSE ;
	END ;

	BKZ := '' ;
	EXDKZ := '' ;

	DATTR := GetHalbwort (VBL + 7) BAND 0FH ;

	lfd := FALSE ;

	CASE DATTR OF
	0,	(* Kernspeicherdatei *)
	1,	(* Gebietsdatei auf Platte *)
	2 :	(* Gebietsdatei auf Trommel *)
		IF GetLokalDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
			SSRfehler (dateiFS, dateiFStext) ;
			RETURN FALSE ;
		END ;
	|
	3 :	(* Magnetbanddatei *)
			unzulaessigeAnwendung ('DATTR=3') ;
			RETURN FALSE ;
	|
	4 :	(* Wechselplattendatei *)
		adr := GetHalbwort (VBL + 14) ;
		IF AktTypenkennung = 3 THEN
			GetStringAusSpeicher (EXDKZ, VBL + 14, 6) ;
		ELSE
			GetStringAusSpeicher (EXDKZ, adr, 6) ;		(* Liste - max 1 EXDKZ unterstützt *)
		END ;
		CheckDateinamen (EXDKZ) ;
		GetStringAusSpeicher (BKZ, VBL + 20, 6) ;
		IF AktTypenkennung <> 3 THEN
			BKZ := '' ;
		END ;
		CheckDateinamen (BKZ) ;
		IF GetExternDateinamen (Dateiname, BKZ, EXDKZ, AktDateiname, FALSE) <> TreeOK THEN
			SSRfehler (dateiFS, dateiFStext) ;
			RETURN FALSE ;
		END ;
	|
	5 :	(* LFD-Datei *)
		GetStringAusSpeicher (BKZ, VBL + 20, 6) ;
		IF AktTypenkennung <> 3 THEN
			BKZ := AktAuftrag.BKZ ;
		END ;
		CheckDateinamen (BKZ) ;
		IF GetExternDateinamen (Dateiname, BKZ, '', AktDateiname, TRUE) <> TreeOK THEN
			SSRfehler (dateiFS, dateiFStext) ;
			RETURN FALSE ;
		END ;
		lfd := TRUE ;
	ELSE
		unzulaessigeAnwendung ('DATTR > 5') ;
		RETURN FALSE ;
	END ;

	IF GENVNR <> 0 THEN
		GENVNRausZahl (GENVNR, zw) ;
		PutStringInRegister (zw, RegH) ;
	END ;

	IF lfd THEN
		PutStringInRegister (BKZ, RegD) ;
	END ;
	RETURN TRUE ;
END upDateiAnf ;


PROCEDURE upGet_AADTB_LNGDTB (VAR Dateiname : ARRAY OF CHAR ; VBLrelativ : CARDINAL ; VAR GENVNR : CARDINAL) : BOOLEAN ;
	VAR	AADTB :		SpeicherAdresse ;
		i :		CARDINAL ;
		LNGDTB :	CARDINAL8 ;
		str :		ARRAY [0..7] OF CHAR ;
BEGIN
	AADTB := GetHalbwort (VBL + VBLrelativ) ;

	IF ODD (AADTB) THEN
		formaleFehler ;
		RETURN FALSE ;
	END ;

	LNGDTB := GetHalbwort (VBL + VBLrelativ + 1) BAND 0FH ;
	IF LNGDTB > 3 THEN
		formaleFehler ;
		RETURN FALSE ;
	END ;

	SSRpuffer ('DTB', VBLrelativ) ;

	IF LNGDTB > 2 THEN
		GetStringAusSpeicher (Dateiname, AADTB, 12) ;
		GetStringAusSpeicher (str, AADTB + 4, 6) ;
		GENVNR := GENVNRtoZahl (str) ;
	ELSE
		GetStringAusSpeicher (Dateiname, AADTB, LNGDTB * 6) ;
		GENVNR := 0 ;
	END ;
	FOR i := LENGTH(Dateiname) TO 11 DO
		Dateiname [i] := 0C ;
	END ;
	IF HIGH(Dateiname) > 11 THEN
		Dateiname [12] := 0C ;
	END ;

	CheckDateinamen (Dateiname) ;

	RETURN TRUE ;
END upGet_AADTB_LNGDTB ;


PROCEDURE upDateiName (VAR DBN, Dateiname : ARRAY OF CHAR ; VAR GENVNR : CARDINAL) : BOOLEAN ;
	VAR	i :		CARDINAL ;
BEGIN
	GetStringAusSpeicher (DBN, VBL + 2, 6) ;
	FOR i := LENGTH(DBN) TO 5 DO
		DBN [i] := 0C ;
	END ;
	IF HIGH(DBN) > 5 THEN
		DBN [6] := 0C ;
	END ;

	CheckDateinamen (DBN) ;

	IF NOT upGet_AADTB_LNGDTB (Dateiname, 4, GENVNR) THEN
		RETURN FALSE ;
	END ;

	RETURN TRUE ;
END upDateiName ;


PROCEDURE ZustandsWahlschalter (nr : WahlschalterNummer) : BOOLEAN ;
	TYPE	Wset =	SET OF WahlschalterNummer ;
	VAR	bits :	Wset ;
BEGIN
	bits := CAST (Wset, Wahlschalter) ;
	RETURN nr IN bits ;
END ZustandsWahlschalter ;


PROCEDURE SetZustandsWahlschalter (nr : WahlschalterNummer ; setzen : BOOLEAN) ;
	TYPE	Wset =	SET OF WahlschalterNummer ;
	VAR	bits :	Wset ;
BEGIN
	bits := CAST (Wset, Wahlschalter) ;
	IF setzen THEN
		bits := bits + Wset { nr } ;
	ELSE
		bits := bits - Wset { nr } ;
	END ;
	Wahlschalter := CAST (CARDINAL16, bits) ;
END SetZustandsWahlschalter ;


PROCEDURE upStrmnr (VAR strmnr : CARDINAL) : BOOLEAN ;
	VAR	s :	CARDINAL ;
BEGIN
	strmnr := GetHalbwort (VBL + 1) BAND 0FFFFH ;
	IF GetStrom (strmnr) = 0 THEN
		(*
		SSRfehler (45H, 'Dateibearbeitung nicht eröffnet') ;
		*)
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END upStrmnr ;


PROCEDURE GNRausGBK (reladr : CARDINAL) : CARDINAL ;
	VAR	wort :	CARDINAL64 ;
		GNR,
		TK :	CARDINAL ;
		OGNM,
		PGNM :	ARRAY [0..9] OF CHAR ;
BEGIN
	wort := GetGanzwort (VBL + reladr) ;
	TK := AktTypenkennung ;
	IF TK = 1 THEN
		GNR := wort ;
		IF NOT CheckGNR (GNR) THEN
			SSRfehler (8, 'Gebietsname ist unbekannt') ;
			RegQ.inh := wort ;
			RETURN 0 ;
		END ;
		RETURN GNR ;
	ELSIF TK = 2 THEN
		GetStringAusSpeicher (OGNM, VBL + reladr, 6) ;
		OGNM [6] := 0C ;
		GNR := SucheOGNM (OGNM) ;
	ELSIF TK = 3 THEN
		GetStringAusSpeicher (PGNM, VBL + reladr, 6) ;
		PGNM [6] := 0C ;
		GNR := SuchePGNM (PGNM) ;
	ELSE
		formaleFehler ;
		RETURN 0 ;
	END ;
	IF GNR = 0 THEN
		SSRfehler (8, 'Gebietsname ist unbekannt') ;
		RegQ.inh := wort ;
		RegQ.TK := TK ;
		RETURN 0 ;
	END ;
	RETURN GNR ;
END GNRausGBK ;


PROCEDURE AusgabeZeile (AA, LNG : CARDINAL ; Konsole, Ablauf : BOOLEAN) : BOOLEAN ;
	VAR	i, j,
		nl :		CARDINAL ;
		wort :		CARDINAL64 ;
		byte1 :		CARDINAL8 ;
		ch :		CHAR ;
BEGIN
	AA := AA BAND 0FFFFFEH ;		(* Ganzwort-Adresse erforderlich *)

	IF XABgefunden THEN
		RETURN TRUE ;			(* Ausgabe abgebrochen *)
	END ;

	byte1 := GetHalbwort (AA) SHR 16 ;	(* Vorschubsteuer-Zeichen *)
	nl := 0 ;
	ch := 0C ;
	IF byte1 < 48 THEN
		CASE byte1 BAND 0FH OF
		  0 :	nl := 6 ;
		| 1 :	nl := 5 ;
		| 2 :	nl := 4 ;
		| 3 :	nl := 3 ;
		| 4 :	nl := 2 ;
		| 5 :	nl := 1 ;				(* lf *)
		| 6 :	IF Konsole THEN
				Terminal.Write (CHR(13)) ;	(* cr *)
			END ;
		| 7 :	IF NOT Konsole THEN
				ch := CHR(12) ;			(* nf *)
			END ;
			nl := 1 ;
		| 0FH :	nl := 7 ;
		ELSE	nl := 1 ;				(* lf *)
		END ;
	ELSE
		nl := 1 ;
		ch := CHR (byte1) ;
	END ;
										AsmTrace.WriteLn ;
	IF Konsole THEN
		Takte (LNG * 1800) ;				(* geschätzte Taktzahl für 1 Protokoll-Zeile *)
	END ;
	IF Ablauf THEN
		Takte (LNG * 1800) ;				(* geschätzte Taktzahl für 1 Protokoll-Zeile *)
	END ;

	FOR i := 1 TO nl DO
		IF Konsole THEN
			Terminal.WriteLn ;
		END ;
		IF Ablauf THEN
			insAblaufProtokoll (CHR(10)) ;
		END ;
	END ;

	IF ch <> 0C THEN
		IF Konsole THEN
			Terminal.Write (ch) ;
		END ;
		IF Ablauf THEN
			insAblaufProtokoll (ch) ;
		END ;
	END ;

	FOR i := 1 TO LNG DO
		wort := GetGanzwort (AA + (i-1) * 2) SHL 16 ;
		IF AlarmGewesen THEN
			RETURN FALSE ;
		END ;
		FOR j := 0 TO 5 DO
			wort := wort ROL 8 ;
			IF (i <> 1) OR (j <> 0) THEN			(* 1. Byte war schon behandelt *)
				ch := ZC1toANSI [CHR (wort BAND 0FFH)] ;
				CASE ch OF
				0C :
				|
				eot :	RETURN TRUE ;			(* Textende ist Stringende *)
				ELSE
					IF Konsole THEN
						Terminal.Write (ch) ;
					END ;
					IF Ablauf THEN
						insAblaufProtokoll (ch) ;
					END ;
										AsmTrace.WriteString (ch) ;
				END ;
			END ;
		END ;
	END ;
	RETURN TRUE ;
END AusgabeZeile ;


PROCEDURE AusgabeListe (Konsole, Ablauf : BOOLEAN) : BOOLEAN ;
	VAR	AA,
		LNG,
		i :	CARDINAL ;
		liste :	BOOLEAN ;
BEGIN
	IF Konsole THEN
		TerminalEingabe.AusgabeBeginn ;
	END ;
	AA := GetHalbwort (VBL + 2) ;
	IF AktTypenkennung <> 2 THEN				(* Liste auswerten *)
		FOR i := 1 TO GetHalbwort (VBL + 3) DO
			IF AlarmGewesen THEN
				RETURN FALSE ;
			END ;
			IF NOT AusgabeZeile (GetHalbwort (AA + (i-1) * 2), GetHalbwort (AA + (i-1) * 2 + 1), Konsole, Ablauf) THEN
				RETURN FALSE ;
			END ;
		END ;
	ELSE
		IF NOT AusgabeZeile (AA, GetHalbwort (VBL + 3), Konsole, Ablauf) THEN
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END AusgabeListe ;



PROCEDURE upStart (VAR ON, OLN : ARRAY OF CHAR)  ;
	VAR	AA,
		LNG,
		OLKnr,
		i :		CARDINAL ;
		wort :		CARDINAL64 ;
BEGIN
	AA := GetHalbwort (VBL + 1) ;
	LNG := GetHalbwort (VBL + 2) SHR 8 BAND 0FFH ;
	GetStringAusSpeicher (ON, VBL + 6, 12) ;
	GetStringAusSpeicher (OLN, VBL + 10, 12) ;
	wort := GetGanzwort (AA) ;
	OLKnr := AktOperator + 1 ;
	IF OLKnr > HIGH (Operatoren) THEN
		SSRfehler (60H, 'zu viele verschachtelte Operatorläufe') ;
		RETURN ;
	END ;
	TraceF ('Start Stufe %c : %s als %s', OLKnr, ON, OLN) ;
										AsmTrace.WriteF ("\n\t* Start Stufe %c : Programm %s als %s", OLKnr, ON, OLN) ;
	WITH Operatoren [OLKnr].SteuerInformation DO
		Kopf.AbsOLK := AktOLK ;
		Kopf.LNG := LNG ;
		Kopf.TYP := 1 ;			(* TYP = STartsatz *)
		wort := VAL (CARDINAL64, AktOLK) SHL 24 BOR VAL(CARDINAL64, (LNG SHL 16) + 1) ;
		ZuwGanzwort (Inhalt [0], 2, wort) ;
		FOR i := 1 TO LNG DO
			GetGanzwortU (AA + (i - 1) * 2, Inhalt [i]) ;
			IF AlarmGewesen THEN
				BREAK ;
			END ;
		END ;
	END ;
END upStart ;


%IF %NOT WEB %THEN

PROCEDURE DateiDrucken (typ : CHAR) ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		Dateiname,
		Zeile :		ARRAY [0..255] OF CHAR ;
		SatzPuffer :	ARRAY [0..511] OF CHAR ;
		str :		ARRAY [0..15] OF CHAR ;
		i,
		j,
		SatzLaenge,
		GENVNR,
		strmnr :	CARDINAL ;
		zeilennr :	INTEGER ;
		ch :		CHAR ;

	PROCEDURE NL (anzahl : INTEGER) ;
	BEGIN
		CRausgeben ;
		IF zeilennr >= 0 THEN
			Vorschub (anzahl) ;
		ELSIF anzahl > 1 THEN
			Vorschub (anzahl - 1) ;
		END ;
		zeilennr := zeilennr + anzahl ;
	END NL ;

BEGIN
	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;

	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
		RETURN ;
	END ;

	AppendExtension (GENVNR, AktDateiname) ;

	strmnr := DateiOeffnen (AktDateiname, FALSE) ;
	IF strmnr = 0 THEN
		unzulaessigeAnwendung ('DateiOeffnen') ;
		RETURN ;
	END ;

	IF GetDATTR (strmnr) >= DattrMagnetband THEN
		unzulaessigeAnwendung ('DATTR extern') ;
		RETURN ;
	END ;

	IF GetDateityp (strmnr) <> SEQdatei THEN
		unzulaessigeAnwendung ('Dateityp nicht SEQ') ;
		RETURN ;
	END ;


	IF NOT OpenGDIdrucker (Drucker) THEN
		unzulaessigeAnwendung ('Drucker unbekannt') ;
		RETURN ;
	END ;

	JobInit ('TR440-Druck') ;

	GesamtInitialisierung ;

	SetZeilenAbstand (1) ;

	SetDefaultFontAktuell (DruckerFont) ;

	SetFontZeilenAbstand ;

	TeilInitialisierung (FALSE) ;

	SeitenAnfangAusgeben ;

	IF SetPosition (strmnr, 0) THEN						(* auf ersten Satz positionieren *)

		zeilennr := -1 ;
		REPEAT
			SatzLesen (strmnr, 0, SatzPuffer, SatzLaenge) ;			(* aktuellen Satz lesen *)

			ch := SatzPuffer [0] ;

			KonvertSatzNachExtern (SatzPuffer, Zeile, SatzLaenge) ;

			ZeilenAnfang ;

			IF typ = 'A' THEN
				Zeile := Zeile [1..SatzLaenge-1] ;
				CASE ch OF
				ZC1_NF :
					zeilennr := 255 ;
				|
				ZC1_CR :
					CRausgeben ;
				|
				ZC1_NL :
					NL (1) ;
				|
				ZC1_NL2 :
					NL (2) ;
				|
				ZC1_NL3 :
					NL (3) ;
				|
				ZC1_NL4 :
					NL (4) ;
				|
				ZC1_NL5 :
					NL (5) ;
				|
				ZC1_NL6 :
					NL (6) ;
				|
				ZC1_NL7 :
					NL (7) ;
				ELSE
					(* ungültiges Vorschubzeichen *)
					NL (1) ;
				END ;
			ELSE
				zeilennr := zeilennr + 1 ;
				CRausgeben ;
				IF zeilennr > 0 THEN
					ZeilenEndeAusgeben ;
				END ;
			END ;
			IF zeilennr >= MaxDruckZeilen THEN
				ZeilenEndeAusgeben ;
				SeitenWechselAusgeben ;
				SeitenAnfangAusgeben ;
				zeilennr := 0 ;
			END ;
			ZeichenAusgeben (Zeile) ;
		UNTIL NOT SetPositionRelativ (strmnr, 1) ;				(* auf nächsten Satz positionieren *)
	END ;

	DateiSchliessen (strmnr) ;

	SeitenWechselAusgeben ;

	TeilExit ;

	JobExit ;

	IF FileFunc.DeleteFile (AktDateiname) THEN
		IF debug.Dateien THEN
			TraceF ('Druckdatei gelöscht : %s', AktDateiname) ;
		END ;
	ELSE
		IF debug.Dateien THEN
			TraceF ('Druckdatei nicht löschbar : %s', AktDateiname) ;
		END ;
	END ;

	CloseGDIdrucker ;

END DateiDrucken ;

%END


PROCEDURE SSRmeldung (bez, str : ARRAY OF CHAR) ;
BEGIN
%IF %NOT WEB %THEN
	Terminal.WriteLn ;
	Terminal.WriteString ('+++++ SSR ') ;
	Terminal.WriteCard (ORD(SSRl)) ;
	Terminal.WriteString (' ') ;
	Terminal.WriteCard (ORD(SSRr)) ;
	Terminal.WriteString (' ') ;
	IF bez [0] <> 0C THEN
		Terminal.Write ("'") ;
		Terminal.WriteString (bez) ;
		Terminal.Write ("'") ;
		Terminal.WriteString (' : ') ;
	END ;
	Terminal.WriteString (str) ;
										AsmTrace.WriteLn ;
										AsmTrace.WriteString ('+++++ SSR ') ;
										AsmTrace.WriteCard (ORD(SSRl)) ;
										AsmTrace.WriteString (' ') ;
										AsmTrace.WriteCard (ORD(SSRr)) ;
										AsmTrace.WriteString (' ') ;
										IF bez [0] <> 0C THEN
											AsmTrace.WriteString ("'") ;
											AsmTrace.WriteString (bez) ;
											AsmTrace.WriteString ("'") ;
											AsmTrace.WriteString (' : ') ;
										END ;
										AsmTrace.WriteString (str) ;
%END
END  SSRmeldung ;


PROCEDURE SSRunzulaessig ;
BEGIN
	SSRmeldung ('', ' unbekannt') ;
	FS := 21H ;
END SSRunzulaessig ;


PROCEDURE upDumpe (wort : CARDINAL64) ;
	VAR	bytes :	ARRAY [0..7] OF CHAR ;
		j :	CARDINAL ;
		ch :	CHAR ;
BEGIN
	bytes [6] := 0C ;
	FOR j := 0 TO 5 DO
		ch := ZC1toANSI [CHR (wort SHR (40 - j*8))] ;
		IF ch < ' ' THEN
			ch := '.' ;
		END ;
		bytes [j] := ch ;
	END ;
	IF NOT debug.Lesen THEN
		TraceF ("\t%c %'06h %'06h  %s", ORD(AktTypenkennung),
			ORD ((wort SHR 24) BAND 0FFFFFFH),
			ORD (wort BAND 0FFFFFFH), bytes) ;
	END ;
	IF debug.BildschirmProtokoll THEN
		Terminal.WriteF ("\n\t\t* %c %'06h %'06h  %s",
			ORD(AktTypenkennung),
			ORD ((wort SHR 24) BAND 0FFFFFFH),
			ORD (wort BAND 0FFFFFFH), bytes) ;
	END ;
										AsmTrace.WriteF ("\n\t\t* %c %'06h %'06h  %s", ORD(AktTypenkennung),
												ORD ((wort SHR 24) BAND 0FFFFFFH), ORD (wort BAND 0FFFFFFH), bytes) ;
END upDumpe ;


PROCEDURE SSRaufruf (txt : ARRAY OF CHAR ; VBLworte : CARDINAL) : BOOLEAN ;
	VAR	wort :	CARDINAL64 ;
		i :	CARDINAL ;
BEGIN
	IF debug.SSRprotokoll THEN
		TraceF ("* SSR %c %c : %s", ORD(SSRl), ORD(SSRr), txt) ;

		IF debug.BildschirmProtokoll THEN
			Terminal.WriteF ("\n\t* SSR %c %c : %s", ORD(SSRl),
							ORD(SSRr), txt) ;
		END ;
										AsmTrace.WriteF ("\n\t* SSR %c %c : %s", ORD(SSRl), ORD(SSRr), txt) ;
		FOR i := 1 TO VBLworte DO
			wort := GetGanzwort (VBL + (i - 1) * 2) ;
			IF SpeicherschutzAlarmGewesen THEN
				RETURN FALSE ;
			END ;
			upDumpe (wort) ;
		END ;
	END ;
	RETURN TRUE ;
END SSRaufruf ;


PROCEDURE SSRpuffer (name : ARRAY OF CHAR; reladr : CARDINAL) ;
	VAR	wort :	CARDINAL64 ;
		i,
		adr :	CARDINAL ;
BEGIN
	IF debug.SSRprotokoll THEN
		adr := GetHalbwort (VBL + reladr) ;
		TraceF ("* %s : %h", name, adr) ;

		IF debug.BildschirmProtokoll THEN
			Terminal.WriteF ("\n\t* %s : %h", name, adr) ;
		END ;
										AsmTrace.WriteF ("\n\t* %s : %h", name, adr) ;
		FOR i := 1 TO 3 DO
			wort := GetGanzwort (adr + (i-1) * 2) ;
			IF SpeicherschutzAlarmGewesen THEN
				SpeicherschutzAlarmGewesen := FALSE ;
				BREAK ;
			END ;
			upDumpe (wort) ;
		END ;
	END ;
END SSRpuffer ;


PROCEDURE SSRfehler (fs : CARDINAL ; txt : ARRAY OF CHAR) ;
BEGIN
	FS := fs ;
	IF debug.SSRprotokoll THEN
		TraceF ("*** SSR %c %c - Fehler %'02h : %s",
					ORD(SSRl), ORD(SSRr), FS, txt) ;
		IF debug.BildschirmProtokoll THEN
			Terminal.WriteF ("\n\t* Fehler %'02h : %s", FS, txt) ;
		END ;
										AsmTrace.WriteF ("\n\t* Fehler %'02h : %s", FS, txt) ;
	END ;
END SSRfehler ;


PROCEDURE checkOEFDB (DBN : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF ExStrings.EqualI (DBN, _OEFDB) THEN
		fehlerOEFDB ;
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END checkOEFDB ;


PROCEDURE fehlerOEFDB ;
BEGIN
	SSRfehler (34H, 'Verändern der &OEFDB ist nicht zulässig') ;
END fehlerOEFDB ;


PROCEDURE unzulaessigeAnwendung (grund : ARRAY OF CHAR) ;
BEGIN
	SSRfehler (56H, 'unzulässige Anwendung des SSRs') ;
	IF debug.SSRprotokoll THEN
		TraceF (' unzul. %s', grund) ;
		IF debug.BildschirmProtokoll THEN
			Terminal.WriteF ('\n\t* unzul. %s', grund) ;
		END ;
										AsmTrace.WriteF ('\n\t* unzul. %s', grund) ;
	END ;
END unzulaessigeAnwendung ;


PROCEDURE formaleFehler ;
BEGIN
	SSRfehler (20H, 'formale Fehler in der Versorgung') ;
END formaleFehler ;


PROCEDURE SSRnimpl (txt : ARRAY OF CHAR) ;
BEGIN
	SSRmeldung (txt, ' nicht implementiert') ;
	SSRfehler (21H, 'Leistung nicht implementiert') ;
END SSRnimpl ;



END AbwicklerUp.
