
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_253;

(*	13.06.18	*)

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM DateiVerwaltung IMPORT * ;

FROM DateiBearbeitung IMPORT * ;

IMPORT Strings, ExStrings, Terminal, FileFunc, FileIO, AsmTrace, debug ;

FROM Trace IMPORT
	TraceF ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM AbwicklerUp IMPORT * ;

FROM upFestkomma IMPORT * ;

CONST
	GW0 : ARRAY OF CHAR =	{
				0C,				(* TK0 *)
				0C, 0C, 0C, 0C, 0C, 0C		(* 6 0-Bytes *)
				} ;

VAR
	frischPositioniert :	BOOLEAN = TRUE ;

	TeilauftragsNummer :	CARDINAL = 1 ;


PROCEDURE _SSR_253_1 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		str :		ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT SSRaufruf ('Kreation einer Datenbasis', 2) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|			DBA	*)
								(* 02     		DBN = Verzeichnisname			*)

	GetStringAusSpeicher (DBN, VBL + 2, 6) ;

        IF GetEchtenDatenbasisNamen (DBN, str) = TreeOK THEN
		SSRfehler (42H, 'Datenbasis existiert schon') ;
		RETURN ;
	END ;
	CreateDatenbasis (DBN, GetHalbwort (VBL+1) BAND 0FH) ;

END _SSR_253_1 ;


PROCEDURE _SSR_253_2 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		str :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT SSRaufruf ('Aufgabe einer Datenbasis', 2) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|				*)
								(* 02     		DBN = Verzeichnisname			*)

	GetStringAusSpeicher (DBN, VBL + 2, 6) ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF ExStrings.EqualI (DBN, _STDDB) OR ExStrings.EqualI (DBN, _PROTO) THEN
		SSRfehler (34H, 'Löschen der Datenbasis unzulässig') ;
		RETURN ;
	END ;

        IF GetEchtenDatenbasisNamen (DBN, str) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;
	DeleteDatenbasis (DBN) ;

END _SSR_253_2 ;


PROCEDURE _SSR_253_3 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		BKZ :		ARRAY [0..7] OF CHAR ;
		EXDKZ :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		GENVNR :	CARDINAL ;
		datei :		FileIO.FILE ;
		W6,
		W8,
		W18 :		CARDINAL64 ;
		lfd :		BOOLEAN ;
		typ,
		DTT :		tDateityp ;
BEGIN

	IF NOT SSRaufruf ('Kreation einer Datei', 13) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|			DTT	*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|			LNGDTB	*)
								(* 06							DATTR	*)
								(* 14				EXDKZL				*)
								(* 20 GB		BKZ = Oberverzeichnis			*)
	W6 := GetGanzwort (VBL + 6) ;
	W8 := GetGanzwort (VBL + 8) ;
	W18 := GetGanzwort (VBL + 18) ;

	DTT := VAL (tDateityp, (GetHalbwort (VBL + 1) SHR 4) BAND 0FFH) ;	(* SEQ/RAN/RAM/RAS/PHYS *)

	IF NOT upDateiAnf (DBN, Dateiname, BKZ, EXDKZ, GENVNR, lfd) THEN
		RETURN ;
	END ;


	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF W6 BAND 0FH = 3 THEN					(* Magnetband *)
		SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
		RETURN ;
	END ;

	IF DTT = RASdatei THEN
		SSRnimpl ('RAS-Dateien') ;
		RETURN ;
	END ;

	IF GENVNR = 0 THEN
		IF DateiExistiert (AktDateiname, GENVNR) THEN
			GENVNR := (GENVNR DIV 100 + 1) * 100 ;
		ELSE
			GENVNR := 100 ;
		END ;
	ELSIF DateiExistiert (AktDateiname, GENVNR) THEN
		IF EXDKZ [0] <> 0C THEN
			SSRfehler (80H, 'Datei existiert bereits auf Benutzerstapel') ;
		ELSE
			SSRfehler (44H, 'Datei existiert bereits') ;
		END ;
		RETURN ;
	END ;

	MerkeInfo (DTT,
		(* DL = *)	W8 SHR 24,
		(* WZ = *)	W8 BAND 0FFFFH,
		(* D = *)	(W8 SHR 20) BAND 0FH,
		(* Z= *)	(W8 SHR 16) BAND 0FH,
		(* DATTR = *)	W6 BAND 0FH,
		(* EZ= *)	(W18 SHR 4) BAND 0FFFFFH,
		(* E= *)	W18 BAND 0FH,
				BKZ,
				EXDKZ
				) ;

	IF NOT DateiKreieren (AktDateiname, GENVNR) THEN
		SSRfehler (73H, 'Datei ist zerstört') ;
		RETURN ;
	END ;

 	IF (W6 BAND 0FH) > 2 THEN	(* MB / WSP / LFD *)
		Einschleusen (DBN, Dateiname, BKZ, EXDKZ, GENVNR, lfd) ;
	END ;

	GENVNRausZahl (GENVNR, AktDateiname) ;
	PutStringInRegister (AktDateiname, RegH) ;
	IF (W6 BAND 0FH) = 5 THEN		(* LFD *)
		PutStringInRegister (BKZ, RegD) ;
	END ;

END _SSR_253_3 ;


PROCEDURE _SSR_253_4 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Löschen einer Datei', 3) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|			DTT	*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|	VTDT		LNGDTB	*)

	IF (GetGanzwort (VBL + 2) = 0) AND (GetGanzwort (VBL + 4) = 0) THEN
		(* kaputten Löschbefehl von PS&DATTRANSP ignorieren *)
		IF debug.Dateien THEN
			TraceF ('** sinnloser Löschauftrag SSR 253 4 ohne DBN/DTN') ;
		END ;
		RETURN ;
	END ;

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	ELSIF NOT DateiLoeschen (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
		RETURN ;
	END ;
	Ausschleusen (DBN, Dateiname) ;
END _SSR_253_4 ;


PROCEDURE _SSR_253_5 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Kenndaten verändern', 15) THEN
		RETURN ;
	END ;

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;
	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
	END ;

	GENVNRausZahl (GENVNR, Dateiname) ;
	PutStringInRegister (Dateiname, RegH) ;
END _SSR_253_5 ;


PROCEDURE _SSR_253_6 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		attr :		FileFunc.FileAttributeSet ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Schreibsperre setzen', 3) THEN
		RETURN ;
	END ;

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;
	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
	END ;
	AppendExtension (GENVNR, AktDateiname) ;

	IF NOT FileFunc.GetFileAttr (AktDateiname, attr) THEN
		unzulaessigeAnwendung ('GetFileAttr') ;
		RETURN ;
	END ;

	IF NOT FileFunc.SetFileAttr (AktDateiname, attr + FileFunc.FileAttributeSet {FileFunc.ReadOnly} ) THEN
		unzulaessigeAnwendung ('SetReadOnly') ;
	END ;
END _SSR_253_6 ;


PROCEDURE _SSR_253_7 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		attr :		FileFunc.FileAttributeSet ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Schreibsperre lösen', 3) THEN
		RETURN ;
	END ;

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;
	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
	END ;
	AppendExtension (GENVNR, AktDateiname) ;

	IF NOT FileFunc.GetFileAttr (AktDateiname, attr) THEN
		unzulaessigeAnwendung ('GetAttr') ;
		RETURN ;
	END ;

	IF NOT FileFunc.SetFileAttr (AktDateiname, attr - FileFunc.FileAttributeSet {FileFunc.ReadOnly} ) THEN
		unzulaessigeAnwendung ('SetReadWrite') ;
	END ;
END _SSR_253_7 ;


PROCEDURE _SSR_253_8 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		BKZ :		ARRAY [0..7] OF CHAR ;
		EXDKZ :		ARRAY [0..7] OF CHAR ;
		Dateiname :	ARRAY [0..255] OF CHAR ;
		lfd :		BOOLEAN ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Einschleusen einer Datei', 11) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|				*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|			LNGDTB	*)
								(* 06							DATTR	*)
								(* 14				EXDKZL				*)
								(* 20 GB		BKZ = Oberverzeichnis			*)

	IF NOT upDateiAnf (DBN, Dateiname, BKZ, EXDKZ, GENVNR, lfd) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	IF GetHalbwort (VBL + 7) BAND 0FH = 3 THEN					(* Magnetband *)
		SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
		RETURN ;
	END ;

	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
		RETURN ;
	END ;

 	Einschleusen (DBN, Dateiname, BKZ, EXDKZ, GENVNR, lfd) ;

	GENVNRausZahl (GENVNR, AktDateiname) ;
	PutStringInRegister (AktDateiname, RegH) ;
END _SSR_253_8 ;


PROCEDURE _SSR_253_9 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		str,
		Dateiname :	ARRAY [0..255] OF CHAR ;
		strmnr :	CARDINAL ;
		AARUECK :	SpeicherAdresse ;
		BA :		CARDINAL8 ;
		GENVNR :	CARDINAL ;
BEGIN

	IF NOT SSRaufruf ('Bearbeitung eröffnen', 8) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|		strnr/AARUECK	*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|			LNGDTB	*)
								(* 06						      dattr BA	*)
								(* 14			exdkz zurück				*)

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	END ;

	AARUECK := GetHalbwort (VBL + 1) ;

	IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
		RETURN ;
	END ;

	AppendExtension (GENVNR, AktDateiname) ;

	BA := GetHalbwort (VBL + 7) BAND 0FH ;

	IF (BA > 2) AND NOT checkOEFDB (DBN) THEN
		RETURN ;
	END ;

	strmnr := DateiOeffnen (AktDateiname, BA > 2) ;
	IF strmnr = 0 THEN
		unzulaessigeAnwendung ('DateiOeffnen') ;
		RETURN ;
	END ;

	SetDBN (strmnr, DBN) ;

	IF BA = 0 THEN
		SetPosition (strmnr, 0FFFFFFFFH) ;
	ELSE
		SetPosition (strmnr, 0) ;
	END ;

	IF (GetHalbwort (VBL + 7) BAND 100H) <> 0 THEN		(* J = 1 : AARUECK ist gültig *)
		IF ODD(AARUECK) THEN
			formaleFehler ;
			RETURN ;
		END ;
		IF AARUECK <> 0 THEN
			NeueTypenkennung := 3 ;
			PutStringInGanzwortSpeicher (Dateiname, AARUECK, 12) ;
			GENVNRausZahl (GENVNR, str) ;
			NeueTypenkennung := 3 ;
			PutStringInGanzwortSpeicher (str, AARUECK+4, 6) ;
		END ;
	END ;

	PutHalbwort (VBL + 1, strmnr) ;

	GENVNRausZahl (GENVNR, str) ;
	PutStringInRegister (str, RegH) ;

	SetAktSatzIndexGW (strmnr, 0) ;

END _SSR_253_9 ;


PROCEDURE _SSR_253_10 ;
	VAR	strmnr,
		PufferAdresse,
		PufferLaenge,
		AnzahlBytes,
		S7S8,
		i :			CARDINAL ;
		satznr :		CARDINAL64 ;
		AAOP :			SpeicherAdresse ;
		LNGOP,
		SLNG,
		indexGW :			CARDINAL ;
		merk,
		FS :			BOOLEAN ;
		BA :			tBetriebsart ;
		DTT :			tDateityp ;

BEGIN
	IF NOT SSRaufruf ('Satztransport', 6) THEN
		RETURN ;
	END ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	S7S8 := (GetHalbwort (VBL + 1) SHR 16) BAND 3 ;

	AAOP := GetHalbwort (VBL + 8) ;
	IF ODD (AAOP) THEN
		formaleFehler ;
		RETURN ;
	END ;
	FS := AktTypenkennung = 3 ;
	LNGOP := GetHalbwort (VBL + 9) ;

	SSRpuffer ('Buffer', 8) ;

	IF AktSatz = NIL THEN
		NEW (AktSatz) ;
	END ;

	BA := GetDateiBetriebsart (strmnr) ;
	DTT :=	GetDateityp (strmnr) ;

	satznr := GetPosition (strmnr) ;
	PutGanzwort (VBL + 10, 3, satznr) ;		(* spos *)

	RegH.TK := 3 ;

	RegD.inh := 0 ;		(* RM muss nochmal untersucht werden *)
	RegD.TK := 1 ;

	RegA.TK := 3 ;		(* vorläufig : Puffer enthält das Satzende *)

	RegQ.TK := 3 ;		(* kein Materialende *)

	IF BA = BAschreibend THEN
		AnzahlBytes := LNGOP * SIZE(Ganzwort) ;

		CASE DTT OF
		SEQdatei :	IF S7S8 <> 0 THEN
					formaleFehler ;
					RETURN ;
				END ;
		|
		RANdatei,
		RAMdatei :	CASE S7S8 OF
				0 :	(* auf jeden Fall schreiben - egal ob existiert oder nicht *)
				|
				1 :	(* nur schreiben, wenn noch nicht existiert *)
					IF (GetAktSatzIndexGW (strmnr) <> 0) AND SatzSuchen (strmnr, satznr) THEN
						RegD.inh := 5 ;		(* Satz existiert bereits *)
						RETURN ;
					END ;
				|
				2 :	(* nur schreiben, wenn schon existiert *)
					IF (GetAktSatzIndexGW (strmnr) = 0) AND NOT SatzSuchen (strmnr, satznr) THEN
						RegD.inh := 6 ;		(* Satz existiert nicht *)
						RETURN ;
					END ;
				ELSE
					formaleFehler ;
				END ;
		|
		RASdatei :	SSRnimpl ('RAS-Schreiben') ;
		|
		ELSE		formaleFehler ;
		END ;

		IF LNGOP = 0 THEN
			indexGW := GetAktSatzIndexGW (strmnr) ;
			IF FS AND (indexGW = 0) THEN					(* kompletter Leersatz soll geschrieben werden *)
				IF NOT SatzSchreiben (strmnr, satznr, GW0) THEN	(* wird intern ersetzt durch 1 GW 0 mit TK0 *)
					SSRfehler (4EH, 'Datei wird zu gross') ;
					RETURN ;
				END ;
			END ;

			RegQ.inh := 0 ;
		ELSE
			merk := debug.Lesen ;
			debug.Lesen := FALSE ;
			FOR i := 1 TO LNGOP DO
				GetGanzwortU (AAOP + (i-1) * 2, AktSatz ^ .GWe [i-1]) ;
			END ;
			debug.Lesen := merk ;

			IF NOT SatzSchreiben (strmnr, satznr, AktSatz ^ .bytes [0..AnzahlBytes-1]) THEN
				SSRfehler (4EH, 'Datei wird zu gross') ;
				RETURN ;
			END ;

			RegQ.inh := GetAktSatzLaengeGW (strmnr)  ;
		END ;

		IF FS THEN
			SetAktSatzIndexGW (strmnr, 0) ;
			SetPosition (strmnr, satznr+1) ;
		END ;

	ELSE
		indexGW := GetAktSatzIndexGW (strmnr) ;

		IF NOT SatzLesen (strmnr, 0, AktSatz ^ .bytes [0..LNGOP * SIZE(Ganzwort) - 1] , AnzahlBytes) THEN
			SSRfehler (49H, 'Datei leer oder zu lesender Satz nicht definiert') ;
			RETURN ;
		END ;

		SLNG := GetAktSatzLaengeGW (strmnr)  ;

		IF SLNG - indexGW <= LNGOP THEN
			LNGOP := SLNG - indexGW ;
		ELSE
			RegA.TK := 2 ;		(* Puffer enthält nicht das Satzende *)
		END ;

		merk := debug.Speichern ;
		debug.Speichern := FALSE ;
		FOR i := 1 TO LNGOP DO
			PutGanzwortU (AAOP + (i-1) * 2, AktSatz ^ .GWe [i-1]) ;
		END ;
		debug.Speichern := merk ;
		RegQ.inh := SLNG ;

		SSRpuffer ('gelesen', 8) ;

		IF FS THEN
			SetAktSatzIndexGW (strmnr, 0) ;
			frischPositioniert := TRUE ;
			IF BA = BAvorwaerts THEN
				IF NOT SetPositionRelativ (strmnr, 1) THEN
					RegH.TK := 2 ;		(* DE=2 : Satz ist der letzte in der Datei *)
				END ;
			ELSE
				IF NOT SetPositionRelativ (strmnr, -1) THEN
					RegH.TK := 2 ;		(* DE=2 : Satz ist der erste in der Datei *)
				END ;
			END ;
		END ;
	END ;

	Takte (LNGOP * 1700) ;				(* geschätzte Taktzahl pro Satztransport *)

	RegA.inh := LNGOP ;

	RegH.inh := satznr ;
END _SSR_253_10 ;


PROCEDURE _SSR_253_11 ;
	VAR	strmnr,
		T,
		RMspez :	CARDINAL ;
		SPOS :		CARDINAL64 ;
		SRB :		INTEGER ;
		posok :		BOOLEAN ;
BEGIN
	IF NOT SSRaufruf ('Satzposition einstellen', 6) THEN
		RETURN ;
	END ;


	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	SetAktSatzIndexGW (strmnr, 0) ;

	SRB := tr24toInt32 (GetHalbwort (VBL + 7)) ;
	T := AktTypenkennung ;

	SPOS := GetGanzwort (VBL + 10) ;

	posok := TRUE ;
	RMspez := 0 ;

	CASE GetDateityp (strmnr) OF
	SEQdatei :
		CASE T OF
		1 :	posok := SetPosition (strmnr, 0) ;
		|
		2 :	posok := SetPosition (strmnr, 0FFFFFFFFH) ;
		|
		3 :	IF SRB = 1 THEN
				posok := SetPositionRelativ (strmnr, 1) ;
			ELSIF SRB = -1 THEN
				posok := SetPositionRelativ (strmnr, -1) ;
			ELSIF SRB = 0 THEN
				(* schon korrekt positioniert *)
			ELSE
				formaleFehler ;
				RETURN ;
			END ;
		ELSE
		END ;
	| RANdatei :
		CASE T OF
		1 :	IF GetDateiBetriebsart (strmnr) = BAschreibend THEN
				SetPosition (strmnr, 1 + SRB) ;
				posok := TRUE ;
			ELSE
				posok := SetPosition (strmnr, 0) ;
				IF SRB > 0 THEN
					posok := SetPositionRelativ (strmnr, SRB) ;
				END ;
			END ;
		|
		2 :	posok := SetPosition (strmnr, 0FFFFFFFFH) ;
			IF SRB < 0 THEN
				posok := SetPositionRelativ (strmnr, SRB) ;
			END ;
		|
		3 :	IF GetDateiBetriebsart (strmnr) = BAschreibend THEN
				SetPosition (strmnr, VAL (INTEGER, SPOS) + SRB) ;
			ELSE
				posok := SetPosition (strmnr, SPOS) ;
				IF SRB <> 0 THEN
					posok := SetPositionRelativ (strmnr, SRB) ;
				END ;
				IF NOT posok AND (SPOS > GetLetzteSatznr (strmnr)) THEN
					RMspez := 2 ;
				END ;
			END ;
		ELSE
		END ;
	| RAMdatei :
		CASE T OF
		1 :	posok := SetPosition (strmnr, 0) ;
			IF SRB > 0 THEN
				posok := SetPositionRelativ (strmnr, SRB) ;
			END ;
		|
		2 :	posok := SetPosition (strmnr, 0FFFFFFFFH) ;
			IF SRB < 0 THEN
				posok := SetPositionRelativ (strmnr, SRB) ;
			END ;
		|
		3 :	posok := SetPosition (strmnr, SPOS) ;
			IF GetDateiBetriebsart (strmnr) <> BAschreibend THEN
				IF SRB <> 0 THEN
					posok := SetPositionRelativ (strmnr, SRB) ;
				END ;
				IF NOT posok AND (SPOS > GetLetzteSatznr (strmnr)) THEN
					RMspez := 2 ;
				END ;
			ELSE
				posok := TRUE ;
			END ;
		ELSE
		END ;
	| RASdatei :
		SSRnimpl ('RAS-Transposrt') ;
	| PHYSdatei :
		unzulaessigeAnwendung ('Positionierung PHYSdatei') ;
	ELSE
		unzulaessigeAnwendung ('DateiTyp') ;
	END ;

	PutGanzwort (VBL + 10, 3, GetPosition (strmnr)) ;	(* spos *)

	RegQ.inh := 0 ;
	RegQ.TK := 3 ;		(* kein Materialende *)

	IF RMspez <> 0 THEN
		RegD.inh := RMspez ;	(* RM = 2/3/4 *)
	ELSIF posok THEN
		RegD.inh := 0 ;		(* RM : Positionierung gelungen *)
	ELSE
		RegD.inh := 1 ;		(* RM : Positionierung misslungen *)
	END ;

	frischPositioniert := TRUE ;

	RegD.TK := 1 ;

END _SSR_253_11 ;


PROCEDURE _SSR_253_12 ;
	VAR	strmnr,
		BA :	CARDINAL ;
BEGIN
	IF NOT SSRaufruf ('Betriebsart einstellen', 4) THEN
		RETURN ;
	END ;


	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	BA := GetHalbwort (VBL + 7) BAND 0FH ;
	CASE BA OF
	0 :		SetDateiBetriebsart (strmnr, BArueckwaerts) ;
	|
	2 :		SetDateiBetriebsart (strmnr, BAvorwaerts) ;
	|
	5,
	6,
	7 :		SetDateiBetriebsart (strmnr, BAschreibend) ;

	ELSE
		formaleFehler ;
		RETURN ;
	END ;

END _SSR_253_12 ;


PROCEDURE _SSR_253_13 ;
	VAR	strmnr :	CARDINAL ;
		SRBAB,
		SRBEB :		INTEGER ;
		I :		CARDINAL ;
		SPOSAB,
		SPOSEB :	CARDINAL64 ;
		DTT :		tDateityp ;
BEGIN
	IF NOT SSRaufruf ('Sätze löschen', 7) THEN
		RETURN ;
	END ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	DTT := GetDateityp (strmnr) ;

	SetAktSatzIndexGW (strmnr, 0) ;

	GetGanzwort (VBL +2) ;
	I := AktTypenkennung ;

	SRBAB := GetHalbwort (VBL + 6) ;

	CASE DTT OF
	SEQdatei :	IF I = 3 THEN
				unzulaessigeAnwendung ('SEQdatei') ;
				RETURN ;
			END ;
			SPOSAB := GetPosition (strmnr) + VAL (CARDINAL64, SRBAB) ;
			SPOSEB := 0FFFFFFFFH ;
			SetPosition (strmnr, SPOSAB) ;
	|
	RANdatei,
	RAMdatei :
			SPOSAB := GetGanzwort (VBL + 10) ;
			IF SetPosition (strmnr, SPOSAB) THEN
				IF SRBAB <> 0 THEN
					SetPositionRelativ (strmnr, 1) ;
				END ;
			ELSE
				SetPositionRelativ (strmnr, 1) ;
			END ;
			SPOSAB := GetPosition (strmnr) ;
			IF I <> 3 THEN
				SPOSEB := 0FFFFFFFFH ;	(* ab SPOSAB Rest der Datei löschen *)
			ELSE
				SPOSEB := GetGanzwort (VBL + 12) ;
				SRBEB := GetHalbwort (VBL + 7) ;
				IF SetPosition (strmnr, SPOSEB) THEN
					IF SRBEB <> 0 THEN
						SetPositionRelativ (strmnr, -1 ) ;
					END ;
				END ;
				SPOSEB := GetPosition (strmnr) ;
			END ;
	|
	RASdatei :
		SSRnimpl ('Sätze löschen') ;
		RETURN ;
	ELSE
		unzulaessigeAnwendung ('DateiTyp') ;
		RETURN ;
	END ;

	LOOP
		IF NOT SatzLoeschen (strmnr, SPOSAB) THEN
			EXIT ;
		END ;
		SPOSAB := GetPosition (strmnr) ;	(* automatisch nächster def. Satz *)
		IF (SPOSAB = 0) OR (SPOSAB > SPOSEB) THEN
			EXIT ;
		END ;
	END ;

END _SSR_253_13 ;


PROCEDURE _SSR_253_14 ;
	VAR	strmnr :	CARDINAL ;
BEGIN
	IF NOT SSRaufruf ('Bearbeitung beenden', 1) THEN
		RETURN ;
	END ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	DateiSchliessen (strmnr) ;
END _SSR_253_14 ;


PROCEDURE _SSR_253_15 ;
BEGIN
	SSRnimpl ('Kennsatz schreiben') ;
END _SSR_253_15 ;


PROCEDURE _SSR_253_16 ;
BEGIN
	SSRnimpl ('Kennsatz lesen') ;
END _SSR_253_16 ;


PROCEDURE _SSR_253_17 ;
	VAR	strmnr :	CARDINAL ;
BEGIN
	SSRaufruf ('Freie Satzpositionen ausliefern', 6) ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	RegA.TK := 1 ;
	RegA.inh := LueckeSuchen (strmnr, GetHalbwort (VBL + 11)) ;
END _SSR_253_17 ;


PROCEDURE _SSR_253_18 ;
	VAR	strmnr,
		satznr,
		IT :		CARDINAL ;
BEGIN
	IF NOT SSRaufruf ('Belegte Satzpositionen ausliefern', 6) THEN
		RETURN ;
	END ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	SetAktSatzIndexGW (strmnr, 0) ;

	IT := GetHalbwort (VBL + 11) ;
	CASE IT OF
	1 :	satznr := GetErsteSatznr (strmnr) ;
	|
	2 :	satznr := GetLetzteSatznr (strmnr) ;
	ELSE
		unzulaessigeAnwendung ('IT > 2') ;
		RETURN ;
	END ;

	RegA.TK := 3 ;
	RegA.inh := satznr ;
END _SSR_253_18 ;


PROCEDURE _SSR_253_19 ;
	VAR	INS,
		strmnr,
		GENVNR,
		DTT, DL, WZ,
		D, Z, DATTR,
		EZ, E,
		SAETZE,
		AADTB,
		dh, lf,
		VTDT :		CARDINAL ;
		DBN,
		str,
		DTB,
		Dateiname :	ARRAY [0..19] OF CHAR ;
		genvnr,
		BKZ,
		EXDKZ,
		BKZ2,
		EXDKZ2 :	ARRAY [0..7] OF CHAR ;
BEGIN
	IF NOT SSRaufruf ('Kenndaten ausliefern', 14) THEN
		RETURN ;
	END ;

	AADTB := GetHalbwort (VBL + 4) ;
	INS := AktTypenkennung ;

	CASE INS OF
	1 :
		strmnr := GetHalbwort (VBL + 5) ;
		IF NOT CheckInfoStrmnr (strmnr) THEN
			SSRfehler (45H, 'Dateibearbeitung nicht eröffnet') ;
			RETURN  ;
		END ;

	|
	2 :	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
			RETURN ;
		END ;

		VTDT := GetHalbwort (VBL + 5) SHR 16 ;
		CASE VTDT OF
		1 :		(*	LFD	*)
			IF (GetGanzwort (VBL + 26) = 0) OR (AktTypenkennung <> 3) THEN
				BKZ := AktAuftrag.BKZ ;
			ELSE
				GetStringAusSpeicher (BKZ, VBL + 26, 12) ;
			END ;
			IF GetExternDateinamen (Dateiname, BKZ, '', AktDateiname, TRUE) <> TreeOK THEN
				SSRfehler (dateiFS, dateiFStext) ;
				RETURN ;
			END ;
			DATTR := ORD(DattrLFD) ;
		|
		2 :		(*	WSP	*)
			GetStringAusSpeicher (BKZ, VBL + 26, 12) ;
			GetStringAusSpeicher (EXDKZ, VBL + 14, 6) ;
			IF GetExternDateinamen (Dateiname, BKZ, EXDKZ, AktDateiname, FALSE) <> TreeOK THEN
				SSRfehler (dateiFS, dateiFStext) ;
				RETURN ;
			END ;
			DATTR := ORD(DattrWechselplatte) ;
		ELSE
			IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
				SSRfehler (dateiFS, dateiFStext) ;
				RETURN ;
			END ;
		END ;

		IF NOT DateiExistiert (AktDateiname, GENVNR) THEN
			SSRfehler (43H, 'Datei existiert nicht') ;
			RETURN ;
		END ;
		AppendExtension (GENVNR, AktDateiname) ;
		IF NOT CheckInfoDatnam (AktDateiname) THEN
			RETURN ;
		END ;
	|
	3 :
		strmnr := GetHalbwort (VBL + 5) ;
		IF NOT CheckInfoStrmnr (strmnr) THEN
			SSRfehler (45H, 'Dateibearbeitung nicht eröffnet') ;
			RETURN  ;
		END ;
	ELSE
		formaleFehler ;
		RETURN ;
	END ;


	GetInfo (DTB, GENVNR, DTT, DL, WZ, D, Z, DATTR, EZ, E, SAETZE, BKZ2, EXDKZ2) ;
	IF BKZ2 [0] > ' ' THEN
		BKZ := BKZ2 ;
	END ;
	IF EXDKZ2 [0] > ' ' THEN
		EXDKZ := EXDKZ2 ;
	END ;


	IF INS = 1 THEN
		NeueTypenkennung := 3 ;
		PutStringInGanzwortSpeicher (DTB, AADTB, 12) ;

		GENVNRausZahl (GENVNR, str) ;
		NeueTypenkennung := 3 ;
		PutStringInGanzwortSpeicher (str, AADTB+4, 6) ;
	END ;

	PutHalbwort (VBL + 1, DTT SHL 4) ;

	lf := 0 ;
	dh := 2 ;
	CASE DATTR OF
	4 :	(* WSP *)
		dh := 3 ;
	|
	5 :	(* LFD *)
		dh := 3 ;
		lf := 800000H ;
	ELSE
	END ;

	GENVNRausZahl (GENVNR, genvnr) ;

	PutGanzwort (VBL + 6, dh, lf BOR DATTR) ;

	PutGanzwort (VBL + 8, 3, (D SHL 20) BOR (Z SHL 16) BOR WZ) ;
	PutHalbwort (VBL + 8, DL) ;

	PutGanzwort (VBL + 10, 2, 0) ;						(* kein Verfall *)

	NeueTypenkennung := 3 ;
	PutStringInGanzwortSpeicher (EXDKZ, VBL + 14, 6) ;			(* exdkz *)

	PutGanzwort (VBL + 16, 2, 0) ;						(* gk / SLD / SL *)
	PutGanzwort (VBL + 18, 2, (EZ SHL 4) BOR E) ;				(* ez / e *)

	NeueTypenkennung := 3 ;
	PutStringInGanzwortSpeicher (genvnr, VBL + 20, 6) ;			(* gennr / versnr *)

	IF BKZ [0] = 0C THEN
		PutGanzwort (VBL + 26, 2, 0) ;
	ELSE
		NeueTypenkennung := 3 ;
		PutStringInGanzwortSpeicher (BKZ, VBL + 26, 6) ;		(* GB / BKU *)
	END ;

	NeueTypenkennung := 3 ;
	PutStringInRegister (genvnr, RegH) ;

	RegA.inh := VAL (CARDINAL64, SAETZE) SHL 24 ;
	RegA.TK := 3 ;
END _SSR_253_19 ;


PROCEDURE _SSR_253_20 ;
	VAR	DBN,
		str,
		Dateiname,
		DateinameNeu :		ARRAY [0..19] OF CHAR ;
		AktDateiNameNeu :	ARRAY [0..255] OF CHAR ;
		GENVNRalt,
		GENVNRneu :		CARDINAL ;
BEGIN
	IF NOT SSRaufruf ('Dateibezeichnung verändern', 4) THEN
		RETURN ;
	END ;
								(* VBL :							*)
								(* 00	FA			|				*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|			LNGDTB	*)
								(* 06	AADTBN					        LNGDTBN	*)
	IF NOT upDateiName (DBN, Dateiname, GENVNRalt) THEN
		RETURN ;
	END ;

	IF NOT upGet_AADTB_LNGDTB (DateinameNeu, 6, GENVNRneu) THEN
		RETURN ;
	END ;

	IF GetEchtenDateinamen (DBN, Dateiname, AktDateiname) <> TreeOK THEN
		SSRfehler (dateiFS, dateiFStext) ;
		RETURN ;
	ELSIF NOT DateiExistiert (AktDateiname, GENVNRalt) THEN
		SSRfehler (43H, 'Datei existiert nicht') ;
		RETURN ;
	END ;

	FileFunc.ConstructFileName (DateinameNeu, AktDateiname, AktDateiNameNeu) ;
	IF GENVNRneu <> 0 THEN
		IF DateiExistiert (AktDateiNameNeu, GENVNRneu) THEN
			SSRfehler (44H, 'neue Datei existiert schon') ;
			RETURN ;
		END ;
	ELSIF DateiExistiert (AktDateiNameNeu, GENVNRneu) THEN
		GENVNRneu := (GENVNRneu DIV 100 + 1) * 100 ;
	ELSE
		GENVNRneu := 100 ;
	END ;
	AppendExtension (GENVNRalt, AktDateiname) ;
	AppendExtension (GENVNRneu, AktDateiNameNeu) ;
	IF debug.Dateien THEN
		TraceF ('rename %s --> %s', AktDateiname, AktDateiNameNeu) ;
	END ;
	IF NOT FileFunc.RenameFile (AktDateiname, AktDateiNameNeu) THEN
		unzulaessigeAnwendung ('Rename') ;
		RETURN ;
	END ;

	GENVNRausZahl (GENVNRneu, str) ;
	PutStringInRegister (str, RegH) ;

END _SSR_253_20 ;


PROCEDURE _SSR_253_21 ;
BEGIN
	SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
END _SSR_253_21 ;


PROCEDURE _SSR_253_22 ;
	VAR	strmnr :	CARDINAL ;
		AAOP :		SpeicherAdresse ;
		satznr,
		AnzahlBytes,
		LNGOP,
		i,
		SPOS,
		BA :		CARDINAL ;
		merk,
		FS :		BOOLEAN ;
		aktBA :		tBetriebsart ;
BEGIN
	IF NOT SSRaufruf ('Sätze überschreiben', 6) THEN
		RETURN ;
	END ;


	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	SetAktSatzIndexGW (strmnr, 0) ;

	BA := GetHalbwort (VBL + 7) BAND 0FH ;

	AAOP := GetHalbwort (VBL + 8) ;
	IF ODD (AAOP) THEN
		formaleFehler ;
		RETURN ;
	END ;
	FS := AktTypenkennung = 3 ;
	LNGOP := GetHalbwort (VBL + 9) ;
	SPOS := GetGanzwort (VBL + 10) ;

	SSRpuffer ('Buffer', 8) ;

	IF AktSatz = NIL THEN
		NEW (AktSatz) ;
	END ;

	aktBA := GetDateiBetriebsart (strmnr) ;

	AnzahlBytes := LNGOP * SIZE(Ganzwort) ;
	satznr := GetPosition (strmnr) ;

	SetAktSatzIndexGW (strmnr, 0) ;

	RegD.inh := 0 ;		(* RM muss nochmal untersucht werden *)
	RegD.TK := 1 ;

	IF (aktBA = BAschreibend) <> (BA > 2) THEN		(* Betriebsart-Wechsel *)
		IF BA = 2 THEN
						(* Wechsel schreiben -> lesen *)
			SetPosition (strmnr, satznr-1) ;
			IF SetPositionRelativ (strmnr, 1) THEN
				satznr := GetPosition (strmnr) ;
			END ;
			SetDateiBetriebsart (strmnr, BAvorwaerts) ;
		ELSE
						(* Wechsel lesen -> schreiben *)
			SetDateiBetriebsart (strmnr, BAschreibend) ;
		END ;
		aktBA := GetDateiBetriebsart (strmnr) ;
	ELSE
		IF (BA = 2) AND NOT frischPositioniert THEN
			IF SetPositionRelativ (strmnr, 1) THEN
				satznr := GetPosition (strmnr) ;
			END ;
		END ;
	END ;

	IF aktBA = BAschreibend THEN
		IF LNGOP = 0 THEN
			IF NOT SatzSchreiben (strmnr, satznr, GW0) THEN	(* wird intern ersetzt durch 1 GW 0 mit TK0 *)
				SSRfehler (4EH, 'Datei wird zu gross') ;
				RETURN ;
			END ;

			RegQ.inh := 0 ;
		ELSE
			merk := debug.Lesen ;
			debug.Lesen := FALSE ;
			FOR i := 1 TO LNGOP DO
				GetGanzwortU (AAOP + (i-1) * 2, AktSatz ^ .GWe [i-1]) ;
			END ;
			debug.Lesen := merk ;

			IF NOT SatzSchreiben (strmnr, satznr, AktSatz ^ .bytes [0..AnzahlBytes-1]) THEN
				SSRfehler (4EH, 'Datei wird zu gross') ;
				RETURN ;
			END ;

			RegQ.inh := GetAktSatzLaengeGW (strmnr)  ;
		END ;

		SetAktSatzIndexGW (strmnr, 0) ;
		SetPosition (strmnr, satznr+1) ;
	ELSE
		SetAktSatzIndexGW (strmnr, 0) ;
		IF NOT SatzLesen (strmnr, 0, AktSatz ^ .bytes [0..(LNGOP-1) * SIZE(Ganzwort)] , AnzahlBytes) THEN
			SSRfehler (49H, 'Datei leer oder zu lesender Satz nicht definiert') ;
			RETURN ;
		END ;

		RegQ.inh := GetAktSatzLaengeGW (strmnr)  ;

		IF AnzahlBytes <= (LNGOP * SIZE(Ganzwort)) THEN
			LNGOP := AnzahlBytes DIV SIZE(Ganzwort) ;
		ELSE
			RegA.TK := 2 ;		(* Puffer enthält nicht das Satzende *)
		END ;

		merk := debug.Speichern ;
		debug.Speichern := FALSE ;
		FOR i := 1 TO LNGOP DO
			PutGanzwortU (AAOP + (i-1) * 2, AktSatz ^ .GWe [i-1]) ;
		END ;
		debug.Speichern := merk ;

		SSRpuffer ('gelesen', 8) ;
	END ;

	RegA.inh := AnzahlBytes DIV SIZE(Ganzwort) ;
	RegA.TK := 3 ;		(* Puffer enthält das Satzende *)
	RegQ.inh := GetAktSatzLaengeGW (strmnr)  ;
	RegQ.TK := 0 ;		(* kein Materialende *)
	RegH.inh := satznr ;

	PutGanzwort (VBL + 10, 3, satznr) ;

	frischPositioniert := FALSE ;

	IF letzterSatz (strmnr) THEN
		RegH.TK := 2 ;		(* =2 : Satz ist der letzte in der Datei *)
	ELSE
		RegH.TK := 3 ;
	END ;

	SetAktSatzIndexGW (strmnr, 0) ;

END _SSR_253_22 ;


PROCEDURE _SSR_253_23 ;
	VAR	strmnr,
		AAOP,
		T, ANZ, M,
		ABN :		CARDINAL ;
		BA :		tBetriebsart ;
BEGIN
	IF NOT SSRaufruf ('Transport eines Blockes', 3) THEN
		RETURN ;
	END ;

	IF NOT upStrmnr (strmnr) THEN
		RETURN ;
	END ;

	AAOP := GetHalbwort (VBL + 2) ;
	IF ODD (AAOP) THEN
		formaleFehler ;
		RETURN ;
	END ;
	T := AktTypenkennung ;
	ANZ := GetHalbwort (VBL + 3) ;
	M := GetHalbwort (VBL + 4) ;
	IF M <> 0 THEN			(* MB-Datei nicht möglich *)
		unzulaessigeAnwendung ('MB-Datei') ;
		RETURN ;
	END ;
	IF T = 3 THEN			(* Positionieren *)
		RETURN ;
	END ;

	SSRpuffer ('Buffer', 2) ;

	ABN := GetHalbwort (VBL + 5) ;

	IF AktSatz = NIL THEN
		NEW (AktSatz) ;
	END ;

	BA := GetDateiBetriebsart (strmnr) ;

	IF (T = 2) AND NOT SetDateiBetriebsart (strmnr, BAschreibend) THEN
		unzulaessigeAnwendung ('Block schreiben') ;
		RETURN ;
	END ;

	IF T = 1 THEN
		PHYSlesen (strmnr, ABN, AAOP, ANZ) ;

		SSRpuffer ('gelesen', 2) ;
	ELSE
		PHYSschreiben (strmnr, ABN, AAOP, ANZ) ;
	END ;

END _SSR_253_23 ;


PROCEDURE _SSR_253_24 ;
	VAR	DBN :		ARRAY [0..7] OF CHAR ;
		datnam :	ARRAY [0..19] OF CHAR ;
		GENVNR :	CARDINAL ;
		M,
		VTDT :		CARDINAL8 ;
BEGIN

	IF NOT SSRaufruf ('Abmelden einer Datei', 3) THEN
		RETURN ;
	END ;
								(* VBL :						    	*)
								(* 00	FA			|			    M	*)
								(* 02     		DBN = Verzeichnisname			*)
								(* 04	AADTB			|	VTDT		LNGDTB	*)

	M := GetHalbwort (VBL + 1) SHR 1 BAND 1 ;
	VTDT := GetHalbwort (VBL + 5) SHR 16 ;
	upDateiName (DBN, datnam, GENVNR) ;
	IF M = 0 THEN
		IF NOT Ausschleusen (DBN, datnam) THEN
			SSRfehler (43H, 'Datei existiert nicht') ;
		END ;
	ELSE
		alleAusschleusen (DBN, VTDT) ;
	END ;
END _SSR_253_24 ;



PROCEDURE _SSR_253_25 ;
BEGIN
	SSRnimpl ('Abspannauftrag') ;
END _SSR_253_25 ;


PROCEDURE _SSR_253_26 ;
	VAR	DBN,
		Dateiname :	ARRAY [0..31] OF CHAR ;
		GENVNR :	CARDINAL ;
BEGIN
	IF NOT SSRaufruf ('Platzreservierung', 4) THEN
		RETURN ;
	END ;

	IF NOT upDateiName (DBN, Dateiname, GENVNR) THEN
		RETURN ;
	END ;
	RegA.inh := 200 ;		(* KB belegt *)
	RegA.TK := 1 ;
END _SSR_253_26 ;



PROCEDURE _SSR_253_27 ;
	VAR	DTBQ,
		DTBZ :		ARRAY [0..15] OF CHAR ;
		DBNQ,
		DBNZ,
		genvnr :	ARRAY [0..7] OF CHAR ;
		GENVNRQ,
		GENVNRZ :	CARDINAL ;
		datnamQ,
		datnamZ :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT SSRaufruf ('Datei gebietsweise kopieren', 5) THEN
		RETURN ;
	END ;

	GetStringAusSpeicher (DBNQ, VBL + 2, 6) ;
	IF NOT upGet_AADTB_LNGDTB (DTBQ, 4, GENVNRQ) THEN
		RETURN ;
	END ;
	GetStringAusSpeicher (DBNZ, VBL + 6, 6) ;
	IF NOT upGet_AADTB_LNGDTB (DTBZ, 8, GENVNRZ) THEN
		RETURN ;
	END ;

	IF NOT checkOEFDB (DBNZ) THEN
		RETURN ;
	END ;

	IF (GetEchtenDateinamen (DBNQ, DTBQ, datnamQ) <> TreeOK)
	OR NOT DateiExistiert (datnamQ, GENVNRQ) THEN
		SSRfehler (43H, 'Quelldatei existiert nicht') ;
		RETURN ;
	END ;
	IF (GetEchtenDateinamen (DBNZ, DTBZ, datnamZ) <> TreeOK)
	OR NOT DateiExistiert (datnamZ, GENVNRZ) THEN
		SSRfehler (43H, 'Zieldatei existiert nicht') ;
		RETURN ;
	END ;
	AppendExtension (GENVNRQ, datnamQ) ;
	AppendExtension (GENVNRZ, datnamZ) ;

	IF debug.Dateien THEN
		TraceF ('copy %s --> %s', datnamQ, datnamZ) ;
	END ;
	IF NOT FileFunc.CopyFile (datnamQ, datnamZ) THEN
		unzulaessigeAnwendung ('CopyFile') ;
		RETURN ;
	END ;

	GENVNRausZahl (GENVNRZ, genvnr) ;
	PutStringInRegister (genvnr, RegH) ;

END _SSR_253_27 ;


PROCEDURE DBNok (name : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i :	CARDINAL ;
BEGIN
	IF LENGTH (name) > 6 THEN		(* Directoryname zu lang für TR440 *)
		RETURN FALSE ;
	END ;
	FOR i := 1 TO LENGTH (name) DO
		IF name [i-1] > '_' THEN
			RETURN FALSE ;		(* Kleinbuchstabe in TR440-Directory gibts nicht *)
		END ;
	END ;
	RETURN CheckDateinamen (name) ;
END DBNok ;


PROCEDURE DATNAMok (name : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i :	CARDINAL ;
BEGIN
	IF LENGTH (name) > 12 THEN		(* Directoryname zu lang für TR440 *)
		RETURN FALSE ;
	END ;
	FOR i := 1 TO LENGTH (name) DO
		IF name [i-1] > '_' THEN
			RETURN FALSE ;		(* Kleinbuchstabe in TR440-Directory gibts nicht *)
		END ;
	END ;
	RETURN CheckDateinamen (name) ;
END DATNAMok ;


PROCEDURE _SSR_253_32 ;
	VAR	Z1,
		Z2,
		IS,
		nr,
		R,
		AAINF :		CARDINAL ;
		zw :		CARDINAL64 ;
		DBNBKZ,
		EXDKZ :		ARRAY [0..7] OF CHAR ;
		str :		ARRAY [0..255] OF CHAR ;
		str2 :		ARRAY [0..15] OF CHAR ;

	PROCEDURE InfoDir ;
		VAR	entry :		FileFunc.SearchEntry ;
	BEGIN
		IF FileFunc.FindFirst (str, FileFunc.AllAttributes, FileFunc.MustHaveDirectory, entry) THEN
			REPEAT
				IF DBNok (entry.name) THEN
					IF Z1 > 0 THEN
						Z1 := Z1 - 1 ;
					ELSE
						INC (nr) ;
						NeueTypenkennung := 3 ;
						PutStringInGanzwortSpeicher (entry.name, AAINF + (nr-1)*2, 6) ;
						IF SpeicherschutzAlarmGewesen THEN
							RETURN ;
						END ;
						IF (Z2 <> 0) AND (Z2 <= nr) THEN
							BREAK ;
						END ;
					END ;
				END ;
			UNTIL NOT FileFunc.FindNext (entry) ;
			FileFunc.FindClose (entry) ;
		END ;
		PutGanzwort (AAINF + nr*2, 0, 0) ;
	END InfoDir ;

	PROCEDURE InfoDat (lng : CARDINAL) ;
		VAR	reladr,
			genvnr,
			anz,
			geslng :		CARDINAL ;
			entry :		FileFunc.SearchEntry ;
			parts :		FileFunc.FileNameParts ;
	BEGIN
		anz := 0 ;
		geslng := 0 ;
		IF FileFunc.FindFirst (str, FileFunc.AllAttributes, FileFunc.MustHaveNormalFile, entry) THEN
			REPEAT
				geslng := geslng + ORD (entry.size) ;
				FileFunc.ParseFileName (entry.name, parts) ;
				IF DATNAMok (parts.name) THEN
					IF parts.extension [0] = '.' THEN
						INC (anz) ;
						genvnr := GENVNRtoZahl (parts.extension [1..HIGH(parts.extension)]) ;
						IF Z1 > 0 THEN
							Z1 := Z1 - 1 ;
						ELSIF (Z2 <> 0) AND (Z2 <= nr) THEN
							INC (R) ;
						ELSE
							INC (nr) ;
							reladr := AAINF + (nr-1)*lng*2 ;
							NeueTypenkennung := 3 ;
							PutStringInGanzwortSpeicher (parts.name, reladr, 12) ;
							IF SpeicherschutzAlarmGewesen THEN
								RETURN ;
							END ;
							IF genvnr <> 0 THEN
								GENVNRausZahl (genvnr, str2) ;
								NeueTypenkennung := 3 ;
								PutStringInGanzwortSpeicher (str2, reladr + 4, 6) ;
								IF SpeicherschutzAlarmGewesen THEN
									RETURN ;
								END ;
								ManipGanzwort (reladr + 4, zt2) ;
								IF lng = 4 THEN
									PutGanzwort (reladr + 6, 2, 0) ;	(* KT = G-Datei *)
								END ;
								IF SpeicherschutzAlarmGewesen THEN
									RETURN ;
								END ;
							END ;
						END ;
					END ;
				END ;
			UNTIL NOT FileFunc.FindNext (entry) ;
			FileFunc.FindClose (entry) ;
		END ;
		IF (IS = 3) AND ExStrings.EqualI (DBNBKZ, AktAuftrag.BKZ) THEN
			PutGanzwort (AAINF + nr*lng*2, 0, 0FFFFFF000000H + VAL (CARDINAL64, (anz SHL 16) + ((geslng + 512) DIV 1024 BAND 0FFFFH))) ;
		ELSE
			PutGanzwort (AAINF + nr*lng*2, 0, 0) ;
		END ;
	END InfoDat ;

	PROCEDURE InfoEingeschleuste (nurLFD, nurWSP : BOOLEAN) ;
		VAR	reladr :	CARDINAL ;
			ein :		pEinschleusDatei ;
	BEGIN
		ein := Einschleus0 ;
		WHILE ein <> NIL DO
			IF Strings.Equal (DBNBKZ, ein ^ .datenbasis) THEN	(* nur die in dieser DBN eingeschleusten *)
				IF  (NOT nurLFD OR (ein ^ .lfd))
				AND (NOT nurWSP OR (NOT ein ^ .lfd)) THEN
					IF Z1 > 0 THEN
						Z1 := Z1 - 1 ;
					ELSIF (Z2 <> 0) AND (Z2 <= nr) THEN
						INC (R) ;
					ELSE
						INC (nr) ;
						reladr := AAINF + (nr-1)*6;
						NeueTypenkennung := 3 ;
						PutStringInGanzwortSpeicher (ein ^ .dateiname, reladr, 12) ;
						IF SpeicherschutzAlarmGewesen THEN
							RETURN ;
						END ;
						GENVNRausZahl (ein ^ .genvnr, str2) ;
						NeueTypenkennung := 3 ;
						PutStringInGanzwortSpeicher (str2, reladr + 4, 6) ;
						IF SpeicherschutzAlarmGewesen THEN
							RETURN ;
						END ;
						IF (ein ^ .genvnr > 0) AND (ein ^ .genvnr < 1000000) THEN	(* GENVNR gültig *)
							ManipGanzwort (reladr + 4, zt2) ;
						END ;
					END ;
				END ;
			END ;
			ein := ein ^ .next ;
		END ;
		PutGanzwort (AAINF + nr*6, 0, 0) ;
	END InfoEingeschleuste ;

BEGIN
	IF NOT SSRaufruf ('Dateibezeichnungen ausliefern', 4) THEN
		RETURN ;
	END ;

	IS := GetHalbwort (VBL + 1) ;
	IF IS <> 2 THEN
		zw := GetGanzwort (VBL + 2) ;
		IF AktTypenkennung = 2 THEN
			DBNBKZ := AktAuftrag.BKZ ;
		ELSE
			GetStringAusSpeicher (DBNBKZ, VBL + 2, 6) ;
		END ;
	END ;
	zw := GetGanzwort (VBL + 4) ;
	IF AktTypenkennung = 3 THEN
		Z1 := zw SHR 40 ;
		Z2 := zw SHR 24 BAND 0FFFFH ;
	ELSE
		Z2 := zw SHR 40 ;
		Z1 := zw SHR 24 BAND 0FFFFH ;
	END ;
	AAINF := zw BAND 0FFFFFFH ;
	IF IS > 16 THEN
		GetStringAusSpeicher (EXDKZ, VBL + 6, 6) ;
	END ;
	RegA.inh := 0 ;

	nr := 0 ;
	R := 0 ;

	CASE IS OF
	0 :					(* alle Dateien in der Datenbasis DBN *)
		IF GetLokalDateinamen (DBNBKZ, '*', str) = TreeOK THEN
			InfoDat (3) ;
		END ;
		InfoEingeschleuste (FALSE, FALSE) ;
	|
	1 :					(* alle LFD-Dateien in dieser Datenbasis *)
		InfoEingeschleuste (TRUE, FALSE) ;
	|
	2 :					(* alle Datenbasen *)
		GetEchtenDatenbasisNamen ('', str) ;
		Strings.Append ('*', str) ;
		InfoDir ;
	|
	3 :					(* alle Dateien von BKZ *)
		IF GetExternDateinamen ('*', DBNBKZ, '', str, TRUE) = TreeOK THEN
			InfoDat (4) ;
		END ;
	|
	4 :					(* alle auftragseigenen BKZs *)
		IF (Z2 = 0) OR (Z2 > 3) THEN
			Z2 := 3 ;		(* maximal 3 möglich *)
		END ;
		IF GetExternDateinamen ('*', '', '', str, TRUE) = TreeOK THEN
			IF Z1 > 0 THEN
				Z1 := Z1 - 1 ;
			END ;
			INC (nr) ;
			NeueTypenkennung := 3 ;
			PutStringInGanzwortSpeicher (AktAuftrag.BKZ, AAINF, 6) ;	(* Standard-BKZ als 1. *)
			AAINF := AAINF + 2 ;
			PutGanzwort (AAINF, 0, 0) ;	(* das ist da einzige auftragseigene *)
			PutGanzwort (AAINF+2, 0, 0) ;
			(* InfoDir ;			(* deshalb kein 'InfoDir' mehr *)*)
		END ;
	|
	17 :					(* alle WSP-Dateien in DBN *)
		InfoEingeschleuste (FALSE, TRUE) ;
	|
	18 :					(* alle WSP-Dateien von BKZ auf EXDKZ *)
		IF GetExternDateinamen ('*', DBNBKZ, EXDKZ, str, FALSE) = TreeOK THEN
			InfoDat (4) ;
		END ;
	ELSE
		SSRnimpl('Modus') ;
	END ;

	RegA.inh := R ;

END _SSR_253_32 ;


PROCEDURE _SSR_253_39 ;
BEGIN
	SSRnimpl ('Datei in Gebiet wandeln') ;
END _SSR_253_39 ;


PROCEDURE _SSR_253_40 ;
BEGIN
	IF SSRaufruf ('Ausgabe einer Datei', 8) THEN

		CASE GetHalbwort (VBL+9) BAND 0FFH OF		(* MOD *)
		 0 :	(* Ausgabeauftrag *)

%IF WEB %THEN
			SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
%ELSE
			IF GetHalbwort (VBL+8) BAND 0FF00H <> 0 THEN		(* TYP = 0 ist Drucker - was anderes gibt's nicht *)
				SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
			ELSE

				RegQ.inh := VAL(CARDINAL64, TeilauftragsNummer) ;
				RegQ.TK := 1 ;
				CASE GetHalbwort (VBL+7) BAND 1FH OF	(* A / AF *)
				0 :	(* Datei im O-Format *)
					DateiDrucken ('O') ;
					INC (TeilauftragsNummer) ;
				|
				1 :	(* Datei im A-Format *)
					DateiDrucken ('A') ;
					INC (TeilauftragsNummer) ;
				|
				2 :	(* Datei im W-Format *)
					DateiDrucken ('W') ;
					INC (TeilauftragsNummer) ;
				ELSE
					SSRnimpl ('Gebiet-Ausgabe') ;
				END ;
			END ;
%END
		 |
		 5 :
			SSRfehler (3EH, 'Gerätetyp nicht vorhanden') ;
		ELSE
			(* Definitionen ignorieren *)
		END ;
	END ;
END _SSR_253_40 ;


END SSR_253.
