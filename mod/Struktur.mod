
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Struktur;

(*	20.06.18	*)

FROM SYSTEM IMPORT
	ADR, CAST, SWAPENDIAN ;

FROM Storage IMPORT
	ALLOCATE ;

FROM Register IMPORT * ;

FROM upFestkomma IMPORT * ;

IMPORT debug ;

IMPORT Terminal, AsmTrace ;

FROM ZC1 IMPORT
	ZC1toANSI,
	ANSItoZC1 ;

FROM FormatString IMPORT
	FormatString ;

FROM AsmDisasm IMPORT
%IF %NOT WEB %THEN
	GetMnemo,
	Get_R_RLR,
%END
	CardToHex4, CardToHex6, CardToHex12 ;

FROM Menue IMPORT
	Grundzustand ;

FROM Trace IMPORT
	TraceF ;

FROM DateiVerwaltung IMPORT
	_STDDB, _OEFDB ;

<*/ValidVersion:optimieren*>
<*/ValidVersion:SeitenKachelTabelle*>
<*/ValidVersion:IndexAssoziativ*>
<*/ValidVersion:Dreierprobe*>

<*/Version:optimieren*>
<*/Version:SeitenKachelTabelle*>
<*/Version:IndexAssoziativ*>

<*/NoVersion:Dreierprobe*>


(* optimieren bedeutet, sich auf Byte-(Wort-Operationen etc. verlassen, um BAND-Befehle einzusparen *)



TYPE
	pGanzwort =	POINTER TO Ganzwort ;


VAR
	initialisiert :	BOOLEAN = FALSE ;


PROCEDURE NullBefehl ;
BEGIN
END NullBefehl ;


PROCEDURE Takte (anz : CARDINAL) ;			(* 16 Takte pro Mikrosekunde *)
BEGIN
	INC (GesamtZahlTakte, anz) ;
	INC (InsgesamtZahlTakte, anz) ;
	IF AbschnittsModus THEN
		IF InsgesamtZahlTakte > MaximalZahlTakte THEN	(* RZS - Überlauf *)
			RZSueberlaufGewesen := TRUE ;
			AlarmGewesen := TRUE ;
			INC (MaximalZahlTakte, 1000000) ;	(* Reserve, damit nur 1-mal Alarm gemeldet wird *)
		END ;
	END ;
END Takte ;


PROCEDURE InitAlarm ;
BEGIN
	AlarmGewesen := TRUE ;
	TKalarmGewesen := FALSE ;
	BUEalarmGewesen := FALSE ;
	SpeicherschutzAlarmGewesen := FALSE ;
	BefehlsAlarmGewesen := FALSE ;
	UalarmGewesen := FALSE ;
	RZSueberlaufGewesen := FALSE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		SaveRegister (reg) ;
		BefAlarm := FALSE ;
		STB1 := 2 ;				(* Normalmodus *)
		BC := AktBefehlscode ;
	END ;
	RegA.inh := 1 ;
	RegA.TK := 1 ;
	RegQ.inh := 0 ;
	RegQ.TK := 2 ;
END InitAlarm ;


PROCEDURE SpeicherschutzAlarm (art : ARRAY OF CHAR ; reladr : SpeicherAdresse) ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	InitAlarm ;
	SpeicherschutzAlarmGewesen := TRUE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		STB1 := STB1 BOR 200H ;
	END ;
	FormatString ("+++++ Speicherschutzalarm %s %'04h", str, art, reladr) ;
	Terminal.WriteLn ;							AsmTrace.WriteLn ;
	Terminal.WriteString (str) ;						AsmTrace.WriteString (str) ;
	IF debug.an THEN
		TraceF ('%s', str) ;
	END ;
END SpeicherschutzAlarm ;


PROCEDURE TypenkennungsAlarm2 ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	InitAlarm ;
	TKalarmGewesen := TRUE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		STB1 := STB1 BOR 800H ;
	END ;
	TKalarmErkannt := FALSE ;
	FormatString ("+++++ TK-Alarm %'04h", str, MerkReladr) ;
	Terminal.WriteLn ;							AsmTrace.WriteLn ;
	Terminal.WriteString (str) ;						AsmTrace.WriteString (str) ;
	IF debug.an THEN
		TraceF ('%s', str) ;
	END ;
END TypenkennungsAlarm2 ;


PROCEDURE TypenkennungsAlarm (reladr : SpeicherAdresse) ;
BEGIN
	TKalarmErkannt := TRUE ;
	MerkReladr := reladr ;
	IF debug.Befehle THEN
		TraceF ("* TK-Alarm %'04h", reladr) ;
	END ;
END TypenkennungsAlarm ;


PROCEDURE BefehlsAlarm ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	InitAlarm ;
	Operatoren [AktOperator].AlarmInformation.BefAlarm := TRUE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		STB1 := STB1 BOR 80H ;
		BefAlarm := MakroAlarm ;
	END ;
	MakroAlarm := FALSE ;
	BefehlsAlarmGewesen := TRUE ;
	FormatString ("+++++ Befehlsalarm %'04h %'02h.%'04h", str, RegF, ORD (AktBefehlscode), AktAdressteil) ;
		Terminal.WriteLn ;						AsmTrace.WriteLn ;
	Terminal.WriteString (str) ;						AsmTrace.WriteString (str) ;
	IF debug.an THEN
		TraceF ('%s', str) ;
	END ;
END BefehlsAlarm ;


PROCEDURE ArithmetischerAlarm2 ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	InitAlarm ;
	BUEalarmGewesen := TRUE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		STB1 := STB1 BOR 400H ;
	END ;
	BUEalarmErkannt := FALSE ;
	FormatString ("+++++ Arithmetischer Alarm %'04h  %'02h.%'04h", str, MerkRegF, ORD (MerkBefehlscode), MerkAdressteil) ;
	Terminal.WriteLn ;							AsmTrace.WriteLn ;
	Terminal.WriteString (str) ;						AsmTrace.WriteString (str) ;
	IF debug.an THEN
		TraceF ('%s', str) ;
	END ;
END ArithmetischerAlarm2 ;


PROCEDURE ArithmetischerAlarm ;
BEGIN
	BUEalarmErkannt := TRUE ;
	MerkRegF := RegF ;
	MerkBefehlscode := AktBefehlscode ;
	MerkAdressteil := AktAdressteil ;
	IF debug.Befehle THEN
		TraceF ("* BÜ-Alarm %'04h  %'02h.%'04h", RegF, AktBefehlscode, AktAdressteil) ;
	END ;
END ArithmetischerAlarm ;


PROCEDURE U_Alarm ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	InitAlarm ;
	UalarmGewesen := TRUE ;
	WITH Operatoren [AktOperator].AlarmInformation DO
		STB1 := STB1 BOR 100H ;
	END ;
	FormatString ("+++++ U-Alarm %'04h %'02h.%'04h", str, RegF, ORD (AktBefehlscode), AktAdressteil) ;
	Terminal.WriteLn ;							AsmTrace.WriteLn ;
	Terminal.WriteString (str) ;						AsmTrace.WriteString (str) ;
	IF debug.an THEN
		TraceF ('%s', str) ;
	END ;
END U_Alarm ;


PROCEDURE XbasisPruefen ;
BEGIN
	IF NOT AlarmGewesen THEN
		IF NOT TestAdresse (RegX) OR AktSchreibschutz OR NOT TestAdresse (RegX + 255) OR AktSchreibschutz THEN
			SpeicherschutzAlarm ('bei Indexbasis', RegX) ;
		END ;
	END ;
END XbasisPruefen ;


PROCEDURE RegFbesetzen (neuerWert : CARDINAL) ;
BEGIN
	Takte (5) ;
	IF GrossSeitenInvarianz THEN			(* z.B. SE, SUE, SFBE o.ä. oder MABI oder MU / MUE vorher *)
		RegF := neuerWert BAND 03FFFFFH ;
	ELSE
		RegF := (RegF BAND 03F0000H) BOR (neuerWert BAND 0FFFFH) ;	(* sonst kein Großseitenwechsel *)
	END ;
	FolgeAdresse := RegF ;
	IF MUmodifizierung THEN
		IF ORD(RegU) = 255 THEN
			U_Alarm ;
		END ;
		RegU := trSub24 (RegU, 1) ;
	END ;
END RegFbesetzen ;


PROCEDURE mod2Check ;
BEGIN
	IF mod2 <> 0 THEN
		Takte (8) ;
		AktAdressteil := trAdd24 (AktAdressteil, mod2) ;
		mod2 := 0 ;
	END ;
END mod2Check ;


PROCEDURE mod2Ohne ;
BEGIN
	mod2 := 0 ;
END mod2Ohne ;


PROCEDURE CheckUeberlauf (reg : reg48) ;		(* beim Abspeichern in den Speicher *)
BEGIN
	IF ORD(reg.TK) < 2 THEN
		IF (reg.inh BAND MarkenBit) <> 0 THEN		(* negativ *)
			IF (reg.inh BAND VorzeichenBit = 0) THEN		(* negativ untergelaufen *)
				ArithmetischerAlarm ;
			END ;
			AktWertFuerSpeicher := reg.inh BAND MarkenBitWeg ;
		ELSE
			IF (reg.inh BAND VorzeichenBit) <> 0 THEN		(* positiv übergelaufen *)
				ArithmetischerAlarm ;
			END ;
			AktWertFuerSpeicher := reg.inh BAND MarkenBitWeg ;
		END ;
	ELSE
		AktWertFuerSpeicher := reg.inh ;
	END ;
END CheckUeberlauf ;


PROCEDURE CheckMarke (VAR reg : reg48) ;		(* beim Laden aus dem Speicher *)
BEGIN
	IF reg.hi > 3FFFH THEN				(* Markenbit oder Vorzeichenbit gesetzt *)
		IF reg.TK = 1 THEN
			IF reg.hi > 7FFFH THEN					(* Markenbit gesetzt : Markenregister setzen *)
				RegM := TRUE ;
				IF (reg.hi BAND 4000H) = 0 THEN			(* markierte positive Zahl : Markenbit löschen *)
					reg.hi := reg.hi BAND 3FFFH ;
				ELSE
										(* markierte negative Zahl *)
				END ;
			ELSE							(* unmarkierte negative Zahl : Markenbit angleichen *)
				reg.hi := reg.hi BOR 0C000H ;
			END ;
		ELSIF reg.TK = 0 THEN
			IF reg.hi > 7FFFH THEN					(* Markenbit gesetzt : Markenregister setzen *)
				RegM := TRUE ;
				IF (reg.hi BAND 4000H) = 0 THEN			(* markierte positive Zahl : Markenbit löschen *)
					reg.hi := reg.hi BAND 3FFFH ;
				ELSE
										(* markierte negative Zahl *)
				END ;
			ELSE							(* unmarkierte negative Zahl : Markenbit angleichen *)
				reg.hi := reg.hi BOR 0C000H ;
			END ;
		END ;
	END ;
END CheckMarke ;


%IF SeitenKachelTabelle %THEN


PROCEDURE TestAdresse (reladr : SpeicherAdresse) : BOOLEAN ;
	VAR	snr :			CARDINAL ;
BEGIN
	reladr := reladr BAND Cardinal24Bit ;
	IF reladr > AktMaxGanzwortNummer * 2 + 1 THEN
		RETURN FALSE ;
	END ;
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		IF zugeteilt THEN
			AktSchreibschutz := NOT beschreibbar ;
		ELSE
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END TestAdresse ;


PROCEDURE GetSeitenAdresse (reladr : SpeicherAdresse) : pSeite ;	(* Pointer auf Seite oder NIL *)
	VAR	snr :			CARDINAL ;
BEGIN
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		IF zugeteilt THEN
			AktSchreibschutz := NOT beschreibbar ;
			RETURN Hauptspeicher [kachel] ;
		END ;
	END ;
	RETURN NIL ;
END GetSeitenAdresse ;


PROCEDURE GetGanzwortAdresse (reladr : SpeicherAdresse) : pGanzwort ;
	VAR	GW :			pGanzwort ;
		snr,
		rel :			CARDINAL ;
BEGIN
	reladr := reladr BAND Cardinal24Bit ;
	IF reladr > MaxGanzwortNummer * 2 + 1 THEN
		RETURN NIL ;
	END ;

	snr := reladr SHR 11 ;					(* Seitennummer *)
	rel := (reladr BAND MaxHalbwortInSeite) SHR 1 ;		(* Relativ-Ganzwortnummer in Seite *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		IF zugeteilt THEN
			AktSchreibschutz := SchreibschutzAktiv AND NOT beschreibbar ;
			GW := ADR (Hauptspeicher [kachel] ^ [rel]) ;
			%IF Dreierprobe %THEN
				AktDreierprobe := GW ^ .DP ;
			%END
			AktTypenkennung := GW ^ .TK ;
			RETURN GW ;
		END ;
	END ;
	RETURN NIL ;
END GetGanzwortAdresse ;


PROCEDURE SetProtect (adresse : SpeicherAdresse; protected : BOOLEAN) ;	(* Schreibschutz für die zugehörige Seite ja/nein *)
	VAR	snr :		CARDINAL ;
BEGIN
	snr := adresse SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		beschreibbar := NOT protected ;
	END ;
END SetProtect ;


PROCEDURE SetExist (adresse : SpeicherAdresse; existent : BOOLEAN) ;	(* Existenz für die zugehörige Seite ja/nein *)
	VAR	snr :			CARDINAL ;
BEGIN
	snr := adresse SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		zugeteilt := existent ;
	END ;
END SetExist ;


PROCEDURE SpeicherZuordnung (reladr : SpeicherAdresse ; echteAdresse : pSeite ; Schreibschutz : BOOLEAN) : BOOLEAN ;
	VAR	snr,
		k :		CARDINAL ;
BEGIN
	IF reladr > MaxGanzwortNummer * 2 THEN
		RETURN FALSE ;
	END ;
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	IF reladr > AktMaxGanzwortNummer * 2 + 1 THEN
		AktMaxGanzwortNummer := ((snr + 1) SHL 10) - 1 ;
	END ;
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		kachel := KachelBelegen () ;
		k := kachel ;
		beschreibbar := NOT Schreibschutz ;
		zugeteilt := TRUE ;
	END ;
	AktSchreibschutz := Schreibschutz ;
	IF echteAdresse = NIL THEN			(* Standard-KSP-Seite zuordnen *)
		Hauptspeicher [k] := KSP [k] ;
		IF debug.MemoryProtokoll THEN
			TraceF ("%'05h:KSP[%'03h] +", reladr, k) ;
		END ;
	ELSE
		Hauptspeicher [k] := echteAdresse ;
		IF debug.MemoryProtokoll THEN
			TraceF ("%'05h:KSP[%'03h]:%'08h +", reladr, k, echteAdresse) ;
		END ;
	END ;
	RETURN TRUE ;
END SpeicherZuordnung ;


PROCEDURE SpeicherZuordnungWeg (reladr : SpeicherAdresse) : BOOLEAN ;
	VAR	snr,
		k :		CARDINAL ;
BEGIN
	IF reladr > MaxGanzwortNummer * 2 THEN
		RETURN FALSE ;
	END ;
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	WITH SeitenKachelTabelle [snr SHR 5, snr BAND MaxSeiteInGrossSeite] DO
		k := kachel ;
		IF NOT zugeteilt THEN
			TraceF ("nicht frei %'05h[%'03h]", reladr, k) ;
			RETURN FALSE ;			(* war gar nicht zugeordnet *)
		END ;
		zugeteilt := FALSE ;			(* aber aus Zugriff ausblenden *)
		AktSchreibschutz := TRUE ;
	END ;
	KachelFreigeben (k) ;
	IF Hauptspeicher [k] <> KSP [k] THEN		(* da war ein Gebiet eingemapped *)
		IF debug.MemoryProtokoll THEN
			TraceF ("%'05h:KSP[%'03h]:%'08h -", reladr, k, Hauptspeicher [k]) ;
		END ;
(*
		IF Hauptspeicher [k] <> NIL THEN
			DISPOSE (Hauptspeicher [k]) ;	(* geht nicht, weil nur 1 Stück eines Gebietes *)
		END ;
*)
		Hauptspeicher [k] := KSP [k] ;		(* Standard-KSP-Seite zuordnen *)
	ELSE
		IF debug.MemoryProtokoll THEN
			TraceF ("%'05h:KSP[%'03h] -", reladr, k) ;
		END ;
	END ;
	IF debug.MemoryProtokoll THEN
		TraceF ("KSP[%'05h] -", reladr) ;
	END ;
	RETURN TRUE ;
END SpeicherZuordnungWeg ;


%ELSE


PROCEDURE TestAdresse (reladr : SpeicherAdresse) : BOOLEAN ;
	VAR	snr :			CARDINAL ;
BEGIN
	IF reladr > AktMaxGanzwortNummer * 2 - 1 THEN
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END TestAdresse ;

PROCEDURE GetGanzwortAdresse (reladr : SpeicherAdresse) : pGanzwort [INLINE ] ;
	VAR	GW :			pGanzwort ;
		snr,
		rel :			CARDINAL ;
BEGIN
	IF reladr > AktMaxGanzwortNummer * 2 THEN
		RETURN NIL ;
	END ;
	snr := reladr SHR 11 ;					(* Seitennummer *)
	rel := (reladr BAND MaxHalbwortInSeite) SHR 1 ;		(* Relativ-Ganzwortnummer in Seite *)
	GW := ADR (Hauptspeicher [snr] ^ [rel]) ;
	AktTypenkennung := GW ^ .TK ;
	RETURN GW ;
END GetGanzwortAdresse ;

PROCEDURE GetSeitenAdresse (reladr : SpeicherAdresse) : pSeite ;	(* Pointer auf Seite oder NIL *)
	VAR	GW :			pGanzwort ;
		snr,
		rel :			CARDINAL ;
BEGIN
	IF reladr > AktMaxGanzwortNummer * 2 THEN
		RETURN NIL ;
	END ;
	snr := reladr SHR 11 ;					(* Seitennummer *)
	RETURN Hauptspeicher [snr] ;
END GetSeitenAdresse ;

PROCEDURE SetProtect (adresse : SpeicherAdresse; protected : BOOLEAN) ;	(* Schreibschutz für die zugehörige Seite ja/nein *)
BEGIN
END SetProtect ;


PROCEDURE SetExist (adresse : SpeicherAdresse; existent : BOOLEAN) ;	(* Existenz für die zugehörige Seite ja/nein *)
BEGIN
END SetExist ;


PROCEDURE SpeicherZuordnung (reladr : SpeicherAdresse ; echteAdresse : pSeite ; Schreibschutz : BOOLEAN) : BOOLEAN ;
	VAR	snr :		CARDINAL ;
BEGIN
	IF reladr > MaxGanzwortNummer * 2 THEN
		RETURN FALSE ;
	END ;
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	IF reladr > AktMaxGanzwortNummer * 2 + 1 THEN
		AktMaxGanzwortNummer := ((snr + 1) SHL 10) - 1 ;
	END ;
	IF echteAdresse = NIL THEN			(* Standard-KSP-Seite zuordnen *)
		Hauptspeicher [snr] := KSP [snr] ;
	ELSE
		Hauptspeicher [snr] := echteAdresse ;
	END ;
	RETURN TRUE ;
END SpeicherZuordnung ;


PROCEDURE SpeicherZuordnungWeg (reladr : SpeicherAdresse) : BOOLEAN ;
	VAR	snr :		CARDINAL ;
BEGIN
	IF reladr > MaxGanzwortNummer * 2 THEN
		RETURN FALSE ;
	END ;
	snr := reladr SHR 11 BAND MaxSeitenNummer ;	(* Seitennummer *)
	Hauptspeicher [snr] := KSP [snr] ;		(* Standard-KSP-Seite zuordnen *)
	RETURN TRUE ;
END SpeicherZuordnungWeg ;


%END




PROCEDURE LiesHalbwort (hw : Halbwort) : CARDINAL [INLINE] ;
	VAR zw [ALIAS hw] :	CARDINAL ;
BEGIN
	RETURN SWAPENDIAN (zw) SHR 8 ;
(* Alternative :
	RETURN (zw SHL 16) BAND 0FF0000H BOR (zw BAND 0FF00H) BOR ((zw SHR 16) BAND 0FFH) ;
*)
(* Alternative :
	RETURN (ORD(hw [0]) SHL 8 BOR ORD(hw [1])) SHL 8 BOR ORD(hw [2]) ;
*)
END LiesHalbwort ;


PROCEDURE extLiesHalbwort (hw : Halbwort) : CARDINAL ;
BEGIN
	RETURN LiesHalbwort (hw) ;
END extLiesHalbwort ;


PROCEDURE upZuwHalbwort (VAR hw : Halbwort ; wert : CARDINAL) [INLINE]  ;
BEGIN
	hw [0] := wert SHR 16 %IF %NOT optimieren %THEN BAND 0FFH %END ;
	hw [1] := wert SHR 8 %IF %NOT optimieren %THEN BAND 0FFH %END ;
	hw [2] := wert %IF %NOT optimieren %THEN BAND 0FFH %END ;
END upZuwHalbwort ;


PROCEDURE ZuwHalbwort (VAR hw : Halbwort ; wert : CARDINAL) ;
BEGIN
	upZuwHalbwort (hw, wert) ;
END ZuwHalbwort ;


PROCEDURE LiesGanzwort (wert : Wort6Bytes) : CARDINAL64 [INLINE] ;
	VAR	w [ALIAS wert] :	CARDINAL64 ;
BEGIN
	RETURN SWAPENDIAN (w) SHR 16 ;
(* Alternative :

	RETURN ((w SHR 40) BAND 0FFH) BOR ((w SHR 24) BAND 0FF00H) BOR ((w SHR 8) BAND 0FF0000H)
	   BOR ((w SHL 8) BAND 0FF000000H) BOR ((w SHL 24) BAND 0FF00000000H) BOR ((w SHL 40) BAND 0FF0000000000H) ;
*)
END LiesGanzwort ;


PROCEDURE extLiesGanzwort (wert : Wort6Bytes) : CARDINAL64 ;
BEGIN
	RETURN LiesGanzwort (wert) ;
END extLiesGanzwort ;


PROCEDURE RegAufbereiten (VAR reg : reg48) : CARDINAL64 [INLINE] ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	AktTypenkennung := reg.TK ;
	wert := reg.inh ;
(*
	IF AktTypenkennung < 2 THEN
		(* Marke oder Vorzeichen verändern ? *)
	END ;
*)
	RETURN wert ;
END RegAufbereiten ;


PROCEDURE GetRegisterWertGW (adrteil : CARDINAL8) : CARDINAL64 ;
	VAR	wert :		CARDINAL64 ;
BEGIN
	R_Befehl := FALSE ;
	CASE (adrteil SHR 3) BAND 1FH OF
	0,
	1 :	IF AktBefehlscode <> 0 THEN
			BefehlsAlarm ;
		END ;
	|
	2 :	wert := RegAufbereiten (RegH) ;
	|
	4 :	wert := RegAufbereiten (RegD) ;
	|
	8 :	wert := RegAufbereiten (RegQ) ;
	|
	16 :	wert := RegAufbereiten (RegA) ;
	|
	3 :	wert := ORD (RegY) ;
		AktTypenkennung := 1 ;
	|
	5 :	wert := tr24to48 (RegB) ;
		AktTypenkennung := 1 ;
	|
	9 :	wert := ORD (RegU) ;
		AktTypenkennung := 1 ;
	|
	17 :	wert := tr24to48 (RegF) + 1 ;
		AktTypenkennung := 1 ;
	ELSE
		BefehlsAlarm ;
	END ;
	RETURN wert ;
END GetRegisterWertGW ;


PROCEDURE GetRegisterWertHW (adrteil : CARDINAL8) : CARDINAL ;
	VAR	wert :		CARDINAL ;
		rechts :	BOOLEAN ;
BEGIN
	R_Befehl := FALSE ;
	rechts := ODD(adrteil) ;
	CASE (adrteil SHR 3) BAND 1FH OF
	0,
	1 :	IF AktBefehlscode <> 0 THEN
			BefehlsAlarm ;
		END ;
	|
	2 :	AktTypenkennung := RegH.TK ;
		IF rechts THEN
			wert := RegH.lo BAND Cardinal24Bit ;
		ELSE
			wert := (RegH.inh SHR 24) BAND Cardinal24Bit ;
		END ;
	|
	4 :	AktTypenkennung := RegD.TK ;
		IF rechts THEN
			wert := RegD.lo BAND Cardinal24Bit ;
		ELSE
			wert := (RegD.inh SHR 24) BAND Cardinal24Bit ;
		END ;
	|
	8 :	AktTypenkennung := RegQ.TK ;
		IF rechts THEN
			wert := RegQ.lo BAND Cardinal24Bit ;
		ELSE
			wert := (RegQ.inh SHR 24) BAND Cardinal24Bit ;
		END ;
	|
	16 :	AktTypenkennung := RegA.TK ;
		IF rechts THEN
			wert := RegA.lo BAND Cardinal24Bit ;
		ELSE
			wert := (RegA.inh SHR 24) BAND Cardinal24Bit ;
		END ;
	|
	3 :	wert := ORD (RegY) ;
		AktTypenkennung := 1 ;
	|
	5 :	wert := RegB BAND Cardinal24Bit ;
		AktTypenkennung := 1 ;
	|
	9 :	wert := ORD (RegU) ;
		AktTypenkennung := 1 ;
	|
	17 :	wert := RegF + 1 ;
		AktTypenkennung := 1 ;
	ELSE
		BefehlsAlarm ;
	END ;
	RETURN wert ;
END GetRegisterWertHW ;


PROCEDURE GetGanzwort (reladr : SpeicherAdresse) : CARDINAL64 ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL64 ;
		str,
		zus :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF R_Befehl THEN
		inh := GetRegisterWertGW (reladr) ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			Get_R_RLR (reladr, zus) ;
			CardToHex12 (inh, str) ;
			TraceF ("<- [R.%s] = %h %s", zus, ORD(AktTypenkennung), str) ;
		END ;
%END
	ELSE
		Takte (8) ;
		GW := GetGanzwortAdresse (reladr) ;

		IF GW = NIL THEN
			SpeicherschutzAlarm ('lesend Ganzwort', reladr) ;
			RETURN 0C7FEFEC7BEECH ;
		END ;

		inh := LiesGanzwort (GW ^ .byte6) ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			CardToHex12 (inh, str) ;
			TraceF ("<- G[%'04h] = %h %s", reladr, ORD(AktTypenkennung), str) ;
		END ;
%END
	END ;
	RETURN inh ;
END GetGanzwort ;


PROCEDURE GetHalbwort (reladr : SpeicherAdresse) : CARDINAL ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL ;
		str,
		zus :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF R_Befehl THEN
		inh := GetRegisterWertHW (reladr) ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			Get_R_RLR (reladr, zus) ;
			TraceF ("<- 1/2[R.%s] = %h %'06h", zus, ORD(AktTypenkennung), inh) ;	(* 'L' ist evtl. schon drin *)
		END ;
%END
	ELSE
		Takte (8) ;
		GW := GetGanzwortAdresse (reladr) ;
		IF GW = NIL THEN
			SpeicherschutzAlarm ('lesend Halbwort', reladr) ;
			RETURN  0C7BEECH ;
		END ;
		IF ODD (reladr) THEN
			inh := LiesHalbwort (GW ^ .lo) ;
		ELSE
			inh := LiesHalbwort (GW ^ .hi) ;
		END ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			CardToHex6 (inh, str) ;
			TraceF ("<- H[%'04h] = %h %s", reladr, ORD(AktTypenkennung), str) ;
		END ;
%END
	END ;
	RETURN inh ;
END  GetHalbwort ;


%IF IndexAssoziativ %THEN

TYPE
	tAssocIndex =	RECORD
				wert,
				counter :	CARDINAL ;
				indexnummer :	CARDINAL8 ;
				definiert,
				veraendert :	BOOLEAN ;
				reserve :	CARDINAL8 ;
			END ;

VAR
	AssocCounter :	CARDINAL = 0 ;
	AssocIndex :	ARRAY [1..4] OF tAssocIndex ;




PROCEDURE BackAssocIndex (i : CARDINAL) ;
	VAR	merk :	BOOLEAN ;
BEGIN
	WITH AssocIndex [i] DO
		IF definiert AND veraendert THEN
			veraendert := FALSE ;
			IF debug.IndexWert THEN
				merk := debug.Speichern ;
				debug.Speichern := FALSE ;
				PutHalbwort (ORD(RegX) + ORD(indexnummer), wert) ;
				debug.Speichern := merk ;
				IF debug.AssoziativProtokoll THEN
					TraceF ("KSP[%'06h] <- X%'02h = %'06h", ORD(RegX) + ORD(indexnummer), ORD(indexnummer), wert) ;
				END ;
			ELSE
				PutHalbwort (ORD(RegX) + ORD(indexnummer), wert) ;
			END ;
			Takte (9) ;
		END ;
	END ;
END BackAssocIndex ;


PROCEDURE InitAssocIndex ;
BEGIN
(*
	FOR i := 1 TO 4 DO
		AssocIndex [i].definiert := FALSE ;
	END ;
*)
	AssocIndex [1].definiert := FALSE ;
	AssocIndex [2].definiert := FALSE ;
	AssocIndex [3].definiert := FALSE ;
	AssocIndex [4].definiert := FALSE ;
END InitAssocIndex ;


PROCEDURE ClearAssocIndex ;
BEGIN
(*
	FOR i := 1 TO 4 DO
		BackAssocIndex (i) ;
		WITH AssocIndex [i] DO
			definiert := FALSE ;
		END ;
	END ;
*)
	BackAssocIndex (1) ;
	AssocIndex [1].definiert := FALSE ;
	BackAssocIndex (2) ;
	AssocIndex [2].definiert := FALSE ;
	BackAssocIndex (3) ;
	AssocIndex [3].definiert := FALSE ;
	BackAssocIndex (4) ;
	AssocIndex [4].definiert := FALSE ;
END ClearAssocIndex ;


PROCEDURE ClearAssocCounter ;
BEGIN
(*
	FOR i := 1 TO 4 DO
		AssocIndex [i].counter := AssocCounter ;
	END ;
*)
	AssocIndex [1].counter := AssocCounter ;
	AssocIndex [2].counter := AssocCounter ;
	AssocIndex [3].counter := AssocCounter ;
	AssocIndex [4].counter := AssocCounter ;
END ClearAssocCounter ;


PROCEDURE SetNeuestenAssocIndex (i : CARDINAL) ;
BEGIN
	INC (AssocCounter) ;
	IF AssocCounter = 0 THEN
		ClearAssocCounter ;	(* Überlauf bereinigen *)
	END ;
	AssocIndex [i].counter := AssocCounter ;
END SetNeuestenAssocIndex ;



PROCEDURE GetAeltestenAssocIndex () : CARDINAL ;
	VAR	i,
		count : CARDINAL ;
BEGIN
	WITH AssocIndex [1] DO
		IF NOT definiert THEN	(* unbelegter per Definition ältester *)
			RETURN 1 ;
		END ;
		count := counter ;
		i := 1 ;
	END ;
	WITH AssocIndex [2] DO
		IF NOT definiert THEN	(* unbelegter per Definition ältester *)
			RETURN 2 ;
		END ;
		IF counter < count THEN
			i := 2 ;
			count := counter ;
		END ;
	END ;
	WITH AssocIndex [3] DO
		IF NOT definiert THEN	(* unbelegter per Definition ältester *)
			RETURN 3 ;
		END ;
		IF counter < count THEN
			i := 3 ;
			count := counter ;
		END ;
	END ;
	WITH AssocIndex [4] DO
		IF NOT definiert THEN	(* unbelegter per Definition ältester *)
			RETURN 4 ;
		END ;
		IF counter < count THEN
			RETURN 4 ;
		END ;
	END ;

(*
	i := 1 ;
	count := AssocIndex [1].counter ;
	FOR j := 2 TO 4 DO
		WITH AssocIndex [j] DO
			IF NOT definiert THEN	(* unbelegter per Definition ältester *)
				RETURN j ;
			END ;
			IF counter < count THEN
				i := j ;
				count := counter ;
			END ;
		END ;
	END ;
*)

	RETURN i ;
END GetAeltestenAssocIndex ;


PROCEDURE GetFreeIndexAssoziativ () : CARDINAL ;
	VAR	i :	CARDINAL ;
BEGIN
	i := GetAeltestenAssocIndex () ;
	WITH AssocIndex [i] DO
		IF definiert THEN
			BackAssocIndex (i) ;
		ELSE
			definiert := TRUE ;
		END ;
		SetNeuestenAssocIndex (i) ;
		RETURN i ;
	END ;
END GetFreeIndexAssoziativ ;


PROCEDURE GetAktIndexAssoc (index : CARDINAL8) : (* index : *) CARDINAL ;
	VAR	i :	CARDINAL ;
BEGIN
	WITH AssocIndex [1] DO
		IF definiert AND (indexnummer = index) THEN
			SetNeuestenAssocIndex (1) ;
			RETURN 1 ;
		END ;
	END ;
	WITH AssocIndex [2] DO
		IF definiert AND (indexnummer = index) THEN
			SetNeuestenAssocIndex (2) ;
			RETURN 2 ;
		END ;
	END ;
	WITH AssocIndex [3] DO
		IF definiert AND (indexnummer = index) THEN
			SetNeuestenAssocIndex (3) ;
			RETURN 3 ;
		END ;
	END ;
	WITH AssocIndex [4] DO
		IF definiert AND (indexnummer = index) THEN
			SetNeuestenAssocIndex (4) ;
			RETURN 4 ;
		END ;
	END ;
(*
	FOR i := 1 TO 4 DO
		WITH AssocIndex [i] DO
			IF definiert AND (indexnummer = index) THEN
				SetNeuestenAssocIndex (i) ;
				RETURN i ;
			END ;
		END ;
	END ;
*)
	RETURN 0 ;
END GetAktIndexAssoc ;


PROCEDURE GetIndexWertAusAssoziativ (index : CARDINAL8) : CARDINAL ;
	VAR	inh :	CARDINAL ;
		i :	CARDINAL ;
		merk :	BOOLEAN ;
BEGIN
	i := GetAktIndexAssoc (index) ;
	IF i = 0 THEN		(* noch nicht im Assoziativspeicher *)
		i := GetFreeIndexAssoziativ() ;
		IF debug.IndexWert THEN
			merk := debug.Speichern ;
			debug.Lesen := FALSE ;
			inh := GetHalbwort (ORD(RegX) + ORD(index)) ;
			debug.Lesen := merk ;
			IF debug.AssoziativProtokoll THEN
				TraceF ("<- X%'02h <- [%'06h] = %'06H", ORD(index), ORD(RegX) + ORD(index), inh) ;
			ELSE
				TraceF ("<- X%'02h = %'06H", ORD(index), inh) ;
			END ;
		ELSE
			inh := GetHalbwort (ORD(RegX) + ORD(index)) ;
		END ;
		WITH AssocIndex [i] DO
			veraendert := FALSE ;
			indexnummer := index ;
			wert := inh ;
		END ;
	ELSE
		WITH AssocIndex [i] DO
			inh := wert ;
		END ;
		IF debug.IndexWert THEN
			IF debug.AssoziativProtokoll THEN
				TraceF ("<- Ass[X%'02h] = %'06H", ORD(index), inh) ;
			ELSE
				TraceF ("<- X%'02h = %'06H", ORD(index), inh) ;
			END ;
		END ;
	END ;
	RETURN inh ;
END GetIndexWertAusAssoziativ ;


PROCEDURE PutIndexWertInAssoziativ (index : CARDINAL8 ; inh : CARDINAL) ;
	VAR	i :	CARDINAL ;
		merk :	BOOLEAN ;
BEGIN
	i := GetAktIndexAssoc (index) ;
	IF i = 0 THEN		(* noch nicht im Assoziativspeicher *)
		i := GetFreeIndexAssoziativ() ;
		WITH AssocIndex [i] DO
			wert := inh ;
			veraendert := TRUE ;
			indexnummer := index ;
		END ;
	ELSE
		WITH AssocIndex [i] DO
			wert := inh ;
			veraendert := TRUE ;
		END ;
	END ;
	IF debug.IndexWert THEN
		TraceF ("X%'02h <- %'06H", ORD(index), inh) ;
	END ;
END PutIndexWertInAssoziativ ;


%ELSE


PROCEDURE ClearAssocIndex ;	(* Dummy wenn ohne Assoziativ-Register *)
BEGIN
END ClearAssocIndex ;


PROCEDURE InitAssocIndex ;	(* Dummy wenn ohne Assoziativ-Register *)
BEGIN
END InitAssocIndex ;

%END


PROCEDURE SuchOperator (OLK : CARDINAL) : OperatorNummer ;
	VAR	i :	CARDINAL ;
BEGIN
	IF OLK = 0 THEN
		RETURN AktOperator ;
	END ;
	FOR i := 1 TO MAX(OperatorNummer) DO
		IF Operatoren [i].OLK = OLK THEN
			RETURN i ;
		END ;
	END ;
	RETURN 0 ;
END SuchOperator ;


PROCEDURE ManipGanzwort (reladr : SpeicherAdresse ; p : GanzwortProc) ;
	VAR	GW :	pGanzwort ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('bearbeitend Ganzwort', reladr) ;
		RETURN ;
	END ;
	p (GW ^ ) ;
END ManipGanzwort ;


PROCEDURE GetGanzwortU (reladr : SpeicherAdresse ; VAR ziel : Ganzwort) ;
	VAR	GW :	pGanzwort ;
		str :	ARRAY [0..31] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('GetU Ganzwort', reladr) ;
		RETURN ;
	END ;
	ziel := GW ^ ;
	IF debug.Lesen THEN
		CardToHex12 (LiesGanzwort (ziel.byte6), str) ;
		TraceF ("<- GWU[%'05h] = %c %s", reladr, ziel.TK, str) ;
	END ;
END GetGanzwortU ;


PROCEDURE PutGanzwortU (reladr : SpeicherAdresse ; VAR quelle : Ganzwort) ;
	VAR	GW :	pGanzwort ;
		str :	ARRAY [0..31] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('PutU Ganzwort', reladr) ;
		RETURN ;
	END ;
	GW ^ := quelle ;
	IF debug.Speichern THEN
		CardToHex12 (LiesGanzwort (quelle.byte6), str) ;
		TraceF ("%c %s -> GWU[%'05h]", ORD (quelle.TK), str, reladr) ;
	END ;
END PutGanzwortU ;


PROCEDURE GetSechstelwort (reladr : SpeicherAdresse) : CARDINAL8 ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL ;
		zus :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF R_Befehl THEN
		inh := GetRegisterWertHW (reladr) BAND 0FFH ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			Get_R_RLR (reladr, zus) ;
			TraceF ("<- 1/6[R.%s] = %'02h", zus, inh) ;
		END ;
%END
	ELSE
		Takte (8) ;
		GW := GetGanzwortAdresse (reladr) ;
		IF GW = NIL THEN
			SpeicherschutzAlarm ('lesend Sechstelwort', reladr) ;
			RETURN  0 ;
		END ;
		IF ODD (reladr) THEN
			inh := LiesHalbwort (GW ^ .lo) BAND 0FFH ;
		ELSE
			inh := LiesHalbwort (GW ^ .hi) BAND 0FFH ;
		END ;
		IF debug.Lesen THEN
			TraceF ("<- 1/6[%'04h] = %'02h", reladr, inh) ;
		END ;
	END ;
	RETURN inh ;
END  GetSechstelwort ;


PROCEDURE GetDrittelwort (reladr : SpeicherAdresse) : CARDINAL16 ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL ;
		zus :	ARRAY [0..31] OF CHAR ;
BEGIN
	IF R_Befehl THEN
		inh := GetRegisterWertHW (reladr) BAND 0FFFFH ;
%IF %NOT WEB %THEN
		IF debug.Lesen THEN
			Get_R_RLR (reladr, zus) ;
			TraceF ("<- 1/3[R.%s] = %'03h", zus, inh) ;
		END ;
%END
	ELSE
		Takte (8) ;
		GW := GetGanzwortAdresse (reladr) ;
		IF GW = NIL THEN
			SpeicherschutzAlarm ('lesend Drittelwort', reladr) ;
			RETURN  0BEECH ;
		END ;
		IF ODD (reladr) THEN
			inh := LiesHalbwort (GW ^ .lo) BAND 0FFFFH ;
		ELSE
			inh := LiesHalbwort (GW ^ .hi) BAND 0FFFFH ;
		END ;
		IF debug.Lesen THEN
			TraceF ("<- 1/3[%'04h] = %'04h", reladr, inh) ;
		END ;
	END ;
	RETURN inh ;
END  GetDrittelwort ;


PROCEDURE GetIndexZellenWert (index : CARDINAL8) : CARDINAL ;
	VAR	inh :	CARDINAL ;
		merk :	BOOLEAN ;
BEGIN
	%IF %NOT optimieren %THEN index := index BAND 0FFH ; %END
%IF IndexAssoziativ %THEN
	RETURN tr24to48 (GetIndexWertAusAssoziativ (index)) ;
%ELSE
	IF debug.IndexWert THEN
		merk := debug.Lesen ;
		debug.Lesen := FALSE ;
		inh := GetHalbwort (ORD(RegX) + ORD(index)) ;
		debug.Lesen := merk ;
		TraceF ("<- <X%'02h> = %'04h", ORD (index), inh) ;
	ELSE
		inh := GetHalbwort (ORD(RegX) + ORD(index)) ;
	END ;
	RETURN tr24to48 (inh) ;
%END
END GetIndexZellenWert ;


PROCEDURE ManipHalbwort (reladr : SpeicherAdresse ; p : HalbwortProc) ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('bearbeitend Halbwort', reladr) ;
		RETURN  ;
	END ;
	IF ODD (reladr) THEN
		p (GW ^ .lo) ;
	ELSE
		p (GW ^ .hi) ;
	END ;
END ManipHalbwort ;


VAR
	BefehlsHilfsSpeicher :		CARDINAL ;
	BefehlsHilfsSpeicherAdresse :	CARDINAL = 1FFFFFFH ;


PROCEDURE GetBefehlsWort (reladr : SpeicherAdresse) : CARDINAL ;
	VAR	GW :	pGanzwort ;
		inh :	CARDINAL ;
		str :	ARRAY [0..15] OF CHAR ;
BEGIN
	IF ODD (reladr) THEN
		IF BefehlsHilfsSpeicherAdresse = reladr-1 THEN		(* 2. Halbwort hinter vorigem Befehlswort *)
			BefehlsHilfsSpeicherAdresse := 1FFFFFFH ;
			AktTypenkennung := 2 ;
%IF %NOT WEB %THEN
			IF debug.BefehlLesen THEN
				GetMnemo (BefehlsHilfsSpeicher, str) ;
				TraceF ("* %'04h %'06h %s", reladr, BefehlsHilfsSpeicher, str) ;
			END ;
%END
			Takte (2) ;
			RETURN BefehlsHilfsSpeicher ;
		END ;
	END ;
	GW := GetGanzwortAdresse (reladr) ;
	IF GW = NIL THEN
		SpeicherschutzAlarm ('lesend Befehlwort', reladr) ;
		RETURN  0C7BEECH ;
	END ;
	IF ODD (reladr) THEN
		inh := LiesHalbwort (GW ^ .lo) ;
		BefehlsHilfsSpeicherAdresse := 1FFFFFFH ;
	ELSE
		inh :=  LiesHalbwort (GW ^ .hi) ;
		BefehlsHilfsSpeicher := LiesHalbwort (GW ^ .lo) ;
		BefehlsHilfsSpeicherAdresse := reladr ;
	END ;
%IF %NOT WEB %THEN
	IF debug.BefehlLesen THEN
		GetMnemo (inh, str) ;
		TraceF ("* %'04h %'06h %s", reladr, inh, str) ;
	END ;
%END
	IF AktTypenkennung <> 2 THEN
		TypenkennungsAlarm (reladr) ;	(* erkannt : *)
		TypenkennungsAlarm2 ;		(* sofort auslösen *)
	END ;
	Takte (8) ;
	RETURN inh ;
END GetBefehlsWort ;


PROCEDURE PutBefehlsWort (reladr : SpeicherAdresse ; inh : CARDINAL) ;
	VAR	GW :	pGanzwort ;
		str,
		str1,
		str2 :	ARRAY [0..63] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Befehl', reladr) ;
		RETURN  ;
	END ;
	IF ODD (reladr) THEN
		upZuwHalbwort (GW ^ .lo, inh) ;
	ELSE
		upZuwHalbwort (GW ^ .hi, inh) ;
	END ;
	IF NeueTypenkennung < 4 THEN		(* für TOK etc. *)
		GW ^ .TK := NeueTypenkennung ;
		NeueTypenkennung := 4 ;
	ELSIF NOT ODD(reladr) THEN
		GW ^ .TK := 2 ;
	END ;
%IF %NOT WEB %THEN
	IF debug.AssemblerProtokoll THEN
		GetMnemo (inh, str) ;
		CardToHex4 (reladr, str1) ;
		CardToHex6 (inh, str2) ;
		TraceF ('* %s %s %s', str1, str2, str) ;
	END ;
%END
END PutBefehlsWort ;


PROCEDURE PutCodeWort (reladr : SpeicherAdresse ; inh : CARDINAL) ;
	VAR	GW :	pGanzwort ;
		str1,
		str2 :	ARRAY [0..63] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Befehl', reladr) ;
		RETURN  ;
	END ;
	IF ODD (reladr) THEN
		upZuwHalbwort (GW ^ .lo, inh) ;
	ELSE
		upZuwHalbwort (GW ^. hi, inh) ;
	END ;
	IF NeueTypenkennung < 4 THEN		(* für TOK etc. *)
		GW ^ .TK := NeueTypenkennung ;
	ELSIF NOT ODD(reladr) THEN
		GW ^ .TK := 2 ;
	END ;
	IF debug.AssemblerProtokoll THEN
		CardToHex4 (reladr, str1) ;
		CardToHex6 (inh, str2) ;
		TraceF ('* %s %c %s', str1, ORD (GW ^ .TK), str2) ;
	END ;
END PutCodeWort ;


PROCEDURE PutCodeGanzwort (reladr : SpeicherAdresse; typenkennung : CARDINAL8 ; wert: CARDINAL64) ;
	VAR	GW :	pGanzwort ;
		str1,
		str2 :	ARRAY [0..63] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Befehl', reladr) ;
		RETURN  ;
	END ;
	upZuwHalbwort (GW ^ .hi, VAL (CARDINAL, wert SHR 24) %IF %NOT optimieren %THEN BAND Cardinal24Bit %END) ;
	upZuwHalbwort (GW ^ .lo, VAL (CARDINAL, wert %IF %NOT optimieren %THEN BAND Cardinal24Bit %END)) ;
	GW ^ .TK := typenkennung ;
	IF debug.AssemblerProtokoll THEN
		CardToHex4 (reladr, str1) ;
		CardToHex12 (wert, str2) ;
		TraceF ('* %s %c %s', str1, ORD(typenkennung), str2) ;
	END ;
END PutCodeGanzwort ;


PROCEDURE PutSechstelwort (reladr : SpeicherAdresse; wert: CARDINAL8) ;
	VAR	GW :	pGanzwort ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Drittelwort', reladr) ;
		RETURN ;
	END ;
	%IF %NOT optimieren %THEN wert := wert BAND 0FFH ; %END
	IF debug.Speichern THEN
		TraceF ("1/6[%'04h] <- %'02h", reladr, wert) ;
	END ;
	IF ODD (reladr) THEN
		upZuwHalbwort (GW ^ .lo, (LiesHalbwort(GW ^ .lo) BAND 0FFFF00H) BOR ORD(wert)) ;
	ELSE
		upZuwHalbwort (GW ^ .hi, (LiesHalbwort(GW ^ .hi) BAND 0FFFF00H) BOR ORD(wert)) ;
	END ;
	Takte (8) ;
END PutSechstelwort ;


PROCEDURE PutDrittelwort (reladr : SpeicherAdresse; wert: CARDINAL16) ;
	VAR	GW :	pGanzwort ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Drittelwort', reladr) ;
		RETURN ;
	END ;
	%IF %NOT optimieren %THEN wert := wert BAND 0FFFFH ; %END
	IF debug.Speichern THEN
		TraceF ("1/3[%'04h] <- %'04h", reladr, wert) ;
	END ;
	IF ODD (reladr) THEN
		upZuwHalbwort (GW ^ .lo, (LiesHalbwort(GW ^ .lo) BAND 0FF0000H) BOR ORD(wert)) ;
	ELSE
		upZuwHalbwort (GW ^ .hi, (LiesHalbwort(GW ^ .hi) BAND 0FF0000H) BOR ORD(wert)) ;
	END ;
	Takte (8) ;
END PutDrittelwort ;


PROCEDURE PutHalbwort (reladr : SpeicherAdresse; wert: CARDINAL) ;
	VAR	GW :	pGanzwort ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Halbwort', reladr) ;
		RETURN ;
	END ;
	wert := wert BAND Cardinal24Bit ;
	IF debug.Speichern THEN
		TraceF ("H[%'04h] <- %'06h", reladr, wert) ;
	END ;
	IF ODD (reladr) THEN
		upZuwHalbwort (GW ^ .lo, wert) ;
	ELSE
		upZuwHalbwort (GW ^ .hi, wert) ;
	END ;
	IF NeueTypenkennung < 4 THEN		(* für TOK etc. *)
		GW ^ .TK := NeueTypenkennung ;
		NeueTypenkennung := 4 ;
	END ;
	Takte (8) ;
END PutHalbwort ;


PROCEDURE ZuwGanzwort (VAR ziel : Ganzwort ; typenkennung : CARDINAL8 ; wert : CARDINAL64) ;
BEGIN
	ziel.TK := typenkennung ;
	upZuwHalbwort (ziel.hi, VAL (CARDINAL, wert SHR 24) %IF %NOT optimieren %THEN BAND Cardinal24Bit %END) ;
	upZuwHalbwort (ziel.lo, VAL (CARDINAL, wert %IF %NOT optimieren %THEN BAND Cardinal24Bit %END)) ;
END ZuwGanzwort ;


PROCEDURE PutGanzwort (reladr : SpeicherAdresse; typenkennung : CARDINAL8 ; wert: CARDINAL64) ;
	VAR	GW :	pGanzwort ;
		val :	CARDINAL ;
		str :	ARRAY [0..31] OF CHAR ;
BEGIN
	GW := GetGanzwortAdresse (reladr) ;
	IF (GW = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('schreibend Ganzwort', reladr) ;
		RETURN ;
	END ;
	IF typenkennung < 4 THEN
		GW ^ .TK := typenkennung ;
	END ;
	upZuwHalbwort (GW ^ .hi, VAL (CARDINAL, wert SHR 24) %IF %NOT optimieren %THEN BAND Cardinal24Bit %END) ;
	upZuwHalbwort (GW ^ .lo, VAL (CARDINAL, wert %IF %NOT optimieren %THEN BAND Cardinal24Bit %END)) ;
	IF debug.Speichern THEN
		CardToHex12 (wert, str) ;
		TraceF ("G[%'04h] <- %h %s", ORD(reladr), ORD(GW ^ .TK), str) ;
	END ;
	Takte (8) ;
END PutGanzwort ;



PROCEDURE getstr (VAR ziel : ARRAY OF CHAR; trans : CARDINAL; halbwort : CARDINAL) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO 3 DO
		ziel [trans + 3 - i] := ZC1toANSI [CHR (halbwort BAND 0FFH)] ;
		halbwort := halbwort SHR 8 ;
	END ;
	ziel [trans + 3] := 0C ;
END getstr ;


PROCEDURE GetStringAusSpeicher (VAR ziel : ARRAY OF CHAR ; reladr : SpeicherAdresse; lng : CARDINAL) ;	(* ZC1 -> ANSI *)
	VAR	i,
		wert :	CARDINAL ;
		tk :	CARDINAL8 ;
BEGIN
	FOR i := 1 TO lng BY 3 DO
		wert := GetHalbwort (reladr) ;
		IF i = 1 THEN
			tk := AktTypenkennung ;
		END ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
		getstr (ziel, i - 1, wert) ;
		INC (reladr) ;
	END ;
	ziel [lng] := 0C ;
	ziel [lng+1] := 0C ;
	AktTypenkennung := tk ;
END GetStringAusSpeicher ;


PROCEDURE GetStringAusGanzwort (quelle : Ganzwort ; VAR str : ARRAY OF CHAR) ;
	VAR	i,
		lng :	CARDINAL ;
BEGIN
	lng := 6 ;
	FOR i := 0 TO 5 DO
		IF quelle.byte6 [i] = 0 THEN
			lng := i ;
			BREAK ;
		END ;
		str [i] := ZC1toANSI [CHR (quelle.byte6 [i])] ;
	END ;
	IF HIGH(str) >= lng THEN
		str [lng] := 0C ;
	END ;
END GetStringAusGanzwort ;


PROCEDURE putstrU (quelle : ARRAY OF CHAR ; trans : CARDINAL) : CARDINAL ;
	VAR	i,
		wert :	CARDINAL ;
		ch :	CHAR ;
BEGIN
	wert := 0 ;
	FOR i := 1 TO 3 DO
		ch := quelle [trans + i - 1] ;
		wert := (wert SHL 8) BOR ORD (ANSItoZC1 [ch]) ;
	END ;
	RETURN wert ;
END putstrU ;


PROCEDURE PutStringInSpeicherU (quelle : ARRAY OF CHAR ; reladr : SpeicherAdresse; lng : CARDINAL) ;	(* ANSI -> ZC1 *)
	VAR	i, tk,
		wert :	CARDINAL ;
BEGIN
	quelle [lng] := 0C ;
	quelle [lng+1] := 0C ;
	tk := NeueTypenkennung ;
	FOR i := 1 TO lng BY 3 DO
		NeueTypenkennung := tk ;
		PutHalbwort (reladr, putstrU (quelle, i-1)) ;
		INC (reladr) ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
	END ;
END PutStringInSpeicherU ;


VAR
	zuEnde :	BOOLEAN ;


PROCEDURE putstr (quelle : ARRAY OF CHAR ; trans : CARDINAL) : CARDINAL ;
	VAR	i,
		wert :	CARDINAL ;
		ch :	CHAR ;
BEGIN
	wert := 0 ;
	FOR i := 1 TO 3 DO
		IF zuEnde THEN
			wert := wert SHL 8 ;
		ELSE
			ch := quelle [trans + i - 1] ;
			IF ch = 0C THEN
				zuEnde := TRUE ;
			END ;
			wert := (wert SHL 8) BOR ORD (ANSItoZC1 [ch]) ;
		END ;
	END ;
	RETURN wert ;
END putstr ;


PROCEDURE PutStringInSpeicher (quelle : ARRAY OF CHAR ; reladr : SpeicherAdresse; lng : CARDINAL) ;	(* ANSI -> ZC1 *)
	VAR	i, tk,
		wert :	CARDINAL ;
BEGIN
	zuEnde := FALSE ;
	quelle [lng] := 0C ;
	quelle [lng+1] := 0C ;
	tk := NeueTypenkennung ;
	FOR i := 1 TO lng BY 3 DO
		NeueTypenkennung := tk ;
		IF zuEnde THEN
			PutHalbwort (reladr, 0) ;
		ELSE
			PutHalbwort (reladr, putstr (quelle, i-1)) ;
		END ;
		INC (reladr) ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
	END ;
END PutStringInSpeicher ;


PROCEDURE PutStringInGanzwortSpeicher (quelle : ARRAY OF CHAR ; reladr : SpeicherAdresse; lng : CARDINAL) ;	(* ANSI -> ZC1 *)
	VAR	i, tk,
		wert :	CARDINAL ;
BEGIN
	zuEnde := FALSE ;
	quelle [lng] := 0C ;
	quelle [lng+1] := 0C ;
	tk := NeueTypenkennung ;
	FOR i := 1 TO lng BY 3 DO
		NeueTypenkennung := tk ;
		IF zuEnde THEN
			PutHalbwort (reladr, 0) ;
		ELSE
			PutHalbwort (reladr, putstr (quelle, i-1)) ;
		END ;
		INC (reladr) ;
		IF AlarmGewesen THEN
			RETURN ;
		END ;
	END ;
	IF ODD(reladr) THEN
		PutHalbwort (reladr, 0) ;		(* auf Ganzwortgrenze auffüllen *)
	END ;
END PutStringInGanzwortSpeicher ;


PROCEDURE PutStringInRegister (quelle : ARRAY OF CHAR ; VAR reg : reg48) ;				(* ANSI -> ZC1 *)
	VAR	i, h :		CARDINAL ;
		inh :		CARDINAL64 ;
BEGIN
	inh := 0 ;
	h := LENGTH (quelle)-1 ;
	IF h > 5 THEN
		h := 5 ;
	END ;
	FOR i := 0 TO h DO
		inh := (inh SHL 8) BOR VAL (CARDINAL64, ORD(ANSItoZC1[quelle[i]])) ;
	END ;
	FOR i := h+1 TO 5 DO
		inh := inh SHL 8 ;
	END ;
	reg.inh := inh ;
	IF NeueTypenkennung < 4 THEN
		reg.TK := NeueTypenkennung ;
		NeueTypenkennung := 4 ;
	ELSE
		reg.TK := 3 ;
	END ;
END PutStringInRegister ;


PROCEDURE GetStringAusRegister (reg : reg48 ; VAR ziel : ARRAY OF CHAR) ;				(* ZC1 -> ANSI *)
	VAR	i :	CARDINAL ;
		inh :	CARDINAL64 ;
BEGIN
	inh := reg.inh ;
	FOR i := 0 TO 5 DO
		ziel [5 - i] := ZC1toANSI [CHR(inh BAND 0FFH)] ;
		inh := inh SHR 8 ;
	END ;
	IF HIGH(ziel) > 5 THEN
		ziel [6] := 0C ;
	END ;
END GetStringAusRegister ;


PROCEDURE GetAchtelSeite (quelle : SpeicherAdresse ; VAR as : tAchtelSeite) ;
	VAR	p :	pAchtelSeite ;
BEGIN
	IF debug.Lesen THEN
		TraceF ("<- AS[%'06h]", quelle) ;
	END ;
	p := CAST (pAchtelSeite, GetGanzwortAdresse (quelle BAND 3FFF00H)) ;
	IF p = NIL THEN
		SpeicherschutzAlarm ('bei Achtelseitentransport', quelle) ;
		RETURN ;
	END ;
	as := p ^ ;
END GetAchtelSeite ;


PROCEDURE PutAchtelSeite (ziel : SpeicherAdresse ; VAR as : tAchtelSeite) ;
	VAR	p :	pAchtelSeite ;
BEGIN
	IF debug.Lesen THEN
		TraceF ("-> AS[%'06h]", ziel) ;
	END ;
	p := CAST (pAchtelSeite, GetGanzwortAdresse (ziel BAND 3FFF00H)) ;
	IF (p = NIL) OR AktSchreibschutz THEN
		SpeicherschutzAlarm ('bei Achtelseitentransport', ziel) ;
		RETURN ;
	END ;
	p ^ := as ;
END PutAchtelSeite ;


PROCEDURE PutIndexZellenWert (index : CARDINAL8 ; wert : CARDINAL) ;
	VAR	str,
		str2 :	ARRAY [0..31] OF CHAR ;
		merk :	BOOLEAN ;
BEGIN
	%IF %NOT optimieren %THEN index := index BAND 0FFH ; %END

%IF IndexAssoziativ %THEN
	PutIndexWertInAssoziativ (index, wert) ;
%ELSE
	IF debug.IndexWert THEN
		merk := debug.Speichern ;
		debug.Speichern := FALSE ;
		PutHalbwort (ORD(RegX) + ORD(index), wert) ;
		debug.Speichern := merk ;
		CardToHex2 (ORD(index), str) ;
		CardToHex4 (wert BAND Cardinal24Bit, str2) ;
		TraceF ('X%s <- %s', str, str2) ;
	ELSE
		PutHalbwort (ORD(RegX) + ORD(index), wert) ;
	END ;
%END
END PutIndexZellenWert ;


PROCEDURE zt0 (VAR GW : Ganzwort) ;
BEGIN
	GW.TK := 0 ;
END zt0 ;


PROCEDURE zt1 (VAR GW : Ganzwort) ;
BEGIN
	GW.TK := 1 ;
END zt1 ;


PROCEDURE zt2 (VAR GW : Ganzwort) ;
BEGIN
	GW.TK := 2 ;
END zt2 ;


PROCEDURE zt3 (VAR GW : Ganzwort) ;
BEGIN
	GW.TK := 3 ;
END zt3 ;


PROCEDURE LeitblockNext ;
	VAR	i,
		j,
		snr :	CARDINAL ;
BEGIN
	snr := 0 ;
	FOR i := 0 TO AktMaxBelegteGrossSeite DO							(* Großseiten 0 bis 3 anordnen = 128 KW *)
		FOR j := 0 TO MaxSeiteInGrossSeite DO
			WITH SeitenKachelTabelle [i, j] DO
				IF zugeteilt THEN		(* ist belegt - neu zuordnen *)
					kachel := KachelBelegen () ;
				END ;
				zugeteilt := TRUE ;
				beschreibbar := TRUE ;
			END ;
		END ;
	END ;
END LeitblockNext ;


PROCEDURE Leitblock0 ;
	VAR	i,
		j,
		snr :	CARDINAL ;
BEGIN
	snr := 0 ;
	FOR i := 0 TO AktMaxBelegteGrossSeite DO							(* Großseiten 0 bis 3 anordnen = 128 KW 1:1 *)
		FOR j := 0 TO MaxSeiteInGrossSeite DO
			WITH SeitenKachelTabelle [i, j] DO
				zugeteilt := TRUE ;
				beschreibbar := TRUE ;
				kachel := snr ;
				INC (snr) ;
			END ;
		END ;
	END ;

	FOR i := AktMaxBelegteGrossSeite + 1 TO MaxLeitblockNummer DO					(* restliche Großseiten sind undefiniert *)
		FOR j := 0 TO MaxSeiteInGrossSeite DO
			WITH SeitenKachelTabelle [i, j] DO
				zugeteilt := FALSE ;
				beschreibbar := FALSE ;
				kachel := 0 ;
			END ;
		END ;
	END ;

	FOR i := 0 TO AktAnzahlGrossSeiten * (MaxSeiteInGrossSeite + 1) - 1 DO				(* evtl. eingemappte Gebiete weg *)
	(*
		KachelBelegt [i] := TRUE ;
	*)
		Hauptspeicher [i] := KSP [i] ;
	END ;
END Leitblock0 ;


PROCEDURE StdLeitblock ;
BEGIN
	IF Grundzustand THEN
		IF debug.MemoryProtokoll THEN
			TraceF ('Grund-Leitblock :') ;
		END ;
		Leitblock0 ;
	ELSE
		IF debug.MemoryProtokoll THEN
			TraceF ('Hilfs-Leitblock :') ;
		END ;
		LeitblockNext ;
	END ;
END StdLeitblock ;


PROCEDURE LeitblockFreigeben ;
	VAR	i,
		j,
		snr :	CARDINAL ;
BEGIN
	snr := 0 ;
	IF debug.MemoryProtokoll THEN
		TraceF ('Leitblock freigeben') ;
	END ;
	FOR i := 0 TO AktMaxBelegteGrossSeite DO							(* Großseiten 0 bis 3 anordnen = 128 KW *)
		FOR j := 0 TO MaxSeiteInGrossSeite DO
			WITH SeitenKachelTabelle [i, j] DO
				IF zugeteilt THEN		(* ist belegt - freigeben *)
					KachelFreigeben (kachel) ;
				END ;
			END ;
		END ;
	END ;
END LeitblockFreigeben ;


PROCEDURE upVorbes (MaxGrossSeite : CARDINAL) ;
	VAR	i :	CARDINAL ;
		zw :	BOOLEAN ;
BEGIN
	zw := debug.Speichern ;
	debug.Speichern := FALSE ;
	FOR i := 0 TO (MaxGrossSeite + 1) * HalbworteInGrossSeite - 2 BY 2 DO				(* Großseiten vorbesetzen *)
		PutGanzwort (i, 3, 0CAFFEEFAFFEEH) ;
	END ;
	debug.Speichern := zw ;
END upVorbes ;


PROCEDURE Vorbes2 ;
BEGIN
%IF WEB %THEN
	upVorbes (0) ;											(* nur die unterste Standard-Großseite vorbesetzen *)
%ELSE
	upVorbes (3) ;											(* nur die untersten 4 Standard-Großseiten vorbesetzen *)
%END
END Vorbes2 ;


PROCEDURE Vorbes ;
BEGIN
	upVorbes (AktMaxBelegteGrossSeite) ;								(* alle reservierten Großseiten vorbesetzen *)
END Vorbes ;


PROCEDURE SpeicherVorbesetzung ;
	VAR	wert :	CARDINAL64 ;
		wert32,
		i, j :	CARDINAL ;
		vb :	Wort6Bytes ;
BEGIN
	vb [5] := 55H ;
	vb [4] := 44H ;
	vb [3] := 33H ;
	vb [2] := 22H ;
	vb [1] := 11H ;
	vb [0] := 00H ;

	wert := 1000000000000H ;
	FOR i := 1 TO 48 DO
		wert := wert SHR 1 ;
		Bitmasken48 [i] := wert ;
	END ;

	wert32 := 1000000H ;
	FOR i := 1 TO 24 DO
		wert32 := wert32 SHR 1 ;
		Bitmasken24 [i] := wert32 ;
	END ;
	wert32 := 10000H ;
	FOR i := 1 TO 16 DO
		wert32 := wert32 SHR 1 ;
		Bitmasken16 [i] := wert32 ;
	END ;
	wert32 := 100H ;
	FOR i := 1 TO 8 DO
		wert32 := wert32 SHR 1 ;
		Bitmasken8 [i] := wert32 ;
	END ;

	FOR i := 0 TO AktAnzahlGrossSeiten * (MaxSeiteInGrossSeite + 1) - 1 DO
		IF Hauptspeicher [i] <> NIL THEN
			FOR j := 0 TO MaxGanzwortInSeite DO
				WITH Hauptspeicher [i] ^ [j] DO
%IF Dreierprobe %THEN
					DP := 3 ;
%END
					TK := 3 ;
					byte6 := vb ;
				END ;
			END ;
		END ;
	END ;
END SpeicherVorbesetzung ;


PROCEDURE SpeicherReservieren (AnzahlGrossSeiten : CARDINAL) ;
	VAR	i :	CARDINAL ;
BEGIN
	AktMaxBelegteGrossSeite := AnzahlGrossSeiten - 1 ;

	AktMaxGanzwortNummer := (AnzahlGrossSeiten) * HalbworteInGrossSeite DIV 2 - 1 ;

	IF AnzahlGrossSeiten > AktAnzahlGrossSeiten THEN				(* Speicher real vergrößern *)

		FOR i := 0 TO AnzahlGrossSeiten * (MaxSeiteInGrossSeite + 1) - 1 DO
			IF KSP [i] = NIL THEN
				NEW (KSP [i]) ;
			END ;
			Hauptspeicher [i] := KSP [i] ;
		END ;

		AktAnzahlGrossSeiten := AnzahlGrossSeiten ;				(* reale Speicher-Größe *)

		StdLeitblock ;

		SpeicherVorbesetzung ;

		Vorbes ;

	ELSE

		StdLeitblock ;

	END ;

END SpeicherReservieren ;


PROCEDURE CheckKSPbelegung ;
	VAR	max,
		k :	CARDINAL ;
BEGIN
	KSPanz := 0 ;
	max := 0 ;
	FOR k := 1 TO MAX(KachelNummer) DO
		IF KachelBelegt [k] THEN				(* Kachel ist belegt *)
			INC (KSPanz) ;
		END ;
		IF KSP [k] <> NIL THEN					(* Kachel ist belegt *)
			INC (max) ;
		END ;
	END ;
	IF KSPanz > KSPmax THEN
		KSPmax := KSPanz ;
	END ;
	IF max > KSPmax THEN
		KSPmax := max ;
	END ;
END CheckKSPbelegung ;


PROCEDURE KachelBelegen () : KachelNummer ;
	VAR	i, k :		KachelNummer ;
		gwVorbes :	Ganzwort ;
BEGIN
	gwVorbes.TK := 0 ;
	gwVorbes.byte6 [0] := 0 ;
	gwVorbes.byte6 [1] := 47H ;
	gwVorbes.byte6 [2] := 0 ;
	gwVorbes.byte6 [3] := 0 ;
	gwVorbes.byte6 [4] := 11H ;
	gwVorbes.byte6 [5] := 0 ;

	FOR k := 0 TO MAX(KachelNummer) DO
		IF NOT KachelBelegt [k] THEN					(* Kachel ist noch nicht belegt *)
			IF KSP [k] = NIL THEN				(* für Kachel ist noch kein Speicher da *)
				NEW (KSP [k]) ;
			END ;
			Hauptspeicher [k] := KSP [k] ;
			FOR i := 0 TO MaxGanzwortInSeite DO
				Hauptspeicher [k] ^ [i] := gwVorbes ;
			END ;
			KachelBelegt [k] := TRUE ;
			IF debug.MemoryProtokoll THEN
				TraceF ("Kachel %'03h belegt", k) ;
			END ;
			RETURN k ;
		END ;
	END ;
	RETURN 0 ;
END KachelBelegen ;


PROCEDURE KachelFreigeben (k : KachelNummer) ;
BEGIN
	IF KachelBelegt [k] THEN					(* Kachel ist belegt *)
		IF k > AktMaxBelegteGrossSeite * (MaxSeiteInGrossSeite + 1) THEN
			KachelBelegt [k] := FALSE ;
			IF debug.MemoryProtokoll THEN
				TraceF ("Kachel %'03h frei", k) ;
			END ;
		END ;
	ELSE
		IF debug.MemoryProtokoll THEN
			TraceF ("Kachel %'03h war frei", k) ;
		END ;
	END ;
END KachelFreigeben ;


PROCEDURE clear (VAR puf : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 0 TO HIGH (puf) DO
		puf [i] := 0C ;
	END ;
END clear ;


PROCEDURE Init ;
	VAR	i :	CARDINAL ;
		wert :	CARDINAL64 ;
BEGIN
	AktAnzahlGrossSeiten := 0 ;	(* noch kein Speicher reserviert *)


	FOR i := 0 TO MaxKachelNummer DO
		KSP [i] := NIL ;
		Hauptspeicher [i]:= NIL ;	(* noch nix reserviert *)
		KachelBelegt [i] := FALSE ;	(* und noch nix belegt *)
	END ;

%IF WEB %THEN
	SpeicherReservieren (1) ;		(* standardmäßig 1 GrossSeite = 32 KW = 192 KB reservieren *)
%ELSE
	SpeicherReservieren (4) ;		(* standardmäßig 4 GrossSeiten = 128 KW = 768 KB reservieren *)
%END

	KSPmax := 0 ;
	CheckKSPbelegung ;

	wert := 1 ;
	FOR i := 1 TO 48 DO
		wert := wert * 2 ;
		ShiftFaktoren [i] := wert ;
	END ;

	FOR i := 0 TO MAX(OperatorNummer) DO
		Operatoren [i].OLK := 0 ;
	END ;

	PutStringInRegister (_STDDB, RegA) ;
	AktAuftrag.Hierarchie [0] := RegA.inh ;
	PutStringInRegister (_OEFDB, RegA) ;
	AktAuftrag.Hierarchie [1] := RegA.inh ;

	OLKpegel := 101 ;
	AktOLK := OLKpegel ;
	AktOperator := 0 ;
	Operatoren [0].OLK := 0 ;
END Init ;


PROCEDURE InitSpeicher ;
BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
END InitSpeicher ;


END Struktur.
