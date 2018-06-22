
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Gebiet;


(*	31.03.18	*)


FROM SYSTEM IMPORT
	ADR ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Trace IMPORT
	TraceF ;

IMPORT	Strings, debug ;

FROM Terminal IMPORT
	WriteF ;

FROM Struktur IMPORT * ;

FROM AbwicklerUp IMPORT
	SSRfehler ;


CONST
	cGebietsNummer1 =	1000 ;

VAR
	vorbes :	Ganzwort ;

	GebietsNummer :	CARDINAL = cGebietsNummer1 ;

	initialisiert :	BOOLEAN = FALSE ;




PROCEDURE NewSeite () : pRAMelem ;
	VAR	p :	pRAMelem ;
		i :	CARDINAL ;
BEGIN
	NEW (p) ;
	p ^ .prev := NIL ;
	p ^ .next := NIL ;

	FOR i := 0 TO MaxGanzwortInSeite DO
		p ^ .Seite [i] := vorbes ;
	END ;

	IF debug.MemoryProtokoll THEN
		TraceF ("ram %h +", p) ;
	END ;

	RETURN p ;
END NewSeite ;


PROCEDURE DeleteSeite (VAR p : pRAMelem) ;
BEGIN
	IF debug.MemoryProtokoll THEN
		TraceF ("ram %h -", p) ;
	END ;
	DISPOSE (p) ;
END DeleteSeite ;


PROCEDURE LastGebiet () : pGebiet ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := Gebiet0 ;
	IF geb = NIL THEN
		RETURN NIL ;
	END ;
	WHILE geb ^ .next <> NIL DO
		geb := geb ^ .next ;	(* letztes Gebiet der Liste suchen *)
	END ;
	RETURN geb ;
END LastGebiet ;


PROCEDURE NewGebiet (Laenge : CARDINAL) : pGebiet ;
	VAR	geb :		pGebiet ;
		aktRAM :	pRAMelem ;
		i :		CARDINAL ;
BEGIN
	NEW (geb) ;
	geb ^ .next := NIL ;
	geb ^ .prev := LastGebiet () ;
	IF Gebiet0 = NIL THEN
		Gebiet0 := geb ;
	END ;
	IF geb ^ .prev <> NIL THEN
		geb ^ .prev ^ .next := geb ;
	END ;
	INC (GebietsNummer) ;
	IF debug.MemoryProtokoll THEN
		TraceF ("gebiet %c +", GebietsNummer) ;
	END ;
	geb ^ .eigenGebietsNummer := GebietsNummer  ;
	geb ^ .GebietsNummer := GebietsNummer ;		(* eindeutige Gebiets-Nummer 'GNR' *)
	geb ^ .Laenge := Laenge ;
	geb ^ .OLK := AktOLK ;
	geb ^ .adressiert := FALSE ;
	geb ^ .Schreibsperre := FALSE ;
	geb ^ .Stellvertreter := FALSE ;
	geb ^ .Master := geb ;
	geb ^ .RAM := NIL ;
	IF Laenge > 0 THEN
		aktRAM := NewSeite() ;				(* 1. Seite reservieren *)
		geb ^ .RAM := aktRAM ;
		FOR i := 2 TO Laenge DO
			aktRAM ^ .next := NewSeite() ;		(* restliche Seiten reservieren *)
			aktRAM ^ .next ^ .prev := aktRAM ;
			aktRAM := aktRAM ^ .next ;
		END ;
	END ;
	RETURN geb ;
END NewGebiet ;


PROCEDURE KillGebiet (geb : pGebiet) ;
	VAR	aktRAM,
		zwRAM :		pRAMelem ;
		i :		CARDINAL ;
BEGIN
	IF geb <> NIL THEN
		IF debug.MemoryProtokoll THEN
			IF geb ^ .GebietsNummer <> geb ^ .eigenGebietsNummer THEN
				TraceF ("gebiet %c (-> %c) -", geb ^ .eigenGebietsNummer, geb ^ .GebietsNummer) ;
			ELSE
				TraceF ("gebiet %c -", geb ^ .GebietsNummer) ;
			END ;
		END ;
		IF NOT geb ^ .Stellvertreter THEN		(* Speicher freigeben *)
			aktRAM := geb ^ .RAM ;
			WHILE aktRAM <> NIL DO
				zwRAM := aktRAM ;
				aktRAM := aktRAM ^ .next ;
				DeleteSeite (zwRAM) ;
			END ;
		END ;
		IF geb ^ .adressiert THEN
			FOR i := 1 TO geb ^ .Laenge DO
				SpeicherZuordnungWeg (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1)) ;
			END ;
		END ;
		IF geb ^ .next <> NIL THEN
			geb ^ .next ^ .prev := geb ^ .prev ;
		END ;
		IF geb ^ .prev = NIL THEN			(* erstes Gebiet der Liste wird gelöscht *)
			Gebiet0 := geb ^ .next ;
		ELSE
			geb ^ .prev ^ .next := geb ^ .next ;	(* aus Liste ausklinken *)
		END ;
		DISPOSE (geb) ;
	END ;
END KillGebiet ;


PROCEDURE KillAlleOLgebiete (OLK : CARDINAL) ;
	VAR	geb :	pGebiet ;
		ok :	BOOLEAN ;
BEGIN
	LOOP
		ok := FALSE ;
		geb := Gebiet0 ;
		WHILE geb <> NIL DO
			IF (geb ^ .Stellvertreter OR NOT geb ^ .Dauergebiet) AND (geb ^ .OLK = OLK) THEN
				ok := TRUE ;
				KillGebiet (geb) ;
				BREAK ;
			END ;
			geb := geb ^ .next ;
		END ;
		IF NOT ok THEN
			EXIT ;
		END ;
	END ;
END KillAlleOLgebiete ;


PROCEDURE AlleGebieteLoeschen ;
	VAR	geb :	pGebiet ;
BEGIN
	LOOP
		geb := Gebiet0 ;
		IF geb = NIL THEN
			EXIT ;
		END ;
		KillGebiet (geb) ;
	END ;
END AlleGebieteLoeschen ;


PROCEDURE InitGebiete ;		(* nur Notnagel : dabei kann Speicher-Leak entstehen *)
	VAR	geb :	pGebiet ;
BEGIN
	WHILE Gebiet0 <> NIL DO
		geb := Gebiet0 ;
		Gebiet0 := Gebiet0 ^ .next ;
		DISPOSE (geb) ;
	END ;
END InitGebiete ;


PROCEDURE upGetGebiet (GNR : CARDINAL) : pGebiet ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := Gebiet0 ;
	WHILE geb <> NIL DO
		IF geb ^ .GebietsNummer = GNR THEN
			RETURN geb ;
		END ;
		geb := geb ^ .next ;
	END ;
	SSRfehler (8, 'Gebietsnummer unbekannt') ;
	IF debug.MemoryProtokoll THEN
		TraceF ('Gebiet %h unbekannt', GNR) ;
	END ;
	RETURN NIL ;
END upGetGebiet ;


PROCEDURE GetGebiet (GNR : CARDINAL) : pGebiet ;
	VAR	geb,
		geb2 :	pGebiet ;
BEGIN
	geb := upGetGebiet (GNR) ;
	geb2 := geb ;
	WHILE geb2 <> NIL DO
		IF (geb2 ^ .OLK = AktOLK) AND (geb2 ^ .GebietsNummer = GNR) THEN
			RETURN geb2 ;
		END ;
		geb2 := geb2 ^ .next ;
	END ;
	RETURN geb ;
END GetGebiet ;



PROCEDURE LastSeite (geb : pGebiet) : pRAMelem ;
	VAR	aktRAM :	pRAMelem ;
BEGIN
	aktRAM := geb ^ .RAM ;
	IF aktRAM <> NIL THEN
		WHILE aktRAM ^ .next <> NIL DO
			aktRAM := aktRAM ^ .next ;	(* letzte Seite des Gebietes suchen *)
		END ;
	END ;
	RETURN aktRAM ;
END LastSeite ;


PROCEDURE VerlaengereGebiet (geb : pGebiet ; neueLaenge : CARDINAL) : BOOLEAN ;
	VAR	neuRAM0,
		ram :		pRAMelem ;
		i :		CARDINAL ;
BEGIN
	IF debug.MemoryProtokoll THEN
		TraceF ("gebiet %c := %c >", geb ^ .GebietsNummer, neueLaenge) ;
	END ;
	ram := LastSeite (geb) ;
	neuRAM0 := NIL ;
	FOR i := geb ^ .Laenge+1 TO neueLaenge DO							(* erstmal phys. Speicher belegen *)
		IF ram = NIL THEN
			ram := NewSeite () ;
			geb ^ .RAM := ram ;
		ELSE
			ram ^ .next := NewSeite () ;
			ram ^ .next ^ .prev := ram ;
			ram := ram ^ .next ;
		END ;
		IF neuRAM0 = NIL THEN
			neuRAM0 := ram ;		(* erste hinzugekommene Seite *)
		END ;
	END ;
	IF geb ^ .adressiert THEN
		FOR i := geb ^ .Laenge + 1 TO neueLaenge DO						(* prüfen, ob Adressraum für verlängertes Gebiet frei *)
			IF TestAdresse (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1)) THEN	(* diese Adresse ist schon belegt *)
				IF debug.MemoryProtokoll THEN
					TraceF ('Geb-Verlaengere-Adresse belegt : %h', geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1)) ;
				END ;
				RETURN FALSE ;
			END ;
		END ;

		ram := neuRAM0 ;
		FOR i := geb ^ .Laenge + 1 TO neueLaenge DO						(* nun Speicher in Adressraum hängen *)
			SpeicherZuordnung (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1), ADR (ram ^ .Seite), geb ^ .Schreibsperre) ;
			ram := ram ^ .next ;
		END ;
	END ;
	geb ^ .Laenge := neueLaenge ;
	RETURN TRUE ;
END VerlaengereGebiet ;


PROCEDURE KuerzeGebiet (geb : pGebiet ; neueLaenge : CARDINAL) ;
	VAR	ram,
		zwRAM :		pRAMelem ;
		i :		CARDINAL ;
BEGIN
	IF debug.MemoryProtokoll THEN
		TraceF ("gebiet %c := %c <", geb ^ .GebietsNummer, neueLaenge) ;
	END ;
	IF geb ^ .adressiert THEN
		FOR i := neueLaenge + 1 TO geb ^ .Laenge DO
			SpeicherZuordnungWeg (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1)) ;
		END ;
	END ;
	ram := LastSeite (geb) ;
	FOR i := neueLaenge+1 TO geb ^ .Laenge DO
		zwRAM := ram ;
		ram := ram ^ .prev ;
		DeleteSeite (zwRAM) ;
	END ;
	IF ram = NIL THEN
		geb ^ .RAM := NIL ;
	ELSE
		ram ^ .next := NIL ;
	END ;
	geb ^ .Laenge := neueLaenge ;
END KuerzeGebiet ;


PROCEDURE SuchePGNM (PGNM : ARRAY OF CHAR) (* GNR *) : CARDINAL ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := Gebiet0 ;
	WHILE geb <> NIL DO
		IF Strings.Equal (PGNM, geb ^ .PGNM) THEN
			RETURN geb ^ .GebietsNummer ;
		END ;
		geb := geb ^ .next ;
	END ;
	RETURN 0 ;
END SuchePGNM ;


PROCEDURE SucheOGNM (OGNM : ARRAY OF CHAR) (* GNR *) : CARDINAL ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := Gebiet0 ;
	WHILE geb <> NIL DO
		IF Strings.Equal (OGNM, geb ^ .OGNM) AND (geb ^ .OLK = AktOLK) THEN
			RETURN geb ^ .GebietsNummer ;
		END ;
		geb := geb ^ .next ;
	END ;
	RETURN 0 ;
END SucheOGNM ;


PROCEDURE CheckGNR (GNR : CARDINAL) : BOOLEAN ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb <> NIL THEN
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END CheckGNR ;


PROCEDURE CreateGebiet (OGNM, PGNM : ARRAY OF CHAR ; Laenge : CARDINAL ; Dauergebiet : BOOLEAN ) (* GNR *) : CARDINAL ;
	VAR	geb :	pGebiet ;
BEGIN
	IF (PGNM [0] <> 0C) AND (SuchePGNM (PGNM) <> 0) THEN	(* PGNM gibt's schon *)
		IF debug.MemoryProtokoll THEN
			TraceF ('Geb-Create : PGNM %s schon vorhanden', PGNM) ;
		END ;
		RETURN 0 ;
	END ;
	IF (OGNM [0] <> 0C) AND (SucheOGNM (OGNM) <> 0) THEN	(* OGNM gibt's schon *)
		IF debug.MemoryProtokoll THEN
			TraceF ('Geb-Create : OGNM %s schon vorhanden', OGNM) ;
		END ;
		RETURN 0 ;
	END ;
	geb := NewGebiet (Laenge) ;
	geb ^ .PGNM := PGNM ;
	geb ^ .PGNM [6] := 0C ;
	geb ^ .OGNM := OGNM ;
	geb ^ .OGNM [6] := 0C ;
	geb ^ .Dauergebiet := Dauergebiet ;
	geb ^ .AnfangsSeite := 0 ;
	RETURN geb ^ .GebietsNummer ;
END CreateGebiet ;


PROCEDURE upAbmelden (geb : pGebiet) ;
	VAR	ram :	pRAMelem ;
		i :	CARDINAL ;
BEGIN
	IF geb ^ .adressiert THEN
		IF debug.MemoryProtokoll THEN
			IF geb ^ .GebietsNummer <> geb ^ .eigenGebietsNummer THEN
				TraceF ('Geb-abmelden %c (-> %c)', geb ^ .eigenGebietsNummer, geb ^ .GebietsNummer) ;
			ELSE
				TraceF ('Geb-abmelden %c', geb ^ .eigenGebietsNummer) ;
			END ;
		END ;
		geb ^ .adressiert := FALSE ;
		ram := geb ^ .RAM ;
		FOR i := 1 TO geb ^ .Laenge DO
			SpeicherZuordnungWeg (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1)) ;
			ram := ram ^ .next ;
		END ;
	END ;
END upAbmelden ;


PROCEDURE DeleteGebiet (GNR : CARDINAL) : BOOLEAN ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb <> NIL THEN
		upAbmelden (geb) ;
		IF geb ^ .Stellvertreter THEN
			KillGebiet (geb ^ .Master) ;
		END ;
		KillGebiet (geb) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END DeleteGebiet ;


PROCEDURE AnmeldeGebiet (GNR : CARDINAL ; adresse : SpeicherAdresse ; OGNMneu : ARRAY OF CHAR) : BOOLEAN ;
	VAR	geb,
		geb2 :	pGebiet ;
		ram :	pRAMelem ;
		i :	CARDINAL ;
BEGIN
	geb := upGetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;
	IF geb ^ .OLK <> AktOLK THEN
		geb2 := NewGebiet (0) ;
		IF debug.MemoryProtokoll THEN
			TraceF ('Geb-Stv : %c(%c) -> %c(%c)', geb2 ^ .GebietsNummer, AktOLK, geb ^ .GebietsNummer, geb ^ .OLK) ;
		END ;
		geb2 ^ .Master := geb ;
		geb2 ^ .OLK := AktOLK ;
		geb2 ^ .GebietsNummer:= geb ^ .GebietsNummer ;
		geb2 ^ .Laenge := geb ^ .Laenge ;
		IF OGNMneu [0] <> 0C THEN
			geb2 ^ .OGNM := OGNMneu ;
		ELSE
			geb2 ^ .OGNM := geb ^ .OGNM ;
		END ;
		geb2 ^ .PGNM := geb ^ .PGNM ;
		geb2 ^ .Dauergebiet := geb ^ .Dauergebiet ;
		geb2 ^ .Schreibsperre := geb ^ .Schreibsperre ;
		geb2 ^ .Stellvertreter := TRUE ;
		geb2 ^ .RAM := geb ^ .RAM ;
		geb := geb2 ;
	END ;

	IF adresse <> 0 THEN
		adresse := adresse BAND Max24bitWertPositiv ;
		FOR i := 1 TO geb ^ .Laenge DO							(* prüfen, ob Adressraum für kompl. Gebiet frei *)
			IF TestAdresse (adresse + (i-1) * (MaxHalbwortInSeite + 1)) THEN	(* diese Adresse ist schon belegt *)
				IF debug.MemoryProtokoll THEN
					TraceF ('Geb-Anmelde-Adresse belegt : %h', adresse + (i-1) * (MaxHalbwortInSeite + 1)) ;
				END ;
				RETURN FALSE ;
			END ;
		END ;

		IF debug.MemoryProtokoll THEN
			TraceF ('Gebiet %c -> KSP %h', GNR, adresse) ;
		END ;

		ram := geb ^ .RAM ;
		FOR i := 1 TO geb ^ .Laenge DO
			SpeicherZuordnung (adresse + (i-1) * (MaxHalbwortInSeite + 1), ADR (ram ^ .Seite), geb ^ .Schreibsperre) ;
			ram := ram ^ .next ;
		END ;
		geb ^ .adressiert := TRUE ;
		geb ^ .AnfangsSeite := adresse ;
	END ;
	RETURN TRUE ;
END AnmeldeGebiet ;


PROCEDURE AbmeldeGebiet (GNR : CARDINAL) ;
	VAR	geb,
		geb2 :	pGebiet ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN ;
	END ;
	upAbmelden (geb) ;
	IF NOT geb ^ .Dauergebiet AND (geb ^ .OLK = AktOLK) THEN
		KillGebiet (geb) ;
	END ;
END AbmeldeGebiet ;


PROCEDURE EntladeGebiet (GNR : CARDINAL) ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN ;
	END ;
	upAbmelden (geb) ;
END EntladeGebiet ;


PROCEDURE LadeGebiet (GNR : CARDINAL) : BOOLEAN ;
	VAR	geb :	pGebiet ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;
	IF NOT geb ^ .adressiert AND (geb ^ .AnfangsSeite <> 0) THEN
		RETURN AnmeldeGebiet (geb ^ .GebietsNummer, geb ^ .AnfangsSeite, '') ;
	END ;
	RETURN FALSE ;
END LadeGebiet ;


PROCEDURE VeraendereGebiet (GNR, neueLaenge : CARDINAL) : BOOLEAN ;
	VAR	geb :	pGebiet ;
BEGIN
	IF neueLaenge = 0 THEN		(* Länge 0 ist unzulässig *)
		RETURN FALSE ;
	END ;
	geb := upGetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;

	IF geb ^ .Laenge < neueLaenge THEN
		RETURN VerlaengereGebiet (geb, neueLaenge) ;
	ELSIF geb ^ .Laenge > neueLaenge THEN
		KuerzeGebiet (geb, neueLaenge) ;
	END ;
	RETURN TRUE ;
END VeraendereGebiet ;


PROCEDURE SetSchreibSperre (GNR : CARDINAL ; sperre : BOOLEAN) ;
	VAR	geb :	pGebiet ;
		i :	CARDINAL ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN ;
	END ;
	geb ^ .Schreibsperre := sperre ;

	IF geb ^ .adressiert THEN
		FOR i := 1 TO geb ^ .Laenge DO
			SetProtect (geb ^ .AnfangsSeite + (i-1) * (MaxHalbwortInSeite + 1), sperre) ;
		END ;
	END ;
END SetSchreibSperre ;


PROCEDURE GetAchtelseite (geb : pGebiet ; nr : CARDINAL) : pAchtelSeite ;
	VAR	ram :	pRAMelem ;
		i :	CARDINAL ;
BEGIN
	ram := geb ^ .RAM ;
	FOR i := 8 TO nr BY 8 DO
		ram := ram ^ .next ;
		IF ram = NIL THEN
			RETURN NIL ;
		END ;
	END ;
	RETURN ADR (ram ^ .Seite [nr MOD 8 * 128]) ;
END GetAchtelseite ;


PROCEDURE LiesGebiet (GNR, Achtelseite : CARDINAL ; VAR puffer : tAchtelSeite) : BOOLEAN ;
	VAR	geb :	pGebiet ;
		as :	pAchtelSeite ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;
	as := GetAchtelseite (geb, Achtelseite) ;
	IF as = NIL THEN
		RETURN FALSE ;
	END ;
	IF debug.Lesen THEN
		TraceF ("<- GEB%c[%'02h]", GNR, Achtelseite) ;
	END ;
	puffer := as ^ ;
	RETURN TRUE ;
END LiesGebiet ;


PROCEDURE SchreibGebiet (GNR, Achtelseite : CARDINAL ; VAR puffer : tAchtelSeite) : BOOLEAN ;
	VAR	geb :	pGebiet ;
		as :	pAchtelSeite ;
BEGIN
	geb := GetGebiet (GNR) ;
	IF geb = NIL THEN
		RETURN FALSE ;
	END ;
	IF geb ^ .Schreibsperre THEN
		RETURN FALSE ;
	END ;
	as := GetAchtelseite (geb, Achtelseite) ;
	IF as = NIL THEN
		RETURN FALSE ;
	END ;
	IF debug.Lesen THEN
		TraceF ("-> GEB%c[%'02h]", GNR, Achtelseite) ;
	END ;
	as ^ := puffer ;
	RETURN TRUE ;
END SchreibGebiet ;


PROCEDURE upShowGebiete ;
	VAR	geb,
		geb2 :	pGebiet ;
BEGIN
	WriteF ('\nGNR\tPGNM\tOGNM\tOLK\tTyp\tLNG\tAdr.') ;
	geb := Gebiet0 ;
	WHILE geb <> NIL DO
		WriteF ('\n%c\t%s\t%s\t%c', geb ^ .GebietsNummer, geb ^ .PGNM, geb ^ .OGNM, geb ^ .OLK) ;
		IF geb ^ .Stellvertreter THEN
			WriteF ('\tStv') ;
		ELSIF geb ^ .Dauergebiet THEN
			WriteF ('\tDgb') ;
		ELSE
			WriteF ('\tLgb') ;
		END ;
		IF geb ^ . Schreibsperre THEN
			WriteF ('SSP') ;
		END ;
		WriteF ('\t%cK', geb ^ .Laenge) ;
		IF geb ^ .adressiert THEN
			WriteF ("\t%'05h", geb ^ .AnfangsSeite) ;
		ELSE
			WriteF ('\t-') ;
		END ;
		geb := geb ^ .next ;
	END ;
END upShowGebiete ;


PROCEDURE Init ;
BEGIN
	Gebiet0 := NIL ;
	vorbes.TK := 3 ;
	vorbes.byte6 [0] := 0CAH ;
	vorbes.byte6 [1] := 0FFH ;
	vorbes.byte6 [2] := 0EEH ;
	vorbes.byte6 [3] := 0FAH ;
	vorbes.byte6 [4] := 0FFH ;
	vorbes.byte6 [5] := 0EEH ;
END Init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
END Gebiet.
