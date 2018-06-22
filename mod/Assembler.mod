
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Assembler;

(*	28.05.16	*)

IMPORT Strings, ExStrings, Conversions, Terminal, RunProg, RConversions ;
IMPORT FileIO, debug, FileFunc, Menue ;

FROM Trace IMPORT
	TraceF, TraceAktiv ;

FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM upGleitkomma IMPORT
	gkIntelToTR440runden,
	gkIntelToTR440 ;

FROM upFestkomma IMPORT
	trInt64to48, trInvert ;

FROM Register IMPORT * ;
FROM Struktur IMPORT * ;

FROM ZC1 IMPORT
	ANSItoZC1,
	ZC1toANSI ;

FROM ASCII IMPORT
	ht ;

%IF %NOT WEB %THEN

FROM Namen IMPORT * ;

FROM AsmDisasm IMPORT
	TRIM,
	EqualI,
	BefehlGueltig,
	GetBefehl,
	GetZusatzbits,
	GetZusatzbitsTest,
	ErsetzBefehle,
	RzweitCodeErlaubt,
	IndexBefehle,
	GrossSeitenRelativ,
	GrossSeite0Bezug,
	Adrteil1,
	Adrteil2,
	tOpcode,
	spezIR,
	spezLA,
	spezLR,
	spezLZL,
	spezRXMRX,
	spezSL,
	spezRRLR,
	spezRRLRbit21,
	spezRT,
	spezSH,
	spezSHB,
	spezSTSTN,
	spezTXRTRX,
	spezUS,
	spezZK,
	spezZK2,
	spezZTR,
	CardToHex2,
	CardToHex4,
	CardToHex12 ;


CONST
	tetrade =		'0123456789ABCDEF' ;


TYPE
	tZonenArt =		(ZoneUndef,
				 ZoneK, ZoneV, ZoneB, ZoneB1, ZoneD,
				 ZoneF,
				 dynZone1, dynZone2, dynZone3, dynZone4, dynZone5, dynZone6, dynZone7, dynZone8, dynZone9) ;

	tZonenArtDyn =		[dynZone1 .. dynZone9] ;

	tZahlart =		(zGleitkomma, zFestkomma, zHexa, zString, zUndef) ;


VAR	ziel :			FileIO.FILE ;

	AktName,
	AktSpezifikation,
	lastASMname,
	protname,
	zwZeile,
	protzeile,
	parm1,
	parm2,
	parm3,
	befehl :	       	ARRAY [0..255] OF CHAR ;

	WIEDwerte :		ARRAY [0..31], [0..31] OF CHAR ;

	ZonenNamen :		ARRAY tZonenArtDyn, [0..31] OF CHAR ;

	dynZonenAblage :	ARRAY tZonenArt OF tZonenArt ;

	prot :			FileIO.FILE ;

	initialisiert :		BOOLEAN = FALSE ;

	protausgabeAktiv,
	protausgabe :		BOOLEAN ;

	LaufEnde :		BOOLEAN ;

	NamenDefiniert :	BOOLEAN ;

	DeklarationenIgnorieren:BOOLEAN ;

	VorbesInhalt :		CARDINAL64 ;

	Zahlart :		tZahlart ;

	VorbesTK,
	DRUCK,
	REPLanzahl,
	MaxIndexPegel,
	IndexPegel,
	WIEDindex,
	ZeilenNummer,
	InitZonenAdresse,
	ZonenAdresse,
	merkCzonePegel,
	AnzFehlermeldungen,
	InformationsEinheiten :	CARDINAL ;

	DoppeltGenauRest :	CARDINAL64 ;

	STARRzone,
	AktZonenArt,
	AktCzoneArt :		tZonenArt ;

	InitZonenPegel,
	ZonenAnfang,
	ZonenPegel :		ARRAY tZonenArt OF CARDINAL ;

	ImpliziterIndexVerboten,		(* Strikt 1/3/4 *)
	AbsoluterIndexVerboten,			(* Strikt 2/3/4 *)
	AbsolutAdressteilVerboten,		(* Strikt 4 *)
	VORBESda,
	UnterdrueckeVerbotenerBezug,
	UnterdrueckeUnbekannt,
	ErsetzungVorhanden,
	FzoneAktiv,
	imLiteral,
	inMakroDefinition,
	inMakroAufruf,
	inWIEDausfuehrung,
	imWIED,
	inVersion,
	VersionBedingung,
	VersionAusgefuehrt,
	DoppeltGenau,
	LiteralErster :		BOOLEAN ;
	LiteralAdresse :	CARDINAL ;

	WIEDname :		pName = NIL ;

	STARRaktiv,
	SpezGerade,
	SpezUngerade,
	SpezHalbwort,
	SpezAdresse,
	SpezRechts,
	SpezLinks,
	SpezMarke,
	SpezAdressbit2,
	SpezFortsetzung,
	SpezZoneK,
	SpezZoneV,
	SpezZoneB,
	SpezZoneD :		BOOLEAN ;


%END


PROCEDURE enthalten (str, such : ARRAY OF CHAR) : BOOLEAN ;
	VAR	pos :	CARDINAL ;
		b :	BOOLEAN ;
BEGIN
	Strings.FindNext(such, str, 0, b, pos) ;
	IF b THEN
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END enthalten ;


PROCEDURE ziffer (ch : CHAR) : BOOLEAN [INLINE] ;
BEGIN
	RETURN (ch >= '0') AND (ch <= '9') ;
END ziffer ;


PROCEDURE Equal (str1, str2 : ARRAY OF CHAR) : BOOLEAN [INLINE] ;	(* funktioniert nur für Zeichenfolgen >= 4 Zeichen *)
	VAR	s1 [ALIAS str1] :	CARDINAL ;
		s2 [ALIAS str2] :	CARDINAL ;
BEGIN
	IF s1 = s2 THEN
		RETURN Strings.Equal (str1, str2) ;
	END ;
	RETURN FALSE ;
END Equal ;

%IF %NOT WEB %THEN


PROCEDURE GetZahl64 (str : ARRAY OF CHAR; VAR wert : CARDINAL64) : BOOLEAN ;
	VAR	i64 :		LONGINT ;
		c64a,
		c64b :		CARDINAL64 ;
		p :		pName ;
		i,
		zw,
		posOfPattern,
		l :		CARDINAL;
		izw :		INTEGER ;
		patternFound,
		fertig :	BOOLEAN ;

	PROCEDURE RealZahl () : BOOLEAN ;
		VAR	zust :	(anf, vzda, vorkomma, kommada, nachkomma, expoanf, expovzda, expo) ;
			j :	CARDINAL ;
	BEGIN
		zust := anf ;
		FOR j := 0 TO l DO
			CASE str [j] OF
			'+',
			'-' :		IF zust = anf THEN
						zust := vzda ;
					ELSIF zust = expoanf THEN
						zust := expovzda ;
					ELSE
						RETURN FALSE ;
					END ;
			|
			'0'..'9' :	IF zust < vorkomma THEN
						zust := vorkomma ;
					ELSIF zust = kommada THEN
						zust := nachkomma ;
					ELSIF (zust = expoanf) OR (zust = expovzda) THEN
						zust := expo ;
					END ;
			|
			' ' :		(* skip *)
			|
			'.' :		IF (zust = anf) OR (zust = vorkomma) THEN
						zust := kommada ;
					ELSE
						RETURN FALSE ;
					END ;
			|
			'E' :		IF (zust = vorkomma) OR (zust = nachkomma) THEN
						zust := expoanf ;
					ELSE
						RETURN FALSE ;
					END ;
			|
			'D' :
					IF (zust = vorkomma) OR (zust = nachkomma) THEN
						zust := expoanf ;
					ELSE
						RETURN FALSE ;
					END ;
					DoppeltGenau := TRUE ;

			ELSE
				RETURN FALSE ;
			END ;
		END ;
		IF (zust <> nachkomma) AND (zust <> expo) AND (zust <> expoanf) THEN
			RETURN FALSE ;
		END ;
		RETURN TRUE ;
	END RealZahl ;

	PROCEDURE GetRealZahl () : CARDINAL64 ;
		VAR	j,
			k,
			pos :	CARDINAL ;
			gw1 :	CARDINAL64 ;
			zahl :	LONGREAL ;
			str2 :	ARRAY [0..63] OF CHAR ;
			done :	BOOLEAN ;

		PROCEDURE putgk (ch : CHAR) ;
		BEGIN
			IF k <= HIGH (str2) THEN
				str2 [k] := ch ;
				INC (k) ;
			END ;
		END putgk ;

	BEGIN
		k := 0 ;
		FOR j := 0 TO l DO
			CASE str [j] OF
			'+',
			'-',
			'.',
			'E',
			'0'..'9' :	putgk (str [j]) ;
			|
			'D' :		putgk ('E') ;
			ELSE
			END ;
		END ;
		putgk (0C) ;
		pos := 0 ;
		RConversions.StringToReal (str2, pos, zahl, done);
		IF done THEN
			IF DoppeltGenau THEN
				IF gkIntelToTR440 (zahl, gw1, DoppeltGenauRest) THEN
					RETURN gw1 ;
				END ;
			ELSE
				IF gkIntelToTR440runden (zahl, gw1) THEN
					RETURN gw1 ;
				END ;
			END ;
		END ;
		SyntaxWarnung ('+++ unzul. Gleitkommazahl') ;
		RETURN 0 ;
	END GetRealZahl ;

BEGIN
	DoppeltGenau := FALSE ;

	TRIM (str) ;
	l := LENGTH (str) - 1 ;

	IF RealZahl () THEN
		wert := GetRealZahl () ;
		Zahlart := zGleitkomma ;
		RETURN TRUE ;
	END ;

	fertig := FALSE ;

	IF str [0] = '"' THEN
		(* String: keine Arithmetik prüfen *)
	ELSE
		Strings.FindNext ('-', str, 1, patternFound, posOfPattern) ;
		IF patternFound THEN
			Zahlart := zFestkomma ;
			IF GetZahl64 (str [0..posOfPattern-1], c64a) AND GetZahl64 (str [posOfPattern+1..l], c64b) THEN
				i64 := c64a - c64b ;
				fertig := TRUE ;
				Zahlart := zFestkomma ;
			ELSE
				RETURN FALSE ;
			END ;
		END ;
		IF NOT fertig THEN
			Strings.FindNext ('+', str, 1, patternFound, posOfPattern) ;
			IF patternFound THEN
				Zahlart := zFestkomma ;
				IF GetZahl64 (str [0..posOfPattern-1], c64a) AND GetZahl64 (str [posOfPattern+1..l], c64b) THEN
					i64 := c64a + c64b ;
					fertig := TRUE ;
					Zahlart := zFestkomma ;
				ELSE
					RETURN FALSE ;
				END ;
			END ;
		END ;
		(*
		IF NOT fertig THEN
			Strings.FindNext ('*', str, 1, patternFound, posOfPattern) ;
			IF patternFound THEN
				Zahlart := zFestkomma ;
				IF GetZahl64 (str [0..posOfPattern-1], c64a) AND GetZahl64 (str [posOfPattern+1..l], c64b) THEN
					i64 := c64a * c64b ;
					fertig := TRUE ;
					Zahlart := zFestkomma ;
				ELSE
					RETURN FALSE ;
				END ;
			END ;
		END ;
		*)
	END ;

	IF NOT fertig THEN
		CASE str [0] OF

		"'" :						(* Hexwert *)
			IF str [l] <> "'" THEN
				RETURN FALSE ;
			END ;
			Zahlart := zHexa ;
			IF NOT Conversions.StrBaseToLong (str [1..l-1], 16, wert) THEN
				RETURN FALSE ;
			ELSE
				RETURN TRUE ;
			END ;
		|
		'"' :						(* String *)
			Zahlart := zString ;
			IF (str [l] <> '"') OR (l=0) THEN
				RETURN FALSE ;
			END ;
			i64 := 0 ;
			FOR i := 1 TO l - 1 DO
				i64 := i64 * 256 + VAL(LONGINT, (ANSItoZC1 [str [i]])) ;
			END ;
		|
		'+' :						(* positive Zahl *)
			IF str [l] = 'R' THEN			(* relativ *)
				Zahlart := zHexa ;
				IF NOT Conversions.StrToCard (str [1..l-1], zw) THEN
					RETURN FALSE ;
				END ;
				i64 := VAL (INTEGER64, AblageAdresse + zw) ;
			ELSE
				Zahlart := zFestkomma ;
				IF NOT Conversions.StrToLong (str [1..l], i64) THEN
					RETURN FALSE ;
				END ;
			END ;
		|
		'-' :						(* negative Zahl *)
			IF str [l] = 'R' THEN			(* relativ *)
				Zahlart := zHexa ;
				IF NOT Conversions.StrToCard (str [1..l-1], zw) THEN
					RETURN FALSE ;
				END ;
				i64 := VAL (INTEGER64, AblageAdresse - zw) ;
			ELSE
				Zahlart := zFestkomma ;
				IF NOT Conversions.StrToLong (str[1..l], i64) THEN
					RETURN FALSE ;
				END ;
				i64 := trInvert (i64) ;		(* B-1 *)
			END ;
		|
		'A'..'Z',
		'*',
		'&',
		'_' :
			p := NamenSuchen (str) ;
			Zahlart := zFestkomma ;
			IF p = NIL THEN
				wert := 3EFEFEH ;
				IF imLauf2 THEN
					RETURN FALSE ;
				END ;
				i64 := wert ;
			ELSIF (p ^ .wert = 0) AND WarVerweis AND imLauf2 THEN
				IF NOT UnterdrueckeVerbotenerBezug THEN
					SyntaxWarnung ('Externbezug nicht gefunden') ;
				END ;
				i64 := 3EFEFEH ;
			ELSE
				i64 := VAL (LONGINT, p ^ .wert) ;
			END ;
		|
		'0'..'9' :
			IF str [l] = 'R' THEN			(* relativ *)
				Zahlart := zHexa ;
				IF NOT Conversions.StrToCard (str [0..l-1], zw) THEN
					RETURN FALSE ;
				END ;
				i64 := VAL (INTEGER64, AblageAdresse + zw) ;
			ELSE
				Zahlart := zFestkomma ;
				IF NOT Conversions.StrToLong (str, i64) THEN
					RETURN FALSE ;
				END ;
			END ;
		ELSE
			Zahlart := zHexa ;
			RETURN FALSE ;
		END ;
	END ;
	wert := VAL (CARDINAL64, i64) ;
	RETURN TRUE ;
END GetZahl64 ;


PROCEDURE GetZahl (str : ARRAY OF CHAR; VAR wert : CARDINAL) : BOOLEAN ;
	VAR	zw :	CARDINAL64 ;
BEGIN
	IF GetZahl64 (str, zw) THEN
		wert := VAL (CARDINAL, zw) ;
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END GetZahl ;


PROCEDURE CheckSpezifikation ;
	VAR	i,
		l,
		zahl :		CARDINAL ;
		str :		ARRAY [0..63] OF CHAR ;
		nurzahl :	BOOLEAN ;
		ch :		CHAR ;
BEGIN
	SpezGerade := FALSE ;
	SpezUngerade := FALSE ;
	SpezHalbwort := FALSE ;
	SpezAdresse := FALSE ;
	SpezZoneK := FALSE ;
	SpezZoneV := FALSE ;
	SpezZoneB := FALSE ;
	SpezZoneD := FALSE ;
	SpezLinks := FALSE ;
	SpezRechts := FALSE ;
	SpezMarke := FALSE ;
	SpezAdressbit2 := FALSE ;
	SpezFortsetzung := FALSE ;

	NeueTypenkennung := 4 ;

	l := LENGTH (AktSpezifikation) ;

	FOR i := 1 TO l DO
		CASE AktSpezifikation [i-1] OF
		'G' :	SpezGerade := TRUE ;
		|
		'U' :	SpezUngerade := TRUE ;
		|
		'H' :	SpezHalbwort := TRUE ;
		|
		'A' :	SpezAdresse := TRUE ;
		|
		'K' :	SpezZoneK := TRUE ;
		|
		'V' :	SpezZoneV := TRUE ;
		|
		'B' :	SpezZoneB := TRUE ;
		|
		'D' :	SpezZoneD := TRUE ;
(*
		|
		'F' :	SpezFortsetzung := TRUE ;
*)
		|
		'L' :	SpezLinks := TRUE ;
		|
		'R' :	SpezRechts := TRUE ;
		|
		'M' :	SpezMarke := TRUE ;
		|
		'N' :	SpezAdressbit2 := TRUE ;
		|
		'0' :	NeueTypenkennung := 0 ;
		|
		'1' :	NeueTypenkennung := 1 ;
		|
		'2' :	NeueTypenkennung := 2 ;
		|
		'3' :	NeueTypenkennung := 3 ;
		|
		1C .. ' ' :	(* ignorieren *)
		ELSE
			str := 'unbekannte Spezifikation : ' ;
			Strings.Append (AktSpezifikation [0..3], str) ;
			SyntaxWarnung (str) ;
			BREAK ;
		END ;
	END ;
END CheckSpezifikation ;


PROCEDURE upAblageAdresse (typ : tZonenArt) ;
	VAR	neuzone :	tZonenArt ;
BEGIN
	neuzone := typ ;
	IF imLiteral AND (typ = ZoneB) THEN
		neuzone := ZoneB1 ;
	END ;
	IF FzoneAktiv AND (typ = ZoneD) THEN
		neuzone := ZoneF ;
	END ;
	IF dynZonenAblage [neuzone] <> ZoneUndef THEN
		neuzone := dynZonenAblage [neuzone] ;
	END ;
	AktZonenArt := neuzone ;
	AblageAdresse := ZonenPegel [AktZonenArt] ;
END upAblageAdresse ;


PROCEDURE GetAblageAdresse (default : tZonenArt) ;
BEGIN
	IF SpezZoneK THEN
		upAblageAdresse (ZoneK) ;
	ELSIF SpezZoneV THEN
		upAblageAdresse (ZoneV) ;
	ELSIF SpezZoneB THEN
		upAblageAdresse (ZoneB) ;
	ELSIF SpezZoneD THEN
		upAblageAdresse (ZoneD) ;
	ELSIF STARRaktiv THEN
		upAblageAdresse (STARRzone) ;
	ELSIF (default = ZoneB)
	  AND (SpezAdresse OR SpezHalbwort OR SpezLinks OR SpezRechts OR SpezMarke OR SpezAdressbit2 OR (NeueTypenkennung < 4)) THEN
		upAblageAdresse (ZoneK) ;
	ELSE
		upAblageAdresse (default) ;
	END ;
END GetAblageAdresse ;



PROCEDURE protstuelp ;
BEGIN
	IF protausgabe AND imLauf2 THEN
		IF protzeile [0] <> 0C THEN
			FileIO.WriteF (prot, "\t\t%s", protzeile) ;
			protzeile [0] := 0C ;
			FileIO.WriteLn (prot, '') ;
		END ;
	END ;
END protstuelp ;


PROCEDURE protstuelpF ;
BEGIN
	IF protausgabe AND imLauf2 THEN
		IF protzeile [0] <> 0C THEN
			FileIO.WriteF (prot, "\t\t%s", protzeile) ;
			protzeile [0] := 0C ;
		END ;
		FileIO.WriteF (prot, "\n\t", '') ;
	END ;
END protstuelpF ;


PROCEDURE protstuelpFehl ;
	VAR	i :	CARDINAL ;
BEGIN
	IF protausgabe AND imLauf2 THEN
		IF protzeile [0] <> 0C THEN
			i := 1 ;
			WHILE (i < 4) AND (protzeile [i] = CHR(9)) DO
				protzeile [i] := ' ' ;
				i := i + 1 ;
			END ;
			FileIO.WriteF (prot, "%s", protzeile) ;
			protzeile [0] := 0C ;
		END ;
		FileIO.WriteF (prot, "\n\t", '') ;
	END ;
END protstuelpFehl ;


PROCEDURE protstuelpAbschluss ;
BEGIN
	IF protausgabe AND imLauf2 THEN
		IF protzeile [0] <> 0C THEN
			FileIO.WriteF (prot, "\t\t%s", protzeile) ;
			protzeile [0] := 0C ;
		END ;
		FileIO.WriteLn (prot, '') ;
	END ;
END protstuelpAbschluss ;


PROCEDURE ProtQuellZeile (str : ARRAY OF CHAR) ;
BEGIN
	Terminal.WriteString (' in ') ;
	Terminal.WriteString (AktQuellName) ;
	Terminal.WriteString (', Zeile ') ;
	Terminal.WriteCard (ZeilenNummer) ;
	Terminal.WriteString (' :') ;
	Terminal.WriteString (ht) ;
	Terminal.WriteString (AktZeile) ;
	Terminal.WriteLn ;
	IF str [0] <> 0C THEN
		Terminal.WriteString ('    ') ;
		Terminal.WriteString (str) ;
		Terminal.WriteLn ;
	END ;
END ProtQuellZeile ;

PROCEDURE FehlerMeldung (str1, str2 : ARRAY OF CHAR) ;
BEGIN
	INC (AnzFehlermeldungen) ;
	Terminal.WriteLn ;
	Terminal.WriteString ('+++ ') ;
	Terminal.WriteString (str1) ;
	ProtQuellZeile (str2) ;
	IF protausgabe THEN
		FileIO.WriteF (prot, "\n+++ %s (%s) : %s ", str1, AktQuellName, str2) ;
		protstuelpFehl ;
	END ;
END FehlerMeldung ;


PROCEDURE SyntaxFehler (str : ARRAY OF CHAR) ;
BEGIN
	FehlerMeldung ('Syntaxfehler', str) ;
END SyntaxFehler ;


PROCEDURE SyntaxWarnung (str : ARRAY OF CHAR) ;
BEGIN
	FehlerMeldung ('Warnung', str) ;
END SyntaxWarnung ;


PROCEDURE DateiFehlt ;
BEGIN
	FehlerMeldung ('Datei nicht gefunden', lastDateiname) ;
END DateiFehlt ;


PROCEDURE GetZonenArt (bez : ARRAY OF CHAR) : tZonenArt ;
BEGIN
	CASE bez [0] OF
	'K' :
		RETURN ZoneK ;
	|
	'V' :
		RETURN ZoneV ;
	|
	'B' :
		RETURN ZoneB ;
	|
	'D' :
		RETURN ZoneD ;
	ELSE
		SyntaxFehler ('Zonentyp K/V/B/D erwartet') ;
		RETURN ZoneV ;
	END ;
END GetZonenArt ;


PROCEDURE BefehlVermutet (befehl : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i,
		l :	CARDINAL ;
		ch :	CHAR ;
BEGIN
	CASE befehl [0] OF
	'A'..'Z' :						(* damit könnte ein Befehl anfangen *)
			IF AktSpezifikation [0] > ' ' THEN	(* Befehle haben fast nie Spezifikationen *)
				RETURN FALSE ;
			END ;
			l := LENGTH (befehl) ;
			FOR i := 2 TO l DO
				CASE befehl [i-1] OF
				'0'..'9',
				'*',
				'_',
				'&',
				'A'..'Z' :
				ELSE
					RETURN FALSE ;		(* seltsames Zeichen für einen Befehl *)
				END ;
			END ;
			RETURN TRUE ;				(* das wird wohl ein fehlerhafter Befehl sein *)
	ELSE
	END ;
	RETURN FALSE ;
END BefehlVermutet ;


PROCEDURE CheckAbsoluterIndex (parm : ARRAY OF CHAR ; wert : INTEGER) ;
BEGIN
	IF imLauf2 THEN
		IF AbsoluterIndexVerboten AND (parm [0] < 'A') THEN
			SyntaxWarnung ('absoluter Index') ;
		ELSIF (wert < 0) OR (wert > 255) THEN
			SyntaxWarnung ('Index erwartet') ;
		END ;
	END ;
END CheckAbsoluterIndex ;



PROCEDURE BeginVersion ;
	VAR	ind :		CARDINAL ;
		nam,
		wert,
		aktwert :	ARRAY [0..63] OF CHAR ;
BEGIN
	IF parm1 [0] = '(' THEN								(* vers (pp=xyz) *)
		ind := 1 ;
		IF ExStrings.GetNextItem (parm1, ind, nam, '=') THEN
			IF nam [LENGTH(nam)-1] = ')' THEN
				nam [LENGTH(nam)-1]:= 0C ;
				inVersion := TRUE ;
				GetErsetzungsInhalt (nam, aktwert) ;
				IF aktwert [0] <> 0C THEN
					VersionBedingung := TRUE ;
					VersionAusgefuehrt := TRUE ;
				ELSE
					VersionBedingung := FALSE ;
				END ;
			ELSIF ExStrings.GetNextItem (parm1, ind, wert, ')') THEN
				inVersion := TRUE ;
				GetErsetzungsInhalt (nam, aktwert) ;
				IF Strings.Equal (wert, aktwert) THEN
					VersionBedingung := TRUE ;
					VersionAusgefuehrt := TRUE ;
				ELSE
					VersionBedingung := FALSE ;
				END ;
			END ;
		END ;
	END ;
END BeginVersion ;


PROCEDURE GetEinzelBefehl (zeile : ARRAY OF CHAR) : BOOLEAN ;
	VAR
		wert64 :	CARDINAL64 ;
		merkAktWB,
		merkAktSegmWB,
		merkAktQuelle,
		wbVerweis :	pName ;
		merkZeilenNummer,
		wert :		CARDINAL ;
		str :		ARRAY [0..63] OF CHAR ;
		halbwort,
		Index,
		posOfPattern,
		wort,
		i, j,
		l, lng,
		p1,
		p2 :		CARDINAL ;
		p :		pName ;
		adrteil :	CARDINAL16 ;
		befehlscode :	tOpcode ;
		neueTK :	CARDINAL ;
		REPLzone :	tZonenArt ;
		ch :		CHAR ;
		BitwertDa,
		fertig,
		patternFound :	BOOLEAN ;
		merkAktQuellName : ARRAY [0..63] OF CHAR ;


	PROCEDURE ImpliziterIndex (parm : ARRAY OF CHAR ; VAR ind : CARDINAL) : BOOLEAN ;
		VAR	p :	pName ;
	BEGIN
		IF befehlscode IN IndexBefehle THEN
			IF (parm[0] >= 'A') AND (parm [0] <= 'Z') THEN		(* könnte ein Name sein *)
				p := NamenEintragen (parm) ;
				IF ImpliziterIndexVerboten OR (p ^ .wert >= 254) THEN
					SyntaxWarnung ('impliziter Index') ;
				END ;
				p ^ .wert := IndexPegel ;
				IF IndexPegel > MaxIndexPegel THEN
					MaxIndexPegel := IndexPegel ;
				END ;
				ind := IndexPegel ;
				INC (IndexPegel) ;
				IF debug.AssemblerProtokoll THEN
					TraceF ("%s = %'02h", parm, ind) ;
				END ;
				RETURN TRUE ;
			ELSE
				CheckAbsoluterIndex (parm, 0) ;
			END ;
		END ;
		ind := 0 ;
		RETURN FALSE ;
	END ImpliziterIndex ;


	PROCEDURE upGetBefehlsZeile (zeile : ARRAY OF CHAR) : BOOLEAN ;
		VAR	i,
			l,
			Index,
			AktInd,
			posOfPattern,
			anz,
			anf,
			anfkl,
			groesse :	CARDINAL ;
			globalWB :	pName ;
			rm,
			globalName,
			StringDa,
			patternFound :	BOOLEAN ;
			str :		ARRAY [0..127] OF CHAR ;

		PROCEDURE upASPbefehl (VAR INOUT ablageadr : CARDINAL) : BOOLEAN ;
		BEGIN
			Index := 0 ;
			Strings.FindNext('K', parm1, Index, patternFound, posOfPattern);
			IF patternFound THEN						(* Seiten-Spezifikation nK / nK2 / NK4 / nK8 *)
				IF parm1 [0] = 'K' THEN
					anz := 0 ;
				ELSE
					IF NOT GetZahl (parm1 [0..posOfPattern-1], anz) THEN
						SyntaxWarnung ('unzul. ASP-Anzahlangabe') ;
						RETURN FALSE ;
					END ;
				END ;
				IF parm1 [posOfPattern+1] > ' ' THEN
					IF NOT GetZahl (parm1 [posOfPattern+1..LENGTH(parm1)], groesse) THEN
						SyntaxWarnung ('unzul. ASP-Seitengrößen-Angabe') ;
						RETURN FALSE ;
					END ;
					groesse := 2048 DIV groesse ;
				ELSE
					groesse := 2048 ;
				END ;
				IF (ablageadr BAND (groesse-1)) <> 0 THEN		(* AblageAdresse updaten *)
					ablageadr := ablageadr - (ablageadr BAND (groesse-1)) + groesse ;
				END ;
				AblageLaenge :=  anz * groesse ;
			ELSE
				IF SpezGerade AND ODD (ablageadr)
				OR SpezUngerade AND NOT ODD (ablageadr) THEN
					INC (ablageadr) ;
				END ;
				IF NOT GetZahl (parm1, groesse) THEN
					SyntaxWarnung ('unzul. ASP-Längenangabe') ;
					RETURN FALSE ;
				END ;
				AblageLaenge :=  groesse ;
			END ;
			befehl := '' ;
			RETURN TRUE ;
		END upASPbefehl ;

		PROCEDURE VarFestlegen () : BOOLEAN ;
		BEGIN
			IF NOT upASPbefehl (AblageAdresse) THEN
				RETURN FALSE ;
			END ;

			IF p <> NIL THEN
				p ^ .wert := AblageAdresse ;

				IF debug.AssemblerProtokoll THEN
					TraceF ("%s : %'04h", p ^ .name, AblageAdresse) ;
				END ;
			END ;
			IF protausgabe THEN
				FileIO.WriteF (prot, "%'05h\t", AblageAdresse) ;
			END ;

			AblageAdresse := AblageAdresse + AblageLaenge ;
			ZonenPegel [AktZonenArt] := AblageAdresse ;

			p := NIL ;
			AblageLaenge := 0 ;
			RETURN TRUE ;
		END VarFestlegen ;

		PROCEDURE ASPbefehl () : BOOLEAN ;
		BEGIN
			GetAblageAdresse (ZoneV) ;
			RETURN VarFestlegen () ;
		END ASPbefehl ;

		PROCEDURE DSPbefehl () : BOOLEAN ;
		BEGIN
			GetAblageAdresse (ZoneD) ;
			RETURN VarFestlegen () ;
		END DSPbefehl ;

		PROCEDURE ZonenReservierung (typ : tZonenArt) : BOOLEAN ;				(* Spezialversion für ZONE_x = DSP ... *)
			VAR	ablageadr :	CARDINAL ;
		BEGIN
			IF NOT upASPbefehl (ZonenAdresse) THEN
				RETURN FALSE ;
			END ;

			p ^ .wert := ZonenAdresse ;

			ablageadr := ZonenAdresse ;
			ZonenAdresse := ZonenAdresse + AblageLaenge ;
			ZonenPegel [typ] := ablageadr ;
			ZonenAnfang [typ] := ablageadr ;

			IF debug.AssemblerProtokoll THEN
				TraceF ("%s ab %'04h", p ^ .name, ablageadr) ;
			END ;

			IF protausgabe THEN
				FileIO.WriteF (prot, "%'05h\t", ablageadr) ;
			END ;

			p := NIL ;
			AblageLaenge := 0 ;
			RETURN TRUE ;
		END ZonenReservierung ;


		PROCEDURE dynZonenReservierung (zonnam : ARRAY OF CHAR) : BOOLEAN ;			(* Spezialversion für ZONE__Zonenname = DSP ... *)
			VAR	ablageadr :	CARDINAL ;
				z :		tZonenArt ;
		BEGIN
			FOR z := MIN (tZonenArtDyn) TO MAX (tZonenArtDyn) DO
				IF ZonenNamen [z, 0] = 0C THEN		(* freies Element *)
					ZonenNamen [z] := zonnam ;
					RETURN ZonenReservierung (z) ;
				END ;
			END ;
			RETURN FALSE ;
		END dynZonenReservierung ;


		PROCEDURE Bitfolge ;
			VAR	wort,
				wert :	CARDINAL64 ;
				ind,
				ind2,
				anz :	CARDINAL ;
				zw,
				elem,
				lng :	ARRAY [0..63] OF CHAR ;
				bf :	ARRAY [0..255] OF CHAR ;
		BEGIN
			Strings.FindNext (')', zeile, Index+1, patternFound, posOfPattern) ;
			IF NOT patternFound THEN
				RETURN ;				(* keine KlammerZu gefunden *)
			END ;
			bf := zeile [Index+1 .. posOfPattern-1] ;	(* Inhalt der Bitfolge-Konstante *)
			befehl := '' ;

			wort := 0 ;
			ind := 0 ;
			LOOP
				IF ExStrings.GetNextItem (bf, ind, zw, ',') THEN				(* Bit-Ausdruck gefunden *)
					ind2 := 0 ;
					IF ExStrings.GetNextItem (zw, ind2, elem, '/') THEN			(* Längenangabe dahinter *)
						IF ExStrings.GetNextItem (zw, ind2, lng, ',') THEN			(* Längenangabe dahinter *)
							IF GetZahl64 (elem, wert) THEN					(* korrekter Bitwert *)
								IF GetZahl (lng, anz) THEN				(* korrekte Längenangabe *)
									wert := wert BAND Cardinal48Bit ;
									IF (wert SHR anz) <> 0 THEN
										IF imLauf2 THEN
											SyntaxWarnung ('Bitfolge-Element zu gross') ;
										END ;
										wert := Cardinal64Bit SHR (64-anz) ;	(* 'FFF' draus machen *)
									END ;
									wort := (wort SHL anz) BOR wert ;
								ELSE			(* Länge ungültig *)
									IF imLauf2 THEN
										SyntaxWarnung ('Bitfolge-Element hat unzul. Länge') ;
									END ;
									EXIT ;
								END ;
							ELSE
											(* Bitfeld-Wert ungültig *)
								IF NOT UnterdrueckeUnbekannt THEN
									IF imLauf2 THEN
										SyntaxWarnung ('Bitfolge-Element unzulässig') ;
									END ;
								END ;
								IF GetZahl (lng, anz) THEN				(* korrekte Längenangabe *)
									wort := (wort SHL anz) ;
								ELSE			(* Länge ungültig *)
									EXIT ;
								END ;
							END ;
						ELSE
							EXIT ;			(* keine Längenangabe da *)
						END ;
					ELSE
						EXIT ;				(* komisch *)
					END ;
				ELSE
					EXIT ;						(* alle Bitfelder bearbeitet *)
				END ;
			END ;
			IF wort > 0FFFFFFFFH THEN
				CardToHex12 (wort, zw) ;
			ELSE
				CardToHex2 (wort, zw) ;
			END ;
			befehl := "'" ;
			Strings.Append (zw, befehl) ;
			Strings.Append ("'", befehl) ;
			BitwertDa := TRUE ;
			Index := posOfPattern+1 ;
		END Bitfolge ;

		PROCEDURE REPLausfuehren ;
			VAR	IEmerk,
				anz :	CARDINAL ;
				merk :	BOOLEAN ;
				zeile :	ARRAY [0..255] OF CHAR ;
		BEGIN
			anz := REPLanzahl ;
			REPLanzahl := 0 ;
			DeklarationenIgnorieren := TRUE ;
			merk := protausgabe ;
			protausgabe := FALSE ;
			IEmerk := InformationsEinheiten ;
			WHILE anz > 1 DO		(* das erstemal wurde schon beim Aufzeichnen ausgeführt *)
				DEC (anz) ;
				GetFirstREPLzeile (zeile) ;
				REPEAT
					GetBefehlsZeile (zeile) ;
				UNTIL NOT GetNextREPLzeile (zeile) ;
			END ;
			protausgabe := merk ;
			InformationsEinheiten := IEmerk ;
			DeklarationenIgnorieren := FALSE ;
			REPLloeschen ;
		END REPLausfuehren ;

		PROCEDURE WIEDausfuehren ;
			VAR	IEmerk,
				anz :	CARDINAL ;
				zeile :	ARRAY [0..255] OF CHAR ;
		BEGIN
			protstuelpAbschluss ;
			imWIED := FALSE ;
			WIEDindex := 1 ;
			WHILE WIEDwerte [WIEDindex, 0] <> 0C DO		(* nacheinander alle WIED-Werte als Ersetzung definieren *)
				PutErsetzungsInhalt (WIEDname, WIEDwerte [WIEDindex]) ;
				IF debug.AssemblerProtokoll THEN
					TraceF ('WIED %s -> %s', WIEDname ^ .name, WIEDwerte [WIEDindex]) ;
				END ;
				GetFirstWIEDzeile (zeile) ;
				REPEAT
					IF debug.AssemblerProtokoll THEN
						TraceF ('WIED : %s', zeile) ;
					END ;
					inWIEDausfuehrung := TRUE ;
					IF NOT GetBefehlsZeile (zeile) THEN
						BREAK ;
					END ;
				UNTIL NOT GetNextWIEDzeile (zeile) ;
				INC (WIEDindex) ;
			END ;
			inWIEDausfuehrung := FALSE ;
			WIEDloeschen ;
		END WIEDausfuehren ;

		PROCEDURE NeuesSEGM ;
			VAR	zwnam :	ARRAY [0..63] OF CHAR ;
		BEGIN
			zwnam := '*SEGM-' ;
			IF p <> NIL THEN
				Strings.Append (p ^ .name [0..LENGTH(p ^ .name)], zwnam) ;
				AktSegmWB := WoerterbuchEintragen (zwnam) ;
			END ;
		END NeuesSEGM ;

		PROCEDURE NeueCZONE ;
			VAR	zone :	tZonenArt ;
		BEGIN
			IF p <> NIL THEN
				zone := GetZonenArt (parm1) ;
				p := CZONEeintragen (p ^ .name [0..LENGTH(p ^ .name)]) ;

				IF NeuAngelegt THEN
					p ^ .wert := ZonenPegel [zone] ;			(* aktuellen Pegel als CZONE-Anfang merken *)
				ELSE
					AktCzoneArt := zone ;
					merkCzonePegel := ZonenPegel [zone] ;
					AblageAdresse := p ^ .wert ;				(* Pegel auf CZONE-Anfang zurücksetzen *)
					AktZonenArt := zone ;
				END ;
			END ;
		END NeueCZONE ;

		PROCEDURE AblageFehler ;
		BEGIN
			SyntaxFehler ('Ablagezone K0/V0/B0/D0 erwartet') ;
		END AblageFehler ;

		PROCEDURE CheckDynAblage ;
			VAR	z :	tZonenArt ;
		BEGIN
			FOR z := MIN (tZonenArtDyn) TO MAX (tZonenArtDyn) DO
				IF Strings.Equal (ZonenNamen [z], parm1) THEN			(* es ist eine dynamisch deklarierte Zone *)

					l := LENGTH(parm2)-1 ;
					IF parm2 [l] = ')' THEN
						parm2 [l] := ' ' ;
					END ;

					REPEAT
						TRIM (parm2) ;
						l := LENGTH(parm2) ;
						IF parm2 [0] = '(' THEN
							parm2 [0] := ' ' ;
							TRIM (parm2) ;
						END ;
						IF parm2 [l-1] = ')' THEN
							parm2 [l-1] := ' ' ;
							TRIM (parm2) ;
						END ;
						IF parm2 [1] = '0' THEN
							CASE parm2 [0] OF
							'K' :	dynZonenAblage [ZoneK] := z ;
							|
							'V' :	dynZonenAblage [ZoneV] := z ;
							|
							'B' :	dynZonenAblage [ZoneB] := z ;
							|
							'D' :	dynZonenAblage [ZoneD] := z ;
							ELSE
								AblageFehler ;
								RETURN ;
							END ;
						ELSIF parm2 [1] = '1' THEN
							CASE parm2 [0] OF
							'B' :	dynZonenAblage [ZoneB1] := z ;
							ELSE
								AblageFehler ;
								RETURN ;
							END ;
						ELSE
							AblageFehler ;
							RETURN ;
						END ;
					UNTIL NOT ExStrings.GetNextItem (zeile, Index, parm2, ',') ;
				END ;
			END ;
		END CheckDynAblage ;

		PROCEDURE EndeDynZonenAblage ;
			VAR	z :	tZonenArt ;
		BEGIN
			FOR z := MIN (tZonenArt) TO MAX (tZonenArt) DO
				dynZonenAblage [z] := ZoneUndef ;
			END ;
		END EndeDynZonenAblage ;

		PROCEDURE NamenMerken (VAR AktName : ARRAY OF CHAR) ;
		BEGIN
			TRIM (AktName) ;
			l := LENGTH (AktName) ;
			IF AktName [l-1] = '.' THEN						(* Kennzeichnung Globalname ignorieren *)
				globalName := TRUE ;
				AktName [l-1] := 0C ;
				TRIM (AktName) ;
			ELSE
				globalName := FALSE ;
			END ;
			IF NOT DeklarationenIgnorieren (* im REPL *) THEN
				IF globalName THEN
					p := NamenEintragen (AktName) ;
				ELSIF AktSegmWB <> NIL THEN
					globalWB := AktWB ;
					AktWB := AktSegmWB ;
					p := NamenEintragen (AktName) ;
					AktWB := globalWB ;
				ELSE
					p := NamenEintragen (AktName) ;
				END ;
			ELSE
				AktName := '' ;
			END ;
		END NamenMerken ;

		PROCEDURE PseudoGebiet (geladen : BOOLEAN) ;
			VAR	ONAME :	ARRAY [0..7] OF CHAR ;
				AnfAdr,
				lng :	CARDINAL ;
		BEGIN
			p := NIL ;
			AblageLaenge := 0 ;
			befehl := '' ;

			IF imLauf2 THEN
				ONAME := parm1 ;

				IF parm2 [0] = '(' THEN
					parm2 [0] := ' ' ;
				END ;
				TRIM (parm2) ;
				parm1 := parm2 ;
				IF ExStrings.GetNextItem (zeile, Index, parm2, ')') THEN
					TRIM (parm2) ;
					IF PseudoGebiete [0] > ' ' THEN
						Strings.Append (';', PseudoGebiete) ;
					END ;
					Strings.Append (ONAME, PseudoGebiete) ;		(* ONAME *)
					Strings.Append (':', PseudoGebiete) ;
					GetZahl (parm1, AnfAdr) ;
					Conversions.CardBaseToStr (AnfAdr, 16, parm1) ;
					Strings.Append (parm1, PseudoGebiete) ;		(* Anf.Adr. *)
					Strings.Append (':', PseudoGebiete ) ;
					Strings.Append (parm2, PseudoGebiete) ;		(* LNG *)
					Strings.Append (':', PseudoGebiete ) ;
					IF geladen THEN
						Strings.Append ('1', PseudoGebiete ) ;
					ELSE
						Strings.Append ('0', PseudoGebiete ) ;
					END ;
				END ;
			END ;
			p := NIL ;
		END PseudoGebiet ;


	BEGIN	(* upGetBefehlsZeile *)

		AblageLaenge := 0 ;								(* falls gar kein Befehl gefunden wird *)

		Index := 0 ;
		p := NIL ;

		IF zeile [0] <= ' ' THEN
			befehl := '' ;
			(*
			protstuelpAbschluss ;
			*)
			RETURN TRUE ;								(* Leerzeile *)
		END ;

		AblageLaenge := 1 ;								(* meistens : Befehl 1 Halbwort lang *)

(*
		Strings.FindNext ('"', zeile, 0, patternFound, posOfPattern) ;
		IF patternFound THEN
			anf := posOfPattern ;
		ELSE
			anf := 3FFFH ;
		END ;
*)

		Strings.FindNext ('(', zeile, 0, patternFound, posOfPattern) ;
		IF patternFound THEN
			anfkl := posOfPattern ;
		ELSE
			anfkl := 3FFFH ;
		END ;

		Strings.FindNext('=', zeile, Index, patternFound, posOfPattern);
		IF patternFound (* AND (posOfPattern < anf)
				*) AND (posOfPattern < anfkl) THEN				(* '=' nicht in Klammer, also Name vor Befehl *)
			AktName := zeile [0..posOfPattern - 1] ;
			Index := posOfPattern + 1 ;
			NamenMerken (AktName) ;
		ELSE
			AktName := '' ;
		END ;
		WHILE zeile [Index] = ' ' DO
			INC (Index) ;
		END ;

		StringDa := FALSE ;
		BitwertDa := FALSE ;

		IF zeile [Index] = '"' THEN							(* String-Sonderbehandlung *)
			anf := Index ;
			Strings.FindNext ('"', zeile, anf+1, patternFound, posOfPattern) ;
			IF patternFound THEN
				StringDa := TRUE ;
				befehl := zeile [anf .. posOfPattern] ;
				Index := posOfPattern + 1 ;
				WHILE zeile [Index] = ' ' DO
					INC (Index) ;
				END ;
			END ;
		ELSIF zeile [Index] = '(' THEN							(* Bitfolgen-Konstante *)
			Bitfolge ;
			WHILE zeile [Index] = ' ' DO
				INC (Index) ;
			END ;
		END ;

		Strings.FindNext ('/', zeile, Index, patternFound, posOfPattern) ;
		IF patternFound THEN								(* Spezifikation abspalten *)
			AktSpezifikation := zeile [posOfPattern + 1 .. LENGTH(zeile)] ;
			TRIM (AktSpezifikation) ;
			zeile [posOfPattern] := 0C ;
			TRIM (zeile) ;

			CheckSpezifikation ;

			GetAblageAdresse (ZoneB) ;

			IF SpezGerade AND ODD (AblageAdresse) THEN
				IF imLauf2 THEN
					PutCodeWort (AblageAdresse, 0) ;
				END ;
				INC (AblageAdresse) ;						(* gerade AblageAdresse erzeugen *)
				ZonenPegel [AktZonenArt] := AblageAdresse ;

			ELSIF SpezUngerade AND NOT ODD(AblageAdresse) THEN
				IF imLauf2 THEN
					PutCodeWort (AblageAdresse, 0) ;
				END ;
				INC (AblageAdresse) ;						(* ungerade AblageAdresse erzeugen *)
				ZonenPegel [AktZonenArt] := AblageAdresse ;
			END ;
		ELSE
			AktSpezifikation := '' ;

			CheckSpezifikation ;

			GetAblageAdresse (ZoneB) ;

		END ;

		IF StringDa OR BitwertDa THEN
			parm1 := '' ;
			parm2 := '' ;
			RETURN TRUE ;
		END ;

		IF ExStrings.GetNextItem (zeile, Index, befehl, ' ') THEN			(* Befehl gefunden *)
			WHILE zeile [Index] = ' ' DO
				INC (Index) ;
			END ;
			parm1 := '' ;
			IF zeile [Index] = '"' THEN						(* String-Parameter *)
				Strings.FindNext ('"', zeile, Index+1, patternFound, posOfPattern) ;
				IF patternFound THEN
					parm1 := zeile [Index.. posOfPattern] ;
					Index := posOfPattern + 1 ;
					WHILE zeile [Index] = ' ' DO
						INC (Index) ;
					END ;
				END ;
			END ;
			parm2 := '' ;
			IF zeile [Index] = 0C THEN
				(* nix mehr nach dem 1. Parameter *)
			ELSIF ExStrings.GetNextItem (zeile, Index, parm1, ' ,') THEN		(* 1. Parameter *)
				WHILE zeile [Index] = ' ' DO
					INC (Index) ;
				END ;
				IF ExStrings.GetNextItem (zeile, Index, parm2, ' ,') THEN	(* 2. Parameter *)
					TRIM (parm2) ;
				ELSE
					parm2 := '' ;
				END ;
			ELSE
				parm1 := '' ;
			END ;

			IF Equal (befehl, 'ASP') THEN
				befehl := '' ;
				IF NOT ASPbefehl () THEN
					RETURN FALSE ;
				END ;

			ELSIF Equal (befehl, 'DSP')THEN
				befehl := '' ;
				IF p <> NIL THEN						(* Spezialnamen für DSP-Befehl checken *)
					IF (LENGTH (p ^ .name) > 5) AND Strings.Equal (p ^ .name [0..4], 'ZONE_') THEN
						str := p ^ .name [5..LENGTH(p ^ .name)] ;
						   IF Strings.Equal (str, 'K') THEN					(*	ZONE_K		*)
							RETURN ZonenReservierung (ZoneK) ;
						ELSIF Strings.Equal (str, 'V') THEN					(*	ZONE_V		*)
							RETURN ZonenReservierung (ZoneV) ;
						ELSIF Strings.Equal (str, 'B1') THEN					(*	ZONE_B1		*)
							RETURN ZonenReservierung (ZoneB1) ;
						ELSIF Strings.Equal (str, 'B') THEN					(*	ZONE_B		*)
							IF ZonenReservierung (ZoneB) THEN
								AblageAdresse := ZonenPegel [ZoneB] ;
								RETURN TRUE ;
							END ;
						ELSIF Strings.Equal (str, 'D') THEN					(*	ZONE_D		*)
							RETURN ZonenReservierung (ZoneD) ;
						ELSIF Strings.Equal (str, 'F') THEN					(*	ZONE_F		*)
							RETURN ZonenReservierung (ZoneF) ;
						ELSIF str [0] = '_' THEN						(*	ZONE__Zonenname	*)
							RETURN dynZonenReservierung (str [1..HIGH(str)]) ;
						END ;
					END ;
				END ;
				IF NOT DSPbefehl () THEN
					RETURN FALSE ;
				END ;

			ELSIF Equal (befehl, 'GLCH') THEN
				IF GetZahl (parm1, p1) AND (p <> NIL) THEN
					p ^ .wert := p1 ;
				END ;
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF protausgabe THEN
					FileIO.WriteF (prot, "\t\t%'06h", p1) ;
				END ;

			ELSIF Equal (befehl, 'STARR')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				STARRzone := GetZonenArt (parm1) ;
				STARRaktiv := TRUE ;

			ELSIF Equal (befehl, 'REPL')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				DeklarationenIgnorieren := FALSE ;
				REPLloeschen ;					(* keine geschachtelten REPLs *)
				befehl := '' ;
				REPLzone := GetZonenArt (parm1) ;

				IF parm2 [0] = '(' THEN								(* repl zone (anzahl) *)
					l := LENGTH(parm2)-1 ;
					IF parm2 [l] = ')' THEN
						parm2 [l] := ' ' ;
					END ;
					parm2 [0] := ' ' ;
					TRIM (parm2) ;
					IF NOT GetZahl (parm2, REPLanzahl) THEN
						SyntaxWarnung ('Anzahl erwartet') ;
					END ;
				ELSE
					SyntaxWarnung ('REPL zone (anzahl) erwartet') ;
				END ;

			ELSIF Equal (befehl, 'REND')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF REPLanzahl > 0 THEN
					REPLausfuehren ;
				END ;

			ELSIF Equal (befehl, 'WIED')THEN
				IF p = NIL THEN
					SyntaxFehler ('X=WIED(p1,p2,...) erwartet') ;
				ELSE
					IF parm1 [0] = '(' THEN								(* x=wied (name1, name2) *)
						parm1 := parm1 [1..HIGH(parm1)] ;
						WIEDwerte [0] := parm1 ;
						WIEDwerte [1] := parm2 ;
						WIEDindex := 2 ;
						LOOP
							IF ExStrings.GetNextItem (zeile, Index, parm2, ',)') THEN
								TRIM (parm2) ;
							ELSE
								EXIT ;
							END ;
							IF parm2 [0] <> 0C THEN
								WIEDwerte [WIEDindex] := parm2 ;
								INC (WIEDindex) ;
							END ;
						END ;
						WIEDwerte [WIEDindex] := '' ;
						imWIED := TRUE ;
					END ;
					WIEDname := ErsetzungEintragen (p ^ .name, WIEDwerte [0]) ;		(* schonmal auf 1. Wert der Liste einstellen *)
				END ;
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'WEND')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF imWIED THEN
					WIEDausfuehren ;
				END ;

			ELSIF Equal (befehl, 'STEND')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				STARRaktiv := FALSE ;

			ELSIF Equal (befehl, 'VORBES')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				VORBESda := TRUE ;
				IF parm1 [0] = '(' THEN
					parm1 := parm1 [1..HIGH(parm1)] ;
					TRIM (parm1) ;
				END ;
				l := LENGTH (parm2) ;
				IF (l > 1) AND (parm2 [l-1] = ')') THEN
					parm2 [l-1] := 0C ;
				END ;
				IF GetZahl (parm1, p1) THEN
                                	VorbesTK := p1 ;
				END ;
				IF GetZahl64 (parm2, wert64) THEN
					VorbesInhalt := wert64 ;
				END ;
			ELSIF Equal (befehl, 'UVB')THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF GetZahl (parm1, p1) THEN
					UnterdrueckeVerbotenerBezug := p1 <> 0 ;		(* UVB 1 *)
					UnterdrueckeUnbekannt := p1 > 1 ;			(* UVB 2 *)
				END ;

			ELSIF Equal (befehl, 'INDEX') THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

				IF parm1 [0] = '(' THEN
					AktInd := IndexPegel + 1 ;
					parm1 [0] := ' ' ;
					TRIM (parm1) ;
					NamenMerken (parm1) ;
					p ^ .wert := AktInd ;
					IF debug.AssemblerProtokoll THEN
						TraceF ("%s = %'02h", parm1, AktInd) ;
					END ;
					INC (AktInd) ;
				ELSE
					IF NOT GetZahl (parm1, AktInd) OR ( parm2 [0] <> '(') THEN
						SyntaxFehler ('INDEX - Befehl erwartet') ;
						RETURN FALSE ;
					END ;
					parm2 [0] := ' ' ;
				END ;

				IndexPegel := AktInd ;
				l := LENGTH(parm2)-1 ;
				IF parm2 [l] = ')' THEN
					parm2 [l] := ' ' ;
				END ;

				REPEAT
					TRIM (parm2) ;
					l := LENGTH(parm2) ;
					IF parm2 [l-1] = ')' THEN
						parm2 [l-1] := ' ' ;
						TRIM (parm2) ;
					END ;
					IF parm2 [0] > ' ' THEN
						NamenMerken (parm2) ;
						IF p <> NIL THEN
							p ^ .wert := AktInd ;
						END ;
						IF AktInd > IndexPegel THEN
							IndexPegel := AktInd ;
						END ;
						IF IndexPegel > MaxIndexPegel THEN
							MaxIndexPegel := IndexPegel ;
						END ;
						IF debug.AssemblerProtokoll THEN
							TraceF ("%s = %'02h", parm2, AktInd) ;
						END ;
						INC (AktInd) ;
					END ;
				UNTIL NOT ExStrings.GetNextItem (zeile, Index, parm2, ',') ;
				p := NIL ;

			ELSIF Equal (befehl, 'STRICT') THEN
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF GetZahl (parm1, p1) THEN
					AbsoluterIndexVerboten := (p1 BAND 1) <> 0 ;
					ImpliziterIndexVerboten := (p1 BAND 2) <> 0 ;
					AbsolutAdressteilVerboten := (p1 BAND 4) <> 0 ;
				END ;

			ELSIF Equal (befehl, 'EXTERN')
			   OR Equal (befehl, 'EXTOPT') THEN
				p := NIL ;
				AblageLaenge := 0 ;

				IF parm2 [0] = '(' THEN								(* extern mo (name1, name2) *)
					l := LENGTH(parm2)-1 ;
					IF parm2 [l] = ')' THEN
						parm2 [l] := ' ' ;
					END ;
					parm2 [0] := ' ' ;

					wbVerweis := WoerterbuchEintragen (parm1) ;

					REPEAT
						TRIM (parm2) ;
						l := LENGTH(parm2) ;
						IF parm2 [l-1] = ')' THEN
							parm2 [l-1] := ' ' ;
							TRIM (parm2) ;
						END ;
						IF parm2 [0] > ' ' THEN
							p  := WBeintragen (wbVerweis ^ .nam0, parm2) ;		(* externen Namen lokalisieren *)
							p := ExternNamenEintragen (parm2, p) ;			(* und als lokalen Link speichern *)
							wert := p ^ .echtname ^ .wert ;
							IF imLauf2 AND (wert = 0) AND NOT Equal (befehl, 'EXTOPT') THEN
								IF NOT UnterdrueckeUnbekannt THEN
									str := 'Extern-Bezug ' ;
									Strings.Append (parm2, str) ;
									Strings.Append (' nicht gefunden', str) ;
									SyntaxWarnung (str) ;
								END ;
							END ;
							IF debug.AssemblerProtokoll THEN
								TraceF ("%s = %'02h", parm2, wert) ;
							END ;
						END ;
					UNTIL NOT ExStrings.GetNextItem (zeile, Index, parm2, ',') ;
				ELSE 										(* extern moname *)
					wbVerweis := WoerterbuchEintragen (parm1) ;
					p  := WBeintragen (wbVerweis ^ .nam0, parm1) ;				(* externen Namen lokalisieren *)
					p := ExternNamenEintragen (parm1, p) ;					(* und als lokalen Link speichern *)

				END ;
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'HDEF') THEN							(* hdef (dateiname) *)
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
				IF parm1 [0] = '(' THEN
					parm1 := parm1 [1..LENGTH(parm1)-2] ;
					p := INCLeintragen (parm1) ;
					IF NeuAngelegt THEN			(* Datei nur 1-mal includen *)

						merkAktQuelle := AktQuelle ;
						merkAktQuellName := AktQuellName ;
						merkZeilenNummer := ZeilenNummer ;
						ZeilenNummer := 0 ;

						AktQuellName := parm1 ;

						IF debug.AssemblerProtokoll THEN
							TraceF ('*** HDEF -> %s ***', AktQuellName) ;
						END ;

						IF protausgabe THEN
							FileIO.WriteF (prot, "\n\n*** HDEF : %s --> %s ***\n\n", merkAktQuellName, AktQuellName) ;
						END ;

						rm := LaufKern (FALSE) ;

						IF protausgabe THEN
							FileIO.WriteF (prot, "\n\n*** HDEF.ENDE %s <-- %s ***\n\n", merkAktQuellName, AktQuellName) ;
						END ;

						IF debug.AssemblerProtokoll THEN
							TraceF ('*** HDEF.ENDE %s <- %s ***', merkAktQuellName, AktQuellName) ;
						END ;

						AktQuelle := merkAktQuelle ;
						AktQuellName := merkAktQuellName ;
						ZeilenNummer := merkZeilenNummer ;

						LaufEnde := FALSE ;
					END ;

					p := NIL ;
					AblageLaenge := 0 ;
					befehl := '' ;
				ELSE
					SyntaxWarnung ('HDEF (dateiname) erwartet') ;
				END ;

			ELSIF Equal (befehl, 'INCL') THEN
				p := INCLeintragen (parm1) ;
				IF NeuAngelegt THEN			(* Datei nur 1-mal includen *)
					p := NIL ;
					AblageLaenge := 0 ;
					befehl := '' ;

					LaufEnde := FALSE ;

					merkAktWB := AktWB ;
					merkAktSegmWB := AktSegmWB ;
					merkAktQuelle := AktQuelle ;
					merkAktQuellName := AktQuellName ;
					merkZeilenNummer := ZeilenNummer ;
					ZeilenNummer := 0 ;

					AktQuellName := parm1 ;

					IF debug.AssemblerProtokoll THEN
						TraceF ('*** INCL -> %s ***', AktQuellName) ;
					END ;

					IF protausgabe THEN
						FileIO.WriteF (prot, "\n\n*** INCL : %s --> %s ***\n\n", merkAktQuellName, AktQuellName) ;
					END ;

					rm := LaufKern (TRUE) ;

					IF protausgabe THEN
						FileIO.WriteF (prot, "\n\n*** INCL.ENDE %s <-- %s ***\n\n", merkAktQuellName, AktQuellName) ;
					END ;

					IF debug.AssemblerProtokoll THEN
						TraceF ('*** INCL.ENDE %s <- %s ***', merkAktQuellName, AktQuellName) ;
					END ;

					AktWB := merkAktWB ;
					AktSegmWB := merkAktSegmWB ;
					AktQuelle := merkAktQuelle ;
					AktQuellName := merkAktQuellName ;
					ZeilenNummer := merkZeilenNummer ;

					LaufEnde := FALSE ;
				END ;

				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'ENDE') THEN
				IF debug.AssemblerProtokoll THEN
					TraceF ('ENDE') ;
				END ;
				AblageLaenge := 0 ;
				LaufEnde := TRUE ;
				befehl := '' ;

			ELSIF Equal (befehl, 'ABLAGE') THEN
				IF Equal (parm1, 'FZONE') THEN
					FzoneAktiv := TRUE ;
				ELSE
					CheckDynAblage ;
				END ;
			   	p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'FZONE') THEN
				FzoneAktiv := TRUE ;
			   	p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'ZONE') THEN
			   	FzoneAktiv := FALSE ;
			   	p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
			ELSIF Equal (befehl, 'AEND') THEN
			   	FzoneAktiv := FALSE ;
				IF AktCzoneArt <> ZoneUndef THEN
					ZonenPegel [AktCzoneArt] := merkCzonePegel ;
					AblageAdresse := merkCzonePegel ;
					AktZonenArt := AktCzoneArt ;
					AktCzoneArt := ZoneUndef ;
				ELSE
					EndeDynZonenAblage ;
				END ;
			   	p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'CZONE') THEN
				NeueCZONE ;
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'SEGM') THEN
				NeuesSEGM ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'ABSADR') THEN		(*	ABSADR 'xxx' B		*)
				IF parm2 [0] = 0C THEN
					GetZahl (parm1, ZonenAdresse) ;
				ELSE
					GetZahl (parm1, AblageAdresse) ;
					AktZonenArt := GetZonenArt (parm2) ;
				END ;
				p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;

			ELSIF Equal (befehl, 'HGEBIET') THEN		(*	HGEBIET oname (anfadr, lng)	*)
				PseudoGebiet (FALSE) ;

			ELSIF Equal (befehl, 'VGEBIET') THEN		(*	VGEBIET oname (anfadr, lng)	*)
				PseudoGebiet (TRUE) ;

			ELSIF Equal (befehl, 'GEBIET')
			   OR Equal (befehl, 'STRUKT')
			   OR Equal (befehl, 'EINGG')
			   OR Equal (befehl, 'GEBAN')
			   OR Equal (befehl, 'ZONAN')
			   OR Equal (befehl, 'LUECKE')
			    					THEN				(* ignorieren *)
			   	p := NIL ;
				AblageLaenge := 0 ;
				befehl := '' ;
			END ;

			RETURN TRUE ;
		END ;
		RETURN FALSE ;
	END upGetBefehlsZeile ;


	PROCEDURE CheckRelativSprung () : BOOLEAN ;
		VAR	i :	INTEGER ;
	BEGIN
		CASE befehlscode OF
		opSZX,					(* SZX *)
		opST,					(* ST *)
		opSTN,					(* STN *)
		opSBIT,					(* SBIT *)
		opSL,					(* SL *)
		opSLL,					(* SLL *)
		opSLN,					(* SLN *)
		opSNL,					(* SNL *)
		opSEGG :				(* SEGG *)
			IF p1 > 7FFFFFFFH THEN
				i := VAL (INTEGER, p1) ;
				IF i < - 127 THEN
					RETURN FALSE ;
				END ;
				p1 := VAL (CARDINAL, i)
(*			ELSIF p1 > 127 THEN					funktioniert nicht bei Ablageadresse 0		*)
			ELSIF p1 > 15 THEN
				i := VAL (INTEGER, p1 - AblageAdresse) ;
				IF (i < -127) OR (i > 127) THEN
					RETURN FALSE ;
				END ;
				IF i < 0 THEN
					DEC (i) ;					(* B-1 *)
				END ;
				p1 := VAL (CARDINAL, i) BAND 0FFH ;
			END ;
		ELSE
		END ;
		RETURN TRUE ;
	END CheckRelativSprung ;

	PROCEDURE GetIndexOperand (parm : ARRAY OF CHAR) : CARDINAL8 ;
		VAR	z :	CARDINAL ;
	BEGIN
		IF GetZahl (parm, z) THEN
			CheckAbsoluterIndex (parm, z) ;
		ELSIF NOT ImpliziterIndex (parm, z) THEN
			SyntaxWarnung ('Index erwartet') ; ;
		END ;
		RETURN z ;
	END GetIndexOperand ;

	PROCEDURE CheckGeradeUngeradeAdresse ;
	BEGIN
		IF SpezGerade AND ODD(AblageAdresse) OR SpezUngerade AND NOT (ODD(AblageAdresse)) THEN
			INC (AblageAdresse) ;
		END ;
	END CheckGeradeUngeradeAdresse ;

	PROCEDURE GetZahlAusZiffern (parm : ARRAY OF CHAR) : CARDINAL ;
		VAR	i,
			j :	CARDINAL ;
			str :	ARRAY [0..31] OF CHAR ;

		PROCEDURE p (ch : CHAR) ;
		BEGIN
			str [j] := ch ;
			INC (j) ;
		END p ;

	BEGIN
		j := 0 ;
		FOR i := 1 TO LENGTH (parm) DO
			CASE parm [i-1] OF
			'0'..'9', "'" :		p (parm [i-1]) ;
			ELSE
			END ;
		END ;
		p (0C) ;
		IF GetZahl (str, i) THEN
			RETURN i ;
		END ;
		RETURN 0 ;
	END GetZahlAusZiffern ;

	PROCEDURE SonderBehandlung ( ): BOOLEAN ;
		VAR	j,
			zahl,
			pos,
			zw :	CARDINAL ;
			found :	BOOLEAN ;
	BEGIN
		zw := 0 ;

		CASE befehlscode OF

		opIR :		zw := GetZusatzbits (parm1, spezIR) ;
		|
		opLA :		zw := GetZusatzbits (parm1, spezLA) ;
		|
		opLR :		zw := GetZusatzbits (parm1, spezLR) ;
		|
		opLZL :		IF Strings.Equal (parm1, '0') THEN
					zw := 0 ;
				ELSE
					zw := GetZusatzbits (parm1, spezLZL) ;
				END ;
				IF NOT Strings.Equal (parm2, '0') THEN
					 zw := zw BOR ORD (GetZusatzbits (parm2, spezSL)) ;
				END ;
		|
		opRXMRX :	zw := ORD (GetZusatzbits (parm1, spezRXMRX)) BOR ORD (GetIndexOperand (parm2)) ;
		|
		opSL,
		opSLL,
		opSLN,
		opSNL :		IF GetZahl (parm1, p1) THEN
					IF CheckRelativSprung () THEN
						IF parm2 [0] <= ' ' THEN
							SyntaxWarnung ('Merklichter erwartet') ;
						ELSIF NOT UnterdrueckeVerbotenerBezug AND (GetZahlAusZiffern (parm2) = 0) THEN
							SyntaxWarnung ('Merklichter erwartet') ;
						END ;
						zw := ((p1 BAND 0FFH) SHL 8) BOR ORD (GetZusatzbits (parm2, spezSL)) ;
					ELSE
						SyntaxWarnung ('Relativadresse erwartet') ;
					END ;
				ELSE
					SyntaxWarnung ('Sprungziel erwartet') ;
				END ;
		|
		opSEGG :	IF GetZahl (parm1, p1) THEN
					IF CheckRelativSprung () THEN
						zw := ((p1 BAND 0FFH) SHL 8) ;
					ELSE
						SyntaxWarnung ('Relativadresse erwartet') ;
					END ;
				ELSE
					SyntaxWarnung ('Sprungziel erwartet') ;
				END ;
				Strings.FindNext ('N', parm2, 0, found, pos) ;
				IF found THEN
					parm2 [pos] := ' ' ;
					TRIM (parm2) ;
					p2 := 0 - GetZahlAusZiffern (parm2) - 1 ;	(* B-1 *)
				ELSE
					p2 := GetZahlAusZiffern (parm2) ;
				END ;
				zw := zw BOR (p2 BAND 0FFH) ;

		|
		opNL :		IF GetZahl (parm1, p1) THEN
					zw := GetZusatzbits (parm1, spezSL) ;
				END ;
				IF ((zw BAND 0FFH) = 0) AND NOT UnterdrueckeVerbotenerBezug THEN
					SyntaxWarnung ('Merklichter erwartet') ;
				END ;
		|
		opNRM :		IF Strings.Equal (parm1, 'N') THEN
					zw := 0 ;
				ELSIF Strings.Equal (parm1, 'L') THEN
					zw := 20H ;
				ELSIF Strings.Equal (parm1, 'F') THEN
					zw := 40H ;
				ELSIF Strings.Equal (parm1, 'F4') THEN
					zw := 60H ;
				ELSIF Strings.Equal (parm1, 'G') THEN
					zw := 80H ;
				ELSIF Strings.Equal (parm1, 'FG') THEN
					zw := 0C0H ;
				ELSE
					SyntaxWarnung ('NRM-Spezifikation erwartet') ;;
				END ;
		|
		opR :		zw := GetZusatzbitsTest (parm2, spezRRLRbit21) ;
				IF zw = 0 THEN
					zw := GetZusatzbits (parm2, spezRRLR) ;
				END ;
				IF NOT (VAL(tOpcode, (adrteil SHR 8)) IN RzweitCodeErlaubt) THEN
					SyntaxWarnung ('dieser Befehl ist nach R nicht erlaubt') ;;
				END ;

		|
		opRLR :		zw := GetZusatzbitsTest (parm2, spezRRLRbit21) ;
				IF zw = 0 THEN
					zw := GetZusatzbits (parm2, spezRRLR) ;
				END ;
		|
		opRT :		zw := GetZusatzbits (parm1, spezRT) ;
		|
		opSBIT :	IF GetZahl (parm1, p1) THEN
					IF CheckRelativSprung () THEN
						zw := ((p1 BAND 0FFH) SHL 8)  ;
					ELSE
						SyntaxWarnung ('Relativadresse erwartet') ;
					END ;
				ELSE
					SyntaxFehler ('Sprungziel erwartet') ;
					zw := 0 ;
				END ;

				IF enthalten (parm2, 'A') THEN
					(* zw := zw BOR 0 ; *)
				ELSIF enthalten (parm2, 'Q') THEN
					zw := zw BOR 40H ;
				ELSIF enthalten (parm2, 'D') THEN
					zw := zw BOR 80H ;
				ELSIF enthalten (parm2, 'H') THEN
					zw := zw BOR 0C0H ;
				ELSE
					SyntaxWarnung ('Register erwartet') ;
				END ;
				zw := zw BOR (GetZahlAusZiffern (parm2) BAND 3FH) ;
				IF NOT UnterdrueckeVerbotenerBezug THEN
					IF (zw BAND 3FH) = 0 THEN
						SyntaxWarnung ('Bitnummer erwartet') ;
					END ;
				END ;
		|
		opSH :		IF NOT GetZahl (parm2, p2) THEN
					SyntaxWarnung ('Shift-Anzahl erwartet') ;
				END ;
				zw := ORD (GetZusatzbits (parm1, spezSH)) BOR (p2 BAND 0FFH) ;
		|
		opSHB :		IF NOT GetZahl (parm2, p2) THEN
					SyntaxWarnung ('Shift-Anzahl erwartet') ;
				END ;
				IF Strings.Equal (parm1, 'R') THEN
					zw := 0 ;
				ELSE
					zw := ORD (GetZusatzbits (parm1, spezSHB)) ;
				END ;
				zw := zw BOR (p2 BAND 0FFH) ;
		|
		opST,
		opSTN :		IF GetZahl (parm1, p1) THEN
					IF CheckRelativSprung () THEN
						zw := ((p1 BAND 0FFH) SHL 8) BOR ORD (GetZusatzbits (parm2, spezSTSTN))  ;
					ELSE
						SyntaxWarnung ('Relativadresse erwartet') ;
					END ;
				ELSE
					SyntaxWarnung ('Sprungziel erwartet') ;
				END ;
				IF ((zw BAND 0C0H) = 0) AND NOT enthalten (parm2, '0') THEN
					SyntaxWarnung ('Typenkennung erwartet') ;
				END ;
				IF (zw BAND 3CH) = 0 THEN
					SyntaxWarnung ('Register erwartet') ;
				END ;
		|
		opTXR,
		opTRX :		zw := ORD (GetZusatzbits (parm1, spezTXRTRX)) BOR ORD (GetIndexOperand (parm2)) ;
		|
		opUS :		IF Strings.Equal (parm1, 'CE') OR Strings.Equal (parm1, 'EC') THEN	(* 'E' ist identisch zum Fehlen von 'G' *)
					parm1 := 'C' ;
				END ;
				zw := ORD (GetZusatzbits (parm1, spezUS)) BOR ORD (GetIndexOperand (parm2)) ;
		|
		opZK :
				zahl := 0 ;
				FOR j := 1 TO LENGTH(parm1) DO
					IF (parm1 [j-1] >= '0') AND (parm1 [j-1] <='9') THEN
						zahl := zahl * 10 + ORD (parm1 [j-1]) - ORD('0') ;
						parm1 [j-1] := ' ' ;
					END ;
				END ;
				zw := GetZusatzbits (parm1, spezZK) ;
				IF (zw BAND 1000H) = 0 THEN		(* Index *)
					zw := zw BAND 0FF00H BOR ORD (GetIndexOperand (parm2)) ;
				ELSE
					zw := zw BOR ORD (GetZusatzbits (parm2, spezZK2)) ;
				END ;

				zw := zw BOR ((zahl BAND 0FH) SHL 8) ;
		|
		opZTR :		zw := GetZusatzbits (parm1, spezZTR) ;
				IF parm1 [0] = 0C THEN
					SyntaxWarnung ('Typenkennung & Register erwartet') ;
				ELSIF ((zw BAND 0C0H) = 0) AND NOT enthalten (parm1, '0') THEN
					SyntaxWarnung ('Typenkennung erwartet') ;
				ELSIF (zw BAND 3EH) = 0 THEN
					SyntaxWarnung ('Register erwartet') ;
				END ;
		|
		opSZX :		IF GetZahl (parm1, p1) THEN
					IF CheckRelativSprung () THEN
						p2 := 0 ;
						IF NOT GetZahl (parm2, p2) AND NOT ImpliziterIndex (parm2, p2) THEN
							IF NOT UnterdrueckeUnbekannt THEN
								SyntaxWarnung ('Index erwartet') ;
							END ;
						ELSE
							CheckAbsoluterIndex (parm2, p2) ;
						END ;
						zw := ((p1 BAND 0FFH) SHL 8) BOR (p2 BAND 0FFH)  ;
					ELSE
						SyntaxWarnung ('Relativadresse erwartet') ;
					END ;
				ELSE
					SyntaxWarnung ('Sprungziel erwartet') ;
				END ;
		|
		opMAB,
		opMABI,
		opMU,
		opEMU :		IF parm2 [0] > ' ' THEN
					p2 := 0 ;
					IF GetZahl (parm2, p2) THEN
						adrteil :=  p2 BAND 0FFH ;
					ELSE
						adrteil := 0 ;
					END ;
				END ;
		|
		opZX,
		opHXP,
		opMH,
		opHBPX :	IF parm1 [0] > ' ' THEN
					p1 := 0 ;
					IF NOT GetZahl (parm1, p1) THEN
						IF NOT UnterdrueckeUnbekannt THEN
							SyntaxWarnung ('unzulässiger Operand') ;
						END ;
						p1 := 1FEFEH ;
					END ;
					IF parm2 [0] > ' ' THEN
						p2 := 0 ;
						IF NOT GetZahl (parm2, p2) AND NOT ImpliziterIndex (parm2, p2) THEN
							IF NOT UnterdrueckeUnbekannt THEN
								SyntaxWarnung ('unzulässiger zweiter Operand') ;
							END ;
							CheckAbsoluterIndex (parm2, p2) ;
						END ;

						adrteil := ((p1 BAND 0FFH) SHL 8) BOR (p2 BAND 0FFH) ;
					ELSE
						adrteil := p1 ;
					END ;
				END ;
		|
		opZU :		IF parm1 [0] > ' ' THEN
					p1 := 0 ;
					IF NOT GetZahl (parm1, p1) THEN
						IF NOT UnterdrueckeUnbekannt THEN
							SyntaxWarnung ('unzulässiger Operand') ;
						END ;
						p1 := 1FEFEH ;
					ELSIF (p1 < 0) OR (p1 > 255) THEN
						SyntaxWarnung ('unzulässiger ZU') ;
					END ;
					adrteil := p1 ;
				END ;
		ELSE
			RETURN FALSE ;
		END ;
		adrteil := ORD (adrteil) BOR zw ;
		RETURN TRUE ;
	END SonderBehandlung ;

	PROCEDURE BefehlAssemblieren ;
		VAR	zw :	CARDINAL ;
	BEGIN
		IF AbsolutAdressteilVerboten THEN
			IF (befehlscode IN Adrteil1)  AND (parm2 [0] > ' ') THEN
				SyntaxWarnung ('zu viele Adressteile') ;
			ELSIF (befehlscode IN Adrteil2)  AND (parm2 [0] <= '' ) THEN
				SyntaxWarnung ('zu wenig Adressteile') ;
			END ;
		END ;
		IF befehlscode IN ErsetzBefehle THEN
			zw := GetBefehl (parm1) ;					(* Zweitbefehl auswerten *)
			IF zw > 1000000H THEN
				SyntaxWarnung ('unzulässiger Zweit-Befehl') ;
			END ;
			halbwort := halbwort BOR (zw SHR 8 BAND 0FF80H) ;
			adrteil := halbwort ;

			CASE befehlscode OF
			opMAB,
			opMABI,
			opMU,
			opEMU,
			opR,
			opRLR :
				SonderBehandlung ;
			ELSE
				IF parm2 [0] > ' ' THEN
					p2 := 0 ;
					IF GetZahl (parm2, p2) THEN
						CheckAbsoluterIndex (parm2, p2) ;
					ELSIF NOT ImpliziterIndex (parm2, p2) THEN
						IF NOT UnterdrueckeUnbekannt THEN
							SyntaxWarnung ('unzulässiger Zweit-Befehl-Operand') ;
						END ;
					END ;
					adrteil :=  p2 BAND 0FFH ;
				ELSE
					adrteil := 0 ;
				END ;
			END ;
		ELSE									(* Normalbefehl auswerten *)
			adrteil := halbwort ;
			IF NOT SonderBehandlung () THEN
				IF parm1 [0] > ' ' THEN
					p1 := 0 ;
					IF NOT GetZahl (parm1, p1) AND NOT ImpliziterIndex (parm1, p1) THEN
						IF NOT UnterdrueckeUnbekannt THEN
							SyntaxWarnung ('unzulässiger Operand') ;
						END ;
						p1 := 1FEFEH ;
					ELSIF befehlscode IN IndexBefehle THEN
						CheckAbsoluterIndex (parm1, p1) ;
					END ;

					IF p1 = 1FEFEH THEN
						(* Fehlermeldung ist schon raus *)

					ELSIF befehlscode IN GrossSeitenRelativ THEN
						IF (AblageAdresse BAND 0FF0000H) <> (p1 BAND 0FF0000H) THEN	(* nicht selbe GrossSeite *)
							   IF p1 = 3EFEFEH THEN
							   	halbwort := (halbwort BAND 0FFFFH) BOR 9E0000H ;	(* Sprung in nicht anmontierten EXTOPT *)
							ELSIF (p1 > 1000H) AND (p1 < 800000H) THEN			(* nicht wahrscheinlich für Modifizierung *)
								IF NOT UnterdrueckeVerbotenerBezug THEN
									SyntaxWarnung ('unzul. Sprung in andere Großseite') ;
								END ;
							END ;
						END ;

					ELSIF befehlscode IN GrossSeite0Bezug THEN
						IF (p1 > 0FFFFH) AND (p1 < 800000H) THEN			(* nicht wahrscheinlich für Modifizierung *)
							IF NOT UnterdrueckeVerbotenerBezug THEN
								SyntaxWarnung ('16-Bit-Adressteil erwartet') ;
							END ;
						END ;

					ELSIF NOT CheckRelativSprung () THEN
						SyntaxWarnung ('Relativ-Adresse erwartet') ;
					END ;
					IF parm2 [0] > ' ' THEN
						p2 := 0 ;
						IF NOT GetZahl (parm2, p2) AND NOT ImpliziterIndex (parm2, p2) THEN
							IF NOT UnterdrueckeUnbekannt THEN
								SyntaxWarnung ('unzulässiger zweiter Operand') ;
							END ;
						ELSIF befehlscode IN IndexBefehle THEN
							CheckAbsoluterIndex (parm2, p2) ;
						END ;

						adrteil := ((p1 BAND 0FFH) SHL 8) BOR (p2 BAND 0FFH) ;
					ELSE
						adrteil := p1 ;
					END ;
				END ;
			END ;
		END ;

		AktBefehlswort := halbwort BOR ORD (adrteil) ;
	END BefehlAssemblieren ;

	PROCEDURE GanzwortAblegen ;
		VAR	str :	ARRAY [0..15] OF CHAR ;
	BEGIN
		IF ODD (AblageAdresse) THEN	(* Ganzwort muss auf Ganzwortadresse abgelegt werden *)
			neueTK := NeueTypenkennung ;
			NeueTypenkennung := 4 ;
			PutCodeWort (AblageAdresse, 0) ;
			NeueTypenkennung := neueTK ;
			INC (AblageAdresse) ;
		END ;
		IF NeueTypenkennung = 4 THEN
			CASE Zahlart OF
			zGleitkomma :	NeueTypenkennung := 0 ;
			|
			zFestkomma :	NeueTypenkennung := 1 ;
			|
			zHexa :		NeueTypenkennung := 2 ;
			|
			zString :	NeueTypenkennung := 3 ;
			ELSE
			END ;
		END ;
		IF protausgabe THEN
			CardToHex12 (wert64 BAND Cardinal48Bit, str) ;
			FileIO.WriteF (prot, "%'05h\t%c %s", AblageAdresse, NeueTypenkennung, str) ;
		END ;
		PutCodeGanzwort (AblageAdresse, NeueTypenkennung, wert64 BAND Cardinal48Bit) ;
		IF DoppeltGenau THEN
			IF protausgabe THEN
				protstuelpAbschluss ;
				CardToHex12 (DoppeltGenauRest BAND Cardinal48Bit, str) ;
				FileIO.WriteF (prot, "\t%'05h\t1 %s", AblageAdresse+2, str) ;
			END ;
			PutCodeGanzwort (AblageAdresse + 2, 1, DoppeltGenauRest) ;
			AblageLaenge := 4 ;
		ELSE
			AblageLaenge := 2 ;
		END ;
	END GanzwortAblegen ;

	PROCEDURE ConvertString (q : ARRAY OF CHAR ; VAR z : ARRAY OF CHAR) : CARDINAL ;
		VAR	i, j,
			l :	CARDINAL ;
			ch :	CHAR ;
	BEGIN
		i := 0 ;
		j := 0 ;
		l := LENGTH (q)-1 ;
		REPEAT
			ch := q [i] ;
			IF (ch = '*') AND ziffer (q [i+1]) AND ziffer (q [i+2]) AND ziffer (q [i+3]) THEN		(* *nnn Ersatzdarstellung *)
				ch := ZC1toANSI [CHR ((ORD (q [i+1]) - ORD('0')) * 100 + (ORD (q [i+2]) - ORD('0')) * 10 + ORD(q [i+3]) - ORD('0'))] ;
				i := i + 3 ;
			END ;
			z [j] := ch ;
			INC (i) ;
			INC (j) ;
		UNTIL i > l ;
		z [j] := 0C ;
		RETURN j ;
	END ConvertString ;

	PROCEDURE ProtHalbwort (TK, halbwort : CARDINAL) ;
	BEGIN
		IF ODD (AblageAdresse) OR (TK > 3) THEN
			FileIO.WriteF (prot, "%'05h\t  %'06h  ", AblageAdresse, halbwort BAND Cardinal24Bit) ;
		ELSE
			FileIO.WriteF (prot, "%'05h\t%c %'06h  ", AblageAdresse, TK, halbwort BAND Cardinal24Bit) ;
		END ;
	END ProtHalbwort ;

	PROCEDURE BefehlsForm () : BOOLEAN ;	(* ist das vllt. ein falsch geschriebener Befehl ? *)
		VAR	i,
			l :	CARDINAL ;
	BEGIN
		IF (AktSpezifikation [0] > ' ') AND (AktSpezifikation [0] <> 'B') THEN
			RETURN FALSE ;
		END ;
		IF (befehl [0] >= '0') AND (befehl [0] <= '9') THEN
			RETURN FALSE ;
		END ;
		l := LENGTH (befehl) ;
		IF l > 5 THEN
			RETURN FALSE ;
		END ;
		FOR i := 1 TO l DO
			CASE befehl [i-1] OF
			'A' .. 'Z',
			'0' .. '9' :	(* *)
			ELSE
				RETURN FALSE ;
			END ;
		END ;
		RETURN TRUE ;
	END BefehlsForm ;


BEGIN	(* GetEinzelBefehl *)

	befehl := '' ;

	Index := 0 ;

	INC (InformationsEinheiten) ;

	AblageLaenge := 1 ;
	p := NIL ;

	IF (ErsetzungVorhanden OR imLiteral) AND (DRUCK = 3) THEN
		protzeile := zeile ;
	END ;

	IF NOT upGetBefehlsZeile (zeile) THEN
		RETURN FALSE ;
	END ;

	IF befehl [0] = 0C THEN
		(* schon fertig bearbeitet *)

	ELSIF Equal (befehl, 'XBASIS') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, RegX) THEN
				SyntaxFehler ('Indexbasis erwartet') ;
				RegX := 0 ;
			END ;
			StartX := RegX ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("XB = %'04h", RegX) ;
			END ;
		END ;

	ELSIF Equal (befehl, 'UNTPR') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, l) OR (l < 0) OR (l > 255) THEN
				SyntaxFehler ('Unterprogramm-Zähler erwartet') ;
				l := 254 ;
			END ;
			RegU := l ;
			StartU := RegU ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("U = %'02h", ORD(RegU)) ;
			END ;
		END ;

	ELSIF Equal (befehl, 'START') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, StartAdresse) THEN
				SyntaxFehler ('Startadresse erwartet') ;
				StartAdresse := 1000000H ;
			END ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("START auf %'04h", StartAdresse) ;
			END ;
		END ;

	ELSIF Equal (befehl, 'ALARM') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, AlarmAdresse) THEN
				SyntaxFehler ('Alarmadresse erwartet') ;
				AlarmAdresse := 0 ;
			END ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("ALARM auf %'04h", AlarmAdresse) ;
			END ;
		END ;
(*
	ELSIF Equal (befehl, 'PROTECT') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, l) THEN
				SyntaxFehler ('Speicheradresse erwartet') ;
				l := 0 ;
			END ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("PROTECT auf %'06h", l) ;
			END ;
			SetProtect (l, TRUE) ;
		END ;
*)
(*
	ELSIF Equal (befehl, 'LUECKE') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, l) THEN
				SyntaxFehler ('Speicheradresse erwartet') ;
				l := 0 ;
			END ;
			IF debug.AssemblerProtokoll THEN
				TraceF ("LUECKE auf %'06h", l) ;
			END ;
			SetExist (l, FALSE) ;
		END ;
*)
	ELSIF Equal (befehl, 'DRUCK') THEN
		AblageLaenge := 0 ;
		IF imLauf2 THEN
			IF NOT GetZahl (parm1, l) OR (l > 3) THEN
				SyntaxFehler ('0/1/2/3 erwartet') ;
			ELSE
				IF DRUCK <> l THEN
					IF (DRUCK = 0) AND protausgabeAktiv THEN
						FileIO.WriteF (prot, "%' 4c\t\t\t%s", ZeilenNummer, AktZeile) ;
					END ;
					protausgabe := protausgabeAktiv ;
					protstuelpAbschluss ;
				END ;
				DRUCK := l ;
				IF DRUCK = 0 THEN
					protausgabe := FALSE ;
				ELSE
					protausgabe := protausgabeAktiv ;
				END ;
			END ;
		END ;

	ELSIF Equal (befehl, 'DEF') THEN
		AblageLaenge := 0 ;
		MakroErzeugen (parm1) ;
		inMakroDefinition := TRUE ;

	ELSIF Equal (befehl, 'DEND') THEN
		AblageLaenge := 0 ;
		inMakroDefinition := FALSE ;

	ELSIF Equal (befehl, 'VERS') THEN
		AblageLaenge := 0 ;
		VersionAusgefuehrt := FALSE ;
		BeginVersion ;

	ELSIF Strings.Equal (befehl, 'OA') THEN
		IF imLauf2 AND GetZahl (parm1, p1) AND GetZahl (parm2, p2) THEN
			p2 := p2 * 3 + p1 ;
		END ;
		GetAblageAdresse (ZoneK) ;
		CheckGeradeUngeradeAdresse ;
		befehl := '' ;
		IF imLauf2 THEN
			PutCodeWort (AblageAdresse, p2) ;
		END ;
		IF protausgabe THEN
			FileIO.WriteF (prot, "%'05h\t%'06h", AblageAdresse, p2) ;
		END ;

	ELSIF imLauf2 THEN

		IF SpezAdresse THEN						(* Adresskonstante *)
			GetAblageAdresse (ZoneK) ;
			CheckGeradeUngeradeAdresse ;
			IF NOT GetZahl64 (befehl, wert64) THEN
				IF NOT UnterdrueckeUnbekannt THEN
					SyntaxWarnung ('Programmadresse erwartet') ;
				END ;
				wert64 := 3EFEFEH ;
			END ;
			neueTK := NeueTypenkennung ;
			IF neueTK > 3 THEN
				IF NOT ODD(AblageAdresse) THEN
					neueTK := 2 ;
					NeueTypenkennung := 2 ;
				END ;
			END ;
			IF SpezMarke THEN
				wert64 := wert64 BOR 800000H ;
			END ;
			IF SpezAdressbit2 THEN
				wert64 := wert64 BOR 400000H ;
			END ;
			IF protausgabe THEN
				halbwort := wert64 BAND Cardinal24Bit ;
				ProtHalbwort (NeueTypenkennung, halbwort) ;
			END ;
			PutCodeWort (AblageAdresse, wert64) ;
			befehl := '' ;
		END ;

		IF SpezZoneV THEN
			GetAblageAdresse (ZoneV) ;				(* Variable *)
		ELSIF SpezZoneK THEN
			GetAblageAdresse (ZoneK) ;
		END ;

		IF befehl [0] > ' ' THEN
			IF befehl [0] = '"' THEN				(* String *)
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
				Index := 1 ;
				Strings.FindNext('"', befehl, Index, patternFound, posOfPattern);
				IF patternFound THEN
					lng := ConvertString (befehl [1 .. posOfPattern-1], parm3) ;
					IF SpezHalbwort THEN
						AblageLaenge := 1 ;
						IF lng > 3 THEN
							parm3 [3] := 0C ;	(* auf Halbwort verkürzen *)
							lng := 3 ;
						END ;
					ELSE
						IF ODD (AblageAdresse) THEN
							INC (AblageAdresse) ;		(* Ganzwortadresse für Strings gefordert *)
						END ;
						AblageLaenge := (lng + 5) DIV 6 * 2 ;	(* auf Anzahl Ganzworte verlängert, aber Anzahl Halbworte *)
					END ;
					IF lng < AblageLaenge * 3 THEN		(* auffüllen *)
						IF SpezRechts THEN
							parm3 [AblageLaenge * 3 - lng .. AblageLaenge * 3 - 1] := parm3 [0..lng-1] ;
							FOR j := 0 TO AblageLaenge * 3 - lng - 1 DO
								parm3 [j] := 0C ;			(* Ignores davor einfügen *)
							END ;
						ELSE
							FOR j := 0 TO AblageLaenge * 3 - lng DO
								parm3 [lng + j] := 0C ;			(* Ignores dahinter packen *)
							END ;
						END ;
					END ;
					parm3 [AblageLaenge * 3] := 0C ;
				ELSE
					SyntaxFehler ('Stringende nicht gefunden') ;
					AblageLaenge := 2 ;
					parm3 := ' ' ;
				END ;
				neueTK := NeueTypenkennung ;
				IF neueTK > 3 THEN
					neueTK := 3 ;
					IF NOT ODD(AblageAdresse) THEN
						NeueTypenkennung := 3 ;
					END ;
				END ;
				PutStringInSpeicherU (parm3 [0..AblageLaenge*3-1], AblageAdresse, AblageLaenge*3) ;
				IF protausgabe THEN
					IF AblageLaenge = 1 THEN
						halbwort := GetHalbwort (AblageAdresse) ;
						ProtHalbwort (AktTypenkennung, halbwort) ;
					ELSE
						FOR j := AblageAdresse TO AblageAdresse + AblageLaenge - 1 BY 2 DO
							IF j > AblageAdresse THEN
								protstuelpAbschluss ;
								FileIO.WriteF (prot, "\t") ;
							END ;
							wert64 := GetGanzwort (j) ;
							CardToHex12 (wert64, parm3) ;
							FileIO.WriteF (prot, "%'05h\t%c %s", j, AktTypenkennung, parm3) ;
						END ;
					END ;
				END ;

			ELSIF SpezLinks AND (befehl [0] = "'") AND (befehl [LENGTH(befehl)-1] = "'") THEN		(* reiner Hex-Wert mit /L *)
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
				neueTK := NeueTypenkennung ;
				IF neueTK > 3 THEN
					neueTK := 2 ;
					IF NOT ODD(AblageAdresse) THEN
						NeueTypenkennung := 2 ;
					END ;
				END ;
				IF SpezHalbwort THEN
					l := LENGTH(befehl) ;
					IF l < 8 THEN			(* auffüllen *)
						WHILE l < 8 DO
							befehl [l-1] := '0' ;
							INC (l) ;
						END ;
						befehl [l-1] := "'" ;
						befehl [l] := 0C ;
					END ;
					IF NOT GetZahl (befehl, halbwort) THEN
						SyntaxWarnung ('unzul. Hex-Halbwort') ;
						halbwort := 0 ;
					END ;
					IF SpezMarke THEN
						halbwort := halbwort BOR 800000H ;
					END ;
					IF protausgabe THEN
						ProtHalbwort (NeueTypenkennung, halbwort) ;
					END ;
					PutCodeWort (AblageAdresse, halbwort) ;
				ELSE
					l := LENGTH(befehl) ;
					IF l < 14 THEN			(* auffüllen *)
						WHILE l < 14 DO
							befehl [l-1] := '0' ;
							INC (l) ;
						END ;
						befehl [l-1] := "'" ;
						befehl [l] := 0C ;
					END ;
					IF NOT GetZahl64 (befehl, wert64) THEN
						SyntaxWarnung ('unzul. Hex-Wort') ;
						wert64 := 0 ;
					END ;
					IF SpezMarke THEN
						wert64 := wert64 BOR MarkenBit ;
					END ;
					GanzwortAblegen ;
				END ;

			ELSIF NOT BefehlGueltig (befehl) AND NOT BefehlVermutet (befehl) AND GetZahl64 (befehl, wert64) THEN
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
				neueTK := NeueTypenkennung ;
				IF neueTK > 3 THEN
					CASE Zahlart OF
					zGleitkomma :	neueTK := 0 ;
					|
					zFestkomma :	neueTK := 1 ;
					|
					zHexa :		neueTK := 2 ;
					|
					zString :	neueTK := 3 ;
					ELSE
					END ;
					IF NOT ODD(AblageAdresse) THEN
						NeueTypenkennung := neueTK ;
					END ;
				END ;
				IF SpezHalbwort THEN
					halbwort := wert64 BAND Cardinal24Bit ;
					IF SpezMarke THEN
						halbwort := halbwort BOR 800000H ;
					END ;
					IF protausgabe THEN
						ProtHalbwort (neueTK, halbwort) ;
					END ;
					PutCodeWort (AblageAdresse, halbwort) ;
				ELSE
					IF SpezMarke THEN
						wert64 := wert64 BOR MarkenBit ;
					END ;
					GanzwortAblegen ;
				END ;
			ELSE
				halbwort := GetBefehl (befehl) ;
				IF halbwort > 1000000H THEN
					str := 'unbekannter Befehl : ' ;
					Strings.Append (befehl, str) ;
					SyntaxFehler (str) ;
					halbwort := 0E90000H ;
				END ;

				IF NeueTypenkennung > 3 THEN
					NeueTypenkennung := 2 ;
				END ;

				befehlscode := VAL (tOpcode, halbwort SHR 16) ;			(* ab hier Befehle gezielt assemblieren *)

				BefehlAssemblieren ;

				ZeileEintragen (AblageAdresse, AktZeile) ;			(* Zeile merken für Debugger *)

				IF protausgabe THEN
					FileIO.WriteF (prot, "%'05h\t%'02h %'04h", AblageAdresse, (AktBefehlswort SHR 16) BAND 0FFH, AktBefehlswort BAND 0FFFFH) ;
				END ;

				PutBefehlsWort (AblageAdresse, AktBefehlswort) ;		(* Befehlswort im Programm ablegen *)
			END ;

			IF p <> NIL THEN
				IF (p ^ .wert <> AblageAdresse) AND NOT imInterpreter THEN
					SyntaxFehler('Asynchronität Lauf 1 / Lauf2') ;
(*
					IF ABS(VAL (INTEGER, p ^ .wert) - VAL (INTEGER, AblageAdresse)) < 5 THEN
						AblageAdresse := p ^ .wert ;
					END ;
*)
				END ;
			END ;

		END ;
	ELSE (* Lauf 1 *)
		IF befehl [0] > ' ' THEN
			IF SpezAdresse OR SpezHalbwort THEN						(* Adresskonstante *)
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
			ELSIF befehl [0] = '"' THEN							(* String *)
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
				Index := 1 ;
				Strings.FindNext('"', befehl, Index, patternFound, posOfPattern);
				IF patternFound THEN
					lng := ConvertString (befehl [1..posOfPattern-1], parm3) ;
					IF SpezHalbwort THEN
						AblageLaenge := 1 ;
        				ELSE
						IF ODD(AblageAdresse) THEN
							INC (AblageAdresse) ;
						END ;
						AblageLaenge := (lng + 5) DIV 6 * 2 ;
					END ;
				ELSE
					SyntaxFehler ('Stringende nicht gefunden') ;
					AblageLaenge := 2 ;
				END ;
			ELSIF BefehlGueltig (befehl) OR BefehlVermutet (befehl) THEN			(* Befehl *)
				GetAblageAdresse (ZoneB) ;
				CheckGeradeUngeradeAdresse ;
			ELSIF GetZahl64 (befehl, wert64) THEN						(* Zahl *)
				GetAblageAdresse (ZoneK) ;
				CheckGeradeUngeradeAdresse ;
				IF BefehlsForm () THEN
					GetAblageAdresse (ZoneB) ;				(* wahrscheinlich Syntaxfehler *)
				ELSIF SpezHalbwort THEN
					AblageLaenge := 1 ;
				ELSE
					IF ODD (AblageAdresse) THEN	(* Ganzwort muss auf Ganzwortadresse abgelegt werden *)
						INC (AblageAdresse) ;
					END ;
					IF DoppeltGenau THEN
						AblageLaenge := 4 ;
					ELSE
						AblageLaenge := 2 ;
					END ;
				END ;
			END ;
		END ;
		IF p <> NIL THEN
			IF (p ^ .wert <> AblageAdresse) AND (p ^ .wert <> 0) THEN
				SyntaxWarnung('Name bereits definiert') ;
				p := NIL ;
			END ;
		END ;
	END ;

	IF p <> NIL THEN
		p ^ .wert := AblageAdresse ;		(* Namen updaten *)

		IF debug.AssemblerProtokoll THEN
			TraceF ("%s : %'04h", p ^ .name, AblageAdresse) ;
		END ;
	END ;

	IF LiteralErster THEN
		LiteralAdresse := AblageAdresse ;
		LiteralErster := FALSE ;
	END ;

	protstuelpAbschluss ;

	RETURN TRUE ;
END GetEinzelBefehl ;


PROCEDURE GetBefehlsZeile (pzeile : ARRAY OF CHAR) : BOOLEAN ;			(* wird auch zeilenweise vom Interpreter aufgerufen *)
										(* kann mehrere durch Komma getrennte Befehle und Literale enthalten *)
	VAR	zeile,
		zwZeile :	ARRAY [0..255] OF CHAR ;
		befname :	ARRAY [0..31] OF CHAR ;
		i0,
		i, j, k,
		letztesBefehlsZeichen,
		adr,
		anf,
		end :		CARDINAL ;
		ch :		CHAR ;

	PROCEDURE put (ch : CHAR) ;
	BEGIN
		zwZeile [j] := ch ;
		INC (j) ;
	END put ;


	PROCEDURE Ersetzung ;
		VAR	ch :	CHAR ;
			j,
			k :	CARDINAL ;
			nam :	ARRAY [0..31] OF CHAR ;
			inh :	ARRAY [0..255] OF CHAR ;

		PROCEDURE put2 (ch : CHAR) ;
		BEGIN
			nam [k] := ch ;
			IF k < HIGH (nam) THEN
				INC (k) ;
			END ;
		END put2 ;

	BEGIN
		INC (i) ;
		INC (i) ;
		k := 0 ;
		REPEAT
			ch := zeile [i] ;
			CASE ch OF
			")" :
				BREAK ;
			|
			1C .. ' ' :
					(* Layout entfernen *)
			|
			'a'..'z' :	put2 (CHR(ORD(ch) - 20H)) ;	(* Großbuchstabe *)
			ELSE
					put2 (ch) ;
			END ;
			INC (i) ;
		UNTIL i > end ;
		put2 (0C) ;

		GetErsetzungsInhalt (nam, inh) ;

		FOR j := 1 TO LENGTH (inh) DO
			put (inh [j-1]) ;
		END ;

		IF inh [0] <> 0C THEN
			ErsetzungVorhanden := TRUE ;
			IF debug.AssemblerProtokoll THEN
				TraceF ('*ers %s -> %s', nam, inh) ;
			END ;
		ELSE
			IF debug.AssemblerProtokoll THEN
				TraceF ('*ers %s -> ?', nam) ;
			END ;
		END ;
	END Ersetzung ;


	PROCEDURE SonderBefehl ;
		VAR	KlammerZahl :	CARDINAL ;
	BEGIN
		KlammerZahl := 0 ;
		IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
			put (' ') ;
		END ;
		WHILE i <= end DO
			ch := zeile [i] ;
			CASE ch OF
			0C :	i := end ;
				BREAK ;
			|
			"'" :
				IF zeile [i+1] = "'" THEN		(* String in Sonderbefehl *)
					INC (i) ;
					put ('"') ;
					REPEAT
						INC (i) ;
						ch := zeile [i] ;
						IF (ch = "'") AND (zeile [i+1] = "'") THEN
							INC (i) ;
							BREAK ;
						END ;
						put (ch) ;
					UNTIL i > end ;
					put ('"') ;
				ELSE
					put (ch) ;
				END ;
			|
			'"' :						(* String in Sonderbefehl *)
				put ('"') ;
				REPEAT
					INC (i) ;
					ch := zeile [i] ;
					IF ch = '"' THEN
						BREAK ;
					END ;
					put (ch) ;
				UNTIL i > end ;
				put ('"') ;
			|
			1C..' ' :						(* Layout ignorieren *)
			|
			'+' :
				IF zeile [i+1] = '(' THEN			(* Ersetzung in Sonderbefehl *)
					Ersetzung ;
				ELSE
					put ('+') ;
				END ;
			|
			'(' :	put ('(') ;
				KlammerZahl := KlammerZahl + 1 ;
			|
			')' :
				put (')') ;
				IF KlammerZahl > 1 THEN
					KlammerZahl := KlammerZahl - 1 ;
				ELSE
					i := end ;				(* Restzeile hinter Klammerzu wegschmeissen *)
				END ;
			|
			'a'..'z' :
				put (CHR(ORD(ch) BAND 0DFH)) ;			(* als Großbuchstabe *)
			ELSE
				put (ch) ;
			END ;
			INC (i) ;
		END ;
		put (0C) ;
	END SonderBefehl ;


	PROCEDURE BitfeldKonstante ;
	BEGIN
		IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
			put (' ') ;
		END ;
		WHILE i <= end DO
			ch := zeile [i] ;
			CASE ch OF
			0C :	i := end ;
				BREAK ;
			|
			1C..' ' :						(* Layout ignorieren *)
			|
			"'" :
				IF zeile [i+1] = "'" THEN			(* String in BitfeldKonstante *)
					INC (i) ;
					put ('"') ;
					REPEAT
						INC (i) ;
						ch := zeile [i] ;
						IF (ch = "'") AND (zeile [i+1] = "'") THEN
							INC (i) ;
							BREAK ;
						END ;
						put (ch) ;
					UNTIL i > end ;
					put ('"') ;
				ELSE
					put (ch) ;
				END ;
			|
			'"' :							(* String in BitfeldKonstante *)
				put ('"') ;
				REPEAT
					INC (i) ;
					ch := zeile [i] ;
					IF ch = '"' THEN
						BREAK ;
					END ;
					put (ch) ;
				UNTIL i > end ;
				put ('"') ;
			|
			'+' :
				IF zeile [i+1] = '(' THEN			(* Ersetzung in BitfeldKonstante *)
					Ersetzung ;
				ELSE
					put ('+') ;
				END ;
			|
			')' :
				put (')') ;
				BREAK ;
			|
			'a'..'z' :
				put (CHR(ORD(ch) BAND 0DFH)) ;			(* als Großbuchstabe *)
			ELSE
				put (ch) ;
			END ;
			INC (i) ;
		END ;
	END BitfeldKonstante ;


	PROCEDURE TextKonstante ;
	BEGIN
		IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
			put (' ') ;
		END ;
		WHILE i <= end DO
			ch := zeile [i] ;
			CASE ch OF
			0C :	i := end ;
				BREAK ;
			|
			1C..' ' :						(* Layout ignorieren *)
				put ('"') ;
				REPEAT
					INC (i) ;
					IF i > end THEN
						BREAK ;							(* kein Stringabschluss da *)
					END ;
					ch := zeile [i] ;
					put (ch) ;							(* Strings unveraendert beibehalten *)
				UNTIL ch = '"' ;
			|
			'"' :								(* String *)
				put ('"') ;
				INC (i) ;
				REPEAT
					ch := zeile [i] ;
					IF (ch = '+') AND (zeile [i+1] = '(') THEN
						Ersetzung ;
					ELSE
						put (ch) ;						(* Strings unveraendert beibehalten *)
					END ;
					INC (i) ;
					IF i > end THEN
						BREAK ;							(* kein Stringabschluss da *)
					END ;
				UNTIL ch = '"' ;
			|
			"'" :								(* Apostroph *)
				IF zeile [i+1] = "'" THEN				(* Doppelapostroph für String *)
					put ('"') ;
					INC (i) ;
					INC (i) ;
					REPEAT
						ch := zeile [i] ;
						IF (ch = '+') AND (zeile [i+1] = '(') THEN
							Ersetzung ;
						ELSE
							IF (ch = "'") AND (zeile [i+1] = "'") THEN
								INC (i) ;
								BREAK ;
							END ;
							put (ch) ;
						END ;
						INC (i) ;
					UNTIL i > end ;
					put ('"') ;
				ELSE							(* Hexwert *)
					put ("'") ;
					ch := ' ' ;
					LOOP
						CASE ch OF
						0C :	EXIT ;							(* kein Hex-Abschluss *)
						|
						1C .. ' ' :
						|								(* Layout in Hexwerten ignorieren *)
						'a'..'f' :
							put (CHR(ORD(ch) BAND 0DFH)) ;				(* als Großbuchstabe *)
						|
						'0'..'9',
						'A'..'F' :
							put (ch) ;
						|
						'+' :
							Ersetzung ;						(* Tetrade *)
						|
						"'" :
							put (ch) ;
							EXIT ;							(* Hex-Abschluss *)
						ELSE
							DEC (i) ; ;
							EXIT ;							(* unzulässiges Zeichen im Hexwert : Abbruch *)
						END ;
						INC (i) ;
						IF i > end THEN
							EXIT ;							(* kein Hex-Abschluss da *)
						END ;
						ch := zeile [i] ;
					END ;
				END ;
			|
			'+' :
				IF zeile [i+1] = '(' THEN			(* Ersetzung in Sonderbefehl *)
					Ersetzung ;
				ELSE
					put ('+') ;
				END ;
			|
			')' :
				put (')') ;
				IF zeile [i+1] = "'" THEN			(* Ende Textkonstante *)
					put ("'") ;
					INC (i) ;
					BREAK ;
				END ;
			|
			'a'..'z' :
				put (CHR(ORD(ch) BAND 0DFH)) ;			(* als Großbuchstabe *)
			ELSE
				put (ch) ;
			END ;
			INC (i) ;
		END ;
	END TextKonstante ;


	PROCEDURE CheckSonderBefehl () : BOOLEAN ;		(* TRUE, wenn Klammer aktuell nicht als Literale anzusehen sind *)
		VAR	k :	CARDINAL ;


		PROCEDURE testV (bef : ARRAY OF CHAR) : BOOLEAN [INLINE] ;
		BEGIN
			RETURN Equal (zwZeile [0..HIGH(bef)-1], bef) ;
		END testV ;


		PROCEDURE test (bef : ARRAY OF CHAR) : BOOLEAN [INLINE] ;
		BEGIN
			RETURN testV (bef) OR Equal (zwZeile [k-HIGH(bef)+1 .. k], bef) ;
		END test ;


	BEGIN
		k := j - 1 ;
		WHILE (k > 1) AND (zwZeile [k] <= ' ') DO
			DEC (k) ;				(* k sitzt jetzt auf dem letzten Zeichen des Befehls *)
		END ;

		IF k >= 3 THEN
			IF testV ('REPL') THEN				(* REPL-Befehl *)
				RETURN TRUE ;
			END ;
			IF test ('WIED') THEN				(* WIED-Befehl *)
				RETURN TRUE ;
			END ;
			IF testV ('VERS') THEN				(* VERS-Befehl *)
				RETURN TRUE ;
			END ;
			IF testV ('FORM') THEN				(* FORM-Befehl *)
				RETURN TRUE ;
			END ;
			IF testV ('AEND') THEN				(* AEND-Befehl *)
				RETURN TRUE ;
			END ;
			IF testV ('HDEF') THEN				(* HDEF-Befehl *)
				RETURN TRUE ;
			END ;

			IF k >= 4 THEN
				IF testV ('INDEX') THEN			(* INDEX-Befehl *)
					RETURN TRUE ;
				END ;
				IF testV ('EINGG') THEN			(* EINGG-Befehl *)
					RETURN TRUE ;
				END ;
				IF testV ('ZONAN') THEN			(* ZONAN-Befehl *)
					RETURN TRUE ;
				END ;
				IF testV ('GEBAN') THEN			(* GEBAN-Befehl *)
					RETURN TRUE ;
				END ;

				IF k >= 5 THEN
					IF testV ('EXTERN') THEN		(* EXTERN-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('EXTOPT') THEN		(* EXTOPT-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('ABLAGE') THEN		(* ABLAGE-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('STRUKT') THEN		(* STRUKT-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('GEBIET') THEN		(* GEBIET-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('LUECKE') THEN		(* LUECKE-Befehl *)
						RETURN TRUE ;
					END ;
					IF testV ('VORBES') THEN		(* VORBES-Befehl *)
						RETURN TRUE ;
					END ;

					IF k >= 6 THEN
						IF testV ('HGEBIET') THEN		(* HGEBIET-Befehl *)
							RETURN TRUE ;
						END ;
						IF testV ('VGEBIET') THEN		(* VGEBIET-Befehl *)
							RETURN TRUE ;
						END ;
					END ;
				END ;

			END ;
		END ;
		RETURN FALSE ;
	END CheckSonderBefehl ;


	PROCEDURE CheckLayout ;							(* auf jeden Fall einrücken ohne Namen *)
		VAR	i,
			lng :		CARDINAL ;
	BEGIN
		lng := LENGTH (pzeile) ;
		FOR i := 1 TO lng DO
			CASE pzeile [i-1] OF
			0C .. ' ' :	BREAK ;
			|
			'=' :		IF pzeile [i] = ht THEN
						zeile := pzeile ;
					ELSE
						zeile [0..i-1] := pzeile [0..i-1] ;
						zeile [i] := ht ;
						zeile [i+1..lng+1] := pzeile [i..lng] ;
					END ;
					RETURN ;
			|
			'"' :		BREAK ;
			|
			"'" :		BREAK ;
			|
			'(' :		BREAK ;
			|
			'/' :		BREAK ;
			ELSE
			END ;
		END ;
		zeile [0] := ht ;
		zeile [1..lng+1] := pzeile [0..lng] ;
	END CheckLayout ;


	PROCEDURE Ersetzungen ;
		VAR	ind,
			posfound,
			posfound2 :	CARDINAL ;
			nam :		ARRAY [0..31] OF CHAR ;
			inh :		ARRAY [0..255] OF CHAR ;
			found :		BOOLEAN ;
	BEGIN
		ind := 0 ;
		LOOP		(* Schleife über Ersetzungen *)
			Strings.FindNext ('+(', zeile, ind, found, posfound) ;
			IF NOT found THEN
				EXIT ;
			END ;
			Strings.FindNext (')', zeile, posfound+3, found, posfound2) ;
			IF NOT found THEN
				EXIT ;
			END ;
			nam := zeile [posfound+2..posfound2-1] ;
			Strings.Capitalize (nam) ;
			GetErsetzungsInhalt (nam, inh) ;
			Strings.Delete (zeile, posfound, posfound2-posfound+1) ;
			ind := posfound ;
			IF inh [0] <> 0C THEN
				ind := posfound + LENGTH (inh) ;
				Strings.Insert (inh, posfound, zeile) ;
				ErsetzungVorhanden := TRUE ;
				IF debug.AssemblerProtokoll THEN
					TraceF ('*ers %s -> %s', nam, inh) ;
				END ;
			ELSE
				IF debug.AssemblerProtokoll THEN
					TraceF ('*ers %s -> ?', nam) ;
				END ;
			END ;
		END ;
	END Ersetzungen ;


BEGIN	(* GetBefehlsZeile *)

	IF (pzeile [0] > ' ') AND NOT imLiteral THEN
		CheckLayout ;
	ELSE
		zeile := pzeile ;
	END ;

	IF inMakroDefinition THEN
		befname := zeile ;
		TRIM (befname) ;
		IF ExStrings.EqualI (befname [0..3], 'DEND') THEN
			inMakroDefinition := FALSE ;
		ELSIF NOT imLauf2 THEN
			MAKROzeileEintragen (zeile) ;
		END ;
		IF protausgabe AND imLauf2 AND (DRUCK >= 2) THEN
			FileIO.WriteF (prot, "%' 4c\t\t\t%s", ZeilenNummer, zeile) ;
				protstuelpAbschluss ;
		END ;
		RETURN TRUE ;
	END ;

	IF debug.AssemblerProtokoll AND imLauf2 THEN
		TraceF ("\t\t\t\t\%' 4c  %s", ZeilenNummer, zeile) ;
	END ;

	IF imWIED THEN
		WIEDeintragen (zeile) ;
	END ;

	Ersetzungen ;

	IF inVersion THEN
		befname := zeile ;
		TRIM (befname) ;
		IF ExStrings.EqualI (befname [0..4], 'SONST') THEN
			IF NOT VersionBedingung THEN
				VersionBedingung := NOT VersionAusgefuehrt ;
				parm1 := befname [5..HIGH(befname)] ;
				TRIM (parm1) ;
				IF parm1 [0] = '(' THEN
					BeginVersion ;
				END ;
			ELSE
				VersionBedingung := FALSE ;
			END ;
			IF protausgabe AND imLauf2 AND VersionBedingung THEN
				FileIO.WriteF (prot, "%' 4c &\t\t\t%s", ZeilenNummer, zeile) ;
				AblageLaenge := 0 ;
				protstuelpAbschluss ;
			END ;
			RETURN TRUE ;
		ELSIF ExStrings.EqualI (befname [0..3], 'VEND') THEN
			AblageLaenge := 0 ;
			inVersion := FALSE ;
			RETURN TRUE ;
		ELSIF NOT VersionBedingung THEN
			AblageLaenge := 0 ;
			RETURN TRUE ;
		END ;
	END ;

	IF protausgabe AND imLauf2 THEN
		IF inMakroAufruf OR inWIEDausfuehrung THEN
			FileIO.WriteF (prot, "%' 4c &\t", ZeilenNummer) ;
			IF NOT imLiteral THEN
				protzeile := zeile ;
			END ;
 		ELSE
			FileIO.WriteF (prot, "%' 4c\t", ZeilenNummer) ;
			IF NOT imLiteral THEN
				protzeile := zeile ;
			END ;
		END ;
	END ;

	IF NOT inMakroAufruf THEN
		IF REPLanzahl <> 0 THEN
			REPLeintragen (zeile) ;
		END ;
	END ;

	ErsetzungVorhanden := FALSE ;

	FOR i0 := 1 TO LENGTH (zeile) DO							(* Steuerzeichen durch Space ersetzen *)
		IF zeile [i0-1] < ' ' THEN
			zeile [i0-1] := ' ' ;
		END ;
	END ;


	anf := 0 ;
	WHILE zeile [anf] <= ' ' DO
		IF (anf > HIGH(zeile)) OR (zeile [anf] = 0C) THEN
			AblageLaenge := 0 ;
			protstuelpAbschluss ;
			RETURN TRUE ;
		END ;
		INC (anf) ;					(* führende Layoutzeichen ignorieren *)
	END ;

	end := LENGTH (zeile) - 1 ;

	WHILE (end > 0) AND (zeile [end] <= ' ') DO
		DEC (end) ;					(* schleppende Layoutzeichen ignorieren *)
	END ;

	letztesBefehlsZeichen := 0 ;

	i := anf ;
	j := 0 ;

	LOOP
		ch := zeile [i] ;
		CASE ch OF
		0C :
			EXIT ;
		|
		1C .. ' ' :							(* Layoutzeichen *)
			WHILE (i <= end) AND (zeile [i] <= ' ') DO
				INC (i) ;
			END ;
			IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
				letztesBefehlsZeichen := j-1 ;
				put (' ') ;							(* mehrere Layoutzeichen als 1 Space ablegen *)
			END ;

			DEC (i) ;								(* erstes Nicht-Layout-Zeichen noch bearbeiten *)
		|
		'"' :								(* String *)
			IF (j > 0) AND (zwZeile [j-1] >= 'A') THEN
				put (' ') ;
			END ;
			put ('"') ;
			INC (i) ;
			REPEAT									(* im String einige Syntaxzeichen ändern *)
				ch := zeile [i] ;
				CASE ch OF
				'/' :	put ('\') ;
				|
				'=' :	put ('÷') ;
				|
				'(' :	put ('{') ;
				|
				')' :	put ('}') ;
				|
				',' :	put ('¸') ;
				|
				'+' :	IF zeile [i+1] = '(' THEN
						Ersetzung ;
					ELSE
						put (ch) ;
					END ;
				|
				'"' :	BREAK ;
				ELSE
					put (ch) ;						(* Strings ansonsten unveraendert beibehalten *)
				END ;
				INC (i) ;
			UNTIL i > end ;
			put ('"') ;
		|
		"'" :								(* Apostroph *)
			IF zeile [i+1] = "'" THEN				(* Doppelapostroph für String *)
				IF (j > 0) AND (zwZeile [j-1] >= 'A') THEN
					put (' ') ;
				END ;
				put ('"') ;
				INC (i) ;
				INC (i) ;
				REPEAT								(* im String einige Syntaxzeichen ändern *)
					ch := zeile [i] ;
					CASE ch OF
					'/' :	put ('\') ;
					|
					'=' :	put ('÷') ;
					|
					'"' :	put ('“') ;
					|
					'(' :	put ('{') ;
					|
					')' :	put ('}') ;
					|
					',' :	put ('¸') ;
					|
					'+' :	IF zeile [i+1] = '(' THEN
							Ersetzung ;
						ELSE
							put (ch) ;
						END ;
					|
					"'" :	IF (zeile [i+1] = "'") AND (zeile [i+2] <> "'") THEN
							INC (i) ;
							BREAK ;
						ELSE
							put (ch) ;
						END ;
					ELSE
					       	put (ch) ;					(* Strings ansonsten unveraendert beibehalten *)
					END ;
					INC (i) ;
				UNTIL i > end ;
				put ('"') ;

			ELSIF zeile [i+1] = '(' THEN				(* '( für Textkonstante *)
				TextKonstante ;
			ELSE							(* Hexwert *)
				IF (j > 0) AND (zwZeile [j-1] >= 'A') THEN
					put (' ') ;
				END ;
				put ("'") ;
				ch := ' ' ;
				LOOP
					CASE ch OF
					0C :	EXIT ;							(* kein Hex-Abschluss *)
					|
					1C .. ' ' :
					|								(* Layout in Hexwerten ignorieren *)
					'a'..'f' :
						put (CHR(ORD(ch) BAND 0DFH)) ;				(* als Großbuchstabe *)
					|
					'0'..'9',
					'A'..'F' :
						put (ch) ;						(* Tetrade *)
					|
					'+' :
						IF zeile [i+1] = '(' THEN
							Ersetzung ;
						ELSE
							put (ch) ;
						END ;
					|
					"'" :
						put (ch) ;
						EXIT ;							(* Hex-Abschluss *)
					ELSE
						DEC (i) ; ;
						EXIT ;							(* unzulässiges Zeichen im Hexwert : Abbruch *)
					END ;
					INC (i) ;
					IF i > end THEN
						EXIT ;							(* kein Hex-Abschluss da *)
					END ;
					ch := zeile [i] ;
				END ;

			END ;
		|
		'-' :								(* evtl. Kommentar *)
			IF zeile [i+1] = '-' THEN
				IF i = anf THEN							(* Zeile nur aus Kommentar *)
					INC (InformationsEinheiten) ;
				END ;
				i := end ;							(* Kommentar entfernen *)
			ELSE
				put ('-') ;
			END ;
		|
		'+' :								(* evtl. Ersetzung *)
			IF zeile [i+1] = '(' THEN						(* Ersetzung *)
				Ersetzung ;
			ELSE
				put ('+') ;
			END ;
		|
		':' :								(* zweite Kommentar-Form *)
			IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
				put (' ') ;
			END ;
			ch := ' ' ;
			REPEAT
				IF ch = ';' THEN						(* Kommentar-Ende *)
					BREAK ;
				END ;
				INC (i) ;
			UNTIL i > end ;
		|
		'a'..'z' :							(* in Großbuchstaben speichern *)
			put (CHR(ORD(ch) BAND 0DFH)) ;
		|
		',' :								(* Komma als Befehlstrenner *)
			INC (i) ;
			WHILE (i <= end) AND (zeile [i] <= ' ') DO
				INC (i) ;
			END ;
			IF i <= end THEN								(* hinter dem Komma folgt noch etwas *)
												(* Befehl schonmal auswerten, um danach den nächsten angehen zu können *)

				put (0C) ;
				IF NOT GetEinzelBefehl (zwZeile) THEN
					RETURN TRUE ;
				END ;
				IF protausgabe THEN
					FileIO.WriteString (prot, CHR(9)) ;
				END ;
				AktZeile := '' ;						(* nur beim ersten Befehl der Zeile für Debugger abspeichern *)
				j := 0 ;							(* nächsten Befehl beginnen *)
				IncAblageAdresse ;
				DEC (i) ;
			END ;
		|
		'(' :
			IF (j = 0) OR (zwZeile [letztesBefehlsZeichen] = '=') THEN		(* in Klammern Bitfolge-Angaben *)
				BitfeldKonstante ;

			ELSIF CheckSonderBefehl () THEN
				SonderBefehl ;

			ELSIF MakroName (zwZeile [0..j-2]) THEN
				SonderBefehl ;

       			ELSIF NOT imLiteral THEN				(* Literal *)
				imLiteral := TRUE ;

				ZeileEintragen (AblageAdresse, AktZeile) ;			(* Zeile merken für Debugger *)

				LiteralErster := TRUE ;
				IF protausgabe THEN
					FileIO.WriteString (prot, CHR(9)) ;
					protstuelpAbschluss ;
				END ;
				IF ((i+1) >= end) OR NOT GetBefehlsZeile (zeile [i+1 .. end-1]) THEN		(* Rekursion für Literal-Befehle *)
					imLiteral := FALSE ;
					RETURN TRUE ;
				END ;
				IF protausgabe THEN
					FileIO.WriteString (prot, CHR(9)) ;
				END ;
				IF zwZeile [j-1] > ' ' THEN
					put (' ') ;
				END ;
				put ("'") ;
				FOR k := 1 TO 6 DO
					put (tetrade [(LiteralAdresse SHR 20) BAND 0FH]) ;
					LiteralAdresse := LiteralAdresse SHL 4 ;
				END ;
				put ("'") ;
				put (0C) ;
				IF protausgabe AND (DRUCK > 2) THEN
					protzeile := zwZeile ;
				END ;
				i := end ;
				imLiteral := FALSE ;
			ELSE							(* Literal im Literal unzulässig *)
				IF (j > 0) AND (zwZeile [j-1] > ' ') THEN
					put (' ') ;
				END ;
				put ('(') ;
			END ;
		|
		')' :
			put (0C) ;
			IF GetEinzelBefehl (zwZeile) THEN
				IncAblageAdresse ;
				imLiteral := FALSE ;
				RETURN TRUE ;
			END ;
			imLiteral := FALSE ;
	    		IncAblageAdresse ;
			RETURN TRUE ;
		ELSE
			put (ch) ;						(* sonstiges Zeichen speichern *)
		END ;
		INC (i) ;
		IF i > end THEN
			EXIT ;
		END ;
	END ;
	put (0C) ;

	IF letztesBefehlsZeichen > 0 THEN
		befname := zwZeile [0..letztesBefehlsZeichen] ;
	ELSE
		befname := zwZeile ;
	END ;
	IF MakroName (befname) THEN
		IF letztesBefehlsZeichen > 0 THEN
			MakroAufruf (zwZeile [letztesBefehlsZeichen+2..HIGH(zwZeile)]) ;
		ELSE
			MakroAufruf ('') ;
		END ;
		RETURN TRUE ;
	END ;

	IF GetEinzelBefehl (zwZeile) THEN
    		IncAblageAdresse ;
		RETURN TRUE ;
	END ;

	IncAblageAdresse ;
	protstuelpAbschluss ;
	RETURN TRUE ;
END GetBefehlsZeile ;


PROCEDURE FORMbefehl (formalparam, param : ARRAY OF CHAR) ;
	VAR	par1,
		par2,
		par3 :		ARRAY [0..31] OF CHAR ;
		fIndex,
		pIndex :	CARDINAL ;
		b :		BOOLEAN ;
BEGIN
	fIndex := 0 ;
	pIndex := 0 ;
	IF debug.AssemblerProtokoll THEN
		TraceF ('*FORM %s -> %s', formalparam, param) ;
	END ;
	IF ExStrings.GetNextItem (formalparam, fIndex, par1, '(') THEN			(* Makroname gefunden *)
		IF NOT ExStrings.GetNextItem (param, pIndex, par3, '(') THEN
			SyntaxWarnung ('makro (p1,p2 ... erwartet') ;
			RETURN
		END ;
		LOOP

			IF  ExStrings.GetNextItem (formalparam, fIndex, par2, ',)') THEN
				IF param [pIndex] = '(' THEN				(* Liste als Parameter *)
					b := ExStrings.GetNextItem (param, pIndex, par3, ')') ;
					Strings.Append (')', par3) ;
				ELSE
					b := ExStrings.GetNextItem (param, pIndex, par3, ',)') ;
				END ;
				IF b THEN
					TRIM (par2) ;
					TRIM (par3) ;
					Strings.Capitalize (par2) ;

					IF debug.AssemblerProtokoll THEN
						TraceF ('*par %s -> %s', par2, par3) ;
					END ;
					ErsetzungEintragen (par2, par3) ;
				END ;
			ELSE
				EXIT ;
			END ;
		END ;
	ELSE
		SyntaxWarnung ('FORM makro (p1,p2 ... erwartet') ;
	END ;
END FORMbefehl ;


PROCEDURE MakroAufruf (param : ARRAY OF CHAR) ;
	VAR	zeile :		ARRAY [0..255] OF CHAR ;
		befname :	ARRAY [0..255] OF CHAR ;
		merkprot,
		merk :		BOOLEAN ;
BEGIN
	merk := inMakroAufruf ;
	merkprot := protausgabe ;
	protstuelpAbschluss ;
	IF DRUCK < 3 THEN
		protausgabe := FALSE ;
	END ;
	ErsetzungEintragen (AktMakro ^ .name [0.. LENGTH(AktMakro ^ .name)], param) ;		(* kompletten Makro-Parameter als Ersetzung eintragen *)
	WHILE GetNextMAKROzeile (zeile) DO
		inMakroAufruf := TRUE ;
		befname := zeile ;
		TRIM (befname) ;
		IF ExStrings.EqualI (befname [0..3], 'FORM') THEN	(* FORM - Befehl *)
			FORMbefehl (befname [5..HIGH(befname)], param) ;
		ELSE
			ErsetzungVorhanden := TRUE ;
			AktZeile := zeile ;
			IF NOT GetBefehlsZeile (zeile) THEN
				BREAK ;
			END ;
		END ;
	END ;
	inMakroAufruf := merk ;		(* falls geschachtelter Makroaufruf *)
	protausgabe := merkprot ;
END MakroAufruf ;


PROCEDURE IncAblageAdresse ;
BEGIN
	AblageAdresse := AblageAdresse + AblageLaenge ;
	ZonenPegel [AktZonenArt] := AblageAdresse ;
END IncAblageAdresse ;


PROCEDURE InitAsm ;							(* nur vor dem Gesamtanfang des Assemblers, wird immer auch beim Start vom Interpreter aufgerufen *)
	VAR	i :	CARDINAL ;
		z :	tZonenArt ;
BEGIN
	AlleNamenLoeschen ;

	InitZonenPegel [ZoneK] := 800H ;
	InitZonenPegel [ZoneV] := 8000H ;
	InitZonenPegel [ZoneB] := 10000H ;
	InitZonenPegel [ZoneB1] := 1F000H ;
	InitZonenPegel [ZoneD] := 20000H ;
	FOR z := MIN (tZonenArtDyn) TO MAX (tZonenArtDyn) DO
		InitZonenPegel [z] := 2F000H ;
		ZonenNamen [z] := '' ;
	END ;
	InitZonenPegel [ZoneF] := 30000H ;

	InitZonenAdresse := 800H ;					(* für explizite Anordnung mit ZONE_x = DSP nK *)

	REPLanzahl := 0 ;
	DRUCK := 2 ;
	imWIED := FALSE ;
	inVersion := FALSE ;
	inMakroAufruf := FALSE ;
	inWIEDausfuehrung := FALSE ;
	WIEDname := NIL ;
	AktWB := NIL ;
	ErsWB := NIL ;
	MakroWB := NIL ;
	CzoneWB := NIL ;
	StartAdresse := 1000000H ;
	AlarmAdresse := 0 ;
	StartX := 0 ;
	StartU := 254 ;
	PseudoGebiete := '' ;
	frischAssembliert := FALSE ;
	ImpliziterIndexVerboten := FALSE ;
	AbsoluterIndexVerboten := FALSE ;

	VORBESda := FALSE ;

	UnterdrueckeVerbotenerBezug := FALSE ;
	UnterdrueckeUnbekannt := FALSE ;

	inMakroDefinition := FALSE ;

	FOR i := 0 TO HIGH (WIEDwerte) DO
		WIEDwerte [i] := '' ;
	END ;

	IF debug.AssemblerProtokoll THEN
		TraceF ('*** Initialisierung') ;
	END ;
END InitAsm ;


PROCEDURE InitProgrammlauf ;						(* vor Beginn des 1. und vor Beginn des 2. Laufes, wird immer auch beim Start vom Interpreter aufgerufen *)
	VAR	i :	tZonenArt ;
BEGIN
	FOR i := MIN (tZonenArtDyn) TO MAX (tZonenArtDyn) DO
		ZonenNamen [i] := '' ;
	END ;

	FOR i := MIN(tZonenArt) TO MAX(tZonenArt) DO
		ZonenPegel [i] := InitZonenPegel [i] ;
	END ;

	ZonenAdresse := InitZonenAdresse ;				(* für explizite Anordnung mit ZONE_x = DSP nK *)

	RegX := StartX ;
	RegU := StartU ;

	AktZonenArt := ZoneB ;
	AblageAdresse := ZonenPegel [AktZonenArt] ;
	StartAdresse := AblageAdresse ;
	AlarmAdresse := 0 ;

	ZonenPegel [ZoneUndef] := AblageAdresse ;

	FOR i := MIN(tZonenArt) TO MAX(tZonenArt) DO
		ZonenAnfang [i] := ZonenPegel [i] ;
	END ;

	ZeilenNummer := 0 ;
	LaufEnde := FALSE ;
	IndexPegel := 0 ;
	MaxIndexPegel := 0 ;

	Operatoren [AktOperator].ON := AktQuellName ;
	Operatoren [AktOperator].OLN := AktQuellName ;

END InitProgrammlauf ;


PROCEDURE LaufKern (WBneu : BOOLEAN) : BOOLEAN ;
	VAR	name,
		zwname :	ARRAY [0..255] OF CHAR ;
		quelle :	FileFunc.File ;
		z :		tZonenArt ;
		buffer :	ARRAY [0..8191] OF CHAR ;

BEGIN

	IF WBneu THEN

		FOR z := MIN (tZonenArt) TO MAX (tZonenArt) DO
			dynZonenAblage [z] := ZoneUndef ;
		END ;

		imInterpreter := FALSE ;

		FzoneAktiv := FALSE ;
		STARRaktiv := FALSE ;
		AktCzoneArt := ZoneUndef ;

		AktQuelle := QuelleEintragen (AktQuellName) ;
		AktWB := WoerterbuchEintragen (AktQuellName) ;
		ErsWB := WoerterbuchEintragen ('*Ersetzungen*') ;
		MakroWB := WoerterbuchEintragen ('*Makros*') ;
		AktSegmWB := NIL ;

		inWIEDausfuehrung := FALSE ;
		inMakroAufruf := FALSE ;
		inVersion := FALSE ;

		AlarmGewesen := FALSE ;
	END ;

	FileFunc.ConstructFileName (AktQuellName, lastDateiname, name) ;

	IF WBneu AND imLauf2 AND NOT protausgabeAktiv THEN
		FileFunc.ConstructFileName (ListingDirectory, lastDateiname, zwname) ;
		FileFunc.ConstructFileName (TasListingExtension, zwname, protname) ;
		InitProtokoll ;
		protausgabe := protausgabeAktiv ;
	END ;

	FileFunc.OpenFileEx (quelle, name, FileFunc.ReadOnlyDenyWrite, FileFunc.FileUseInfoSet { FileFunc.SequentialAccess } ) ;
	ZeilenNummer := 0 ;
	IF quelle.status = 0 THEN
		FileFunc.SetFileBuffer (quelle, buffer) ;
		WHILE FileIO.ReadLn (quelle, AktZeile) DO
			INC (ZeilenNummer) ;
			IF NOT GetBefehlsZeile (AktZeile) THEN
				FileFunc.CloseFile (quelle) ;

				REPLanzahl := 0 ;
				imWIED := FALSE ;
				inVersion := FALSE ;
				inMakroAufruf := FALSE ;
				inWIEDausfuehrung := FALSE ;

				RETURN FALSE ;
			END ;
			IF LaufEnde THEN
				BREAK ;
			END ;
		END ;
		FileFunc.CloseFile (quelle) ;

		REPLanzahl := 0 ;
		imWIED := FALSE ;
		inVersion := FALSE ;
		inMakroAufruf := FALSE ;
		inWIEDausfuehrung := FALSE ;

		RETURN TRUE ;
	END ;
	zwname := lastDateiname ;
	lastDateiname := name ;
	DateiFehlt ;
	lastDateiname := zwname ;

	REPLanzahl := 0 ;
	imWIED := FALSE ;
	inVersion := FALSE ;
	inMakroAufruf := FALSE ;
	inWIEDausfuehrung := FALSE ;

	RETURN FALSE ;
END LaufKern ;


PROCEDURE Lauf1 (dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	ok :	BOOLEAN ;
BEGIN
	IF dateiname [0] > ' ' THEN
		lastDateiname := dateiname ;
	END ;

	InitAsm ;

	WITH Operatoren [AktOperator+1] DO				(* kein Startsatz da, weil Start 'zu Fuß' *)
		SteuerInformation.Kopf.LNG := 0 ;
		SteuerInformation.Kopf.TYP := 0 ;
	END ;

	IF NOT Strings.Equal (lastASMname, lastDateiname) THEN
		lastASMname := lastDateiname ;
	END ;

	Terminal.WriteF ('\n\nStart Assembler   %s\n', lastDateiname) ;

	TraceF ('***** ASM Lauf 1 *****') ;
	imLauf2 := FALSE ;

	InitProgrammlauf ;

	CzoneWB := WoerterbuchEintragen ('*CZONEN*') ;
	InclDateien := WoerterbuchEintragen ('*INCL*') ;

	ok := LaufKern (TRUE) ;

	NamenLoeschen (CzoneWB ^ .nam0) ;
	NamenLoeschen (InclDateien ^ .nam0) ;

	RETURN ok ;
END Lauf1 ;


PROCEDURE SpeicherVorbesetzungAusfuehren ;
	VAR	adr :	SpeicherAdresse ;
		z :	tZonenArt ;
		zw :	BOOLEAN ;

	PROCEDURE vorb (von, bis : SpeicherAdresse) ;
		VAR	i :	CARDINAL ;
	BEGIN
		IF bis <> 0 THEN
			IF (bis BAND 7FFH) <> 0 THEN
				bis := (bis + 7FEH) BAND 0FFF800H ;		(* Seitenrest vorlöschen *)
			END ;
			TraceF ("vorbes %'05h - %'05h", von, bis) ;
			FOR i := von TO bis-1 BY 2 DO
				PutGanzwort (i, VorbesTK, VorbesInhalt) ;
				IF SpeicherschutzAlarmGewesen THEN
					BREAK ;
				END ;
			END ;
		END ;
	END vorb ;

BEGIN
	zw := debug.Speichern ;
	debug.Speichern := FALSE ;

	FOR z := ZoneK TO ZoneD DO
		vorb (ZonenAnfang [z], ZonenPegel [z]) ;
	END ;

	FOR z := MIN (tZonenArtDyn) TO MAX (tZonenArtDyn) DO
		vorb (ZonenAnfang [z], ZonenPegel [z]) ;
	END ;

	debug.Speichern := zw ;

END SpeicherVorbesetzungAusfuehren ;


PROCEDURE SpeicherBelegungAusfuehren ;
	VAR	adr :	SpeicherAdresse ;
		zz :	tZonenArt ;

	PROCEDURE ZoneBelegen (z : tZonenArt ; SS : BOOLEAN) ;
		VAR	adr :	SpeicherAdresse ;
	BEGIN
		IF ZonenPegel [z] > ZonenAnfang [z] THEN	(* Zone ist belegt *)
			FOR adr := ZonenAnfang [z] TO ZonenPegel [z] - 1 BY HalbworteInSeite DO
				IF TestAdresse (adr) THEN
					Terminal.WriteF ("+++++ Adressraum-Überschneidung : %'05h", adr) ;
					IF protausgabe THEN
						FileIO.WriteF (prot, "\n+++++ Adressraum-Überschneidung : %'05h\n", adr) ;
					END ;
				END ;
				SetExist (adr, TRUE) ;
				SetProtect (adr, SS) ;
			END ;
		END ;
	END ZoneBelegen ;

BEGIN
	FOR adr := 0 TO (AktMaxBelegteGrossSeite + 1) * HalbworteInGrossSeite - 1 BY HalbworteInSeite DO
		SetExist (adr, FALSE) ;									(* erstmal alles auf Lücke setzen *)
	END ;

	ZoneBelegen (ZoneK, TRUE) ;
	ZoneBelegen (ZoneV, FALSE) ;
	ZoneBelegen (ZoneB, TRUE) ;
	ZoneBelegen (ZoneB1, TRUE) ;
	ZoneBelegen (ZoneD, FALSE) ;

	FOR zz := MIN(tZonenArtDyn) TO MAX (tZonenArtDyn) DO
		ZoneBelegen (zz, FALSE) ;
	END ;

END SpeicherBelegungAusfuehren ;


PROCEDURE Lauf2 () : BOOLEAN ;
	VAR	adr :	SpeicherAdresse ;
		rm,
		merk :	BOOLEAN ;
		z :	tZonenArt ;
BEGIN
	TraceF ('***** ASM Lauf 2 *****') ;

	imLauf2 := TRUE ;

	Vorbes2 ;							(* Standard-Speicher vorbesetzen *)

	merk := SchreibschutzAktiv ;
	SchreibschutzAktiv := FALSE ;


	IF VORBESda THEN
		SpeicherVorbesetzungAusfuehren ;
	END ;

	InformationsEinheiten := 0 ;
	AnzFehlermeldungen := 0 ;

	InitProgrammlauf ;

	rm := LaufKern (TRUE) ;

	IF StartAdresse = 1000000H THEN
		SyntaxWarnung ('START nicht definiert') ;
	ELSIF StartX = 0 THEN
		SyntaxWarnung ('XBASIS nicht definiert') ;
	ELSIF StartU = 0 THEN
		SyntaxWarnung ('UNTPR nicht definiert') ;
	ELSIF AlarmAdresse = 0 THEN
		SyntaxWarnung ('ALARM nicht definiert') ;
	ELSIF rm THEN
		frischAssembliert := TRUE ;
	END ;

	IF AnzFehlermeldungen > 0 THEN
		Terminal.WriteF ('\n\n%c Fehlermeldung(en)\n', AnzFehlermeldungen) ;
	END ;

	SpeicherBelegungAusfuehren ;


	SchreibschutzAktiv := merk ;

	FOR z := MIN (tZonenArt) TO MAX (tZonenArt) DO
		InitZonenPegel [z] := ZonenPegel [z] ;
	END ;

	InitZonenAdresse := ZonenAdresse ;

	IF protausgabeAktiv THEN
		IF AnzFehlermeldungen > 0 THEN
			FileIO.WriteF (prot, '\n\n%c Fehlermeldung(en)\n', AnzFehlermeldungen) ;
		END ;
		IF IndexPegel = 0 THEN
			FileIO.WriteF (prot, "\n\nINDEXPEGEL\t-") ;
		ELSE
			FileIO.WriteF (prot, "\n\nINDEXPEGEL\t%c", MaxIndexPegel-1) ;
		END ;
		FileIO.WriteF (prot, "\nI.E.-ANZAHL\t%c\n", InformationsEinheiten) ;
	END ;

	ExitProtokoll ;

	RETURN rm  ;
END Lauf2 ;


PROCEDURE InitProtokoll ;
BEGIN
	IF FileIO.CreateFile (prot, protname) THEN
		protausgabeAktiv := TRUE ;
		protausgabe := TRUE ;
	ELSE
		Terminal.WriteF ('\n+++++ TAS-Protokolldatei %s nicht erzeugbar\n', protname) ;
	END ;
END InitProtokoll ;


PROCEDURE ExitProtokoll ;
BEGIN
	IF protausgabeAktiv THEN
		protausgabeAktiv := FALSE ;
		protausgabe := FALSE ;
		FileIO.CloseFile (prot) ;
	END ;
END ExitProtokoll ;


PROCEDURE Abspeichern (dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	zeile :	ARRAY [0..255] OF CHAR ;
		ch :	CHAR ;
BEGIN
	IF dateiname [0] > ' ' THEN
		lastDateiname := dateiname ;
	END ;
	IF GetFirstZeile (zeile) THEN
		IF FileIO.CreateFile (ziel, lastDateiname) THEN
			REPEAT
				ch := CHR(9) ;
				FileIO.WriteByte (ziel, ch) ;
				FileIO.WriteLn (ziel, zeile) ;
			UNTIL NOT GetNextZeile (zeile) ;
			FileIO.CloseFile (ziel) ;
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END Abspeichern ;


PROCEDURE Editor (dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	status :	CARDINAL ;
		quelle :	FileFunc.File ;
		str :		ARRAY [0..255] OF CHAR ;
		i :		CARDINAL ;
		gef :		BOOLEAN ;
BEGIN
	IF dateiname [0] > ' ' THEN
		lastDateiname := dateiname ;
	END ;
	IF Menue.Editor (lastDateiname) THEN
		RETURN TRUE ;
	END ;
	DateiFehlt ;
	RETURN FALSE ;
END Editor ;


PROCEDURE Init ;
BEGIN
	lastDateiname := '' ;
	lastASMname := '' ;
	InitAsm ;
END Init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;

%END

END Assembler.
