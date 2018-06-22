
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Namen;

(*	06.05.16	*)

IMPORT Strings ;

FROM Storage IMPORT
	ALLOCATE, DEALLOCATE ;

FROM Struktur IMPORT
	SpeicherAdresse ;

IMPORT Terminal ;

FROM Trace IMPORT
	TraceF ;

%IF %NOT WEB %THEN

FROM AsmDisasm IMPORT
	BefehlGueltig ;
%END

VAR

	AktREPLzeile,
	AktMAKROzeile,
	AktWIEDzeile,
	REPL0,
	MAKRO0,
	WIED0,
	Ersetzung0,
	nameV :			pName ;

	initialisiert :		BOOLEAN = FALSE ;



%IF %NOT WEB %THEN

PROCEDURE WBsuchen (WB : pName ; Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
BEGIN
	p := WB ;
	nameV := NIL ;
	WHILE p <> NIL DO
		IF Strings.Equal (Name, p ^ .name [0..LENGTH(p ^ .name)]) THEN
			NameGefunden := TRUE ;
			RETURN p ;
		END ;
		nameV := p ;
		p := p ^ .next ;
	END ;
	NameGefunden := FALSE ;
	RETURN NIL ;
END WBsuchen ;


PROCEDURE NewElem (Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
		l :	CARDINAL ;
BEGIN

	l := LENGTH (Name) ;
	ALLOCATE (p, SIZE (p ^ ) - SIZE (p ^ .name) + l + 1) ;
	p ^ .next := NIL ;
	p ^ .verweis := NIL ;
	p ^ .typ := ElemUndef ;
	p ^ .wert := 0 ;
	p ^ .name [0..l] := Name [0..l] ;
	RETURN p ;
END NewElem ;


PROCEDURE DeleteElem (VAR p : pName) ;
BEGIN
	IF p <> NIL THEN
		DEALLOCATE (p, SIZE (p ^ ) - SIZE (p ^ .name) + LENGTH (p ^ .name) + 1) ;
	END ;
END DeleteElem ;


PROCEDURE WBeintragen (VAR WB : pName ; Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
BEGIN
	Strings.Capitalize (Name) ;
	p := WBsuchen (WB, Name) ;
	IF p <> NIL THEN
		NeuAngelegt := FALSE ;
		RETURN p ;
	END ;

	NeuAngelegt := TRUE ;

	p := NewElem (Name) ;

	IF WB = NIL THEN
		WB := p ;
	ELSIF nameV <> NIL THEN
		nameV ^ .next := p ;
	END ;
	RETURN p ;
END WBeintragen ;


PROCEDURE NamenEintragen (Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
		str :	ARRAY [0..63] OF CHAR ;
BEGIN
	IF BefehlGueltig (Name) THEN
		TraceF ('\n*** "%s" ist als Name kritisch', Name) ;
		p := WBeintragen (AktWB ^ .nam0, Name) ;
	ELSE
		p := WBeintragen (AktWB ^ .nam0, Name) ;
	END ;
	p ^ .typ := ElemName ;
	p ^ .wb := AktWB ;
	RETURN p ;
END NamenEintragen ;


PROCEDURE CZONEeintragen (Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
		str :	ARRAY [0..63] OF CHAR ;
BEGIN
	IF BefehlGueltig (Name) THEN
		TraceF ('\n*** "%s" ist als Name kritisch', Name) ;
		p := WBeintragen (CzoneWB ^ .nam0, Name) ;
	ELSE
		p := WBeintragen (CzoneWB ^ .nam0, Name) ;
	END ;
	p ^ .typ := ElemName ;
	p ^ .wb := CzoneWB ;
	RETURN p ;
END CZONEeintragen ;


PROCEDURE INCLeintragen (Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
		str :	ARRAY [0..63] OF CHAR ;
BEGIN
	p := WBeintragen (InclDateien ^ .nam0, Name) ;
	p ^ .typ := ElemName ;
	p ^ .wb := InclDateien ;
	RETURN p ;
END INCLeintragen ;


PROCEDURE ExternNamenEintragen (Name : ARRAY OF CHAR ; verweis : pName) : pName ;
	VAR	p :	pName ;
BEGIN
	p := WBeintragen (AktWB ^ .nam0, Name) ;
	p ^ .typ := ElemVerweis ;
	p ^ .echtname := verweis;
	RETURN p ;
END ExternNamenEintragen ;


PROCEDURE NamenSuchen (Name : ARRAY OF CHAR) : pName ;
	VAR	p,
		wb :		pName ;
		posOfPattern :	CARDINAL ;
		patternFound :	BOOLEAN ;
BEGIN
	WarVerweis := FALSE ;
	Strings.FindNext('.', Name, 1, patternFound, posOfPattern) ;
	IF patternFound AND (posOfPattern = LENGTH(Name)-1) THEN			(* Punkt am Namensende entfernen *)
		Name [posOfPattern] := 0C ;
		patternFound := FALSE ;
	END ;
	IF patternFound THEN								(* Name ist 'Quelle.Name' *)
		wb := WBsuchen (woerterbuch0, Name [0..posOfPattern-1]) ;		(* Wörterbuch gefunden *)
		IF wb <> NIL THEN
			p := WBsuchen (wb ^ .nam0, Name [posOfPattern+1 .. HIGH(Name)]) ;	(* Namen in diesem Wörterbuch suchen *)
		ELSE
			p := WBsuchen (AktWB ^ .nam0, Name) ;
		END ;
	ELSIF AktSegmWB <> NIL THEN							(* sonst im aktuellen lokalen SEGM-Wörterbuch suchen *)
		p := WBsuchen (AktSegmWB ^ .nam0, Name) ;
		IF p = NIL THEN
			IF AktWB <> NIL THEN						(* sonst im aktuellen globalen Wörterbuch suchen *)
				p := WBsuchen (AktWB ^ .nam0, Name) ;
			END ;
		END ;
	ELSIF AktWB <> NIL THEN
		p := WBsuchen (AktWB ^ .nam0, Name) ;					(* sonst im aktuellen Wörterbuch suchen *)
	ELSE
		p := NIL ;
	END ;
	IF p <> NIL THEN
		IF p ^ .typ = ElemVerweis THEN						(* Name ist ein Verweis *)
			p := p ^ .echtname ;						(* dann echtes Element holen *)
			WarVerweis := TRUE ;
		END ;
	END ;
	RETURN p ;
END NamenSuchen ;


PROCEDURE ErsetzungsInhaltLoeschen (p : pName) ;
	VAR	p2 :	pName ;
BEGIN
	p2 := p ^ .inhalt ;
	p := Ersetzung0 ;
	nameV := NIL ;
	WHILE (p <> NIL) AND (p <> p2) DO
		nameV := p ;
		p := p ^ .next ;
	END ;
	IF nameV = NIL THEN
		Ersetzung0 := p2 ^ .next ;
	ELSE
		nameV ^ .next := p2 ^ .next ;
	END ;
	DeleteElem (p) ;		(* Inhaltselement löschen *)
END ErsetzungsInhaltLoeschen ;


PROCEDURE ErsetzungEintragen (Name : ARRAY OF CHAR ; Inhalt : ARRAY OF CHAR) : pName ;
	VAR	p,
		p2 :	pName ;
BEGIN
	p := WBsuchen (ErsWB ^ .nam0, Name) ;
	IF p <> NIL THEN			(* altes Inhaltselement löschen *)
		ErsetzungsInhaltLoeschen (p) ;
	ELSE
		p := WBeintragen (ErsWB ^ .nam0, Name) ;
	END ;
	PutErsetzungsInhalt (p, Inhalt) ;
	RETURN p ;
END ErsetzungEintragen ;


PROCEDURE ErsetzungLoeschen (Name : ARRAY OF CHAR) ;
	VAR	p :	pName ;
BEGIN
	p :=  WBsuchen (ErsWB ^ .nam0, Name) ;
	IF p <> NIL THEN
		IF nameV <> NIL THEN
			nameV ^ .next := p ^ .next ;
		END ;
		ErsetzungsInhaltLoeschen (p) ;
		DeleteElem (p) ;		(* Namenselement löschen *)
	END ;
END ErsetzungLoeschen ;


PROCEDURE GetErsetzungsInhalt (Name : ARRAY OF CHAR ; VAR inhalt : ARRAY OF CHAR) ;
	VAR	p :	pName ;
BEGIN
	IF ErsWB <> NIL THEN
		p := WBsuchen (ErsWB ^ .nam0, Name) ;
	ELSE
		p := NIL ;
	END ;
	IF p = NIL THEN
		inhalt := '???' ;
	ELSE
		p := p ^ .inhalt ;
		IF p = NIL THEN
			inhalt := '' ;
		ELSE
			inhalt := p ^ .name [0..LENGTH(p ^ .name)] ;
		END ;
	END ;
END GetErsetzungsInhalt ;


PROCEDURE PutErsetzungsInhalt (p : pName ; inhalt : ARRAY OF CHAR) ;
BEGIN
	IF p <> NIL THEN
		p ^ .typ := ElemErsetzung ;
		p ^ .inhalt := TextEintragen (Ersetzung0, inhalt) ;
	END ;
END PutErsetzungsInhalt ;


PROCEDURE WoerterbuchEintragen (Name : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
BEGIN
	p := WBeintragen (woerterbuch0, Name) ;
	p ^ .typ := ElemWoerterbuch ;
	RETURN p ;
END WoerterbuchEintragen ;


PROCEDURE QuelleEintragen (Name : ARRAY OF CHAR) : pName ;
BEGIN
	AktQuelle :=  WBeintragen (quelle0, Name) ;
	AktQuelle ^ .typ := ElemQuelle ;
	RETURN AktQuelle ;
END QuelleEintragen ;


PROCEDURE ZeileSuchen (adr : SpeicherAdresse) : pName ;
	VAR	p :	pName ;
BEGIN
	p := QuellZeile0 ;
	WHILE p <> NIL DO
		IF p ^ .wert = adr THEN
			RETURN p ;
		END ;
		nameV := p ;
		p := p ^ .next ;
	END ;
	RETURN NIL ;
END ZeileSuchen ;


PROCEDURE ZeilenText (adr : CARDINAL ; VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
	VAR	p :	pName ;
		l :	CARDINAL ;
BEGIN
	p := ZeileSuchen (adr) ;
	IF p = NIL THEN
		RETURN FALSE ;
	END ;
	l := LENGTH (p ^ .name) ;
	zeile := p ^ .name [0..l] ;
	RETURN TRUE ;
END ZeilenText ;


PROCEDURE GetNextZeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF AktTextZeile = NIL THEN
		AktTextZeile := QuellZeile0 ;
	ELSE
		AktTextZeile := AktTextZeile ^ .next ;
	END ;
	IF AktTextZeile = NIL THEN
		RETURN FALSE ;
	END ;
	zeile := AktTextZeile ^ .name [0..LENGTH(AktTextZeile ^ .name)] ;
	RETURN TRUE ;
END GetNextZeile ;


PROCEDURE GetFirstZeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	AktTextZeile := NIL ;
	RETURN GetNextZeile (zeile) ;
END GetFirstZeile ;


PROCEDURE ZeileEintragen (adr : SpeicherAdresse ; text : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
		l :	CARDINAL ;
BEGIN
	p := ZeileSuchen (adr) ;
	IF p <> NIL THEN
		p ^ .wert := adr ;
		RETURN p ;
	END ;

	p := NewElem (text) ;
	p ^ .wert := adr ;
	p ^ .typ := ElemZeile ;
	p ^ .quelle := AktQuelle ;
	IF QuellZeile0 = NIL THEN
		QuellZeile0 := p ;
	ELSE
		nameV ^ .next := p ;
	END ;
	AktTextZeile := p ;
	RETURN p ;
END ZeileEintragen ;


PROCEDURE GetNextREPLzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF AktREPLzeile = NIL THEN
		AktREPLzeile := REPL0 ;
	ELSE
		AktREPLzeile := AktREPLzeile ^ .next ;
	END ;
	IF AktREPLzeile = NIL THEN
		RETURN FALSE ;
	END ;
	zeile := AktREPLzeile ^ .name [0..LENGTH(AktREPLzeile ^ .name)] ;
	RETURN TRUE ;
END GetNextREPLzeile ;


PROCEDURE GetFirstREPLzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	AktREPLzeile := NIL ;
	RETURN GetNextREPLzeile (zeile) ;
END GetFirstREPLzeile ;


PROCEDURE TextEintragen (VAR basis : pName ; zeile : ARRAY OF CHAR) : pName ;
	VAR	p :	pName ;
BEGIN
	p := NewElem (zeile) ;

	IF basis = NIL THEN
		basis := p ;
	ELSE
		nameV := basis ;
		WHILE nameV ^ .next <> NIL DO
			nameV := nameV ^ .next ;
		END ;
		nameV ^ .next := p ;
	END ;
	p ^ .typ := ElemZeile ;
	p ^ .quelle := AktQuelle ;
	RETURN p ;
END TextEintragen ;


PROCEDURE REPLeintragen (zeile : ARRAY OF CHAR) ;
BEGIN
	TextEintragen (REPL0, zeile) ;
END REPLeintragen ;


PROCEDURE REPLloeschen ;
BEGIN
	NamenLoeschen (REPL0) ;
END REPLloeschen ;


PROCEDURE GetNextWIEDzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF AktWIEDzeile = NIL THEN
		AktWIEDzeile := WIED0 ;
	ELSE
		AktWIEDzeile := AktWIEDzeile ^ .next ;
	END ;
	IF AktWIEDzeile = NIL THEN
		RETURN FALSE ;
	END ;
	zeile := AktWIEDzeile ^ .name [0..LENGTH(AktWIEDzeile ^ .name)] ;
	RETURN TRUE ;
END GetNextWIEDzeile ;


PROCEDURE GetFirstWIEDzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	AktWIEDzeile := NIL ;
	RETURN GetNextWIEDzeile (zeile) ;
END GetFirstWIEDzeile ;


PROCEDURE WIEDeintragen (zeile : ARRAY OF CHAR) ;
BEGIN
	TextEintragen (WIED0, zeile) ;
END WIEDeintragen ;


PROCEDURE WIEDloeschen ;
BEGIN
	NamenLoeschen (WIED0) ;
END WIEDloeschen ;


PROCEDURE GetNextMAKROzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	IF AktMAKROzeile = NIL THEN
		AktMAKROzeile := AktMakro ^ .zeile0 ;
	ELSE
		AktMAKROzeile := AktMAKROzeile ^ .next ;
	END ;
	IF AktMAKROzeile = NIL THEN
		RETURN FALSE ;
	END ;
	zeile := AktMAKROzeile ^ .name [0..LENGTH(AktMAKROzeile ^ .name)] ;
	RETURN TRUE ;
END GetNextMAKROzeile ;


PROCEDURE GetFirstMAKROzeile (VAR zeile : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	AktMAKROzeile := NIL ;
	RETURN GetNextMAKROzeile (zeile) ;
END GetFirstMAKROzeile ;


PROCEDURE MAKROzeileEintragen (zeile : ARRAY OF CHAR) ;
	VAR	p :	pName ;
BEGIN
	p := NewElem (zeile) ;

	nameV := AktMakro ^ .zeile0 ;
	IF nameV = NIL THEN
		AktMakro ^ .zeile0 := p ;
	ELSE
		WHILE nameV ^ .next <> NIL DO
			nameV := nameV ^ .next ;
		END ;
		nameV ^ .next := p ;
	END ;

	p ^ .typ := ElemZeile ;
	p ^ .quelle := AktMakro ;
END MAKROzeileEintragen ;


PROCEDURE MakroErzeugen (name : ARRAY OF CHAR) ;
BEGIN
	AktMakro := WBeintragen (MakroWB, name) ;
	AktMakro ^ .typ := ElemMakro ;
END MakroErzeugen ;


PROCEDURE MAKROloeschen ;
BEGIN
	NamenLoeschen (MAKRO0) ;
END MAKROloeschen ;


PROCEDURE MakroName (name : ARRAY OF CHAR) : BOOLEAN ;
	VAR	p :	pName ;
BEGIN
	p := WBsuchen (MakroWB, name) ;
	IF p = NIL THEN
		RETURN FALSE ;
	END ;
	AktMakro := p ;
	AktMAKROzeile := NIL ;
	RETURN TRUE ;
END MakroName ;


PROCEDURE NamenLoeschen (VAR name0 : pName) ;
	VAR	p :	pName ;
BEGIN
	WHILE name0 <> NIL DO
		p := name0 ;
		name0 := name0 ^ .next ;
		DeleteElem (p) ;
	END ;
END NamenLoeschen ;


PROCEDURE WoerterbuecherLoeschen ;
	VAR	p :	pName ;
BEGIN
	p := woerterbuch0 ;
	WHILE p <> NIL DO
		NamenLoeschen (p ^ .nam0) ;
		p := p ^ .next ;
	END ;
	NamenLoeschen (woerterbuch0) ;
END WoerterbuecherLoeschen ;


PROCEDURE AlleNamenLoeschen ;
BEGIN
	NamenLoeschen (quelle0) ;
	WoerterbuecherLoeschen ;
	NamenLoeschen (QuellZeile0) ;
	NamenLoeschen (REPL0) ;
	NamenLoeschen (Ersetzung0) ;
	WIEDloeschen ;

	AktWB := NIL ;
	AktSegmWB := NIL ;
	CzoneWB := NIL ;
	AktQuelle := NIL ;
END AlleNamenLoeschen ;

%END

PROCEDURE Init ;
BEGIN
	quelle0 := NIL ;
	QuellZeile0 := NIL ;
	woerterbuch0 := NIL ;
	REPL0 := NIL ;
	AktWB := NIL ;
	AktSegmWB := NIL ;
	CzoneWB := NIL ;
	AktQuelle := NIL ;
	WIED0 := NIL ;
	ErsWB := NIL ;
	MakroWB := NIL ;
	Ersetzung0 := NIL ;
END Init ;



BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
END Namen.
