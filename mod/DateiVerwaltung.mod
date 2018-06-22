
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE DateiVerwaltung;

(*	13.06.18	*)


FROM Storage IMPORT
	ALLOCATE,
	DEALLOCATE ;

FROM Strings IMPORT
	Length, Equal ;

FROM Struktur IMPORT * ;

FROM DateiBearbeitung IMPORT
	SetDateityp ;

FROM Trace IMPORT
	TraceF ;

IMPORT Strings, ExStrings, Terminal, Conversions, debug, FileFunc, AbwicklerUp ;


CONST
	cDatentraeger =	'C:\TR440\' ;


	_Lock =		'#User#.lck' ;		(* Lockdatei ist offen, wenn User läuft *)


TYPE

        pDatenbasis =		POINTER TO tDatenbasis ;

	tDBA =			(DBoperatorlauf, DBspeziell, DBnix, DBabschnitt) ;

	tDatenbasis =		RECORD
					prev,
					next :		pDatenbasis ;
					OLK :		CARDINAL ;		(* Eigentümer - Operator *)
					name :		ARRAY [0..5] OF CHAR ;
					DBA :		tDBA ;
				END ;


VAR

	LockDatei :		FileFunc.File ;

	initialisiert :		BOOLEAN = FALSE ;

	Datenbasis0 :		pDatenbasis = NIL ;


PROCEDURE SuchDB (name : ARRAY OF CHAR) : pDatenbasis ;
	VAR	db :	pDatenbasis ;
BEGIN
	db := Datenbasis0 ;
	WHILE db <> NIL DO
		IF Strings.Equal (name, db ^ .name) THEN
			RETURN db ;
		END ;
		db := db ^ .next ;
	END ;
	RETURN NIL ;
END SuchDB ;


PROCEDURE NewDB (name : ARRAY OF CHAR) : pDatenbasis ;
	VAR	db :	pDatenbasis ;
BEGIN
     	db := SuchDB (name) ;
        IF db <> NIL THEN
		RETURN db ;
	END ;

	NEW (db) ;

	db ^ .prev := NIL ;
	db ^ .next := Datenbasis0 ;
	IF Datenbasis0 <> NIL THEN
		Datenbasis0 ^ .prev := db ;
	END ;

	db ^ .OLK := AktOLK ;
	db ^ .name := name ;
	db ^ .DBA := DBoperatorlauf ;
	Datenbasis0 := db ;
	RETURN db ;
END NewDB ;


PROCEDURE DeleteDB (VAR db : pDatenbasis) ;
BEGIN
	IF db ^ .prev = NIL THEN
		Datenbasis0 := db ^ .next ;
	ELSE
		db ^ .prev ^ .next := db ^ .next ;
	END ;
	IF db ^ .next <> NIL THEN
		db ^ .next ^ .prev := db ^ .prev ;
	END ;
	DISPOSE (db) ;
END DeleteDB ;


PROCEDURE CheckDatentraeger () : BOOLEAN ;		(* alle Standard-Verzeichnisse kreieren *)
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT FileFunc.DirExists (Datentraeger) THEN
		IF NOT FileFunc.CreateDirTree (Datentraeger) THEN
			Terminal.WriteLn ;
			Terminal.WriteString ('+++++ Datenträger nicht erzeugbar : ') ;
			Terminal.WriteString (Datentraeger) ;
			DatentraegerOk := FALSE ;
			RETURN FALSE ;
		END ;
	END ;

	CreateBKZ ;

	datnam := Datentraeger ;
	Strings.Append (_OEFDB, datnam) ;
	Strings.Append ('\', datnam) ;
	FileFunc.CreateDirTree (datnam) ;

	IF UserTemporaer [0] <> 0C THEN
		datnam := Datentraeger ;
		Strings.Append (UserTemporaer, datnam) ;
		FileFunc.CreateDirTree (datnam) ;
        END ;

	datnam := Datentraeger ;
	Strings.Append (UserTemporaer, datnam) ;
	Strings.Append (_Lock, datnam) ;
	FileFunc.CreateFile (LockDatei, datnam) ;
	IF LockDatei.status <> 0 THEN
		Terminal.WriteLn ;
		Terminal.WriteString ('+++++ TR440-Instanz läuft bereits : ') ;
		Terminal.WriteString (datnam) ;
		Terminal.WriteLn ;
		Terminal.WriteLn ;
		DatentraegerOk := FALSE ;
		RETURN FALSE ;
	END ;

	DeleteSTDDB ;

	(*
	datnam := Datentraeger ;
	Strings.Append (_STDDB, datnam) ;
	Strings.Append ('\', datnam) ;
	FileFunc.CreateDirTree (datnam) ;

	datnam := Datentraeger ;
	Strings.Append (_PROTO, datnam) ;
	Strings.Append ('\', datnam) ;
	FileFunc.CreateDirTree (datnam) ;
	*)

	RETURN TRUE ;
END CheckDatentraeger ;


PROCEDURE FreeDatentraeger ;
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	FileFunc.CloseFile (LockDatei) ;
	datnam := Datentraeger ;
	Strings.Append (UserTemporaer, datnam) ;
	Strings.Append (_Lock, datnam) ;
	FileFunc.DeleteFile (datnam) ;
END FreeDatentraeger ;


PROCEDURE CreateSTDDB ;
BEGIN
	IF DatentraegerOk THEN
		CreateDatenbasis (_STDDB, 3) ;
	END ;
END CreateSTDDB ;


PROCEDURE CreatePROTO ;
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF DatentraegerOk THEN
		datnam := Datentraeger ;
		Strings.Append (UserTemporaer, datnam) ;
		Strings.Append (_PROTO, datnam) ;
		Strings.Append ('\', datnam) ;
		FileFunc.CreateDirTree (datnam) ;
	END ;
END CreatePROTO ;


PROCEDURE DeleteSTDDB ;
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF DatentraegerOk THEN
		datnam := Datentraeger ;
		Strings.Append (UserTemporaer, datnam) ;
		Strings.Append (_STDDB, datnam) ;
		DirectoryLoeschen (datnam) ;		(* &STDDB zu Beginn löschen - PS&ENTSCHL will sie kreieren *)
	END ;
END DeleteSTDDB ;


PROCEDURE CreateBKZ ;
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	datnam := Datentraeger ;
	Strings.Append (_LFD, datnam) ;
	Strings.Append (AktAuftrag.BKZ, datnam) ;
	Strings.Append ('\', datnam) ;
%IF WEB %THEN
	IF NOT Strings.Equal (AktAuftrag.BKZ, BKZschreibBerechtigung) THEN
		RETURN ;
	END ;
%END
	FileFunc.CreateDirTree (datnam) ;

(*
	datnam := Datentraeger ;
	Strings.Append (_WSP, datnam) ;
	Strings.Append (AktAuftrag.BKZ, datnam) ;
	Strings.Append ('\', datnam) ;
	FileFunc.CreateDirTree (datnam) ;
*)
END CreateBKZ ;


PROCEDURE DirectoryLoeschen (name : ARRAY OF CHAR) ;
	VAR	entry :		FileFunc.SearchEntry ;
		dat,
		nam,
		dir :		ARRAY [0..276] OF CHAR ;
BEGIN
	IF NOT DatentraegerOk THEN
		RETURN ;
	END ;

	nam :=	name ;
	IF nam [LENGTH(nam)-1] = '\' THEN
		nam [LENGTH(nam)-1] := 0C ;
	END ;
	dir := nam ;
	Strings.Append ('\*', dir) ;
	IF FileFunc.FindFirst(dir, FileFunc.StdAttributes, FileFunc.MustHaveNormalFile, entry) THEN
		dir := nam ;
		Strings.Append ('\', dir) ;
		REPEAT
			dat := dir ;
			Strings.Append (entry.name, dat) ;
			IF FileFunc.DeleteFile (dat) THEN
				IF debug.Dateien THEN
					TraceF ('Datei gelöscht : %s', dat) ;
				END ;
			ELSE
				IF debug.Dateien THEN
					TraceF ("+++ Datei nicht löschbar : %s", dat) ;
				END ;
			END ;
		UNTIL NOT FileFunc.FindNext(entry);
		FileFunc.FindClose(entry);
	END;

	IF FileFunc.DeleteDir (nam) THEN
		IF debug.Dateien THEN
			TraceF ('DB gelöscht : %s', nam) ;
		END ;
	ELSE
		IF debug.Dateien THEN
			TraceF ('+++ DB nicht löschbar : %s', nam) ;
		END ;
	END ;
END DirectoryLoeschen ;


PROCEDURE GetEchtenDatenbasisNamen (DBN : ARRAY OF CHAR ; VAR str : ARRAY OF CHAR) : tTreeZustand ;					(*	C:\TR440\dbname		*)
BEGIN
	TreeZustand := TreeOK ;
	str := Datentraeger ;
	IF NOT ExStrings.EqualI (DBN, _OEFDB) THEN
		Strings.Append (UserTemporaer, str) ;
	END ;
	Strings.Append (DBN, str) ;
	CheckTree (str, tDBN) ;
	RETURN TreeZustand ;
END GetEchtenDatenbasisNamen ;


PROCEDURE CheckDateinamen (VAR INOUT dateiname : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i :	CARDINAL ;
		ok :	BOOLEAN ;
BEGIN
	ok := TRUE ;
	FOR i := 1 TO LENGTH (dateiname) DO
		CASE dateiname [i-1] OF
		'<',
		'>',
		'?',
		'"',
		':',
		'|',
		'\',
		'/',
		'*',
		'.' :
			ok := FALSE ;
			dateiname [i-1] := '$' ;				(* unzulässige Zeichen ersetzen *)
		|
		'a'..'z' :
			dateiname [i-1] := CHR(ORD(dateiname[i-1]) - 32) ;	(* Großbuchstaben draus machen *)
		ELSE
		END ;
	END ;
	RETURN ok ;
END CheckDateinamen ;


PROCEDURE Einschleusen (dbnam, datnam, bkznam, exdkznam : ARRAY OF CHAR ; genvnr : CARDINAL ; lfd : BOOLEAN) ;
	VAR	p :	pEinschleusDatei ;
BEGIN
	p := Einschleus0 ;
        IF p = NIL THEN
		NEW (p) ;
		Einschleus0 := p ;
	ELSE
		IF  Strings.Equal (p ^ .datenbasis, dbnam)
		AND Strings.Equal (p ^ .dateiname, datnam)
		AND (p ^ .genvnr = genvnr) THEN
			IF debug.Dateien THEN
				TraceF ('schon eingeschleust : %s.%s(%c)', dbnam, datnam, genvnr) ;
			END ;
			RETURN ;
		END ;
		WHILE p ^ .next <> NIL DO
			p := p ^ .next ;	(* ans Ende der Liste *)
		END ;
		NEW (p ^ .next) ;
		p := p ^ .next ;
	END ;
	p ^ .datenbasis := dbnam ;										(* neues Element an die Liste anhängen *)
	p ^ .dateiname := datnam ;
	p ^ .genvnr := genvnr ;
	p ^ .bkz := bkznam ;
	p ^ .lfd := lfd ;
	p ^ .next := NIL ;
	lastBKZ := bkznam ;
	lastLFD := lfd ;
	IF debug.Dateien THEN
		TraceF ('Datei eingeschleust : %s.%s(%c) --> %s', bkznam, datnam, genvnr, dbnam) ;
	END ;
	IF NOT lfd THEN
		p ^ .exdkz := exdkznam ;
		lastEXDKZ := exdkznam ;
	ELSE
		p ^ .exdkz := '' ;
		lastEXDKZ := '' ;
	END ;
	IF (genvnr > 999999) AND (lastExtension [0] > '9') THEN
		p ^ .Extension := lastExtension ;
	ELSE
		p ^ .Extension := '' ;
	END ;
END Einschleusen ;


PROCEDURE Ausschleusen (dbnam, datnam : ARRAY OF CHAR) : BOOLEAN ;
	VAR	p,
		v :	pEinschleusDatei ;
BEGIN
	v := NIL ;
	p := Einschleus0 ;
        WHILE p <> NIL DO
		IF Strings.Equal (p ^ .datenbasis, dbnam) AND Strings.Equal (p ^ .dateiname, datnam) THEN	(* das ist die eingeschleuste Datei *)
			IF debug.Dateien THEN
				TraceF ('Datei ausgeschleust : %s.%s(%c)', dbnam, datnam, p ^ .genvnr) ;
			END ;
			IF v = NIL THEN
				Einschleus0 := p ^ .next ;
			ELSE
				v ^ .next := p ^ .next ;
			END ;
			DISPOSE (p) ;										(* Element löschen *)
			RETURN TRUE ;
		END ;
		v := p ;
		p := p ^ .next ;
        END ;
	RETURN FALSE ;
END Ausschleusen ;


PROCEDURE alleAusschleusen (dbnam : ARRAY OF CHAR ; VTDT : CARDINAL8) ;
	VAR	p,
		v :		pEinschleusDatei ;
		nurlfd :	BOOLEAN ;
BEGIN
	v := NIL ;
	nurlfd := VTDT = 1 ;
	p := Einschleus0 ;
        WHILE p <> NIL DO
		IF NOT nurlfd OR p ^ .lfd THEN
			IF Strings.Equal (p ^ .datenbasis, dbnam) THEN						(* das ist eine eingeschleuste Datei *)
				IF v = NIL THEN
					Einschleus0 := p ^ .next ;
				ELSE
					v ^ .next := p ^ .next ;
				END ;
				DISPOSE (p) ;									(* Element löschen *)
				p := v ;
			END ;
		END ;
		v := p ;
		IF v = NIL THEN
			p := Einschleus0 ;
		ELSE
			p := v ^ .next ;
		END ;
        END ;
	RETURN ;
END alleAusschleusen ;


PROCEDURE CheckTree (tree : ARRAY OF CHAR ; zust : tTreeZustand) ;
BEGIN
	IF TreeZustand = TreeOK THEN
		dateiFS := 0 ;
		dateiFStext := '' ;
		IF NOT FileFunc.DirExists (tree) THEN
			TraceF('+++ %s nicht da', tree) ;
			TreeZustand := zust ;
			CASE zust OF
			tDBN :		dateiFS := 41H ;
					dateiFStext := 'Datenbasis existiert nicht' ;
			|
			tLFD :		dateiFS := 100H ;
					dateiFStext := 'keine LFD vorhanden' ;
			|
			tLFDbkz :	dateiFS := 101H ;
					dateiFStext := 'BKZ ist nicht LFD-berechtigt' ;
			|
			tWSP :		dateiFS := 100H ;
					dateiFStext := 'keine LFD vorhanden' ;
			|
			tWSPexdkz :	dateiFS := 53H ;
					dateiFStext := 'EXDKZ ist nicht bekannt' ;
			|
			tWSPbkz,
			tWSPexdkzBKZ :	dateiFS := 102H ;
					dateiFStext := 'BKZ nicht auftragseigen' ;
			ELSE
			END ;
			IF debug.Dateien THEN
				TraceF ('Tree %s - FS=%c : %s',tree, dateiFS, dateiFStext) ;
			END ;
		END ;
	END ;
END CheckTree ;


PROCEDURE GENVNRtoZahl (str : ARRAY OF CHAR) : CARDINAL ;
	VAR	GENVNR :	CARDINAL ;
BEGIN
	IF (str [0] < '0') OR (str [0] > '9') THEN		(* auch führende Nullen sind hier ok fuer GENVNR *)
		GENVNR := 1000000 ;
		IF debug.Dateien THEN
			TraceF ('GENVNR %s -', str) ;
		END ;
	ELSIF NOT Conversions.StrToCard (str, GENVNR) THEN	(* ungültige Zahl ist nicht ok *)
		GENVNR := 0 ;
		IF debug.Dateien THEN
			TraceF ('GENVNR %s = 0', str) ;
		END ;
	ELSIF GENVNR < 100 THEN					(* < 1.00 ist nicht ok fuer GENVNR *)
		GENVNR := 1000000 ;
		IF debug.Dateien THEN
			TraceF ('GENVNR %s <', str) ;
		END ;
	END ;
	RETURN GENVNR ;
END GENVNRtoZahl ;


PROCEDURE GENVNRausZahl (zahl : CARDINAL ; VAR GENVNR : ARRAY OF CHAR) ;
	VAR	str :	ARRAY [0..7] OF CHAR ;
BEGIN
	IF (zahl = 0) OR (zahl > 999999) THEN
		GENVNR := '' ;
	ELSE
		zahl := zahl + 1000000 ;
		Conversions.CardToStr (zahl, str) ;
		GENVNR := str [1..6] ;
		IF HIGH (GENVNR) > 5 THEN
			GENVNR [6] := 0C ;
		END ;
	END ;
END GENVNRausZahl ;


PROCEDURE MakeExtension (GENVNR : CARDINAL ; VAR ext : ARRAY OF CHAR) ;
	VAR	str :	ARRAY [0..7] OF CHAR ;
BEGIN
	IF GENVNR = 0 THEN
		ext := '.100' ;
	ELSIF GENVNR > 999999 THEN
		ext := '' ;
	ELSE
		ext := '.' ;
		GENVNRausZahl (GENVNR, str) ;
		IF GENVNR < 1000 THEN
			Strings.Append (str [3..5], ext) ;
		ELSIF GENVNR < 10000 THEN
			Strings.Append (str [2..5], ext) ;
		ELSIF GENVNR < 100000 THEN
			Strings.Append (str [1..5], ext) ;
		ELSE
			Strings.Append (str, ext) ;
		END ;
	END ;
END MakeExtension ;


PROCEDURE AppendExtension (GENVNR : CARDINAL ; VAR INOUT dateiname : ARRAY OF CHAR) ;
	VAR	ext :	ARRAY [0..15] OF CHAR ;
BEGIN
	IF (GENVNR > 999999) AND (lastExtension [0] > '9') THEN		(* keine TR440-Datei sondern Windows-native *)
		Strings.Append ('.', dateiname) ;
		Strings.Append (lastExtension, dateiname) ;
	ELSE
		MakeExtension (GENVNR, ext) ;
		Strings.Append (ext, dateiname) ;
	END ;
END AppendExtension ;


PROCEDURE GetExternDateinamen (datnam, bkz, exdkz : ARRAY OF CHAR ; VAR dateiname : ARRAY OF CHAR ; lfd : BOOLEAN) : tTreeZustand ;
BEGIN
	TreeZustand := TreeOK ;
	dateiname := Datentraeger ;
	lastBKZ := bkz ;
	lastEXDKZ := exdkz ;
	IF lfd THEN
		lastLFD := TRUE ;
		Strings.Append (_LFD, dateiname) ;							(*	C:\TR440\#-LFD-#\			*)
		CheckTree (dateiname, tLFD) ;
	ELSE
		lastLFD := FALSE ;
		Strings.Append (_WSP, dateiname) ;							(*	C:\TR440\#-WSP-#\			*)
		CheckTree (dateiname, tWSP) ;
		IF exdkz [0] > ' ' THEN
			Strings.Append (exdkz, dateiname) ;						(*	C:\TR440\#-WSP-#\exdkz\		*)
			Strings.Append ('\', dateiname) ;
			CheckTree (dateiname, tWSPexdkz) ;
		END ;
	END ;

	IF bkz [0] > ' ' THEN
		Strings.Append (bkz, dateiname) ;							(*		C:\TR440\#-WSP-#\exdkz\bkz\	*)
		Strings.Append ('\', dateiname) ;							(*	oder	C:\TR440\#-LFD-#\bkz\		*)
													(*	oder	C:\TR440\#-WSP-#\exdkz\		*)
		IF lfd THEN
			CheckTree (dateiname, tLFDbkz) ;
		ELSIF exdkz [0] > ' ' THEN
			CheckTree (dateiname, tWSPexdkzBKZ) ;
		ELSE
			CheckTree (dateiname, tWSPbkz) ;
		END ;

	END ;
	Strings.Append (datnam, dateiname) ;
	IF debug.Dateien THEN
		TraceF ('Datei %s', dateiname) ;
	END ;
	RETURN TreeZustand ;
END GetExternDateinamen ;


PROCEDURE GetLokalDateinamen (dbnam, datnam : ARRAY OF CHAR ; VAR dateiname : ARRAY OF CHAR) : tTreeZustand ;
BEGIN
	TreeZustand := TreeOK ;
	GetEchtenDatenbasisNamen (dbnam, dateiname) ;
	Strings.Append ('\', dateiname) ;
	CheckTree (dateiname, tDBN) ;
	Strings.Append (datnam, dateiname) ;									(*	C:\TR440\dbname\dateiname	*)
	IF debug.Dateien THEN
		TraceF ('Datei %s', dateiname) ;
	END ;
	RETURN TreeZustand ;
END GetLokalDateinamen ;


PROCEDURE GetEchtenDateinamen (dbnam, datnam : ARRAY OF CHAR ; VAR dateiname : ARRAY OF CHAR) : tTreeZustand ;
	VAR	p :	pEinschleusDatei ;
		merk :	BOOLEAN ;
BEGIN
	p := Einschleus0 ;
	lastBKZ := '' ;
	lastEXDKZ := '' ;
	lastLFD := FALSE ;
	WHILE p <> NIL DO
		IF ExStrings.EqualI (p ^ .datenbasis, dbnam) AND ExStrings.EqualI (p ^ .dateiname, datnam) THEN	(* das ist eine eingeschleuste Datei	*)
			merk := debug.Dateien ;
			debug.Dateien := FALSE ;
			GetExternDateinamen (datnam, p ^ .bkz, p ^ .exdkz, dateiname, p ^ .lfd) ;
			lastBKZ := p ^ .bkz ;
			lastEXDKZ := p ^ .exdkz ;
			lastLFD := p ^ .lfd ;
			lastExtension := p ^ .Extension ;
			debug.Dateien := merk ;
			IF debug.Dateien THEN
				TraceF ('Datei %s -> %s', dbnam, dateiname) ;
			END ;
			RETURN TreeZustand ;
		END ;
		p := p ^ .next ;
	END ;
	RETURN GetLokalDateinamen (dbnam, datnam, dateiname) ;
END GetEchtenDateinamen ;


PROCEDURE DateiExistiert (dateiname : ARRAY OF CHAR ; VAR INOUT GENVNR : CARDINAL) : BOOLEAN ;
	VAR	datnam :	ARRAY [0..255] OF CHAR ;
		entry :		FileFunc.SearchEntry ;
		parts :		FileFunc.FileNameParts ;
		zahlda,
		zahl :		CARDINAL ;
		str :		ARRAY [0..15] OF CHAR ;
BEGIN
	datnam := dateiname ;
	lastExtension := '' ;
	IF (GENVNR <> 0) AND (GENVNR < 1000000) THEN
		AppendExtension (GENVNR, datnam) ;
		IF FileFunc.FileExists (datnam) THEN
			IF debug.Dateien THEN
				TraceF ('Datei expl. existiert : %s(%c)', dateiname, GENVNR) ;
			END ;
			RETURN TRUE ;
		ELSE
			IF debug.Dateien THEN
				TraceF ('Datei existiert nicht : %s(%c)', dateiname, GENVNR) ;
			END ;
			RETURN FALSE ;
		END ;
	END ;

	Strings.Append ('.*', datnam) ;

	zahlda := 0 ;

 	IF FileFunc.FindFirst(datnam, FileFunc.StdAttributes, FileFunc.MustHaveNormalFile, entry) THEN
		REPEAT
			FileFunc.ParseFileName (entry.name, parts) ;
			IF parts.extension [0] = 0C THEN
				str := '' ;
				zahl := 0 ;
			ELSE
				str := parts.extension [1..HIGH(parts.extension)] ;
				IF (str [0] = '0')			(* führende Nullen sind in Windows-Extension nicht erlaubt *)
				OR (str [Length(str)-1] = '~')		(* schleppende Tilden sind in Windows-Extension nicht erlaubt *)
				OR Equal (str, 'BAK')			(* .BAK - Dateien sind als Windows-Extension nicht erlaubt *)
				OR Equal (str, 'bak')			(* .bak - Dateien sind als Windows-Extension nicht erlaubt *)
				 THEN
					zahl := 0 ;			(* führende Nullen und schleppende Tilden sind in Windows-Extension nicht erlaubt *)
					str := '' ;			(* auch nicht für sonstige externe Extension *)
				ELSE
					zahl := GENVNRtoZahl (str) ;
				END ;
			END ;
			IF (zahl <> 0) AND (zahl < 1000000) THEN	(* Element hat ordentliche Zahl-Extension *)
				IF zahl > zahlda THEN
					zahlda := zahl ;
				END ;
				lastExtension := '' ;
			ELSIF (zahlda = 0) AND (str [0] <> 0C) THEN				(* noch keine Zahl-Extension gefunden *)
				lastExtension := str ;
			END ;
		UNTIL NOT FileFunc.FindNext (entry) ;
		FileFunc.FindClose (entry) ;
		IF zahlda <> 0 THEN
			GENVNR := zahlda ;
			IF debug.Dateien THEN
				TraceF ('Datei existiert : %s(%c)', dateiname, GENVNR) ;
			END ;
		ELSE
			IF debug.Dateien THEN
				TraceF ('Datei existiert : %s.%s', dateiname, lastExtension) ;
			END ;
			GENVNR := 1000000 ;
		END ;
		RETURN TRUE ;
	END ;
	lastExtension := '' ;
	RETURN FALSE ;
END DateiExistiert ;


PROCEDURE DateiSchreibSchutz (datnam : ARRAY OF CHAR) : BOOLEAN ;
	VAR	found :	BOOLEAN ;
		pos,
		pos2 :	CARDINAL ;
		bkz :	ARRAY [0..15] OF CHAR ;
		str :	ARRAY [0..267] OF CHAR ;
BEGIN
	str := Datentraeger ;
	Strings.Append (_OEFDB, str) ;
	Strings.Append ('\', str) ;
	Strings.FindNext (str, datnam, 0, found, pos) ;
	IF found THEN
		FSdateiverwaltung := 34H ;		(* Ändern der &OEFDB unzulässig *)
		RETURN TRUE ;
	END ;
	Strings.FindNext (_LFD, datnam, 0, found, pos) ;
	IF found THEN				(* LFD-Datei *)
		pos := pos + Length(_LFD) ;
		Strings.FindNext ('\', datnam, pos, found, pos2) ;
		bkz := datnam [pos .. pos2-1] ;
		IF debug.Dateien THEN
			TraceF ('Datei-LFD-BKZ : %s', bkz) ;
		END ;
%IF WEB %THEN
		IF NOT Strings.Equal (bkz, BKZschreibBerechtigung) THEN
			FSdateiverwaltung := 102H ;	(* LFD-BKZ nicht auftragseigen *)
			RETURN TRUE ;
		END ;
%END
	END ;

	RETURN FALSE ;
END DateiSchreibSchutz ;


PROCEDURE DateiKreieren (datnam : ARRAY OF CHAR ; GENVNR : CARDINAL) : BOOLEAN ;
	VAR	datei :	FileFunc.File ;
		dateiname :	ARRAY [0..267] OF CHAR ;
BEGIN
	IF NOT DatentraegerOk THEN
		RETURN FALSE ;
	END ;
	dateiname := datnam ;
	IF GENVNR = 0 THEN
		GENVNR := 100 ;
	END ;
	lastExtension := '' ;
	AppendExtension (GENVNR, dateiname) ;
	IF DateiSchreibSchutz (dateiname) THEN
		TraceF ('Kreationsschutz : %s', datnam) ;
		RETURN FALSE ;
	END ;
	FileFunc.CreateFile (datei, dateiname) ;
	IF datei.status = 0 THEN
		SetDateityp (datei) ;		(* Die Infos stehen schon in 'DateiBearbeitung.MerkInfo' *)
		IF debug.Dateien THEN
			TraceF ('Datei kreiert : %s', dateiname) ;
		END ;
		FileFunc.CloseFile (datei) ;
		RETURN TRUE ;
	END ;
	IF debug.Dateien THEN
		TraceF ('Kreationsfehler %c : %s', datei.status, dateiname) ;
	END ;
	RETURN FALSE ;
END DateiKreieren ;


PROCEDURE DateiLoeschen (dateiname : ARRAY OF CHAR ; GENVNR : CARDINAL) : BOOLEAN ;
	VAR	datnam :	ARRAY [0..267] OF CHAR ;
BEGIN
	IF NOT DatentraegerOk THEN
		RETURN FALSE ;
	END ;
	IF GENVNR = 0 THEN
		GENVNR := 100 ;
	END ;
	IF DateiExistiert (dateiname, GENVNR) THEN
		datnam := dateiname ;
		AppendExtension (GENVNR, datnam) ;
		IF DateiSchreibSchutz (datnam) THEN
			TraceF ('Löschschutz : %s', datnam) ;
			RETURN FALSE ;
		END ;
		IF FileFunc.DeleteFile (datnam) THEN
			IF debug.Dateien THEN
				TraceF ('Datei gelöscht : %s', datnam) ;
			END ;
			RETURN TRUE ;
		END ;
		IF debug.Dateien THEN
			TraceF ('Loeschfehler : %s', datnam) ;
		END ;
	END ;
	RETURN FALSE ;
END DateiLoeschen ;



PROCEDURE CreateDatenbasis (DBN : ARRAY OF CHAR ; DBA : CARDINAL) ;
	VAR	db :		pDatenbasis ;
		datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT DatentraegerOk THEN
		RETURN ;
	END ;
	datnam := Datentraeger ;
	Strings.Append (UserTemporaer, datnam) ;
	Strings.Append (DBN, datnam) ;
	IF FileFunc.MakeDir (datnam) THEN
		IF debug.Dateien THEN
			TraceF ('DB kreiert : %s', datnam) ;
		END ;
	ELSE
		IF debug.Dateien THEN
			TraceF ('DBN-Kreationsfehler : %s', datnam) ;
		END ;
	END ;
	db := NewDB (DBN) ;
	db ^ .DBA := VAL (tDBA, DBA) ;
END CreateDatenbasis ;


PROCEDURE DeleteDatenbasis (DBN : ARRAY OF CHAR) ;
	VAR	db :		pDatenbasis ;
		datnam :	ARRAY [0..255] OF CHAR ;
BEGIN
	IF NOT DatentraegerOk THEN
		RETURN ;
	END ;

	IF DBN [0] = 0C THEN		(* alle löschen *)
		db := Datenbasis0 ;
	ELSE
		db := SuchDB (DBN) ;
	END ;
	WHILE db <> NIL DO
		IF NOT ExStrings.EqualI (db ^ .name, _OEFDB) THEN
			datnam := Datentraeger ;
			Strings.Append (UserTemporaer, datnam) ;
			Strings.Append (db ^ .name, datnam) ;
			DirectoryLoeschen (datnam) ;

			alleAusschleusen (db ^ .name, 0) ;
			DeleteDB (db) ;
		END ;

		IF DBN [0] <> 0C THEN
			BREAK ;
		END ;
		db := Datenbasis0 ;
	END ;
END DeleteDatenbasis ;


PROCEDURE DeleteOperatorlaufDatenbasen ;
	VAR	db :	pDatenbasis ;
BEGIN
	db := Datenbasis0 ;
	WHILE db <> NIL DO
		IF (db ^ .DBA = DBoperatorlauf) AND (db ^ .OLK = AktOLK) THEN
			DeleteDatenbasis (db ^ .name) ;
			db := Datenbasis0 ;
		ELSE
			db := db ^ .next ;
		END ;
	END ;
END DeleteOperatorlaufDatenbasen ;


PROCEDURE Init ;
BEGIN
	Datenbasis0 := NIL ;
	Einschleus0 := NIL ;
	Datentraeger := cDatentraeger ;
END Init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
FINALLY
	IF initialisiert THEN
		initialisiert := FALSE ;
		FreeDatentraeger ;
	END ;
END DateiVerwaltung.
