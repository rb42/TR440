
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Tabelle;

(*	24.04.16	*)

FROM Befehle IMPORT
	nimpl ;

IMPORT debug ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;
FROM upFestkomma IMPORT * ;


PROCEDURE _TLI ;
	VAR	anz :	CARDINAL ;
		vgl,
		wort :	CARDINAL64 ;
		merk :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	RegB := AktAdressteil ;
	anz := 0 ;
	IF RegD.TK = 1 THEN
		vgl := tr48Norm (RegD.inh) ;
	ELSE
		vgl := RegD.inh BAND Cardinal48Bit ;
	END ;
	REPEAT
		INC (anz) ;
		wort := GetGanzwort (RegB) ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
		IF AktTypenkennung = 1 THEN
			wort := tr48Norm (wort) ;
		ELSE
			wort := wort BAND Cardinal48Bit ;
		END ;
		RegB := trAdd24 (RegB, 2) ;
		IF RegD.TK <> AktTypenkennung THEN
			TypenkennungsAlarm (RegB-2) ;
			BREAK ;
		END ;
	UNTIL vgl = wort ;
	RegB := trSub24 (RegB, 2) ;
	RegA := RegD ;
	debug.Lesen := merk ;
	Takte (anz * 14 + anz DIV 2 + 10) ;
END _TLI;


PROCEDURE _TLD ;
	VAR	anz,
		dehn :	CARDINAL ;
		vgl,
		wort :	CARDINAL64 ;
		merk :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	IF RegB = 0 THEN
		RETURN ;
	END ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	dehn := RegB ;
	RegB := AktAdressteil ;
	anz := 0 ;
	IF RegD.TK = 1 THEN
		vgl := tr48Norm (RegD.inh) ;
	ELSE
		vgl := RegD.inh BAND Cardinal48Bit ;
	END ;
	REPEAT
		INC (anz) ;
		wort := GetGanzwort (RegB) ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
		IF AktTypenkennung = 1 THEN
			wort := tr48Norm (wort) ;
		ELSE
			wort := wort BAND Cardinal48Bit ;
		END ;
		RegB := trAdd24 (RegB, dehn) ;
		IF RegD.TK <> AktTypenkennung THEN
			TypenkennungsAlarm (trSub24 (RegB, dehn)) ;
			BREAK ;
		END ;
	UNTIL vgl = wort ;
	RegB := trSub24 (RegB, dehn) ;
	RegA := RegD ;
	debug.Lesen := merk ;
	Takte (anz * 14 + anz DIV 2 + 11) ;
END _TLD;


PROCEDURE _TDM ;
	VAR	anz,
		dehn :	CARDINAL ;
		wort,
		maske :	CARDINAL64 ;
		merk :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	IF RegB = 0 THEN
		RETURN ;
	END ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	dehn := RegB ;
	maske := (BNOT RegH.inh) BAND Cardinal48Bit ;
	RegB := AktAdressteil ;
	anz := 0 ;
	REPEAT
		INC (anz) ;
		wort := GetGanzwort (RegB) ;
		IF AlarmGewesen THEN
			BREAK ;
		END ;
		RegB := trAdd24 (RegB, dehn) ;
		IF RegD.TK <> AktTypenkennung THEN
			TypenkennungsAlarm (trSub24 (RegB, dehn)) ;
			BREAK ;
		END ;
	UNTIL (RegD.inh BAND maske) = (wort BAND maske) ;
	RegB := trSub24 (RegB, dehn) ;
	RegA.inh := wort BAND RegH.inh ;
	RegA.TK := RegH.TK ;
	debug.Lesen := merk ;
	Takte (anz * 14 + anz DIV 2 + 3) ;
END _TDM;


PROCEDURE _TMAX ;
	VAR	anz,
		dehn,
		maxadr :CARDINAL ;
		max,
		wort,
		maske :	CARDINAL64 ;
		TK :	CARDINAL8 ;
		merk :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	IF RegB = 0 THEN
		RETURN ;
	END ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	dehn := RegB ;
	maske := (BNOT RegH.inh) BAND Cardinal48Bit ;
	RegB := AktAdressteil ;
	maxadr := RegB ;
	anz := 0 ;
	max := GetGanzwort (RegB) ;
	TK := AktTypenkennung ;
	LOOP
		INC (anz) ;
		wort := GetGanzwort (RegB) ;
		IF AlarmGewesen THEN
			EXIT ;
		END ;
		IF TK <> AktTypenkennung THEN
			EXIT ;
		END ;
		IF (wort BAND maske) > (max BAND maske) THEN
			max := wort ;
			maxadr := RegB ;
		END ;
		INC (RegB, dehn) ;
	END ;
	RegB := maxadr ;
	RegA.inh := max BAND RegH.inh ;
	RegA.TK := TK ;
	RegD.inh := max BAND maske ;
	RegD.TK := TK ;
	debug.Lesen := merk ;
	Takte (anz * 17 + 4) ;
END _TMAX;


PROCEDURE _TMIN ;
	VAR	anz,
		dehn,
		minadr :CARDINAL ;
		min,
		wort,
		maske :	CARDINAL64 ;
		TK :	CARDINAL8 ;
		merk :	BOOLEAN ;
BEGIN
	mod2Ohne ;
	IF RegB = 0 THEN
		RETURN ;
	END ;
	merk := debug.Lesen ;
	debug.Lesen := FALSE ;
	dehn := RegB ;
	maske := (BNOT RegH.inh) BAND Cardinal48Bit ;
	RegB := AktAdressteil ;
	minadr := RegB ;
	anz := 0 ;
	min := GetGanzwort (RegB) ;
	TK := AktTypenkennung ;
	LOOP
		INC (anz) ;
		wort := GetGanzwort (RegB) ;
		IF AlarmGewesen THEN
			EXIT ;
		END ;
		IF TK <> AktTypenkennung THEN
			EXIT ;
		END ;
		IF (wort BAND maske) < (min BAND maske) THEN
			min := wort ;
			minadr := RegB ;
		END ;
		INC (RegB, dehn) ;
	END ;
	RegB := minadr ;
	RegA.inh := min BAND RegH.inh ;
	RegA.TK := TK ;
	RegD.inh := min BAND maske ;
	RegD.TK := TK ;
	debug.Lesen := merk ;
	Takte (anz * 17 + 4) ;
END _TMIN;


PROCEDURE _TLOG ;
BEGIN
	nimpl ;
END _TLOG;


END Tabelle.
