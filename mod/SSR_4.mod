
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE SSR_4;

(*	05.04.18	*)


FROM WIN32 IMPORT
	GetTickCount ;

FROM Trace IMPORT
	TraceF ;

IMPORT Strings, Terminal, FileFunc, FileIO, AsmTrace ;

FROM Struktur IMPORT * ;
FROM Register IMPORT * ;

FROM Gebiet IMPORT * ;

FROM AbwicklerUp IMPORT * ;


VAR
	InitTickCount :		CARDINAL ;

	initialisiert :		BOOLEAN = FALSE ;



PROCEDURE _SSR_4_0 ;
	VAR	olk,
		AA,
		S :		CARDINAL ;
		Operator :	OperatorNummer ;
BEGIN
	SSRaufruf ('Informiere über Kenndaten', 2) ;
	olk := GetHalbwort (VBL + 1) ;
	AA := GetHalbwort (VBL + 2) ;
	S := GetHalbwort (VBL + 3) BAND 3 ;

	Operator := SuchOperator (olk) ;
	IF Operator = 0 THEN
		SSRfehler (5, 'OLK ist ungültig') ;
		RETURN ;
	END ;


	PutStringInRegister ('001900', RegQ) ;
	RegH := RegQ ;

	WITH Operatoren [Operator] DO
		IF MVNR [0] = 0C THEN
			PutStringInRegister ('000100', RegA) ;
		ELSE
			PutStringInRegister (MVNR, RegA) ;
		END ;
		PutStringInGanzwortSpeicher (ON, 		AA     , 12) ;
		PutStringInGanzwortSpeicher (OLN,		AA +  4, 12) ;
		PutStringInGanzwortSpeicher (AktAuftrag.FKZ,	AA +  8, 6) ;
		PutGanzwort (					AA + 10, 2, OLK) ;
		PutGanzwort (					AA + 12, 2, OLKV) ;
		PutGanzwort (					AA + 14, 2, OLKE) ;
		PutStringInGanzwortSpeicher (AktAuftrag.BEN,	AA + 16, 30) ;
	END ;
	IF S > 0 THEN
		PutGanzwort (					AA + 26, 2,     80000101H) ;	(* KSB=128, WB=1, BB=1 *)
		PutGanzwort (					AA + 28, 1,    200000200H) ;	(* TSB=512, PSB=512 *)
		PutGanzwort (					AA + 30, 1,   1000000100H) ;	(* DRS=4096, RZS=256 *)
		PutGanzwort (					AA + 32, 3, 0C0835000000H) ;	(* SBG=12, AAZ=8:35 *)

		PutGanzwort (					AA + 38, 3, 210411010000H) ;	(* TYP=WA33, ANR=1031, GNR=1 *)
		PutGanzwort (					AA + 40, 3,       2A0000H) ;	(* SNR=42, FH=0 *)

		PutGanzwort (					AA + 44, 3,     40000080H) ;	(* KSPMAX=64, TSPMAX=128 *)
		PutGanzwort (					AA + 46, 3,     80000000H) ;	(* PSPMAX=128 *)
		IF S > 1 THEN
			PutGanzwort (				AA + 48, 2, 80FFFF00H) ;	(* UD=0, ANZBU=0, ANZLF=0, ZEILENBR=128, WNR1 *)
			IF Terminal.MaxSpalte < 72 THEN
				PutHalbwort (			AA + 48, 72) ;			(* ZEILENBR minimal *)
			ELSE	PutHalbwort (			AA + 48, Terminal.MaxSpalte) ;	(* ZEILENBR echt aktuell *)
			END ;
			PutGanzwort (				AA + 50, 2, 492123806249H) ;	(* WNR2='00 492123806249' *)
		END ;
	END ;
END _SSR_4_0 ;


PROCEDURE _SSR_4_4 ;
	VAR	geb :	pGebiet ;
		A,
		VK,
		AA,
		LNG,
		anz,
		OLK :	CARDINAL ;
BEGIN
	SSRaufruf ('Informiere über aktuelle Gebiete', 2) ;
	OLK := GetHalbwort (VBL + 1) BAND 0FFFFH ;
	IF SuchOperator (OLK) = 0 THEN
		SSRfehler (5, 'OLK ist ungültig') ;
		RETURN ;
	END ;
	AA := GetHalbwort (VBL + 2) ;
	LNG := GetHalbwort (VBL + 3) ;
	geb := Gebiet0 ;
	anz := 0 ;
	WHILE (geb <> NIL) AND (geb ^ .OLK = OLK) DO
		INC (anz, 3) ;
		PutGanzwort (AA, 1, geb ^ .GebietsNummer) ;
		IF geb ^ .adressiert THEN
			A := geb ^ .AnfangsSeite ;
			VK := 1C000H ;			(* KSP *)
		ELSE
			A := 800000H ;
			VK := 08000H ;			(* PSP *)
		END ;
		NeueTypenkennung := 2 ;
		PutHalbwort (AA + 2, A) ;
		PutHalbwort (AA + 3, geb ^ .Laenge) ;
		PutHalbwort (AA + 4, VK + ORD(geb ^ .Schreibsperre)) ;

		geb := geb ^ .next ;
		IF anz + 3 > LNG THEN			(* kein Platz mehr im Puffer *)
			BREAK ;
		END ;
	END ;
	RegA.inh := anz ;
	RegA.TK := 1 ;
END _SSR_4_4 ;


PROCEDURE _SSR_4_8 ;
	VAR	AA,
		OLKnr,
		wort :	CARDINAL ;
BEGIN
	SSRaufruf ('Informiere über Alarmursache', 2) ;
	wort := GetHalbwort (VBL + 1) ;
	OLKnr := SuchOperator (wort) ;
	IF OLKnr = 0 THEN
		SSRfehler (5, 'Operatorlaufkennzeichen nicht vorhanden') ;
		RETURN ;
	END ;
	AA := GetHalbwort (VBL + 2) ;
	WITH Operatoren [OLKnr].AlarmInformation DO
		PutGanzwort (AA + 6, 2 + ORD(reg.M), (VAL (CARDINAL64, reg.B) SHL 24) BOR  (VAL (CARDINAL64, reg.K) SHL 16) BOR (VAL (CARDINAL64, reg.Y) SHL 8) BOR VAL (CARDINAL64, reg.U)) ;
		PutGanzwort (AA + 8, reg.A.TK, reg.A.inh) ;
		PutGanzwort (AA + 10, reg.Q.TK, reg.Q.inh) ;
		PutGanzwort (AA + 12, reg.D.TK, reg.D.inh) ;
		PutGanzwort (AA + 14, reg.H.TK, reg.H.inh) ;
		PutGanzwort (AA + 16, 2, VAL (CARDINAL64, reg.T) SHL 24) ;
		PutGanzwort (AA + 18, 2, VAL (CARDINAL64, reg.X) SHL 24) ;
		PutGanzwort (AA + 20, 2, 0) ; (* RS *)
		PutGanzwort (AA + 22, 3, VAL (CARDINAL64, reg.B) SHL 24) ;
		PutGanzwort (AA + 24, 3, (VAL (CARDINAL64, reg.F) SHL 24) BOR VAL (CARDINAL64, STB1)) ;
		PutGanzwort (AA + 26, 3 - ORD (BefAlarm), VAL (CARDINAL64, BC) SHL 40) ;
	END ;
END _SSR_4_8 ;


PROCEDURE _SSR_4_20 ;
	VAR	AA,
		GNR :	CARDINAL ;
		geb :	pGebiet ;
BEGIN
	SSRaufruf ('Informiere über Gebietsnamen', 2) ;
	AA := GetHalbwort (VBL + 1) ;
	GNR := GNRausGBK (2) ;
	IF GNR = 0 THEN
		SSRfehler (8, 'Gebietsname ist unbekannt') ;
		RETURN ;
	END ;
	geb := GetGebiet (GNR) ;
	PutGanzwort (AA, 1, geb ^ .GebietsNummer) ;
	IF geb ^ .OGNM [0] > 0C THEN
		NeueTypenkennung := 2 ;
		PutStringInGanzwortSpeicher (geb ^ .OGNM, AA + 2, 6) ;
	ELSE
		PutGanzwort (AA + 2, 2, 0) ;
	END ;
	IF geb ^ .PGNM [0] > 0C THEN
		NeueTypenkennung := 3 ;
		PutStringInGanzwortSpeicher (geb ^ .PGNM, AA + 4, 6) ;
	ELSE
		PutGanzwort (AA + 4, 3, 0) ;
	END ;
END _SSR_4_20 ;


PROCEDURE _SSR_4_24 ;
	VAR	GNR :		CARDINAL ;
		geb :		pGebiet ;
BEGIN
	SSRaufruf ('Informiere über Gebietslänge', 3) ;
	GNR := GNRausGBK (4) ;
	IF GNR = 0 THEN
		SSRfehler (8, 'Gebietsname ist unbekannt') ;
		RETURN ;
	END ;
	geb := GetGebiet (GNR) ;
	RegA.inh := geb ^ .Laenge ;
	RegA.TK := 1 ;
END _SSR_4_24 ;


PROCEDURE _SSR_4_28 ;
	VAR	ZK :	CARDINAL ;
BEGIN
	SSRaufruf ('Informiere über Speicherberechtigungen', 1) ;
	ZK := GetHalbwort (VBL + 1) BAND 1FH ;
	IF (ZK BAND 1CH) = 1CH THEN		(* KSP *)
		RegA.inh := 100 ;			(* noch 100 K frei *)
	ELSE
		RegA.inh := 400 ;			(* sonst 400 K frei *)
	END ;
	RegA.TK := 1 ;
END _SSR_4_28 ;


PROCEDURE _SSR_4_32 ;
	VAR	T :	CARDINAL ;
BEGIN
	SSRaufruf ('Informiere über Zeit', 1) ;
	T := GetHalbwort (VBL + 1) BAND 0FH ;
	CASE T OF
	1 :		(* absolute Zeit in externer Darstellung *)
			RegA.inh := 9907241857H ;
			RegA.TK := 3 ;
	|
	2 :		(* Tagesdatum in druckfertiger Form *)
		PutStringInRegister ('24.07.', RegA) ;
		RegA.TK := 3 ;
		PutStringInRegister ('99', RegQ) ;
		RegQ.TK := 3 ;
	|
	3 :		(* abschnittsrelative Abwicklerzeit intern in 10 Mikrosekunden-Einheiten *)
		RegA.inh := (VAL (CARDINAL64, GetTickCount ()) - VAL (CARDINAL64, InitTickCount)) * 100 ;
		RegA.TK := 1 ;
		RegQ.inh := 1234 * 100000 ;
		RegQ.TK := 1 ;
		RegH.inh := 1200 * 100000 ;
		RegH.TK := 1 ;
	|
	4 :		(* relative Maschinenzeit intern in 10 Mikrosekunden-Einheiten *)
		RegA.inh := VAL (CARDINAL64, GetTickCount ()) * 100 ;
		RegA.TK := 1 ;
	|
	5 :		(* operatorrelative Nettozeit intern in 10 Mikrosekunden-Einheiten *)
		RegA.inh := (GesamtZahlTakte + TakteProMikroSekunde * 5) DIV (TakteProMikroSekunde * 10) ;
		RegA.TK := 1 ;
		RegQ.inh := 1234 * 100000 ;
		RegQ.TK := 1 ;
		RegH.inh := 1000 * 100000 ;
		RegH.TK := 1 ;
	ELSE
	END ;
END _SSR_4_32 ;


PROCEDURE _SSR_4_36 ;
BEGIN
	SSRaufruf ('Informiere über Signale', 1) ;
	RegA.inh := Signale ;
	RegA.TK := 2 ;
END _SSR_4_36 ;


PROCEDURE Init ;
BEGIN
	InitTickCount := GetTickCount () ;
END Init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		Init ;
	END ;
END SSR_4.
