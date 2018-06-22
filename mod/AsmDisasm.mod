
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE AsmDisasm;


(*	06.12.16	*)


FROM SYSTEM IMPORT
	CAST ;
FROM WIN32 IMPORT
	HANDLE,
	GlobalAlloc, GMEM_MOVEABLE,
	GlobalLock,GlobalUnlock ;
FROM WINUSER IMPORT
        OpenClipboard, EmptyClipboard,
        SetClipboardData, CF_TEXT, CloseClipboard ;

IMPORT Strings, Conversions, Terminal, AsmTrace ;

IMPORT Assembler, Struktur, Register, debug, upFestkomma ;


CONST

	tet =	'0123456789ABCDEF' ;

	TAB =		CHR(9) ;


%IF %NOT WEB %THEN


TYPE
	tMnemo =		ARRAY [0..3] OF CHAR ;


CONST
	Mnemo : ARRAY tOpcode OF tMnemo = {

		'NULL',	'XBA',	'MNA',	'MA',	'EMU',	'MU',	'BCL',	'TBC',	'|08',	'MD',	'SZX',	'MF',	'TXX',	'TTX',	'MHX',	'HBPX',

		'LZL',	'HBA',	'NL',	'VBA',	'MC',	'VBC',	'MCF',	'MCE',	'|18',	'XBAN',	'ZX',	'SW',	'SLN',	'SNL',	'SL',	'SLL',

		'MAB',	'SHB',	'WTV',	'WTR',	'SXI',	'SXGG',	'SXKG',	'SXN',	'EMB',	'E',	'ENZ',	'EZ',	'HXP',	'MH',	'HXX',	'VXX',

		'ZMC',	'LMC',	'LMT',	'LC',	'SM',	'SMN',	'S',	'VSS',	'SU',	'TCB',	'SFB',	'?3B',	'HBC',	'MCFU',	'ZU',	'MABI',

		'AB',	'SBB',	'A',	'AC',	'SBI',	'SBD',	'SB',	'SBC',	'GSBI',	'AU',	'GAC',	'GA',	'GSBD',	'SBU',	'GSBC',	'GSB',

		'?50',	'?51',	'GAB',	'GSBB',	'ML',	'MLR',	'MLA',	'MAR',	'MLN',	'MNR',	'MAN',	'MANR',	'GMLN',	'GMAN',	'GML',	'GMLA',

		'DV',	'DVD',	'DVI',	'VAQ',	'GDV',	'REZ',	'GDVI',	'B2VN',	'VEL',	'AUT',	'ET',	'ZUS',	'B3',	'B3V',	'B2',	'B2V',

		'B',	'BD',	'BQ',	'BH',	'BB',	'BN',	'BR',	'BNR',	'M2N',	'M2NR',	'M2',	'M2R',	'A2',	'SB2',	'AQ',	'SBQ',

		'C',	'CR',	'CMT',	'CMR',	'CN',	'CB',	'CD',	'CQ',	'VLA',	'ATA',	'ETA',	'LA',	'TXR',	'|8D',	'BA',	'CH',

		'ST',	'STN',	'ZTR',	'SEGG',	'KDFR',	'KFLD',	'R',	'RT',	'AA',	'SBA',	'LR',	'SH',	'TRX',	'HALT',	'KB',	'NRM',

		'C2',	'C3',	'CMC',	'BC',	'SI0',	'SKG0',	'SGG0',	'SN0',	'SAT',	'SAA',	'SK',	'SG',	'SI',	'SN',	'SKG',	'SGG',

		'BL',	'VPU',	'SXR',	'SXRN',	'Y',	'LEI',	'BCI',	'ZI',	'SR',	'PDP',	'SRN',	'SSR',	'SE',	'SUE',	'BLEI',	'VMO',

		'?C0',	'?C1',	'?C2',	'?C3',	'?C4',	'?C5',	'?C6',	'?C7',	'ZT0',	'ZT1',	'ZT2',	'ZT3',	'T',	'?CD',	'SXG',	'SXK',

		'CU',	'BZN',	'ZDP',	'BU',	'SG0',	'SK0',	'?D6',	'?D7',	'BZ2',	'BZ',	'BQB',	'CZ',	'BAR',	'BANR',	'?DE',	'BAN',

		'RLR',	'IR',	'?E2',	'US',	'?E4',	'?E5',	'BNZ',	'CNZ',	'?E8',	'?E9',	'TDM',	'TLD',	'TLI',	'TLOG',	'TMIN',	'TMAX',

		'DA',	'DSB',	'DML',	'MLD',	'AT',	'SBT',	'BT',	'CT',	'WB',	'SBIT',	'SFBE',	'BSS',	'ZK',	'TOK',	'QBR',	'QCR'

		} ;

	ZusMnemo : ARRAY OF tMnemo = {
			'N',	'MFU',		'M',		'XB',		'XC',		'XCN',		'RX',		'MRX',		'SIO',	'SKGO',	'SGGO',	'SNO'
		} ;
	ZusOpcode : ARRAY OF tOpcode = {
			opNULL,	opMFUMXB,	opMFUMXB,	opMFUMXB,	opXCXCN,	opXCXCN,	opRXMRX,	opRXMRX,	opSI0,	opSKG0,	opSGG0,	opSN0
		} ;
	ZusOpcodeBytes : ARRAY OF CARDINAL = {
			0,	2000H,		8000H,		4000H,		0,		8000H,		0,		0200H,		0,	0,	0,	0
		} ;


TYPE
	MnemoRecord =	RECORD
				zeichen,
				opcode :		CARDINAL ;
			END ;


VAR
	MnemoSuche :	ARRAY [0..255] OF MnemoRecord ;
	MnemoAnzahl :	CARDINAL ;

	Rekursion :	BOOLEAN = FALSE ;

	initialisiert :	BOOLEAN = FALSE ;

%END


PROCEDURE TRIM (VAR str : ARRAY OF CHAR) ;
	VAR	i,
		l :	CARDINAL ;
BEGIN
	IF str [0] = 0C THEN
		RETURN ;
	END ;
	l := LENGTH (str)-1 ;
	WHILE str [l] <= ' ' DO
		str [l] := 0C ;
		IF l = 0 THEN
			RETURN ;
		END ;
		DEC (l) ;
	END ;
	i := 0 ;
	WHILE ((i < l) AND (str [i] <= ' ')) DO
		INC (i) ;
	END ;
	IF i <= l THEN
		str := str [i..l] ;
	END ;
END TRIM ;


PROCEDURE EqualI (str1, str2 : ARRAY OF CHAR) : BOOLEAN ;
	VAR	i,h :	CARDINAL ;
BEGIN
	h := HIGH (str1) ;
	IF h > HIGH(str2) THEN
		h := HIGH(str2) ;
	END ;
	i := 0 ;
	LOOP
		IF str1 [i] = 0C THEN
			RETURN str2 [i] = 0C ;
		ELSIF str2 [i] = 0C THEN
			RETURN FALSE ;
		END ;
		IF (ORD(str1 [i]) BOR 20H) <> (ORD(str2 [i]) BOR 20H) THEN
			RETURN FALSE ;
		END ;
		IF i < h THEN
			INC (i) ;
		ELSE
			EXIT ;
		END ;
	END ;
	IF HIGH(str1) < HIGH(str2) THEN
		RETURN str2 [i+1] = 0C ;
	ELSIF HIGH(str2) < HIGH(str1) THEN
		RETURN str1 [i+1] = 0C ;
	END ;
	RETURN TRUE ;
END EqualI ;


PROCEDURE CardToHex12 (wert : CARDINAL64 ; VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO 12 DO
		str [12-i] := tet [wert BAND 0FH] ;
		wert := wert SHR 4 ;
	END ;
	str [12] := 0C ;
END CardToHex12 ;


PROCEDURE CardToHex8 (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	FOR i := 1 TO 8 DO
		str [8-i] := tet [wert BAND 0FH] ;
		wert := wert SHR 4 ;
	END ;
	str [8] := 0C ;
END CardToHex8 ;


PROCEDURE CardToHex6 (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	IF wert > 0FFFFFFH THEN
		CardToHex8 (wert, str) ;
	ELSE
		FOR i := 1 TO 6 DO
			str [6-i] := tet [wert BAND 0FH] ;
			wert := wert SHR 4 ;
		END ;
		str [6] := 0C ;
	END ;
END CardToHex6 ;


PROCEDURE CardToHex4 (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	IF wert > 0FFFFH THEN
		CardToHex6 (wert, str) ;
	ELSE
		FOR i := 1 TO 4 DO
			str [4-i] := tet [wert BAND 0FH] ;
			wert := wert SHR 4 ;
		END ;
		str [4] := 0C ;
	END ;
END CardToHex4 ;


PROCEDURE CardToHex2 (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
	VAR	i :	CARDINAL ;
BEGIN
	IF wert > 0FFH THEN
		CardToHex4 (wert, str) ;
	ELSE
		FOR i := 1 TO 2 DO
			str [2-i] := tet [wert BAND 0FH] ;
			wert := wert SHR 4 ;
		END ;
		str [2] := 0C ;
	END ;
END CardToHex2 ;


PROCEDURE CardToHex1 (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
BEGIN
	IF wert > 0FH THEN
		CardToHex2 (wert, str) ;
	ELSE
		str [0] := tet [wert BAND 0FH] ;
		str [1] := 0C ;
	END ;
END CardToHex1 ;


PROCEDURE CardToDez (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
BEGIN
	Conversions.CardToStr (wert, str) ;
END CardToDez ;


PROCEDURE IntToDez (wert : CARDINAL ; VAR str : ARRAY OF CHAR) ;
BEGIN
	Conversions.IntToStr (wert, str) ;
END IntToDez ;


PROCEDURE Int8ToDez (wert : INTEGER ; VAR str : ARRAY OF CHAR) ;
BEGIN
	IF wert > 127 THEN
		wert := wert - 255 ;
	END ;
	Conversions.IntToStr (wert, str) ;
END Int8ToDez ;


PROCEDURE CardToText (wert : CARDINAL ; maske : ARRAY OF CHAR ; VAR str : ARRAY OF CHAR) ;
	VAR	i,
		j :	CARDINAL ;
BEGIN
	j := 0 ;
	str [0] := 0C ;
	FOR i := 0 TO 15 DO
		CASE maske [i] OF
		' ',
		'.' :									(* nicht berücksichtigen *)
		|
		'*' :
			str [0] := CHR ((wert SHR 15) BAND 3 BOR ORD ('0')) ;		(* 2 durch TK überschreiben *)
			j := 1 ;
			str [1] := 0C ;
		|
		'l' :
			IF (wert BAND 8000H) = 0 THEN					(* Zeichen aus Maske, wenn Bit nicht gesetzt *)
				str [j] := 'L' ;
				j := j+1 ;
				str [j] := 0C ;
			END ;
		ELSE
			IF (wert BAND 8000H) <> 0 THEN					(* Zeichen aus Maske, wenn Bit gesetzt *)
				str [j] := maske [i] ;
				j := j+1 ;
				str [j] := 0C ;
			END ;
		END ;
		wert := wert SHL 1 ;
	END ;
END CardToText ;


PROCEDURE protZeile (str : ARRAY OF CHAR) ;
BEGIN
	Terminal.WriteLn ;
	Terminal.WriteString (str) ;
END protZeile ;


PROCEDURE protZeile2 (str1, str2 : ARRAY OF CHAR) ;
	VAR	str :	ARRAY [0..255] OF CHAR ;
BEGIN
	str := TAB ;
	Strings.Append (str1, str) ;
	Strings.Append (TAB, str) ;
	Strings.Append (str2, str) ;
	protZeile (str) ;
END protZeile2 ;


PROCEDURE PutIntoClipboard (txt : ARRAY OF CHAR) ;
        VAR     lng :   CARDINAL ;
                mem :   HANDLE ;
                ptr :   POINTER TO ARRAY [0..1023] OF CHAR ;
BEGIN
        IF NOT OpenClipboard (NIL) THEN
                RETURN ;
        END ;
        EmptyClipboard ;
        lng := LENGTH (txt) ;
        mem := GlobalAlloc (GMEM_MOVEABLE, lng+1) ;
        ptr := GlobalLock (mem) ;
        ptr ^ [0..lng] := txt [0..lng] ;
        GlobalUnlock (mem) ;
        SetClipboardData (CF_TEXT, mem) ;
        CloseClipboard ;
END PutIntoClipboard ;

%IF %NOT WEB %THEN


PROCEDURE ShowMemory ;
	VAR	adr :	Struktur.SpeicherAdresse ;
BEGIN
	protZeile ('akt. Leitblock :') ;
	Terminal.WriteLn ;
	FOR adr := 0 TO Struktur.AktMaxGanzwortNummer BY Struktur.MaxGanzwortInSeite+1 DO
		IF (adr <> 0) AND ((adr BAND 7FFFH) = 0) THEN			(* Großseiten-Grenze *)
			IF (adr BAND 1FFFFH) = 0 THEN		(* 4 Großseiten gezeigt *)
				Terminal.WriteLn ;
			ELSE
				Terminal.WriteString ('  ') ;
			END ;
		END ;
		IF Struktur.TestAdresse (adr * 2) THEN
			IF Struktur.AktSchreibschutz THEN
				Terminal.Write ('S') ;
			ELSE
				Terminal.Write ('+') ;
			END ;
		ELSE
			Terminal.Write ('X') ;
		END ;
	END ;
END ShowMemory ;


PROCEDURE GetMnemo (befehl : CARDINAL; VAR mnemo : ARRAY OF CHAR) ;
	VAR	opcode,
		zweitOp :	tOpcode ;
		zw :		ARRAY [0..15] OF CHAR ;
BEGIN
	opcode := VAL (tOpcode, befehl SHR 16) ;

	CASE opcode OF
	opMFUMXB :					(* MFU / M /XB *)

	  			CASE befehl BAND 0E000H OF
				2000H :	mnemo := 'MFU ' ;
				|
				4000H :	mnemo := 'XB ' ;
				|
				8000H :	mnemo := 'M ' ;
				ELSE
					mnemo := '?08 ' ;
				END ;
				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opXCXCN :					(* XC / XCN *)

				IF (befehl BAND 8000H) <> 0 THEN
					mnemo := 'XCN ' ;
				ELSE	mnemo := 'XC ' ;
				END ;
				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opMF :
				mnemo := 'MF ' ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opRXMRX :					(* RX / MRX *)

				IF befehl BAND 200H <> 0 THEN
					mnemo := 'MRX ' ;
				ELSE
					mnemo := 'RX ' ;
				END ;
				CardToText (befehl, spezRXMRX, zw) ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;
				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opE,					(* E *)
	opEZ,					(* EZ *)
	opENZ,					(* ENZ *)
	opEMB,					(* EMB *)
	opMAB,					(* MAB *)
	opMABI,					(* MABI *)
	opMU,					(* MU *)
	opEMU :					(* EMU *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				Rekursion := TRUE ;
	  			GetMnemo ((befehl SHL 8 BAND 0FF0000H) BOR (befehl BAND 0FFH), zw) ;
				Rekursion := FALSE ;

				Strings.Append (zw, mnemo) ;
	|
	opRLR :					(* RLR *)
				mnemo := 'RLR ' ;

				Rekursion := TRUE ;
	  			GetMnemo ((befehl SHL 8 BAND 0FF0000H) BOR 0FFFFH, zw) ;
				Rekursion := FALSE ;

				Strings.Append (zw, mnemo) ;

				IF (befehl BAND 8) <> 0 THEN
					CardToText (befehl BAND 0FFH, spezRRLRbit21, zw) ;
				ELSE
					CardToText (befehl BAND 0FFH, spezRRLR, zw) ;
				END ;

				Strings.Append (zw, mnemo) ;
	|
	opR :					(* R *)
				mnemo := 'R ' ;

				zweitOp := VAL (tOpcode, befehl SHR 8) ;
				IF NOT (zweitOp IN RzweitCodeErlaubt) THEN
					Strings.Append ('*?', mnemo) ;
				END ;

				Rekursion := TRUE ;
	  			GetMnemo ((befehl SHL 8 BAND 0FF0000H) BOR 0FFFFH, zw) ;
				Rekursion := FALSE ;

				Strings.Append (zw, mnemo) ;

				IF (befehl BAND 8) <> 0 THEN
					CardToText (befehl BAND 0FFH, spezRRLRbit21, zw) ;
				ELSE
					CardToText (befehl BAND 0FFH, spezRRLR, zw) ;
				END ;

				Strings.Append (zw, mnemo) ;
	|
	opBNZ,					(* BNZ *)
	opCNZ,					(* CNZ *)
	opTTX,					(* TTX *)
	opTXX,					(* TXX *)
	opWTV,					(* WTV *)
	opWTR,					(* WTR *)
	opHXX,					(* HXX *)
	opVXX,					(* VXX *)
	opSSR,					(* SSR *)
	opMHX,					(* MHX *)
	opBSS,
	opMD :					(* MD *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				CardToDez ((befehl SHR 8) BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;


				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opTXR,					(* TXR *)
	opTRX :					(* TRX *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				CardToText (befehl, spezTXRTRX, zw) ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opHBPX,					(* HBPX *)
	opHXP,					(* HXP *)
	opMH,					(* MH *)
	opZX,					(* ZX *)
	opSZX :					(* SZX *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				Int8ToDez ((befehl SHR 8) BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;


				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opSL,					(* SL *)
	opSLL,					(* SLL *)
	opSLN,					(* SLN *)
	opSNL,
	opNL :					(* SNL *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				IF opcode <> opNL THEN
					Int8ToDez ((befehl SHR 8) BAND 0FFH, zw) ;
					Strings.Append (zw, mnemo) ;

					Strings.Append (' ', mnemo) ;
				END ;

				CardToText (befehl, spezSL, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opLZL :					(* LZL *)
				mnemo := 'LZL ' ;

				IF (befehl BAND 0FF00H) = 0 THEN
					zw := '0' ;
				ELSE
					CardToText (befehl, spezLZL, zw) ;
				END ;

				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;

				IF (befehl BAND 0FFH) = 0 THEN
					zw := '0' ;
				ELSE
					CardToText (befehl, spezSL, zw) ;
				END ;

				Strings.Append (zw, mnemo) ;
	|
	opST,					(* ST *)
	opSTN :					(* STN *)
				mnemo := Mnemo [opcode] ;

				Strings.Append (' ', mnemo) ;

				Int8ToDez ((befehl SHR 8) BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;


				Strings.Append (' ', mnemo) ;
				CardToText (befehl, spezSTSTN, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opSH :					(* SH *)
				mnemo := 'SH ' ;

				CardToText (befehl, spezSH, zw) ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;

	|
	opSHB :					(* SHB *)
				mnemo := 'SHB ' ;

				IF (befehl BAND 8000H) = 0 THEN
					zw := 'R' ;
				ELSE
					CardToText (befehl, spezSHB, zw) ;
				END ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opLA :					(* LA *)
				mnemo := 'LA ' ;

				CardToText (befehl, spezLA, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opLR :					(* LR *)
				mnemo := 'LR ' ;

				CardToText (befehl, spezLR, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opIR :					(* IR *)
				mnemo := 'IR ' ;

				CardToText (befehl, spezIR, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opZTR :					(* ZTR *)
				mnemo := 'ZTR  ' ;

				CardToText (befehl, spezZTR, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opSBIT :				(* SBIT *)
				mnemo := 'SBIT ' ;

				Int8ToDez ((befehl SHR 8) BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;
				CardToDez (befehl BAND 3FH, zw) ;
				Strings.Append (zw, mnemo) ;

				CASE (befehl SHR 6) BAND 3 OF
				0 :	zw := 'A' ;
				|
				1 :	zw := 'Q' ;
				|
				2 :	zw := 'D' ;
				ELSE
					zw := 'H' ;
				END ;
				Strings.Append (zw, mnemo) ;
	|
	opZK :					(* ZK *)
				mnemo := 'ZK ' ;
				CardToText (befehl, spezZK, zw) ;
				Strings.Append (zw, mnemo) ;

				Strings.Append (' ', mnemo) ;

				IF (befehl BAND 1000H) = 0 THEN
					CardToDez (befehl BAND 0FFH, zw) ;
				ELSE
					CardToText (befehl, spezZK2, zw) ;
				END ;
				Strings.Append (zw, mnemo) ;

	|
	opRT :					(* RT *)
				mnemo := 'RT ' ;

				CardToText (befehl, spezRT, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opSEGG :					(* SEGG *)
				mnemo := 'SEGG ' ;

				Int8ToDez ((befehl SHR 8) BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;


				Strings.Append (' ', mnemo) ;

				Int8ToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opUS :					(* US *)
				mnemo := 'US ' ;

				CardToText (befehl, spezUS, zw) ;
				Strings.Append (zw, mnemo) ;


				Strings.Append (' ', mnemo) ;

				CardToDez (befehl BAND 0FFH, zw) ;
				Strings.Append (zw, mnemo) ;
	|
	opBA,					(* BA *)
	opBAR,					(* BAR *)
	opBAN,					(* BAN *)
	opBANR,					(* BANR *)
	opAA,					(* AA *)
	opSBA,					(* SBA *)
	opXBA,					(* XBA *)
	opXBAN,					(* XBAN *)
	opHBA,					(* HBA *)
	opVBA,					(* VBA *)
	opMA,					(* MA *)
	opMNA,					(* MNA *)
	opWB,					(* WB *)
	opKDFR,					(* KDFR *)
	opKFLD,					(* KFLD *)
	opTOK :					(* TOK *)
				mnemo := Mnemo [opcode] ;
				Strings.Append (' ', mnemo) ;

				IF (befehl BAND 0FFFFH) < 100 THEN
					CardToDez (befehl BAND 0FFFFH, zw) ;
				ELSE
					CardToHex4 (befehl BAND 0FFFFH, zw) ;
				END ;
				Strings.Append (zw, mnemo) ;
	|
	opNRM :
				mnemo := 'NRM ' ;
				CASE (befehl SHR 5) BAND 7 OF
				  0 :	zw := 'N' ;
				| 1 :	zw := 'L' ;
				| 2 :	zw := 'F' ;
				| 3 :	zw := 'F4' ;
				| 4 :	zw := 'G' ;
				| 6 :	zw := 'FG' ;
				ELSE	zw := '??' ;
				END ;
				Strings.Append (zw, mnemo) ;
	ELSE
				mnemo := Mnemo [opcode] ;
				Strings.Append (' ', mnemo) ;
				IF Rekursion THEN
					IF (befehl BAND 0FFFFH) <> 0FFFFH THEN
						Int8ToDez (befehl BAND 0FFH, zw) ;
					ELSE
						zw := '' ;
					END ;
				ELSE
					CardToHex4 (befehl BAND 0FFFFH, zw) ;
				END ;
				Strings.Append (zw, mnemo) ;
	END ;
END GetMnemo ;


PROCEDURE ConstructMnemoSuche ;
	VAR	i, j,
		minindex,
		minMnemo :	CARDINAL ;
		op :		tOpcode ;
		zwMnemoSuche :	ARRAY [0..255] OF MnemoRecord ;
BEGIN
	MnemoAnzahl := 0 ;
	FOR op := MIN(tOpcode) TO MAX(tOpcode) DO
		CASE Mnemo [op] [0] OF
		'?',
		'|' :	(* nicht eintragen *)
		ELSE
			WITH zwMnemoSuche [MnemoAnzahl] DO
				zeichen := CAST (CARDINAL, Mnemo [op]) ;
				opcode := ORD (op) SHL 16 ;
			END ;
			INC (MnemoAnzahl) ;
		END ;
	END ;
	FOR i := 0 TO HIGH(ZusMnemo) DO
		WITH zwMnemoSuche [MnemoAnzahl] DO
			zeichen := CAST (CARDINAL, ZusMnemo [i]) ;
			opcode := ORD (ZusOpcode [i]) SHL 16 BOR ZusOpcodeBytes [i] ;
		END ;
		INC (MnemoAnzahl) ;
	END ;

	FOR i := 1 TO MnemoAnzahl DO			(* sortiert ins Suchfeld *)
		minMnemo := 0FFFFFFFFH ;
		FOR j := 1 TO MnemoAnzahl DO		(* kleinstes nicht zugeordnetes Element suchen *)
			WITH zwMnemoSuche [j-1] DO
				IF zeichen < minMnemo THEN
					minMnemo := zeichen ;
					minindex := j-1 ;
				END ;
			END ;
		END ;
		MnemoSuche [i-1] := zwMnemoSuche [minindex] ;
		zwMnemoSuche [minindex].zeichen := 0FFFFFFFFH ;	(* merke : ist zugeordnet *)
	END ;
END ConstructMnemoSuche ;


PROCEDURE SuchMnemo (mnemo : ARRAY OF CHAR) : CARDINAL ;		(* optimierte binäre Suche *)
	VAR	i,
		l,
		z,
		min,
		max,
		such :		CARDINAL ;
		ch :		CHAR ;
		mn :		tMnemo ;
BEGIN
	l := LENGTH(mnemo) ;
	IF (l = 0) OR (l > 4) THEN
		RETURN 1000 ;
	END ;

	mn := mnemo ;
	FOR i := 0 TO l-1 DO
		ch := mnemo [i] ;
		IF ch >= 'a' THEN					(* Capitalize *)
			mn [i] := CHR (ORD (ch) - ORD ('a') + ORD ('A')) ;
		END ;
	END ;
	FOR i := l TO 3 DO
		mn [i] := 0C ;
	END ;
	such := CAST (CARDINAL, mn) ;

	i := MnemoAnzahl DIV 2 ;
	min := 0 ;
	max := MnemoAnzahl-1 ;
	LOOP
		z := MnemoSuche [i].zeichen ;
		IF z = such THEN
			RETURN i ;			(* gefunden *)
		END ;
		IF z > such THEN
			IF i = 0 THEN
				RETURN 1000 ;		(* nicht gefunden : zu klein *)
			END ;
			max := i - 1 ;
		ELSE
			min := i + 1 ;
		END ;
		IF min > max THEN	(* nicht gefunden *)
			RETURN 1000 ;
		END ;
		i := min + (max - min) DIV 2 ;
	END ;
END SuchMnemo ;


PROCEDURE BefehlGueltig (mnemo : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	RETURN SuchMnemo (mnemo) <> 1000 ;
END BefehlGueltig ;


PROCEDURE GetBefehl (mnemo : ARRAY OF CHAR) : (* 24-Bit-Befehl : *)  CARDINAL ;
	VAR	i :	CARDINAL ;
BEGIN
	i := SuchMnemo (mnemo) ;
	IF i = 1000 THEN
		RETURN 07FFFFFFFH ;	(* nicht gefunden *)
	END ;

	RETURN MnemoSuche [i].opcode ;

END GetBefehl ;


(*	unsortierte Version :


PROCEDURE BefehlGueltig (mnemo : ARRAY OF CHAR) : BOOLEAN ;
	VAR	op :	tOpcode ;
		i :	CARDINAL ;
BEGIN
	Strings.Capitalize (mnemo) ;
	FOR i := LENGTH(mnemo) TO 3 DO
		mnemo [i] := 0C ;
	END ;
	FOR op := MIN(tOpcode) TO MAX(tOpcode) DO
		IF CAST(CARDINAL,mnemo) = CAST (CARDINAL, Mnemo [op]) THEN
			RETURN TRUE ;
		END ;
	END ;
	FOR i := 0 TO HIGH(ZusMnemo) DO
		IF CAST(CARDINAL,mnemo) = CAST (CARDINAL, ZusMnemo [i]) THEN
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END BefehlGueltig ;


PROCEDURE GetBefehl (mnemo : ARRAY OF CHAR) : (* 24-Bit-Befehl : *)  CARDINAL ;
	VAR	op :	tOpcode ;
		i :	CARDINAL ;
BEGIN
	Strings.Capitalize (mnemo) ;
	FOR i := LENGTH(mnemo) TO 3 DO
		mnemo [i] := 0C ;
	END ;
	FOR op := MIN(tOpcode) TO MAX(tOpcode) DO
		IF CAST(CARDINAL,mnemo) = CAST (CARDINAL, Mnemo [op]) THEN
			RETURN ORD(op) SHL 16 ;
		END ;
	END ;
	FOR i := 0 TO HIGH(ZusMnemo) DO
		IF CAST(CARDINAL,mnemo) = CAST (CARDINAL, ZusMnemo [i]) THEN
			RETURN (ORD(ZusOpcode [i]) SHL 16 BOR ZusOpcodeBytes [i]) ;
		END ;
	END ;

	RETURN 07FFFFFFFH ;
END GetBefehl ;

*)

PROCEDURE Get_R_RLR (adressteil : CARDINAL8 ; VAR ext : ARRAY OF CHAR) : BOOLEAN ;
BEGIN
	CASE (ORD(adressteil) SHR 3) BAND 1FH OF
	0,
	1 :	ext := '?' ;
		RETURN FALSE ;
	|
	2 :	ext := 'H' ;
	|
	4 :	ext := 'D' ;
	|
	8 :	ext := 'Q' ;
	|
	16 :	ext := 'A' ;
	|
	3 :	ext := 'Y' ;
	|
	5 :	ext := 'B' ;
	|
	9 :	ext := 'U' ;
	|
	17 :	ext := 'F' ;
	ELSE
		ext := '?' ;
		RETURN FALSE ;
	END ;
	IF NOT ODD (adressteil) THEN
		ext [1] := 'L' ;
		ext [2] := 0C ;
	END ;
	RETURN TRUE ;
END Get_R_RLR ;


VAR
	FehlerUnterdruecken :	BOOLEAN = FALSE ;


PROCEDURE GetZusatzbits (spez, maske : ARRAY OF CHAR) : CARDINAL16 ;
	VAR	l,
		s,
		m :		CARDINAL ;
		wert :		CARDINAL16 ;
		benutzt :	BOOLEAN ;
		str :		ARRAY [0..63] OF CHAR ;
BEGIN
	IF spez [0] = "'" THEN			(* bewusst Hexwert vorgegeben *)
		l := LENGTH (spez)-1 ;
		IF spez [l] = "'" THEN		(* korrektes Hexwert-Ende *)
			IF Assembler.GetZahl (spez, s) THEN
				RETURN s ;
			END ;
		END ;
	END ;

	wert := 0 ;
	benutzt := FALSE ;
	FOR m := 1 TO LENGTH (maske) DO
		IF maske [m-1] = 'l' THEN
			wert := wert BOR Struktur.Bitmasken16 [m] ;
		END ;
	END ;
	FOR s := 1 TO LENGTH (spez) DO
		FOR m := 1 TO LENGTH (maske) DO
			IF (spez [s-1] = maske [m-1]) AND (spez [s-1] <> ' ') THEN
				spez [s-1] := ' ' ;
				wert := wert BOR Struktur.Bitmasken16 [m] ;
				benutzt := TRUE ;
			ELSIF ((spez [s-1] = 'L') AND (maske [m-1] = 'l'))
			   OR ((spez [s-1] = 'E') AND (maske [m-1] = 'e')) THEN
				spez [s-1] := ' ' ;
				wert := wert BAND (BNOT Struktur.Bitmasken16 [m]) ;
			ELSIF maske [m-1] = '*' THEN
				   IF spez [s-1] = '0' THEN
					spez [s-1] := ' ' ;
				ELSIF spez [s-1] = '1' THEN
					spez [s-1] := ' ' ;
					wert := wert BOR Struktur.Bitmasken16 [m] ;
				ELSIF spez [s-1] = '3' THEN
					spez [s-1] := ' ' ;
					wert := wert BOR Struktur.Bitmasken16 [m] BOR Struktur.Bitmasken16 [m-1] ;
				END ;
			END ;
		END ;
	END ;
	IF benutzt THEN
		FOR m := 1 TO LENGTH(maske) DO
			IF maske [m-1] = '.' THEN
				wert := wert BOR Struktur.Bitmasken16 [m] ;
			END ;
		END ;
	END ;
	IF NOT FehlerUnterdruecken THEN
		FOR s := 1 TO LENGTH (spez) DO
			IF spez [s-1] > ' ' THEN
				str := '*** unbekannte Spezifikation : ' ;
				Strings.Append (spez, str) ;
				Assembler.SyntaxWarnung (str) ;
				BREAK ;
			END ;
		END ;
	END ;
	RETURN wert ;
END GetZusatzbits ;


PROCEDURE GetZusatzbitsTest (spez, maske : ARRAY OF CHAR) : CARDINAL16 ;
	VAR	zw :	CARDINAL16 ;
BEGIN
	FehlerUnterdruecken := TRUE ;
	zw :=  GetZusatzbits (spez, maske) ;
	FehlerUnterdruecken := FALSE ;
	RETURN zw ;
END GetZusatzbitsTest ;



PROCEDURE BefehleZeigen (adr : ARRAY OF CHAR) ;		(* Befehle im Speicher disassembliert zeigen *)
	VAR	j,
		adresse,
		befehl :	CARDINAL ;
		str :		ARRAY [0..31] OF CHAR ;
		merk :		BOOLEAN ;
BEGIN
	TRIM (adr) ;
	IF Assembler.GetZahl (adr, adresse) THEN
		merk:= debug.Lesen ;
		debug.Lesen := FALSE ;
		REPEAT
			FOR j := 0 TO 15 DO
				Terminal.WriteLn ;
				CardToHex4 (adresse + j, str) ;
				Terminal.WriteString (str) ;
				Terminal.WriteString (' ') ;
				befehl := Struktur.GetHalbwort(adresse + j) ;
				IF Struktur.SpeicherschutzAlarmGewesen THEN
					BREAK ;
				END ;
				CardToHex6 (befehl, str) ;
				Terminal.WriteString (str) ;
				Terminal.WriteString (' ') ;
				GetMnemo (befehl, str) ;
				Terminal.WriteString (str) ;
			END ;
			INC (adresse, 16) ;
		UNTIL Terminal.ReadChar () = Terminal.Escape ;
		debug.Lesen := merk ;
	ELSE
		protZeile ('+++++ unzul. Adresse') ;
	END ;
END BefehleZeigen ;


PROCEDURE Zeigen (adr : ARRAY OF CHAR) ;		(* Speicherinhalt auf Kommando zeigen *)
	VAR	i,
		j,
		adresse :	CARDINAL ;
		inh :		CARDINAL64 ;
		str :		ARRAY [0..31] OF CHAR ;
		merk :		BOOLEAN ;

	PROCEDURE ShowText (reladr : CARDINAL ; lng : CARDINAL) ;
		VAR	i :	CARDINAL ;
			str :	ARRAY [0..15] OF CHAR ;
			ch :	CHAR ;
	BEGIN
		Struktur.GetStringAusSpeicher (str, reladr, lng) ;
		FOR i := 0 TO lng-1 DO
			ch := str [i] ;
			IF ch >= ' ' THEN
				Terminal.Write (ch) ;
			ELSE
				Terminal.Write ('.') ;
			END ;
		END ;
	END ShowText ;

BEGIN
	TRIM (adr) ;
	merk:= debug.Lesen ;
	debug.Lesen := FALSE ;
	IF Strings.Equal (adr, 'X') THEN		(* Indexzellen zeigen *)
		Terminal.WriteLn ;
		adresse := Register.RegX ;
		FOR i := 0 TO 15 DO
			Terminal.WriteF ("\n%'02h ", i * 16) ;
			FOR j := 0 TO 15 DO
				Terminal.WriteF (" %'06h", Struktur.GetHalbwort (adresse + i * 16 + j)) ;
			END ;
		END ;

	ELSIF Assembler.GetZahl (adr, adresse) THEN
		REPEAT
			Terminal.WriteLn ;
			CardToHex4 (adresse, str) ;
			Terminal.WriteString (str) ;
			Terminal.WriteString (' ') ;
			FOR j := 0 TO 7 BY 2 DO
				Terminal.WriteString ('   ') ;
				IF ODD (adresse) THEN

					CardToHex6 (Struktur.GetHalbwort(adresse + j), str) ;
					IF Struktur.SpeicherschutzAlarmGewesen THEN
						BREAK ;
					END ;
					Terminal.WriteString (str) ;

					Terminal.WriteString (' ') ;
					ShowText (adresse + j, 3) ;

					Terminal.WriteString ('   ') ;
					CardToHex6 (Struktur.GetHalbwort(adresse + j + 1), str) ;
					IF Struktur.SpeicherschutzAlarmGewesen THEN
						BREAK ;
					END ;
					Terminal.WriteString (str) ;

					Terminal.WriteString (' ') ;
					ShowText (adresse + j + 1, 3) ;
				ELSE
					inh := Struktur.GetGanzwort(adresse + j) ;
					IF Struktur.SpeicherschutzAlarmGewesen THEN
						BREAK ;
					END ;
					Terminal.WriteCard (Struktur.AktTypenkennung) ;
					Terminal.WriteString (' ') ;
					CardToHex12 (inh, str) ;
					Terminal.WriteString (str) ;

					Terminal.WriteString (' ') ;
					ShowText (adresse + j, 6) ;
					Terminal.WriteString ('  ') ;
				END ;
			END ;
			INC (adresse, 8) ;
		UNTIL Terminal.ReadChar () = Terminal.Escape ;
	ELSE
		protZeile ('+++++ unzul. Adresse') ;
	END ;
	debug.Lesen := merk ;
END Zeigen ;


PROCEDURE TraceBefehl (bef : ARRAY OF CHAR) ;
	VAR	j :	CARDINAL ;
		txt :	ARRAY [0..15] OF CHAR ;
		wdh :	BOOLEAN ;
BEGIN
	txt := bef ;
	REPEAT
		wdh := FALSE ;

		FOR j := 1 TO LENGTH(txt) DO
			CASE CAP(txt [j-1]) OF
			' ' :	(* skip *)
			|
			'?' :
				protZeile ('Trace-Einstellungen :') ;
				protZeile2 ('0',	'alles aus') ;
				protZeile2 ('1',	'alles an') ;
				protZeile2 ('E',	'Einzelschritt an') ;
				protZeile2 ('B',	'Befehle tracen') ;
				protZeile2 ('Q',	'Quellzeile zeigen') ;
				protZeile2 ('R',	'Registeränderungen tracen') ;
				protZeile2 ('Y',	'SSR-Protokoll mit VBL zeigen') ;
				protZeile2 ('F',	'Einzelschritt ein bei Alarm/SSR-Fehlern') ;
				protZeile2 ('-',	'Trace nicht auf Bildschirm') ;
				protZeile2 ('X',	'für normal-TAS-Debug (~0EBQRY)') ;
				protZeile2 (' ',	' ') ;
				protZeile2 (' ',	'*** Zusatz-Ausgaben SEBtrace : ***') ;
				protZeile2 ('T',	'Takt-Anzahlen zeigen') ;
				protZeile2 ('L',	'Lesen tracen') ;
				protZeile2 ('S',	'Speichern tracen') ;
				protZeile2 ('A',	'Befehlsauslese tracen') ;
				protZeile2 ('I',	'Indexzugriffe tracen') ;
				protZeile2 ('C',	'Assoziativ-Register tracen') ;
				protZeile2 ('Z',	'Zweitbefehle tracen') ;
				protZeile2 ('D',	'Dateizugriffe zeigen') ;
				protZeile2 ('P',	'Assembler-Protokoll zeigen') ;
				protZeile2 ('M',	'Memory-Alloc/Dealloc zeigen') ;
			|
			'0' :
				debug.alle (FALSE) ;
			|
			'1' :
				debug.alle (TRUE) ;
			|
			'R' :
				debug.an := TRUE ;
				debug.RegDump := TRUE ;

			|
			'L' :
				debug.an := TRUE ;
				debug.Lesen := TRUE ;

			|
			'S' :
				debug.an := TRUE ;
				debug.Speichern := TRUE ;
			|
			'A' :
				debug.an := TRUE ;
				debug.BefehlLesen := TRUE ;
			|
			'I' :
				debug.an := TRUE ;
				debug.IndexWert := TRUE ;
			|
			'C' :
				debug.an := TRUE ;
				debug.AssoziativProtokoll := TRUE ;
			|
			'B' :
				debug.an := TRUE ;
				debug.Befehle := TRUE ;

			|
			'Z' :
				debug.an := TRUE ;
				debug.ZweitBefehle := TRUE ;
			|
			'M' :	debug.an := TRUE ;
				debug.MemoryProtokoll := TRUE ;
			|
			'E' :
				debug.an := TRUE ;
				debug.Einzelschritt := TRUE ;
				debug.BildschirmProtokoll := TRUE ;
				debug.Befehle := TRUE ;
			|
			'F' :
				debug.an := TRUE ;
				debug.AlarmEin := TRUE ;
			|
			'Q' :
				debug.an := TRUE ;
				debug.OriginalZeile := TRUE ;
			|
			'T' :	debug.an := TRUE ;
				debug.TakteZeigen := TRUE ;
			|
			'D' :	debug.an := TRUE ;
				debug.Dateien := TRUE ;
			|
			'P' :	debug.an := TRUE ;
				debug.AssemblerProtokoll := TRUE ;
			|
			'Y' :	debug.an := TRUE ;
				debug.SSRprotokoll := TRUE ;
			|
			'-' :	debug.an := TRUE ;
				debug.BildschirmProtokoll := FALSE ;
				debug.Einzelschritt := FALSE ;
			|
			'X' :
				txt := '0EBQRY' ;
				wdh := TRUE ;
				BREAK ;
			|
			']' :
				AsmTrace.ExitTrace ;
			|
			'[' :
				AsmTrace.RestoreTrace ('') ;
			ELSE
				protZeile ('+++++ unbekannte Spezifikation') ;
			END ;
		END ;

	UNTIL NOT wdh ;
END TraceBefehl ;


PROCEDURE init ;
BEGIN
	ConstructMnemoSuche ;
END init ;


BEGIN
	IF NOT initialisiert THEN
		initialisiert := TRUE ;
		init ;
	END ;

%END

END AsmDisasm.
