
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE Befehle;

(*	04.05.16	*)


IMPORT Strings ;

FROM Register IMPORT * ;
FROM TransportBefehle IMPORT * ;
FROM FestkommaArithmetik IMPORT * ;
FROM GleitkommaArithmetik IMPORT * ;
FROM IndexArithmetik IMPORT * ;
FROM Ersetzen IMPORT * ;
FROM Modifizieren IMPORT * ;
FROM SetzenLoeschen IMPORT * ;
FROM TeilwortArithmetik IMPORT * ;
FROM Aufbereitung IMPORT * ;
FROM Tabelle IMPORT * ;
FROM Spruenge IMPORT * ;
FROM SystemModus IMPORT * ;
FROM Abwickler IMPORT * ;
FROM Struktur IMPORT * ;
FROM upFestkomma IMPORT
	trAdd24 ;

FROM AsmDisasm IMPORT
%IF %NOT WEB %THEN
	GetMnemo,
%END
	tOpcode, RechenwerkBenutzt,
	CardToHex2, CardToHex4, CardToHex6 ;

IMPORT debug, FormatString, Terminal ;

FROM Trace IMPORT
	TraceF ;


VAR
	ersterInternBefehl :	BOOLEAN ;


PROCEDURE nimpl ;
	VAR	mnemo :	ARRAY [0..15] OF CHAR ;
		str :	ARRAY [0..255] OF CHAR ;
BEGIN
%IF %NOT WEB %THEN
	IF debug.an THEN
		GetMnemo (ORD(AktBefehlscode) SHL 16 BOR ORD(AktAdressteil) BAND 0FFFFH, mnemo) ;
		FormatString.FormatString ("+++++ nicht implementiert %'02h%'04h %s", str, ORD(AktBefehlscode), AktAdressteil, mnemo) ;
		Terminal.WriteLn ;
		Terminal.WriteString (str) ;
		TraceF ('%s', str) ;
	END ;
%END
	AlarmGewesen := TRUE ;
END nimpl ;


PROCEDURE mod1Check ;
BEGIN
	IF mod1 <> 0 THEN
		Takte (8) ;
		AktAdressteil := trAdd24 (AktAdressteil, mod1) ;
		mod1 := 0 ;
	END ;
END mod1Check ;


PROCEDURE BefehlAusfuehren (befehl : CARDINAL) ;	(* frisch eingelesener Befehl *)
	VAR	mnemo :	ARRAY [0..31] OF CHAR ;
BEGIN
	AktBefehlscode := befehl SHR 16 BAND 0FFH ;

	AktAdressteil := befehl BAND 0FFFFH ;		(* wird evtl. noch modifiziert *)

	FolgeAdresse := RegF + 1 ;

%IF %NOT WEB %THEN
	IF debug.Befehle AND NOT debug.BefehlLesen THEN
		GetMnemo (befehl, mnemo) ;
		TraceF ("* Befehl : %'06h   %s", befehl, mnemo) ;
	END ;
%END

	mod1Check ;

	ersterInternBefehl := TRUE ;

	internBefehlAusfuehren ;

	IF debug.RegDump THEN
		debug.RegisterDumpen ;
	END ;
END BefehlAusfuehren ;


PROCEDURE internBefehlAusfuehren ;			(* auch nach Ersetzungen *)
	VAR	mnemo,
		str1,
		str2 :	ARRAY [0..31] OF CHAR ;
BEGIN
%IF %NOT WEB %THEN

	IF debug.ZweitBefehle THEN
		IF (debug.Befehle OR debug.BefehlLesen) AND ersterInternBefehl THEN
			ersterInternBefehl := FALSE ;
		ELSE
			GetMnemo ((ORD(AktBefehlscode) SHL 16) BOR (AktAdressteil BAND 0FFFFH), mnemo) ;
			TraceF ("  Bef    : %'02h.%'04h   %s", ORD(AktBefehlscode), AktAdressteil, mnemo) ;
		END ;
	END ;
%END

	IF TKalarmErkannt AND (VAL (tOpcode, AktBefehlscode) IN RechenwerkBenutzt) THEN
		TypenkennungsAlarm2 ;
	ELSIF BUEalarmErkannt AND (VAL (tOpcode, AktBefehlscode) IN RechenwerkBenutzt) THEN
		ArithmetischerAlarm2 ;
	ELSE
		CASE AktBefehlscode OF
		00H :	_NULL ;
		|
		01H :	_XBA ;
		|
		02H :	_MNA ;
		|
		03H :	_MA ;
		|
		04H :	_EMU ;
		|
		05H :	_MU ;
		|
		06H :	_BCL ;
		|
		07H :	_TBC ;
		|
		08H :
			CASE (AktAdressteil SHR 13) BAND 7 OF
			1 :	_MFU ;
			|
			2 :	_XB ;
			|
			4 :	_M ;
			ELSE
				BefehlsAlarm ;
			END ;
		|
		09H :	_MD ;
		|
		0AH :	_SZX ;
		|
		0BH :	_MF ;
		|
		0CH :	_TXX ;
		|
		0DH :	_TTX ;
		|
		0EH :	_MHX ;
		|
		0FH :	_HBPX ;

		|
		10H :	_LZL ;
		|
		11H :	_HBA ;
		|
		12H :	_NL ;
		|
		13H :	_VBA ;
		|
		14H :	_MC ;
		|
		15H :	_VBC ;
		|
		16H :	_MCF ;
		|
		17H :	_MCE ;
		|
		18H :
			IF AktAdressteil BAND 8000H <> 0 THEN
				_XCN ;
			ELSE
				_XC ;
			END ;
		|
		19H :	_XBAN ;
		|
		1AH :	_ZX ;
		|
		1BH :	_SW ;
		|
		1CH :	_SLN ;
		|
		1DH :	_SNL ;
		|
		1EH :	_SL ;
		|
		1FH :	_SLL ;

		|
		20H :	_MAB ;
		|
		21H :	_SHB ;
		|
		22H :	_WTV ;
		|
		23H :	_WTR ;
		|
		24H :	_SXI ;
		|
		25H :	_SXGG ;
		|
		26H :	_SXKG ;
		|
		27H :	_SXN ;
		|
		28H :	_EMB ;
		|
		29H :	_E ;
		|
		2AH :	_ENZ ;
		|
		2BH :	_EZ ;
		|
		2CH :	_HXP ;
		|
		2DH :	_MH ;
		|
		2EH :	_HXX ;
		|
		2FH :	_VXX ;

		|
		30H :	_ZMC ;
		|
		31H :	_LMC ;
		|
		32H :	_LMT ;
		|
		33H :	_LC ;
		|
		34H :	_SM ;
		|
		35H :	_SMN ;
		|
		36H :	_S ;
		|
		37H :	_VSS ;
		|
		38H :	_SU ;
		|
		39H :	_TCB ;
		|
		3AH :	_SFB ;

		|
		3CH :	_HBC ;
		|
		3DH :	_MCFU ;
		|
		3EH :	_ZU ;
		|
		3FH :	_MABI ;

		|
		40H :	_AB ;
		|
		41H :	_SBB ;
		|
		42H :	_A ;
		|
		43H :	_AC ;
		|
		44H :	_SBI ;
		|
		45H :	_SBD ;
		|
		46H :	_SB ;
		|
		47H :	_SBC ;
		|
		48H :	_GSBI ;
		|
		49H :	_AU ;
		|
		4AH :	_GAC ;
		|
		4BH :	_GA ;
		|
		4CH :	_GSBD ;
		|
		4DH :	_SBU ;
		|
		4EH :	_GSBC ;
		|
		4FH :	_GSB ;

		|
		52H :	_GAB ;
		|
		53H :	_GSBB ;
		|
		54H :	_ML ;
		|
		55H :	_MLR ;
		|
		56H :	_MLA ;
		|
		57H :	_MAR ;
		|
		58H :	_MLN ;
		|
		59H :	_MNR ;
		|
		5AH :	_MAN ;
		|
		5BH :	_MANR ;
		|
		5CH :	_GMLN ;
		|
		5DH :	_GMAN ;
		|
		5EH :	_GML ;
		|
		5FH :	_GMLA ;

		|
		60H :	_DV ;
		|
		61H :	_DVD ;
		|
		62H :	_DVI ;
		|
		63H :	_VAQ ;
		|
		64H :	_GDV ;
		|
		65H :	_REZ ;
		|
		66H :	_GDVI ;
		|
		67H :	_B2VN ;
		|
		68H :	_VEL ;
		|
		69H :	_AUT ;
		|
		6AH :	_ET ;
		|
		6BH :	_ZUS ;
		|
		6CH :	_B3 ;
		|
		6DH :	_B3V ;
		|
		6EH :	_B2 ;
		|
		6FH :	_B2V ;

		|
		70H :	_B ;
		|
		71H :	_BD ;
		|
		72H :	_BQ ;
		|
		73H :	_BH ;
		|
		74H :	_BB ;
		|
		75H :	_BN ;
		|
		76H :	_BR ;
		|
		77H :	_BNR ;
		|
		78H :	_M2N ;
		|
		79H :	_M2NR ;
		|
		7AH :	_M2 ;
		|
		7BH :	_M2R ;
		|
		7CH :	_A2 ;
		|
		7DH :	_SB2 ;
		|
		7EH :	_AQ ;
		|
		7FH :	_SBQ ;

		|
		80H :
			_C ;
		|
		81H :	_CR ;
		|
		82H :	_CMT ;
		|
		83H :	_CMR ;
		|
		84H :	_CN ;
		|
		85H :	_CB ;
		|
		86H :	_CD ;
		|
		87H :	_CQ ;
		|
		88H :	_VLA ;
		|
		89H :	_ATA ;
		|
		8AH :	_ETA ;
		|
		8BH :	_LA ;
		|
		8CH :	_TXR ;
		|
		8DH :	IF AktAdressteil BAND 200H <> 0 THEN
				_MRX ;
			ELSE
				_RX ;
			END ;
		|
		8EH :	_BA ;
		|
		8FH :	_CH ;

		|
		90H :	_ST ;
		|
		91H :	_STN ;
		|
		92H :	_ZTR ;
		|
		93H :	_SEGG ;
		|
		94H :	_KDFR ;
		|
		95H :	_KFLD ;
		|
		96H :	_R ;
		|
		97H :	_RT ;
		|
		98H :	_AA ;
		|
		99H :	_SBA ;
		|
		9AH :	_LR ;
		|
		9BH :	_SH ;
		|
		9CH :	_TRX ;
		|
		9DH :	_HALT ;
		|
		9EH :	_KB ;
		|
		9FH :	_NRM ;

		|
		0A0H :	_C2 ;
		|
		0A1H :	_C3 ;
		|
		0A2H :	_CMC ;
		|
		0A3H :	_BC ;
		|
		0A4H :	_SI0 ;
		|
		0A5H :	_SKG0 ;
		|
		0A6H :	_SGG0 ;
		|
		0A7H :	_SN0 ;
		|
		0A8H :	_SAT ;
		|
		0A9H :	_SAA ;
		|
		0AAH :	_SK ;
		|
		0ABH :	_SG ;
		|
		0ACH :	_SI ;
		|
		0ADH :	_SN ;
		|
		0AEH :	_SKG ;
		|
		0AFH :	_SGG ;

		|
		0B0H :	_BL ;
		|
		0B1H :	_VPU ;
		|
		0B2H :	_SXR ;
		|
		0B3H :	_SXRN ;
		|
		0B4H :	_Y ;
		|
		0B5H :	_LEI ;
		|
		0B6H :	_BCI ;
		|
		0B7H :	_ZI ;
		|
		0B8H :	_SR ;
		|
		0B9H :	_PDP ;
		|
		0BAH :	_SRN ;
		|
		0BBH :	_SSR ;
		|
		0BCH :	_SE ;
		|
		0BDH :	_SUE ;
		|
		0BEH :	_BLEI ;
		|
		0BFH :	_VMO ;

		|
		0C8H :	_ZT0 ;
		|
		0C9H :	_ZT1 ;
		|
		0CAH :	_ZT2 ;
		|
		0CBH :	_ZT3 ;
		|
		0CCH :	_T ;
		|
		0CEH :	_SXG ;
		|
		0CFH :	_SXK ;

		|
		0D0H :	_CU ;
		|
		0D1H :	_BZN ;
		|
		0D2H :	_ZDP ;
		|
		0D3H :	_BU ;
		|
		0D4H :	_SG0 ;
		|
		0D5H :	_SK0 ;
		|
		0D8H :	_BZ2 ;
		|
		0D9H :	_BZ ;
		|
		0DAH :	_BQB ;
		|
		0DBH :	_CZ ;
		|
		0DCH :	_BAR ;
		|
		0DDH :	_BANR ;
		|
		0DFH :	_BAN ;

		|
		0E0H :	_RLR ;
		|
		0E1H :	_IR ;
		|
		0E3H :	_US ;
		|
		0E6H :	_BNZ ;
		|
		0E7H :	_CNZ ;
		|
		0EAH :	_TDM ;
		|
		0EBH :	_TLD ;
		|
		0ECH :	_TLI ;
		|
		0EDH :	_TLOG ;
		|
		0EEH :	_TMIN ;
		|
		0EFH :	_TMAX ;

		|
		0F0H :	_DA ;
		|
		0F1H :	_DSB ;
		|
		0F2H :	_DML ;
		|
		0F3H :	_MLD ;
		|
		0F4H :	_AT ;
		|
		0F5H :	_SBT ;
		|
		0F6H :	_BT ;
		|
		0F7H :	_CT ;
		|
		0F8H :	_WB ;
		|
		0F9H :	_SBIT ;
		|
		0FAH :	_SFBE ;
		|
		0FBH :	_BSS ;
		|
		0FCH :	_ZK ;
		|
		0FDH :	_TOK ;
		|
		0FEH :	_QBR ;
		|
		0FFH :	_QCR ;

		ELSE
			MakroAlarm := TRUE ;
			BefehlsAlarm ;
		END ;
	END ;

	GrossSeitenInvarianz := FALSE ;		(* nur einmal *)
	MUmodifizierung := FALSE ;		(* dto. *)

END internBefehlAusfuehren ;

END Befehle.
