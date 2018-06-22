
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE upTextFrage;

(*	20.06.18	*)

FROM SYSTEM IMPORT
	UNREFERENCED_PARAMETER ;

FROM WINUSER IMPORT
	DialogBoxParam,
	EndDialog,
	GetDlgItemText,
	SetDlgItemText,
	WM_INITDIALOG,
	WM_COMMAND,
	IDOK, IDCANCEL ;

IMPORT WIN32 ;
IMPORT WINX ;

FROM TR440res IMPORT
	TEXTBEZ,
	TEXT ;


VAR
	gelesen :	ARRAY [0..63] OF CHAR ;



PROCEDURE TextHolen (hWnd : WIN32.HWND; VAR txt : ARRAY OF CHAR) : BOOLEAN ;
	VAR	x :	INTEGER ;
BEGIN
	gelesen := txt ;
	x := DialogBoxParam (WINX.Instance, 'TEXTFRAGE', hWnd, dialog, 87) ;
	IF x < 1 THEN
		x := WIN32.GetLastError () ;
		RETURN FALSE ;
	END ;
	txt := gelesen ;
	RETURN TRUE ;
END TextHolen ;


PROCEDURE dialog (hWnd : WIN32.HWND; uMSG : WIN32.UINT; wParam : WIN32.WPARAM; lParam : WIN32.LPARAM) : %IF ADW %THEN WIN32.INT_PTR %ELSE WIN32.BOOL %END
													[EXPORT, %IF ADW %THEN WINDOWS %ELSE WIN32SYSTEM %END] ;
BEGIN
	UNREFERENCED_PARAMETER(lParam);
	CASE uMSG OF
		  WM_INITDIALOG :
		  	IF SetDlgItemText (hWnd, TEXTBEZ, Bezeichnung)
		  	AND SetDlgItemText (hWnd, TEXT, gelesen) THEN
				%IF ADW %THEN RETURN 1 %ELSE RETURN TRUE ; %END
			END ;
		| WM_COMMAND :
			CASE wParam MOD 10000H OF
				  IDOK :	IF GetDlgItemText (hWnd, TEXT, gelesen, SIZE(gelesen)) = 0 THEN
				  			gelesen := '' ;
						END ;
						IF EndDialog (hWnd, wParam MOD 10000H) THEN
							%IF ADW %THEN RETURN 1 %ELSE RETURN TRUE ; %END
						END ;
				| IDCANCEL :	IF EndDialog (hWnd, wParam MOD 10000H) THEN
							%IF ADW %THEN RETURN 1 %ELSE RETURN TRUE ; %END
						END ;
				ELSE
				END ;
	ELSE
	END ;
	%IF ADW %THEN RETURN 0 %ELSE RETURN FALSE ; %END
END dialog ;


END upTextFrage.
