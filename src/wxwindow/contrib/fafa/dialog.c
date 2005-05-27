/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Fonction fenetre de gestion des dialogues			|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"

/*
|-----------------------------------------------------------------------|
| proc LoopFafaDialog :
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI LoopFafaDialog( HWND w, UINT msg, WPARAM mw, LPARAM ml )
{
	switch(msg)
	{
#ifdef WIN32
	case WM_CTLCOLORDLG:
	{
	HDC pDC = (HDC)mw;
		SetBkColor(pDC, GetSysColor(COLOR_WINDOW)) ;
		return (LRESULT)brushBack ;
	}
	break ;
#else
	case WM_CTLCOLOR:
	{
	int nCtlColor = (int)HIWORD(ml);
		if (nCtlColor==CTLCOLOR_DLG)
		{
		HDC pDC = (HDC)mw;
			SetBkColor(pDC, GetSysColor(COLOR_WINDOW)) ;
			return (LRESULT)brushBack ;
		}
	}
	break ;
#endif
	case WM_SYSCOLORCHANGE:
	{
		CreatePensBrushes() ;
#ifdef WIN32
		SetClassLong(w,GCL_HBRBACKGROUND,(LONG)brushBack) ;
#else
		SetClassWord(w,GCW_HBRBACKGROUND,(WORD)brushBack) ;
#endif
		return 0 ;
	}
	break ;
	case WM_ERASEBKGND :
		return DefWindowProc( w, msg, mw, ml ) ;
	break ;
	default:
		return DefDlgProc( w, msg, mw, ml ) ;
	break ;
	}
  return 0;
}

