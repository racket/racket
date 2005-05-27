/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Cont.c - Controles Windows 					|
|	Fonction fenetre de gestion des types Boutons Continus		|
|									|
|	Copyright (c)	FB			Boulogne		|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"

/*
|-----------------------------------------------------------------------|
| static	 :
|-----------------------------------------------------------------------|
*/

#define	LAPS		20		/* valeur temps timer */
#define	LAPS_FREE	8		/* nombre de laps free */

static	UINT	Htimer ;	/* Handle timer */
static	RECT	Rb ;		/* rectangle */
static	DWORD	LapsFree ;	/* temps d'attente avent serie */

/*
|-----------------------------------------------------------------------|
| prototypes :
|-----------------------------------------------------------------------|
*/

static	void	FafaContKey( HWND w, int down ) ;
static	void	FafaContMouse( HWND w, int down ) ;

static	void	SendTimer( HWND w, int button ) ;
static	void	ResetTimer( HWND w ) ;

void WINAPI ExecuteTimerBtn( HWND w, UINT m, UINT id, DWORD tt ) ;
void WINAPI ExecuteTimerSpace( HWND w, UINT m, UINT id, DWORD tt ) ;

/*
|-----------------------------------------------------------------------|
| proc LoopFafaCont :
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI LoopFafaCont( HWND w, UINT msg, WPARAM mw, LPARAM ml )
{
	switch( msg )
		{
		case WM_CREATE :
			InitButtonWords( w ) ;
			break ;

		case WM_SETFONT:
		{
			HFONT f = (HFONT)mw ;
			SetFont(f) ;
			break ;
		}
		case WM_GETFONT:
			return (LRESULT)GetFont() ;
		case WM_PAINT :
			RedrawFafaButton( w ) ;
			break ;

		case WM_ENABLE :
			if ( ( mw != 0 ) ^ IsDisabled() )
				InvalidateRect( w, 0, 0 ) ;
			break ;

		case WM_SETFOCUS :
			ChangeButtonFocus( w, 1 ) ;
			if ( GetAcc() && KeyIsDown( GetAcc() ) )
				FafaContKey( w, 1 ) ;
			break ;
		case WM_KILLFOCUS :
			ChangeButtonFocus( w, 0 ) ;
			break ;

		case WM_RBUTTONDBLCLK:
			if ( ! TstF( GetStyle(), FB_RIGHTBUT ) )
				break ;
		case WM_LBUTTONDBLCLK:
		{
		WORD	id ;
		HWND	hwnd ;
		WORD	cmd ;
		LPARAM	lparam ;
		WPARAM	wparam ;

			id = GetDlgCtrlID(w) ;
			hwnd = w ;
			cmd = BN_CLICKED ;
#ifdef WIN32
			lparam = (LPARAM)hwnd ;
			wparam = MAKEWPARAM(id,cmd) ;
#else
			wparam = (WPARAM)id ;
			lparam = MAKELPARAM(hwnd,cmd) ;
#endif
			if ( ! TstF( GetStyle(), FB_DBLCLKS ) )
				SendMessage( GetParent( w ), WM_COMMAND,
					     wparam , lparam) ;
			break ;
		}
		case WM_RBUTTONDOWN:
		case WM_RBUTTONUP:
			if ( ! TstF( GetStyle(), FB_RIGHTBUT ) )
				break ;

		case WM_LBUTTONDOWN:
		case WM_LBUTTONUP:
			FafaContMouse( w, ( msg == WM_LBUTTONDOWN ||
					  msg == WM_RBUTTONDOWN ) ) ;
			break ;

		case WM_KEYDOWN:
		case WM_SYSKEYDOWN:
		case WM_KEYUP:
		case WM_SYSKEYUP:
			if ( mw == ' ' || KeyIsAcc( w, mw ) )
				FafaContKey( w, IsMessDown( msg ) ) ;
			else	return DefWindowProc( w, msg, mw, ml ) ;
			break ;

		case WM_WHOAREYOU :
			return ( LONG ) MB_BUTTON ;

		case WM_SETTEXT :
			InvalidateRect( w, 0, 0 ) ;
		default:
			return DefWindowProc( w, msg, mw, ml ) ;
		}
	return 0l ;
}

/*
|-----------------------------------------------------------------------|
| proc FafaContMouse :
|-----------------------------------------------------------------------|
*/

static	void	FafaContMouse( HWND w, int down )
{
WORD	id ;
HWND	hwnd ;
WORD	cmd ;
LPARAM	lparam ;
WPARAM	wparam ;

	id = GetDlgCtrlID(w) ;
	hwnd = w ;
	cmd = BN_UNHILITE ;
#ifdef WIN32
	lparam = (LPARAM)hwnd ;
	wparam = MAKEWPARAM(id,cmd) ;
#else
	wparam = (WPARAM)id ;
	lparam = MAKELPARAM(hwnd,cmd) ;
#endif

	if ( ! TstState( FB_KEYDOWN ) )
		if ( down )
			{
			SetFocus( w ) ;
			SetState( (WORD)FB_CAPTURING ) ;
			SendTimer( w, 1 ) ;
			}
		else if ( TstState( FB_CAPTURING ) )
			{
			ResetTimer( w ) ;
			SendMessage( GetParent( w ), WM_COMMAND,
				     wparam , lparam ) ;
			}
}

/*
|-----------------------------------------------------------------------|
| proc FafaContKey:
|-----------------------------------------------------------------------|
*/

static	void	FafaContKey( HWND w, int down )
{
WORD	id ;
HWND	hwnd ;
WORD	cmd ;
LPARAM	lparam ;
WPARAM	wparam ;

	id = GetDlgCtrlID(w) ;
	hwnd = w ;
	cmd = BN_UNHILITE ;
#ifdef WIN32
	lparam = (LPARAM)hwnd ;
	wparam = MAKEWPARAM(id,cmd) ;
#else
	wparam = (WPARAM)id ;
	lparam = MAKELPARAM(hwnd,cmd) ;
#endif

	if ( ! TstState( FB_CAPTURING ) )
		if ( down && ! TstState( FB_KEYDOWN ) )
			{
			SetState( (WORD)FB_KEYDOWN ) ;
			SendTimer( w, 0 ) ;
			}
		else if ( ! down && TstState( FB_KEYDOWN ) )
			{
			ResetTimer( w ) ;
			SendMessage( GetParent( w ), WM_COMMAND,
				     wparam , lparam) ;
			}
}

/*
|-----------------------------------------------------------------------|
| proc SendTimer :
|-----------------------------------------------------------------------|
*/

static	void	SendTimer( HWND w, int button )
{
	SetCapture( w ) ;

	GetClientRect( w, & Rb ) ;
	ClientToScreen( w, ( POINT * ) & Rb.left  ) ;
	ClientToScreen( w, ( POINT * ) & Rb.right ) ;

	LapsFree = 0 ;
	if ( button )
		{
		ExecuteTimerBtn( w, 0, 0, 0 ) ;
		Htimer = SetTimer( w, 1, 20, ( TIMERPROC ) ExecuteTimerBtn ) ;
		}
	else	{
		SetState( (WORD)FB_SELECTED ) ;
		InvalidateRect( w, 0, 0 ) ;

		ExecuteTimerSpace( w, 0, 0, 0 ) ;
		Htimer = SetTimer( w, 1, 20, ( TIMERPROC ) ExecuteTimerSpace ) ;
		}
	LapsFree = LAPS_FREE ;
}

static	void	ResetTimer( HWND w )
{
	KillTimer( w, Htimer ) ;
	ReleaseCapture() ;

	if ( TstState( FB_SELECTED ) )
		InvalidateRect( w, 0, 0 ) ;

	ClrState( (WORD)(FB_SELECTED | FB_KEYDOWN | FB_CAPTURING) ) ;
}

void WINAPI ExecuteTimerBtn( HWND w, UINT m, UINT id, DWORD tt )
{
POINT	p ;
WORD	myid ;
HWND	hwnd ;
WORD	cmd ;
LPARAM	lparam ;
WPARAM	wparam ;

	myid = GetDlgCtrlID(w) ;
	hwnd = w ;
	cmd = BN_CLICKED ;
#ifdef WIN32
	lparam = (LPARAM)hwnd ;
	wparam = MAKEWPARAM(myid,cmd) ;
#else
	wparam = (WPARAM)myid ;
	lparam = MAKELPARAM(hwnd,cmd) ;
#endif

	if ( LapsFree )
		LapsFree -- ;

	GetCursorPos( & p ) ;

	if ( XyInRect( p.x, p.y, Rb ) )
		{
		if ( ! TstState( FB_SELECTED ) )
			{
			SetState( (WORD)FB_SELECTED ) ;
			InvalidateRect( w, 0, 0 ) ;
			UpdateWindow( w ) ;
			}
		if ( ! LapsFree )
			SendMessage( GetParent( w ), WM_COMMAND,
				     wparam , lparam) ;
		}
	else if ( TstState( FB_SELECTED ) )
		{
		ClrState( (WORD)FB_SELECTED ) ;
		InvalidateRect( w, 0, 0 ) ;
		UpdateWindow( w ) ;
		}
}

void WINAPI ExecuteTimerSpace( HWND w, UINT m, UINT id, DWORD tt )
{
WORD	myid ;
HWND	hwnd ;
WORD	cmd ;
LPARAM	lparam ;
WPARAM	wparam ;

	myid = GetDlgCtrlID(w) ;
	hwnd = w ;
	cmd = BN_CLICKED ;
#ifdef WIN32
	lparam = (LPARAM)hwnd ;
	wparam = MAKEWPARAM(myid,cmd) ;
#else
	wparam = (WPARAM)myid ;
	lparam = MAKELPARAM(hwnd,cmd) ;
#endif

	if ( LapsFree )
		LapsFree -- ;
	if ( ! LapsFree )
		SendMessage( GetParent( w ), WM_COMMAND,
			     wparam , lparam ) ;
}


