/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Fonction fenetre de gestion des types Boutons			|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"

/*
|-----------------------------------------------------------------------|
| define	 :
|-----------------------------------------------------------------------|
*/

#define	MARGE_FOCUS	-6

/*
|-----------------------------------------------------------------------|
| prototypes :
|-----------------------------------------------------------------------|
*/

static	LPSTR	GetBtmTxtParams( LPSTR t, int * b ) ;

static	void	ButtonKeyDown( HWND w, int down ) ;
static	void	MakeAccDown( HWND w ) ;

static	void	MouseDown( HWND w ) ;
static	void	MouseMove( HWND w, DWORD l ) ;
static	void	MouseUp( HWND w, DWORD l, int but ) ;

/*
|-----------------------------------------------------------------------|
| proc LoopFafaButton :
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI	LoopFafaButton( HWND w, UINT msg, WPARAM mw, LPARAM ml )
{
HDC	h ;

	switch( msg )
		{
		case WM_SYSCOLORCHANGE:
			CreatePensBrushes() ;
			break ;
		case WM_CREATE :
			InitButtonWords( w ) ;
			break ;

		case WM_SETFONT:
		{
			HFONT f = (HFONT)mw ;
			SetFont(f) ;
		        h = GetDC( w ) ;
			DrawFafaButton(w,h) ;
		        ReleaseDC( w, h ) ;
			break ;
		}
		case WM_CHANGEBITMAP:
		{
			HBITMAP f = (HBITMAP)ml ;
			WORD width = LOBYTE(mw) ;
			WORD height = HIBYTE(mw) ;
			LONG style = GetStyle() ;

			if (width == 255) {
		           SIZE s;
			   GetBitmapDimensionEx(f, &s);
			   width = s.cx;
			   height = s.cy;
			}
			
			SetBitmap(f) ;
			SetBitmapW(width) ;
			SetBitmapH(height) ;

			if (f)
			{
				/* Switch to Bitmap button */
				style &= 0xfffffff0 ;
				style |= FB_BITMAP ;
			}
			else
			{
				/* Switch to Text button */
				style &= 0xfffffff0 ;
				style |= FB_TEXT ;
			}
			SetStyle(style) ;
		        h = GetDC( w ) ;
			DrawFafaButton(w,h) ;
		        ReleaseDC( w, h ) ;
			break ;
		}
		case WM_GETFONT:
			return (LRESULT)GetFont() ;
		case WM_PAINT :
			RedrawFafaButton( w ) ;
			break ;

		case WM_ENABLE:
			if ( ( mw != 0 ) ^ IsDisabled() )
				InvalidateRect( w, 0, 0 ) ;
			break ;

		case WM_SETFOCUS:
			ChangeButtonFocus( w, 1 ) ;
			if ( GetAcc() && KeyIsDown( GetAcc() ) )
				MakeAccDown( w ) ;
			break ;

		case WM_KILLFOCUS:
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
					     wparam,lparam) ;
			break ;
		}
		case WM_RBUTTONDOWN:
			if ( ! TstF( GetStyle(), FB_RIGHTBUT ) )
				break ;

		case WM_LBUTTONDOWN:
			SetFocus( w ) ;
			MouseDown( w ) ;
			break ;

		case WM_MOUSEMOVE:
			MouseMove( w, ml ) ;
			break ;

		case WM_RBUTTONUP:
			if ( ! TstF( GetStyle(), FB_RIGHTBUT ) )
				break ;

		case WM_LBUTTONUP:
			MouseUp( w, ml, ( msg == WM_RBUTTONUP ) ) ;
			break ;

		case WM_KEYDOWN:
		case WM_SYSKEYDOWN:
			if ( mw == ' ' )
				ButtonKeyDown( w, 1 ) ;
			else if ( KeyIsAcc( w, mw ) )
				MakeAccDown( w ) ;
			else	return DefWindowProc( w, msg, mw, ml ) ;
			break ;

		case WM_KEYUP:
		case WM_SYSKEYUP:
			if ( mw == ' ' )
				ButtonKeyDown( w, 0 ) ;
			else	return DefWindowProc( w, msg, mw, ml ) ;
			break;

		case WM_ERASEBKGND :
			break ;

		case WM_WHOAREYOU :
			return ( LONG ) FB_BUTTON ;

		case WM_SETTEXT :
			InvalidateRect( w, 0, 0 ) ;
		default:
			return DefWindowProc( w, msg, mw, ml ) ;
		}
	return 0 ;
}

/*
|-----------------------------------------------------------------------|
| proc initButtonWords : initialise state et Acc			|
|-----------------------------------------------------------------------|
*/

void	InitButtonWords( HWND w )
{
	char	tmp[ CTLTITLE + 1 ] ;
	int	i, l ;

	ClrState( 0xff ) ;
	SetAcc( 0 ) ;
	SetFont(0) ;
	SetBitmap(0) ;
	SetBitmapW(0) ;
	SetBitmapH(0) ;
	l = GetWindowText( w, tmp, sizeof tmp ) ;

	for ( i = 0 ; i < l ; i ++ )
		if ( tmp[ i ] == '&' )
			{
#ifdef WIN32
			SetAcc( VkKeyScan( ( TCHAR ) tmp[ i + 1 ] ) & 0xff ) ;
#else
			SetAcc( VkKeyScan( ( UINT ) tmp[ i + 1 ] ) & 0xff ) ;
#endif
			return ;
			}
}

/*
|-----------------------------------------------------------------------|
| proc RedrawFafaButton :
|-----------------------------------------------------------------------|
*/

void	RedrawFafaButton( HWND w )
{
	HDC		h ;
	PAINTSTRUCT	p ;

	h = BeginPaint( w, & p ) ;
	DrawFafaButton(w,h) ;
	EndPaint( w, & p ) ;

}

void DrawFafaButton(HWND w, HDC h)
{
	RECT		r ;
	int		s, v ;
	char		tmp[ CTLTITLE + 1 ] ;
	HFONT		fnt,was;
	HBITMAP		bmp ;
	WORD		width,height ;

	fnt = GetFont() ;
	GetClientRect( w, & r ) ;
	GetWindowText( w, tmp, sizeof tmp ) ;

	if (fnt)
		was = SelectObject(h,fnt) ;

	s = TstState( FB_SELECTED ) ;
	v = BtStyle( GetStyle() ) ;
	
	switch( v )
		{
		case FB_TEXT:
			DrawPodium( h, & r, s ) ;
			DrawString( h, & r, tmp, s ) ;
			break ;
		case FB_BITMAP:
			bmp = ffGetBitmap() ;
			width = GetBitmapW() ;
			height = GetBitmapH() ;
			DrawPodium( h, & r, s ) ;
			if (bmp==0)
				DrawBitmap( h, & r, MyAtoi( tmp ), s ) ;
			else
				DrawBitmapLoaded( h, & r, bmp,width,height, s ) ;
			break ;
		case FB_BTMTXT_H:
		case FB_BTMTXT_V:
			{
			char *	t ;
			int		b ;

			t = GetBtmTxtParams( tmp, & b ) ;
			DrawBtmTxtHV( h, &r, b, t, s, ( v == FB_BTMTXT_V ) ) ;
			}
			break ;
		case FB_MAC :
			DrawMac( h, & r, tmp, s ) ;
			break ;
		case FB_DBLBTM:
			DrawPodium( h, & r, s ) ;
			DrawBitmap( h, & r, MyAtoi( tmp ) + s, s ) ;
		}

	if ( IsDisabled() )
		DrawButtonDisable( h, & r, 1 ) ;
	else if ( TstState( FB_FOCUS ) )
		DrawButtonFocus( h, & r, s ) ;
	if (fnt)
		SelectObject(h,was) ;
}

/*
|-----------------------------------------------------------------------|
| proc ChangeButtonFocus :	
|-----------------------------------------------------------------------|
*/

void	ChangeButtonFocus( HWND w, int on )
{
	if ( ( on != 0 ) ^ TstF( ffGetState(), FB_FOCUS ) )
		{
		HDC	h ;
		RECT	r ;

		if ( ! on )
			{
			int	s = TstState( FB_SELECTED ) ;

			ClrState(FB_SELECTED|FB_CAPTURING|FB_KEYDOWN|FB_FOCUS) ;
			if ( s )
				{
				InvalidateRect( w, 0, 0 ) ;
				return ;
				}
			}
		else	SetState( FB_FOCUS ) ;

		h = GetDC( w ) ;
		GetClientRect( w, & r ) ;

		DrawButtonFocus( h, & r, 0 ) ;
		ReleaseDC( w, h ) ;
		}
}

/*
|-----------------------------------------------------------------------|
| proc GetBtmTxtParams :
|-----------------------------------------------------------------------|
*/

static	LPSTR	GetBtmTxtParams( LPSTR t, int * b )
{
	* b = MyAtoi( t ) ;
	while ( * t != ';' && * t != '\0' )
		t ++ ;
	return ( * t == '\0' ) ? t : ++ t ;
}

/*
|-----------------------------------------------------------------------|
| proc MyAtoi :
|-----------------------------------------------------------------------|
*/

UINT	MyAtoi( LPSTR s )
{
	UINT	r = 0u ;

	while ( * s >= '0' && * s <= '9' )
		r = r * 10u + ( UINT )( * s ++ ) - '0' ;
	return r ;
}

/*
|-----------------------------------------------------------------------|
| proc ButtonKeyDown : 							|
|-----------------------------------------------------------------------|
*/

static	void	ButtonKeyDown( HWND w, int down )
{
	if ( ! TstState( FB_CAPTURING ) )
		if ( down && ! TstState( FB_KEYDOWN ) )
			{
			SetCapture( w ) ;
			SetState( FB_SELECTED | FB_KEYDOWN ) ;
			InvalidateRect( w, 0, 1 ) ;
			}
		else if ( ( ! down ) && TstState( FB_KEYDOWN ) )
			{
			WORD	id ;
			HWND	hwnd ;
			WORD	cmd ;
			LPARAM	lparam ;
			WPARAM	wparam ;

			ReleaseCapture() ;
			ClrState( FB_SELECTED | FB_KEYDOWN ) ;
			InvalidateRect( w, 0, 1 ) ;

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
			SendMessage( GetParent( w ), WM_COMMAND,
				     wparam,lparam) ;
			}
}

/*
|-----------------------------------------------------------------------|
| proc MakeAccDown :
|-----------------------------------------------------------------------|
*/

static	void	MakeAccDown( HWND w )
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

	SetState( FB_SELECTED ) ;
	InvalidateRect( w, 0, 0 ) ;

	SendMessage( GetParent( w ), WM_COMMAND, wparam,lparam) ;

	ClrState( FB_SELECTED ) ;
	InvalidateRect( w, 0, 0 ) ;
}

/*
|-----------------------------------------------------------------------|
| proc Mouse controls :
|-----------------------------------------------------------------------|
*/

static	void	MouseDown( HWND w )
{
	if ( ! TstState( FB_KEYDOWN ) )
		{
		SetCapture( w ) ;
		SetState( FB_CAPTURING | FB_SELECTED ) ;
		InvalidateRect( w, 0, 0 ) ;
		}
}

static	void	MouseMove( HWND w, DWORD l )
{
	if ( TstState( FB_CAPTURING ) )
		{
		RECT	r ;

		GetClientRect( w, & r ) ;

		if ( XyInRect( LOWORD( l ), HIWORD( l ), r ) )
			{
			if ( ! TstState( FB_SELECTED ) )
				{
				SetState( FB_SELECTED ) ;
				InvalidateRect( w, 0, 0 ) ;
				}
			}
		else if ( TstState( FB_SELECTED ) )
			{
			ClrState( FB_SELECTED ) ;
			InvalidateRect( w, 0, 0 ) ;
			}
		}
}

static	void	MouseUp( HWND w, DWORD l, int but )
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

	if ( TstState( FB_CAPTURING ) )
		{
		RECT	r ;

		ReleaseCapture() ;
		ClrState( FB_CAPTURING ) ;

		if ( TstState( FB_SELECTED ) )
			{
			ClrState( FB_SELECTED ) ;
			InvalidateRect( w, 0, 0 ) ;
			}

		GetClientRect( w, & r ) ;
		if ( XyInRect( LOWORD( l ), HIWORD( l ), r ) )
			SendMessage(
				GetParent( w ),
				WM_COMMAND,
				wparam,
				lparam) ;
		}
}

