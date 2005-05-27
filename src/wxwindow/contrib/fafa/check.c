/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Fonction fenetre de gestion des types Check Boutons		|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include "fafapriv.h"

#include "stdio.h"
#include "stdarg.h"
#include "string.h"

#ifdef DEBUG_FAFA
void MyDebugMsg(char *fmt , ...)
{
  va_list ap;
  static char buffer[512];

  va_start(ap, fmt);

  wvsprintf(buffer,fmt,ap) ;
  OutputDebugString(buffer) ;

  va_end(ap);
}
#endif

/*
|-----------------------------------------------------------------------|
| defines	 :
|-----------------------------------------------------------------------|
*/

#define	OFFSET_TEXT	20
#define	MARGE		2
#define	SIZE_BMP	14

/*
|-----------------------------------------------------------------------|
| prototypes								|
|-----------------------------------------------------------------------|
*/

static	void	ChangeCheckFocus( HWND w, int on ) ;
static	int	ChangeCheckSelect( HWND w, int s ) ;

static	void	RedrawFafaCheck( HWND w ) ;
static	void	DrawFafaCheck( HWND w , HDC h) ;
static	void	DrawCheck( HDC h, RECT * r, int n, LPSTR s ) ;
static	void	DrawCheckFocus( HDC h, RECT * r, LPSTR s ) ;
static	void	CheckKeyDown( HWND w, int down ) ;

static	void	MouseDown( HWND w ) ;
static	void	MouseMove( HWND w, DWORD l ) ;
static	void	MouseUp( HWND w, DWORD l ) ;

static	void	MakeRadioIteration( HWND w1 ) ;

/*
|-----------------------------------------------------------------------|
| proc LoopFafaCheck :
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI LoopFafaCheck( HWND w, UINT msg, WPARAM mw, LPARAM ml )
{
HDC	h ;

	switch( msg )
		{
		case WM_SYSCOLORCHANGE:
			CreatePensBrushes() ;
			break ;
		case WM_CREATE :
			InitButtonWords( w ) ;
			if ( TstF( GetStyle(), FC_PRESELECT ) )
				SetState( (WORD)FB_SELECTED ) ;
			break ;

		case WM_SETFONT:
		{
			HFONT f = (HFONT)mw ;
			SetFont(f) ;
		        h = GetDC( w ) ;
			if ( TstF( GetStyle(), FC_BUTTONDRAW ) )
				DrawFafaButton( w ,h ) ;
			else	DrawFafaCheck( w ,h ) ;
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
				style &= 0xffffff70 ;
				style |= (FC_BUTTONDRAW|FB_BITMAP) ;
			}
			else
			{
				/* Switch to Text button */
				style &= 0xffffff70 ;
				/* default text style for check items */
				if (style&FC_RADIO)
					style |= FC_CIR_DWN ;
				else
					style |= FC_REC_DWN ;
			}
			SetStyle(style) ;
		        h = GetDC( w ) ;
			if ( TstF( GetStyle(), FC_BUTTONDRAW ) )
				DrawFafaButton( w ,h) ;
			else	DrawFafaCheck( w ,h) ;
		        ReleaseDC( w, h ) ;
			break ;
		}
		case WM_GETFONT:
			return (LRESULT)GetFont() ;
		case WM_PAINT :
			if ( TstF( GetStyle(), FC_BUTTONDRAW ) )
				RedrawFafaButton( w ) ;
			else	RedrawFafaCheck( w ) ;
			break ;

		case WM_SETFOCUS:
			ChangeCheckFocus( w, 1 ) ;
			if ( GetAcc() && KeyIsDown( GetAcc() ) )
				CheckKeyDown( w, 1 ) ;
			break ;
		case WM_KILLFOCUS:
			ChangeCheckFocus( w, 0 ) ;
			break ;

		case WM_ENABLE:
			if ( ( mw != 0 ) ^ IsDisabled() )
				InvalidateRect( w, 0, 0 ) ;
			break ;

		case WM_RBUTTONDOWN:
			if ( ! TstF( GetStyle(), FC_RIGHTBUT ) )
				break ;

		case WM_LBUTTONDOWN:
			SetFocus( w ) ;
			MouseDown( w ) ;
			break ;

		case WM_MOUSEMOVE:
			MouseMove( w, ml ) ;
			break ;

		case WM_RBUTTONUP:
			if ( ! TstF( GetStyle(), FC_RIGHTBUT ) )
				break ;

		case WM_LBUTTONUP:
			MouseUp( w, ml ) ;
			break ;

		case FAFA_GETCHECK :
#ifdef DEBUG_FAFA
MyDebugMsg("State of check %x is %d\n",w,TstState(FB_SELECTED));
#endif
			return ( LONG ) TstState( FB_SELECTED ) ;

		case FAFA_SETCHECK :
#ifdef DEBUG_FAFA
MyDebugMsg("Setting Check %x to %d\n",w,mw) ;
#endif
			if ( ChangeCheckSelect( w, ( mw != 0 ) ) && mw )
				MakeRadioIteration( w ) ;
			break ;

		case WM_KEYDOWN:
		case WM_SYSKEYDOWN:
		case WM_KEYUP:
		case WM_SYSKEYUP:
			if ( mw == ' ' || KeyIsAcc( w, mw ) )
				CheckKeyDown( w, IsMessDown( msg ) ) ;
			else	return DefWindowProc( w, msg, mw, ml ) ;
			break;

		case WM_WHOAREYOU :
			return ( LONG ) FB_CHECK ;

		case WM_SETTEXT :
			InvalidateRect( w, 0, 0 ) ;
		default:
			return DefWindowProc( w, msg, mw, ml ) ;
		}

	return 0l ;
}

/*
|-----------------------------------------------------------------------|
| proc RedrawFafaCheck :
|-----------------------------------------------------------------------|
*/

#define	BTM_OFF	9000

static	void	RedrawFafaCheck( HWND w )
{
	HDC		h ;
	PAINTSTRUCT	p ;

	h = BeginPaint( w, & p ) ;

	DrawFafaCheck(w,h) ;

	EndPaint(w,&p) ;

}

static void DrawFafaCheck(HWND w , HDC h) 
{
	RECT		r ;
	UINT		sel ;
	char		s[ CTLTITLE + 1 ] ;
	HFONT		fnt,was;

	fnt = GetFont() ;
	GetClientRect( w, & r ) ;
	GetWindowText( w, s, sizeof s ) ;
	sel = TstState( FB_SELECTED ) ;

	if (fnt)
		was = SelectObject(h,fnt) ;

	DrawCheck( h, & r, BTM_OFF + 2 * BtStyle( GetStyle() ) + sel, s ) ;

	if ( IsDisabled() )
		DrawButtonDisable( h, & r, 0 ) ;
	else if ( TstState( FB_FOCUS ) )
		DrawCheckFocus( h, & r, s ) ;

	if (fnt)
		SelectObject(h,was) ;
}

static	void	DrawCheckFocus( HDC h, RECT * r, LPSTR s )
{
	RECT	rt ;

	rt.left = rt.right = rt.top = rt.bottom = 0 ;
	DrawText( h, s, -1, & rt, DT_CALCRECT ) ;

	OffsetRect( & rt, OFFSET_TEXT, ( r->bottom - rt.bottom ) / 2 ) ;
	InflateRect( & rt, MARGE, MARGE ) ;
	DrawFocusRect( h, & rt ) ;
}

/*
|-----------------------------------------------------------------------|
| proc ChangeCheckFocus :	
|-----------------------------------------------------------------------|
*/

static	void	ChangeCheckFocus( HWND w, int on )
{
	if ( ( on != 0 ) ^ TstF( ffGetState(), FB_FOCUS ) )
		{
		HDC	h ;
		RECT	r ;
		char	s[ CTLTITLE + 1 ] ;

		h = GetDC( w ) ;
		GetClientRect( w, & r ) ;
		GetWindowText( w, s, sizeof s ) ;

		if ( TstF( GetStyle(), FC_BUTTONDRAW ) )
			DrawButtonFocus( h, & r, TstState( FB_SELECTED ) ) ;
		else	DrawCheckFocus( h, & r, s ) ;

		ReleaseDC( w, h ) ;
		XorState( (WORD)FB_FOCUS ) ;
		}
}

/*
|-----------------------------------------------------------------------|
| proc ChangeCheckSelect :
|-----------------------------------------------------------------------|
*/

static	int	ChangeCheckSelect( HWND w, int s )
{
	if ( s ^ TstState( FB_SELECTED ) )
		{
		XorState( (WORD)FB_SELECTED ) ;
		InvalidateRect( w, 0, 1 ) ;
		return 1 ;
		}
	return 0 ;
}

/*
|-----------------------------------------------------------------------|
| proc MakeRadioIteration :						|
|-----------------------------------------------------------------------|
*/

static	void	MakeRadioIteration( HWND w1 )
{
char	className[32] ;
int	count ;
HWND	w = w1 ;

	if ( ! TstF( GetStyle(), FC_RADIO ) )
		return ;

	w = GetWindow( w1, GW_HWNDNEXT ) ;
	while ( w )
	{
		count = GetClassName(w,className,31) ;
		if (strncmp(className,FafaChck,count))
			break ; /* not a fafa item */
                if (!TstF( GetStyle(), FC_RADIO )  ||
                     TstF (GetStyle() , WS_GROUP)
		   )
	      		break ; /* Not a radio item, or first of other radio */
		ChangeCheckSelect( w, 0 ) ;
		w = GetWindow( w, GW_HWNDNEXT ) ;
	}

	w = w1 ;
	if (!TstF( GetStyle(), WS_GROUP))
	{
		/* first item of radio not yet reached */
		w = GetWindow( w1, GW_HWNDPREV ) ;
		while ( w )
		{
			count = GetClassName(w,className,31) ;
			if (strncmp(className,FafaChck,count))
				break ; /* not a fafa item */
			if (!TstF( GetStyle(), FC_RADIO ))
				break ; /* not a radio item */
			ChangeCheckSelect( w, 0 ) ;
			if (TstF( GetStyle(), WS_GROUP))
				break ; /* first reached */
			w = GetWindow( w, GW_HWNDPREV ) ;
		}
	}
}

/*
|-----------------------------------------------------------------------|
| proc CheckKeyDown :
|-----------------------------------------------------------------------|
*/

static	void	CheckKeyDown( HWND w, int down )
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

	if ( ! TstState( FB_CAPTURING ) )
		if ( down && ! TstState( FB_KEYDOWN ) )
			{
			if ( ! TstState( FB_SELECTED ) ||
				! TstF( GetStyle(), FC_RADIO ) )
					{
					XorState( (WORD)FB_SELECTED ) ;
					SetState( (WORD)FB_KEYDOWN ) ;
					InvalidateRect( w, 0, 1 ) ;
					}
			}
		else if ( ( ! down ) && TstState( FB_KEYDOWN ) )
			{
			ClrState( (WORD)FB_KEYDOWN ) ;
			MakeRadioIteration( w ) ;
			SendMessage( GetParent( w ), WM_COMMAND,
				     wparam , lparam) ;
			}
}

/*
|-----------------------------------------------------------------------|
| proc DrawCheck :
|-----------------------------------------------------------------------|
*/

extern HBRUSH		brushBack ;		/* Brosse gris clair */

static	void	DrawCheck( HDC h, RECT * r, int n, LPSTR s )
{
	HBITMAP		b ;
	BITMAP		o ;
	HDC		hm ;
	int		y ;
	COLORREF	old ;

	hm = CreateCompatibleDC( h ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	b = LoadOneBitmap( n, & o ) ;
	if ( b )
		{
		b = SelectObject( hm, b ) ;
		y = ( r->top + r->bottom ) / 2 ;

		BitBlt( h, 0, y - o.bmHeight / 2, o.bmWidth, o.bmHeight,
							hm, 0, 0, SRCCOPY ) ;
		DeleteObject( SelectObject( hm, b ) ) ;

		if ( s[ 0 ] != '\0' )
			{
			RECT	rt ;

			rt.left = rt.right = rt.top = rt.bottom = 0 ;
			DrawText( h, s, -1, & rt, DT_CALCRECT ) ;
			OffsetRect( & rt, OFFSET_TEXT, y - rt.bottom / 2 ) ;

		        FillRect( h, & rt, brushBack ) ;
			SetBkMode( h, TRANSPARENT ) ;
			old = SetTextColor(h,colorText) ;
			DrawText( h, s, -1, & rt, DT_NOCLIP | DT_CENTER ) ;
			SetTextColor(h,old) ;
			}
		}
	DeleteDC( hm ) ;
}

/*
|-----------------------------------------------------------------------|
| proc Mouse controls :
|-----------------------------------------------------------------------|
*/

static	void	MouseDown( HWND w )
{
	if ( ! TstState( FB_KEYDOWN ) &&
	   ( ! TstState( FB_SELECTED ) || ! TstF( GetStyle(), FC_RADIO ) ) )
		{
		SetCapture( w ) ;
		XorState( (WORD)FB_SELECTED ) ;
		SetState( (WORD)(FB_CAPTURING | FB_TO_SET) ) ;
		InvalidateRect( w, 0, 1 ) ;
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
			if ( ! TstState( FB_TO_SET ) )
				{
				SetState( (WORD)FB_TO_SET ) ;
				XorState( (WORD)FB_SELECTED ) ;
				InvalidateRect( w, 0, 1 ) ;
				}
			}
		else if ( TstState( FB_TO_SET ) )
			{
			ClrState( (WORD)FB_TO_SET ) ;
			XorState( (WORD)FB_SELECTED ) ;
			InvalidateRect( w, 0, 1 ) ;
			}
		}
}

static	void	MouseUp( HWND w, DWORD l )
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

		MouseMove( w, l ) ;

		ReleaseCapture() ;
		ClrState( FB_TO_SET | FB_CAPTURING ) ;

		if ( TstState( (WORD)FB_SELECTED ) )
			MakeRadioIteration( w ) ;
		SendMessage( GetParent( w ), WM_COMMAND,
			wparam , lparam) ;
		}
}

