/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Fonction fenetre de gestion des types Statiques			|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"
#include		<custcntl.h>
#include		<stdarg.h>

/*
|-----------------------------------------------------------------------|
| prototypes
|-----------------------------------------------------------------------|
*/

static	void	RedrawFafaStatic( HWND w ) ;
static	void	DrawFafaStatic( HWND w , HDC h) ;

static	void	DrawMeplat( HDC h, RECT * r ) ;
static	void	EmptyRectangle( HDC h, RECT * r ) ;

static	void DrawTaggedText( HDC h, RECT * r, int x, int y, LPSTR t ) ;
static	void DrawTaggedBitmap( HDC h, RECT * r, int x, int y, int n ) ;
static	void DrawTaggedBitmapLoaded( HDC h, RECT * r, int x, int y, HBITMAP b,int width,int height) ;
static	void DrawCadreRelief( HDC h, RECT * t, int y, LPSTR s, int ) ;

static	void	DrawTextOnly( HWND w, LPSTR s ) ;

/*
|-----------------------------------------------------------------------|
| proc LoopFafaStatic :
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI LoopFafaStatic( HWND w, UINT msg, WPARAM mw, LPARAM ml )
{
HDC	h ;

	switch( msg )
		{
		case WM_SYSCOLORCHANGE:
			CreatePensBrushes() ;
			break ;
		case WM_CREATE:
			SetFont(0) ;
			return(0) ;
		case WM_SETFONT:
		{
			HFONT f = (HFONT)mw ;
			SetFont(f) ;
		        h = GetDC( w ) ;
			DrawFafaStatic(w,h) ;
		        ReleaseDC( w, h ) ;
			return(0) ;
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
				style |= FS_BITMAP ;
			}
			else
			{
				/* Switch to Text button */
				style &= 0xfffffff0 ;
				style |= FS_TEXT ;
			}
			SetStyle(style) ;
		        h = GetDC( w ) ;
			DrawFafaStatic(w,h) ;
		        ReleaseDC( w, h ) ;
			return 0 ;
		}
		case WM_CHANGEICON:
		{
			HICON f = (HICON)ml ;
			LONG style = GetStyle() ;
			
			ffSetIcon(f) ;

			/* Switch to Bitmap button */
			style &= 0xfffffff0 ;
			style |= FS_ICON ;
			SetStyle(style) ;
		        h = GetDC( w ) ;
			DrawFafaStatic(w,h) ;
		        ReleaseDC( w, h ) ;
			return 0 ;
		}
		case WM_GETFONT:
			return (LRESULT)GetFont() ;
		case WM_PAINT :
			return RedrawFafaStatic( w ), 0l ;
		case WM_NCHITTEST :
			return HTTRANSPARENT ;
		case WM_WHOAREYOU :
			return ( LONG ) FB_STATIC ;
		case WM_ENABLE:
			if ( ( mw != 0 ) ^ IsDisabled() )
				InvalidateRect( w, 0, 0 ) ;
			return 0 ;

		case WM_SETTEXT:
			if ( BtStyle( GetStyle() ) >= FS_CADRE_DOWN )
				DrawTextOnly( w, ( LPSTR ) ml ) ;
			else	InvalidateRect( w, 0, 1 ) ;
		default :
			return DefWindowProc( w, msg, mw, ml ) ;
		}
 return 0;
}

/*
|-----------------------------------------------------------------------|
| proc RedrawFafaStatic :
|-----------------------------------------------------------------------|
*/

static	void	RedrawFafaStatic( HWND w )
{
	HDC		h ;
	PAINTSTRUCT	p ;

	h = BeginPaint( w, & p ) ;
	DrawFafaStatic(w,h) ;
	EndPaint( w, & p ) ;

}

static void DrawFafaStatic(HWND w, HDC h)
{
	RECT		r ;
	DWORD		v ;
	char		t[ CTLTITLE + 1 ] ;
	HFONT		fnt,was;
	HBITMAP		bmp ;
	WORD		width,height ;

	fnt = GetFont() ;
	GetClientRect( w, & r ) ;

	if (fnt)
		was = SelectObject(h,fnt) ;

	GetWindowText( w, t, sizeof t ) ;
	v = GetStyle() ;
	switch( BtStyle( v ) )
		{
		case FS_MEPLAT_TXT:
			DrawMeplat( h, & r ) ;
			goto text ;
		case FS_TEXT:
			if ( v & WS_BORDER )
				EmptyRectangle( h, & r ) ;
text:			DrawTaggedText( h, & r, ( int ) GetStX( v ),
						( int ) GetStY( v ), t ) ;
			break ;
		case FS_MEPLAT_BTM:
			DrawMeplat( h, & r ) ;
			DrawTaggedBitmap( h, & r, ( int ) GetStX( v ),
					( int ) GetStY( v ), MyAtoi( t ) ) ;
			break ;
		case FS_BITMAP:
			bmp = ffGetBitmap() ;
			width = GetBitmapW() ;
			height = GetBitmapH() ;
			if ( v & WS_BORDER )
				EmptyRectangle( h, & r ) ;
			if (bmp==0)
				DrawTaggedBitmap( h, & r, ( int ) GetStX( v ),
					( int ) GetStY( v ), MyAtoi( t ) ) ;
			else
				DrawTaggedBitmapLoaded( h, & r, 
							(int)GetStX(v),
							(int)GetStY(v),
							bmp,width,height ) ;
			break ;
		case FS_CADRE_DOWN :
		case FS_CADRE_UP :
			DrawCadreRelief( h, & r, ( int ) GetStY( v ), t,
				( BtStyle( v ) == FS_CADRE_UP ) ) ;
			goto text ;
		case FS_ICON:
		  {
		    RECT	rt ;
		    rt.left = ( int ) GetStX( v ) - 1;
		    rt.top = ( int ) GetStY( v ) - 1;
		    rt.right = rt.left + GetSystemMetrics(SM_CXICON);
		    rt.bottom = rt.top + GetSystemMetrics(SM_CYICON);
		    FillRect( h, & rt, brushFace ) ;
		    DrawIcon(h, rt.left, rt.top, ffGetIcon());
		  }
		  break;
		}

	if ( IsDisabled() )
		DrawButtonDisable( h, & r, 1 ) ;
	if (fnt)
		SelectObject(h,was) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawTaggedText :
|-----------------------------------------------------------------------|
*/

#define	MARGE	6

static	void DrawTaggedText( HDC h, RECT * r, int x, int y, LPSTR t )
{
COLORREF old ;

	if ( t[ 0 ] != '\0' )
		{
		RECT	rt ;

		rt.left = rt.right = rt.top = rt.bottom = 0 ;
		DrawText( h, t, -1, & rt, DT_CALCRECT ) ;

		switch( x )
			{
			case 0:	x = r->left + MARGE ;			break ;
			case 1:	x = ( r->right +r->left -rt.right )/2 ;	break ;
			case 2:	x = r->right - rt.right - MARGE ;
			case 3:	x = r->left ;
			}
		switch( y )
			{
			case 0:	y = MARGE ;				break ;
			case 1:	y = ( r->bottom - rt.bottom ) / 2 ;	break ;
			case 2:	y = r->bottom - rt.bottom - MARGE ;
			case 3:	y = 0 ;
			}

		OffsetRect( & rt, x, y ) ;
		FillRect( h, & rt, brushBack ) ;

		SetBkMode( h, TRANSPARENT ) ;
		old = SetTextColor(h,colorLabel) ;
		DrawText( h, t, -1, & rt, DT_NOCLIP | DT_CENTER ) ;
		SetTextColor(h,old) ;
		}
}

/*
|-----------------------------------------------------------------------|
| proc EmptyRectangle :
|-----------------------------------------------------------------------|
*/

static	void	EmptyRectangle( HDC h, RECT * r )
{
	HBRUSH	b ;
	HPEN	p ;

	b = SelectObject( h, brushBack ) ;
	p = SelectObject( h, staticBorder ) ;

	Rectangle( h, r->left, r->top, r->right, r->bottom ) ;

	SelectObject( h, b ) ;
	SelectObject( h, p ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawTaggedBitmapLoaded :
|-----------------------------------------------------------------------|
*/

static	void DrawTaggedBitmapLoaded( HDC h, RECT * r, int x, int y, HBITMAP b, int width,int height)
{
	HDC		hm ;

	hm = CreateCompatibleDC( h ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	if ( b )
		{
		switch( x )
			{
			case 0: x = r->left + MARGE ;
				break ;
			case 1:	x = ( r->left + r->right - width )/2 ;
				break ;
			case 2:	x = r->right - MARGE - width ;
			}
		switch( y )
			{
			case 0:	y = r->top + MARGE ;
				break ;
			case 1:	y = ( r->top + r->bottom - height )/2 ;
				break ;
			case 2:	y = r->bottom - height - MARGE ;
			}

		b = SelectObject( hm, b ) ;
		BitBlt( h, x, y, width, height, hm, 0, 0, SRCCOPY ) ;
		SelectObject( hm, b ) ;
		}
	DeleteDC( hm ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawTaggedBitmap :
|-----------------------------------------------------------------------|
*/

static	void DrawTaggedBitmap( HDC h, RECT * r, int x, int y, int n )
{
	HBITMAP		b ;
	BITMAP		o ;
	HDC		hm ;

	hm = CreateCompatibleDC( h ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	b = LoadOneBitmap( n, & o ) ;
	if ( b )
		{
		switch( x )
			{
			case 0: x = r->left + MARGE ;
				break ;
			case 1:	x = ( r->left + r->right - (int)o.bmWidth )/2 ;
				break ;
			case 2:	x = r->right - MARGE - ( int )o.bmWidth ;
			}
		switch( y )
			{
			case 0:	y = r->top + MARGE ;
				break ;
			case 1:	y = ( r->top + r->bottom - (int)o.bmHeight )/2 ;
				break ;
			case 2:	y = r->bottom - ( int ) o.bmHeight - MARGE ;
			}

		b = SelectObject( hm, b ) ;
		BitBlt( h, x, y, o.bmWidth, o.bmHeight, hm, 0, 0, SRCCOPY ) ;
		DeleteObject( SelectObject( hm, b ) ) ;
		}
	DeleteDC( hm ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawMeplat :
|-----------------------------------------------------------------------|
*/

static	void	DrawMeplat( HDC h, RECT * r )
{
HPEN	oldp ;
HBRUSH	oldb ;

	oldp = SelectObject( h, staticLight) ;
	oldb = SelectObject( h, brushBack ) ;
	Rectangle( h, r->left, r->top, r->right, r->bottom ) ;

	SelectObject( h, staticShadow ) ;
	MoveToEx( h, r->left, r->bottom , NULL ) ;
	LineTo( h, r->left, r->top ) ;
	LineTo( h, r->right, r->top ) ;
	SelectObject(h,oldp) ;
	SelectObject(h,oldb) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawCadreRelief :
|-----------------------------------------------------------------------|
*/

static	void DrawCadreRelief( HDC h, RECT * t, int y, LPSTR s, int up )
{
	RECT	rc ;
	int	dy ;
	HPEN	oldp ;
	HBRUSH	oldb ;

	if ( s[ 0 ] == '\0' )
		y = 1 ;

	if ( y != 1 )
		{
		rc.left = rc.right = rc.top = rc.bottom = 0 ;
		DrawText( h, s, -1, & rc, DT_CALCRECT ) ;
		dy = rc.bottom / 2 ;
		}

	rc.top    = rc.left = MARGE / 2 + 1 ;
	rc.right  = t->right - MARGE / 2 + 1 ;
	rc.bottom = t->bottom - MARGE / 2 + 1 ;

	if ( ! y )
		rc.top += dy + MARGE / 2 ;
	else if ( y == 2 )
		rc.bottom -= dy + MARGE / 2 ;

	oldp = SelectObject( h, ( up ? staticShadow : staticLight ) ) ;
	oldb = SelectObject( h, GetStockObject( NULL_BRUSH ) ) ;
	Rectangle( h, rc.left, rc.top, rc.right, rc.bottom ) ;

	SelectObject( h, ( up ? staticLight : staticShadow ) ) ;
	OffsetRect( & rc, -1, -1 ) ;
	Rectangle( h, rc.left, rc.top, rc.right, rc.bottom ) ;

	InflateRect( t, - MARGE / 2, 0 ) ;
	SelectObject(h,oldp) ;
	SelectObject(h,oldb) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawTextOnly :
|-----------------------------------------------------------------------|
*/

static	void	DrawTextOnly( HWND w, LPSTR s )
{
	RECT		r ;
	HDC		h = GetDC( w ) ;
	DWORD		v = GetStyle() ;

	GetClientRect( w, & r ) ;
	InflateRect( & r, - MARGE / 2, 0 ) ;

	DrawTaggedText( h, & r, ( int ) GetStX( v ), ( int ) GetStY( v ), s ) ;
	ReleaseDC( w, h ) ;
}

