/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Utilitaires generaux de dessin.					|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"

/*
|-----------------------------------------------------------------------|
| proc define	 :
|-----------------------------------------------------------------------|
*/

#define	MARGE_FOCUS		2		/* marge cadre focus */
#define	DEFAULT_BTM		8200		/* bitmap par defaut */

HBITMAP		DisableBitmap ;		/* bitmap pour controles disables */

/*
|-----------------------------------------------------------------------|
| proc DrawButtonFocus :
|-----------------------------------------------------------------------|
*/

void	DrawButtonFocus( HDC h, RECT * r, int sel )
{
	InflateRect( r, - MARGE_FOCUS, - MARGE_FOCUS ) ;
	if ( sel )
		OffsetRect( r, 1, 1 ) ;
	DrawFocusRect( h, r ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawButtonDisable :
|-----------------------------------------------------------------------|
*/

void	DrawButtonDisable( HDC h, RECT * r, int with_marge )
{
	HBITMAP		o = SelectObject( h, DisableBitmap ) ;

	if ( with_marge )
		PatBlt( h, MARGE_FOCUS, MARGE_FOCUS,
			r->right - 2 * MARGE_FOCUS, r->bottom - 2 * MARGE_FOCUS,
				0xfa0089ul ) ;
	else	PatBlt( h, 0, 0, r->right, r->bottom, 0xfa0089ul ) ;

	SelectObject( h, o ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawPodium :
|-----------------------------------------------------------------------|
*/

void	DrawPodium( HDC h, RECT * r, int sel )
{
HPEN	oldp;
HBRUSH	oldb ;

	oldp = SelectObject( h, GetStockObject( NULL_PEN ) ) ;
	oldb = SelectObject( h, brushFace ) ;
	Rectangle( h, r->left, r->top, r->right, r->bottom ) ;
	SelectObject( h, penBorder) ;
        MoveToEx(h,r->left+1,r->top,NULL);LineTo(h,r->right-1,r->top);
        MoveToEx(h,r->left,r->top+1,NULL);LineTo(h,r->left,r->bottom-1);
        MoveToEx(h,r->left+1,r->bottom-1,NULL);LineTo(h,r->right-1,r->bottom-1);
        MoveToEx(h,r->right-1,r->top+1,NULL);LineTo(h,r->right-1,r->bottom-1);

	SelectObject( h, penShadow) ;
	if (sel)
	{
		MoveToEx(h,r->left+1	,r->bottom-2	,NULL) ;
		LineTo(h,  r->left+1	,r->top+1) ;
		LineTo(h,  r->right-2	,r->top+1) ;
	}
	else
	{
		MoveToEx(h,r->left+1	,r->bottom-2	,NULL) ;
		LineTo(h,  r->right-2	,r->bottom-2) ;
		LineTo(h,  r->right-2	,r->top) ;
		MoveToEx(h,r->left+2	,r->bottom-3	,NULL) ;
		LineTo(h,  r->right-3	,r->bottom-3) ;
		LineTo(h,  r->right-3	,r->top+1) ;

		SelectObject( h, penLight) ;

		MoveToEx(h,r->left+1	,r->bottom-2	,NULL) ;
		LineTo(h,  r->left+1	,r->top+1) ;
		LineTo(h,  r->right-2	,r->top+1) ;
	}
	SelectObject(h,oldp) ;
	SelectObject(h,oldb) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawBitmap :
|-----------------------------------------------------------------------|
*/

void	DrawBitmap( HDC hb, RECT * r, int n, int selected )
{
	HDC	hm ;
	int	x, y ;
	HBITMAP	b ;
	BITMAP	o ;

	hm = CreateCompatibleDC( hb ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	b = LoadOneBitmap( n, & o ) ;

	if ( b )
		{
		n -- ;
		b = SelectObject( hm, b ) ;

		x = ( r->left + r->right  - o.bmWidth ) / 2 ;
		y = ( r->top  + r->bottom - o.bmHeight ) / 2 ;

		if ( selected )
			{
			x ++ ;
			y ++ ;
			}

		BitBlt( hb, x, y, o.bmWidth, o.bmHeight, hm, 0, 0, SRCCOPY ) ;
		DeleteObject( SelectObject( hm, b ) ) ;
		}
	DeleteDC( hm ) ;
}

void	DrawBitmapLoaded( HDC hb, RECT * r, HBITMAP b,int width,int height, int selected )
{
	HDC	hm ;
	int	x, y ;

	hm = CreateCompatibleDC( hb ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	if ( b )
		{
		b = SelectObject( hm, b ) ;

		x = ( r->left + r->right  - width ) / 2 ;
		y = ( r->top  + r->bottom - height ) / 2 ;

		if ( selected )
			{
			x ++ ;
			y ++ ;
			}

		BitBlt( hb, x, y, width, height, hm, 0, 0, SRCCOPY ) ;
		SelectObject( hm, b ) ;
		}
	DeleteDC( hm ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawString :
|-----------------------------------------------------------------------|
*/

void	DrawString( HDC hb, RECT *r, LPSTR s, int selected )
{
	RECT	b ;
	int	m ;
	COLORREF old ;

	b.left   = r->left + 5 ;
	b.right  = r->right - 5 ;
	b.top	 = b.bottom = 0 ;

	DrawText( hb, s, -1, & b, DT_CALCRECT ) ;
	m = b.bottom - b.top ;

	b.left   = r->left + 5 ;
	b.right  = r->right - 5 ;
	b.top    = ( r->top + r->bottom - m ) / 2 ;
	b.bottom = ( r->top + r->bottom + m ) / 2 ;

	if ( selected )
		{
		b.left  ++ ;
		b.top   ++ ;
		b.right ++ ;
		b.bottom++ ;
		}

	SetBkMode( hb, TRANSPARENT ) ;
	old = SetTextColor(hb , colorText) ;
	DrawText( hb, s, -1, & b, DT_NOCLIP | DT_CENTER ) ;
	SetTextColor(hb , old) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawBtStrX :
|-----------------------------------------------------------------------|
*/

void	DrawBtmTxtHV( HDC h, RECT * r, int btm, LPSTR txt, int sel, int t )
{
	HDC	hm ;
	HBITMAP	b ;
	BITMAP	o ;
	COLORREF old ;

	DrawPodium( h, r, sel ) ;

	hm = CreateCompatibleDC( h ) ;
	if ( hm == ( HDC ) 0 )
		return ;

	b = LoadOneBitmap( btm, & o ) ;

	if ( b )
		{
		RECT	rt ;

		rt.left = rt.right = rt.top = rt.bottom = 0 ;
		DrawText( h, txt, -1, & rt, DT_CALCRECT ) ;

		if ( t )
			{
			rt.left = ( r->right - o.bmWidth ) / 2 ;
			rt.top  = ( r->bottom - rt.bottom - o.bmHeight ) / 3 ;
			}
		else	{
			rt.left = ( r->right - rt.right - o.bmWidth ) / 3 ;
			rt.top  = ( r->bottom - o.bmHeight ) / 2 ;
			}

		if ( sel )
			{
			rt.left ++ ;
			rt.top ++ ;
			}

		b = SelectObject( hm, b ) ;
		BitBlt( h, rt.left, rt.top, o.bmWidth, o.bmHeight,
							hm, 0, 0, SRCCOPY ) ;
		DeleteObject( SelectObject( hm, b ) ) ;

		if ( t )
			{
			rt.left += ( o.bmWidth - rt.right ) / 2 ;
			rt.top  += o.bmHeight + rt.top - sel ;
			}
		else	{
			rt.left += o.bmWidth + rt.left - sel ;
			rt.top  += ( o.bmHeight - rt.bottom ) / 2 ;
			}
		rt.right  += rt.left ;
		rt.bottom += rt.top  ;

		SetBkMode( h, TRANSPARENT ) ;
		old = SetTextColor(h , colorText) ;
		DrawText( h, txt, -1, & rt, DT_NOCLIP | DT_CENTER ) ;
		SetTextColor(h , old) ;
		}
	DeleteDC( hm ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DrawMac :
|-----------------------------------------------------------------------|
*/

void	DrawMac( HDC h, RECT * r, LPSTR txt, int sel )
{
HBRUSH old ;

	old = SelectObject( h, sel ? brushLight : brushFace);

	RoundRect( h, r->left, r->top, r->right, r->bottom, 8, 8 ) ;
	DrawString( h, r, txt, 0 ) ;
	SelectObject(h,old) ;
}

/*
|-----------------------------------------------------------------------|
| proc LoadOneBitmap :
|-----------------------------------------------------------------------|
*/

HBITMAP	LoadOneBitmap( int n, BITMAP * o )
{
	HBITMAP	b = LoadBitmap( Inst, MAKEINTRESOURCE( n ) ) ;

	if ( ! b )
		b = LoadBitmap( Inst, MAKEINTRESOURCE( DEFAULT_BTM ) ) ;

	if ( b )
		GetObject( b, sizeof( BITMAP ), ( LPSTR ) o ) ;
	return b ;
}

