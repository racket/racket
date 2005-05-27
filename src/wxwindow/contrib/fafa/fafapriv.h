/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	FafaDll 				Version 1.0		|
|									|
|	Dll Controles Windows 						|
|	FafaDll.h - Definitions privees					|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#ifndef			__FAFAPRIV_H
#define			__FAFAPRIV_H

#include		"fafa.h"

#ifdef WIN32
#define CTLTITLE 256	/* Sorry, I've not found equiv in NT ??? */
#endif

/*
|-----------------------------------------------------------------------|
| defines generaux :
|-----------------------------------------------------------------------|
*/

enum	{				/* Controles fafa */
	FB_BUTTON,
	FB_STATIC,
	FB_CHECK,
	MB_BUTTON,

	NB_FAFACTRL			/* nombre de controles fafa */
	} ;

#define	OF_RES		8000		/* offset des ressources types */

enum	{				/* etats */
	FB_SELECTED	= 0x0001,
	FB_CAPTURING	= 0x0002,
	FB_FOCUS	= 0x0004,
	FB_KEYDOWN	= 0x0008,
	FB_TO_SET	= 0x0010
	} ;

enum	{				/* Extra BYTES, Offsets : */
	OF_RESERVED	= 0,		/* oblige par windows !! */
	OF_STATE	= 2,		/* etat courant */
	OF_ACC		= 4,		/* touch acceleratrice */
	OF_FONT		= 6,		/* Font */
	OF_BITMAP	= 10,		/* Bitmap */
	OF_BWIDTH	= 14,
	OF_BHEIGHT	= 16
	} ;

#define	FB_EXTRA	18

/*
|-----------------------------------------------------------------------|
| defines	 :
|-----------------------------------------------------------------------|
*/

#define	GetStX( w )	( ( ( ( UINT ) ( w ) ) >> 4 ) & 0x3 )
#define	GetStY( w )	( ( ( ( UINT ) ( w ) ) >> 6 ) & 0x3 )

#define	SetStX( v )	( ( ( ( UINT ) ( v ) ) & 0x3 ) << 4 )
#define	SetStY( v )	( ( ( ( UINT ) ( v ) ) & 0x3 ) << 6 )

#define	SetState(f) SetWindowWord(w,OF_STATE,(WORD)(GetWindowWord(w,OF_STATE)|(f)))
#define	ClrState(f) SetWindowWord(w,OF_STATE,(WORD)(GetWindowWord(w,OF_STATE)&(~(f))))
#define	XorState(f) SetWindowWord(w,OF_STATE,(WORD)(GetWindowWord(w,OF_STATE )^(f)))
#define	TstState(f) ( ( GetWindowWord( w, OF_STATE ) & (f) ) != 0 )

#define	ffGetState()	GetWindowWord( w, OF_STATE )
#define	GetStyle()	GetWindowLong( w, GWL_STYLE )
#define	SetStyle(s)	SetWindowLong( w, GWL_STYLE , (LONG) (s))

#define	GetAcc()	GetWindowWord( w, OF_ACC )
#define	SetAcc( c )	SetWindowWord( w, OF_ACC, ( WORD ) ( c ) )

#define	GetFont()	(HFONT)GetWindowLong( w, OF_FONT )
#define	SetFont( f )	SetWindowLong( w, OF_FONT, ( LONG ) ( f ) )

#define	ffGetBitmap()	(HBITMAP)GetWindowLong( w, OF_BITMAP )
#define	SetBitmap( f )	SetWindowLong( w, OF_BITMAP, ( LONG ) ( f ) )

#define	GetBitmapW()	GetWindowWord( w, OF_BWIDTH )
#define	SetBitmapW( f )	SetWindowWord( w, OF_BWIDTH,  ( f ) )

#define	GetBitmapH()	GetWindowWord( w, OF_BHEIGHT )
#define	SetBitmapH( f )	SetWindowWord( w, OF_BHEIGHT,  ( f ) )

#define	ffGetIcon()	(HICON)GetWindowLong( w, OF_BITMAP )
#define	ffSetIcon( f )	SetWindowLong( w, OF_BITMAP, ( LONG ) ( f ) )

#define	TstF( w, f )	( ( ( w ) & f ) != 0 )
#define	BtStyle( w )	( ( ( UINT ) ( w ) ) & 0xf )

#define	IsDisabled()	TstF( GetStyle(), WS_DISABLED )

#define	XyInRect( x, y, r )	( ( r ).left   < (int)( x )	&&	\
				  ( r ).right  > (int)( x )	&&	\
				  ( r ).top    < (int)( y )	&&	\
				  ( r ).bottom > (int)( y )	)

#define	KeyIsDown( v )		( (UINT) GetKeyState( v ) & 0x8000u )
#define	IsMessDown( m )		( m == WM_KEYDOWN || m == WM_SYSKEYDOWN )
#define	KeyIsAcc( w, mw )	( ! KeyIsDown( VK_CONTROL ) && mw == GetAcc() )

/*
|-----------------------------------------------------------------------|
|	Externes
|-----------------------------------------------------------------------|
*/

extern	HANDLE		Inst ;			/* instance librairie */
extern	HBITMAP		DisableBitmap ;		/* bitmap controles disables */

#ifdef __cplusplus
extern	"C" char	FafaWind[] ;
extern	"C" char	FafaButt[] ;
extern	"C" char	FafaStat[] ;
extern	"C" char	FafaChck[] ;
extern	"C" char	MichButt[] ;
#else
extern	char	FafaWind[] ;
extern	char	FafaButt[] ;
extern	char	FafaStat[] ;
extern	char	FafaChck[] ;
extern	char	MichButt[] ;
#endif

/*
|-----------------------------------------------------------------------|
|	Protos
|-----------------------------------------------------------------------|
*/

UINT	MyAtoi( char * s ) ;

void	InitButtonWords( HWND w ) ;

void	RedrawFafaButton( HWND w ) ;
void	DrawFafaButton( HWND w ,HDC h) ;
void	ChangeButtonFocus( HWND w, int on ) ;

void	DrawButtonFocus( HDC h, RECT * r, int sel ) ;
void	DrawButtonDisable( HDC h, RECT * r, int with_marge ) ;
void	DrawPodium( HDC h, RECT * r, int sel ) ;
void	DrawBitmap( HDC hb, RECT * r, int n, int selected ) ;
void	DrawBitmapLoaded( HDC hb, RECT * r, HBITMAP b,int width,int height, int selected ) ;
void	DrawString( HDC hb, RECT *r, LPSTR s, int selected ) ;
void	DrawBtmTxtHV( HDC h, RECT * r, int btm, LPSTR txt, int sel, int ) ;
void	DrawMac( HDC h, RECT * r, LPSTR txt, int sel ) ;

HBITMAP	LoadOneBitmap( int n, BITMAP * o ) ;

#endif	/* __FAFAPRIV_H */

