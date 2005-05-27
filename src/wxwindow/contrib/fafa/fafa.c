/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.1		|
|									|
|	Controles Windows 						|
|	Initialisations de la librairie					|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#include		"fafapriv.h"
#ifdef __WATCOMC__
#include                <string.h>
#else
#include		<memory.h>
#endif
#include		"wx_setup.h"

/*
|-----------------------------------------------------------------------|
| Parametres
|-----------------------------------------------------------------------|
*/

LRESULT	WINAPI LoopFafaDialog( HWND w, UINT msg, UINT mw, LPARAM ml ) ;
LRESULT	WINAPI LoopFafaButton( HWND w, UINT msg, UINT mw, LPARAM ml ) ;
LRESULT	WINAPI LoopFafaStatic( HWND w, UINT msg, UINT mw, LPARAM ml ) ;
LRESULT	WINAPI LoopFafaCheck ( HWND w, UINT msg, UINT mw, LPARAM ml ) ;
LRESULT	WINAPI LoopFafaCont  ( HWND w, UINT msg, UINT mw, LPARAM  ) ;

HANDLE	Inst ;

//*** added by Chubraev for __WATCOMC__ multiple instances support
#ifdef  _MULTIPLE_INSTANCES
#include <stdio.h>
#define ZZ	32
char	FafaWind[ZZ];
char	FafaButt[ZZ];
char	FafaStat[ZZ];
char	FafaChck[ZZ];
char	MichButt[ZZ];
#undef	ZZ
//***
#else
char	FafaWind[] = FAFA_WIND ;
char	FafaButt[] = FAFA_BUTTON ;
char	FafaStat[] = FAFA_STATIC ;
char	FafaChck[] = FAFA_CHECK ;
char	MichButt[] = FAFA_CONT ;
#endif

#define	DISABLE_BTM		9100

static	int	MyRegisterClass( void ) ;

HPEN		penBorder,
		penShadow,
		penLight;
HBRUSH		brushFace,
		brushBack,
		brushShadow,
		brushFrame,
		brushLight;
COLORREF 	colorText,
		colorLabel;
HPEN		staticBorder,
		staticShadow,
		staticLight;

/*
|-----------------------------------------------------------------------|
| proc LibMain :
|-----------------------------------------------------------------------|
*/

int InitFafa( HANDLE inst)
{
#ifdef _MULTIPLE_INSTANCES
	sprintf(FafaWind,"FafaDialog%d",inst);
	sprintf(FafaButt,"FafaButton%d",inst);
	sprintf(FafaStat,"FafaStatic%d",inst);
	sprintf(FafaChck,"FafaCheck%d",inst);
	sprintf(MichButt,"FafaCont%d",inst);
#endif
	Inst = inst ;
	CreatePensBrushes() ;

	if ( MyRegisterClass() )
		{
		LOGBRUSH	lb ;

		lb.lbStyle = BS_PATTERN;
		//lb.lbHatch = (LONG)LoadBitmap( Inst, MAKEINTRESOURCE(DISABLE_BTM) ) ;
		//int for Win31. A voir pour Win32
		lb.lbHatch = (int)LoadBitmap( Inst, MAKEINTRESOURCE(DISABLE_BTM) ) ;

		DisableBitmap = CreateBrushIndirect( & lb ) ;
		DeleteObject( (HGDIOBJ)lb.lbHatch ) ;

		return 1 ;
		}
	else	
		DeletePensBrushes() ;
	return 0 ;
}

void EndFafa()
{
	if (DisableBitmap)
		DeleteObject(DisableBitmap) ;
	DeletePensBrushes() ;

}

/*
|-----------------------------------------------------------------------|
| proc MyRegisterClass :
|-----------------------------------------------------------------------|
*/

static	int	MyRegisterClass( void )
{
	WNDCLASS	w ;

	memset( & w, 0, sizeof w ) ;
	w.lpszClassName = FafaWind ;
	w.hInstance	= Inst ;
	w.lpfnWndProc	= LoopFafaDialog ;
	w.hCursor	= NULL /* LoadCursor( 0, IDC_ARROW ) */ ;
	w.hIcon		= LoadIcon( Inst, "fafa_icn") ;
	w.hbrBackground = ((HBRUSH)(COLOR_WINDOW+1)) ;
	w.style		= CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	w.cbWndExtra	= DLGWINDOWEXTRA ;
	if ( ! RegisterClass( & w ) )
		return 0 ;

	w.lpszClassName = FafaButt ;
	w.lpfnWndProc	= LoopFafaButton ;
	w.cbWndExtra	= FB_EXTRA ;
	w.hCursor	= 0 ;
	w.hIcon		= 0 ;
	if ( ! RegisterClass( & w ) )
		return 0 ;

	w.lpszClassName = FafaChck ;
	w.lpfnWndProc	= LoopFafaCheck ;
	if ( ! RegisterClass( & w ) )
		return 0 ;

	w.lpszClassName = MichButt ;
	w.lpfnWndProc	= LoopFafaCont ;
	if ( ! RegisterClass( & w ) )
		return 0 ;

	w.lpszClassName = FafaStat ;
	w.lpfnWndProc	= LoopFafaStatic ;
	w.style		= CS_HREDRAW | CS_VREDRAW ;
	return RegisterClass( & w ) ;
}

/*
|-----------------------------------------------------------------------|
| proc DeletePensBrushes :
|-----------------------------------------------------------------------|
*/
void DeletePensBrushes(void)
{
	if (staticBorder)
		DeleteObject(staticBorder) ;
	if (staticShadow)
		DeleteObject(staticShadow) ;
	if (staticLight)
		DeleteObject(staticLight) ;
	if (penBorder)
		DeleteObject(penBorder) ;
	if (penShadow)
		DeleteObject(penShadow) ;
	if (penLight)
		DeleteObject(penLight) ;
	if (brushFace)
		DeleteObject(brushFace) ;
	if (brushBack)
		DeleteObject(brushBack) ;
	if (brushLight)
		DeleteObject(brushLight) ;
	if (brushFrame)
		DeleteObject(brushFrame) ;
	if (brushShadow)
		DeleteObject(brushShadow) ;

	staticBorder = NULL ;
	staticShadow = NULL ;
	staticLight = NULL ;
	brushFrame = NULL ;
	brushShadow = NULL ;
	penShadow = NULL ;
	penLight = NULL ;
	brushFace = NULL ;
	brushBack = NULL ;
	brushLight = NULL ;

}

/*
|-----------------------------------------------------------------------|
| proc CreatePensBrushes :
|-----------------------------------------------------------------------|
*/
void CreatePensBrushes(void)
{
COLORREF ms_color ;
BYTE	 red,green,blue ;
unsigned int val ;

	DeletePensBrushes() ;

	ms_color = GetSysColor(COLOR_WINDOWFRAME) ;
	penBorder = CreatePen(PS_SOLID,0,ms_color) ;

	ms_color = GetSysColor(COLOR_BTNSHADOW) ;
	penShadow = CreatePen(PS_SOLID,0,ms_color) ;

	ms_color = GetSysColor(COLOR_BTNHIGHLIGHT) ;
	penLight = CreatePen(PS_SOLID,0,ms_color) ;
	brushLight = CreateSolidBrush(ms_color) ;

	colorText = GetSysColor(COLOR_BTNTEXT) ;

	colorLabel = GetSysColor(COLOR_WINDOWTEXT) ;

	ms_color = GetSysColor(COLOR_BTNFACE) ;
	brushFace = CreateSolidBrush(ms_color) ;

	ms_color = GetSysColor(COLOR_APPWORKSPACE) ;
	brushFrame = CreateSolidBrush(ms_color) ;

	ms_color = GetSysColor(COLOR_WINDOW) ;
// #if USE_GREY_BACKGROUND
// 	brushBack = CreateSolidBrush(RGB(200, 200, 200)) ;
// #else
	brushBack = CreateSolidBrush(ms_color) ;
// #endif
	/* Foreground color (static) */
	red   = GetRValue(ms_color) ;
	green = GetGValue(ms_color) ;
	blue  = GetBValue(ms_color) ;
	val = red ;
	val -= (val*45)/100 ;
	val -= (val*4)/10 ;
	red = val & 0xff ;
	val = green ;
	val -= (val*45)/100 ;
	val -= (val*4)/10 ;
	green = val & 0xff ;
	val = blue ;
	val -= (val*45)/100 ;
	val -= (val*4)/10 ;
	blue = val & 0xff ;
	staticBorder = CreatePen(PS_SOLID,0,RGB(red,green,blue)) ;

	/* Top shadow color (static) */
	red   = GetRValue(ms_color) ;
	green = GetGValue(ms_color) ;
	blue  = GetBValue(ms_color) ;
	val = red ;
	val += (val*4)/10 ;
	if (val>255)	val = 255 ;
	red = val ;
	val = green ;
	val += (val*4)/10 ;
	if (val>255)	val = 255 ;
	green = val ;
	val = blue ;
	val += (val*4)/10 ;
	if (val>255)	val = 255 ;
	blue = val ;
	staticLight = CreatePen(PS_SOLID,0,RGB(red,green,blue)) ;

	/* Bottom shadow color (static) */
	red   = GetRValue(ms_color) ;
	green = GetGValue(ms_color) ;
	blue  = GetBValue(ms_color) ;
	val = red ;
	val -= (val*45)/100 ;
	red = val & 0xff ;
	val = green ;
	val -= (val*45)/100 ;
	green = val & 0xff ;
	val = blue ;
	val -= (val*45)/100 ;
	blue = val & 0xff ;
	staticShadow = CreatePen(PS_SOLID,0,RGB(red,green,blue)) ;
	brushShadow = CreateSolidBrush(RGB(red,green,blue)) ;

}
