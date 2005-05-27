/*
|-----------------------------------------------------------------------|
|									|
|	F.Blancher				Mar.	1993		|
|	Fafa	 				Version 1.0		|
|									|
|	Controles Windows 						|
|	Fafa.h - Definitions exportees					|
|									|
|	Copyright (c)	FB			Paris			|
|									|
|-----------------------------------------------------------------------|
*/

#ifndef			__FAFA_H
#define			__FAFA_H

#ifndef			__WINDOWS_H
#define			NOCOM
#include		<windows.h>
#endif

#include <custcntl.h>

	/* Microsoft says: do not use message below WM_USER+0x100 ... */

enum	{				/* Messages users */
	WM_WHOAREYOU	= ( WM_USER + 0x101 ),
	WM_CHANGEBITMAP	= ( WM_USER + 0x102 ),
	WM_CHANGEICON	= ( WM_USER + 0x105 ),
	/* Do not use BM_GET... : they are translated!!! */
	FAFA_GETCHECK   = ( WM_USER + 0x103 ),
	FAFA_SETCHECK   = ( WM_USER + 0x104 )
	} ;

#define	FAFA_WIND	"FafaDialog"
#define	FAFA_BUTTON	"FafaButton"
#define	FAFA_STATIC	"FafaStatic"
#define	FAFA_CHECK	"FafaCheck"
#define FAFA_CONT	"FafaCont"

/*
|-----------------------------------------------------------------------|
| Controle FafaButton (et FafaContinue)
|-----------------------------------------------------------------------|
*/

#define	FB_MARGIN	5

enum	{				/* Fafa Button Styles : */
	FB_TEXT,			/* texted multilignes */
	FB_BITMAP,			/* bitmap */
	FB_BTMTXT_H,			/* Bitmap-texte horizontal */
	FB_BTMTXT_V,			/* Bitmap-texte vertical */
	FB_MAC,				/* texte mode Mac */
	FB_DBLBTM,			/* Double Bitmap */

	FB_DBLCLKS	= 0x0010,	/* double click autorise */
	FB_RIGHTBUT	= 0x0020	/* bouton droit autorise */
	} ;

#define	FB_DEFAULT	( FB_TEXT | WS_CHILD | WS_VISIBLE | WS_TABSTOP )

/*
|-----------------------------------------------------------------------|
| Controle FB_STATIC
|-----------------------------------------------------------------------|
*/

enum	{				/* Fafa Button Styles : */
	FS_TEXT,			/* texte */
	FS_BITMAP,			/* bitmap */
	FS_MEPLAT_TXT,			/* meplat + texte */
	FS_MEPLAT_BTM,			/* meplat + bitmap */
	FS_CADRE_DOWN,			/* cadre enfonce */
	FS_CADRE_UP,			/* cadre relief */
	FS_ICON,			/* icons */

	FS_X1		= ( 0 << 4 ),		/* position en X */
	FS_X2		= ( 1 << 4 ),
	FS_X3		= ( 2 << 4 ),
	FS_X4		= ( 3 << 4 ),

	FS_Y1		= ( 0 << 6 ),		/* position en Y */
	FS_Y2		= ( 1 << 6 ),
	FS_Y3		= ( 2 << 6 ),
	FS_Y4		= ( 3 << 6 ),
	} ;

#define	FS_DEFAULT	( FS_TEXT | WS_CHILD | WS_VISIBLE | WS_GROUP )

/*
|-----------------------------------------------------------------------|
| Controle FB_CHECK
|-----------------------------------------------------------------------|
*/

enum	{				/* Fafa Check Styles : */
	FC_CIR_DWN,			/* cercle enfonce */
	FC_CIR_UP,			/* cercle en relief */
	FC_REC_DWN,			/* carre enfonce */
	FC_REC_UP,			/* carre en relief */

	FC_RIGHTBUT	= 0x0010,	/* bouton droit autorise */
	FC_PRESELECT	= 0x0020,	/* preselectionne */
	FC_RADIO	= 0x0040,	/* mode radio */
	FC_BUTTONDRAW	= 0x0080	/* dessin mode bouton */
	} ;

#define	FC_DEFAULT		( WS_CHILD | WS_VISIBLE | WS_TABSTOP )

#ifdef __cplusplus
extern "C" {
#endif

void CreatePensBrushes(void) ;
void DeletePensBrushes(void) ;
int InitFafa(HANDLE);
void EndFafa(void);

extern HPEN	staticBorder,
		staticShadow,
		staticLight;
extern HPEN	penBorder,
		penShadow,
		penLight;
extern HBRUSH	brushFace,
		brushLight,
		brushFrame,
		brushShadow,
		brushBack;
extern COLORREF colorText,
		colorLabel;

#ifdef __cplusplus
}
#endif

#endif	/* __FAFA_H */


