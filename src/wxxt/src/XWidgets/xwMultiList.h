/****************************************************************************

	MultiList.h

	This file is the public header file for the MultiList widget, an
	extension to the Athena List widget.

	This code is loosely based on the Athena List source which
	is why the MIT copyright notice appears below.

	The code was changed substantially in V3.4 to change the
	action/callback interface which was unnecessarily ugly.  Code
	using some features of the old interface may need to be changed.
	Hope the changes don't make people's lives too miserable.

 ****************************************************************************/

/*
 * Author:
 * 	Brian Totty
 * 	Department of Computer Science
 * 	University Of Illinois at Urbana-Champaign
 *	1304 West Springfield Avenue
 * 	Urbana, IL 61801
 * 
 * 	totty@cs.uiuc.edu
 * 	
 */ 

/*
 * Copyright 2004-2005 PLT Scheme, Inc.
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Original Athena Author:  Chris D. Peterson, MIT X Consortium
 */

#ifndef _MULTILIST_H_
#define _MULTILIST_H_

#include <X11/Xaw/Simple.h>
#ifdef WX_USE_XFT
# include <X11/Xft/Xft.h>
#endif

/*---------------------------------------------------------------------------*

      R E S O U R C E    D E S C R I P T I O N S    A N D    N O T E S

 *---------------------------------------------------------------------------*/

/*

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback            Callback           XtCallbackList  NULL                *1
 columnWidth         Width              Dimension       0                   *9
 columnSpacing       Spacing            Dimension       8
 cursor		     Cursor		Cursor		left_ptr
 defaultColumns      Columns            int             1                   *2
 destroyCallback     Callback		Pointer		NULL 
 font		     Font		XFontStruct*	XtDefaultFont
 forceColumns        Columns            Boolean         False               *2
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	0                   *3
 highlightBackground HBackground	Pixel		XtDefaultForeground *4
 highlightForeground HForeground	Pixel		XtDefaultBackground *4
 insensitiveBorder   Insensitive	Pixmap		Gray
 list                List               String *        NULL                *5
 longest             Longest            int             0                   *6
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 maxSelectable       Value              int             1                   *7
 numberStrings       NumberStrings      int             0                   *5
 pasteBuffer         Boolean            Boolean         False
 rowHeight           Height             Dimension       0                   *9
 rowSpacing          Spacing            Dimension       2
 sensitive	     Sensitive		Boolean		True
 sensitiveArray      List               Boolean *       NULL                *8
 shadeSurplus	     Boolean		Boolean		True		    *10
 verticalList        Boolean            Boolean         False
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

 *1 -  The callback functions are called whenever a highlight or unhighlight
       takes place.  More precisely, a callback occurs whenever the Notify()
       action is triggered.  By default, this occurs when the mouse button is
       lifted after a selection or deselection occurred.  The callback returns
       an XfwfMultiListReturnStruct data structure which contains numerous
       fields describing the selection state.  The most important fields
       indicate the total number of items selected, and a list of those items.

 *2 -  The defaultColumns resource is used in two cases.  If forceColumns
       is true, the widget will set the number of columns to the value of
       default columns.  If the widget width is unconstrained by the parent
       widgets, the defaultColumns is also used to determine the number of
       columns and the resulting width.  Otherwise, the number of columns
       will be calcultaed based on the current width and will be changed to
       an appropriate value.

 *3 -  If the width or height is set to zero (0), which is the default case,
       then the widget will calculate the size of that dimension to be just
       large enough to hold the contents of the widget.

 *4 -  The highlightForeground and highlightBackground colors specify the
       colors used to highlight the text (foreground) and the surrounding
       background space of a list item when it is selected (highlighted).
       The default is the reverse of the default foreground and background
       colors.

 *5 -  The list resource is an array of strings (char * array) which tell
       the names of each item of the list.  The number of elements of this
       array are indicated by the resource numberStrings.  If numberStrings
       is set to 0 (the default), then the MultiList widget will count the
       number of strings in the list.  This requires that the list be
       NULL terminated.  If list is NULL, then the widget treats it as an
       empty list.  Once the list is set the list resource is set to NULL,
       so you won't be able to read back the list after it has been set.  The
       widgets copies the strings internally, so the user can free the list
       storage after setting it.

 *6 -  This resource represent the longest string in pixels.  If this
       resource is zero (0), which is the default and probably the value
       most people should use, the longest string length is calculated
       and the resource is updated.

 *7 -  The maxSelectable resource indicates the maximum number of items
       which can be selected at any one time.  In the original Athena
       widget, you could have at most one item selected at a time.  In
       this widget, you can choose how many will be selected at a time.

 *8 -  Each item in the MultiList can be made insensitive, so it is printed in
       gray shading and can not be highlighted.  This can be done by
       setting the sensitivity list, which is an array of Booleans which
       indicate whether or not the corresponding item is sensitive (can be
       selected).  If sensitivity list is NULL, all items are sensitive.  The
       widget copies the sensitivity information, so the user can delete the
       sensitivity array storage after setting it.  The widget sets the
       resource to NULL after it has been set, so the user cannot read the
       old list back.

 *9 -  These values are intended for reading only.  They indicate the pixel
       width/height of the column/row.

 *10 - If the list height is made larger than the sum of the list entry
       heights, the surplus space is shaded in the background color if
       shadeSurplus is False, or in a gray stipple pattern if shadeSurplus
       is True.       

*/

/*---------------------------------------------------------------------------*

                    S T R I N G    D E F I N I T I O N S

 *---------------------------------------------------------------------------*/

#define XtCMLXftFont          "XftFont"
#define XtNmlXftFont          "xftFont"

#define XtCList			"List"
#define XtCSpacing		"Spacing"
#define XtCColumns		"Columns"
#define XtCLongest		"Longest"
#define XtCNumberStrings	"NumberStrings"
#define	XtCHForeground		"HForeground"
#define	XtCHBackground		"HBackground"

#ifndef XtNcursor
#define XtNcursor		"cursor"
#endif

#define XtNoffset		"offset"

#define	XtNhighlightForeground	"highlightForeground"
#define	XtNhighlightBackground	"highlightBackground"
#define XtNcolumnSpacing	"columnSpacing"
#define XtNrowSpacing		"rowSpacing"
#define XtNdefaultColumns	"defaultColumns"
#define XtNforceColumns		"forceColumns"
#define XtNpasteBuffer		"pasteBuffer"
#define XtNverticalList		"verticalList"
#define XtNlongest		"longest"
#define XtNnumberStrings	"numberStrings"
#define XtNlist			"list"
#define	XtNsensitiveArray	"sensitiveArray"
#define	XtNmaxSelectable	"maxSelectable"
#define	XtNshadeSurplus		"shadeSurplus"
#define XtNclickExtends         "clickExtends"
#define XtNdrawgray             "drawgray"

#define	XtNrowHeight		"rowHeight"
#define	XtNcolumnWidth		"columnWidth"
 
#ifndef XtNtablist
#define XtNtablist "tablist"
#endif
#ifndef XtCTablist
#define XtCTablist "Tablist"
#endif

	/* Class Record Constants */

extern WidgetClass xfwfMultiListWidgetClass;

typedef struct _XfwfMultiListClassRec *XfwfMultiListWidgetClass;
typedef struct _XfwfMultiListRec      *XfwfMultiListWidget;

/*---------------------------------------------------------------------------*

                     R E T U R N    S T R U C T U R E

 *---------------------------------------------------------------------------*/

#define	XfwfMultiListActionNothing		0
#define	XfwfMultiListActionHighlight		1
#define	XfwfMultiListActionUnhighlight		2
#define	XfwfMultiListActionStatus		3
#define	XfwfMultiListActionDClick		4

typedef struct _XfwfMultiListReturnStruct
{
	int num_selected;		/* Number Of Items Now Selected */
	int *selected_items;		/* Indexes Of Selected Items */

	int action;			/* Last Action Performed */
	int item;			/* Last Item Index Modified */
	String string;			/* String Of Last Index Modified */
} XfwfMultiListReturnStruct;

/*---------------------------------------------------------------------------*

                     U T I L I T Y    R O U T I N E S

 *---------------------------------------------------------------------------*/

_XFUNCPROTOBEGIN

#if (!NeedFunctionPrototypes)

extern Boolean		XfwfMultiListHighlightItem();
extern void		XfwfMultiListHighlightAll();
extern void		XfwfMultiListUnhighlightItem();
extern void		XfwfMultiListUnhighlightAll();
extern int		XfwfMultiListToggleItem();
extern XfwfMultiListReturnStruct *
			XfwfMultiListGetHighlighted();
extern Boolean		XfwfMultiListIsHighlighted();
extern Boolean		XfwfMultiListGetItemInfo();
extern void		XfwfMultiListSetNewData();

#else

extern Boolean		XfwfMultiListHighlightItem(XfwfMultiListWidget mlw,
				int item_index);
extern void		XfwfMultiListHighlightAll(XfwfMultiListWidget mlw);
extern void		XfwfMultiListUnhighlightItem(XfwfMultiListWidget mlw,
				int item_index);
extern void		XfwfMultiListUnhighlightAll(XfwfMultiListWidget mlw);
extern int		XfwfMultiListToggleItem(XfwfMultiListWidget mlw,
				int item_index);
extern XfwfMultiListReturnStruct *
			XfwfMultiListGetHighlighted(XfwfMultiListWidget mlw);
extern Boolean		XfwfMultiListIsHighlighted(XfwfMultiListWidget mlw,
				int item_index);
extern Boolean		XfwfMultiListGetItemInfo(XfwfMultiListWidget mlw,
				int item_index, String *str_ptr,
				Boolean *h_ptr, Boolean *s_ptr);
extern void		XfwfMultiListSetNewData(XfwfMultiListWidget mlw,
				String *list, int nitems, int longest,
				Boolean resize, Boolean *sensitivity_array);

#endif

_XFUNCPROTOEND

#endif
