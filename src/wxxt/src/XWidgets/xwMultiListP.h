/****************************************************************************

	MultiListP.h

	This file is the private header file for the MultiList widget, an
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

#ifndef _MULTILISTP_H_
#define _MULTILISTP_H_

#include <X11/Xaw/SimpleP.h>
#include <xwMultiList.h>

/*---------------------------------------------------------------------------*

      L O C A L    D A T A    S T R U C T U R E    D E F I N I T I O N S

 *---------------------------------------------------------------------------*/

typedef	struct
{
	Boolean		sensitive;
	Boolean		highlighted;
	String		string;
} XfwfMultiListItem;

/*---------------------------------------------------------------------------*

     W I D G E T    D A T A    S T R U C T U R E    D E F I N I T I O N S

 *---------------------------------------------------------------------------*/

typedef struct
{
	int		foo;
} XfwfMultiListClassPart;

typedef struct _XfwfMultiListClassRec
{
	CoreClassPart		core_class;
	SimpleClassPart		simple_class;
	XfwfMultiListClassPart	multiList_class;
} XfwfMultiListClassRec;

extern XfwfMultiListClassRec xfwfMultiListClassRec;

typedef struct
{
	Pixel			foreground;
	Pixel			highlight_fg;
	Pixel			highlight_bg;
	Dimension		column_space;
	Dimension		row_space;
	int			default_cols;
	Boolean			force_cols;
	Boolean			paste;
	Boolean			row_major;
	int			longest;
	int			nitems;
	XFontStruct		*font;
#ifdef WX_USE_XFT
	XftFont	                *xft_font;
#endif
	String			*list;
	Boolean			*sensitive_array;
	XtCallbackList  	callback;
	int			max_selectable;
	Boolean			shade_surplus;
	Dimension		col_width;
	Dimension		row_height;

        int                     offset;
	int			nrows;
	int			ncols;
	int			most_recent_clicked_item;
	int			most_recent_action;
	Time			last_button_release;
	GC			erase_gc;
	GC			draw_gc;
	GC			highlight_bg_gc;
	GC			highlight_fg_gc;
	GC			gray_gc;
	XfwfMultiListItem	*item_array;
	int			num_selected;
	int			*sel_array;

	char			*tablist;
	int			*tabs;
	Boolean                 clickExtends;
	Boolean                 drawgray;
} XfwfMultiListPart;

typedef struct _XfwfMultiListRec
{
	CorePart		core;
	SimplePart		simple;
	XfwfMultiListPart	multiList;
} XfwfMultiListRec;

/*---------------------------------------------------------------------------*

          D A T A    S T R U C T U R E    A C C E S S    M A C R O S

 *---------------------------------------------------------------------------*/

#define	MultiListItemSensitive(i)	((i)->sensitive)
#define	MultiListItemHighlighted(i)	((i)->highlighted)
#define	MultiListItemString(i)		((i)->string)

#define	InstanceCore(w)			(&((w)->core))
#define	InstanceSimple(w)		(&((w)->simple))
#define	InstanceMultiList(w)		(&((w)->multiList))

#define	MultiListWidth(w)		(InstanceCore(w)->width)
#define	MultiListHeight(w)		(InstanceCore(w)->height)
#define	MultiListBG(w)			(InstanceCore(w)->background_pixel)
#define	MultiListSensitive(w)		(InstanceCore(w)->sensitive)
#define	MultiListAncesSensitive(w)	(InstanceCore(w)->ancestor_sensitive)
#define	MultiListDepth(w)		(InstanceCore(w)->depth)

#define	MultiListFG(w)			(InstanceMultiList(w)->foreground)
#define	MultiListHighlightFG(w)		(InstanceMultiList(w)->highlight_fg)
#define	MultiListHighlightBG(w)		(InstanceMultiList(w)->highlight_bg)
#define	MultiListColumnSpace(w)		(InstanceMultiList(w)->column_space)
#define	MultiListRowSpace(w)		(InstanceMultiList(w)->row_space)
#define	MultiListDefaultCols(w)		(InstanceMultiList(w)->default_cols)
#define	MultiListForceCols(w)		(InstanceMultiList(w)->default_cols)
#define	MultiListPaste(w)		(InstanceMultiList(w)->paste)
#define	MultiListRowMajor(w)		(InstanceMultiList(w)->row_major)
#define	MultiListLongest(w)		(InstanceMultiList(w)->longest)
#define	MultiListNumItems(w)		(InstanceMultiList(w)->nitems)
#define	MultiListFont(w)		(InstanceMultiList(w)->font)
#ifdef WX_USE_XFT
# define MultiListXftFont(w)		(InstanceMultiList(w)->xft_font)
#else
# define MultiListXftFont(w)		NULL
#endif
#define	MultiListList(w)		(InstanceMultiList(w)->list)
#define	MultiListSensitiveArray(w)	(InstanceMultiList(w)->sensitive_array)
#define	MultiListCallback(w)		(InstanceMultiList(w)->callback)
#define	MultiListMaxSelectable(w)	(InstanceMultiList(w)->max_selectable)
#define	MultiListShadeSurplus(w)	(InstanceMultiList(w)->shade_surplus)
#define	MultiListClickExtends(w)	(InstanceMultiList(w)->clickExtends)
#define	MultiListDrawGray(w)	        (InstanceMultiList(w)->drawgray)

#define	MultiListOffset(w)	        (InstanceMultiList(w)->offset)

#define	MultiListColWidth(w)		(InstanceMultiList(w)->col_width)
#define	MultiListRowHeight(w)		(InstanceMultiList(w)->row_height)
#define	MultiListNumRows(w)		(InstanceMultiList(w)->nrows)
#define	MultiListNumCols(w)		(InstanceMultiList(w)->ncols)
#define	MultiListMostRecentItem(w)	(InstanceMultiList(w)->most_recent_clicked_item)
#define	MultiListMostRecentAct(w)	(InstanceMultiList(w)->most_recent_action)
#define MultiListLastRelease(w)		(InstanceMultiList(w)->last_button_release)
#define	MultiListEraseGC(w)		(InstanceMultiList(w)->erase_gc)
#define	MultiListDrawGC(w)		(InstanceMultiList(w)->draw_gc)
#define	MultiListHighlightForeGC(w)	(InstanceMultiList(w)->highlight_fg_gc)
#define	MultiListHighlightBackGC(w)	(InstanceMultiList(w)->highlight_bg_gc)
#define	MultiListGrayGC(w)		(InstanceMultiList(w)->gray_gc)
#define	MultiListItemArray(w)		(InstanceMultiList(w)->item_array)
#define	MultiListNthItem(w,n)		(&(MultiListItemArray(w)[n]))
#define	MultiListSelArray(w)		(InstanceMultiList(w)->sel_array)
#define	MultiListNumSelected(w)		(InstanceMultiList(w)->num_selected)

#define	MultiListTabList(w)		(InstanceMultiList(w)->tablist)
#define	MultiListTabs(w)		(InstanceMultiList(w)->tabs)

#endif
