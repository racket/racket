/****************************************************************************

  Ugly-hacked MrEd version!
  (Allows lists with a virtual size > max-short)

***********************************************************************/

/****************************************************************************

	MultiList.c

	This file contains the implementation of the Picasso List
	widget.  Its functionality is intended to be similar to
	The Athena List widget, with some extra features added.

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

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "xwMultiListP.h"
#include "xwTabString.h"

/*===========================================================================*

          D E C L A R A T I O N S    A N D    D E F I N I T I O N S

 *===========================================================================*/

Pixmap XmuCreateStippledPixmap();
extern void XawInitializeWidgetSet();
extern int wxGetMultiClickTime(Display*);

#define	SUPERCLASS	&(simpleClassRec)

#ifdef WX_USE_XFT
# define	FontAscent(f, xf)	(xf ? (xf)->ascent : (f)->max_bounds.ascent)
# define	FontDescent(f, xf)	(xf ? (xf)->descent : (f)->max_bounds.descent)
#else
# define	FontAscent(f, xf)	((f)->max_bounds.ascent)
# define	FontDescent(f, xf)	((f)->max_bounds.descent)
#endif

#define	FontH(f, xf)	(FontAscent(f, xf) + FontDescent(f, xf) + 2)
#define	FontW(f,s,w)	(XfwfTextWidth(f,s,strlen(s), MultiListTabs(w)) + 1)
#define	FontMaxCharW(f)	((f)->max_bounds.rbearing-(f)->min_bounds.lbearing+1)

#ifndef abs
#define abs(a)			((a) < 0 ? -(a) : (a))
#endif

#define max(a,b)		((a) > (b) ? (a) : (b))
#define min(a,b)		((a) < (b) ? (a) : (b))
#define XtStrlen(s)		((s) ? strlen(s) : 0)

#define	TypeAlloc(t,n)		(t *)malloc(sizeof(t) * n)
#define	StrCopy(s)		strcpy(TypeAlloc(char,strlen(s)+1),s)
#define	StrCopyRetLength(s,lp)	strcpy(TypeAlloc(char,(*lp=(strlen(s)+1))),s)

#define CoreFieldOffset(f)	XtOffset(Widget,core.f)
#define	SimpleFieldOffset(f)	XtOffset(XfwfMultiListWidget,simple.f)
#define MultiListFieldOffset(f)	XtOffset(XfwfMultiListWidget,multiList.f)

/*===========================================================================*

        I N T E R N A L    P R O C E D U R E    D E C L A R A T I O N S

 *===========================================================================*/

#if (!NeedFunctionPrototypes)

static void			Initialize();
static void			Redisplay();
static XtGeometryResult		PreferredGeometry();
static void			Resize();
static Boolean			SetValues();

static void			DestroyOldData();
static void			InitializeNewData();
static void			CreateNewGCs();

static void			RedrawAll();
static void			RedrawItem();
static void			RedrawRowColumn();

static void			PixelToRowColumn();
static void			RowColumnToPixels();
static Boolean			RowColumnToItem();
static Boolean			ItemToRowColumn();

static void			Select();
static void			SelectOne();
static void			Unselect();
static void			Toggle();
static void			Extend();
static void			Notify();

#else

static void		Initialize(Widget request, Widget new);
static void 		Redisplay(XfwfMultiListWidget mlw,
				XEvent *event, Region rectangle_union);
static XtGeometryResult PreferredGeometry(XfwfMultiListWidget mlw,
				XtWidgetGeometry *parent_idea,
				XtWidgetGeometry *our_idea);
static void		Resize(XfwfMultiListWidget mlw);
static Boolean		SetValues(XfwfMultiListWidget cpl,
				XfwfMultiListWidget rpl,
			XfwfMultiListWidget npl);
static void		DestroyOldData(XfwfMultiListWidget mlw);
static void		InitializeNewData(XfwfMultiListWidget mlw);
static void		CreateNewGCs(XfwfMultiListWidget mlw);
static void		RedrawAll(XfwfMultiListWidget mlw);
static void		RedrawItem(XfwfMultiListWidget mlw, int item_index);
static void		RedrawRowColumn(XfwfMultiListWidget mlw,
				int row, int column);
static void		PixelToRowColumn(XfwfMultiListWidget mlw,
				int x, int y, int *row_ptr, int *column_ptr);
static void		RowColumnToPixels(XfwfMultiListWidget mlw,
				int row, int col, int *x_ptr, int *y_ptr,
				int *w_ptr, int *h_ptr);
static Boolean		RowColumnToItem(XfwfMultiListWidget mlw,
				int row, int column, int *item_ptr);
static Boolean		ItemToRowColumn(XfwfMultiListWidget mlw,
				int item_index, int *row_ptr, int *column_ptr);
static void		Select(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
static void		SelectOne(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
static void		Unselect(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
static void		Toggle(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
static void		Extend(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
static void		Notify(XfwfMultiListWidget mlw, XEvent *event,
				String *params, Cardinal *num_params);
#endif

/*===========================================================================*

              R E S O U R C E    I N I T I A L I Z A T I O N

 *===========================================================================*/

static XtResource resources[] =
{
	{XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	    CoreFieldOffset(width), XtRString, "0"},
	{XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	    CoreFieldOffset(height), XtRString, "0"},
	{XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	    CoreFieldOffset(background_pixel),XtRString,"XtDefaultBackground"},

	{XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	    SimpleFieldOffset(cursor), XtRString, "left_ptr"},
	{XtNhighlightForeground, XtCHForeground, XtRPixel, sizeof(Pixel),
	    MultiListFieldOffset(highlight_fg), XtRString, "XtDefaultBackground"},
	{XtNhighlightBackground, XtCHBackground, XtRPixel, sizeof(Pixel),
	    MultiListFieldOffset(highlight_bg), XtRString, "XtDefaultForeground"},
	{XtNcolumnSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	    MultiListFieldOffset(column_space), XtRImmediate, (caddr_t)8},
	{XtNrowSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	    MultiListFieldOffset(row_space), XtRImmediate, (caddr_t)0},
	{XtNdefaultColumns, XtCColumns, XtRInt,  sizeof(int),
	    MultiListFieldOffset(default_cols), XtRImmediate, (caddr_t)1},
	{XtNforceColumns, XtCColumns, XtRBoolean, sizeof(Boolean),
	    MultiListFieldOffset(force_cols), XtRString, (caddr_t) "False"},
	{XtNpasteBuffer, XtCBoolean, XtRBoolean, sizeof(Boolean),
	    MultiListFieldOffset(paste), XtRString, (caddr_t) "False"},
	{XtNnumberStrings, XtCNumberStrings, XtRInt,  sizeof(int),
	    MultiListFieldOffset(nitems), XtRImmediate, (caddr_t)0},
	{XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	    MultiListFieldOffset(font),XtRString, "XtDefaultFont"},
#ifdef WX_USE_XFT
	{XtNmlXftFont,  XtCMLXftFont, XtRPointer, sizeof(void*),
         MultiListFieldOffset(xft_font),XtRPointer, (XtPointer)0},
#endif
	{XtNlist, XtCList, XtRPointer, sizeof(char **),
	    MultiListFieldOffset(list), XtRString, NULL},
	{XtNsensitiveArray, XtCList, XtRPointer, sizeof(Boolean *),
	    MultiListFieldOffset(sensitive_array), XtRString, NULL},
	{XtNcallback, XtCCallback, XtRCallback, sizeof(caddr_t),
	    MultiListFieldOffset(callback), XtRCallback, NULL},
	{XtNmaxSelectable, XtCValue, XtRInt, sizeof(int),
	    MultiListFieldOffset(max_selectable), XtRImmediate, (caddr_t) 1},

	{XtNoffset, XtCValue, XtRInt, sizeof(int),
	    MultiListFieldOffset(offset), XtRString, "0"},

	{XtNshadeSurplus, XtCBoolean, XtRBoolean, sizeof(Boolean),
	    MultiListFieldOffset(shade_surplus), XtRString, "True"},

	{XtNcolumnWidth, XtCValue, XtRDimension, sizeof(Dimension),
	    MultiListFieldOffset(col_width), XtRImmediate, (caddr_t)0},
	{XtNrowHeight, XtCValue, XtRDimension, sizeof(Dimension),
	    MultiListFieldOffset(row_height), XtRImmediate, (caddr_t)0},

	{XtNtablist, XtCTablist, XtRString, sizeof(int *),
	    MultiListFieldOffset(tablist), XtRImmediate, (XtPointer)NULL },

	{XtNclickExtends, XtCBoolean, XtRBoolean, sizeof(Boolean),
            MultiListFieldOffset(clickExtends), XtRString, "False"},

	{XtNdrawgray, XtCBoolean, XtRBoolean, sizeof(Boolean),
            MultiListFieldOffset(drawgray), XtRString, "False"}

};

/*===========================================================================*

        A C T I O N    A N D    T R A N S L A T I O N    T A B L E S

 *===========================================================================*/

static char defaultTranslations[] =
"	Ctrl <Btn1Down>:			Toggle()\n\
	Shift <Btn1Down>:			SelectOne()\n\
	<Btn1Down>:			        Select()\n\
	Button1 <Btn1Motion>:			Extend()\n\
	<Btn1Up>:				Notify()";

static char extendTranslations[] =
"	Ctrl <Btn1Down>:			Toggle()\n\
	Shift <Btn1Down>:			SelectOne()\n\
	<Btn1Down>:			        SelectOne()\n\
	Button1 <Btn1Motion>:			Extend()\n\
	<Btn1Up>:				Notify()";

static XtActionsRec actions[] =
{
	{"Select",				(XtActionProc)Select},
	{"Unselect",				(XtActionProc)Unselect},
	{"Toggle",				(XtActionProc)Toggle},
	{"Extend",				(XtActionProc)Extend},
	{"SelectOne",				(XtActionProc)SelectOne},
	{"Notify",				(XtActionProc)Notify},
	{NULL,					(XtActionProc)NULL}
};

/*===========================================================================*

                    C L A S S    A L L O C A T I O N

 *===========================================================================*/

XfwfMultiListClassRec xfwfMultiListClassRec =
{
	{
		/* superclass		*/	(WidgetClass)SUPERCLASS,
		/* class_name		*/	"XfwfMultiList",
		/* widget_size		*/	sizeof(XfwfMultiListRec),
		/* class_initialize	*/	NULL,
		/* class_part_initialize*/	NULL,
		/* class_inited		*/	FALSE,
		/* initialize		*/	(XtInitProc)Initialize,
		/* initialize_hook	*/	NULL,
		/* realize		*/	XtInheritRealize,
		/* actions		*/	actions,
		/* num_actions		*/	XtNumber(actions),
		/* resources		*/	resources,
		/* resource_count	*/	XtNumber(resources),
		/* xrm_class		*/	NULLQUARK,
		/* compress_motion	*/	TRUE,
		/* compress_exposure	*/	FALSE,
		/* compress_enterleave	*/	TRUE,
		/* visible_interest	*/	FALSE,
		/* destroy		*/	NULL,
		/* resize		*/	(XtWidgetProc)Resize,
		/* expose		*/	(XtExposeProc)Redisplay,
		/* set_values		*/	(XtSetValuesFunc)SetValues,
		/* set_values_hook	*/	NULL,
		/* set_values_almost	*/	XtInheritSetValuesAlmost,
		/* get_values_hook	*/	NULL,
		/* accept_focus		*/	NULL,
		/* version		*/	XtVersion,
		/* callback_private	*/	NULL,
		/* tm_table		*/	defaultTranslations,
		/* query_geometry       */	(XtGeometryHandler)
							PreferredGeometry,
		/* display_accelerator  */	XtInheritDisplayAccelerator,
		/* extension            */	NULL
	}, /* Core Part */
	{
		/* change_sensitive     */	XtInheritChangeSensitive
	}
};

WidgetClass xfwfMultiListWidgetClass = (WidgetClass)&xfwfMultiListClassRec;

/*===========================================================================*

                       T O O L K I T    M E T H O D S

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	Initialize()

	This procedure is called by the X toolkit to initialize
	the widget instance.  The hook to this routine is in the
	initialize part of the core part of the class.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Initialize(request,new)
Widget request,new;
{
	XfwfMultiListWidget mlw;

	mlw = (XfwfMultiListWidget)new;
	MultiListFG(mlw) = BlackPixelOfScreen(XtScreen(new));
	CreateNewGCs(mlw);
	InitializeNewData(mlw);
	MultiListLastRelease(mlw) = CurrentTime;
	MultiListTabs(mlw) = XfwfTablist2Tabs(MultiListTabList(mlw));
	MultiListWidth(mlw) = MultiListHeight(mlw) = 100;
	MultiListNumCols(mlw) = 1;
	MultiListNumRows(mlw) = 1;
	if (MultiListClickExtends(mlw)) {
	  XtTranslations t = XtParseTranslationTable(extendTranslations);
	  XtOverrideTranslations((Widget)new, t);
	  XtFree((char *)t);
	}
} /* Initialize */


/*---------------------------------------------------------------------------*

	Redisplay(mlw,event,rectangle_union)

	This routine redraws the MultiList widget <mlw> based on the exposure
	region requested in <event>.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Redisplay(mlw,event,rectangle_union)
XfwfMultiListWidget mlw;
XEvent *event;
Region rectangle_union;
{
	GC shade_gc;
	int i,x1,y1,w,h,x2,y2,row,col,ul_row,ul_col,lr_row,lr_col;

	if (MultiListShadeSurplus(mlw))
		shade_gc = MultiListGrayGC(mlw);
	    else
		shade_gc = MultiListEraseGC(mlw);
	if (event == NULL)
	{
		XFillRectangle(XtDisplay(mlw),XtWindow(mlw),shade_gc,0,0,
			       MultiListWidth(mlw),MultiListHeight(mlw));
		for (i = 0; i < MultiListNumItems(mlw); i++) RedrawItem(mlw,i);
	}
	    else
	{
		x1 = event->xexpose.x;
		y1 = event->xexpose.y;
		w = event->xexpose.width;
		h = event->xexpose.height;
		x2 = x1 + w;
		y2 = y1 + h;
		XFillRectangle(XtDisplay(mlw),XtWindow(mlw),
			       shade_gc,x1,y1,w,h);
		PixelToRowColumn(mlw,x1,y1,&ul_row,&ul_col);
		PixelToRowColumn(mlw,x2,y2,&lr_row,&lr_col);
		lr_row = min(lr_row,MultiListNumRows(mlw) - 1);
		lr_col = min(lr_col,MultiListNumCols(mlw) - 1);
		for (col = ul_col; col <= lr_col; col++)
		{
			for (row = ul_row; row <= lr_row; row++)
			{
				RedrawRowColumn(mlw,row,col);
			}
		}
	}
} /* End Redisplay */


/*---------------------------------------------------------------------------*

	PreferredGeometry(mlw,parent_idea,our_idea)

	This routine is called by the parent to tell us about the
	parent's idea of our width and/or height.  We then suggest
	our preference through <our_idea> and return the information
	to the parent.

 *---------------------------------------------------------------------------*/

extern void XfwfCallComputeInside(Widget self,Position * x,Position * y,int * w,int * h);

static XtGeometryResult PreferredGeometry(mlw,parent_idea,our_idea)
XfwfMultiListWidget mlw;
XtWidgetGeometry *parent_idea,*our_idea;
{
        Position px, py;
	int nw,nh;
	Widget parent;

	parent = XtParent(mlw);
	XfwfCallComputeInside(parent, &px, &py, &nw, &nh);

	our_idea->request_mode |= (CWWidth | CWHeight);
	our_idea->width = nw;
	our_idea->height = nh;

	return XtGeometryAlmost;
} /* End PreferredGeometry */


/*---------------------------------------------------------------------------*

	Resize(mlw)

	This function is called when the widget is being resized.  It
	recalculates the layout of the widget.

 *---------------------------------------------------------------------------*/

static void Resize(mlw)
XfwfMultiListWidget mlw;
{
  MultiListColWidth(mlw) = MultiListWidth(mlw);
} /* End Resize */


/*---------------------------------------------------------------------------*

	SetValues(cpl,rpl,npl)

	This routine is called when the user is changing resources.  <cpl>
	is the current widget before the user's changes have been instituted.
	<rpl> includes the original changes as requested by the user.  <npl>
	is the new resulting widget with the requested changes and with all
	superclass changes already made.

 *---------------------------------------------------------------------------*/

/*ARGSUSED*/
static Boolean SetValues(cpl,rpl,npl)
XfwfMultiListWidget cpl,rpl,npl;
{
	Boolean redraw,recalc;

	redraw = False;
	recalc = False;

		/* Graphic Context Changes */

	if ((MultiListFG(cpl) != MultiListFG(npl)) ||
	    (MultiListBG(cpl) != MultiListBG(npl)) ||
	    (MultiListHighlightFG(cpl) != MultiListHighlightFG(npl)) ||
	    (MultiListHighlightBG(cpl) != MultiListHighlightBG(npl)) ||
	    (MultiListFont(cpl) != MultiListFont(npl)) ||
	    (MultiListXftFont(cpl) != MultiListXftFont(npl)))
	{
		XtDestroyGC(MultiListEraseGC(cpl));
		XtDestroyGC(MultiListDrawGC(cpl));
		XtDestroyGC(MultiListHighlightForeGC(cpl));
		XtDestroyGC(MultiListHighlightBackGC(cpl));
		XtDestroyGC(MultiListGrayGC(cpl));
		CreateNewGCs(npl);
		redraw = True;
	}

		/* Changes That Require Redraw */

	if ((MultiListSensitive(cpl) != MultiListSensitive(npl)) ||
	    (MultiListAncesSensitive(cpl) != MultiListAncesSensitive(npl)) ||
	    (MultiListDrawGray(cpl) != MultiListDrawGray(npl)))
	{
		redraw = True;
	}

		/* Changes That Require Data Initialization */

	if ((MultiListList(cpl) != MultiListList(npl)) ||
	    (MultiListMaxSelectable(cpl) != MultiListMaxSelectable(npl)) ||
	    (MultiListNumItems(cpl) != MultiListNumItems(npl)) ||
	    (MultiListSensitiveArray(cpl) != MultiListSensitiveArray(npl)))
	{
		DestroyOldData(cpl);
		InitializeNewData(npl);
		recalc = True;
		redraw = True;
	}

	if (MultiListTabList(cpl) != MultiListTabList(npl))
	{
		if (MultiListTabs(cpl))
			XtFree( (char *) MultiListTabs(cpl));
		MultiListTabs(npl) = XfwfTablist2Tabs(MultiListTabList(npl));
	}

		/* Changes That Require Recalculating Coordinates */

	if ((MultiListWidth(cpl) != MultiListWidth(npl)) ||
	    (MultiListHeight(cpl) != MultiListHeight(npl)) ||
	    (MultiListColumnSpace(cpl) != MultiListColumnSpace(npl)) ||
	    (MultiListRowSpace(cpl) != MultiListRowSpace(npl)) ||
	    (MultiListDefaultCols(cpl) != MultiListDefaultCols(npl)) ||
	    ((MultiListForceCols(cpl) != MultiListForceCols(npl)) &&
	     (MultiListNumCols(cpl) != MultiListNumCols(npl))) ||
	    (MultiListFont(cpl) != MultiListFont(npl)) ||
	    (MultiListXftFont(cpl) != MultiListXftFont(npl)))
	{
		recalc = True;
		redraw = True;
	}

	if (MultiListOffset(cpl) != MultiListOffset(npl))
	  redraw = True;


	if (MultiListColWidth(cpl) != MultiListColWidth(npl))
	{
		XtWarning("columnWidth Resource Is Read-Only");
		MultiListColWidth(npl) = MultiListColWidth(cpl);
	}
	if (MultiListRowHeight(cpl) != MultiListRowHeight(npl))
	{
		XtWarning("rowHeight Resource Is Read-Only");
		MultiListRowHeight(npl) = MultiListRowHeight(cpl);
	}

	if (!XtIsRealized((Widget)cpl))
		return(False);
	    else
		return(redraw);
} /* End SetValues */

/*===========================================================================*

                  D A T A    I N I T I A L I Z A T I O N

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	DestroyOldData(mlw)

	This routine frees the internal list item array and sets the
	item count to 0.  This is normally done immediately before
	calling InitializeNewData() to rebuild the internal item
	array from new user specified arrays.

 *---------------------------------------------------------------------------*/

static void DestroyOldData(mlw)
XfwfMultiListWidget mlw;
{
	int i;

	if (MultiListItemArray(mlw) != NULL)	/* Free Old List */
	{
		for (i = 0; i < MultiListNumItems(mlw); i++)
		{
			free(MultiListItemString(MultiListNthItem(mlw,i)));
		}
		free((char *)MultiListItemArray(mlw));
	}
	if (MultiListSelArray(mlw) != NULL)
	    free((char *)MultiListSelArray(mlw));
	MultiListSelArray(mlw) = NULL;
	MultiListNumSelected(mlw) = 0;
	MultiListItemArray(mlw) = NULL;
	MultiListNumItems(mlw) = 0;
} /* End DestroyOldData */


/*---------------------------------------------------------------------------*

	InitializeNewData(mlw)

	This routine takes a MultiList widget <mlw> and builds up new
	data item tables based on the string list and the sensitivity array.
	All previous data should have already been freed.  If the number
	of items is 0, they will be counted, so the array must be NULL
	terminated.  If the list of strings is NULL, this is treated as
	a list of 0 elements.  If the sensitivity array is NULL, all
	items are treated as sensitive.

	When this routine is done, the string list and sensitivity array
	fields will all be set to NULL, and the widget will not reference
	them again.

 *---------------------------------------------------------------------------*/

static void InitializeNewData(mlw)
XfwfMultiListWidget mlw;
{
	int i;
	XfwfMultiListItem *item;
	String *string_array;

	string_array = MultiListList(mlw);
	if (string_array == NULL) MultiListNumItems(mlw) = 0;

	if (MultiListNumItems(mlw) == 0)		/* Count Elements */
	{
		if (string_array == NULL)		/* No elements */
		{
			MultiListNumItems(mlw) = 0;
		}
		    else
		{
			for (i = 0; string_array[i] != NULL; i++);
			MultiListNumItems(mlw) = i;
		}
	}
	if (MultiListNumItems(mlw) == 0)		/* No Items */
	{
		MultiListItemArray(mlw) = NULL;
	}
	    else
	{
		MultiListItemArray(mlw) =
			TypeAlloc(XfwfMultiListItem,MultiListNumItems(mlw));
		for (i = 0; i < MultiListNumItems(mlw); i++)
		{
			item = MultiListNthItem(mlw,i);
			if (MultiListSensitiveArray(mlw) == NULL ||
			    (MultiListSensitiveArray(mlw)[i] == True))
			{
				MultiListItemSensitive(item) = True;
			}
			    else
			{
				MultiListItemSensitive(item) = False;
			}
			MultiListItemString(item) = StrCopy(string_array[i]);
			MultiListItemHighlighted(item) = False;
		}
	}
	if (MultiListMaxSelectable(mlw) == 0)
	{
		MultiListSelArray(mlw) = NULL;
		MultiListNumSelected(mlw) = 0;
	}
	    else
	{
		MultiListSelArray(mlw) =
			TypeAlloc(int,MultiListMaxSelectable(mlw));
		MultiListNumSelected(mlw) = 0;
	}

	MultiListList(mlw) = NULL;
	MultiListSensitiveArray(mlw) = NULL;
} /* End InitializeNewData */
		

/*---------------------------------------------------------------------------*

	CreateNewGCs(mlw)

	This routine takes a MultiList widget <mlw> and creates a new set of
	graphic contexts for the widget based on the colors, fonts, etc.
	in the widget.  Any previous GCs are assumed to have already been
	destroyed.

 *---------------------------------------------------------------------------*/
extern Boolean  get_scaled_color(Widget,float ,Pixel ,Pixel *);

static void CreateNewGCs(mlw)
XfwfMultiListWidget mlw;
{
	XGCValues values;
	unsigned int attribs;

	attribs = GCForeground | GCBackground;
	values.foreground = MultiListFG(mlw);
	values.background = MultiListBG(mlw);
	if (MultiListFont(mlw)) {
	  values.font = MultiListFont(mlw)->fid;
	  attribs |= GCFont;
	}
	MultiListDrawGC(mlw) = XtGetGC((Widget)mlw,attribs,&values);

	values.foreground = MultiListBG(mlw);
	MultiListEraseGC(mlw) = XtGetGC((Widget)mlw,attribs,&values);

	values.foreground = MultiListHighlightFG(mlw);
	values.background = MultiListHighlightBG(mlw);
	MultiListHighlightForeGC(mlw) = XtGetGC((Widget)mlw,attribs,&values);

	values.foreground = MultiListHighlightBG(mlw);
	values.background = MultiListHighlightBG(mlw);
	MultiListHighlightBackGC(mlw) = XtGetGC((Widget)mlw,attribs,&values);

	if (wx_enough_colors(XtScreen(mlw))) {
	  get_scaled_color((Widget)mlw, 0.6, MultiListBG(mlw), &values.foreground);
	} else {
	  attribs |= GCTile | GCFillStyle;
	  values.foreground = MultiListFG(mlw);
	  values.background = MultiListBG(mlw);
	  values.fill_style = FillTiled;
	  values.tile = XmuCreateStippledPixmap(XtScreen(mlw),MultiListFG(mlw),
						MultiListBG(mlw),MultiListDepth(mlw));
	}
	MultiListGrayGC(mlw) = XtGetGC((Widget)mlw,attribs,&values);
} /* End CreateNewGCs */

/*===========================================================================*

                    R E D R A W    R O U T I N E S

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	RedrawAll(mlw)

	This routine simple calls Redisplay to redraw the entire
	MultiList widget <mlw>.

 *---------------------------------------------------------------------------*/

static void RedrawAll(mlw)
XfwfMultiListWidget mlw;
{
	Redisplay(mlw,NULL,NULL);
} /* End RedrawAll */


/*---------------------------------------------------------------------------*

	RedrawItem(mlw,item_index)

	This routine redraws the item with index <item_index> in the
	MultiList widget <mlw>.  If the item number is bad, nothing is drawn.

 *---------------------------------------------------------------------------*/

static void RedrawItem(mlw,item_index)
XfwfMultiListWidget mlw;
int item_index;
{
	int row,column;

	if (ItemToRowColumn(mlw,item_index,&row,&column))
	{
		RedrawRowColumn(mlw,row,column);
	}
} /* End RedrawItem */


/*---------------------------------------------------------------------------*

	RedrawRowColumn(mlw,row,column)

	This routine paints the item in row/column position <row>,<column>
	on the MultiList widget <mlw>.  If the row/column coordinates are
	outside the widget, nothing is drawn.  If the position is empty,
	blank space is drawn.

 *---------------------------------------------------------------------------*/

static void RedrawRowColumn(mlw,row,column)
XfwfMultiListWidget mlw;
int row,column;
{
  GC bg_gc=0,fg_gc=0;
  XfwfMultiListItem *item=NULL;
  int ul_x,ul_y,str_x,str_y,w,h,item_index,has_item,text_h;
  int xmode;
	
  if (!XtIsRealized((Widget)mlw)) return;
  has_item = RowColumnToItem(mlw,row,column,&item_index);
  RowColumnToPixels(mlw,row,column,&ul_x,&ul_y,&w,&h);

  if (has_item == False)	/* No Item */
    {
      if (MultiListShadeSurplus(mlw))
	bg_gc = MultiListGrayGC(mlw);
      else
	bg_gc = MultiListEraseGC(mlw);
      xmode = 1;
    } else {
      item = MultiListNthItem(mlw,item_index);
      if ((!MultiListSensitive(mlw)) ||
	  (MultiListDrawGray(mlw)) ||
	  (!MultiListItemSensitive(item))) /* Insensitive */
	{
	  if (MultiListItemHighlighted(item)) /* Selected */
	    {
	      bg_gc = MultiListGrayGC(mlw);
	      fg_gc = MultiListEraseGC(mlw);
	      xmode = -1;
	    }
	  else			/* !Selected */
	    {
	      bg_gc = MultiListEraseGC(mlw);
	      fg_gc = MultiListGrayGC(mlw);
	      xmode = 0;
	    }
	}
      else			/* Sensitive */
	{
	  if (MultiListItemHighlighted(item)) /* Selected */
	    {
	      xmode = -1;
	      bg_gc = MultiListHighlightBackGC(mlw);
	      fg_gc = MultiListHighlightForeGC(mlw);
	    }
	  else			/* !Selected */
	    {
	      xmode = 1;
	      bg_gc = MultiListEraseGC(mlw);
	      fg_gc = MultiListDrawGC(mlw);
	    }
	}
    }
  XFillRectangle(XtDisplay(mlw),XtWindow(mlw),bg_gc,ul_x,ul_y,w,h);
  if (has_item == True) {
    text_h = min(FontH(MultiListFont(mlw), MultiListXftFont(mlw)) +
		 (int)MultiListRowSpace(mlw),(int)MultiListRowHeight(mlw));
    str_x = ul_x + MultiListColumnSpace(mlw) / 2;
    str_y = ul_y + FontAscent(MultiListFont(mlw), MultiListXftFont(mlw)) +
      ((int)MultiListRowHeight(mlw) - text_h) / 2;
    XfwfDrawString(XtDisplay(mlw),XtWindow(mlw),
		   (MultiListXftFont(mlw) ? NULL : fg_gc),
		   str_x,str_y,MultiListItemString(item),
		   strlen(MultiListItemString(item)),
		   MultiListTabs(mlw), NULL, 
		   MultiListXftFont(mlw), xmode,
		   0, NULL);
  }
} /* End RedrawRowColumn */
	
/*===========================================================================*

               I T E M    L O C A T I O N    R O U T I N E S

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	void PixelToRowColumn(mlw,x,y,row_ptr,column_ptr)

	This routine takes pixel coordinates <x>, <y> and converts
	the pixel coordinate into a row/column coordinate.  This row/column
	coordinate can then easily be converted into the specific item
	in the list via the function RowColumnToItem().

	If the pixel lies in blank space outside of the items, the
	row & column numbers will be outside of the range of normal
	row & columns numbers, but will correspond to the row & column
	of the item, if an item was actually there.

 *---------------------------------------------------------------------------*/

static void PixelToRowColumn(mlw,x,y,row_ptr,column_ptr)
XfwfMultiListWidget mlw;
int x,y,*row_ptr,*column_ptr;
{
	*row_ptr = (y / (int)MultiListRowHeight(mlw)) + MultiListOffset(mlw);
	*column_ptr = x / (int)MultiListColWidth(mlw);
} /* End PixelToRowColumn */

/*---------------------------------------------------------------------------*

	void RowColumnToPixels(mlw,row,col,x_ptr,y_ptr,w_ptr,h_ptr)

	This routine takes a row/column coordinate <row>,<col> and
	converts it into the bounding pixel rectangle which is returned.

 *---------------------------------------------------------------------------*/

static void RowColumnToPixels(mlw,row,col,x_ptr,y_ptr,w_ptr,h_ptr)
XfwfMultiListWidget mlw;
int row,col,*x_ptr,*y_ptr,*w_ptr,*h_ptr;
{
	*x_ptr = col * MultiListColWidth(mlw);
	*y_ptr = (row - MultiListOffset(mlw)) * MultiListRowHeight(mlw);
	*w_ptr = MultiListColWidth(mlw);
	*h_ptr = MultiListRowHeight(mlw);
} /* End RowColumnToPixels */

/*---------------------------------------------------------------------------*

	Boolean RowColumnToItem(mlw,row,column,item_ptr)

	This routine takes a row number <row> and a column number <column>
	and tries to resolve this row and column into the index of the
	item in this position of the MultiList widget <mlw>.  The resulting
	item index is placed through <item_ptr>.  If there is no item at
	this location, False is returned, else True is returned.

 *---------------------------------------------------------------------------*/

static Boolean RowColumnToItem(mlw,row,column,item_ptr)
XfwfMultiListWidget mlw;
int row,column,*item_ptr;
{
	register int x_stride,y_stride;

	if (row < 0 || row >= MultiListNumRows(mlw) ||
	    column < 0 || column >= MultiListNumCols(mlw))
	{
		return(False);
	}
	x_stride = MultiListNumRows(mlw);
	y_stride = 1;
	*item_ptr = row * y_stride + column * x_stride;
	if (*item_ptr >= MultiListNumItems(mlw))
		return(False);
	    else
		return(True);
} /* End RowColumnToItem */


/*---------------------------------------------------------------------------*

	Boolean ItemToRowColumn(mlw,item_index,row_ptr,column_ptr)

	This routine takes an item number <item_index> and attempts
	to convert the index into row and column numbers stored through
	<row_ptr> and <column_ptr>.  If the item number does not
	corespond to a valid item, False is returned, else True is
	returned.

 *---------------------------------------------------------------------------*/

static Boolean ItemToRowColumn(mlw,item_index,row_ptr,column_ptr)
XfwfMultiListWidget mlw;
int item_index,*row_ptr,*column_ptr;
{
	if (item_index < 0 || item_index >= MultiListNumItems(mlw))
	{
		return(False);
	}
	*row_ptr = item_index % MultiListNumRows(mlw);
	*column_ptr = item_index / MultiListNumRows(mlw);

	return(True);
} /* End ItemToRowColumn */

/*===========================================================================*

                E V E N T    A C T I O N    H A N D L E R S

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	Select(mlw,event,params,num_params)

	This action handler is called when a user selects an item in the
	MultiList.  This action first unselects all previously selected
	items, then selects the item under the mouse, if it is not a
	background gap, and if it is sensitive.

	The MultiListMostRecentItem(mlw) variable will be set to the
	item clicked on, or -1 if the item is background or insensitive.
	The MultiListMostRecentAct(mlw) variable will be set to
	XfwfMultiListActionHighlight, in case the selection region is extended.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Select(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	int click_x,click_y;
	int status,item_index,row,column;

	click_x = event->xbutton.x;
	click_y = event->xbutton.y;
	PixelToRowColumn(mlw,click_x,click_y,&row,&column);
	MultiListMostRecentAct(mlw) = XfwfMultiListActionHighlight;
	status = RowColumnToItem(mlw,row,column,&item_index);
	if ((status == False) ||
	    (!MultiListItemSensitive(MultiListNthItem(mlw,item_index))))
	{
	        MultiListMostRecentAct(mlw) = XfwfMultiListActionNothing;
		MultiListMostRecentItem(mlw) = -1;
	}
	    else
	{
	        XfwfMultiListUnhighlightAll(mlw);
		MultiListMostRecentItem(mlw) = item_index;
		XfwfMultiListHighlightItem(mlw,item_index);
	}
} /* End Select */


/* ARGSUSED */
static void SelectOne(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	int click_x,click_y;
	int status,item_index,row,column;

	click_x = event->xbutton.x;
	click_y = event->xbutton.y;
	PixelToRowColumn(mlw,click_x,click_y,&row,&column);
	MultiListMostRecentAct(mlw) = XfwfMultiListActionHighlight;
	status = RowColumnToItem(mlw,row,column,&item_index);
	if ((status == False) ||
	    (!MultiListItemSensitive(MultiListNthItem(mlw,item_index))))
	{
	        MultiListMostRecentAct(mlw) = XfwfMultiListActionNothing;
		MultiListMostRecentItem(mlw) = -1;
	}
	    else
	{
		MultiListMostRecentItem(mlw) = item_index;
		XfwfMultiListHighlightItem(mlw,item_index);
	}
} /* End Select */


/*---------------------------------------------------------------------------*

	Unselect(mlw,event,params,num_params)

	This function unselects the single text item pointed to by the
	mouse, if any.  Any remaining selected entries are left selected.

	The MultiListMostRecentItem(mlw) variable will be set to -1, and
	the MultiListMostRecentAct(mlw) variable will be set to
	XfwfMultiListActionUnhighlight, in case the deselection region is
	extended.
	
 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Unselect(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	int click_x,click_y;
	int status,item_index,row,column;

	click_x = event->xbutton.x;
	click_y = event->xbutton.y;
	PixelToRowColumn(mlw,click_x,click_y,&row,&column);
	MultiListMostRecentItem(mlw) = -1;
	MultiListMostRecentAct(mlw) = XfwfMultiListActionUnhighlight;
	status = RowColumnToItem(mlw,row,column,&item_index);
	if ((status == True) &&
	    (MultiListItemSensitive(MultiListNthItem(mlw,item_index)))) {
		XfwfMultiListUnhighlightItem(mlw,item_index);
	} else {
	  MultiListMostRecentAct(mlw) = XfwfMultiListActionNothing;
	}
} /* End Unselect */


/*---------------------------------------------------------------------------*

	Toggle(mlw,event,params,num_params)

	This action handler implements the toggling of selection status
	for a single item.  Any remaining selected entries are left selected.

	If the mouse is not over a selectable text item, the
	MultiListMostRecentAct(mlw) variable is set to
	XfwfMultiListActionHighlight, in case the region is extended into
	selectable items later.  MultiListMostRecentItem(mlw) is set to -1.

	If the mouse is over a selectable text item, the item highlight is
	toggled.  If the item is currently selected, it becomes deselected.
	If unselected, the item becomes selected.  At the same time, the
	MultiListMostRecentAct(mlw) variable is set to
	XfwfMultiListActionHighlight if the item was not previously selected,
	or XfwfMultiListActionUnhighlight if the item was previously selected.
	MultiListMostRecentItem(mlw) is set to the index of the item clicked
	on if the item is selected, or -1 if it is unselected.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Toggle(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	int click_x,click_y;
	int status,item_index,row,column;

	click_x = event->xbutton.x;
	click_y = event->xbutton.y;
	PixelToRowColumn(mlw,click_x,click_y,&row,&column);
	status = RowColumnToItem(mlw,row,column,&item_index);
	if ((status == False) ||
	    (!MultiListItemSensitive(MultiListNthItem(mlw,item_index))))
	{
	        MultiListMostRecentAct(mlw) = XfwfMultiListActionNothing;
		MultiListMostRecentItem(mlw) = -1;
	}
	    else
	{
		MultiListMostRecentAct(mlw) =
			XfwfMultiListToggleItem(mlw,item_index);
		MultiListMostRecentItem(mlw) = item_index;
	}
} /* End Toggle */


/*---------------------------------------------------------------------------*

	Extend(mlw,event,params,num_params)

	This action handler implements the extension of a selection/
	deselection region.

	The MultiListMostRecentAct(mlw) variable is used to determine
	if items are to be selected or unselected.  This routine performs
	select or unselect actions on each item it is called on.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Extend(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	int click_x,click_y;
	int status,item_index,row,column;

	click_x = ((XMotionEvent*)event)->x;
	click_y = ((XMotionEvent*)event)->y;
	PixelToRowColumn(mlw,click_x,click_y,&row,&column);
	status = RowColumnToItem(mlw,row,column,&item_index);
	if ((status == True) &&
	    (MultiListItemSensitive(MultiListNthItem(mlw,item_index))))
	{
	  MultiListMostRecentItem(mlw) = item_index;
	  if (MultiListMostRecentAct(mlw) == XfwfMultiListActionHighlight)
	    XfwfMultiListHighlightItem(mlw,item_index);
	  else if (MultiListMostRecentAct(mlw) == XfwfMultiListActionUnhighlight)
	    XfwfMultiListUnhighlightItem(mlw,item_index);
	}
} /* End Extend */


/*---------------------------------------------------------------------------*

	Notify(mlw,event,params,num_params)

	This function performs the Notify action, which issues a callback
	after a selection/unselection has completed.  All callbacks on the
	callback list are invoked, and a XfxfMultiListReturnStruct describing
	the selection state is returned.

	In addition, if the XtNpasteBuffer resource is true and at least one
	text item is selected, all the selected items are placed in the X
	cut buffer (buf(0)) separated by newlines.

 *---------------------------------------------------------------------------*/

/* ARGSUSED */
static void Notify(mlw,event,params,num_params)
XfwfMultiListWidget mlw;
XEvent *event;
String *params;
Cardinal *num_params;
{
	char *buffer;
	String string;
	int i,byte_count,item_index;
	XfwfMultiListReturnStruct ret_value;

	if (MultiListMostRecentAct(mlw) == XfwfMultiListActionNothing)
	  return;

	/* handle double click events using the timestamp of event */
	if (event->xbutton.time - MultiListLastRelease(mlw)
	    < wxGetMultiClickTime(XtDisplay(mlw)))
	{
	    Select(mlw, event, params, num_params);
	    MultiListMostRecentAct(mlw) = XfwfMultiListActionDClick;
	}
	MultiListLastRelease(mlw) = event->xbutton.time;

	if ((MultiListNumSelected(mlw) != 0) && MultiListPaste(mlw))
	{
		byte_count = 0;
		for (i = 0; i < MultiListNumSelected(mlw); i++)
		{
			item_index = MultiListSelArray(mlw)[i];
			string = MultiListItemString(MultiListNthItem(mlw,
				item_index));
			byte_count = byte_count + strlen(string) + 1;
		}
		buffer = (char *)malloc(byte_count);
		buffer[0] = '\0';
		for (i = 0; i < MultiListNumSelected(mlw); i++)
		{
			if (i != 0) strcat(buffer,"\n");
			item_index = MultiListSelArray(mlw)[i];
			string = MultiListItemString(MultiListNthItem(mlw,
				item_index));
			strcat(buffer,string);
		}
		XStoreBytes(XtDisplay(mlw),buffer,byte_count);
		free(buffer);
	}

	ret_value.action = MultiListMostRecentAct(mlw);
	ret_value.item = MultiListMostRecentItem(mlw);
	if (ret_value.item == -1)
		ret_value.string = NULL;
	    else
		ret_value.string = MultiListItemString(MultiListNthItem(mlw,
			ret_value.item));
	ret_value.num_selected = MultiListNumSelected(mlw);
	ret_value.selected_items = MultiListSelArray(mlw);
	XtCallCallbacks((Widget)mlw,XtNcallback,(caddr_t)&ret_value);
} /* End Notify */

/*===========================================================================*

        U S E R    C A L L A B L E    U T I L I T Y    R O U T I N E S

 *===========================================================================*/

/*---------------------------------------------------------------------------*

	Boolean XfwfMultiListHighlightItem(mlw,item_index)

	This routine selects an item with index <item_index> in the
	MultiList widget <mlw>.  If a maximum number of selections is specified
	and exceeded, the earliest selection will be unselected.  If
	<item_index> doesn't correspond to an item the most recently
	clicked item will be set to -1 and this routine will immediately
	return, otherwise the most recently clicked item will be set to the
	current item.  If the clicked on item is not sensitive, or if the
	click is not on an item, False is returned, else True is returned.

 *---------------------------------------------------------------------------*/

Boolean XfwfMultiListHighlightItem(mlw,item_index)
XfwfMultiListWidget mlw;
int item_index;
{
	XfwfMultiListItem *item;

	if (MultiListMaxSelectable(mlw) == 0) return(False);
	if (item_index < 0 || item_index >= MultiListNumItems(mlw))
	{
		MultiListMostRecentItem(mlw) = -1;
		return(False);
	}
	item = MultiListNthItem(mlw,item_index);
	if (MultiListItemSensitive(item) == False) return(False);
	MultiListMostRecentItem(mlw) = item_index;
	if (MultiListItemHighlighted(item) == True) return(True);
	if (MultiListNumSelected(mlw) == MultiListMaxSelectable(mlw))
	{
		XfwfMultiListUnhighlightItem(mlw,MultiListSelArray(mlw)[0]);
	}
	MultiListItemHighlighted(item) = True;
	MultiListSelArray(mlw)[MultiListNumSelected(mlw)] = item_index;
	++ MultiListNumSelected(mlw);
	RedrawItem(mlw,item_index);
	return(True);
} /* End XfwfMultiListHighlightItem */


/*---------------------------------------------------------------------------*

	XfwfMultiListHighlightAll(mlw)

	This routine highlights all highlightable items in the MultiList
	widget <mlw>, up to the maximum number of allowed highlightable
	items;

 *---------------------------------------------------------------------------*/

void XfwfMultiListHighlightAll(mlw)
XfwfMultiListWidget mlw;
{
	int i;
	XfwfMultiListItem *item;

	MultiListNumSelected(mlw) = 0;
	for (i = 0; i < MultiListNumItems(mlw); i++)
	{
		item = MultiListNthItem(mlw,i);
		MultiListItemHighlighted(item) = False;
	}
	for (i = 0; i < MultiListNumItems(mlw); i++)
	{
		if (MultiListNumSelected(mlw) == MultiListMaxSelectable(mlw))
			break;
		item = MultiListNthItem(mlw,i);
		if (MultiListItemSensitive(item) == False) continue;
		MultiListItemHighlighted(item) = True;
		MultiListSelArray(mlw)[MultiListNumSelected(mlw)] = i;
		++ MultiListNumSelected(mlw);
	}
	RedrawAll(mlw);
} /* End XfwfMultiListHighlightAll */


/*---------------------------------------------------------------------------*

	XfwfMultiListUnhighlightItem(mlw,item_index)

	This routine unselects the item with index <item_index> in the
	MultiList widget <mlw>.  If <item_index> doesn't correspond to a
	selected item, then nothing will happen.  Otherwise, the item
	is unselected and the selection array and count are updated.

 *---------------------------------------------------------------------------*/

void XfwfMultiListUnhighlightItem(mlw,item_index)
XfwfMultiListWidget mlw;
int item_index;
{
	int i;
	XfwfMultiListItem *item;

	if (MultiListMaxSelectable(mlw) == 0) return;
	if (item_index < 0 || item_index >= MultiListNumItems(mlw)) return;
	item = MultiListNthItem(mlw,item_index);
	if (MultiListItemHighlighted(item) == False) return;
	MultiListItemHighlighted(item) = False;

	for (i = 0; i < MultiListNumSelected(mlw); i++)
		if (MultiListSelArray(mlw)[i] == item_index) break;
	for (i = i + 1; i < MultiListNumSelected(mlw); i++)
		MultiListSelArray(mlw)[i - 1] = MultiListSelArray(mlw)[i];
	-- MultiListNumSelected(mlw);

	RedrawItem(mlw,item_index);
} /* End XfwfMultiListUnhighlightItem */


/*---------------------------------------------------------------------------*

	XfwfMultiListUnhighlightAll(mlw)

	This routine unhighlights all items in the MultiList widget <mlw>.

 *---------------------------------------------------------------------------*/

void XfwfMultiListUnhighlightAll(mlw)
XfwfMultiListWidget mlw;
{
	int i;
	XfwfMultiListItem *item;

	for (i = 0; i < MultiListNumItems(mlw); i++)
	{
		item = MultiListNthItem(mlw,i);
		if (MultiListItemHighlighted(item))
			XfwfMultiListUnhighlightItem(mlw,i);
	}
	MultiListNumSelected(mlw) = 0;
} /* End XfwfMultiListUnhighlightAll */


/*---------------------------------------------------------------------------*

	int XfwfMultiListToggleItem(mlw,item_index)

	This routine highlights the item with index <item_index>
	if it is unhighlighted and unhighlights it if it is already
	highlighted.  The action performed by the toggle is returned
	(XfwfMultiListActionHighlight or XfwfMultiListActionUnhighlight).

 *---------------------------------------------------------------------------*/

int XfwfMultiListToggleItem(mlw,item_index)
XfwfMultiListWidget mlw;
int item_index;
{
	XfwfMultiListItem *item;

	if (MultiListMaxSelectable(mlw) == 0)
		return(XfwfMultiListActionNothing);
	if (item_index < 0 || item_index >= MultiListNumItems(mlw))
		return(XfwfMultiListActionNothing);
	item = MultiListNthItem(mlw,item_index);
	if (MultiListItemSensitive(item) == False)
		return(XfwfMultiListActionNothing);
	if (MultiListItemHighlighted(item))
	{
		XfwfMultiListUnhighlightItem(mlw,item_index);
		return(XfwfMultiListActionUnhighlight);
	}
	    else
	{
		XfwfMultiListHighlightItem(mlw,item_index);
		return(XfwfMultiListActionHighlight);
	}
} /* End XfwfMultiListToggleItem */


/*---------------------------------------------------------------------------*

	XfwfMultiListReturnStruct *XfwfMultiListGetHighlighted(mlw)

	This routine takes a MultiList widget <mlw> and returns a
	XfwfMultiListReturnStruct whose num_selected and selected_items
	fields contain the highlight information.  The action field
	is set to MULTILIST_ACTION_STATUS, and the item_index and string
	fields are invalid.

 *---------------------------------------------------------------------------*/

XfwfMultiListReturnStruct *XfwfMultiListGetHighlighted(mlw)
XfwfMultiListWidget mlw;
{
	XfwfMultiListItem *item;
	static XfwfMultiListReturnStruct ret_value;

	ret_value.action = XfwfMultiListActionStatus;
	if (MultiListNumSelected(mlw) == 0)
	{
		ret_value.item = -1;
		ret_value.string = NULL;
	}
	    else
	{
		ret_value.item = MultiListSelArray(mlw)
			[MultiListNumSelected(mlw) - 1];
		item = MultiListNthItem(mlw,ret_value.item);
		ret_value.string = MultiListItemString(item);
	}
	ret_value.num_selected = MultiListNumSelected(mlw);
	ret_value.selected_items = MultiListSelArray(mlw);
	return(&ret_value);
} /* End XfwfMultiListGetHighlighted */


/*---------------------------------------------------------------------------*

	Boolean XfwfMultiListIsHighlighted(mlw,item_index)

	This routine checks if the item with index <item_index>
	is highlighted and returns True or False depending.  If
	<item_index> is invalid, False is returned.

 *---------------------------------------------------------------------------*/

Boolean XfwfMultiListIsHighlighted(mlw,item_index)
XfwfMultiListWidget mlw;
int item_index;
{
	XfwfMultiListItem *item;

	if (item_index < 0 || item_index >= MultiListNumItems(mlw))
		return(False);
	item = MultiListNthItem(mlw,item_index);
	return(MultiListItemHighlighted(item));
} /* End XfwfMultiListIsHighlighted */


/*---------------------------------------------------------------------------*

	Boolean XfwfMultiListGetItemInfo(mlw,item_index,str_ptr,h_ptr,s_ptr)

	This routine returns the string, highlight status and
	sensitivity information for the item with index <item_index>
	via the pointers <str_ptr>, <h_ptr> and <s_ptr>.  If the item
	index is invalid, False is returned, else True is returned.

 *---------------------------------------------------------------------------*/

Boolean XfwfMultiListGetItemInfo(mlw,item_index,str_ptr,h_ptr,s_ptr)
XfwfMultiListWidget mlw;
int item_index;
String *str_ptr;
Boolean *h_ptr,*s_ptr;
{
	XfwfMultiListItem *item;

	if (item_index < 0 || item_index >= MultiListNumItems(mlw))
		return(False);
	item = MultiListNthItem(mlw,item_index);
	*str_ptr = MultiListItemString(item);
	*h_ptr = MultiListItemHighlighted(item);
	*s_ptr = MultiListItemSensitive(item);
	return(True);
} /* End XfwfMultiListGetItemInfo */


/*---------------------------------------------------------------------------*

	XfwfMultiListSetNewData(mlw,list,nitems,longest,resize,
		sensitivity_array)

	This routine will set a new set of strings <list> into the
	MultiList widget <mlw>.  If <resize> is True, the MultiList widget will
	try to resize itself.

 *---------------------------------------------------------------------------*/

#if NeedFunctionPrototypes
void
XfwfMultiListSetNewData(XfwfMultiListWidget mlw, String *list,
			int nitems, int longest, Boolean resize,
			Boolean *sensitivity_array)
#else
void
XfwfMultiListSetNewData(mlw,list,nitems,longest,resize,sensitivity_array)
XfwfMultiListWidget mlw;
String *list;
int nitems,longest;
int resize;
Boolean *sensitivity_array;
#endif
{
	DestroyOldData(mlw);
	MultiListList(mlw) = list;
	MultiListNumItems(mlw) = max(nitems,0);
	MultiListSensitiveArray(mlw) = sensitivity_array;
	MultiListNumCols(mlw) = 1;
	MultiListNumRows(mlw) = nitems;

	MultiListColWidth(mlw) = MultiListWidth(mlw);
        MultiListRowHeight(mlw) = FontH(MultiListFont(mlw), MultiListXftFont(mlw));

	if (MultiListNumItems(mlw) == 0) {
	    MultiListList(mlw) = NULL;
	    MultiListSensitiveArray(mlw) = NULL;
	}
	InitializeNewData(mlw);
	if (XtIsRealized((Widget)mlw)) Redisplay(mlw,NULL,NULL);
} /* End XfwfMultiListSetNewData */
