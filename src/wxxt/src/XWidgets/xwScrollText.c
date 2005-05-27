/*
 * Copyright 2004-2005 PLT Scheme, Inc.
 * Copyright 1992, 1994 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <xwScrollTextP.h>

#define CLASS(field) scrollingTextClassRec.scrolling_text_class.field
#define PRIVATE(w,field) (((ScrollingTextWidget) w)->scrollingText.field)

static XtResource resources[] = {
#define Offset(field) XtOffsetOf(ScrollingTextRec, scrollingText.field)
    { XtNscrollOnMovement, XtCScrollOnMovement, XtRBoolean, sizeof(Boolean),
	Offset(scroll_on_movement), XtRImmediate, (XtPointer) True },
    { XtNtextWidget, XtCTextWidget, XtRWidget, sizeof(Widget),
	Offset(text_widget), XtRImmediate, (XtPointer) NULL },
#undef Offset
};

static void ClassInitialize(), Initialize(), Destroy();
static Boolean SetValues();

static void AdjustForMovement(), AdjustForChange();
static Boolean AfterAction();

static char translations[] = "<Key>Return:no-op() \n <Key>Linefeed:no-op()";

ScrollingTextClassRec scrollingTextClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &portholeClassRec,
    /* class_name		*/	"ScrollingText",
    /* widget_size		*/	sizeof(ScrollingTextRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* composite fields */
    /* geometry_manager		*/	XtInheritGeometryManager,
    /* change_managed		*/	XtInheritChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	NULL
  },
  { /* porthole fields */
    /* ignore                   */	0
  },
  { /* scrollingText fields */
    /* extension                   */	NULL
  },
};

WidgetClass scrollingTextWidgetClass = (WidgetClass) &scrollingTextClassRec;

static void
ClassInitialize()
{
     CLASS(translations) = XtParseTranslationTable(translations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    XFontStruct *font;
    Dimension width, height;

    Widget text = PRIVATE(new,text_widget) =
        XtVaCreateManagedWidget("text", asciiTextWidgetClass, new, NULL);

    XtOverrideTranslations(text, CLASS(translations));
    XtAddCallback(XawTextGetSource(text), XtNcallback, AdjustForChange, NULL);

    if (PRIVATE(new,scroll_on_movement))
    {
	PRIVATE(new,action_hook) = 
	    XtAppAddActionHook(XtWidgetToApplicationContext(new),
		                          AdjustForMovement, (XtPointer) text);
    }
    else
    {
	PRIVATE(new,action_hook) = (XtActionHookId) 0;
    }

    XtVaGetValues(text,
		  XtNfont, &font,
		  XtNwidth, &width,
		  XtNheight, &height,
		  NULL);

    PRIVATE(new,font_width) = font->max_bounds.width;

    if (!new->core.width)
    {
	new->core.width = width;
    }

    if (!new->core.height)
    {
	new->core.height = height;
    }

    PRIVATE(new,work_proc) = (XtWorkProcId) 0;
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    if (PRIVATE(old,scroll_on_movement) != PRIVATE(new,scroll_on_movement))
    {
	if (PRIVATE(new,scroll_on_movement))
	{
	    PRIVATE(new,action_hook) = 
		XtAppAddActionHook
		    (XtWidgetToApplicationContext(new),
		     AdjustForMovement, (XtPointer) PRIVATE(new,text_widget));
	}
	else
	{
	    XtRemoveActionHook(PRIVATE(new,action_hook));
	    PRIVATE(new,action_hook) = (XtActionHookId) 0;
	}
    }

    return False;
}

static void
Destroy(w)
     Widget w;
{
    if (PRIVATE(w,action_hook))
    {
	XtRemoveActionHook(PRIVATE(w,action_hook));
    }
}

/* ARGSUSED */
static void
AdjustForMovement(widget, client_data, action, event, params, num_params)
     Widget widget;
     XtPointer client_data;
     String action;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget text = (Widget) client_data;

    if (widget != text)
    {
	return;
    }

    /*
     * This routine gets called before the action takes place so set
     * up a work proc to do the work after the action has taken place
     */
    if (strcmp(action, "forward-character")  == 0 ||
	strcmp(action, "backward-character") == 0 ||
	strcmp(action, "forward-word")       == 0 ||
	strcmp(action, "backward-word")      == 0 ||
	strcmp(action, "beginning-of-line")  == 0 ||
	strcmp(action, "end-of-line")        == 0)
    {
       /*
        * Positioning with the mouse results in work_proc being corrupted with
        * the result that the test fails and scrolling stops happening. I don't
        * understand what is going on. Therefore, for the time being always
        * register a work proc. It is probably unlikely that events will come
        * in fast enough anyway for there to be an unexecuted one still around.

	if (!PRIVATE(text,work_proc))
	{
        */
	    PRIVATE(text,work_proc) =
		XtAppAddWorkProc(XtWidgetToApplicationContext(widget),
				              AfterAction, (XtPointer) widget);
     /* } */
    }
}

static Boolean
AfterAction(client_data)
     XtPointer client_data;
{
    Widget text = (Widget) client_data;


    AdjustForChange(XawTextGetSource(text), (XtPointer) 0, (XtPointer) 0);

    PRIVATE(text,work_proc) = (XtWorkProcId) 0;

    return True;
}

/* ARGSUSED */
static void
AdjustForChange(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget source, sink, text = XtParent(widget), porthole = XtParent(text);
    Position x, prevx, top;
    Dimension lm, width, phwidth, fwidth = PRIVATE(porthole,font_width);
    XawTextPosition pos, eol, junk;
    int pd, ed, morejunk;

    XtVaGetValues(text,
		  XtNtextSource, &source,
		  XtNtextSink, &sink,
		  XtNleftMargin, &lm,
		  XtNx, &prevx,
		  NULL);

    XtVaGetValues(porthole, XtNwidth, &phwidth, NULL);
    top = phwidth - fwidth - fwidth;

    pos = XawTextGetInsertionPoint(text);
    eol = XawTextSourceScan(source, pos, XawstAll, XawsdRight, 1, True);

    /* Calculations of pd and ed take tabs into account */
    XawTextSinkFindDistance(sink, 0, lm, pos, &pd, &junk, &morejunk);
    XawTextSinkFindDistance(sink, 0, lm, eol, &ed, &junk, &morejunk);

    if (pd > top - prevx)
    {
	x =  top - pd;
    }
    else if (pd < fwidth - prevx)
    {
	x = fwidth - pd;
    }
    else
    {
	x = prevx;
    }

    /* Stretch widget to eliminate solid block at end of line */
    width = ed + fwidth;
    
    XtVaSetValues(text, XtNx, x, XtNwidth, width, NULL);
}

#undef PRIVATE
#undef CLASS
