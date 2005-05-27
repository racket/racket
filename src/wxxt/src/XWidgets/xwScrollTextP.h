/*
 * Copyright 2004-2005 PLT Scheme, Inc.
 * Copyright 1992 The University of Newcastle upon Tyne
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

#ifndef _ScrollingTextP_h
#define _ScrollingTextP_h

#include <X11/Xaw/PortholeP.h>
#include <xwScrollText.h>

typedef struct {
    XtPointer       extension;
    XtTranslations  translations;
} ScrollingTextClassPart;

typedef struct _ScrollingTextClassRec {
    CoreClassPart          core_class;
    CompositeClassPart     composite_class;
    PortholeClassPart      porthole_class;
    ScrollingTextClassPart scrolling_text_class;
} ScrollingTextClassRec;

extern ScrollingTextClassRec scrollingTextClassRec;

typedef struct {			/* new fields in widget */
    /* resources... */
    Boolean         scroll_on_movement;
    Widget          text_widget;

    /* private data... */
    int             font_width;
    XtActionHookId  action_hook;
    XtWorkProcId    work_proc;
} ScrollingTextPart;

typedef struct _ScrollingTextRec {
    CorePart          core;
    CompositePart     composite;
    PortholePart      porthole;
    ScrollingTextPart scrollingText;
} ScrollingTextRec;

#endif /* _ScrollingTextP_h */
