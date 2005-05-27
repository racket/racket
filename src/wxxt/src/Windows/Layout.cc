/*								-*- C++ -*-
 *
 * Purpose: layout classes
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "Layout.h"
#endif

#define  Uses_wxLayout
#define  Uses_wxWindow
#define  Uses_wxPanel
#define  Uses_wxFrame
#define  Uses_wxTypeTree
#include "wx.h"

//-----------------------------------------------------------------------------
// wxLayoutConstraint
//-----------------------------------------------------------------------------

wxLayoutConstraints::wxLayoutConstraints(void)
: wxObject(FALSE)
{
    __type = wxTYPE_CONSTRAINTS;

#ifdef MZ_PRECISE_GC
    left = new wxIndividualLayoutConstraint;
    top = new wxIndividualLayoutConstraint;
    right = new wxIndividualLayoutConstraint;
    bottom = new wxIndividualLayoutConstraint;
    centreX = new wxIndividualLayoutConstraint;
    centreY = new wxIndividualLayoutConstraint;
    width = new wxIndividualLayoutConstraint;
    height = new wxIndividualLayoutConstraint;
#endif

    wxLC_MEM(left, myEdge)    = wxLeft;
    wxLC_MEM(top, myEdge)     = wxTop;
    wxLC_MEM(right, myEdge)   = wxRight;
    wxLC_MEM(bottom, myEdge)  = wxBottom;
    wxLC_MEM(centreX, myEdge) = wxCentreX;
    wxLC_MEM(centreY, myEdge) = wxCentreY;
    wxLC_MEM(width, myEdge)   = wxWidth;
    wxLC_MEM(height, myEdge)  = wxHeight;
}

Bool wxLayoutConstraints::SatisfyConstraints(wxWindow *child)
{
    Bool changes = FALSE;

    if (!wxLC_MEM(width, done))
	changes |= wxLC_MEM(width, SatisfyConstraint(this, child));
    if (!wxLC_MEM(height, done))
	changes |= wxLC_MEM(height, SatisfyConstraint(this, child));
    if (!wxLC_MEM(left, done))
	changes |= wxLC_MEM(left, SatisfyConstraint(this, child));
    if (!wxLC_MEM(top, done))
	changes |= wxLC_MEM(top, SatisfyConstraint(this, child));
    if (!wxLC_MEM(right, done))
	changes |= wxLC_MEM(right, SatisfyConstraint(this, child));
    if (!wxLC_MEM(bottom, done))
	changes |= wxLC_MEM(bottom, SatisfyConstraint(this, child));
    if (!wxLC_MEM(centreX, done))
	changes |= wxLC_MEM(centreX, SatisfyConstraint(this, child));
    if (!wxLC_MEM(centreY, done))
	changes |= wxLC_MEM(centreY, SatisfyConstraint(this, child));

    return changes;
}

void wxLayoutConstraints::UnDone(void)
{
    wxLC_MEM(left, done)    = FALSE;
    wxLC_MEM(top, done)     = FALSE;
    wxLC_MEM(right, done)   = FALSE;
    wxLC_MEM(bottom, done)  = FALSE;
    wxLC_MEM(centreX, done) = FALSE;
    wxLC_MEM(centreY, done) = FALSE;
    wxLC_MEM(width, done)   = FALSE;
    wxLC_MEM(height, done)  = FALSE;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint
//-----------------------------------------------------------------------------

wxIndividualLayoutConstraint::wxIndividualLayoutConstraint(void)
  : wxObject(FALSE)
{
    otherWinSR	 = NULL;
    otherEdge	 = wxTop;
    myEdge	 = wxTop;
    relationship = wxUnconstrained;
    margin = value = percent = 0;
    done	 = FALSE;
}

void wxIndividualLayoutConstraint::Set(wxRelationship rel, wxWindow **otherW,
				       wxEdge otherE, int val, int marg)
{
    relationship = rel;
    otherWinSR   = otherW;
    otherEdge    = otherE;
    value        = val;
    margin       = marg;
}

void wxIndividualLayoutConstraint::PercentOf(wxWindow **otherW, wxEdge wh, int per)
{
    otherWinSR = otherW;
    relationship = wxPercentOf;
    percent = per;
    otherEdge = wh;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint: Try to satisfy constraint
//-----------------------------------------------------------------------------

Bool wxIndividualLayoutConstraint::SatisfyConstraint(wxLayoutConstraints *constraints,
						     wxWindow *win)
{
    int edge_pos;
    wxWindow *otherWin;

    if (relationship == wxAbsolute || done == TRUE) {
	return (done = TRUE);
    }

    otherWin = (otherWinSR ? (wxWindow *)GET_SAFEREF(otherWinSR) : NULL);

    edge_pos = (win && otherWin) ? GetEdge(otherEdge, win, otherWin) : -1;
    switch (myEdge) {
    case wxLeft:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->right, done) && wxLC_MEM(constraints->width, done)) {
		// compute using right edge and width
		value = wxLC_MEM(constraints->right, value) - wxLC_MEM(constraints->width, value) + margin;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->centreX, done) && wxLC_MEM(constraints->width, done)) {
		// compute using centreX and width
		value = (int)(wxLC_MEM(constraints->centreX, value) - (wxLC_MEM(constraints->width, value)/2)
			      + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxRight:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) - margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->left, done) && wxLC_MEM(constraints->width, done)) {
		// compute using left edge and width
		value = wxLC_MEM(constraints->left, value) + wxLC_MEM(constraints->width, value) - margin;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->centreX, done) && wxLC_MEM(constraints->width, done)) {
		// compute using centreX and width
		value = (int)(wxLC_MEM(constraints->centreX, value) + (wxLC_MEM(constraints->width, value)/2)
			      - margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxTop:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->bottom, done) && wxLC_MEM(constraints->height, done)) {
		// compute using bottom edge and height
		value = wxLC_MEM(constraints->bottom, value) - wxLC_MEM(constraints->height, value) + margin;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->centreY, done) && wxLC_MEM(constraints->height, done)) {
		// compute using centreY and height
		value = (int)(wxLC_MEM(constraints->centreY, value) - (wxLC_MEM(constraints->height, value)/2)
			      + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxBottom:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) - margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->top, done) && wxLC_MEM(constraints->height, done)) {
		// compute using top edge and height
		value = wxLC_MEM(constraints->top, value) + wxLC_MEM(constraints->height, value) - margin;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->centreY, done) && wxLC_MEM(constraints->height, done)) {
		// compute using centreY and height
		value = (int)(wxLC_MEM(constraints->centreY, value) + (wxLC_MEM(constraints->height, value)/2)
			      - margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxCentreX:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->left, done) && wxLC_MEM(constraints->width, done)) {
		// compute using left edge and width
		value = (int)(wxLC_MEM(constraints->left, value) + (wxLC_MEM(constraints->width, value)/2)
			      + margin);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->right, done) && wxLC_MEM(constraints->width, done)) {
		// compute using right edge and width
		value = (int)(wxLC_MEM(constraints->right, value) - (wxLC_MEM(constraints->width, value)/2)
			      + margin);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->left, done) && wxLC_MEM(constraints->right, done)) {
		// compute using left and right edge
		value = (int)(wxLC_MEM(constraints->left, value) 
			      + (wxLC_MEM(constraints->right, value)-wxLC_MEM(constraints->left, value))/2
			    + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxCentreY:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->top, done) && wxLC_MEM(constraints->height, done)) {
		// compute using top edge and height
		value = (int)(wxLC_MEM(constraints->top, value) + (wxLC_MEM(constraints->height, value)/2)
			      + margin);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->bottom, done) && wxLC_MEM(constraints->height, done)) {
		// compute using bottom edge and height
		value = (int)(wxLC_MEM(constraints->bottom, value) - (wxLC_MEM(constraints->height, value)/2)
			      + margin);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->top, done) && wxLC_MEM(constraints->bottom, done)) {
		// compute using top and bottom edge
		value = (int)(wxLC_MEM(constraints->top, value) 
			      + (wxLC_MEM(constraints->bottom, value)-wxLC_MEM(constraints->top, value))/2
			      + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxWidth:
	switch (relationship) {
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01));
		done = TRUE;
	    }
	    break;
        case wxAsIs:
	    if (win) {
		int h;
		win->GetSize(&value, &h);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->left, done) && wxLC_MEM(constraints->right, done)) {
		// compute using left and right edge
		value = wxLC_MEM(constraints->right, value) - wxLC_MEM(constraints->left, value);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->left, done) && wxLC_MEM(constraints->centreX, done)) {
		// compute using left edge and centreX
		value = (wxLC_MEM(constraints->centreX, value) - wxLC_MEM(constraints->left, value)) * 2;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->right, done) && wxLC_MEM(constraints->centreX, done)) {
		// compute using right edge and centreX
		value = (wxLC_MEM(constraints->right, value) - wxLC_MEM(constraints->centreX, value)) * 2;
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxHeight:
	switch (relationship) {
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01));
		done = TRUE;
	    }
	    break;
        case wxAsIs:
	    if (win) {
		int w;
		win->GetSize(&w, &value);
		done = TRUE;
	    }
	case wxUnconstrained:
	    if (wxLC_MEM(constraints->top, done) && wxLC_MEM(constraints->bottom, done)) {
		// compute using top and bottom edge
		value = wxLC_MEM(constraints->bottom, value) - wxLC_MEM(constraints->top, value);
		done = TRUE;
	    } else if (wxLC_MEM(constraints->top, done) && wxLC_MEM(constraints->centreY, done)) {
		// compute using top edge and centreY
		value = (wxLC_MEM(constraints->centreY, value) - wxLC_MEM(constraints->top, value)) * 2;
		done = TRUE;
	    } else if (wxLC_MEM(constraints->bottom, done) && wxLC_MEM(constraints->centreY, done)) {
		// compute using right edge and centreX
		value = (wxLC_MEM(constraints->bottom, value) - wxLC_MEM(constraints->centreY, value)) * 2;
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    }
    return done;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint: get edge of other if obtainable
//-----------------------------------------------------------------------------

int wxIndividualLayoutConstraint::GetEdge(wxEdge which, wxWindow *thisWin,
					  wxWindow *other)
{
    if (!other)
	return -1;
    if ((wxWindow*)thisWin->GetParent() == other) { // dimension is obtainable immediately
	// Compute size of client area of parent
	int w, h; other->GetClientSize(&w, &h);
	switch (which) {
	case wxLeft: case wxTop:	    return 0;
	case wxRight: case wxWidth:	    return w;
	case wxBottom: case wxHeight:	    return h;
	case wxCentreX:			    return (w/2);
	case wxCentreY:			    return (h/2);
	}
    } else {
	wxLayoutConstraints *constr;
	wxIndividualLayoutConstraint *iconstr = NULL;
	constr = other->GetConstraints();
	switch (which) {
	case wxLeft:	    iconstr = wxLC_ADDR(constr->left); break;
	case wxTop:	    iconstr = wxLC_ADDR(constr->top); break;
	case wxRight:	    iconstr = wxLC_ADDR(constr->right); break;
	case wxBottom:	    iconstr = wxLC_ADDR(constr->bottom); break;
	case wxWidth:	    iconstr = wxLC_ADDR(constr->width); break;
	case wxHeight:	    iconstr = wxLC_ADDR(constr->height); break;
	case wxCentreX:	    iconstr = wxLC_ADDR(constr->centreX); break;
	case wxCentreY:	    iconstr = wxLC_ADDR(constr->centreY); break;
	}
	if (iconstr->done)
	    return (iconstr->value);
    }
    return -1;
}

//-----------------------------------------------------------------------------
// do layout
//-----------------------------------------------------------------------------

void wxWindow::Layout(void)
{
    /*
     * Main constrained layout algorithm. Look at all the child
     * windows, and their constraints (if any).
     * The idea is to keep iterating through the constraints
     * until all left, right, bottom and top edges, and widths and heights,
     * are known (or no change occurs and we've failed to resolve all
     * constraints).
     *
     * If the user has not specified a dimension or edge, it will be
     * be calculated from the other known values. E.g. If we know
     * the right hand edge and the left hand edge, we now know the width.
     * The snag here is that this means we must specify absolute dimensions
     * twice (in constructor and in constraint), if we wish to use the
     * constraint notation to just set the position, for example.
     * Otherwise, if we only set ONE edge and no dimension, it would never
     * find the other edge.
     *
     * Algorithm:
     *
     *    Mark all constraints as not done.
     *
     *    iterations = 0;
     *    until (no change) or (iterations >= max iterations)
     *    {
     *        For each child:
     *        {
     *            Calculate all constraints
     *        }
     *        ++iterations;
     *    }
     *
     *    For each child:
     *        Set each calculated position and size
     *
     */

    wxChildNode *node;
    wxWindow *child;
    wxLayoutConstraints *constr;

    // Layout only if children
    if (children->Number() == 0)
	return;
#ifdef MZ_PRECISE_GC
    if (__type == wxTYPE_MENU_BAR)
      return;
#endif

    // reset all constraints to NOT done
    for (node = children->First(); node; node = node->Next()) {
	child  = (wxWindow *)node->Data();
	if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	constr = child->GetConstraints();
	constr->UnDone();
    }
    // iterate through child until (no changes) || (no left iterations)
    {
      int  left_iterations = wxLAYOUT_MAX_ITERATIONS;
      Bool changes;
      do {
	changes = FALSE;
	for (node = children->First(); node; node = node->Next()) {
	  child  = (wxWindow *)node->Data();
	  if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	  constr = child->GetConstraints();
	  changes |= constr->SatisfyConstraints(child);
	}
      } while (changes && --left_iterations);
    }
    // set sizes and positions as computed above
    for (node = children->First(); node; node = node->Next()) {
	child  = (wxWindow *)node->Data();
	if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	constr = child->GetConstraints();
	if (wxLC_MEM(constr->left, done) && wxLC_MEM(constr->right, done)
	    && wxLC_MEM(constr->width, done) && wxLC_MEM(constr->height, done)) {
	    // Configure calls OnSize()
	    child->Configure(wxLC_MEM(constr->left, value),  wxLC_MEM(constr->top, value),
			     wxLC_MEM(constr->width, value), wxLC_MEM(constr->height, value),
			     wxPOS_USE_MINUS_ONE);
	    // layout child
	    child->Layout();
	}
    }
}

void wxPanel::Layout(void)
{
  /* We can stop layout at this level, because constraints are only
     used for a frame's menu-bar and status-line children. */
  // wxWindow::Layout();
}

void wxFrame::Layout(void)
{
    wxWindow *one_child   = NULL;
    int      num_children = 0;
    wxWindow *child;
    wxChildNode *node;

    // check if frame has only ONE child
    if (children) {
      for (node = children->First(); node; node = node->Next()) {
	child = (wxWindow*)(node->Data());
	if ( child && !wxSubType(child->__type, wxTYPE_FRAME) ) {
	  // skip menubar and status line for computation
	  int i;
	  for (i = 0; i < num_status; i++) {
	    if (child == (wxWindow*)status[i])
	      break;
	  }
	  if (child == (wxWindow*)menubar || i < num_status) {
	    continue;
	  }
	  one_child = child; ++num_children;
	}
      }
    }

    // ONE child shall fit into frame
    if (num_children == 1) {
      int ww, hh;
      GetClientSize(&ww, &hh);
      one_child->SetSize(/* PANEL_HMARGIN */ 0, /* PANEL_VMARGIN */ 0,
			 ww /* -2*PANEL_HMARGIN */, hh /*-2*PANEL_VMARGIN */);
    }

    // layout window (necessary for ONE child too because of menubar and status)
    wxWindow::Layout();
}
