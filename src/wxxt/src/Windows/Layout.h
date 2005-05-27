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

#ifndef Layout_h
#define Layout_h

#ifdef __GNUG__
#pragma interface
#endif

class wxWindow;
class wxLayoutConstraints;

class wxIndividualLayoutConstraint : public wxObject {
public:
    wxIndividualLayoutConstraint(void);

    void  Set(wxRelationship rel, wxWindow **otherWSR, wxEdge otherE,
	      int val = 0, int marg = wxLAYOUT_DEFAULT_MARGIN);
    // Sibling relationships
    inline void  LeftOf(wxWindow **sibling, int marg = wxLAYOUT_DEFAULT_MARGIN)
	{ Set(wxLeftOf, sibling, wxLeft, 0, marg); }
    inline void  RightOf(wxWindow **sibling, int marg = wxLAYOUT_DEFAULT_MARGIN)
	{ Set(wxRightOf, sibling, wxRight, 0, marg); }
    inline void  Above(wxWindow **sibling, int marg = wxLAYOUT_DEFAULT_MARGIN)
	{ Set(wxAbove, sibling, wxTop, 0, marg); }
    inline void  Below(wxWindow **sibling, int marg = wxLAYOUT_DEFAULT_MARGIN)
	{ Set(wxBelow, sibling, wxBottom, 0, marg); }
    // 'Same edge' alignment
    inline void  SameAs(wxWindow **otherW, wxEdge edge, int marg=wxLAYOUT_DEFAULT_MARGIN)
	{ Set(wxPercentOf, otherW, edge, 0, marg); percent = 100; }
    // The edge is a percentage of the other window's edge
    void  PercentOf(wxWindow **otherWSR, wxEdge wh, int per);
    // Edge has absolute value
    inline void Absolute(int val)
	{ value = val; relationship = wxAbsolute; }
    // Dimension is unconstrained
    inline void Unconstrained(void)
	{ relationship = wxUnconstrained; }
    // Dimension is 'as is' (use current size settings)
    inline void AsIs(void)
	{ relationship = wxAsIs; }
    // Try to satisfy constraint
    Bool SatisfyConstraint(wxLayoutConstraints *constraints, wxWindow *win);
    // Get the value of this edge or dimension, or if this
    // is not determinable, -1.
    int GetEdge(wxEdge which, wxWindow *thisWin, wxWindow *other);
private:
    friend class wxLayoutConstraints;
    friend class wxWindow;

    wxWindow	**otherWinSR;	// parent or sibling of 'this' window
    wxEdge	otherEdge;	// edge of parent or sibling

    wxEdge	   myEdge;	// constraints for this edge
    wxRelationship relationship;// relationship to otherWin
    int		   margin;	// margin to otherWin
    int		   value;	// value of this contraint
    int		   percent;	// percent of otherWin's edge
    Bool	   done;	// used for wxDoLayout
};

#ifdef MZ_PRECISE_GC
# define wxLC_DECL(x) *x
# define wxLC_MEM(x, m) x->m
# define wxLC_ADDR(x) x
#else
# define wxLC_DECL(x) x
# define wxLC_MEM(x, m) x.m
# define wxLC_ADDR(x) &x
#endif

class wxLayoutConstraints : public wxObject {
public:
    wxLayoutConstraints(void);

    // satisfy all individual constraints
    Bool SatisfyConstraints(wxWindow *child);
    void UnDone(void);

    // Edge constraints
    wxIndividualLayoutConstraint wxLC_DECL(left);
    wxIndividualLayoutConstraint wxLC_DECL(top);
    wxIndividualLayoutConstraint wxLC_DECL(right);
    wxIndividualLayoutConstraint wxLC_DECL(bottom);
    // Size constraints
    wxIndividualLayoutConstraint wxLC_DECL(width);
    wxIndividualLayoutConstraint wxLC_DECL(height);
    // Centre constraints
    wxIndividualLayoutConstraint wxLC_DECL(centreX);
    wxIndividualLayoutConstraint wxLC_DECL(centreY);
};

#endif // Layout_h
