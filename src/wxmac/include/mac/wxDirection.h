///////////////////////////////////////////////////////////////////////////////
// File:	wxDirection.h
// Purpose:	Direction (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxDirectionh
#define wxDirectionh

enum {wxTop = 1, wxLeft = 2, wxBottom = 4, wxRight = 8,
	wxVertical = wxTop | wxBottom,
	wxHorizontal = wxLeft | wxRight,
	wxAll = wxVertical | wxHorizontal};
typedef int Direction;

#endif // wxDirectionh
