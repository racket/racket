///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollData.h
// Purpose:	ScrollData (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2005 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxScrollDatah
#define wxScrollDatah


/* For the precise-GC transformer, we pretend that wxWhatScrollData is
   atomic. It has no pointers, after all. */
#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

class wxWhatScrollData // 95-02-19
{
  public:
  	enum WhatScrollData
  		{
  			wxOriginX	= 1,
  			wxOriginY	= 2,
  			wxSizeW		= 4,
  			wxSizeH		= 8,
  			wxPositionH	= 16,
  			wxPositionV	= 32,
  			wxUnitW		= 64,
  			wxUnitH		= 128,
  			wxPageW		= 256,
  			wxPageH		= 512,

			wxOrigin	= wxOriginX | wxOriginY,
			wxSize		= wxSizeW | wxSizeH,
			wxPosition	= wxPositionH | wxPositionV,
			wxUnit		= wxUnitW | wxUnitH,
			wxPage		= wxPageW | wxPageH,
			wxAll		= wxOrigin | wxSize | wxPosition | wxUnit | wxPage
  		};

  protected:
	long v;

  public:
	wxWhatScrollData(long w = 0) { v = w; }
	operator WhatScrollData() { return WhatScrollData(v); }
	operator long() { return v; }
	wxWhatScrollData operator |(const wxWhatScrollData& what)
	{
		return this->v | what.v;
	}

	wxWhatScrollData& operator |=(const wxWhatScrollData& what)
	{
		*this = *this | what;
		return *this;
	}
};

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

class wxScrollData
{
  protected:
	int cOriginX;		// min horizontal scroll value
	int cOriginY;		// min vertical scroll value
	int cSizeW;			// total number of horizontal scroll units
	int cSizeH;			// total number of vertical scroll units
	int cPositionH;		// current horizontal scroll position (starting at cOriginX value)
	int cPositionV;		// current vertical scroll position (starting at cOriginY value)
	int cUnitW;			// number of pixels for one horizontal unit scroll
	int cUnitH;			// number of pixels for one vertical unit scroll
	int cPageW;			// number of pixels for one horizontal page scroll
	int cPageH;			// number of pixels for one vertical page scroll

  public:
  	wxScrollData(void);

	wxScrollData
	(
		int					value,			// initial value for selected items
		wxWhatScrollData	whatScrollData	// what items to initialize with given value
	);

  	~wxScrollData(void);

	void SetValue
	(
		wxScrollData* 		newScrollData,	// may have changes only
		wxWhatScrollData 	whatScrollData	// items to be changed
	);

	void SetValue
	(
		int 				value,	// value for items to be changed
		wxWhatScrollData 	whatScrollData	// items to be changed
	);

	int GetValue
	(
		wxWhatScrollData 	whatScrollData	// what item to get
	);

};

#endif // wxScrollDatah
