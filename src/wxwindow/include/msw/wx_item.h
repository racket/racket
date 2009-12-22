/*
 * File:	wx_item.h
 * Purpose:	Declares base panel item class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_itemh
#define wx_itemh

#include "wb_item.h"

// General item class
class wxBrush;
class wxFont;
class wxColour;
class wxPanel;
class wxItem: public wxbItem
{
 protected:
   Bool isFafa;      // because we can mix Fafa/non-Fafa controls
 public:
   int *subControls; // For controls like radiobuttons which are really composite
   int numSubControls;
   void **subControlPtrs;

   FARPROC oldWndProc; // For subclassed controls
   Bool isBeingDeleted; // Fudge because can't access parent
                        // when being deleted (don't know why)

   wxItem(wxPanel *panel);
   ~wxItem(void);

   void GetSize(int *width, int *height);
   void GetPosition(int *x, int *y);
   void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
   void SetClientSize(int width, int height);
   void SetFocus(void);
   void SetLabel(char *label);
   char *GetLabel(void);

   Bool Show(Bool show);

   // Windows subclassing
   void SubclassControl(HWND hWnd);
   void UnsubclassControl(HWND hWnd);

   // For ownerdraw items
   virtual Bool MSWOnDraw(DRAWITEMSTRUCT *WXUNUSED(item)) { return FALSE; };
   virtual Bool MSWOnMeasure(MEASUREITEMSTRUCT *WXUNUSED(item)) { return FALSE; };

   void GetLabelExtent(const char *string, double *x, double *y, wxFont *fnt = NULL);

   void SetFont(wxFont *f);
};

long NewId(wxItem *i);
void DoneIds(wxItem *i);

void wxSetWinFont(wxFont *buttonFont, HANDLE ms_handle);

#endif // wx_itemh
