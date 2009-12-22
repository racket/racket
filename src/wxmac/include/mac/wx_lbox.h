/*
 * File:	wx_lbox.h
 * Purpose:	List box panel item
 * Author:	Julian Smart/Cecil Coupe (mac version)
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_lboxh
#define wx_lboxh

#include "wb_lbox.h"
#include "wxLabelArea.h"
/* #include <Lists.h> */
/* begin A List includes */
#include "TheAList.h"
#include "AListOptimizations.h"
#include "LongCoords.h"
/* end A List includes */


#ifdef IN_CPROTO
typedef       void    *wxListBox ;
#else

class wxBitmap;
class wxPanel;
class wxMessage;
class wxList;

// List box item
class wxListBox: public wxbListBox
{
 public:
	wxLabelArea*	cListTitle;
	ALReference		cListReference;
	int				cHaveVScroll;
	int				cKeycnt;		// next key (number)
	wxList*			cDataList;		// List of ClientData(s) per ListBox Entry
	wxArea*			cBorderArea; 	// mflatt: for showing keyboard focus
	wxFont*  label_font;
	
  wxListBox(
  		wxPanel *panel, 
  		wxFunction func, 
  		char *Title,
		Bool Multiple = wxSINGLE|wxNEEDED_SB,
		int x = -1, 
		int y = -1, 
		int width = -1, 
		int height = -1,
		int N = 0, 
		char **Choices = NULL,
		long style = 0, 
	        wxFont *_font = NULL,
	        wxFont *_label_font = NULL,
		char *name = "listBox",
		WXTYPE		objectType = wxTYPE_LIST_BOX
		);
		
  ~wxListBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title, 
  		Bool Multiple = FALSE,
		int x = -1, 
		int y = -1, 
		int width = -1, 
		int height = -1,
		int N = 0, 
		char **Choices = NULL,
		long style = 0, 
		char *name = "listBox"
	);
  void Append(char *Item);
  void Append(char *Item, char *Client_data);
  void Set(int N, char *Choices[]);
  void Clear(void);
  void SetSelection(int N, Bool select = TRUE, Bool just_one = FALSE);
  void SetOneSelection(int N);
  void SetString(int N, char *s);
  // Get client data
  char *GetClientData(int N);
  void SetClientData(int N, char *Client_data);

  void Deselect(int N);

  // For single choice list item only
  int GetSelection(void);
  void Delete(int N);
  
  // For single or multiple choice list item
  int GetSelections(int **list_selections);
  Bool Selected(int N);
  void InsertItems(int nItems, char **Items, int pos);
  // Set the specified item at the first visible item
  // or scroll to max range.
  void SetFirstItem(int N) ;

  wxFont *GetLabelFont() { return label_font; }

  void OnSetFocus(void);
  void OnKillFocus(void);
  
  void DoShow(Bool);
  
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
  virtual void MaybeMoveControls();

  void MoveBox(int dW, int dH, int dX, int dY);

  char *GetLabel(void);
  void SetLabel(char *label);
  
  int NumberOfVisibleItems();
  int GetFirstItem();

  virtual Bool WantsFocus(void);
  virtual void InternalGray(int gray);
  virtual void ReleaseCurrentDC(int really = 0);
  virtual void Activate(Bool gray);
protected:

  virtual void ChangeToGray(Bool gray);
};

#endif // IN_CPROTO
#endif // wx_lboxh
