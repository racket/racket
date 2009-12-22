/*
 * File:	wb_win.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/*
 * Purpose:  wxWindow class declaration. Base class for all windows and
 *           panel items.
 */


#ifndef wxb_winh
#define wxb_winh

#include "common.h"
#include "wx_obj.h"
#include "wx_stdev.h"
#include "wx_list.h"

#ifdef IN_CPROTO
typedef       void    *wxFunction ;
typedef       void    *wxbWindow ;
#else

class wxCursor;
class wxFont;
class wxIcon;
class wxColourMap;
class wxMenu;
class wxWindow;

// Callback function type definition
typedef void (*wxFunction)(wxObject *, wxEvent *);

/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 *
 */

class wxWindow;
class wxMenu;
class wxbWindow: public wxObject
{
 protected:
  // Store the window's style
  long windowStyle;
 public:
  int doubleClickAllowed ;
  // Font - created on demand, not deleted with window
  wxFont *font;                               // Window's font
  wxCursor *wx_cursor;                        // Window's cursor

  char *wx_client_data;                       // Any user client data
  Bool paintingEnabled;
  Bool winCaptured;

  char *handle;                                // Pointer to real window
  char *windowName;                            // Window name

  wxFunction callback;                         // Callback associated with the window
  virtual void Callback(wxFunction);           // Adds callback

  // Constructors/Destructors
  wxbWindow(void);
  wxbWindow(char* windowName);

  virtual ~wxbWindow(void);

  inline virtual void Paint(void) {};             // Called when needs painting
  inline virtual void OnSize(int width, int height) {}; // Called on resize
  inline virtual void OnEvent(wxMouseEvent *event) {};  // Called on mouse event
  inline virtual void OnChar(wxKeyEvent *event) {};     // Called on character event
  inline virtual Bool OnClose(void) { return TRUE; };  // Delete window if returns TRUE
  inline virtual void OnActivate(Bool active) {};       // Called on window activation (MSW)
  inline virtual void OnSetFocus(void) {};              // Called on setting focus
  inline virtual void OnKillFocus(void) {};             // Called on killing focus
  inline virtual void OnDropFile(char *files) {};
                                                 // Called when files dropped
  inline virtual void OnCommand(wxWindow *win, wxCommandEvent *event) {};
                                                 // Called if child control has no
                                                 // callback function

  virtual void GetSize(int *width, int *height) = 0;
  virtual void GetPosition(int *x, int *y) = 0;
  virtual void GetClientSize(int *width, int *height) = 0; // Size client can use
  virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO) = 0;
  virtual void SetClientSize(int width, int size) = 0;
  virtual void ClientToScreen(int *x, int *y) = 0;
  virtual void ScreenToClient(int *x, int *y) = 0;
  virtual void Enable(Bool enable) = 0;
  virtual void SetFocus(void) = 0;
  virtual void CaptureMouse(void) = 0;
  virtual void ReleaseMouse(void) = 0;
  virtual void DragAcceptFiles(Bool accept) = 0;
  virtual void MakeModal(Bool modal);

  virtual char *GetHandle(void);
  char *GetClientData(void);
  virtual wxWindow *GetParent(void) = 0;
  virtual wxWindow *GetGrandParent(void) = 0;
  virtual wxChildList *GetChildren(void) = 0;

  void SetClientData(char *);
  virtual void Show(Bool show) = 0;
  virtual wxCursor *SetCursor(wxCursor *cursor) = 0;
  virtual void SetColourMap(wxColourMap *cmap) = 0;

  wxFont *GetFont() { return font; }

  virtual double GetCharWidth(void) = 0;
  virtual double GetCharHeight(void) = 0;
  inline virtual void GetTextExtent(const char* string, double* x, double* y, double* descent = NULL,
				    double* externalLeading = NULL, wxFont* the_font = NULL, Bool use16=FALSE) {};
  inline virtual void SetTitle(char *title) {};      // Set window title
  inline virtual char *GetTitle(void) { return NULL; }; // Set window title
  // Most windows have the concept of a label; for frames, this is the
  // title; for items, this is the label or button text.
  inline virtual char *GetLabel(void) { return GetTitle(); }

  inline virtual char *GetName(void) { return windowName; }
  virtual void SetName(char *name);

  inline virtual void Fit(void) {};                  // Size window to fit contents
  inline virtual void Centre(int direction = wxBOTH, wxWindow *parent = NULL) {};
  // Renamed from GetWindowStyle since it clashed with a
  // macro in windowsx.h.
  inline long GetWindowStyleFlag(void) { return windowStyle; }

  inline virtual void EnablePainting(Bool enable) { paintingEnabled = enable; }

  virtual Bool PopupMenu(wxMenu *menu, double x, double y) = 0;

  // INTERNAL FUNCTIONS
  virtual void AddChild(wxObject *child) = 0;      // Adds reference to the child object
  virtual void DestroyChildren(void) = 0;  		   // Removes and destroys all children

  wxWindow *ContextWindow(void);

//=============================================================================
// Private methods
//=============================================================================
private:

	void InitDefaults(void);

};

extern wxChildList *wxGetTopLevelWindowsList(wxObject*);
#define wxTopLevelWindows(w) (wxGetTopLevelWindowsList(w))

extern void *wxGetContextForFrame();

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxGetModalWindow(wxObject *o);
extern void wxPushModalWindow(wxObject *o, wxWindow *win);
extern void wxPopModalWindow(wxObject *o, wxWindow *win);

#endif // IN_CPROTO
#endif // wxb_winh
