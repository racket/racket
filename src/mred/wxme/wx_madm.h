/*
 * File:        wx_madm.cc
 * Purpose:     wxMediaAdmins
 * Author:      Matthew Flatt
 * Created:     1997
 * Copyright:   (c) 2004-2005 PLT Scheme, Inc.
 * Copyright:   (c) 1997, Matthew Flatt
 */


#ifndef __WX_MEDIA_ADMIN__
#define __WX_MEDIA_ADMIN__

class wxMediaAdmin : public wxObject
{
 private:
  friend class wxMediaCanvas;
  friend class wxMediaEdit;
  friend class wxMediaBuffer;
  friend class wxCanvasMediaAdmin;
  int standard; /* Used to recognize standard display. Hack. */
  
 public:
  inline wxMediaAdmin();

  /* Usually called by wxMediaBuffer objects: */
  virtual wxDC *GetDC(double *x = NULL, double *y = NULL) = 0;
  virtual void GetView(double *x, double *y, double *w, double *h, 
		       Bool full = FALSE) = 0;
  virtual Bool ScrollTo(double localx, double localy, double w, double h,
			Bool refresh = TRUE, int bias = 0) = 0;
  virtual void GrabCaret(int = wxFOCUS_GLOBAL) = 0;

  virtual void Resized(Bool redraw_now) = 0;
  virtual void NeedsUpdate(double localx, double localy, double w, double h) = 0;

  virtual void UpdateCursor() = 0;

  virtual void GetMaxView(double *x, double *y, double *w, double *h, 
			  Bool full = FALSE);
  virtual Bool DelayRefresh();

  virtual Bool PopupMenu(void *m, double x, double y) = 0;

  virtual void Modified(Bool) = 0;
};

inline wxMediaAdmin::wxMediaAdmin()
     : wxObject(WXGC_NO_CLEANUP)
{
  standard = 0; 
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_ADMIN; 
#endif
};

class wxCanvasMediaAdmin : public wxMediaAdmin
{
 private:
  friend class wxMediaCanvas;
  friend class os_wxCanvasMediaAdmin;
  friend class wxUpdateCursorTimer;

  wxMediaCanvas *canvas;
  wxCanvasMediaAdmin *nextadmin, *prevadmin;
  Bool resetFlag;
  Bool updateBlock, resizedBlock;
  wxUpdateCursorTimer *updateCursorTimer;

  inline void AdjustStdFlag(void);

  wxCanvasMediaAdmin(wxMediaCanvas *c);

 public:
  ~wxCanvasMediaAdmin();

  /* Usually called by wxMediaBuffer objects: */
  wxDC *GetDC(double *x = NULL, double *y = NULL);
  void GetView(double *x, double *y, double *h, double *w, Bool full = FALSE);
  Bool ScrollTo(double localx, double localy, double, double, 
		Bool refresh = TRUE, int bias = 0);

  void GrabCaret(int = wxFOCUS_GLOBAL);

  void Resized(Bool update);
  void NeedsUpdate(double localx, double localy, double w, double h);

  void UpdateCursor();
  void GetMaxView(double *x, double *y, double *h, double *w, Bool full = FALSE);

  Bool PopupMenu(void *m, double x, double y);

  inline wxMediaCanvas *GetCanvas() { return canvas; }

  void Modified(Bool);
};

/* Used by wxMediaSnipMediaAdmin: */
class wxMSMA_SnipDrawState {
 public:
  Bool drawing;
  double x, y;
  wxDC *dc;
};

class wxMediaSnipMediaAdmin : public wxMediaAdmin
{
  friend class wxMediaSnip;
  friend class os_wxMediaSnipMediaAdmin;
  
  wxMSMA_SnipDrawState *state;

  wxMediaSnip *snip;

  void RestoreState(wxMSMA_SnipDrawState *saved);
  void SaveState(wxMSMA_SnipDrawState *save, wxDC *dc, double x, double y);

  wxMediaSnipMediaAdmin(wxMediaSnip *s);

 public:

  /* Only to make wxs_madm.xc happy */
  inline wxMediaSnipMediaAdmin();
  ~wxMediaSnipMediaAdmin();

  wxDC *GetDC(double *x = NULL, double *y = NULL);
  void GetView(double *x, double *y, double *h, double *w, Bool full = FALSE);
  Bool ScrollTo(double localx, double localy, double w, double h,
			Bool refresh = TRUE, int bias = 0);
  void GrabCaret(int = wxFOCUS_GLOBAL);

  void Resized(Bool redraw_now);
  void NeedsUpdate(double localx, double localy, double w, double h);

  void UpdateCursor();

  virtual Bool DelayRefresh();

  Bool PopupMenu(void *m, double x, double y);

  inline wxMediaSnip* GetSnip() { return snip; }

  void Modified(Bool);
};

/* Only to make wxs_madm.xc happy */
inline wxMediaSnipMediaAdmin::wxMediaSnipMediaAdmin()
{
  snip = NULL;
}

#endif /* __WX_MEDIA_ADMIN__ */
