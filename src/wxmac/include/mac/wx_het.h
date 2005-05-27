
#ifndef WX_HET_H
#define WX_HET_H

extern ControlPartCode wxHETTrackControl(ControlRef theControl, Point startPoint, ControlActionUPP actionProc);
extern void wxHETShowWindow(WindowPtr w);
extern void wxHETShowSheetWindow(WindowPtr w, WindowPtr pw);

extern int wxHETYield(wxWindow *win, int (*f)(void *), void *data);

#endif
