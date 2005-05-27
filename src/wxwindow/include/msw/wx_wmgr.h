
#define wxwmCreateWindowEx ::CreateWindowEx
#define wxwmDestroyWindow ::DestroyWindow
#define wxwmCreateMenu ::CreateMenu
#define wxwmCreatePopupMenu ::CreatePopupMenu
#define wxwmDestroyMenu ::DestroyMenu
#define wxwmGetMessage(m) ::GetMessage(m, NULL, 0, 0)
#define wxwmTranslateMessage ::TranslateMessage
#define wxwmDispatchMessage ::DispatchMessage
#define wxwmTrackPopupMenu ::TrackPopupMenu
#define wxwmSetFocus ::SetFocus
#define wxwmBringWindowToTop ::BringWindowToTop
#define wxwmGetDC ::GetDC
#define wxwmReleaseDC ::ReleaseDC
#define wxwmCreateCompatibleDC ::CreateCompatibleDC

#define wxwmNotify(x, y)

