/* $Id: widgets.h,v 1.2 2000/09/13 22:56:07 mflatt Exp $ */

/*
 * Main include file for widgets used by the Xt port of wxWindows
 */

#include <X11/StringDefs.h>
#ifdef Uses_ArrowWidget
#include "xwArrow.h"
#endif
#ifdef Uses_AsciiTextWidget
#include <X11/Xaw/AsciiText.h>
#endif
#ifdef Uses_BoardWidget
#include "xwBoard.h"
#endif
#ifdef Uses_ButtonWidget
#include "xwButton.h"
#endif
#ifdef Uses_CanvasWidget
#include "xwCanvas.h"
#endif
#ifdef Uses_EnforcerWidget
#include "xwEnforcer.h"
#endif
#ifdef Uses_TraversingEnforcerWidget
#include "xwEnforcer.h"
#define xfwfTraversingEnforcerWidgetClass xfwfEnforcerWidgetClass
#endif
#ifdef Uses_GroupWidget
#include "xwGroup.h"
#endif
#ifdef Uses_LabelWidget
#include "xwLabel.h"
#endif
#ifdef Uses_MenuWidget
#include "xwMenu.h"
#endif
#ifdef Uses_MultiListWidget
#include "xwMultiList.h"
#endif
#ifdef Uses_ScrollTextWidget
#include "xwScrollText.h"
#include <X11/Xaw/AsciiText.h>
#endif
#ifdef Uses_ScrollWinWidget
#include "xwScrollWin.h"
#include "xwscroll.h"
#endif
#ifdef Uses_Scrollbar
#include "xwScrollbar.h"
#endif
#ifdef Uses_SimpleWidget
#include <X11/Xaw/Simple.h>
#endif
#ifdef Uses_ShellWidget
#include <X11/Shell.h>
#endif
#ifdef Uses_SliderWidget
#include "xwSlider2.h"
#include "xwscroll.h"
#endif
#ifdef Uses_ToggleWidget
#include "xwToggle.h"
#endif
