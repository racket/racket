/*
 * File:	wx.h
 * Purpose:	Window library main include file
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_wxh
#define wx_wxh

#include "wx_setup.h"           // Which features to include
#include "common.h"

#include "wx_win.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_buttn.h"
#include "wx_check.h"
#include "wx_choic.h"
#include "wx_messg.h"
#include "wx_lbox.h"
#include "wx_rbox.h"
#include "wx_slidr.h"
#include "wx_gauge.h"
#include "wx_menu.h"
#include "wx_main.h"
#include "wx_stdev.h"
#include "wx_list.h"
#include "wx_gdi.h"
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_cmdlg.h"

#include "wx_wmgr.h"
#include "wx_privt.h"
#include "wx_itemp.h"

/* Windows changes GetStyle to GetStyleA, etc... */
#undef GetStyle
#undef SetStyle
#undef GetFont
#undef SetFont
#undef SetBitmap

#endif // wx_wxh
