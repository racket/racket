/*
 * File:	wx_setup.h
 * Purpose:	Window library main include file
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_setuph
#define wx_setuph

/*
 * General features
 *
 */
 

#define USE_POSTSCRIPT 1	// 0 for no PostScript device context
#define USE_AFM_FOR_POSTSCRIPT 1 // 1 to use font metric files in GetTextExtent
#define USE_RESOURCES  1	// 0 for no wxGetResource/wxWriteResource
#define USE_CLIPBOARD  1	// 0 for no clipboard functions
#define USE_SPLINES    1	// 0 for no splines
#define USE_XFIG_SPLINE_CODE  1	// 1 for XFIG spline code, 0 for AIAI spline code.
// AIAI spline code is slower, but freer of copyright issues.
#define USE_DRAG_AND_DROP 1	// 0 for no drag and drop

#define USE_GAUGE       1
                                    // Define 1 to use Microsoft's gauge (Windows)
                                    // or Bull's gauge (Motif) library (both in contrib).
                                     
#define USE_XPM_IN_X    0
#define USE_XPM_IN_MSW  0
#define USE_XPM_IN_MAC  1
                                    // Define 1 to support the XPM package in wxBitmap,
                                    // separated by platform. If 1, you must link in
                                    // the XPM library to your applications.
#define USE_IMAGE_LOADING_IN_X          0
                                  // Use dynamic icon/bitmap loading/saving code in utils/image under X.
                                  // If this is 1, you will need to link your applications
                                  // with image_X.lib. where X is motif, ol, or hp.

#define USE_IMAGE_LOADING_IN_MAC        1
                                  // Use dynamic icon/bitmap loading/saving code in utils/image under X.
                                  // If this is 1, you will need to link your applications
                                  // with the objects in utils/image
#define USE_IMAGE_LOADING_IN_MSW        0
                                  // Use dynamic DIB loading/saving code in utils/dib under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with dib.lib.
#define USE_RESOURCE_LOADING_IN_MSW     0
                                  // Use dynamic icon/cursor loading/saving code in utils/rcparser
                                  // under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with rcparser.lib.
#define USE_PRINTING_ARCHITECTURE  1
                                    // Set to 0 to disable print/preview architecture code
#define USE_WX_RESOURCES        0
                                  // Use .wxr resource mechanism (requires PrologIO library)

#define USE_TOOLBAR				0
								  // only really used in wx_mac with wx_extend

#define WXGARBAGE_COLLECTION_ON 1
                                 // Use automatic garbage-collection (mflatt)

/*
 * Finer detail
 *
 */
/*
	Macintosh
*/
#define USE_PANEL_CANVAS_IN_MAC 1
#define USE_MAC_DIALOG_PANEL	1	// Set to 1 for Dialogbox to inherit from Panel, not Frame
//#define USE_MAC_STATUS_SOUTH		// Put status line at bottom of frame if #defined
//#define USE_MAC_GRAY_LABEL_BACKGROUND
/*
 * Motif and XView
 *
 */

#define USE_PANEL_CANVAS_IN_X      1
                                    // wxPanel inherits from wxCanvas in X
 
#define USE_GADGETS                1
                                    // More efficient to use gadgets for some
                                    // widgets in Motif. 0 for no gadgets.
                                    // Please note: there is no reason to not
                                    // use it except if you intend to modify
                                    // color of individuals items.
#define USE_BUTTON_GADGET          0
                                    // On JACS's system, gadget buttons
                                    // interfere with default button setting.
#define PIXEL0_DISABLE             0
                                    // Define as 1 to disallow allocation
                                    // of pixel #0 (wxXOR problem).
                                    // JACS - I found this caused problems.

#define MOTIF_MENUBAR_DELETE_FIX   0
                                    // On some systems (Ultrix, OSF), deleting a frame
                                    // from within a menu callback causes a crash.
                                    // Set to 1 to avoid deleting the menubar handle directly,
                                    // which seems to cure it.
#define	DEFAULT_FILE_SELECTOR_SIZE 0
				    // Let Motif defines the size of File
				    // Selector Box (if 1), or fix it to
				    // wxFSB_WIDTH x wxFSB_HEIGHT (if 0)
#define	wxFSB_WIDTH                600
#define wxFSB_HEIGHT               500

#define USE_NOTICES                1
                                    // Under XView, use Notice package
                                    // for small messages instead of
                                    // home-grown dialog boxes


/*
 * Any platform
 *
 */
#define USE_COMMON_DIALOGS         1
                                    // On rare occasions (e.g. using DJGPP) may want
                                    // to omit common dialogs
                                    // (e.g. file selector, printer dialog).
                                    // Switching this off also switches off
                                    // the printing architecture and interactive
                                    // wxPrinterDC.
#define ENHANCED_FONTS             1
                                    // Define 1 to have predefined fonts in
                                    // wxEnhDialogBox.
#define USE_PANEL_IN_PANEL         0
                                    // 1 to use panel in panel in common dialogs
                                    // Advantage: neater dialogs. Disadvantage:
				    // messes up default button and
				    // tab traversal.
#define USE_TYPEDEFS               0
                                    // Use typedefs not classes for wxPoint
				    // and others, to reduce overhead and avoid
				    // MS C7 memory bug. Bounds checker
				    // complains about deallocating
                                    // arrays of wxPoints if wxPoint is a class.

#endif // wx_setuph

