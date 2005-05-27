/*
 * File:	wx_setup.h
 * Purpose:	Window library configuration file. Note: you may need to edit
 *              the main makefile after you have edited this.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	June 1995
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_setuph
#define wx_setuph
 
#define USE_POSTSCRIPT  1
                        	// 0 for no PostScript device context
#define USE_AFM_FOR_POSTSCRIPT 1
                                // 1 to use font metric files in GetTextExtent
#define USE_METAFILE    0
                                // 0 for no Metafile and metafile device context
#define USE_RESOURCES   1
                        	// 0 for no wxGetResource/wxWriteResource
#define USE_CLIPBOARD   1
                        	// 0 for no clipboard functions
#define USE_SPLINES     1
                            	// 0 for no splines
#define USE_GAUGE       1
                                    // Define 1 to use Microsoft's gauge (Windows)
                                    // or Bull's gauge (Motif) library (both in contrib).
#define USE_XPM_IN_MSW  1
                                    // Define 1 to support the XPM package in wxBitmap,
                                    // separated by platform. If 1, you must link in
                                    // the XPM library to your applications.
#define USE_IMAGE_LOADING_IN_MSW        1
                                  // Use dynamic DIB loading/saving code in utils/dib under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with dib.lib.
#define USE_RESOURCE_LOADING_IN_MSW     0
                                  // Use dynamic icon/cursor loading/saving code in utils/rcparser
                                  // under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with rcparser.lib.
#define REMOVE_UNUSED_ARG 1
                                  // Set this to 0 if your compiler can't cope
                                  // with omission of prototype parameters.

#define FAFA_LIB                   1
                                    // Define 1 for Fafa lib. Mandatory for pixmapped
                                    // buttons under Windows. If FAFA_LIB is 1
                                    // and CTL3D is 1, all controls except wxButton
                                    // will be CTL3D controls: wxButton will
                                    // be a FAFA button with optional bitmap.
#define USE_COMMON_DIALOGS         1
                                    // On rare occasions (e.g. using DJGPP) may want
                                    // to omit common dialogs
                                    // (e.g. file selector, printer dialog).
                                    // Switching this off also switches off
                                    // the printing architecture and interactive
                                    // wxPrinterDC.
#define USE_GREY_BACKGROUND        1
                                    // If 1, uses grey (gray!) panels
                                    // in FAFA and non-FAFA, non-CTL3D modes.
                                    // I (JACS) think the controls look better
                                    // this way. CTL3D always uses grey panels.

#define USE_BITMAP_MESSAGE         1
                                    // Define 1 to use bitmap messages.
#define USE_TYPEDEFS               0
                                    // Use typedefs not classes for wxPoint
				    // and others, to reduce overhead and avoid
				    // MS C7 memory bug. Bounds checker
				    // complains about deallocating
                                    // arrays of wxPoints if wxPoint is a class.


#endif // wx_setuph
