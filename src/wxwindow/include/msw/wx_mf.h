/*
 * File:	wx_mf.h
 * Purpose:	Metafiles
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_mfh
#define wx_mfh

#include "wx_setup.h"

#if USE_METAFILE
#include "wb_mf.h"

/*
 * Metafile and metafile device context classes - work in Windows 3.1 only
 *
 */

class wxMetaFile: public wxbMetaFile
{
 public:
  HANDLE metafile;

  wxMetaFile(char *file = NULL);
  ~wxMetaFile(void);

  // After this is called, the metafile cannot be used for anything
  // since it is now owned by the clipboard.
  Bool SetClipboard(int width = 0, int height = 0);

  Bool Play(wxDC *dc);
  inline Bool Ok(void) { return metafile != 0; };

};

class wxMetaFileDC: public wxbMetaFileDC
{
 public:
  wxMetaFile *metafile;

  wxMetaFileDC(char *file = NULL);
  ~wxMetaFileDC(void);

  // Should be called at end of drawing
  wxMetaFile *Close(void);
  void SetMapMode(int mode);
  /* MATTHEW: [2] 16-bit flag */
  void GetTextExtent(const char *string, double *x, double *y,
                     double *descent = NULL, double *externalLeading = NULL, 
		     wxFont *theFont = NULL, Bool use16bit = FALSE);
};

/*
 * Pass filename of existing non-placeable metafile, and bounding box.
 * Adds a placeable metafile header, sets the mapping mode to anisotropic,
 * and sets the window origin and extent to mimic the MM_TEXT mapping mode.
 *
 */
 
Bool wxMakeMetaFilePlaceable(char *filename, int x1, int y1, int x2, int y2, double scale = 1.0);

#endif // USE_METAFILE
#endif // wx_mfh
