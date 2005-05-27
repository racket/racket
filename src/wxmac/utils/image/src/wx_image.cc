/*
 * File:     wx_image.cc
 * Purpose:  
 *
 *                       wxWindows 1.50
 * Copyright (c) 2004-2005 PLT Scheme, Inc.
 * Copyright (c) 1993 Artificial Intelligence Applications Institute,
 *                   The University of Edinburgh
 *
 *                     Author: Julian Smart
 *                        Date: 7-9-93
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice, author statement and this permission
 * notice appear in all copies of this software and related documentation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
 * IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL THE ARTIFICIAL INTELLIGENCE APPLICATIONS INSTITUTE OR THE
 * UNIVERSITY OF EDINBURGH BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
 * DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "common.h"
#include "wx.h"
#include "wx_image.h"

#ifdef wx_mac
# include "wximgfil.h"
# if USE_XPM_IN_MAC
#   define FOR_MAC
#   include "xpm34.h"
# endif
#endif

// Save (device dependent) wxBitmap as a DIB
Bool wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap)
{
  return FALSE;
}


Bool wxLoadPICTIntoBitmap(char *, wxBitmap *, wxColourMap **);

Bool wxLoadPICTIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
  FILE *fp;
  fp = fopen(fileName,"rb");
  if (fp) {
    // I don't know why we skip 512 bytes. I would have
    // thought fopen only processes the data fork. I suppose
    // it could be the "Mac Draw" header block (IM-V, pg 88)
    int fsize;
    GDHandle savegd;
    CGrafPtr saveport;
    QDErr err;
    Rect	bounds;
    PicHandle ph;

    GetGWorld(&saveport, &savegd);
    
    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp) - 512;
    fseek(fp, 512, SEEK_SET);	// 0 didn't work
    ph = (PicHandle)NewHandle(fsize);
    CheckMemOK(ph);
    fread((char *)*ph, 1, fsize, fp);
    fclose(fp);
    // width = (*ph)->picFrame.right;
    bm->SetWidth((*ph)->picFrame.right);
    // bm->height = (*ph)->picFrame.bottom;
    bm->SetHeight((*ph)->picFrame.bottom);
    // bm->depth = wxDisplayDepth();
    bm->SetDepth(wxDisplayDepth());
    ::SetRect(&bounds, 0, 0, bm->GetHeight(), bm->GetWidth());
    err = NewGWorld(&bm->x_pixmap, 32, &bounds, NULL, NULL, 0);
    if (!err) {
      LockPixels(GetGWorldPixMap(bm->x_pixmap));
      SetGWorld(bm->x_pixmap, 0);
      DrawPicture(ph, &bounds);
      DisposeHandle((Handle)ph);
      SetGWorld(saveport, savegd);
      return TRUE;
    } else {
      bm->x_pixmap = NULL;
      return FALSE;
    }
  } else
    return FALSE;
}


Bool wxLoadXPMIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
  return bm->LoadFile(fileName,wxBITMAP_TYPE_XPM);
} 

wxImage::wxImage(void)
{
  gifpic = NULL;
  dEBUG = 0;
}

wxImage::~wxImage(void)
{
  if (gifpic)
    DELETE_OBJ gifpic;
}

Bool wxImage::Load(char *file)
{
  gifpic = new wxGIF(file);
  if (gifpic)
    return TRUE;
  else
    return FALSE;
}

Bool wxImage::Destroy(void)
{
  return FALSE;
}

void wxImage::Draw(wxCanvas *canvas, int x, int y, int width, int height)
{
}

void wxImage::Resize(int width, int height)
{
}

void wxImage::GetSize(int *width, int *height)
{
  *width = 0;
  *height = 0;
}

wxColourMap *wxImage::GetColourMap(void)
{
  return NULL;
}
