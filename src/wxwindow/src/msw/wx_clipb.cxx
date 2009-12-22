/*
 * File:	wx_clipb.cc
 * Purpose:	Clipboard implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_clipb.h"
#include "wx_mf.h"

static WORD PaletteSize (VOID FAR * pv);

Bool wxClipboardIsOpen = FALSE;

Bool wxOpenClipboard(void)
{
  if (wxTheApp->wx_frame && !wxClipboardIsOpen) {
    wxClipboardIsOpen = (Bool)::OpenClipboard(((wxWnd *)wxTheApp->wx_frame->handle)->handle);
    return wxClipboardIsOpen;
  } else
    return FALSE;
}

Bool wxCloseClipboard(void)
{
  if (wxClipboardIsOpen)
    wxClipboardIsOpen = FALSE;
  return (Bool)::CloseClipboard();
}

Bool wxEmptyClipboard(void)
{
  return (Bool)::EmptyClipboard();
}

Bool wxIsClipboardFormatAvailable(int dataFormat)
{
  return ::IsClipboardFormatAvailable(dataFormat);
}

Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width, int height)
{
  switch (dataFormat)
  {
    case wxCF_BITMAP:
    {
      wxBitmap *wxBM = (wxBitmap *)obj;
      HBITMAP hBitmap;
      BITMAP bm;
      HDC hdcMem, hdcSrc;
      HPALETTE origPal = NULL;
      HBITMAP old, old1;
      wxColourMap *cm;
      Bool success;

      hdcMem = CreateCompatibleDC(NULL);
      hdcSrc = CreateCompatibleDC(NULL);
      old = (HBITMAP)::SelectObject(hdcSrc, wxBM->ms_bitmap);

      cm = wxBM->GetColourMap();
      if (cm && cm->ms_palette)
	origPal = SelectPalette(hdcSrc, cm->ms_palette, TRUE);

      GetObject(wxBM->ms_bitmap, sizeof(BITMAP), (LPSTR)&bm);
      hBitmap = CreateBitmapIndirect(&bm);

      if (!hBitmap)
        return FALSE;
      old1 = (HBITMAP)SelectObject(hdcMem, hBitmap);
      BitBlt(hdcMem, 0, 0, wxBM->GetWidth(), wxBM->GetHeight(),
             hdcSrc, 0, 0, SRCCOPY);

      // Select new bitmap out of memory DC
      SelectObject(hdcMem, old1);

      // Set the data
      success = (Bool)::SetClipboardData(CF_BITMAP, hBitmap);

      // Clean up
      if (origPal)
	SelectPalette(hdcSrc, origPal, TRUE);
      SelectObject(hdcSrc, old);
      DeleteDC(hdcSrc);
      DeleteDC(hdcMem);      
      return success;
      break;
    }
    case CF_SYLK:
    case CF_DIF:
    case CF_TIFF:
    case CF_PALETTE:
    case wxCF_DIB:
    {
      return FALSE;
      break;
    }
    case wxCF_OEMTEXT:
      dataFormat = wxCF_TEXT;
    case wxCF_TEXT:
       width = strlen((char *)obj) + 1;
       height = 1;
    default:
    {
      char *s = (char *)obj;
      DWORD l;
      HANDLE hGlobalMemory;
      LPSTR lpGlobalMemory;
      HANDLE success;

      if (dataFormat == wxCF_TEXT) {
	wchar_t *ws;
	ws = wxWIDE_STRING(s);
	width = wx_wstrlen(ws);
	width = sizeof(wchar_t) * (width + 1);
	s = (char *)ws;
	dataFormat = CF_UNICODETEXT;
      }

      l = (width * height);
      hGlobalMemory = GlobalAlloc(GHND, l);
      if (!hGlobalMemory)
        return FALSE;

      lpGlobalMemory = (LPSTR)GlobalLock(hGlobalMemory);

      memcpy(lpGlobalMemory, s, l);

      GlobalUnlock(hGlobalMemory);
      success = SetClipboardData(dataFormat, hGlobalMemory);
      return (Bool)success;
      break;
    }
  }
  return FALSE;
}

wxObject *wxGetClipboardData(int dataFormat, long *len)
{
  switch (dataFormat)
  {
    case wxCF_BITMAP:
    {
      BITMAP bm;
      wxBitmap *wxBM;
      HBITMAP hBitmap;
      HDC hdcMem;
      HDC hdcSrc;
      HBITMAP old;
      HBITMAP hNewBitmap;
      HBITMAP old1;
      HANDLE bits;
      BITMAPINFO *bmi;

      /* I think we should be able to use CF_BITMAP always, but
         it doesn't work right under Windows XP with a particular 
	 image created by copying in Firefox. So, we do things the
         hard way. */
      bits = GetClipboardData(CF_DIB);
      if (bits) {
        bmi = (BITMAPINFO *)GlobalLock(bits);
	
	hBitmap = NULL;
	hdcSrc = NULL;
        old = NULL;
	
	bm.bmBitsPixel = bmi->bmiHeader.biBitCount;
	bm.bmWidth = bmi->bmiHeader.biWidth;
	bm.bmHeight = bmi->bmiHeader.biHeight;
      } else {
	hBitmap = (HBITMAP)GetClipboardData(CF_BITMAP);
        if (!hBitmap)
          return NULL;

        hdcSrc = CreateCompatibleDC(NULL);

        old = (HBITMAP)::SelectObject(hdcSrc, hBitmap);
        GetObject(hBitmap, sizeof(BITMAP), (LPSTR)&bm);

	bmi = NULL;
      }

      hdcMem = CreateCompatibleDC(NULL);
      if (bm.bmBitsPixel == 1)
        hNewBitmap = CreateBitmapIndirect(&bm);
      else
	hNewBitmap = CreateCompatibleBitmap(GetDC(NULL), bm.bmWidth, bm.bmHeight);
      if (!hNewBitmap)
        return NULL;
        
      old1 = (HBITMAP)SelectObject(hdcMem, hNewBitmap);
  	    
      if (bits) {
	int psize;
	psize = PaletteSize(bmi);
        StretchDIBits(hdcMem, 0, 0, bm.bmWidth, bm.bmHeight,
                      0, 0, bm.bmWidth, bm.bmHeight,
		      (char *)bmi XFORM_OK_PLUS bmi->bmiHeader.biSize XFORM_OK_PLUS psize, 
		      bmi, DIB_RGB_COLORS, SRCCOPY);
      } else {
        BitBlt(hdcMem, 0, 0, bm.bmWidth, bm.bmHeight,
               hdcSrc, 0, 0, SRCCOPY);
      }
      
      // Select new bitmap out of memory DC
      SelectObject(hdcMem, old1);
      
      // Clean up
	  if (bits) {
		GlobalUnlock(bits);
	  } else {
        SelectObject(hdcSrc, old);
        DeleteDC(hdcSrc);
	  }

      DeleteDC(hdcMem);

      // Create a new wxBitmap
      wxBM = new wxBitmap;
      wxBM->ms_bitmap = hNewBitmap;
      wxBM->SetWidth(bm.bmWidth);
      wxBM->SetHeight(bm.bmHeight);
      wxBM->SetDepth(-1);
      wxBM->SetOk(TRUE);

      return wxBM;
      break;
    }
    case wxCF_METAFILE:
    case CF_SYLK:
    case CF_DIF:
    case CF_TIFF:
    case CF_PALETTE:
    case wxCF_DIB:
    {
      return FALSE;
      break;
    }
    case wxCF_OEMTEXT:
      dataFormat = wxCF_TEXT;
    case wxCF_TEXT:
    default:
    {
      HANDLE hGlobalMemory;
      int hsize;
      char *s;
      LPSTR lpGlobalMemory;
      
      if (dataFormat == wxCF_TEXT) {
	hGlobalMemory = GetClipboardData(CF_UNICODETEXT);
	if (hGlobalMemory)
	  dataFormat = CF_UNICODETEXT;
      } else
	hGlobalMemory = NULL;
      if (!hGlobalMemory)
	hGlobalMemory = GetClipboardData(dataFormat);
      if (!hGlobalMemory)
        return NULL;

      hsize = (int)GlobalSize(hGlobalMemory);
      if (len)
        *len = hsize;

      s = new char[hsize + 2];
      if (!s)
        return NULL;

      lpGlobalMemory = (LPSTR)GlobalLock(hGlobalMemory);
      memcpy(s, lpGlobalMemory, GlobalSize(hGlobalMemory));
      s[hsize] = 0;
      s[hsize + 1] = 0; /* In case it's Unicode */

      GlobalUnlock(hGlobalMemory);

      if (dataFormat == CF_UNICODETEXT) {
	if (hsize & 0x1) {
	  /* Why is a UTF-16 encoding odd sized? Try
	     dropping a byte. */
	  s[hsize - 1] = 0;
	}
	s = wxNARROW_STRING((wchar_t *)s);
      }

      return (wxObject *)s;
      break;
    }
  }
  return NULL;
}

int  wxEnumClipboardFormats(int dataFormat)
{
  return ::EnumClipboardFormats(dataFormat);
}

int  wxRegisterClipboardFormat(char *formatName)
{
  return ::RegisterClipboardFormat(formatName);
}

Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount)
{
  return (::GetClipboardFormatName(dataFormat, formatName, maxCount) > 0);
}

/*
 * Generalized clipboard implementation by Matthew Flatt
 */

wxClipboard *wxTheClipboard = NULL;

void wxInitClipboard(void)
{
  if (!wxTheClipboard) {
    wxREGGLOB(wxTheClipboard);
    wxTheClipboard = new wxClipboard;
  }
}

 wxClipboardClient::wxClipboardClient()
{
  formats = new wxStringList;
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
}

wxClipboard::~wxClipboard()
{
  if (clipOwner)
    MrEdQueueBeingReplaced(clipOwner);
  if (cbString)
    delete[] cbString;
}

static int FormatStringToID(char *str)
{
  if (!strcmp(str, "TEXT"))
    return wxCF_TEXT;
  
  return wxRegisterClipboardFormat(str);
}

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner) 
    MrEdQueueBeingReplaced(clipOwner);
  clipOwner = client;
  clipOwner->context = wxGetContextForFrame();
  if (cbString) {
    cbString = NULL;
  }

  if (wxOpenClipboard()) {
    char **formats, *data;
    int i, count;
    int ftype;
    long size;

    wxEmptyClipboard();

    formats = clipOwner->formats->ListToArray(FALSE);
	count = clipOwner->formats->Number();
    for (i = 0; i < count; i++) {
      ftype = FormatStringToID(formats[i]);
      data = clipOwner->GetData(formats[i], &size);
      if (!wxSetClipboardData(ftype, (wxObject *)data, size, 1)) {
	got_selection = FALSE;
	break;
      }
    }

    if (i >= count)
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;
  
  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
  }
  if (cbString)
    delete[] cbString;

  cbString = str;

  if (wxOpenClipboard()) {
    wxEmptyClipboard();
    if (!wxSetClipboardData(wxCF_TEXT, (wxObject *)str))
      got_selection = FALSE;
    else
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;

  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str) {
    str = new char[1];
    *str = 0;
  }

  return str;
}

void wxClipboard::SetClipboardBitmap(wxBitmap *bm, long time)
{
  if (clipOwner) {
    MrEdQueueBeingReplaced(clipOwner);
    clipOwner = NULL;
  }
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }
  
  if (wxOpenClipboard()) {
    wxEmptyClipboard();
    wxSetClipboardData(wxCF_BITMAP, bm, 0, 0);
    wxCloseClipboard();
  }
}

wxBitmap *wxClipboard::GetClipboardBitmap(long time)
{
  wxBitmap *bm;

  if (clipOwner || cbString)
    return NULL;

  if (wxOpenClipboard()) {
    bm = (wxBitmap *)wxGetClipboardData(wxCF_BITMAP, NULL);
    wxCloseClipboard();
  } else
    bm = NULL;
  
  return bm;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats->Member(format))
      return clipOwner->GetData(format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
    if (wxOpenClipboard()) {
      char *s;
      s = (char *)wxGetClipboardData(FormatStringToID(format), length);
      receivedString = s;
      wxCloseClipboard();
    } else
      receivedString = NULL;

    return receivedString;
  }
}

/**********************************************************************/

/* Copied from MS example: */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibNumColors(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    : Determines the number of colors in the DIB by looking at   *
 *               the BitCount filed in the info block.                      *
 *                                                                          *
 *  RETURNS    : The number of colors in the DIB.                           *
 *                                                                          *
 ****************************************************************************/

static WORD DibNumColors (VOID FAR * pv)
{
    INT                 bits;
    LPBITMAPINFOHEADER  lpbi;
    LPBITMAPCOREHEADER  lpbc;

    lpbi = ((LPBITMAPINFOHEADER)pv);
    lpbc = ((LPBITMAPCOREHEADER)pv);

    /*  With the BITMAPINFO format headers, the size of the palette
     *  is in biClrUsed, whereas in the BITMAPCORE - style headers, it
     *  is dependent on the bits per pixel ( = 2 raised to the power of
     *  bits/pixel).
     */
    if (lpbi->biSize != sizeof(BITMAPCOREHEADER)){
        if (lpbi->biClrUsed != 0)
            return (WORD)lpbi->biClrUsed;
        bits = lpbi->biBitCount;
    }
    else
        bits = lpbc->bcBitCount;

    switch (bits){
        case 1:
                return 2;
        case 4:
                return 16;
        case 8:
                return 256;
        default:
                /* A 24 bitcount DIB has no color table */
                return 0;
    }
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PaletteSize(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    :  Calculates the palette size in bytes. If the info. block  *
 *                is of the BITMAPCOREHEADER type, the number of colors is  *
 *                multiplied by 3 to give the palette size, otherwise the   *
 *                number of colors is multiplied by 4.                                                          *
 *                                                                          *
 *  RETURNS    :  Palette size in number of bytes.                          *
 *                                                                          *
 ****************************************************************************/
static WORD PaletteSize (VOID FAR * pv)
{
    LPBITMAPINFOHEADER lpbi;
    WORD               NumColors;

    lpbi      = (LPBITMAPINFOHEADER)pv;
    NumColors = DibNumColors(lpbi);

    if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
        return (WORD)(NumColors * sizeof(RGBTRIPLE));
    else
        return (WORD)(NumColors * sizeof(RGBQUAD));
}

