/*******************************************************************************
 *									       *
 * MODULE	: DIB.CC 						       *
 *									       *
 *  DESCRIPTION : Routines for dealing with Device Independent Bitmaps.        *
 *									       *
 *  FUNCTIONS	:                                                              *
 *									       *
 *                ReadDIB()           - Reads a DIB                            *
 *									       *
 *		  WriteDIB()	      - Writes a global handle in CF_DIB format*
 *					to a file.			       *
 *									       *
 *		  PaletteSize()       - Calculates the palette size in bytes   *
 *					of given DIB			       *
 *									       *
 *		  DibNumColors()      - Determines the number of colors in DIB *
 *									       *
 *		  DibFromBitmap()     - Creates a DIB repr. the DDB passed in. *
 *									       *
 *									       *
 *		  lread()	      - Private routine to read more than 64k  *
 *									       *
 *		  lwrite()	      - Private routine to write more than 64k *
 *									       *
 *******************************************************************************/

#include <common.h>
#include <stdio.h>
#include <wx_gdi.h>
#include "dib.h"

#ifndef SEEK_CUR
/* flags for _lseek */
#define	SEEK_CUR 1
#define	SEEK_END 2
#define	SEEK_SET 0
#endif

#define MAXREAD  32768				 /* Number of bytes to be read during */
						 /* each read operation.			 */

#pragma optimize ("", off)

/* Header signatutes for various resources */
#define BFT_ICON	 0x4349	 /* 'IC' */
#define BFT_BITMAP 0x4d42	 /* 'BM' */
#define BFT_CURSOR 0x5450	 /* 'PT' */

/* macro to determine if resource is a DIB */
#define ISDIB(bft) ((bft) == BFT_BITMAP)

/* Macro to align given value to the closest DWORD (unsigned long ) */
#define ALIGNULONG(i)	((i+3)/4*4)

/* Macro to determine to round off the given value to the closest byte */
#define WIDTHBYTES(i)	((i+31)/32*4)

#define PALVERSION		0x300
#define MAXPALETTE	256	  /* max. # supported palette entries */

DWORD PASCAL lread(int fh, VOID FAR *pv, DWORD ul);
DWORD PASCAL lwrite(int fh, VOID FAR *pv, DWORD ul);

BOOL		 WriteDIB (LPSTR szFile,HANDLE hdib);
DWORD		 PaletteSize (VOID FAR * pv);
DWORD		 DibNumColors (VOID FAR * pv);
// HANDLE		 DibFromBitmap (HBITMAP hbm, DWORD biStyle, WORD biBits, HPALETTE hpal);
BOOL             NEAR PASCAL MakeBitmapAndPalette(HDC,HANDLE,HPALETTE *,HBITMAP *);
HPALETTE PASCAL  NEAR MakeDIBPalette(LPBITMAPINFOHEADER);
BOOL             ReadDIB(LPSTR lpFileName, HBITMAP *bitmap, HPALETTE *palette);

/****************************************************************************
 *									    *
 *  FUNCTION   : WriteDIB(LPSTR szFile,HANDLE hdib)			    *
 *									    *
 *  PURPOSE    : Write a global handle in CF_DIB format to a file.	    *
 *									    *
 *  RETURNS    : TRUE  - if successful. 				    *
 *		 FALSE - otherwise					    *
 *									    *
 ****************************************************************************/

BOOL WriteDIB(LPSTR szFile, HANDLE hdib)
{
	BITMAPFILEHEADER	hdr;
	LPBITMAPINFOHEADER  lpbi;
	int                 fh;
	OFSTRUCT            of;

	if (!hdib)
		return FALSE;

	fh = OpenFile(szFile, &of, OF_CREATE | OF_READWRITE);
	if (fh == -1)
		return FALSE;

//	lpbi = (VOID FAR *) GlobalLock(hdib);
        lpbi = (LPBITMAPINFOHEADER) GlobalLock(hdib);
    /* Fill in the fields of the file header */
	hdr.bfType = BFT_BITMAP;
	hdr.bfSize = GlobalSize(hdib) + sizeof(BITMAPFILEHEADER);
	hdr.bfReserved1 = 0;
	hdr.bfReserved2 = 0;
	hdr.bfOffBits = (DWORD) sizeof(BITMAPFILEHEADER) + lpbi->biSize + 
		 PaletteSize(lpbi);

    /* Write the file header */
	_lwrite(fh, (LPSTR) &hdr, sizeof(BITMAPFILEHEADER));

    /* Write the DIB header and the bits */
	lwrite(fh, (LPSTR) lpbi, GlobalSize(hdib));

	GlobalUnlock(hdib);
	_lclose(fh);
	return TRUE;
}

/****************************************************************************
 *									    *
 *  FUNCTION   :  PaletteSize(VOID FAR * pv)				    *
 *									    *
 *  PURPOSE    :  Calculates the palette size in bytes. If the info. block  *
 *		  is of the BITMAPCOREHEADER type, the number of colors is  *
 *		  multiplied by 3 to give the palette size, otherwise the   *
 *		  number of colors is multiplied by 4.								*
 *									    *
 *  RETURNS    :  Palette size in number of bytes.			    *
 *									    *
 ****************************************************************************/

DWORD PaletteSize(VOID FAR * pv)
{
	LPBITMAPINFOHEADER lpbi;
	DWORD	       NumColors;

	lpbi = (LPBITMAPINFOHEADER) pv;
	NumColors = DibNumColors(lpbi);

	if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
		return NumColors * sizeof(RGBTRIPLE);
	else 
		return NumColors * sizeof(RGBQUAD);
}

/****************************************************************************
 *									    *
 *  FUNCTION   : DibNumColors(VOID FAR * pv)				    *
 *									    *
 *  PURPOSE    : Determines the number of colors in the DIB by looking at   *
 *		 the BitCount filed in the info block.			    *
 *									    *
 *  RETURNS    : The number of colors in the DIB.			    *
 *									    *
 ****************************************************************************/

DWORD DibNumColors(VOID FAR *pv)
{
	int 		bits;
	LPBITMAPINFOHEADER	lpbi;
	LPBITMAPCOREHEADER	lpbc;

	lpbi = ((LPBITMAPINFOHEADER) pv);
	lpbc = ((LPBITMAPCOREHEADER) pv);

    /*	With the BITMAPINFO format headers, the size of the palette
     *	is in biClrUsed, whereas in the BITMAPCORE - style headers, it
     *	is dependent on the bits per pixel ( = 2 raised to the power of
     *	bits/pixel).
     */
	if (lpbi->biSize != sizeof(BITMAPCOREHEADER)) {
		if (lpbi->biClrUsed != 0)
			return  lpbi->biClrUsed;
		bits = lpbi->biBitCount;
		}
	else 
		bits = lpbc->bcBitCount;

	switch (bits) {
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
 *									    *
 *  FUNCTION   : DibFromBitmap()					    *
 *									    *
 *  PURPOSE    : Will create a global memory block in DIB format that	    *
 *		 represents the Device-dependent bitmap (DDB) passed in.    *
 *									    *
 *  RETURNS    : A handle to the DIB					    *
 *									    *
 ****************************************************************************/

#if NOTHING
HANDLE DibFromBitmap(HBITMAP hbm, DWORD biStyle, WORD biBits, HPALETTE hpal)
{
	BITMAP               bm;
	BITMAPINFOHEADER     bi;
	BITMAPINFOHEADER FAR *lpbi;
	DWORD                dwLen;
	HANDLE               hdib;
	HANDLE               h;
	HDC                  hdc;

	if (!hbm)
		return NULL;

	if (hpal == NULL)
		hpal = GetStockObject(DEFAULT_PALETTE);

	GetObject(hbm, sizeof (bm), (LPSTR) &bm);

	if (biBits == 0)
		biBits = bm.bmPlanes * bm.bmBitsPixel;

	bi.biSize = sizeof(BITMAPINFOHEADER);
	bi.biWidth = bm.bmWidth;
	bi.biHeight = bm.bmHeight;
	bi.biPlanes = 1;
	bi.biBitCount = biBits;
	bi.biCompression = biStyle;
	bi.biSizeImage = 0;
	bi.biXPelsPerMeter = 0;
	bi.biYPelsPerMeter = 0;
	bi.biClrUsed = 0;
	bi.biClrImportant = 0;

	dwLen = bi.biSize + PaletteSize(&bi);

	hdc = GetDC(NULL);
	hpal = SelectPalette(hdc, hpal, FALSE);
	RealizePalette(hdc);

	hdib = GlobalAlloc(GHND, dwLen);

	if (!hdib) {
		SelectPalette(hdc, hpal, FALSE);
		ReleaseDC(NULL, hdc);
		return NULL;
		}

//	lpbi = (VOID FAR *) GlobalLock(hdib);
	lpbi = (BITMAPINFOHEADER FAR *) GlobalLock(hdib);

	*lpbi = bi;

    /*	call GetDIBits with a NULL lpBits param, so it will calculate the
     *  biSizeImage field for us
     */
	GetDIBits(hdc, hbm, 0,  bi.biHeight,
		 NULL, (LPBITMAPINFO) lpbi, DIB_RGB_COLORS);

	bi = *lpbi;
	GlobalUnlock(hdib);

    /* If the driver did not fill in the biSizeImage field, make one up */
	if (bi.biSizeImage == 0) {
		bi.biSizeImage = WIDTHBYTES((DWORD)bm.bmWidth * biBits) * bm.bmHeight;

		if (biStyle != BI_RGB)
			bi.biSizeImage = (bi.biSizeImage * 3) / 2;
		}

    /*	realloc the buffer big enough to hold all the bits */
	dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;
	if (h = GlobalReAlloc(hdib, dwLen, 0))
		hdib = h;
	else {
		GlobalFree(hdib);
		hdib = NULL;

		SelectPalette(hdc, hpal, FALSE);
		ReleaseDC(NULL, hdc);
		return hdib;
		}

    /*	call GetDIBits with a NON-NULL lpBits param, and actualy get the
     *  bits this time
     */
//	lpbi = (VOID FAR *) GlobalLock(hdib);
	lpbi = (BITMAPINFOHEADER FAR *) GlobalLock(hdib);

	if (GetDIBits(hdc,
		 hbm,
		 0,
		  bi.biHeight,
		 (LPSTR) lpbi + lpbi->biSize + PaletteSize(lpbi),
		 (LPBITMAPINFO) lpbi, DIB_RGB_COLORS) == 0) {
		GlobalUnlock(hdib);
		hdib = NULL;
		SelectPalette(hdc, hpal, FALSE);
		ReleaseDC(NULL, hdc);
		return NULL;
		}

	bi = *lpbi;
	GlobalUnlock(hdib);

	SelectPalette(hdc, hpal, FALSE);
	ReleaseDC(NULL, hdc);
	return hdib;
}
#endif

 /************* PRIVATE ROUTINES TO READ/WRITE MORE THAN 64K ***************/
/****************************************************************************
 *									    *
 *  FUNCTION   : lread(int fh, VOID FAR *pv, DWORD ul)			    *
 *									    *
 *  PURPOSE    : Reads data in steps of 32k till all the data has been read.*
 *									    *
 *  RETURNS    : 0 - If read did not proceed correctly. 		    *
 *		 number of bytes read otherwise.			    *
 *									    *
 ****************************************************************************/

DWORD PASCAL lread(int fh, VOID far *pv, DWORD ul)
{
	DWORD     ulT = ul;
	BYTE *hp = (BYTE *) pv;
	while (ul > (DWORD) MAXREAD) {
		if (_lread(fh, (LPSTR) hp, MAXREAD) != MAXREAD)
			return 0;
		ul -= MAXREAD;
		hp += MAXREAD;
		}
	if (_lread(fh, (LPSTR) hp, ul) != ul)
		return 0;
	return ulT;
}

/****************************************************************************
 *									    *
 *  FUNCTION   : lwrite(int fh, VOID FAR *pv, DWORD ul) 		    *
 *									    *
 *  PURPOSE    : Writes data in steps of 32k till all the data is written.  *
 *									    *
 *  RETURNS    : 0 - If write did not proceed correctly.		    *
 *		 number of bytes written otherwise.			    *
 *									    *
 ****************************************************************************/

DWORD PASCAL lwrite(int fh, VOID FAR *pv, DWORD ul)
{
	DWORD     ulT = ul;
	BYTE *hp = (BYTE *) pv;
	while (ul > MAXREAD) {
		if (_lwrite(fh, (LPSTR) hp, MAXREAD) != MAXREAD)
			return 0;
		ul -= MAXREAD;
		hp += MAXREAD;
		}
	if (_lwrite(fh, (LPSTR) hp, ul) != ul)
		return 0;
	return ulT;
}

/****************************************************************************
 *
 *  FUNCTION   : ReadDIB(hWnd)
 *
 *  PURPOSE    : Reads a DIB from a file, obtains a handle to its
 *		 BITMAPINFO struct. and loads the DIB.  Once the DIB
 *               is loaded, the function also creates a bitmap and
 *               palette out of the DIB for a device-dependent form.
 *
 *  RETURNS    : TRUE  - DIB loaded and bitmap/palette created 
 *                       The DIBINIT structure pointed to by pInfo is
 *                       filled with the appropriate handles.
 *		 FALSE - otherwise
 *
 ****************************************************************************/
BOOL ReadDIB(LPSTR lpFileName, HBITMAP *bitmap, HPALETTE *palette)
{
    int fh;
    LPBITMAPINFOHEADER lpbi;
    OFSTRUCT	       of;
    BITMAPFILEHEADER   bf;
    DWORD		nNumColors;
    BOOL result = FALSE;
    char str[128];
    DWORD offBits, extra_space, got;
    HDC hDC;
    BOOL bCoreHead = FALSE;
    HANDLE hDIB = 0;

    /* Open the file and get a handle to it's BITMAPINFO */

    fh = OpenFile (lpFileName, &of, OF_READ);
    if (fh == -1) {
	// wsprintf(str,"Can't open file '%ls'", (LPSTR)lpFileName);
	// MessageBox(NULL, str, "Error", MB_ICONSTOP | MB_OK);
	return (NULL);
    }

    hDIB = GlobalAlloc(GHND, (DWORD)(sizeof(BITMAPINFOHEADER) + 
    					256 * sizeof(RGBQUAD)));
    if (!hDIB) {
      _lclose(fh);
      return(NULL);
    }

    lpbi = (LPBITMAPINFOHEADER)GlobalLock(hDIB);

    /* read the BITMAPFILEHEADER */
    if (sizeof (bf) != _lread (fh, (LPSTR)&bf, sizeof (bf)))
	goto ErrExit;

    if (bf.bfType != 0x4d42)	/* 'BM' */
	goto ErrExit;

    if (sizeof(BITMAPCOREHEADER) != _lread (fh, (LPSTR)lpbi, sizeof(BITMAPCOREHEADER)))
	goto ErrExit;

    if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
    {
	lpbi->biSize = sizeof(BITMAPINFOHEADER);
	lpbi->biBitCount = ((LPBITMAPCOREHEADER)lpbi)->bcBitCount;
	lpbi->biPlanes = ((LPBITMAPCOREHEADER)lpbi)->bcPlanes;
	lpbi->biHeight = ((LPBITMAPCOREHEADER)lpbi)->bcHeight;
	lpbi->biWidth = ((LPBITMAPCOREHEADER)lpbi)->bcWidth;
	bCoreHead = TRUE;
    }
    else
    {
    	// get to the start of the header and read INFOHEADER
        _llseek(fh,sizeof(BITMAPFILEHEADER),SEEK_SET);
	if (sizeof(BITMAPINFOHEADER) != _lread (fh, (LPSTR)lpbi, sizeof(BITMAPINFOHEADER)))
	    goto ErrExit;
    }

    if (!(nNumColors = lpbi->biClrUsed))
    {
	/* no color table for 24-bit, default size otherwise */
	if (lpbi->biBitCount != 24)
	    nNumColors = 1 << lpbi->biBitCount;	/* standard size table */
    }

    /*	fill in some default values if they are zero */
    if (lpbi->biClrUsed == 0)
	lpbi->biClrUsed = nNumColors;

    /* Always compute the image size. Sometimes the file is wrong. */
    if (1 || (lpbi->biSizeImage == 0))
    {
	lpbi->biSizeImage = ((((lpbi->biWidth * (DWORD)lpbi->biBitCount) + 31) & ~31) >> 3)
			 * lpbi->biHeight;
    }

    /* get a proper-sized buffer for header, color table and bits */ 
    GlobalUnlock(hDIB);
    hDIB = GlobalReAlloc(hDIB, (lpbi->biSize
				+ nNumColors * sizeof(RGBQUAD)
				+ lpbi->biSizeImage), 0);
    if (!hDIB)	/* can't resize buffer for loading */
	goto ErrExit2;

    lpbi = (LPBITMAPINFOHEADER)GlobalLock(hDIB);

    /* read the color table */
    if (!bCoreHead)
	_lread(fh, (LPSTR)(lpbi) + lpbi->biSize, nNumColors * sizeof(RGBQUAD));
    else
    {
	signed int i;
	RGBQUAD FAR *pQuad;
	RGBTRIPLE FAR *pTriple;

	_lread(fh, (LPSTR)(lpbi) + lpbi->biSize, nNumColors * sizeof(RGBTRIPLE));

	pQuad = (RGBQUAD FAR *)((LPSTR)lpbi + lpbi->biSize);
	pTriple = (RGBTRIPLE FAR *) pQuad;
	for (i = nNumColors - 1; i >= 0; i--)
	{
	    pQuad[i].rgbRed = pTriple[i].rgbtRed;
	    pQuad[i].rgbBlue = pTriple[i].rgbtBlue;
	    pQuad[i].rgbGreen = pTriple[i].rgbtGreen;
	    pQuad[i].rgbReserved = 0;
	}
    }

    /* offset to the bits from start of DIB header */
    offBits = lpbi->biSize + nNumColors * sizeof(RGBQUAD);

    if (bf.bfOffBits != 0L)
    {
        _llseek(fh,bf.bfOffBits,SEEK_SET);
    }

    got = lread(fh, (LPSTR)lpbi + offBits, lpbi->biSizeImage);

    if (lpbi->biSizeImage == got)
    {
	GlobalUnlock(hDIB);

	hDC = GetDC(NULL);
	if (!MakeBitmapAndPalette(hDC, hDIB, palette,
					bitmap))
	{
	    ReleaseDC(NULL,hDC);
	    goto ErrExit2;
	}
	else
	{
	    ReleaseDC(NULL,hDC);
            GlobalFree(hDIB);
	    result = TRUE;
	}
    }
    else
    {
ErrExit:
	GlobalUnlock(hDIB);
ErrExit2:
	GlobalFree(hDIB);
    }

    _lclose(fh);
    return(result);
}

/****************************************************************************
 *
 *  FUNCTION   : ReadDIB2(hWnd)
 *
 *  PURPOSE    : Reads a DIB from a file, obtains a handle to its
 *		 BITMAPINFO struct. and loads the DIB. Returns the DIB handle.
 *  Experimental code hacked about by Julian Smart.
 *
 ****************************************************************************/
HANDLE ReadDIB2(LPSTR lpFileName)
{
    int fh;
    LPBITMAPINFOHEADER lpbi;
    OFSTRUCT	       of;
    BITMAPFILEHEADER   bf;
    DWORD		nNumColors;
    BOOL result = FALSE;
    char str[128];
    DWORD offBits;
    HDC hDC;
    BOOL bCoreHead = FALSE;
    HANDLE hDIB = 0;

    /* Open the file and get a handle to it's BITMAPINFO */

    fh = OpenFile (lpFileName, &of, OF_READ);
    if (fh == -1) {
	// wsprintf(str,"Can't open file '%ls'", (LPSTR)lpFileName);
	// MessageBox(NULL, str, "Error", MB_ICONSTOP | MB_OK);
	return (NULL);
    }

    hDIB = GlobalAlloc(GHND, (DWORD)(sizeof(BITMAPINFOHEADER) + 
    					256 * sizeof(RGBQUAD)));
    if (!hDIB)
	return(NULL);

    lpbi = (LPBITMAPINFOHEADER)GlobalLock(hDIB);

    /* read the BITMAPFILEHEADER */
    if (sizeof (bf) != _lread (fh, (LPSTR)&bf, sizeof (bf)))
	goto ErrExit;

    if (bf.bfType != 0x4d42)	/* 'BM' */
	goto ErrExit;

    if (sizeof(BITMAPCOREHEADER) != _lread (fh, (LPSTR)lpbi, sizeof(BITMAPCOREHEADER)))
	goto ErrExit;

    if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
    {
	lpbi->biSize = sizeof(BITMAPINFOHEADER);
	lpbi->biBitCount = ((LPBITMAPCOREHEADER)lpbi)->bcBitCount;
	lpbi->biPlanes = ((LPBITMAPCOREHEADER)lpbi)->bcPlanes;
	lpbi->biHeight = ((LPBITMAPCOREHEADER)lpbi)->bcHeight;
	lpbi->biWidth = ((LPBITMAPCOREHEADER)lpbi)->bcWidth;
	bCoreHead = TRUE;
    }
    else
    {
    	// get to the start of the header and read INFOHEADER
        _llseek(fh,sizeof(BITMAPFILEHEADER),SEEK_SET);
	if (sizeof(BITMAPINFOHEADER) != _lread (fh, (LPSTR)lpbi, sizeof(BITMAPINFOHEADER)))
	    goto ErrExit;
    }

    if (!(nNumColors = lpbi->biClrUsed))
    {
	/* no color table for 24-bit, default size otherwise */
	if (lpbi->biBitCount != 24)
	    nNumColors = 1 << lpbi->biBitCount;	/* standard size table */
    }

    /*	fill in some default values if they are zero */
    if (lpbi->biClrUsed == 0)
	lpbi->biClrUsed = nNumColors;

    if (lpbi->biSizeImage == 0)
    {
	lpbi->biSizeImage = ((((lpbi->biWidth * (DWORD)lpbi->biBitCount) + 31) & ~31) >> 3)
			 * lpbi->biHeight;
    }

    /* get a proper-sized buffer for header, color table and bits */ 
    GlobalUnlock(hDIB);
    hDIB = GlobalReAlloc(hDIB, lpbi->biSize +
    					nNumColors * sizeof(RGBQUAD) +
					lpbi->biSizeImage, 0);
    if (!hDIB)	/* can't resize buffer for loading */
	goto ErrExit2;

    lpbi = (LPBITMAPINFOHEADER)GlobalLock(hDIB);

    /* read the color table */
    if (!bCoreHead)
	_lread(fh, (LPSTR)(lpbi) + lpbi->biSize, nNumColors * sizeof(RGBQUAD));
    else
    {
	signed int i;
	RGBQUAD FAR *pQuad;
	RGBTRIPLE FAR *pTriple;

	_lread(fh, (LPSTR)(lpbi) + lpbi->biSize, nNumColors * sizeof(RGBTRIPLE));

	pQuad = (RGBQUAD FAR *)((LPSTR)lpbi + lpbi->biSize);
	pTriple = (RGBTRIPLE FAR *) pQuad;
	for (i = nNumColors - 1; i >= 0; i--)
	{
	    pQuad[i].rgbRed = pTriple[i].rgbtRed;
	    pQuad[i].rgbBlue = pTriple[i].rgbtBlue;
	    pQuad[i].rgbGreen = pTriple[i].rgbtGreen;
	    pQuad[i].rgbReserved = 0;
	}
    }

    /* offset to the bits from start of DIB header */
    offBits = lpbi->biSize + nNumColors * sizeof(RGBQUAD);

    if (bf.bfOffBits != 0L)
    {
        _llseek(fh,bf.bfOffBits,SEEK_SET);
    }

    if (lpbi->biSizeImage == lread(fh, (LPSTR)lpbi + offBits, lpbi->biSizeImage))
    {
	GlobalUnlock(hDIB);
        return hDIB;
    }
    else
    {
ErrExit:
	GlobalUnlock(hDIB);
ErrExit2:
	GlobalFree(hDIB);
    }

    _lclose(fh);
    return 0;
}

/****************************************************************************
 *
 *  FUNCTION   : MakeBitmapAndPalette
 *
 *  PURPOSE    : Given a DIB, creates a bitmap and corresponding palette
 *               to be used for a device-dependent representation of
 *               of the image.
 *
 *  RETURNS    : TRUE  --> success. phPal and phBitmap are filled with 
 *                         appropriate handles.  Caller is responsible
 *                         for freeing objects.
 *               FALSE --> unable to create objects.  both pointer are
 *                         not valid
 *
 ****************************************************************************/
BOOL NEAR PASCAL MakeBitmapAndPalette(HDC hDC, HANDLE hDIB, 
			HPALETTE * phPal, HBITMAP * phBitmap)
{
    LPBITMAPINFOHEADER lpInfo;
    BOOL result = FALSE;
    HBITMAP hBitmap;
    HPALETTE hPalette, hOldPal;
    LPSTR lpBits;

    lpInfo = (LPBITMAPINFOHEADER) GlobalLock(hDIB);
    if (hPalette = MakeDIBPalette(lpInfo))
    {

#if USE_THE_PALETTE
	// Need to realize palette for converting DIB to bitmap.
	hOldPal = SelectPalette(hDC, hPalette, TRUE);
	RealizePalette(hDC);
#endif


	lpBits = (LPSTR)lpInfo + lpInfo->biSize + 
		lpInfo->biClrUsed * sizeof(RGBQUAD);
	hBitmap = CreateDIBitmap(hDC, lpInfo, CBM_INIT, lpBits, 
				 (LPBITMAPINFO)lpInfo, DIB_RGB_COLORS);

#if USE_THE_PALETTE

	SelectPalette(hDC, hOldPal, TRUE);
	RealizePalette(hDC);
#endif


	if (!hBitmap) {
	  int err;
	  err = GetLastError();
	    DeleteObject(hPalette);
	} else
	{
	    *phBitmap = hBitmap;
	    *phPal = hPalette;
	    result = TRUE;
	}
    }
    return(result);
}

/****************************************************************************
 *									    *
 *  FUNCTION   : MakeDIBPalette(lpInfo)					    *
 *									    *
 *  PURPOSE    : Given a BITMAPINFOHEADER, create a palette based on 
 *		 the color table.
 *		 
 *									    *
 *  RETURNS    : non-zero - handle of a corresponding palette 
 *		 zero - unable to create palette
 *									    *
 ****************************************************************************/
HPALETTE PASCAL NEAR MakeDIBPalette(LPBITMAPINFOHEADER lpInfo)
{
    NPLOGPALETTE npPal;
    RGBQUAD far *lpRGB;
    HPALETTE hLogPal; 
    DWORD i;

    /* since biClrUsed field was filled during the loading of the DIB,
    ** we know it contains the number of colors in the color table.
    */
    if (lpInfo->biClrUsed)
    {
	npPal = (NPLOGPALETTE)LocalAlloc(LMEM_FIXED, sizeof(LOGPALETTE) + 
				lpInfo->biClrUsed * sizeof(PALETTEENTRY));
	if (!npPal)
	    return(FALSE);

	npPal->palVersion = 0x300;
	npPal->palNumEntries = lpInfo->biClrUsed;

	/* get pointer to the color table */
	lpRGB = (RGBQUAD FAR *)((LPSTR)lpInfo + lpInfo->biSize);

	/* copy colors from the color table to the LogPalette structure */
	for (i = 0; i < lpInfo->biClrUsed; i++, lpRGB++)
	{
	    npPal->palPalEntry[i].peRed = lpRGB->rgbRed;
	    npPal->palPalEntry[i].peGreen = lpRGB->rgbGreen;
	    npPal->palPalEntry[i].peBlue = lpRGB->rgbBlue;
	    npPal->palPalEntry[i].peFlags = 0;
	}

	hLogPal = CreatePalette((LPLOGPALETTE)npPal);
	LocalFree((HANDLE)npPal);
	return(hLogPal);
    }

    /* 24-bit DIB with no color table.  return default palette.  Another
    ** option would be to create a 256 color "rainbow" palette to provide
    ** some good color choices.
    */
    else
	return((HPALETTE)GetStockObject(DEFAULT_PALETTE));
}

//---------------------------------------------------------------------
//
// Function:   InitBitmapInfoHeader
//
// Purpose:    Does a "standard" initialization of a BITMAPINFOHEADER,
//             given the Width, Height, and Bits per Pixel for the
//             DIB.
//
//             By standard, I mean that all the relevant fields are set
//             to the specified values.  biSizeImage is computed, the
//             biCompression field is set to "no compression," and all
//             other fields are 0.
//
//             Note that DIBs only allow BitsPixel values of 1, 4, 8, or
//             24.  This routine makes sure that one of these values is
//             used (whichever is most appropriate for the specified
//             nBPP).
//
// Parms:      lpBmInfoHdr == Far pointer to a BITMAPINFOHEADER structure
//                            to be filled in.
//             dwWidth     == Width of DIB (not in Win 3.0 & 3.1, high
//                            word MUST be 0).
//             dwHeight    == Height of DIB (not in Win 3.0 & 3.1, high
//                            word MUST be 0).
//             nBPP        == Bits per Pixel for the DIB.
//
// History:   Date      Reason
//            11/07/91  Created
//             
//---------------------------------------------------------------------

void InitBitmapInfoHeader (LPBITMAPINFOHEADER lpBmInfoHdr,
                                        DWORD dwWidth,
                                        DWORD dwHeight,
                                          int nBPP)
{
//   _fmemset (lpBmInfoHdr, 0, sizeof (BITMAPINFOHEADER));
   memset (lpBmInfoHdr, 0, sizeof (BITMAPINFOHEADER));

   lpBmInfoHdr->biSize      = sizeof (BITMAPINFOHEADER);
   lpBmInfoHdr->biWidth     = dwWidth;
   lpBmInfoHdr->biHeight    = dwHeight;
   lpBmInfoHdr->biPlanes    = 1;

   if (nBPP <= 1)
      nBPP = 1;
   else if (nBPP <= 4)
      nBPP = 4;
   else if (nBPP <= 8)
      nBPP = 8;
/* Doesn't work
   else if (nBPP <= 16)
      nBPP = 16;
*/
   else
      nBPP = 24;

   lpBmInfoHdr->biBitCount  = nBPP;
   lpBmInfoHdr->biSizeImage = WIDTHBYTES (dwWidth * nBPP) * dwHeight;
}




LPSTR FindDIBBits (LPSTR lpbi)
{
   return (lpbi + *(LPDWORD)lpbi + PaletteSize (lpbi));
}

//---------------------------------------------------------------------
//
// Function:   BitmapToDIB
//
// Purpose:    Given a device dependent bitmap and a palette, returns
//             a handle to global memory with a DIB spec in it.  The
//             DIB is rendered using the colors of the palette passed in.
//
//             Stolen almost verbatim from ShowDIB.
//
// Parms:      hBitmap == Handle to device dependent bitmap compatible
//                        with default screen display device.
//             hPal    == Palette to render the DDB with.  If it's NULL,
//                        use the default palette.
//
// History:   Date      Reason
//             6/01/91  Created
//             
//---------------------------------------------------------------------

HANDLE BitmapToDIB (HBITMAP hBitmap, HPALETTE hPal)
{
   BITMAP             Bitmap;
   BITMAPINFOHEADER   bmInfoHdr;
   LPBITMAPINFOHEADER lpbmInfoHdr;
   LPSTR              lpBits;
   HDC                hMemDC;
   HANDLE             hDIB;
   HPALETTE           hOldPal = NULL;

      // Do some setup -- make sure the Bitmap passed in is valid,
      //  get info on the bitmap (like its height, width, etc.),
      //  then setup a BITMAPINFOHEADER.

   if (!hBitmap)
      return NULL;

   if (!GetObject (hBitmap, sizeof (Bitmap), (LPSTR) &Bitmap))
      return NULL;

   InitBitmapInfoHeader (&bmInfoHdr, 
                         Bitmap.bmWidth, 
                         Bitmap.bmHeight, 
                         Bitmap.bmPlanes * Bitmap.bmBitsPixel);


      // Now allocate memory for the DIB.  Then, set the BITMAPINFOHEADER
      //  into this memory, and find out where the bitmap bits go.

   hDIB = GlobalAlloc (GHND, sizeof (BITMAPINFOHEADER) + 
             PaletteSize ((LPSTR) &bmInfoHdr) + bmInfoHdr.biSizeImage);

   if (!hDIB)
      return NULL;

   lpbmInfoHdr  = (LPBITMAPINFOHEADER) GlobalLock (hDIB);
   *lpbmInfoHdr = bmInfoHdr;
   lpBits       = FindDIBBits ((LPSTR) lpbmInfoHdr);


      // Now, we need a DC to hold our bitmap.  If the app passed us
      //  a palette, it should be selected into the DC.

   hMemDC       = GetDC (NULL);

   if (hPal)
      {
      hOldPal = SelectPalette (hMemDC, hPal, FALSE);
      RealizePalette (hMemDC);
      }



      // We're finally ready to get the DIB.  Call the driver and let
      //  it party on our bitmap.  It will fill in the color table,
      //  and bitmap bits of our global memory block.

   if (!GetDIBits (hMemDC, 
                   hBitmap, 
                   0, 
                   Bitmap.bmHeight, 
                   lpBits, 
                   (LPBITMAPINFO) lpbmInfoHdr, 
                   DIB_RGB_COLORS))
      {
      GlobalUnlock (hDIB);
      GlobalFree (hDIB);
      hDIB = NULL;
      }
   else
      GlobalUnlock (hDIB);


      // Finally, clean up and return.

   if (hOldPal)
      SelectPalette (hMemDC, hOldPal, FALSE);

   ReleaseDC (NULL, hMemDC);

   return hDIB;
}

Bool wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap)
{
  HPALETTE hPalette = 0;
  if (colourmap)
    hPalette = colourmap->ms_palette;

  HANDLE dibHandle = BitmapToDIB(bitmap->ms_bitmap, hPalette);
  if (dibHandle)
  {
    Bool success = WriteDIB(filename, dibHandle);
    GlobalFree(dibHandle);
    return success;
  }
  else return FALSE;
}
