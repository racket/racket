/*************************************************************************
 * 									 *
 * FILE : dib.h                                                          *
 * Routines for loading and saving Windows bitmaps                       *
 * 									 *
 ************************************************************************/

/*
 * wxWindows integration
 *
 */
 
// Save (device dependent) wxBitmap as a DIB
Bool             wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap = NULL);

// Load device independent bitmap into device dependent bitmap
wxBitmap         *wxLoadBitmap(char *filename, wxColourMap **colourmap = NULL);

// Load into existing bitmap;
Bool wxLoadIntoBitmap(char *filename, wxBitmap *bitmap, wxColourMap **pal = NULL);

/*
// Load GIF file into existing bitmap
// Doesn't seem to work properly...
Bool wxLoadGifIntoBitmap(char *filename, wxBitmap *bitmap);

// Save wxBitmap to GIF file.
Bool wxSaveBitmapToGif(char *filename, wxBitmap *bitmap);
*/
HANDLE BitmapToDIB (HBITMAP hBitmap, HPALETTE hPal);
BOOL   ReadDIB(LPSTR lpFileName, HBITMAP *bitmap, HPALETTE *palette);
HANDLE ReadDIB2(LPSTR lpFileName);

