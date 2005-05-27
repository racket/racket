
/* From wxMac. */

// wxMac notes:
// mostly from imagetest at skye.aiai with some (power) macintrash additions
// platform/compiler changes and other dress up (mistakes) by me
// LjB birk@moonface.com for macintosh version of Majestic, (another HTML viewer
// with a built in search engine capability)
// this stuff is meant to make it a little easier to use graphic files
// along with resources and needs to be used with the modified _gdi, and _dccanx
// files for macs at aiai
// also may require GUSI
// also only works with color and only slightly tested on powermacs
// not responsible for what this might do to your macintosh, use at your own risk
// sorry I do not have time to add more comments, but hope this is of some use

#include "wx.h"

#include <string.h>

#include "wximgfil.h"

#define INTERLACEMASK 0x40

void CreateOffScreenPixMap (HBITMAP *,wxGIF *gif);

typedef struct {
  char header[6];       
  ushort scrwidth;
  ushort scrheight;
  char pflds;
  char bcindx;
  char pxasrat;
} t_dscgif;
static t_dscgif dscgif;

typedef struct {
  byte sep;
  ushort l;
  ushort t;
  ushort w;
  ushort h;
  byte  pf;
} t_image;
static t_image image;

typedef struct { 
  ushort colres;
  ushort sogct;
  rgb paleta[256];
} t_TabCol;
static t_TabCol TabCol;

static long Code_Mask[] = {
	  0,
	  0x0001, 0x0003,
	  0x0007, 0x000F,
	  0x001F, 0x003F,
	  0x007F, 0x00FF,
	  0x01FF, 0x03FF,
	  0x07FF, 0x0FFF
	  };

void * wxGIF::GetRawImage()
{
  return (void *) lpbi;
}

wxGIF::~wxGIF()
{
}

wxGIF::wxGIF( char * path)
{
  navail_bytes = nbits_left = 0;
  lpbi = NULL;
  for (ushort i=0; i<13; i++) { code_mask[i] = Code_Mask[i]; }
  if (path) {
    fp = fopen(path,"rb");
    if (!fp)
    	return;
    if (ReadHeader(fp)) {
      Create(image.w, image.h, 8);
      if (GetRawImage() != 0)
	Extrae_imagen();
    }
  }
  fclose(fp);
}

void wxGIF::Create(ushort width, ushort height, ushort deep)
{
  Width = width; Height = height; Deep = deep;
  lpbi = 0;
  RawImage = 0;

  lpbi = new char[width*height];
  if (lpbi == 0)
    {
      lpbi = 0; // for debug purposes
    }
  RawImage = (char  *) (lpbi);
  EfeWidth = (long)(((long)Width*Deep + 31) / 32) * 4;
}

ushort wxGIF::out_line(uchar *pixels, ushort linelen)
{
  if (ItOK()) {

	 SetRow(linelen, pixels);
	 (void)ItPrev();
	 return 0;
  }
  else return -1;
}


wxColourMap *wxGIF::getColorMap()
{
	byte r[256], g[256], b[256];

	for (ushort i=0; i<TabCol.sogct ; i++) {
	  r[i] =  TabCol.paleta[i].r;
	  g[i] =  TabCol.paleta[i].g;
	  b[i] =  TabCol.paleta[i].b;
	}
	SetColourMap(TabCol.sogct,r,g,b);
	return NULL;
}


BOOL wxGIF::ReadHeader(FILE *fp)
{
  unsigned char tstA[256];
  unsigned char *rgbTable, single, eid;
  ushort widlow, widhigh, hgtlow, hgthi, i;
  ushort index = 0;
  
  if (fread((char*)&tstA[0],13,1,fp) != 1)
    return FALSE;
  
  for ( i=0; i<6;i++) {
    dscgif.header[i] = tstA[index++];
  }
  widlow = (ushort) tstA[index++];
  widhigh = ((ushort) tstA[index++]) * 256;
  hgtlow = (ushort)  tstA[index++];
  hgthi = ((ushort) tstA[index++]) * 256;
  dscgif.scrwidth = widlow + widhigh;
  dscgif.scrheight = hgtlow + hgthi;
  dscgif.pflds = tstA[index++];
  dscgif.bcindx = tstA[index++];
  dscgif.pxasrat = tstA[index++];

  if (strncmp(dscgif.header,"GIF8",3)!=0)
    return FALSE;

  TabCol.sogct = 1 << ((dscgif.pflds & 7) + 1);
  TabCol.colres = (dscgif.pflds >> 6) & 7 + 1;
  
  if (dscgif.pflds & 0x80) {
    int errcnt;
    int tp;
    rgbTable = new uchar[3*TabCol.sogct];
    if (fread((char *)rgbTable,1, 3*TabCol.sogct,fp) != 3*TabCol.sogct)
      return FALSE;
    tp = 0;
    for (i = 0; i < TabCol.sogct; i++) {
      TabCol.paleta[i].r = rgbTable[tp++];
      TabCol.paleta[i].g = rgbTable[tp++];
      TabCol.paleta[i].b = rgbTable[tp++];
    }
    rgbTable = NULL;
  }

  single = 0;
  transparent_index = -1;
  while (1) {
    if (fread((char *)&single, 1, 1, fp) != 1)
      return FALSE;
    if (single == 0x21) { /* extension */
      if (fread(&eid,1,1,fp) != 1) /* function */
	return FALSE;
      while (1) {
	if (fread(&single,1,1,fp) != 1) /* block size */
	  return FALSE;
	if (single) {
	  /* Note, single is an uchar, so it's <= 255 */
	  if (fread((char*)&tstA[0],single,1,fp) != 1)
	    return FALSE;
	  if ((eid == 0xF9) && (single == 4)) {
	    /* Transparent color index? */
	    if (tstA[0] & 0x1) {
	      transparent_index = tstA[3];
	    }
	  }
	} else
	  break;
      }
    } else
      break;
  }
    
  if (fread((char*)&tstA[0],9,1,fp) != 1)
    return FALSE;
  index = 0;
  image.sep = single;
  image.l = tstA[index++];
  image.l += (256 * tstA[index++]);
  image.t = tstA[index++];
  image.t += (256 * tstA[index++]);
  image.w = tstA[index++];
  image.w += (256 * tstA[index++]);
  image.h = tstA[index++];
  image.h += (256 * tstA[index++]);
  image.pf = tstA[index++];

  if (image.pf & 0x80) {
    int len = (1 << ((image.pf & 7) + 1)), i, j = 0;
    size_t amt;
    for (i = 0; i < len; i++) {
      if (j == 198)
	j = 0;
      if (!j) {
	amt = (len - i > 66) ? 198 : (3 * (len - i));
	if (fread((char*)&tstA[0], amt, 1, fp) != 1)
	  return FALSE;
      }
      TabCol.paleta[i].r = tstA[j++];
      TabCol.paleta[i].g = tstA[j++];
      TabCol.paleta[i].b = tstA[j++];
    }
  }

  return TRUE;
}

BOOL wxGIF::Extrae_imagen()
{
  (void)getColorMap();
  ibf=GIFBUFTAM+1;
  IterImage = 0; 
  ItCount = 0; 
  decoder(GetWidth());
  return TRUE;
}

wxGIF::wxGIF()
{
}
/* This function initializes the decoder for reading a new image.
 */
ushort wxGIF::init_exp(ushort size)
{
  curr_size = size + 1;
  top_slot = 1 << curr_size;
  clear = 1 << size;
  ending = clear + 1;
  slot = newcodes = ending + 1;
  navail_bytes = nbits_left = 0;
  return(0);
}

ushort wxGIF::get_byte()
{
  
  if (ibf>=GIFBUFTAM) {
    fread(buf,GIFBUFTAM,1,fp);
    ibf = 0;
  }
  
  return buf[ibf++];
}

/* get_next_code()
 * - gets the next code from the GIF file.  Returns the code, or else
 * a negative number in case of file errors...
 */
ushort wxGIF::get_next_code()
{
  ushort i, x;
  ulong ret;

  if (nbits_left == 0) {
    if (navail_bytes <= 0) {
      /* Out of bytes in current block, so read next block
       */
      pbytes = 0;
      navail_bytes = get_byte();
      if (navail_bytes < 0)
	return(navail_bytes);
      else if (navail_bytes) {
	for (i = 0; i < navail_bytes; ++i) {
	  if ((x = get_byte()) < 0)
	    return(x);
	  byte_buff[i] = x;
	}
      }
    }
    b1 = byte_buff[pbytes++];
    nbits_left = 8;
    --navail_bytes;
  }

  ret = b1 >> (8 - nbits_left);
  while (curr_size > nbits_left) {
    if (navail_bytes <= 0) {
      /* Out of bytes in current block, so read next block
       */
      pbytes = 0;
      navail_bytes = get_byte();
      if (navail_bytes < 0)
	return(navail_bytes);
      else if (navail_bytes) {
	for (i = 0; i < navail_bytes; ++i) {
	  x = get_byte();
	  if (x < 0)
	    return(x);
	  byte_buff[i] = x;
	}
      }
    }
    b1 = byte_buff[pbytes++];
    ret = ret | (b1 << nbits_left);
    nbits_left += 8;
    --navail_bytes;
  }
  nbits_left -= curr_size;
  ret &= code_mask[curr_size];
  return ret;
}


/* short decoder(linewidth)
 *    short linewidth;               * Pixels per line of image *
 */

void wxGIF::InitInterlaceRow(int linewidth)
{
  int count, lpos;
  
  lpos = ItCount;
  
  count = ((Height - 1) / 8);
  if (lpos <= count) {
    IterImage = (linewidth * (lpos * 8));
  } else {
    lpos -= (count + 1);
    count = ((Height - 5) / 8);
    if (lpos <= count) {
      IterImage = (linewidth * (lpos * 8 + 4));
    } else {
      lpos -= (count + 1);
      count = ((Height - 3) / 4);
      if (lpos <= count) {
        IterImage = (linewidth * (lpos * 4 + 2));
      } else {
	lpos -= (count + 1);
        IterImage = (linewidth * (lpos * 2 + 1));
      }
    }
  }
}

ushort  wxGIF::decoder(ushort linewidth)
{
  int bufptr, sp;
  uchar *lbuf;
  register ushort code, fc, oc, bufcnt;
  ushort c, size, ret;
  int interlace;

  /* Initialize for decoding a new image... */
  if ((size = get_byte()) < 0)
    return(size);
  if (size < 2 || 9 < size)
    return(BAD_CODE_SIZE);
  init_exp(size);
  oc = fc = 0;
  ret = 0;

  lbuf = new WXGC_ATOMIC uchar[linewidth + 1];

  sp = 0;
  bufptr = 0;
  bufcnt = linewidth;

  interlace = image.pf & INTERLACEMASK;

  /* This is the main loop.  For each code we get we pass through the
    * linked list of prefix codes, pushing the corresponding "character" for
    * each code onto the stack.  When the list reaches a single "character"
    * we push that on the stack too, and then start unstacking each
    * character for output in the correct order.  Special handling is
    * included for the clear code, and the whole thing ends when we get
    * an ending code.
    */
  while ((c = get_next_code()) != ending)
  {

      /* If we had a file error, return without completing the decode
		 */
    if (c < 0)
    {
      return(0);
     }

		/* If the code is a clear code, reinitialize all necessary items.
		 */
    if (c == clear)
    {
       curr_size = size + 1;
       slot = newcodes;
       top_slot = 1 << curr_size;

			/* Continue reading codes until we get a non-clear code
			 * (Another unlikely, but possible case...)
			 */
      while ((c = get_next_code()) == clear) ;

         /* If we get an ending code immediately after a clear code
          * (Yet another unlikely case), then break out of the loop.
          */
      if (c == ending)
        break;

         /* Finally, if the code is beyond the range of already set codes,
          * (This one had better NOT happen...  I have no idea what will
          * result from this, but I doubt it will look good...) then set it
          * to color zero.
          */
      if (c >= slot)
        c = 0;

      oc = fc = c;

         /* And let us not forget to put the char into the buffer... And
			 * if, on the off chance, we were exactly one pixel from the end
          * of the line, we have to send the buffer to the out_line()
          * routine...
          */
      lbuf[bufptr++] = c;
      if (--bufcnt == 0)
      {
        if (ItCount < Height)
        {
	  if (interlace)
	    InitInterlaceRow(linewidth);

          for (ushort i=0; i<linewidth; i++) { RawImage[IterImage++] = lbuf[i]; }
          ItCount++;
        }
        else
        {
          return ret;
        }
        bufptr = 0;
        bufcnt = linewidth;
      }
    }
    else
    {

         /* In this case, it's not a clear code or an ending code, so
          * it must be a code code...  So we can now decode the code into
          * a stack of character codes. (Clear as mud, right?)
          */
      code = c;

         /* Here we go again with one of those off chances...  If, on the
          * off chance, the code we got is beyond the range of those already
          * set up (Another thing which had better NOT happen...) we trick
			 * the decoder into thinking it actually got the last code read.
          * (Hmmn... I'm not sure why this works...  But it does...)
			 */
      if (code >= slot)
      {
        if (code > slot)
          ++bad_code_count;
        code = oc;
        stack[sp++] = fc;
       }

         /* Here we scan back along the linked list of prefixes, pushing
          * helpless characters (ie. suffixes) onto the stack as we do so.
			 */
        while (code >= newcodes)
        {
          stack[sp++] = suffix[code];
          code = prefix[code];
        }

         /* Push the last character on the stack, and set up the new
          * prefix and suffix, and if the required slot number is greater
			 * than that allowed by the current bit size, increase the bit
          * size.  (NOTE - If we are all full, we *don't* save the new
          * suffix and prefix...  I'm not certain if this is correct...
          * it might be more proper to overwrite the last code...
			 */
         stack[sp++] = code;
         if (slot < top_slot)
         {
           suffix[slot] = fc = code;
           prefix[slot++] = oc;
           oc = c;
         }
        if (slot >= top_slot)
          if (curr_size < 12)
          {
            top_slot <<= 1;
            ++curr_size;
          } 

         /* Now that we've pushed the decoded string (in reverse order)
          * onto the stack, lets pop it off and put it into our decode
          * buffer...  And when the decode buffer is full, write another
			 * line...
          */
         while (sp > 0)
         {
            lbuf[bufptr++] = stack[--sp];
            if (--bufcnt == 0)
            {
	      if (interlace)
		InitInterlaceRow(linewidth);

              if (ItCount < Height)
              {
                for (ushort i=0; i<linewidth; i++) { RawImage[IterImage++] = lbuf[i]; }
                ItCount++;
              }
              else
              {
                return ret;
              }
              bufptr = 0;
              bufcnt = linewidth;
            }
          }
        }
      }
  ret = 0;
  if (bufcnt != linewidth)
      if (ItCount < Height)
      {
	if (interlace)
	  InitInterlaceRow(linewidth);
        for (ushort i=0; i<(linewidth - bufcnt); i++) { RawImage[IterImage++] = lbuf[i]; }
        ItCount++;
       }
       else
      {
        return ret;
      }
  bufptr = 0;
  bufcnt = linewidth;
  return ret;
}

//// these were all inline 

//inline 
void wxGIF::reset() 
{
 IterImage = 0; 
 ItCount = 0; 
}

//inline 
void wxGIF::upset()
{
  ItCount = Height-1;
  IterImage = EfeWidth*(Height-1);
}

//inline 
Bool wxGIF::ItNext()
{
  if (ItCount++ >= Height) return 0;
  IterImage += EfeWidth;
  return 1;
}

//inline 
Bool wxGIF::ItPrev()
{
  if (ItCount-- < 0) return 0;
  IterImage -= EfeWidth;
  return 1;
}

//inline 
void wxGIF::SetRow(ushort n, byte *buf)
{
  for (ushort i=0; i<n; i++) { RawImage[IterImage+i] = buf[i]; }
}

//inline 
void wxGIF::GetRow(ushort n, byte *buf)
{
  for (ushort i=0; i<n; i++) { buf[i] = RawImage[IterImage+i]; }
}

///// end inliners


BOOL wxGIF::SetColourMap(ushort n, byte *r, byte *g, byte *b)
{
  numcmapentries = n;
  for (int i=0; i < n; i++) {
    red[i] = r[i];
    green[i] = g[i];
    blue[i] = b[i];
  }
  return TRUE;
}

Bool wxLoadGifIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal, int getMask)
{
  wxGIF *gifImage;
 
  if (pal) *pal = NULL;

  gifImage = new wxGIF(fileName);
  if (gifImage && gifImage->GetRawImage() != 0) {
    int p = 0, i, j, h, w, datap;
    unsigned char *data;
    wxMemoryDC *mem;
    HDC hdc;
    unsigned short *red, *green, *blue;
    COLORREF cref;
    int white;

    h = gifImage->GetHeight();
    w = gifImage->GetWidth();
	
    bm->Create(w, h, -1);

    if (!bm->Ok()) {
      delete gifImage;
      return FALSE;
    }

    mem = new wxMemoryDC();
    mem->SelectObject(bm);

    mem->Clear();

    hdc = mem->cdc;

    red = gifImage->red;
    green = gifImage->green;
    blue = gifImage->blue;

    white = -1;
    for (i = gifImage->numcmapentries; i--; ) {
      if (red[i] == 255
	  && green[i] == 255
	  && blue[i] == 255) {
	white = i;
	break;
      }
    }

    data = (unsigned char *)gifImage->GetRawImage();
    datap = 0;
	
    for (j = 0; j < h; j++) {
      for (i = 0; i < w; i++, datap++) {
	int v = data[datap];
	if (v != white) {
	  cref = RGB(red[v], green[v], blue[v]);
	  SetPixelV(hdc, i, j, cref);
	}
      }
    }

    /* Make mask if requested and if there was a transparent
       index: */
    if (getMask && (gifImage->transparent_index >= 0)) {
      wxBitmap *mbm;
      int transparent_index = gifImage->transparent_index;

      mbm = new wxBitmap(w, h, 1);
      if (mbm->Ok()) {
	int datap;
	mem->SelectObject(mbm);
	hdc = mem->cdc;
	data = (unsigned char *)gifImage->GetRawImage();
	datap = 0;
	for (j = 0; j < h; j++) {
	  for (i = 0; i < w; i++, datap++) {
	    int v = data[datap];
	    if (v == transparent_index) {
	      cref = RGB(255, 255, 255);
	    } else {
	      cref = RGB(0, 0, 0);
	    }
	    SetPixelV(hdc, i, j, cref);
	  }
	} 
	bm->SetMask(mbm);
      }
    }
	  
    mem->SelectObject(NULL);
    delete mem;
	
    delete gifImage;
    return TRUE;
  }
  
  if (gifImage)
    delete gifImage;

  bm->SetOk(FALSE);
  return FALSE;
}


void CreateOffScreenPixMap (HBITMAP *_bm, wxGIF *gif)
{
	
	

}

