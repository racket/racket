/*
 * This file has both JPEG and PNG support (despite the file name).
 *
 * The JPEG part Derived from IJG's example.c.
 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#ifdef MZ_PRECISE_GC
# include "common.h"
#endif

#include "wx_dcmem.h"
#include "wx_gdi.h"

#include <stdio.h>
#include <stdlib.h>
extern "C" {
# ifdef WX_USE_LIBJPEG
#  include <jpeglib.h>
# else
#  include "jpeg/jpeglib.h"
# endif
# ifdef WX_USE_LIBPNG
#  include <png.h>
# else
#  include "libpng/png.h"
# endif
}
#include <setjmp.h>

#ifdef MPW_CPLUS
extern "C" { typedef void (*JPEG_ERROR_F_PTR)(j_common_ptr info); }
# define CAST_JPEGP (JPEG_ERROR_F_PTR)
#else
# define CAST_JPEGP /* empty */
#endif

#ifdef wx_x
# define WX_QUANTIZE 1
# define Q_NOT !
#else
# define WX_QUANTIZE 0
# define Q_NOT /* empty */
#endif

static wxColor *the_color;
extern void wxmeError(const char *e);
extern int wxGetPreference(const char *name, char *res, long len);

#ifdef wx_msw
# include "wx_utils.h"
# define wxFOpen(fn, m) _wfopen(wxWIDE_STRING(fn), m)
# define wx_RB_mode L"rb"
# define wx_WB_mode L"wb"
#else
# define wxFOpen(fn, m) fopen(fn, m)
# define wx_RB_mode "rb"
# define wx_WB_mode "wb"
#endif

#ifndef DRAW_SCANLINE_DEFINED

static void draw_scanline(JSAMPROW row, int cols, int rownum, int step, JSAMPARRAY colormap, wxMemoryDC *dc,
			  int mono)
{
  int colnum, r, g, b;

  for (colnum = 0; colnum < cols; colnum++) {
#if WX_QUANTIZE
    if (!mono) {
      int v;
      v = row[colnum];
      r = colormap[0][v];
      g = colormap[1][v];
      b = colormap[2][v];
    } else {
#endif
      if (step == 1) {
	r = row[colnum];
	g = row[colnum];
	b = row[colnum];
      } else {
	r = row[colnum * step];
	g = row[colnum * step + 1];
	b = row[colnum * step + 2];
      }
#if WX_QUANTIZE
    }
#endif
    dc->SetPixelFast(colnum, rownum, r, g, b);
  }
}

#endif

static void get_scanline(JSAMPROW row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, d = 0, r, g, b;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new WXGC_PTRS wxColour(0, 0, 0);
  }

  for (colnum = 0; colnum < cols; colnum++, d += 3) {
    dc->GetPixel(colnum, rownum, the_color);
    r = the_color->Red();
    g = the_color->Green();
    b = the_color->Blue();
    row[d] = r;
    row[d+1] = g;
    row[d+2] = b;
  }
}

wxMemoryDC *create_dc(int width, int height, wxBitmap *bm, int mono)
{
  wxMemoryDC *dc;

  dc = new WXGC_PTRS wxMemoryDC();
  if (width >= 0)
    bm->Create(width, height, mono ? 1 : -1);
  dc->SelectObject(bm);

  if (!dc->Ok()) {
    dc->SelectObject(NULL);
    return NULL;
  }

  return dc;
}

wxMemoryDC *create_reader_dc(wxBitmap *bm, volatile int *desel)
{
  wxMemoryDC *dc;

  dc = new WXGC_PTRS wxMemoryDC(1); /* 1 => read-only */
  dc->SelectObject(bm);
  if (!dc->GetObject()) {
# ifdef wx_msw
    if (bm->selectedInto) {
      /* Even selecting into a read-only dc doesn't seem to work
	 if it already has a dc. Just use that one, then. */
      dc = (wxMemoryDC *)bm->selectedInto;
      *desel = 0;
    } else
# endif
      return NULL;
  }

  return dc;
}

/**********************************************************************/

char jpeg_err_buffer[JMSG_LENGTH_MAX + 256];

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

/*
 * Here's the routine that will replace the standard error_exit method:
 */

static void my_error_exit(j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  (*cinfo->err->format_message)(cinfo, jpeg_err_buffer);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

/*
 * Sample routine for JPEG decompression.  We assume that the source file name
 * is passed in.  We want to return 1 on success, 0 on error.
 */

int read_JPEG_file(char * filename, wxBitmap *bm)
{
  FILE * volatile infile;       /* source file */
  JSAMPARRAY buffer;		/* Output row buffer */
  int row_stride;		/* physical row width in output buffer */
  wxMemoryDC *dc;
#ifdef MZ_PRECISE_GC
  START_XFORM_SKIP;
#endif
  /* This struct contains the JPEG decompression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  /* We use our private extension JPEG error handler.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
#ifdef MZ_PRECISE_GC
  END_XFORM_SKIP;
#endif

  /* In this example we want to open the input file before doing anything else,
   * so that the setjmp() error recovery below can assume the file is open.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to read binary files.
   */

  if ((infile = wxFOpen(filename, wx_RB_mode)) == NULL) {
    sprintf(jpeg_err_buffer, "can't open %.255s\n", filename);
    wxmeError(jpeg_err_buffer);
    return 0;
  }

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = CAST_JPEGP my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    fclose(infile);
    wxmeError(jpeg_err_buffer);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data destnation (eg, a file) */

  jpeg_stdio_src(&cinfo, infile);

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression */
#if WX_QUANTIZE
  cinfo.quantize_colors = 1;
#endif

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 

  dc = create_dc(cinfo.output_width, cinfo.output_height, bm, 0);
  if (!dc) {
    /* couldn't allocate DC or select bitmap */
    return 0;
  }

  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;
  /* Make a one-row-high sample array that will go away when done with image */
  buffer = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  dc->BeginSetPixelFast(0, 0, cinfo.output_width, cinfo.output_height);
  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    (void) jpeg_read_scanlines(&cinfo, buffer, 1);
    /* Assume put_scanline_someplace wants a pointer and sample count. */
    draw_scanline(buffer[0],
		  cinfo.output_width, cinfo.output_scanline - 1, 
		  cinfo.output_components, cinfo.colormap,
		  dc, cinfo.num_components == 1);
  }
  dc->EndSetPixelFast();

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  fclose(infile);

  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */

  /* And we're done! */
  dc->SelectObject(NULL);
  return 1;
}

int write_JPEG_file(char *filename, wxBitmap *bm, int quality)
{
  /* More stuff */
  FILE * volatile outfile;		/* target file */
  JSAMPROW row_pointer;	/* pointer to JSAMPLE row[s] */
  wxMemoryDC * volatile dc;
  int wid;
  volatile int desel = 1;

#ifdef MZ_PRECISE_GC
  START_XFORM_SKIP;
#endif
  /* This struct contains the JPEG compression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   * It is possible to have several such structures, representing multiple
   * compression/decompression processes, in existence at once.  We refer
   * to any one struct (and its associated working data) as a "JPEG object".
   */
  struct jpeg_compress_struct cinfo;
  /* This struct represents a JPEG error handler.  It is declared separately
   * because applications often want to supply a specialized error handler
   * (see the second half of this file for an example).  But here we just
   * take the easy way out and use the standard error handler, which will
   * print a message on stderr and call exit() if compression fails.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
#ifdef MZ_PRECISE_GC
  END_XFORM_SKIP;
#endif

  dc = create_reader_dc(bm, (int *)&desel);

  wid = bm->GetWidth();
  row_pointer = new WXGC_ATOMIC JSAMPLE[3 * wid];

  if ((outfile = wxFOpen(filename, wx_WB_mode)) == NULL) {
    if (desel)
      dc->SelectObject(NULL);
    sprintf(jpeg_err_buffer, "can't open %.255s\n", filename);
    wxmeError(jpeg_err_buffer);
    return 0;
  }
  /* Step 1: allocate and initialize JPEG compression object */

  /* We have to set up the error handler first, in case the initialization
   * step fails.  (Unlikely, but it could happen if you are out of memory.)
   * This routine fills in the contents of struct jerr, and returns jerr's
   * address which we place into the link field in cinfo.
   */
  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = CAST_JPEGP my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    if (desel)
      dc->SelectObject(NULL);
    jpeg_destroy_compress(&cinfo);
    fclose(outfile);
    wxmeError(jpeg_err_buffer);
    return 0;
  }

  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */

  /* Here we use the library-supplied code to send compressed data to a
   * stdio stream.  You can also write your own code to do something else. */
  jpeg_stdio_dest(&cinfo, outfile);

  /* Step 3: set parameters for compression */

  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width = wid; 	/* image width and height, in pixels */
  cinfo.image_height = bm->GetHeight();
  cinfo.input_components = 3;		/* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; 	/* colorspace of input image */
  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);
  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling:
   */
  jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */

  /* TRUE ensures that we will write a complete interchange-JPEG file.
   * Pass TRUE unless you are very sure of what you're doing.
   */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */

  /* Here we use the library's state variable cinfo.next_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   * To keep things simple, we pass one scanline per call; you can pass
   * more if you wish, though.
   */

  while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    get_scanline(row_pointer, wid, cinfo.next_scanline, dc);
    (void)jpeg_write_scanlines(&cinfo, &row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);
  /* After finish_compress, we can close the output file. */
  fclose(outfile);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  if (desel)
    dc->SelectObject(NULL);
  
  /* And we're done! */
  return 1;
}

/*
 * SOME FINE POINTS:
 *
 * In the above code, we ignored the return value of jpeg_read_scanlines,
 * which is the number of scanlines actually read.  We could get away with
 * this because we asked for only one line at a time and we weren't using
 * a suspending data source.  See libjpeg.doc for more info.
 *
 * We cheated a bit by calling alloc_sarray() after jpeg_start_decompress();
 * we should have done it beforehand to ensure that the space would be
 * counted against the JPEG max_memory setting.  In some systems the above
 * code would risk an out-of-memory error.  However, in general we don't
 * know the output image dimensions before jpeg_start_decompress(), unless we
 * call jpeg_calc_output_dimensions().  See libjpeg.doc for more about this.
 *
 * Scanlines are returned in the same order as they appear in the JPEG file,
 * which is standardly top-to-bottom.  If you must emit data bottom-to-top,
 * you can use one of the virtual arrays provided by the JPEG memory manager
 * to invert the data.  See wrbmp.c for an example.
 *
 * As with compression, some operating modes may require temporary files.
 * On some systems you may need to set up a signal handler to ensure that
 * temporary files are deleted if the program is interrupted.  See libjpeg.doc.
 */

/**********************************************************************/

static char *png_err_msg;
static int pem_registered;

#ifdef MPW_CPLUS
extern "C" {
  typedef void (*UEP_PTR)(png_structp png_ptr, png_const_charp msg);
  typedef void (*UWP_PTR)(png_structp png_ptr, png_const_charp msg);
}
# define CAST_UEP (UEP_PTR)
# define CAST_UWP (UWP_PTR)
#else
# define CAST_UEP /* empty */
# define CAST_UWP /* empty */
#endif

static void user_error_proc(png_structp png_ptr, png_const_charp msg)
{
  int len;

  if (!pem_registered) {
    wxREGGLOB(png_err_msg);
  }
  len = strlen(msg);
  png_err_msg = new WXGC_ATOMIC char[len + 1];
  memcpy(png_err_msg, msg, len + 1);
  
  longjmp(png_ptr->jmpbuf, 1);
}

static void user_warn_proc(png_structp info, png_const_charp msg)
{
}

static void png_start_lines(wxMemoryDC *dc, wxMemoryDC *mdc, int width, int height)
{
  dc->BeginSetPixelFast(0, 0, width, height);
  if (mdc)
    mdc->BeginSetPixelFast(0, 0, width, height);
}

static void png_end_lines(wxMemoryDC *dc, wxMemoryDC *mdc)
{
  dc->EndSetPixelFast();
  if (mdc)
    mdc->EndSetPixelFast();
}

static void png_draw_line(png_bytep row, int cols, int rownum, wxMemoryDC *dc, wxMemoryDC *mdc, int step)
{
  int colnum, delta;

  for (colnum = 0, delta = 0; colnum < cols; colnum++, delta += step) {
    dc->SetPixelFast(colnum, rownum,
		     row[delta], 
		     row[delta + 1], 
		     row[delta + 2]);
    if (mdc) {
      mdc->SetPixelFast(colnum, rownum,
			row[delta + 3],
			row[delta + 3],
			row[delta + 3]);
    }
  }
}

static void png_draw_line1(png_bytep row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, delta = 0, bit;

  for (colnum = 0; colnum < cols; delta++) {
    for (bit = 128; (colnum < cols) && bit; colnum++, bit = bit >> 1) {
      if (row[delta] & bit)
	dc->SetPixelFast(colnum, rownum, 255, 255, 255);
      else
	dc->SetPixelFast(colnum, rownum, 0, 0, 0);
    }
  }
}

static void png_get_line(png_bytep row, int cols, int rownum, wxMemoryDC *dc, wxMemoryDC *mdc)
{
  int colnum, delta, r, g, b;
  int step = (mdc ? 4 : 3);

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new WXGC_PTRS wxColour(0, 0, 0);
  }

  for (colnum = 0, delta = 0; colnum < cols; colnum++, delta += step) {
    dc->GetPixel(colnum, rownum, the_color);
    r = the_color->Red();
    g = the_color->Green();
    b = the_color->Blue();
    row[delta] = r;
    row[delta+1] = g;
    row[delta+2] = b;
    if (mdc) {
      mdc->GetPixel(colnum, rownum, the_color);
      r = the_color->Red();
      row[delta+3] = r;
    }
  }
}

static void png_get_line1(png_bytep row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, delta, bit, r, g, b, bits;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new WXGC_PTRS wxColour(0, 0, 0);
  }

  for (colnum = 0, delta = 0; colnum < cols; delta++) {
    bits = 0;
    for (bit = 128; (colnum < cols) && bit; colnum++, bit = bit >> 1) {
      dc->GetPixel(colnum, rownum, the_color);
      r = the_color->Red();
      g = the_color->Green();
      b = the_color->Blue();
      if ((r == 255) && (g == 255) && (b == 255)) 
	bits |= bit;
    }
    row[delta] = bits;
  }
}

/**********************************************************************/

int wx_read_png(char *file_name, wxBitmap *bm, int w_mask, wxColour *bg)
{
   png_structp png_ptr;
   png_structp volatile png_ptr_orig;
   png_infop info_ptr;
   png_infop volatile info_ptr_orig;
   png_uint_32 width, height;
   int bit_depth, color_type, interlace_type, is_mono = 0, row_width;
   unsigned int number_passes, pass, y;
   FILE * volatile fp;
   png_bytep *rows, row;
   wxMemoryDC * volatile dc = NULL;
   wxMemoryDC *mdc = NULL;
   wxBitmap *mbm = NULL;

   if ((fp = wxFOpen(file_name, wx_RB_mode)) == NULL)
     return 0;

   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  REQUIRED
    */
   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, 
				    CAST_UEP user_error_proc, 
				    CAST_UWP user_warn_proc);

   if (png_ptr == NULL)
   {
      fclose(fp);
      return 0;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return 0;
   }

   /* Set error handling if you are using the setjmp/longjmp method (this is
    * the normal method of doing things with libpng).  REQUIRED unless you
    * set up your own error handlers in the png_create_read_struct() earlier.
    */

   png_ptr_orig = png_ptr;
   info_ptr_orig = info_ptr;

   if (setjmp(png_ptr->jmpbuf))
   {
     /* Free all of the memory associated with the png_ptr and info_ptr */
     png_ptr = png_ptr_orig;
     info_ptr = info_ptr_orig;
     png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
     fclose(fp);
     if (dc)
       dc->SelectObject(NULL);
     /* If we get here, we had a problem reading the file */
     return 0;
   }

   /* Set up the input control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   /* The call to png_read_info() gives us all of the information from the
    * PNG file before the first IDAT (image data chunk).  REQUIRED
    */
   png_read_info(png_ptr, info_ptr);

   png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
		&interlace_type, NULL, NULL);

   if (w_mask) {
     /* Is the mask actually useful? */
     if (!png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)
	 && !(color_type & PNG_COLOR_MASK_ALPHA))
       w_mask = 0;
   }

   if ((bit_depth == 1)
       && (color_type == PNG_COLOR_TYPE_GRAY)
       && !png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) {
     /* Special handling for monochrome so that we don't use 32x
	the necessary memory. */
     is_mono = 1;
   } else {
     /* Normalize formal of returned rows: */
     if (color_type == PNG_COLOR_TYPE_PALETTE)
       png_set_palette_to_rgb(png_ptr);
     if (color_type == PNG_COLOR_TYPE_GRAY ||
	 color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
       png_set_gray_to_rgb(png_ptr);
     if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
       png_set_tRNS_to_alpha(png_ptr);
     if (bit_depth == 16)
       png_set_strip_16(png_ptr);

     /* Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel */
#if PNG_LIBPNG_VER >= 10400
     png_set_expand_gray_1_2_4_to_8(png_ptr);
#else
     png_set_gray_1_2_4_to_8(png_ptr);
#endif
   }

   /* Set the background color to draw transparent and alpha images over.
    * It is possible to set the red, green, and blue components directly
    * for paletted images instead of supplying a palette index.  Note that
    * even if the PNG file supplies a background, you are not required to
    * use it - you should use the (solid) application background if it has one.
    */
   if (!w_mask && !is_mono) {
     png_color_16 *image_background;

     if (!bg && png_get_bKGD(png_ptr, info_ptr, &image_background))
       png_set_background(png_ptr, image_background,
			  PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
     else {
       png_color_16 my_background;
       
       if (bg) {
	 int g;
	 my_background.red = bg->Red();
	 my_background.green = bg->Green();
	 my_background.blue = bg->Blue();
	 g = (((int)my_background.red) 
	      + ((int)my_background.green)
	      + ((int)my_background.blue)) / 3;
	 my_background.gray = g;
       } else {
	 my_background.red = 0xff;
	 my_background.green = 0xff;
	 my_background.blue = 0xff;
	 my_background.gray = 0xff;
       }

       if (bit_depth == 16) {
	 my_background.red = (my_background.red << 8) | my_background.red;
	 my_background.green = (my_background.green << 8) | my_background.green;
	 my_background.blue = (my_background.blue << 8) | my_background.blue;
	 my_background.gray = (my_background.gray << 8) | my_background.gray;
       }

       png_set_background(png_ptr, &my_background,
			  PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
     }
   }

   /* Gamma correction --- only if the file has a gamma.
      This gamma correction messes with the ability of
      PNGs to keep exact RGB information, for the many
      cases where that could make sense. So no gamma
      data in the file means we won't try to correct it. */
   {
     double gamma;

     if (png_get_gAMA(png_ptr, info_ptr, &gamma)) {
       double screen_gamma;
       char *gamma_str;
       char buf[30];
       
       if (wxGetPreference("gamma", buf, 30)) {
	 screen_gamma = (double)atof(buf);
       } else if ((gamma_str = getenv("SCREEN_GAMMA"))) {
	 screen_gamma = (double)atof(gamma_str);
       } else
	 screen_gamma = 0;
       
       if (!(screen_gamma > 0.0) || !(screen_gamma < 10.0)) {
	 /* Guess */
#ifdef wx_mac
	 screen_gamma = 1.7;  /* A good guess for Mac systems */
#else
	 screen_gamma = 2.0; /* A good guess for a PC monitor */
#endif
       }

       png_set_gamma(png_ptr, screen_gamma, gamma);
     }
   }

   if (w_mask && !is_mono) {
     /* Add filler (or alpha) byte (before/after each RGB triplet) */
     /* Actually, invert so that it's a mask. */
     png_set_filler(png_ptr, 0, PNG_FILLER_AFTER);
     png_set_invert_alpha(png_ptr);
   }

   /* Turn on interlace handling.  REQUIRED if you are not using
    * png_read_image().  To see how to handle interlacing passes,
    * see the png_read_row() method below:
    */
   number_passes = png_set_interlace_handling(png_ptr);

   /* Optional call to gamma correct and add the background to the palette
    * and update info structure.  REQUIRED if you are expecting libpng to
    * update the palette for you (ie you selected such a transform above).
    */
   png_read_update_info(png_ptr, info_ptr);

   /* Allocate the memory to hold the image using the fields of info_ptr. */

#ifdef MZ_PRECISE_GC
   rows = (png_bytep *)GC_malloc(sizeof(png_bytep) * height);
#else
   rows = new WXGC_PTRS png_bytep[height];
#endif

   row_width = png_get_rowbytes(png_ptr, info_ptr);
   for (y = 0; y < height; y++) {
     row = new WXGC_ATOMIC png_byte[row_width];
     rows[y] = row;
   }

   dc = create_dc(width, height, bm, is_mono);
   if (!dc) {
     if (dc)
       dc->SelectObject(NULL);
     png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
     fclose(fp);
     return 0;
   }

   for (pass = 0; pass < number_passes; pass++) {
     png_read_rows(png_ptr, rows, NULL, height);
   }

   if (is_mono) {
     png_start_lines(dc, mdc, width, height);
     for (y = 0; y < height; y++) {
       png_draw_line1(rows[y], width, y, dc);
     }
     png_end_lines(dc, mdc);
   } else {
     if (w_mask) {
       int mono_mask;
       unsigned int x;

       /* Will a monochrome mask do? */
       for (y = 0; y < height; y++) {
	 row = rows[y];
	 for (x = 0; x < width; x++) {
	   int val;
	   val = row[(x * 4) + 3];
	   if ((val != 0) && (val != 255))
	     break;
	 }
	 if (x < width)
	   break;
       } 

       mono_mask = ((y < height) ? 0 : 1);

       mbm = new WXGC_PTRS wxBitmap(width, height, mono_mask);
       if (mbm->Ok())
	 mdc = create_dc(-1, -1, mbm, mono_mask);
       else
	 mdc = NULL;
     }

     png_start_lines(dc, mdc, width, height);
     for (y = 0; y < height; y++) {
       png_draw_line(rows[y], width, y, dc, mdc, w_mask ? 4 : 3);
     }
     png_end_lines(dc, mdc);
   }

   /* read rest of file, and get additional chunks in info_ptr - REQUIRED */
   png_read_end(png_ptr, info_ptr);

   /* At this point you have read the entire image */

   /* clean up after the read, and free any memory allocated - REQUIRED */
   png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

   /* close the file */
   fclose(fp);

   dc->SelectObject(NULL);
   if (mdc) {
     mdc->SelectObject(NULL);
     bm->SetMask(mbm);
   }

   /* that's it */
   return 1;
}

int wx_write_png(char *file_name, wxBitmap *bm)
{
   png_structp png_ptr;
   png_structp volatile png_ptr_orig;
   png_infop info_ptr;
   png_infop volatile info_ptr_orig;
   int width, height;
   int bit_depth, color_type, row_width;
   int y;
   FILE *volatile fp;
   png_bytep *rows, row;
   wxMemoryDC * volatile dc = NULL;
   wxMemoryDC * volatile mdc = NULL;
   wxBitmap *mbm = NULL;
   volatile int desel = 1;
   volatile int mdesel = 1;

   if ((fp = wxFOpen(file_name, wx_WB_mode)) == NULL)
     return 0;

   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  REQUIRED
    */
   png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, 
				     CAST_UEP user_error_proc, 
				     CAST_UWP user_warn_proc);

   if (png_ptr == NULL)
   {
      fclose(fp);
      return 0;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_write_struct(&png_ptr, NULL);
      return 0;
   }

   /* Set error handling if you are using the setjmp/longjmp method (this is
    * the normal method of doing things with libpng).  REQUIRED unless you
    * set up your own error handlers in the png_create_read_struct() earlier.
    */

   png_ptr_orig = png_ptr;
   info_ptr_orig = info_ptr;
   if (setjmp(png_ptr->jmpbuf)) {
     /* Free all of the memory associated with the png_ptr and info_ptr */
     png_ptr = png_ptr_orig;
     info_ptr = info_ptr_orig;
     png_destroy_write_struct(&png_ptr, &info_ptr);
     fclose(fp);
     if (dc && desel)
       dc->SelectObject(NULL);
     if (mdc && mdesel)
       mdc->SelectObject(NULL);
     /* If we get here, we had a problem reading the file */
     return 0;
   }

   /* Set up the input control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   width = bm->GetWidth();
   height = bm->GetHeight();
   bit_depth = 8;
   
   mbm = bm->GetMask();
   if (mbm && mbm->Ok() && (mbm->GetWidth() == width) &&  (mbm->GetHeight() == height))
     color_type = PNG_COLOR_TYPE_RGB_ALPHA;
   else {
     color_type = PNG_COLOR_TYPE_RGB;
     mbm = NULL;
   }

   if ((bm->GetDepth() == 1) && !mbm) {
     bit_depth = 1;
     color_type = PNG_COLOR_TYPE_GRAY;
   }

   /* Set the image information here.  Width and height are up to 2^31,
    * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
    * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
    * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
    * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
    * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
    * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
    */
   png_set_IHDR(png_ptr, info_ptr, width, height, bit_depth, color_type,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, 
		PNG_FILTER_TYPE_DEFAULT);

   if (mbm)
     png_set_invert_alpha(png_ptr);

   /* Write the file header information.  REQUIRED */
   png_write_info(png_ptr, info_ptr);

   /* Allocate the memory to hold the image using the fields of info_ptr. */
#ifdef MZ_PRECISE_GC
   rows = (png_bytep *)GC_malloc(sizeof(png_bytep) * height);
#else
   rows = new WXGC_PTRS png_bytep[height];
#endif
   row_width = png_get_rowbytes(png_ptr, info_ptr);
   for (y = 0; y < height; y++) {
     row = new WXGC_ATOMIC png_byte[row_width];
     rows[y] = row;
   }

   dc = create_reader_dc(bm, &desel);
   if (mbm)
     mdc = create_reader_dc(mbm, &mdesel);
   else
     mdc = NULL;

   if (bit_depth == 1) {
     for (y = 0; y < height; y++) {
       png_get_line1(rows[y], width, y, dc);
     }
   } else {
     for (y = 0; y < height; y++) {
       png_get_line(rows[y], width, y, dc, mdc);
     }
   }

   png_write_image(png_ptr, rows);

   png_write_end(png_ptr, info_ptr);

   /* clean up after the write, and free any memory allocated */
   png_destroy_write_struct(&png_ptr, &info_ptr);

   /* close the file */
   fclose(fp);

   if (desel)
     dc->SelectObject(NULL);
   if (mdc && mdesel) {
     mdc->SelectObject(NULL);
   }

   /* that's it */
   return 1;
}
