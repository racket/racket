/* $Id: plfreetype.h,v 1.1 2004/03/01 20:54:51 cozmic Exp $
 *
 *    Header file for Support routines for freetype font engine
 *
 *    See plfreetype.c for more details
 */

#ifndef __PLFREETY_H__
#define __PLFREETY_H__

#ifdef HAVE_FREETYPE

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include FT_OUTLINE_H

#define FT_Data _FT_Data_

typedef void (*plD_pixel_fp)     (PLStream *, PLINT, PLINT);

/*--------------------------------------------------------------------------*\
 * Define the FT_Data data structure.
 *
 * These are the "globalish" variables used by Freetype
 * They are kept here so they are moderately thread safe, and stream specific
\*--------------------------------------------------------------------------*/

typedef struct FT_Data {
    short               x;
    short               y;

    char *textbuf;		/* temporary string buffer */

    PLFLT		scale;
/*
 *  If set to 1, scale won't do anything, but this is an "arbitrary" scale
 *  factor for the transformation between virtual and real coordinates. This
 *  is included to fix up the problem with the "hidden line removal bug" of
 *  the 3D plots, which is fixed by providing a super-scaled image. This
 *  should be a mirror, for example, of dev->scale in the PNG driver. If I
 *  was thinking 12 months ahead, I would have put that scale factor in
 *  "pls", not "dev", but at this late stage, we can just live with it
 *  now...
 */

    unsigned char       greek;

    unsigned char       invert_y;
/*
 *  Set "invert_y" to 1 if the y coordinates need to be inverted for
 *  plotting. Most bitmaps will need this.
 */

    short               ymax;
/*
 *  ymax should be equal to, what it says - the maximum y coordinate of the
 *  bitmap. This is used in the process of calculating the inversion of the
 *  bitmap when invert_y is set to 1. If invert_y isn't set, this setting is
 *  ignored.
 */


    plD_pixel_fp        pixel;          /* pointer to a function which draws a single pixel */


    int want_smooth_text; /* flag to request text smoothing (won't */
                          /* necessarily get it though */
    int smooth_text;      /* Flag to indicate type of anti-aliasing used, if freetype text is active */


    char                font_name[5][1024];
/*
 *  List of font names and paths corresponding to the "predefined" fonts of
 *  plplot. 1024 chars is presumably generous for each one's length, but at
 *  least we probably won't get in trouble this way.
 */


    PLINT               cfont;
/*
 *  This is a mirror of pls->cfont and is basically used for detecting when
 *  fonts have been changed .
 */


    FT_Matrix           matrix;         /* used for rotating etc... the font. */
    FT_Vector           pos;            /* used for calculating offsets of text boxes/sizes */


/*
 *  The next few variables hold the original size of CMAP0, the number of
 *  extra slots added for anti-aliasing, and the "width" of the table used
 *  for anti-aliasing.
 */

    PLINT ncol0_org;            /* Original number of colours in CMAP0 */
    PLINT ncol0_xtra;           /* number of extra colours defined in CMAP0 for anti-aliasing */
    PLINT ncol0_width;          /* Number of greyscale levels for each of the original colours */
    PLINT last_icol0;           /* Last colour in cmap0, which should be one of the originals */


/*
 *  The rest of the variables should be considered very much PRIVATE, and
 *  more to the point, subject to change.
 *
 *  Don't rely on them existing in future versions of plplot's freetype
 *  support. If/when the Freetype cache manager is added to plplot, most, if
 *  not all, of these variables will move elsewhere.
 */

    FT_Library          library;        /* pointer to freetype library      */
    FT_Face             face;           /* pointer to a font face           */
    FT_GlyphSlot        slot;           /* pointer to a glyph slot          */
    FT_Glyph            image;          /* bitmap or outline image of font  */

    short               colour;         /* depreciated ?? must check code */

    PLINT shade, col_idx;		/* Used for antialiasing */

} FT_Data;

#endif


#endif /* __PLFREETY_H__ */
