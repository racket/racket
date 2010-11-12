/* $Id: metadefs.h,v 1.1 2004/03/01 20:54:50 cozmic Exp $

	Geoffrey Furnish
	5 May 1991
	
	This file contains definitions of constants and structures which
	are needed by the PLplot metafile writer and renderer.
*/

/*
* PLMETA_HEADER holds the magic string at head of metafile.
* PLMETA_VERSION holds the version number (year & letter).
* 
* Note: All strings written into the file header are limited to a maximum
* of 80 characters.
*/

#define PLMETA_HEADER	"PLPLOT"
#define PLMETA_VERSION	"1993c"

/* These are used by the TK driver client/server code */

#define PLSERV_HEADER	"PLPLOT"
#define PLSERV_VERSION	"1993b"

/* Symbolic constants for old metafile versions (prior to 1992a). */
/* Now these are stored in the metafile header. */

#define PLMETA_X_OLD	10000
#define PLMETA_Y_OLD	10000

/* Virtual dots/mm for our virtual display space. */

#define PIXEL_RES_X_OLD		42
#define PIXEL_RES_Y_OLD		56

/* Macros to make it easier to abort on nonzero return code */
/* Can't call plexit on a write failure since that would be circular */

#define plm_wr(code) \
    if (code) { fprintf(stderr, "Unable to write to MetaFile\n"); exit(1); }

#define plm_rd(code) \
    if (code) plexit( "Unable to read from MetaFile" )

/*
   Metafile commands.

   *** NOTICE !!! ***
   If you change ANY of the following, you will wreck backward
   backward compatibility with old metafiles.  You may add, but do
   NOT delete !!!
*/

#define INITIALIZE	1
#define CLOSE		2
#define SWITCH_TO_TEXT	3	/* Obsolete, replaced by ESCAPE */
#define SWITCH_TO_GRAPH	4	/* Obsolete, replaced by ESCAPE */
#define EOP		5
#define BOP		6
#define NEW_COLOR	7	/* Obsolete, replaced by CHANGE_STATE */
#define NEW_WIDTH	8	/* Obsolete, replaced by CHANGE_STATE */
#define LINE		9
#define LINETO		10
#define ESCAPE		11
#define ADVANCE		12	/* Obsolete, BOP/EOP used instead */
#define POLYLINE	13
#define NEW_COLOR0 	NEW_COLOR
#define NEW_COLOR1	14
#define CHANGE_STATE	15
#define BOP0		16	/* First BOP in a file */
#define END_OF_FIELD	255
