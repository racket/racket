/*
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

/*****************************************************************************\
*  RdFToI.c:                                                                  *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create the image and possibly its mask               *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#include <sys/stat.h>

LFUNC(OpenReadFile, int, (char *filename, xpmData *mdata));
LFUNC(xpmDataClose, void, (xpmData *mdata));

int
XpmReadFileToImage(display, filename,
		   image_return, shapeimage_return, attributes)
    Display *display;
    char *filename;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    XpmImage image;
    XpmInfo info;
    int ErrorStatus;

    /* create an XpmImage from the file */
    if (attributes) {
	xpmInitAttributes(attributes);
	xpmSetInfoMask(&info, attributes);
	ErrorStatus = XpmReadFileToXpmImage(filename, &image, &info);
    } else
	ErrorStatus = XpmReadFileToXpmImage(filename, &image, NULL);

    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    /* create the related ximages */
    ErrorStatus = XpmCreateImageFromXpmImage(display, &image,
					     image_return, shapeimage_return,
					     attributes);
    if (attributes) {
	if (ErrorStatus >= 0)		/* no fatal error */
	    xpmSetAttributes(attributes, &image, &info);
	XpmFreeXpmInfo(&info);
    }
    /* free the XpmImage */
    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}

int
XpmReadFileToXpmImage(filename, image, info)
    char *filename;
    XpmImage *image;
    XpmInfo *info;
{
    xpmData mdata;
    int ErrorStatus;

    /* init returned values */
    xpmInitXpmImage(image);
    xpmInitXpmInfo(info);

    /* open file to read */
    if ((ErrorStatus = OpenReadFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    /* create the XpmImage from the XpmData */
    ErrorStatus = xpmParseData(&mdata, image, info);

    xpmDataClose(&mdata);

    return (ErrorStatus);
}

/*
 * open the given file to be read as an xpmData which is returned.
 */
static int
OpenReadFile(filename, mdata)
    char *filename;
    xpmData *mdata;
{
#ifdef ZPIPE
    char *compressfile, buf[BUFSIZ];
    struct stat status;

#endif

    if (!filename) {
	mdata->stream.file = (stdin);
	mdata->type = XPMFILE;
    } else {
#ifdef ZPIPE
	if (((int) strlen(filename) > 2) &&
	    !strcmp(".Z", filename + (strlen(filename) - 2))) {
	    mdata->type = XPMPIPE;
	    sprintf(buf, "uncompress -c \"%s\"", filename);
	    if (!(mdata->stream.file = popen(buf, "r")))
		return (XpmOpenFailed);

	} else if (((int) strlen(filename) > 3) &&
		   !strcmp(".gz", filename + (strlen(filename) - 3))) {
	    mdata->type = XPMPIPE;
	    sprintf(buf, "gunzip -qc \"%s\"", filename);
	    if (!(mdata->stream.file = popen(buf, "r")))
		return (XpmOpenFailed);

	} else {
	    if (!(compressfile = (char *) XpmMalloc(strlen(filename) + 4)))
		return (XpmNoMemory);

	    strcpy(compressfile, filename);
	    strcat(compressfile, ".Z");
	    if (!stat(compressfile, &status)) {
		sprintf(buf, "uncompress -c \"%s\"", compressfile);
		if (!(mdata->stream.file = popen(buf, "r"))) {
		    XpmFree(compressfile);
		    return (XpmOpenFailed);
		}
		mdata->type = XPMPIPE;
	    } else {
		strcpy(compressfile, filename);
		strcat(compressfile, ".gz");
		if (!stat(compressfile, &status)) {
		    sprintf(buf, "gunzip -c \"%s\"", compressfile);
		    if (!(mdata->stream.file = popen(buf, "r"))) {
			XpmFree(compressfile);
			return (XpmOpenFailed);
		    }
		    mdata->type = XPMPIPE;
		} else {
#endif
		    if (!(mdata->stream.file = fopen(filename, "r"))) {
#ifdef ZPIPE
			XpmFree(compressfile);
#endif
			return (XpmOpenFailed);
		    }
		    mdata->type = XPMFILE;
#ifdef ZPIPE
		}
	    }
	    XpmFree(compressfile);
	}
#endif
    }
    mdata->CommentLength = 0;
    return (XpmSuccess);
}

/*
 * close the file related to the xpmData if any
 */
static void
xpmDataClose(mdata)
    xpmData *mdata;
{
    switch (mdata->type) {
    case XPMFILE:
	if (mdata->stream.file != (stdin))
	    fclose(mdata->stream.file);
	break;
#ifdef ZPIPE
    case XPMPIPE:
	pclose(mdata->stream.file);
	break;
#endif
    }
}
