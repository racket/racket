/* $Id: pdfutils.c,v 1.1 2004/03/01 20:54:50 cozmic Exp $

    pdf_utils.c

    Copyright (C) 1992, 1993, 1994, 1995
    Maurice LeBrun			mjl@dino.ph.utexas.edu
    Institute for Fusion Studies	University of Texas at Austin

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    These functions do the low-level reading/writing of portable data files.
    Data can be written to/read from either a file handle or memory buffer.
*/

#define NEED_PLDEBUG
#include "plplotP.h"

static void print_ieeef	(void *, void *);
static int  pdf_wrx	(const U_CHAR *x, long nitems, PDFstrm *pdfs);

static int debug = 0;

/*--------------------------------------------------------------------------*\
 * void pdf_set (string, value)
 *
 * Set an option.  Pretty sparse right now but you never know.
\*--------------------------------------------------------------------------*/

void
pdf_set(char *option, int value)
{
    if ( ! strcmp(option, "debug"))
	debug = value;
}

/*--------------------------------------------------------------------------*\
 * pdf_fopen()
 *
 * Initializes a PDFstrm for a file oriented device.
 * Used exactly like fopen().
\*--------------------------------------------------------------------------*/

PDFstrm *
pdf_fopen(char *filename, char *mode)
{
    PDFstrm *pdfs;

    dbug_enter("pdf_fopen");

    pdfs = (PDFstrm *) malloc(sizeof(PDFstrm));

    if (pdfs != NULL) {
	pdfs->buffer = NULL;
	pdfs->file = NULL;
#ifdef PLPLOT_USE_TCL_CHANNELS
	pdfs->tclChan = NULL;
	if (1) {
	    char new_mode[3];
	    int binary = 0;
	    char *m, *p;
	    
	    /* Copy over the mode, removing 'b' if needed */
	    for (m = mode, p = new_mode; *m != 0; m++) {
	        if (*m == 'b') {
	            binary = 1;
	        } else {
		    *p = *m;
		    p++;
	        }
	    }
	    *p = 0;
	    
	    pdfs->tclChan = Tcl_OpenFileChannel(NULL, filename, new_mode, 0);
	    if (pdfs->tclChan == NULL) {
		pdf_close(pdfs);
		pdfs = NULL;
	    } else {
		if (binary) {
		    Tcl_SetChannelOption(NULL, pdfs->tclChan, "-translation", 
					 "binary");
		}
	    }
	}
#else
	pdfs->file = fopen(filename, mode);
	if (pdfs->file == NULL) {
	    pdf_close(pdfs);
	    pdfs = NULL;
	}
#endif
    }

    return pdfs;
}

/*--------------------------------------------------------------------------*\
 * pdf_bopen()
 *
 * Initializes a PDFstrm for reading/writing to a memory buffer.
 * If buffer is NULL, a standard buffer is allocated.
\*--------------------------------------------------------------------------*/

PDFstrm *
pdf_bopen(U_CHAR *buffer, long bufmax)
{
    PDFstrm *pdfs;

    dbug_enter("pdf_bopen");

    pdfs = (PDFstrm *) malloc(sizeof(PDFstrm));

    if (pdfs != NULL) {
	pdfs->file = NULL;
#ifdef PLPLOT_USE_TCL_CHANNELS
	pdfs->tclChan = NULL;
#endif
	pdfs->bp = 0;

	if (buffer == NULL) {
	    if (bufmax > 0)
		pdfs->bufmax = bufmax;
	    else
		pdfs->bufmax = 2048;

	    pdfs->buffer = (U_CHAR *) malloc(pdfs->bufmax);
	    if (pdfs->buffer == NULL) {
		pdf_close(pdfs);
		pdfs = NULL;
	    }
	}
	else {
	    pdfs->bufmax = bufmax;
	    pdfs->buffer = buffer;
	}
    }

    return pdfs;
}

/*--------------------------------------------------------------------------*\
 * pdf_finit()
 *
 * Initializes a PDFstrm for a file oriented device.
 * Like pdf_fopen() but an existing file handle is specified.
\*--------------------------------------------------------------------------*/

PDFstrm *
pdf_finit(FILE *file)
{
    PDFstrm *pdfs;

    dbug_enter("pdf_finit");

    pdfs = (PDFstrm *) malloc(sizeof(PDFstrm));

    if (pdfs != NULL) {
	pdfs->buffer = NULL;
	pdfs->file = file;
#ifdef PLPLOT_USE_TCL_CHANNELS
	pdfs->tclChan = NULL;
#endif
	pdfs->bp = 0;
    }

    return pdfs;
}

/*--------------------------------------------------------------------------*\
 * pdf_close()
 *
 * Closes a PDFstrm.
 * Used exactly like fclose().
\*--------------------------------------------------------------------------*/

int
pdf_close(PDFstrm *pdfs)
{
    dbug_enter("pdf_close");

    if (pdfs != NULL) {
	if (pdfs->file != NULL) {
	    fclose(pdfs->file);
#ifdef PLPLOT_USE_TCL_CHANNELS
	} else if (pdfs->tclChan != NULL) {
	    Tcl_Close(NULL, pdfs->tclChan);
#endif
	} else if (pdfs->buffer != NULL) {
	    free ((void *) pdfs->buffer);
	}
	free((void *) pdfs);
    }
    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_putc()
 *
 * Writes a single character.
\*--------------------------------------------------------------------------*/

int
pdf_putc(int c, PDFstrm *pdfs)
{
    int result = EOF;

    if (pdfs->file != NULL) {
	result = putc(c, pdfs->file);
	pdfs->bp++;
#ifdef PLPLOT_USE_TCL_CHANNELS
    } else if (pdfs->tclChan != NULL) {
	result = Tcl_WriteChars(pdfs->tclChan, &c, 1);
	pdfs->bp++;
#endif
    } else if (pdfs->buffer != NULL) {
	if (pdfs->bp >= pdfs->bufmax) {
	    pldebug("pdf_putc",
		    "Increasing buffer to %d bytes\n", pdfs->bufmax);
	    pdfs->bufmax += 512;
	    pdfs->buffer = (U_CHAR *)
		realloc((void *) pdfs->buffer, pdfs->bufmax);
	}
	pdfs->buffer[pdfs->bp++] = c;
	result = c;
    }
    else
	plexit("pdf_putc: Illegal operation");

    return result;
}

/*--------------------------------------------------------------------------*\
 * int pdf_getc()
 *
 * Reads a single character.
\*--------------------------------------------------------------------------*/

int
pdf_getc(PDFstrm *pdfs)
{
    int result = EOF;

    if (pdfs->file != NULL) {
	result = getc(pdfs->file);
	pdfs->bp++;
#ifdef PLPLOT_USE_TCL_CHANNELS
    } else if (pdfs->tclChan != NULL) {
	result = Tcl_Read(pdfs->tclChan, &result, 1);
	pdfs->bp++;
#endif
    } else if (pdfs->buffer != NULL) {
	if (pdfs->bp < pdfs->bufmax)
	    result = pdfs->buffer[pdfs->bp++];
    }
    else
	plexit("pdf_getc: Illegal operation");

    return result;
}

/*--------------------------------------------------------------------------*\
 * int pdf_ungetc()
 *
 * Push back the last command read.
\*--------------------------------------------------------------------------*/

int
pdf_ungetc(int c, PDFstrm *pdfs)
{
    int result = EOF;

    if (pdfs->file != NULL) {
	result = ungetc(c, pdfs->file);
	if (pdfs->bp > 0) 
	    pdfs->bp--;
#ifdef PLPLOT_USE_TCL_CHANNELS
    } else if (pdfs->tclChan != NULL) {
	result = Tcl_Ungets(pdfs->tclChan, &c, 1, 0);
	if (pdfs->bp > 0) 
	    pdfs->bp--;
#endif
    } else if (pdfs->buffer != NULL) {
	if (pdfs->bp > 0) {
	    pdfs->buffer[--pdfs->bp] = c;
	    result = c;
	}
    }
    else
	plexit("pdf_ungetc: Illegal operation");

    return result;
}

/*--------------------------------------------------------------------------*\
 * int pdf_wrx()
 *
 * Writes a record.
\*--------------------------------------------------------------------------*/

static int
pdf_wrx(const U_CHAR *x, long nitems, PDFstrm *pdfs)
{
    int i, result = 0;

    if (pdfs->file != NULL) {
	result = fwrite(x, 1, nitems, pdfs->file);
	pdfs->bp += nitems;
#ifdef PLPLOT_USE_TCL_CHANNELS
    } else if (pdfs->tclChan != NULL) {
	result = Tcl_Write(pdfs->tclChan, x, nitems);
	pdfs->bp += nitems;
#endif
    } else if (pdfs->buffer != NULL) {
	for (i = 0; i < nitems; i++) {
	    if (pdfs->bp >= pdfs->bufmax) {
		pldebug("pdf_wrx",
			"Increasing buffer to %d bytes\n", pdfs->bufmax);
		pdfs->bufmax += 512;
		pdfs->buffer = (U_CHAR *)
		    realloc((void *) (pdfs->buffer), pdfs->bufmax);
	    }
	    pdfs->buffer[pdfs->bp++] = x[i];
	}
	result = i;
    }

    return result;
}

/*--------------------------------------------------------------------------*\
 * int pdf_rdx()
 *
 * Reads a record.
\*--------------------------------------------------------------------------*/

int
pdf_rdx(U_CHAR *x, long nitems, PDFstrm *pdfs)
{
    int i, result = 0;

    if (pdfs->file != NULL) {
	result = fread(x, 1, nitems, pdfs->file);
	pdfs->bp += nitems;
#ifdef PLPLOT_USE_TCL_CHANNELS
    } else if (pdfs->tclChan != NULL) {
	result = Tcl_ReadRaw(pdfs->tclChan, x, nitems);
	pdfs->bp += nitems;
#endif
    } else if (pdfs->buffer != NULL) {
	for (i = 0; i < nitems; i++) {
	    if (pdfs->bp > pdfs->bufmax)
		break;
	    x[i] = pdfs->buffer[pdfs->bp++];
	}
	result = i;
    }

    return result;
}

/*--------------------------------------------------------------------------*\
 * pdf_wr_header()
 *
 * Writes a header string.  Input string must be NULL-terminated.  The
 * written string is terminated by a new-line, not a NULL.  This is done
 * so you can type e.g. "% strings <file> | head" and get sensible output.
\*--------------------------------------------------------------------------*/

int
pdf_wr_header(PDFstrm *pdfs, char *header)
{
    int i;

    dbug_enter("pdf_wr_header");

    for (i = 0; i < 79; i++) {
	if (header[i] == '\0')
	    break;
	if (pdf_putc(header[i], pdfs) == EOF)
	    return PDF_WRERR;
    }
    if (pdf_putc('\n', pdfs) == EOF)
	return PDF_WRERR;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_rd_header
 *
 * Reads a newline-terminated header string from PDFstrm *pdfs, and
 * converts to a usual NULL-terminated string.  80 chars maximum assumed.
\*--------------------------------------------------------------------------*/

int
pdf_rd_header(PDFstrm *pdfs, char *header)
{
    int i, c;

    dbug_enter("pdf_rd_header");

    for (i = 0; i < 79; i++) {
	if ((c = pdf_getc(pdfs)) == EOF)
	    return PDF_RDERR;

	header[i] = c;
	if (header[i] == '\n')
	    break;
    }
    header[i] = '\0';		/* NULL terminate */
    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_wr_string()
 *
 * Writes a null-terminated string.
\*--------------------------------------------------------------------------*/

int
pdf_wr_string(PDFstrm *pdfs, const char *string)
{
    int i;

    dbug_enter("pdf_wr_string");

    for (i = 0; i <= strlen(string); i++) {
	if (pdf_putc(string[i], pdfs) == EOF)
	    return PDF_WRERR;
    }

    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_rd_string
 *
 * Reads a null-terminated string from PDFstrm *pdfs.
 * A max of nmax chars are read.
\*--------------------------------------------------------------------------*/

int
pdf_rd_string(PDFstrm *pdfs, char *string, int nmax)
{
    int i, c;

    dbug_enter("pdf_rd_string");

    for (i = 0; i < nmax; i++) {
	if ((c = pdf_getc(pdfs)) == EOF)
	    return PDF_RDERR;

	string[i] = c;
	if (c == '\0')
	    break;
    }
    string[i] = '\0';		/* handle boundary case */
    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_wr_1byte()
 *
 * Writes a U_CHAR as a single byte.
\*--------------------------------------------------------------------------*/

int
pdf_wr_1byte(PDFstrm *pdfs, U_CHAR s)
{
    U_CHAR x[1];

    x[0] = s;
    if (pdf_wrx(x, 1, pdfs) != 1)
	return PDF_WRERR;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_rd_1byte()
 *
 * Reads a single byte, storing into a U_CHAR.
\*--------------------------------------------------------------------------*/

int
pdf_rd_1byte(PDFstrm *pdfs, U_CHAR *ps)
{
    U_CHAR x[1];

    if ( ! pdf_rdx(x, 1, pdfs))
	return PDF_RDERR;

    *ps = ((U_CHAR) x[0]);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_wr_2bytes()
 *
 * Writes a U_SHORT as two single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_wr_2bytes(PDFstrm *pdfs, U_SHORT s)
{
    U_CHAR x[2];

    x[0] = (U_CHAR) ((U_LONG) (s & (U_LONG) 0x00FF));
    x[1] = (U_CHAR) ((U_LONG) (s & (U_LONG) 0xFF00) >> 8);

    if (pdf_wrx(x, 2, pdfs) != 2)
	return PDF_WRERR;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_rd_2bytes()
 *
 * Reads a U_SHORT from two single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_rd_2bytes(PDFstrm *pdfs, U_SHORT *ps)
{
    U_CHAR x[2];

    if ( ! pdf_rdx(x, 2, pdfs))
	return PDF_RDERR;

    *ps = 0;
    *ps |= (U_LONG) x[0];
    *ps |= (U_LONG) x[1] << 8;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_wr_2nbytes()
 *
 * Writes n U_SHORT's as 2n single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_wr_2nbytes(PDFstrm *pdfs, U_SHORT *s, PLINT n)
{
    PLINT i;
    U_CHAR x[2];

    for (i = 0; i < n; i++) {
	x[0] = (U_CHAR) ((U_LONG) (s[i] & (U_LONG) 0x00FF));
	x[1] = (U_CHAR) ((U_LONG) (s[i] & (U_LONG) 0xFF00) >> 8);

	if (pdf_wrx(x, 2, pdfs) != 2)
	    return PDF_WRERR;
    }
    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_rd_2nbytes()
 *
 * Reads n U_SHORT's from 2n single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_rd_2nbytes(PDFstrm *pdfs, U_SHORT *s, PLINT n)
{
    PLINT i;
    U_CHAR x[2];

    for (i = 0; i < n; i++) {
	if ( ! pdf_rdx(x, 2, pdfs))
	    return PDF_RDERR;

	s[i] = 0;
	s[i] |= (U_SHORT) x[0];
	s[i] |= (U_SHORT) x[1] << 8;
    }
    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_wr_4bytes()
 *
 * Writes an unsigned long as four single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_wr_4bytes(PDFstrm *pdfs, U_LONG s)
{
    U_CHAR x[4];

    x[0] = (U_CHAR) ((s & (U_LONG) 0x000000FF));
    x[1] = (U_CHAR) ((s & (U_LONG) 0x0000FF00) >> 8);
    x[2] = (U_CHAR) ((s & (U_LONG) 0x00FF0000) >> 16);
    x[3] = (U_CHAR) ((s & (U_LONG) 0xFF000000) >> 24);

    if (pdf_wrx(x, 4, pdfs) != 4)
	return PDF_WRERR;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * pdf_rd_4bytes()
 *
 * Reads an unsigned long from 4 single bytes, low end first.
\*--------------------------------------------------------------------------*/

int
pdf_rd_4bytes(PDFstrm *pdfs, U_LONG *ps)
{
    U_CHAR x[4];

    if ( ! pdf_rdx(x, 4, pdfs))
	return PDF_RDERR;

    *ps = 0;
    *ps |= (U_LONG) x[0];
    *ps |= (U_LONG) x[1] << 8;
    *ps |= (U_LONG) x[2] << 16;
    *ps |= (U_LONG) x[3] << 24;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * Here is the IEEE floating point specification in both 32 bit and 64 bit
 * precisions, from page 9 of "IEEE Standard for Binary Floating-Point
 * Arithmetic", copyright 1985, IEEE Std 754-1985:
 * 
 * 
 *                             Single Format
 * 
 * msb means most significant bit
 * lsb means least significant bit
 * 
 *   1         8                                23
 * _____________________________________________________________________
 * |   |                |                                              |
 * | s |       e        |                        f                     |
 * |___|________________|______________________________________________|
 *      msb          lsb msb                                        lsb
 * 
 * 
 * 
 *                             Double Format
 * 
 * msb means most significant bit
 * lsb means least significant bit
 * 
 *   1        11                                52
 * _____________________________________________________________________
 * |   |                |                                              |
 * | s |       e        |                        f                     |
 * |___|________________|______________________________________________|
 *      msb          lsb msb                                        lsb
 * 
 * 
 * (Thanks to: Andy Mai (mai@ncar.ucar.edu))
 * 
 * 
 * According to "inmos: Transputer instruction set" the IEEE standard
 * specifies the floating format as:
 * 
 *      s exp frac
 * 
 * Where: s = sign bit  (1 bit)
 *      exp = exponent (8 bits for 32 bit float / 11 bits for 64 bit float)
 *      frac = fraction (23 bits for 32 bit float / 52 bits for 64 bit float)
 * 
 * value of (s exp frac) = (-1)^s * 1.frac * 2^(exp-bias) ; if exp not 0
 *                         (-1)^s * 0.frac * 2^(1-bias) ; if exp = 0
 * 
 * where bias = 127 for 32 bit float
 *       bias = 1023 for 64 bit float
 * 
 * (Thanks to: Tom Bjorkholm(TBJORKHOLM@abo.fi))
 * 
\*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*\
 * int pdf_wr_ieeef()
 *
 * Writes a float in IEEE single precision (32 bit) format.
\*--------------------------------------------------------------------------*/

int
pdf_wr_ieeef(PDFstrm *pdfs, float f)
{
    double fdbl, fmant, f_new;
    float fsgl, f_tmp;
    int istat, exp, e_new, e_off, bias = 127;
    U_LONG value, s_ieee, e_ieee, f_ieee;

    if (f == 0.0) {
	value = 0;
	return (pdf_wr_4bytes(pdfs, value));
    }
    fsgl = fdbl = f;
    fmant = frexp(fdbl, &exp);

    if (fmant < 0)
	s_ieee = 1;
    else
	s_ieee = 0;

    fmant = fabs(fmant);
    f_new = 2 * fmant;
    e_new = exp - 1;

    if (e_new < 1 - bias) {
	e_off = e_new - (1 - bias);
	e_ieee = 0;
	f_tmp = f_new * pow((double) 2.0, (double) e_off);
    }
    else {
	e_ieee = e_new + bias;
	f_tmp = f_new - 1;
    }
    f_ieee = f_tmp * 8388608;		/* multiply by 2^23 */

    if (e_ieee > 255) {
	if (debug)
	    fprintf(stderr, "pdf_wr_ieeef: Warning -- overflow\n");
	e_ieee = 255;
    }

    s_ieee = s_ieee << 31;
    e_ieee = e_ieee << 23;

    value = s_ieee | e_ieee | f_ieee;

    if ((istat = pdf_wr_4bytes(pdfs, value)))
	return (istat);

    if (debug) {
	fprintf(stderr, "Float value (written):      %g\n", fsgl);
	print_ieeef(&fsgl, &value);
    }

    return 0;
}

/*--------------------------------------------------------------------------*\
 * int pdf_rd_ieeef()
 *
 * Reads a float from a IEEE single precision (32 bit) format.
\*--------------------------------------------------------------------------*/

int
pdf_rd_ieeef(PDFstrm *pdfs, float *pf)
{
    double f_new, f_tmp;
    float fsgl;
    int istat, exp, bias = 127;
    U_LONG value, s_ieee, e_ieee, f_ieee;

    if ((istat = pdf_rd_4bytes(pdfs, &value)))
	return (istat);

    s_ieee = (value & (U_LONG) 0x80000000) >> 31;
    e_ieee = (value & (U_LONG) 0x7F800000) >> 23;
    f_ieee = (value & (U_LONG) 0x007FFFFF);

    f_tmp = (double) f_ieee / 8388608.0;	/* divide by 2^23 */

    if (e_ieee == 0) {
	exp = 1 - bias;
	f_new = f_tmp;
    }
    else {
	exp = (int) e_ieee - bias;
	f_new = 1.0 + f_tmp;
    }

    fsgl = f_new * pow(2.0, (double) exp);
    if (s_ieee == 1)
	fsgl = -fsgl;

    *pf = fsgl;

    if (debug) {
	fprintf(stderr, "Float value (read):      %g\n", fsgl);
	print_ieeef(&fsgl, &value);
    }

    return 0;
}

/*--------------------------------------------------------------------------*\
 * print_ieeef()
 *
 * Prints binary representation for numbers pointed to by arguments.
 * The first argument is the original float, the second is the
 * IEEE representation.  They should be the same on any machine that
 * uses IEEE floats.
\*--------------------------------------------------------------------------*/

static void
print_ieeef(void *vx, void *vy)
{
    int i;
    U_LONG f, *x = (U_LONG *) vx, *y = (U_LONG *) vy;
    char bitrep[33];

    bitrep[32] = '\0';

    f = *x;
    for (i = 0; i < 32; i++) {
	if (f & 1)
	    bitrep[32 - i - 1] = '1';
	else
	    bitrep[32 - i - 1] = '0';
	f = f >> 1;
    }
    fprintf(stderr, "Binary representation:      ");
    fprintf(stderr, "%s\n", bitrep);

    f = *y;
    for (i = 0; i < 32; i++) {
	if (f & 1)
	    bitrep[32 - i - 1] = '1';
	else
	    bitrep[32 - i - 1] = '0';
	f = f >> 1;
    }
    fprintf(stderr, "Converted representation:   ");
    fprintf(stderr, "%s\n\n", bitrep);

    return;
}

/*--------------------------------------------------------------------------*\
 * plAlloc2dGrid()
 *
 * Allocates a block of memory for use as a 2-d grid of PLFLT's.
 * Resulting array can be indexed as f[i][j] anywhere.  This is to be used
 * instead of PLFLT f[nx][ny], which is less useful.  Note that this type
 * of allocation is required by the PLplot functions which take a 2-d
 * grids of PLFLT's as an argument, such as plcont() and plot3d().
 * Example usage:
 *
 *   PLFLT **z;
 *
 *   Alloc2dGrid(&z, XPTS, YPTS);
\*--------------------------------------------------------------------------*/

void
plAlloc2dGrid(PLFLT ***f, PLINT nx, PLINT ny)
{
    PLINT i;

    if ((*f = (PLFLT **) calloc(nx, sizeof(PLFLT *)))==NULL)
        plexit("Memory allocation error in \"plAlloc2dGrid\"");

    for (i = 0; i < nx; i++) {
	if (((*f)[i] = (PLFLT *) calloc(ny ,sizeof(PLFLT)))==NULL)
	   plexit("Memory allocation error in \"plAlloc2dGrid\"");
    }

}

/*--------------------------------------------------------------------------*\
 * Free2dGrid()
 *
 * Frees a block of memory allocated with Alloc2dGrid().
\*--------------------------------------------------------------------------*/

void
plFree2dGrid(PLFLT **f, PLINT nx, PLINT ny)
{
    PLINT i;

    for (i = 0; i < nx; i++)
	free((void *) f[i]);

    free((void *) f);
}

/*--------------------------------------------------------------------------*\
 * MinMax2dGrid()
 *
 * Finds the maximum and minimum of a 2d matrix allocated with plAllc2dGrid().
\*--------------------------------------------------------------------------*/

void
plMinMax2dGrid(PLFLT **f, PLINT nx, PLINT ny, PLFLT *fmax, PLFLT *fmin)
{
    int i, j;
    PLFLT m, M;

    M = m = f[0][0];

    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    if (f[i][j] > M) M = f[i][j];
	    if (f[i][j] < m) m = f[i][j];
	}
    }
    *fmax = M;
    *fmin = m;
}
