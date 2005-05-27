/** zyzgauge.c
 *
 *  DESCRIPTION: 
 *      Yet another 'Gas Gauge Custom Control.'  This control gives you
 *      a 'progress bar' class (named zYzGauge) for use in your applications.
 *      You can set the range, position, font, color, orientation, and 3d
 *      effect of the gauge by sending messages to the control.
 *
 *      Before you can use this control, you MUST first export the window
 *      procedure for the control (or define it with the _export keyword):
 *
 *          EXPORTS     gaugeWndProc
 *
 *      You then need initialize the class before you use it:
 *
 *          if (!gaugeInit(hInstance))
 *              die a horrible death
 *          else
 *              you are good to go
 *
 *      The colors used by the control default to black and white if you
 *      are running on a mono-display.  They default to blue and white
 *      if you are on a color display.  You enable the 3D effect by setting
 *      the ZYZGS_3D style flag in the styles field of the control (like
 *      any other control).
 *
 *      To select your own colors, you can send the ZYZG_SETFGCOLOR and
 *      ZYZG_SETBKCOLOR messages to set the foreground (percent done) and
 *      background (percent not done) colors.  The lParam is the RGB()
 *      value--wParam is ignored.
 *
 *      In all of the following ZYZG_??? messages, the arguments are
 *      WORDS.  If you are setting parameters, the value is sent as
 *      the wParam (lParam is ignored).  If you are getting parameters,
 *      the value is returned as a LONG and should be cast to a *signed*
 *      integer.
 *
 *      To set the depth of the 3D effect (if enabled), you can send the
 *      ZYZG_SETBEZELFACE and ZYZG_SETWIDTH3D messages.  The bezel face
 *      is the flat top on the 3D border--its color will be that of the
 *      button-face.  The 3D width is the width of the bezel itself; inside
 *      and outside.  The light color is white, the dark color is gray.
 *      Both widths *can* be zero--both default to 2 which looks to me.
 *
 *      The range of the control can be set by sending the ZYZG_SETRANGE
 *      message to the control.  It can be any integer from 1 to 32767.
 *      What this specifies is the number of pieces that create a whole.
 *      The default is 100.  You can get the current range setting by
 *      sending the ZYZG_GETRANGE message to the control.
 *
 *      The position (number of pieces out of the whole have been used) is
 *      set with the ZYZG_SETPOSITION message.  It can be any integer from
 *      0 to the current range setting of the control--it will be clipped
 *      if the position is out of bounds.  The default position is 0.  You
 *      can get the current position at any time with the ZYZG_GETPOSITION
 *      message.
 *
 *      You can also set the range using a delta from the current range.
 *      This is done by sending the ZYZG_SETDELTAPOS message with wParam
 *      set to a _signed_ integer value within the range of the control.
 *
 *      The font used for the percentage text can be set using the standard
 *      WM_SETFONT message.  You can get the current font at any time with
 *      the WM_GETFONT message.
 *
 *      The orientation can be left to right, right to left, bottom to top,
 *      or top to bottom.  Whatever suits your needs.  You set this by
 *      sending the ZYZG_ORIENTATION message to the control with one of
 *      the following values (default is ZYZG_ORIENT_LEFTTORIGHT):
 *
 *          ZYZG_ORIENT_LEFTTORIGHT (0)
 *          ZYZG_ORIENT_RIGHTTOLEFT (1)
 *          ZYZG_ORIENT_BOTTOMTOTOP (2)
 *          ZYZG_ORIENT_TOPTOBOTTOM (3)
 *
 *  HISTORY:
 *      3/12/91     cjp     put in this comment
 *      6/19/92     cjp     touched it a bit
 *
 ** cjp */
// COPYRIGHT:
//
//   (C) Copyright Microsoft Corp. 1992.  All rights reserved.
//
//   You have a royalty-free right to use, modify, reproduce and
//   distribute the Sample Files (and/or any modified version) in
//   any way you find useful, provided that you agree that
//   Microsoft has no warranty obligations or liability for any
//   Sample Application Files which are modified.
//


/* get the includes we need */
#include <windows.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include "zyz3d.h"
#include "zyzgauge.h"


/* static global variables */
static char gszzYzGaugeClass[] = "zYzGauge";
    

/* window word position definitions */
#define ZYZG_WW_PZYZGAUGE   0
/* #define ZYZG_WW_EXTRABYTES  2 */
#define ZYZG_WW_EXTRABYTES  4


/* control block structure typedef */
typedef struct tZYZGAUGE
{
    WORD    wRange;
    WORD    wPosition;
    WORD    wOrientation;
    WORD    wWidth3D;
    WORD    wWidthBezelFace;
    HFONT   hFont;
    DWORD   rgbTextColor;
    DWORD   rgbBkColor;

} ZYZGAUGE, *PZYZGAUGE, FAR *LPZYZGAUGE;


/* some default values for the control */
#define ZYZG_DEF_RANGE          100
#define ZYZG_DEF_POSITION       0
#define ZYZG_DEF_ORIENTATION    ZYZG_ORIENT_LEFTTORIGHT
#define ZYZG_DEF_WIDTH3D        2
#define ZYZG_DEF_BEZELFACE      2



/* the default settings for drawing colors--display dependent */
static DWORD    rgbDefTextColor;
static DWORD    rgbDefBkColor;
static BOOL     fSupport3D;

#if !defined(APIENTRY)	// NT defines APIENTRY, 3.x not
#define APIENTRY FAR PASCAL
#endif
 
#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
typedef signed short int SHORT ;
#endif

/* internal function prototypes */
static void NEAR PASCAL gaugePaint(HWND, HDC);
/* LRESULT FAR PASCAL */
LRESULT APIENTRY _EXPORT gaugeWndProc(HWND, UINT, WPARAM, LPARAM);



/** BOOL FAR PASCAL gaugeInit(HINSTANCE hInstance)
 *
 *  DESCRIPTION: 
 *      Registers the window class for the zYzGauge control.  Performs
 *      other initialization for the zYzGauge text control.  This must
 *      be done before the zYzGauge control is used--or it will fail
 *      and your dialog box will not open!
 *
 *  ARGUMENTS:
 *      HINSTANCE hInstance :   Instance handle to register class with.
 *
 *  RETURN (BOOL FAR):
 *      The return value is TRUE if the zYzGauge class was successfully
 *      registered.  It is FALSE if the initialization fails.
 *
 *  NOTES:
 *
 ** cjp */

//#pragma alloc_text(init, gaugeInit)

BOOL FAR PASCAL gaugeInit(HINSTANCE hInstance)
{
    static BOOL fRegistered = FALSE;
    WNDCLASS    wc;
    HDC         hdc;
    
    /* assume already registered if not first instance */
    if (fRegistered)
        return (TRUE);

    /* fill in the class structure for the zyzgauge control */
    wc.hCursor          = LoadCursor(NULL, IDC_ARROW);
    wc.hIcon            = NULL;
    wc.lpszMenuName     = NULL;
    wc.lpszClassName    = gszzYzGaugeClass;
    wc.hbrBackground    = (HBRUSH)(COLOR_WINDOW + 1);
    wc.hInstance        = hInstance;

#ifdef ZYZGAUGE_DLL
    wc.style            = CS_GLOBALCLASS | CS_HREDRAW | CS_VREDRAW;
#else
    wc.style            = CS_HREDRAW | CS_VREDRAW;
#endif

    wc.lpfnWndProc      = gaugeWndProc;
    wc.cbClsExtra       = 0;
    wc.cbWndExtra       = ZYZG_WW_EXTRABYTES;

    /* attempt to register it--return FALSE if fail */
    if (!RegisterClass(&wc))
        return (FALSE);

    /*  Get a DC to determine whether device is mono or not, and set
     *  default foreground/background colors as appropriate.
     */
    if (hdc = CreateIC("DISPLAY", NULL, NULL, NULL))
    {
        /* check for mono-display */
        if ((GetDeviceCaps(hdc, BITSPIXEL) == 1) &&
             (GetDeviceCaps(hdc, PLANES) == 1))
        {
            /* using a mono DC--white foreground, black background */
            rgbDefTextColor = RGB(255, 255, 255);
            rgbDefBkColor = RGB(0, 0, 0);
        }

        /* good! we have color: blue foreground, white background */
        else 
        {
	    rgbDefTextColor = GetSysColor(COLOR_HIGHLIGHT);
            /* rgbDefTextColor = RGB(0, 0, 255); */
            rgbDefBkColor = RGB(255, 255, 255);
        }

        /* need at _least_ 8 for two shades of gray (>=VGA) */
        fSupport3D = (GetDeviceCaps(hdc, NUMCOLORS) >= 8) ? TRUE : FALSE;

        /* get rid of the DC (IC) */
        DeleteDC(hdc);
    }

    /* uh-oh... can't get DC (IC)... fail */
    else
    {
        /* unregister the class */
        UnregisterClass(gszzYzGaugeClass, hInstance);
        return (FALSE);
    }

    /* return success */
    return (fRegistered = TRUE);
} /* gaugeInit() */


/** static void NEAR PASCAL gaugePaint(HWND hwnd, HDC hdc)
 *
 *  DESCRIPTION: 
 *      This function is responsible for painting the zYzGauge control.
 *
 *  ARGUMENTS:
 *      HWND hwnd   :   The window handle for the gauge.
 *
 *      HDC hdc     :   The DC for the gauge's window.
 *
 *  RETURN (void):
 *      The control will have been painted.
 *
 *  NOTES:
 *
 ** cjp */

static void NEAR PASCAL gaugePaint(HWND hwnd, HDC hdc)
{
    PZYZGAUGE   pgauge;
    WORD        iRange, iPos;
    WORD        wOffset;
    DWORD       dwExtent;

    DWORD       dwW, dwH;
    RECT        rc1, rc2;
    HFONT       hFont;
    char        ach[ 6 ];
    WORD        dx, dy, wGomerX, wGomerY;
/* Win32s has no GetTextExtent(); let's try GetTextExtentPoint() instead,
 * which needs a SIZE* parameter */
#if defined(WIN32)
    SIZE size;

    TEXTMETRIC tm;
#endif

    /* get pointer to the control's control block */
//    pgauge = (PZYZGAUGE)GetWindowWord(hwnd, ZYZG_WW_PZYZGAUGE);
    pgauge = (PZYZGAUGE)GetWindowLong(hwnd, ZYZG_WW_PZYZGAUGE);

    /* set the colors into for the gauge into the control */
    SetTextColor(hdc, pgauge->rgbTextColor);
    SetBkColor(hdc, pgauge->rgbBkColor);

    /* draw black rectangle for gauge */
    GetClientRect(hwnd, &rc1);

    /* draw a black border on the _outside_ */
    FrameRect(hdc, &rc1, GetStockObject(BLACK_BRUSH));

    /* we want to draw _just inside_ the black border */
    InflateRect(&rc1, -2, -2);

    /* one line thick so far... */
    wOffset = 1;

    /* for 3D stuff, we need to have at least two shades of gray */
    if ((GetWindowLong(hwnd, GWL_STYLE) & ZYZGS_3D) && fSupport3D)
    {
        Draw3DRect(hdc, &rc1, pgauge->wWidth3D, DRAW3D_OUT);
	InflateRect(&rc1, ~(pgauge->wWidth3D), ~(pgauge->wWidth3D));

        Draw3DFaceFrame(hdc, &rc1, pgauge->wWidthBezelFace);
	InflateRect(&rc1, ~(pgauge->wWidthBezelFace), ~(pgauge->wWidthBezelFace));

        Draw3DRect(hdc, &rc1, pgauge->wWidth3D, DRAW3D_IN);
	InflateRect(&rc1, ~(pgauge->wWidth3D), ~(pgauge->wWidth3D));

        /* draw a black border on the _inside_ */
        FrameRect(hdc, &rc1, GetStockObject(BLACK_BRUSH));

        /* we want to draw _just inside_ the black border */
        InflateRect(&rc1, -1, -1);

        /* add all the other pixels into the border width */
        wOffset += (2 * pgauge->wWidth3D) + pgauge->wWidthBezelFace + 1;
    }
   
    /* dup--one rc for 'how much filled', one rc for 'how much empty' */
    rc2 = rc1;

    /* get the range--make sure it's a valid range */
    if ((iRange = pgauge->wRange) <= 0)
        iRange = 1;

    /* get the position--greater than 100% would be bad */
    if ((iPos = pgauge->wPosition) > iRange)
        iPos = iRange;

    /* compute the actual size of the gauge */
    dx = rc1.right - rc1.left;
    dy = rc1.bottom - rc1.top;
    wGomerX = (WORD)((DWORD)iPos * dx / iRange);
    wGomerY = (WORD)((DWORD)iPos * dy / iRange);

    /* get the orientation and munge rects accordingly */
    switch (pgauge->wOrientation)
    {
        case ZYZG_ORIENT_RIGHTTOLEFT:
            rc1.left = rc2.right = rc1.right - wGomerX;
            break;

        case ZYZG_ORIENT_BOTTOMTOTOP:
            rc1.top = rc2.bottom = rc1.bottom - wGomerY;
            break;

        case ZYZG_ORIENT_TOPTOBOTTOM:
            rc1.bottom = rc2.top += wGomerY;
            break;

        default:
            rc1.right = rc2.left += wGomerX;
            break;
    } /* switch () */

    /* select the correct font */
    hFont = SelectObject(hdc, pgauge->hFont);

    /* build up a string to blit out--ie the meaning of life: "42%" */
#ifdef SHOW_GAUGE_PRECENT
    wsprintf(ach, "%3d%%", (WORD)((DWORD)iPos * 100 / iRange));
#else
    ach[0] = 0;
#endif

/* Win32s has no GetTextExtent(); let's try GetTextExtentPoint() instead */
#if defined(WIN32)
    GetTextExtentPoint(hdc, ach, wGomerX = lstrlen(ach), &size);
    dwW = size.cx;

    dwH = size.cy;

    GetTextMetrics(hdc, &tm);

    dwH -= tm.tmDescent; /* percentage never has a descent */
#else
    dwExtent = GetTextExtent(hdc, ach, wGomerX = lstrlen(ach));
#endif  


    /*  Draw the finished (ie the percent done) side of box.  If
     *  ZYZG_WW_POSITION is 42, (in range of 0 to 100) this ExtTextOut
     *  draws the meaning of life (42%) bar.
     */
    ExtTextOut(hdc, (dx - dwW) / 2 + wOffset,
                    (dy - dwH) / 2 + wOffset,
                    ETO_OPAQUE | ETO_CLIPPED, &rc2, ach, wGomerX, NULL);

    /*  Reverse fore and back colors for drawing the undone (ie the non-
     *  finished) side of the box.
     */
    SetBkColor(hdc, pgauge->rgbTextColor);
    SetTextColor(hdc, pgauge->rgbBkColor);

    ExtTextOut(hdc, (dx - dwW) / 2 + wOffset,
                    (dy - dwH) / 2 + wOffset,
                    ETO_OPAQUE | ETO_CLIPPED, &rc1, ach, wGomerX, NULL);


    SetBkColor(hdc, pgauge->rgbBkColor);

    /* unselect the font */
    SelectObject(hdc, hFont);

} /* gaugePaint() */
  

/** LRESULT FAR PASCAL gaugeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
 *
 *  DESCRIPTION: 
 *      This is the control's window procedure.  Its purpose is to handle
 *      special messages for this custom control.
 *
 *      The special control messages for the gauge control are:
 *
 *          ZYZG_SETRANGE       :   Sets the range of the gauge.  In other
 *                                  words, the number of parts that make a
 *                                  whole.
 *
 *          ZYZG_GETRANGE       :   Returns the current range of the gauge.
 *
 *          ZYZG_SETORIENTATION :   Sets the orientation of the gauge.  This
 *                                  can be one of the ZYZG_ORIENT_?? msgs.
 *
 *          ZYZG_GETORIENTATION :   Gets the current orientation of the 
 *                                  gauge.
 *
 *          ZYZG_SETPOSITION    :   Sets the current position of the gauge.
 *                                  In other words, how many pieces of the
 *                                  whole have been used.
 *
 *          ZYZG_GETPOSITION    :   Gets the current position of the gauge.
 *
 *          ZYZG_SETDELTAPOS    :   Sets the position of the gauge +/- the
 *                                  specified amount.
 *
 *          ZYZG_SETFGCOLOR     :   Sets the foreground (percent done) color.
 *
 *          ZYZG_GETFGCOLOR     :   Gets the foreground (percent done) color.
 *
 *          ZYZG_SETBKCOLOR     :   Sets the background (percent not done)
 *                                  color.
 *
 *          ZYZG_GETBKCOLOR     :   Gets the background (percent not done)
 *                                  color.
 *
 *          WM_SETFONT          :   Sets the font to use for the percentage
 *                                  text of the gauge.
 *
 *          WM_GETFONT          :   Gets the current font in use by the
 *                                  gauge.
 *
 *  NOTES:
 *
 ** cjp */

/* LRESULT FAR PASCAL */

LRESULT APIENTRY _EXPORT gaugeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    HFONT       hFont;
    PAINTSTRUCT ps;
    PZYZGAUGE   pgauge;
    RECT        rc;

//    pgauge = (PZYZGAUGE)GetWindowWord(hwnd, ZYZG_WW_PZYZGAUGE);
    pgauge = (PZYZGAUGE)GetWindowLong(hwnd, ZYZG_WW_PZYZGAUGE);

    /* break to get DefWindowProc() */
    switch (uMsg)
    {
        case WM_CREATE:
            /* need to allocate a control block */
//            pgauge = (PZYZGAUGE)LocalAlloc(LPTR, sizeof(ZYZGAUGE));
            pgauge = (PZYZGAUGE)malloc(sizeof(ZYZGAUGE));
            if (!pgauge)
                return (0L);

            /* hang on to this control block */
//            SetWindowWord(hwnd, ZYZG_WW_PZYZGAUGE, (WORD)pgauge);
            SetWindowLong(hwnd, ZYZG_WW_PZYZGAUGE, (LONG)pgauge);

            /* fill control block with defaults */
            pgauge->wRange           = ZYZG_DEF_RANGE;
            pgauge->wPosition        = ZYZG_DEF_POSITION;
            pgauge->wOrientation     = ZYZG_DEF_ORIENTATION;
            pgauge->wWidth3D         = ZYZG_DEF_WIDTH3D;
            pgauge->wWidthBezelFace  = ZYZG_DEF_BEZELFACE;
            pgauge->rgbTextColor     = rgbDefTextColor;
            pgauge->rgbBkColor       = rgbDefBkColor;

            /* use system font */
            SendMessage(hwnd, WM_SETFONT, NULL, 0L);

            /* go to DefWindowProc() to finish the job */
            break;

        case WM_DESTROY:
            /* get rid of the control's memory */
            if (pgauge)
//                LocalFree((HANDLE)pgauge);
                free(pgauge);
            break;

        case ZYZG_GETPOSITION:
            return (pgauge->wPosition);

        case ZYZG_GETRANGE:
            return (pgauge->wRange);

        case ZYZG_GETORIENTATION:
            return (pgauge->wOrientation);

        case ZYZG_GETWIDTH3D:
            return (pgauge->wWidth3D);

        case ZYZG_GETBEZELFACE:
            return (pgauge->wWidthBezelFace);

        case ZYZG_GETBKCOLOR:
            return (pgauge->rgbTextColor);

        case ZYZG_GETFGCOLOR:
            return (pgauge->rgbBkColor);

        case ZYZG_SETBKCOLOR:
            pgauge->rgbBkColor = lParam;
            return (0L);

        case ZYZG_SETFGCOLOR:
            pgauge->rgbTextColor = lParam;
            return (0L);


        case ZYZG_SETPOSITION:
            pgauge->wPosition = wParam;

zyzgForceRepaint:
            GetClientRect(hwnd, &rc);
            if ((GetWindowLong(hwnd, GWL_STYLE) & ZYZGS_3D) && fSupport3D)
            {
                wParam = (2 * pgauge->wWidth3D) +
                            pgauge->wWidthBezelFace + 2;
            }

            else
                wParam = 1;

	    InflateRect(&rc, ~(wParam), ~(wParam));
            InvalidateRect(hwnd, &rc, FALSE);
            UpdateWindow(hwnd);
            return (0L);

        case ZYZG_SETRANGE:
            pgauge->wRange = wParam;
            goto zyzgForceRepaint;

        case ZYZG_SETORIENTATION:
            pgauge->wOrientation = wParam;
            goto zyzgForceRepaint;

        case ZYZG_SETWIDTH3D:
            pgauge->wWidth3D = wParam;

zyzgForceRepaint3D:
            InvalidateRect(hwnd, NULL, FALSE);
            UpdateWindow(hwnd);
            return (0L);

        case ZYZG_SETBEZELFACE:
            pgauge->wWidthBezelFace = wParam;
            goto zyzgForceRepaint3D;

        case ZYZG_SETDELTAPOS:
/* Watcom doesn't like the following line so removing typecasts */
/*            (int)pgauge->wPosition += (int)wParam; */
            pgauge->wPosition += wParam;
            goto zyzgForceRepaint;

        case WM_PAINT:
            BeginPaint(hwnd, &ps);
            gaugePaint(hwnd, ps.hdc);
            EndPaint(hwnd, &ps);
            return (0L);

        case WM_GETFONT:
            hFont = pgauge->hFont;

            /* if system font, then return NULL handle */
            return ((hFont == GetStockObject(SYSTEM_FONT)) ? NULL : hFont);

        case WM_SETFONT:
            /* if NULL hFont, use system font */
            if (!(hFont = (HFONT)wParam))
                hFont = GetStockObject(SYSTEM_FONT);

            pgauge->hFont = hFont;

            /* redraw if indicated in message */
            if ((BOOL)lParam)
            {
                InvalidateRect(hwnd, NULL, TRUE);
                UpdateWindow(hwnd);
            }
            return (0L);
    } /* switch () */

    /* let the dialog mangler take care of this message */
    return (DefWindowProc(hwnd, uMsg, wParam, lParam));
} /* gaugeWndProc() */


/** EOF: zyzgauge.c **/
