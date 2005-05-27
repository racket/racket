/*
 *	LongCoords.h
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef __QUICKDRAW__
	#include <Quickdraw.h>
#endif

#if !defined( __LONGCOORDINATES__ )
#if !defined( _LongCoords_ )
#define _LongCoords_

typedef struct LongPt {
	long v;
	long h;
} LongPt;

typedef struct LongRect {
	long top;
	long left;
	long bottom;
	long right;
} LongRect;

pascal long PinInRange(long value, long rangeStart, long rangeEnd);
pascal void LongPointToPoint(const LongPt *lp, Point *p);
pascal void PointToLongPoint(Point p, LongPt *lp);
pascal void SetLongRect(LongRect *lr, long left, long top, long right, long bottom);
pascal void LongRectToRect(const LongRect *lr, Rect *r);
pascal void RectToLongRect(const Rect *r, LongRect *lr);
pascal void OffsetLongRect(LongRect *lr, long hOffset, long vOffset);
pascal Boolean LongPointInLongRect(const LongPt *lp, const LongRect *lr);
pascal Boolean EqualLongPoint(const LongPt *lp1, const LongPt *lp2);

#endif
#endif
