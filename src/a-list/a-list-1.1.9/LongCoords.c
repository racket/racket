/*
 *	LongCoords.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/


#include "LongCoords.h"

enum {
	kQDMin = -32767L,
	kQDMax = +32767L
};

pascal long PinInRange(long value, long rangeStart, long rangeEnd)
{
	return ((value > rangeEnd) ? rangeEnd : ((value < rangeStart) ? rangeStart : value));
}

pascal void LongPointToPoint(const LongPt *lp, Point *p)
{
	p->v = PinInRange(lp->v, kQDMin, kQDMax);
	p->h = PinInRange(lp->h, kQDMin, kQDMax);
}

pascal void PointToLongPoint(Point p, LongPt *lp)
{
	lp->v = p.v;
	lp->h = p.h;
}

pascal void SetLongRect(LongRect *lr, long left, long top, long right, long bottom)
{
	lr->top    = top;
	lr->left   = left;
	lr->bottom = bottom;
	lr->right  = right;
}

pascal void LongRectToRect(const LongRect *lr, Rect *r)
{
	LongPointToPoint((const LongPt *) lr, (Point *) r);
	LongPointToPoint((const LongPt *) lr + 1, (Point *) r + 1);
}

pascal void RectToLongRect(const Rect *r, LongRect *lr)
{
	lr->top    = r->top;
	lr->left   = r->left;
	lr->bottom = r->bottom;
	lr->right  = r->right;
}

pascal void OffsetLongRect(LongRect *lr, long hOffset, long vOffset)
{
	lr->top    += vOffset;
	lr->left   += hOffset;
	lr->bottom += vOffset;
	lr->right  += hOffset;
}

pascal Boolean LongPointInLongRect(const LongPt *lp, const LongRect *lr)
{
	return ((lp->v >= lr->top) && (lp->h >= lr->left) && (lp->v <= lr->bottom) && (lp->h <= lr->right));
}

pascal Boolean EqualLongPoint(const LongPt *lp1, const LongPt *lp2)
{
	return ((lp1->v == lp2->v) && (lp1->h == lp2->h));
}
