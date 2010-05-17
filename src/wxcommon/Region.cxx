/********************************************************/
/*                       Regions                        */
/********************************************************/

#ifndef wxPI
# define wxPI 3.141592653589793
#endif

#define CAIRO_DEV ((cairo_t *)target)

#ifdef wx_mac
typedef struct {
  CGContextRef cg;
  CGMutablePathRef path;
} PathTarget;
# define CGPATH ((PathTarget *)target)->path
# define CGCG ((PathTarget *)target)->cg
# define PathTargetPath_t CGMutablePathRef
# define CGXFORM (&current_xform)
# define PATHPATH ((CGMutablePathRef)target)
#endif

#ifdef wx_msw
typedef struct {
  Graphics *g;
  GraphicsPath *path;
  int did_one;
} PathTarget;
# define GP ((PathTarget *)target)->path
# define GP_G ((PathTarget *)target)->g
# define GP_DID_ONE ((PathTarget *)target)->did_one
# define PathTargetPath_t GraphicsPath*
# define CURRENT_GP current_path
# define PATH_GP ((GraphicsPath *)target)
#endif

#ifndef WX_USE_CAIRO
typedef int cairo_matrix_p;
#endif

/**************************************************************************/

wxRegion::wxRegion(wxDC *_dc, wxRegion *r, Bool _no_prgn)
{
  dc = _dc;
  is_ps = wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT);
  locked = 0;
 
#ifdef wx_msw
  lazy_rgn = NULL;
#endif
#ifdef wx_x
  rgn = NULL;
#endif
#ifdef wx_mac
  rgn = NULL;
#endif
  prgn = NULL;
  no_prgn = _no_prgn;
  if (r) Union(r);
}

wxRegion::~wxRegion()
{
  Cleanup();
}

void wxRegion::Cleanup()
{  
#ifdef wx_msw
  if (lazy_rgn) {
    if (real_rgn)
      lazy_rgn->DoneRgn(real_rgn);
    lazy_rgn = NULL;
  }
#endif
#ifdef wx_x
  if (rgn) {
    XDestroyRegion(rgn);
    rgn = NULL;
  }
#endif
#ifdef wx_mac
  if (rgn) {
    DisposeRgn(rgn);
    rgn = NULL;
  }
#endif
  if (!no_prgn) {
    prgn = NULL;
  }
}

void wxRegion::Lock(int delta)
{
#ifdef wx_msw
  if (!locked) {
    if (lazy_rgn) {
      real_rgn = lazy_rgn->GetRgn();
    }
  }
#endif  

  locked += delta;

#ifdef wx_msw
  if (!locked) {
    if (lazy_rgn) {
      lazy_rgn->DoneRgn(real_rgn);
      real_rgn = NULL;
    }
  }
#endif
}

#ifdef wx_msw
HRGN wxRegion::GetRgn()
{
  return real_rgn;
}
#endif

void wxRegion::SetRectangle(double x, double y, double width, double height)
{
  double xw, yh;
  int ix, iy, iw, ih;

  Cleanup();

  if (!no_prgn) {
    prgn = new WXGC_PTRS wxRectanglePathRgn(dc, x, y, width, height);
  }

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToUnscrolledDeviceX(x);
  y = dc->FLogicalToUnscrolledDeviceY(y);
  xw = dc->FLogicalToUnscrolledDeviceX(xw);
  width = xw - x;
  yh = dc->FLogicalToUnscrolledDeviceY(yh);
  height = yh - y;

  if (is_ps) {
    /* So bitmap-based region is right */
    height = -height;
    y  = -y;
  }

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

#ifdef wx_msw
  lazy_rgn = new RectLazyRgn(ix, iy, iw, ih);
#endif
#ifdef wx_x
  {
    XRectangle r;
    rgn = XCreateRegion();
    r.x = ix;
    r.y = iy;
    r.width = iw;
    r.height = ih;
    XUnionRectWithRegion(&r, rgn, rgn);
  }
#endif
#ifdef wx_mac
  rgn = NewRgn();
  SetRectRgn(rgn, ix, iy, ix + iw, iy + ih);
#endif
}

void wxRegion::SetRoundedRectangle(double x, double y, double width, double height, double radius)
{
#ifdef wx_xt
  wxRegion *lt, *rt, *lb, *rb, *w, *h, *r;
#endif
#if defined(wx_msw) || defined(wx_mac)
  double xw, yh;
  int ix, iy, iw, ih;
  int xradius, yradius;
#endif

  Cleanup();

  if (!no_prgn) {
    prgn = new WXGC_PTRS wxRoundedRectanglePathRgn(dc, x, y, width, height, radius);
  }

  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    double smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (double)(- radius * smallest);
  } else
    radius = dc->FLogicalToDeviceXRel(radius);

#ifdef wx_x
  lt = new WXGC_PTRS wxRegion(dc, NULL, TRUE);
  rt = new WXGC_PTRS wxRegion(dc, NULL, TRUE);
  lb = new WXGC_PTRS wxRegion(dc, NULL, TRUE);
  rb = new WXGC_PTRS wxRegion(dc, NULL, TRUE);
  w = new WXGC_PTRS wxRegion(dc, NULL, TRUE);
  h = new WXGC_PTRS wxRegion(dc, NULL, TRUE);

  lt->SetEllipse(x, y, 2 * radius, 2 * radius);
  rt->SetEllipse(x + width - 2 * radius, y, 2 * radius, 2 * radius);
  rb->SetEllipse(x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius);
  lb->SetEllipse(x, y + height - 2 * radius, 2 * radius, 2 * radius);

  w->SetRectangle(x, y + radius, width, height - 2 * radius);
  h->SetRectangle(x + radius, y, width - 2 * radius, height);

  r = lt;
  r->Union(rt);
  r->Union(lb);
  r->Union(rb);
  r->Union(w);
  r->Union(h);

  /* A little hack: steal rgn from r: */
  rgn = r->rgn;
  r->rgn = NULL;
#else
  /* Windows and Mac */
  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToUnscrolledDeviceX(x);
  y = dc->FLogicalToUnscrolledDeviceY(y);
  width = dc->FLogicalToUnscrolledDeviceX(xw) - x;
  height = dc->FLogicalToUnscrolledDeviceY(yh) - y;
  xradius = (int)(dc->FLogicalToDeviceXRel(radius));
  yradius = (int)(dc->FLogicalToDeviceYRel(radius));

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

  if (is_ps) {
    /* So bitmap-based region is right */
    height = -height;
    y = -y;
  }

# ifdef wx_msw
  lazy_rgn = new RoundRectLazyRgn(ix, iy, iw, ih, xradius, yradius);
# endif
# ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    {
      Rect r2;
      SetRect(&r2, ix, iy, ix + iw, iy + ih);
      FrameRoundRect(&r2, xradius, yradius);
      CloseRgn(rgn);
    }

    ::SetGWorld(savep, savegd);
  }    
# endif
#endif
}

void wxRegion::SetEllipse(double x, double y, double width, double height)
{
  double xw, yh;
#if defined(wx_msw) || defined(wx_mac)
  int ix, iy, iw, ih;
#endif

  Cleanup();

  if (!no_prgn) {
#ifdef WX_USE_CAIRO
    /* cairo_arc() went bad for clipping, so we avoid it. */
    {
      wxPath *p;
      p = new WXGC_PTRS wxPath();
      p->Arc(x, y, width, height, 0, 2 * wxPI, FALSE);
      p->Close();
      prgn = new WXGC_PTRS wxPathPathRgn(dc, p, 0, 0, wxWINDING_RULE);
    }
#else
    prgn = new WXGC_PTRS wxArcPathRgn(dc, x, y, width, height, 0, 2 * wxPI);
#endif
  }

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToUnscrolledDeviceX(x);
  y = dc->FLogicalToUnscrolledDeviceY(y);
  width = dc->FLogicalToUnscrolledDeviceX(xw) - x;
  height = dc->FLogicalToUnscrolledDeviceY(yh) - y;

  if (is_ps) {
    /* So bitmap-based region is right */
    height = -height;
    y = -y;
  }

#if defined(wx_msw) || defined(wx_mac)
  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;
#endif

#ifdef wx_msw
  lazy_rgn = new EllipticLazyRgn(ix, iy, iw, ih);
#endif
#ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    {
      Rect r;
      SetRect(&r, ix, iy, ix + iw, iy + ih);
      FrameOval(&r);
      CloseRgn(rgn);
    }

    ::SetGWorld(savep, savegd);
  }
#endif

#ifdef wx_x
  {
    int npoints;
    XPoint *p;
    p = wxEllipseToPolygon(width, height, x, y, &npoints);
    rgn = XPolygonRegion(p, npoints - 1, WindingRule);
  }
#endif
}

#ifdef wx_x
# define POINT XPoint
#endif
#ifdef wx_mac
# define POINT MyPoint
  typedef struct { int x, y; } MyPoint;
#endif

typedef struct { double x, y; } FPoint;

void wxRegion::SetPolygon(int n, wxPoint points[], double xoffset, double yoffset, int fillStyle, int delta)
{
  POINT *cpoints;
  FPoint *fpoints;
  int i, v;
  double vf;

  Cleanup();

  if (n < 2)
    return;

  if (!no_prgn) {
    prgn = new WXGC_PTRS wxPolygonPathRgn(dc, n, points, xoffset, yoffset, fillStyle);
  }

  cpoints = new WXGC_ATOMIC POINT[n];
  fpoints = (is_ps ? new WXGC_ATOMIC FPoint[n] : (FPoint *)NULL);
  for (i = 0; i < n; i++) {
    v = dc->LogicalToUnscrolledDeviceX(points[i+delta].x + xoffset);
    cpoints[i].x = v;
    v = dc->LogicalToUnscrolledDeviceY(points[i+delta].y + yoffset);
    cpoints[i].y = v;
    if (fpoints) {
      vf = dc->FLogicalToUnscrolledDeviceX(points[i+delta].x + xoffset);
      fpoints[i].x = vf;
      vf = dc->FLogicalToUnscrolledDeviceY(points[i+delta].y + yoffset);
      fpoints[i].y = vf;
    }
  }

  if (is_ps) {
    /* So bitmap-based region is right */
    for (i = 0; i < n; i++) {
      cpoints[i].y = -cpoints[i].y;
    }
  }

#ifdef wx_msw
  lazy_rgn = new PolygonLazyRgn(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
#endif
#ifdef wx_x
  rgn = XPolygonRegion(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? EvenOddRule : WindingRule);
#endif
#ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    MoveTo(cpoints[0].x, cpoints[0].y);
    for (i = 0; i < n; i++) {
      LineTo(cpoints[i].x, cpoints[i].y);
    }
    LineTo(cpoints[0].x, cpoints[0].y);
    CloseRgn(rgn);

    ::SetGWorld(savep, savegd);
  }
#endif
}

void wxRegion::SetPath(wxPath *p, double xoffset, double yoffset, int fillStyle)
{
  double **ptss;
  int *lens, cnt, i, total_cnt, j, k;
  wxPoint *a;

  Cleanup();

  if (!no_prgn) {
    prgn = new WXGC_PTRS wxPathPathRgn(dc, p, xoffset, yoffset, fillStyle);
    no_prgn = 1;
  }

  cnt = p->ToPolygons(&lens, &ptss, 1.0, 1.0);

  if (!cnt)
    return;
  
  total_cnt = 0;
  for (i = 0; i < cnt; i++) {
    total_cnt += (lens[i] / 2);
  }

#ifdef MZ_PRECISE_GC
  a = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * total_cnt);
#else
  a = new WXGC_ATOMIC wxPoint[total_cnt];
#endif

  for (i = 0, k = 0; i < cnt; i++) {
    for (j = 0; j < lens[i]; j += 2) {
      a[k].x = ptss[i][j];
      a[k].y = ptss[i][j+1];
      k++;
    }
  }

  if (cnt == 1) {
    SetPolygon(total_cnt, a, xoffset, yoffset, fillStyle, 0);
  } else {
    for (i = 0, k = 0; i < cnt; i++) {
      j = (lens[i] / 2);
      if (i == 0)
	SetPolygon(j, a, xoffset, yoffset, fillStyle, k);
      else {
	wxRegion *r;
	r = new WXGC_PTRS wxRegion(dc, NULL, 1);
	r->SetPolygon(j, a, xoffset, yoffset, fillStyle, k);
	Xor(r);
	DELETE_OBJ r;
      }
      k += j;
    }
  }      
  
  no_prgn = 0;
}

void wxRegion::SetArc(double x, double y, double w, double h, double start, double end)
{
  wxRegion *r;
  static double pi;
  int saw_start = 0, saw_end = 0, closed = 0;
  double cx, cy;
  wxPoint *a;
  int n;
  char save_no_prgn;

#ifdef MZ_PRECISE_GC
  a = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * 20);
#else
  a = new WXGC_ATOMIC wxPoint[20];
#endif

  save_no_prgn = no_prgn;
  if (!no_prgn) {
#ifdef WX_USE_CAIRO
    /* cairo_arc() went bad for clipping, so we avoid it. */
    {
      wxPath *p;
      p = new WXGC_PTRS wxPath();
      p->MoveTo(x + w / 2, y + h / 2);
      p->Arc(x, y, w, h, end, start, FALSE);
      p->Close();
      prgn = new WXGC_PTRS wxPathPathRgn(dc, p, 0, 0, wxWINDING_RULE);
    }
#else
    prgn = new WXGC_PTRS wxArcPathRgn(dc, x, y, w, h, start, end);
#endif
    no_prgn = 1;
  }

  SetEllipse(x, y, w, h);

  if (start == end) return;

  r = new WXGC_PTRS wxRegion(dc, NULL, TRUE);

  if (!pi)
    pi = 2 * asin((double)1.0);

  start = fmod((double)start, 2*pi);
  end = fmod((double)end, 2*pi);
  if (start < 0)
    start += 2*pi;
  if (end < 0)
    end += 2*pi;

  cx = x + w/2;
  cy = y + h/2;

  a[0].x = ((w+2) / 2) * cos(end) + cx;
  a[0].y = ((h+2) / 2) * (-sin(end)) + cy;

  a[1].x = cx;
  a[1].y = cy;

  a[2].x = ((w+2) / 2) * cos(start) + cx;
  a[2].y = ((h+2) / 2) * (-sin(start)) + cy;

  n = 3;

  if (!saw_start && (start < (pi / 2)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y - 2;
  } else
    closed = saw_start;

  if (!saw_start && (start < pi))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x - 2;
    a[n++].y = cy;
  } else
    closed = saw_start;

  if (!saw_start && (start < (1.5 * pi)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y + h + 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h + 2;
  } else
    closed = saw_start;

  saw_start = 1;
  saw_end = (end > start);
  
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y + h + 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x + w + 2;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y - 2; 
  } else
    closed = saw_start;
  
  if (!saw_end && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x - 2;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y + h + 2;
  } 
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h + 2;
  } else
    closed = saw_start;

  if (!closed) {
    a[n].x = x + w + 2;
    a[n++].y = y + h + 2;
  }

  r->SetPolygon(n, a);

  Intersect(r);

  no_prgn = save_no_prgn;
}

void wxRegion::Union(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) return;

  if (!no_prgn) {
    if (!r->prgn) abort();
    if (!prgn)
      prgn = r->prgn;
    else {
      wxPathRgn *pr;
      pr = new WXGC_PTRS wxUnionPathRgn(prgn, r->prgn);
      prgn = pr;
    }
  }

#ifdef wx_msw
  if (!lazy_rgn) {
    lazy_rgn = r->lazy_rgn;
  } else if (!r->lazy_rgn) {
    /* no change */
  } else {
    lazy_rgn = new UnionLazyRgn(lazy_rgn, r->lazy_rgn, RGN_OR);
  }
#endif
#ifdef wx_x
  if (!rgn) {
    rgn = XCreateRegion();
  }
  XUnionRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) {
    rgn = NewRgn();
  }
  UnionRgn(rgn, r->rgn, rgn);
#endif
}

void wxRegion::Intersect(wxRegion *r)
{
  if (r->dc != dc) return;
  if (ReallyEmpty())
    return;
  if (r->ReallyEmpty()) {
    Cleanup();
    return;
  }

  if (!no_prgn) {
    wxPathRgn *rprgn, *pr;
    rprgn = r->prgn;
    if (!rprgn) abort();
    if (prgn->is_rect 
	&& rprgn->is_rect
	&& (prgn->ox == rprgn->ox)
	&& (prgn->oy == rprgn->oy)
	&& (prgn->sx == rprgn->sx)
	&& (prgn->sy == rprgn->sy)) {
      /* Special case: both are rectangles with the same
	 origin and scale. This is a common case, and it 
	 can be a lot faster making a rectangle directly. */
      wxRectanglePathRgn *r1 = (wxRectanglePathRgn *)prgn;
      wxRectanglePathRgn *r2 = (wxRectanglePathRgn *)rprgn;
      double px, py, pw, ph;

      if (r1->x < r2->x)
	px = r2->x;
      else
	px = r1->x;
      if (r1->y < r2->y)
	py = r2->y;
      else
	py = r1->y;
      if (r1->x + r1->width < r2->x + r2->width)
	pw = (r1->x + r1->width) - px;
      else
	pw = (r2->x + r2->width) - px;
      if (r1->y + r1->height < r2->y + r2->height)
	ph = (r1->y + r1->height) - py;
      else
	ph = (r2->y + r2->height) - py;
      
      if ((pw > 0) && (ph > 0))
	pr = new WXGC_PTRS wxRectanglePathRgn(dc, px, py, pw, ph);
      else {
	/* empty */
	Cleanup();
	return;
      }
    } else {
      pr = new WXGC_PTRS wxIntersectPathRgn(prgn, r->prgn);
    }
    prgn = pr;
  }

#ifdef wx_msw
  if (!lazy_rgn) return;
  if (!r->lazy_rgn) {
    lazy_rgn = NULL;
    return;
  }
  
  lazy_rgn = new UnionLazyRgn(lazy_rgn, r->lazy_rgn, RGN_AND);
#endif
#ifdef wx_x
  if (!rgn) return;
  XIntersectRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  SectRgn(rgn, r->rgn, rgn);
#endif

  if (ReallyEmpty()) {
    Cleanup();
  }
}

void wxRegion::Subtract(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) return;

  if (!no_prgn) {
    /* wxDiffPathRgn is only half a subtract; the result must be intersected with the first part */
    wxPathRgn *pr;
    if (!r->prgn) abort();
    pr = new WXGC_PTRS wxDiffPathRgn(prgn, r->prgn);
    pr = new WXGC_PTRS wxIntersectPathRgn(prgn, pr);
    prgn = pr;
  }

#ifdef wx_msw
  if (!lazy_rgn) return;
  if (!r->lazy_rgn) {
    /* No change */
    return;
  }
  lazy_rgn = new UnionLazyRgn(lazy_rgn, r->lazy_rgn, RGN_DIFF);
#endif
#ifdef wx_x
  if (!rgn) return;
  XSubtractRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  DiffRgn(rgn, r->rgn, rgn);
#endif

  if (ReallyEmpty()) {
    Cleanup();
    return;
  }
}
  
void wxRegion::Xor(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) return;

  if (!no_prgn) {
    wxPathRgn *pr;
    if (!r->prgn) abort();
    if (!prgn)
      pr = r->prgn;
    else
      pr = new WXGC_PTRS wxDiffPathRgn(prgn, r->prgn);
    prgn = pr;
  }

#ifdef wx_msw
  if (!lazy_rgn) return;
  if (!r->lazy_rgn) {
    return;
  }
  
  lazy_rgn = new UnionLazyRgn(lazy_rgn, r->lazy_rgn, RGN_XOR);
#endif
#ifdef wx_x
  if (!rgn) return;
  XXorRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  XorRgn(rgn, r->rgn, rgn);
#endif

  if (ReallyEmpty()) {
    Cleanup();
    return;
  }
}
  
void wxRegion::BoundingBox(double *x, double *y, double *w, double *h)
{
  if (Empty()) {
    *x = *y = *w = *h = 0;
    return;
  } else {
    double v;
#ifdef wx_msw
    RECT r;
    HRGN rgn;

    if (real_rgn)
      rgn = real_rgn;
    else
      rgn = lazy_rgn->GetRgn();

    if (rgn)
      GetRgnBox(rgn, &r);
    else {
      r.left = r.top = r.right = r.bottom = 0;
    }

    if (!real_rgn)
      lazy_rgn->DoneRgn(rgn);
  
    *x = r.left;
    *y = r.top;
    *w = r.right - r.left;
    *h = r.bottom - r.top;
#endif
#ifdef wx_x
    XRectangle r;
    
    XClipBox(rgn, &r);
    
    *x = r.x;
    *y = r.y;
    *w = r.width;
    *h = r.height;
#endif
#ifdef wx_mac
    {
      Rect r;
      GetRegionBounds(rgn,&r);
      *x = r.left;
      *y = r.top;
      *w = r.right - *x;
      *h = r.bottom - *y;
    }
#endif

    if (is_ps) {
      /* Bitmap-based region is stored upside-down */
      *y = -(*y);
    }
    
    v = dc->UnscrolledDeviceToLogicalX((int)*x);
    *x = v;
    v = dc->UnscrolledDeviceToLogicalY((int)*y);
    *y = v;
    v = dc->DeviceToLogicalXRel((int)*w);
    *w = v;
    v = dc->DeviceToLogicalYRel((int)*h);
    *h = v;
  }
}

Bool wxRegion::Empty()
{
#ifdef wx_msw
  RECT r;
  HRGN rgn;
  Bool is_empty;

  if (!lazy_rgn) return TRUE;

  if (real_rgn)
    rgn = real_rgn;
  else
    rgn = lazy_rgn->GetRgn();

  if (!rgn)
    is_empty = 1;
  else
    is_empty = (GetRgnBox(rgn, &r) == NULLREGION);

  if (!real_rgn)
    lazy_rgn->DoneRgn(rgn);

  return is_empty;
#endif
#ifdef wx_x
  if (!rgn) return TRUE;
  return XEmptyRegion(rgn);
#endif
#ifdef wx_mac
  if (!rgn) return TRUE;
  return EmptyRgn(rgn);
#endif
}

Bool wxRegion::ReallyEmpty()
{
  return Empty() && !prgn;
}

Bool wxRegion::IsInRegion(double x, double y)
{
  int ix, iy;

  if (Empty()) return FALSE;

  x = dc->FLogicalToUnscrolledDeviceX(x);
  y = dc->FLogicalToUnscrolledDeviceY(y);
  

  ix = (int)floor(x);
  iy = (int)floor(y);

#ifdef wx_xt
  return XPointInRegion(rgn, ix, iy);
#endif
#ifdef wx_msw
  {
    HRGN rgn;
    Bool in_rgn;

    if (real_rgn)
      rgn = real_rgn;
    else
      rgn = lazy_rgn->GetRgn();
    
    if (rgn)
      in_rgn = PtInRegion(rgn, ix, iy);
    else
      in_rgn = 0;

    if (!real_rgn)
      lazy_rgn->DoneRgn(rgn);

    return in_rgn;
  }
#endif
#ifdef wx_mac
  {
    Point p;
    p.h = ix;
    p.v = iy;
    return PtInRgn(p, rgn);
  }
#endif
}

void wxRegion::Install(long target, Bool align)
{
  if (prgn) {
    Bool oe;

#ifdef WX_USE_CAIRO
    cairo_new_path(CAIRO_DEV);
#endif
#ifdef wx_mac
    CGContextRef cg = (CGContextRef)target;
    PathTarget *t;
    CGMutablePathRef path;
    
    path = CGPathCreateMutable();
  
    t = (PathTarget *)malloc(sizeof(PathTarget));
    t->path = path;
    t->cg = cg;
    
    target = (long)t;
#endif
#ifdef wx_msw
    Graphics *g = (Graphics *)target;
    GraphicsPath *gp;
    PathTarget *t;

    gp = wxGPathNew(FillModeAlternate);

    t = (PathTarget *)malloc(sizeof(PathTarget));
    t->path = gp;
    t->g = g;
    t->did_one = 0;

    target = (long)t;
#endif

    oe = prgn->Install(target, 0, align);

#ifdef WX_USE_CAIRO
    if (oe)
      cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_EVEN_ODD);
    cairo_clip(CAIRO_DEV);
    if (oe)
      cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_WINDING);
    cairo_new_path(CAIRO_DEV);
#endif
#ifdef wx_mac
    CGContextBeginPath(cg);
    CGContextAddPath(cg, t->path);
    if (oe)
      CGContextEOClip(cg);
    else
      CGContextClip(cg);
    CGPathRelease(t->path);
    free(t);
#endif
#ifdef wx_msw
    wxGSetClip(g, t->path, t->did_one ? CombineModeIntersect : CombineModeReplace);
    wxGPathRelease(t->path);
    free(t);
#endif
  } else {
    /* Empty region: */
#ifdef WX_USE_CAIRO
    cairo_new_path(CAIRO_DEV);
    /* Empty path confuses some versions of Cairo, so
       clip to two non-overlapping regions */
    cairo_move_to(CAIRO_DEV, 0, 0);
    cairo_line_to(CAIRO_DEV, 1, 0);
    cairo_line_to(CAIRO_DEV, 1, 1);
    cairo_clip(CAIRO_DEV);
    cairo_new_path(CAIRO_DEV);
    cairo_move_to(CAIRO_DEV, 2, 2);
    cairo_line_to(CAIRO_DEV, 3, 2);
    cairo_line_to(CAIRO_DEV, 3, 3);
    cairo_clip(CAIRO_DEV);
#endif
#ifdef wx_mac
    {
      CGContextRef cg = (CGContextRef)target;
      CGRect r;
      r.origin.x = 0;
      r.origin.y = 0;
      r.size.width = 0;
      r.size.height = 0;
      CGContextClipToRect(cg, r);
    }
#endif
#ifdef wx_msw
    {
      GraphicsPath *gp;
      Graphics *g = (Graphics *)target;
      gp = wxGPathNew(FillModeAlternate);
      wxGSetClip(g, gp, CombineModeReplace);
      wxGPathRelease(gp);
    }
#endif
  }
}

void wxRegion::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  Bool oe;

  if (!prgn) return; /* shouldn't happen */

  s->Out("newpath\n");

  oe = prgn->InstallPS(dc, s);

  if (oe)
    s->Out("eoclip\n");
  else
    s->Out("clip\n");
}

/***************************************************************************************/

#ifdef wx_mac
static CGAffineTransform current_xform;
#endif
#ifdef wx_msw
static GraphicsPath *current_path;
#endif

wxPathRgn::wxPathRgn(wxDC *dc)
: wxObject(FALSE)
{
  if (dc) {
    double x, y, xs, ys;
    dc->GetDeviceOrigin(&x, &y);
    dc->GetUserScale(&xs, &ys);
    ox = x;
    oy = y;
    sx = xs;
    sy = ys;
  } else {
    ox = oy = 0.0;
    sx = sy = 1.0;
  }
  is_rect = 0;
}

wxPathRgn::~wxPathRgn()
{ 
  /* If anything important is added here, change constructor chaining
     to wxObject from FALSE to TRUE. Beware that wxPaths can share
     wxPathRgns. */
}

double wxPathRgn::XFormX(double _x, Bool align)
{
  if (align)
    return floor((_x * sx) + ox) + 0.5;
  else
    return _x;
}

double wxPathRgn::XFormY(double _y, Bool align)
{
  if (align)
    return floor((_y * sy) + oy) + 0.5;
  else
    return _y;
}

double wxPathRgn::XFormXB(double _x, Bool align)
{
  if (align)
    return floor((_x * sx) + ox);
  else
    return _x;
}

double wxPathRgn::XFormYB(double _y, Bool align)
{
  if (align)
    return floor((_y * sy) + oy);
  else
    return _y;
}

double wxPathRgn::XFormW(double _w, double _x, Bool align)
{
  if (align)
    return floor(((_x + _w) * sx) + ox) - floor((_x * sx) + ox);
  else
    return _w;
}

double wxPathRgn::XFormH(double _h, double _y, Bool align)
{
  if (align)
    return floor(((_y + _h) * sy) + oy) - floor((_y * sy) + oy);
  else
    return _h;
}

void wxPathRgn::PrepareScale(long target, Bool oe, Bool align, void *_m)
{
#ifdef wx_xt
# ifdef WX_USE_CAIRO
  if (!align) {
    cairo_matrix_p *m = (cairo_matrix_p *)_m;
    cairo_set_matrix_create(*m);
    cairo_current_matrix(CAIRO_DEV, *m);
    cairo_default_matrix(CAIRO_DEV);
    cairo_translate(CAIRO_DEV, ox, oy);
    cairo_scale(CAIRO_DEV, sx, sy);
  }
# endif
#endif
#ifdef wx_mac
  if (align) {
    current_xform = CGAffineTransformMakeTranslation(0, 0);
  } else {
    current_xform = CGAffineTransformMakeTranslation(ox, oy);
    current_xform = CGAffineTransformScale(current_xform, sx, sy);
  }
#endif
#ifdef wx_msw
  current_path = wxGPathNew(oe ?  FillModeAlternate : FillModeWinding);
#endif
}

void wxPathRgn::RestoreScale(long target, Bool align, void *_m)
{
#ifdef WX_USE_CAIRO
  if (!align) {
    cairo_matrix_p *m = (cairo_matrix_p *)_m;
    cairo__set_matrix(CAIRO_DEV, *m);
    cairo_matrix_destroy(*m);
  }
#endif
#ifdef wx_mac
#endif
#ifdef wx_msw
  if (!align) {
    Matrix *m;
    m = wxGMatrixNew();
    wxGMatrixTranslate(m, ox, oy);
    wxGMatrixScale(m, sx, sy);
    wxGPathTransform(current_path, m);
    wxGMatrixRelease(m);
  }
  wxGPathAddPath(GP, current_path, TRUE);
  wxGPathRelease(current_path);
#endif
}


wxRectanglePathRgn::wxRectanglePathRgn(wxDC *dc_for_scale, double _x, double _y, double _width, double _height)
: wxPathRgn(dc_for_scale)
{
  x = _x;
  y = _y;
  width = _width;
  height = _height;
  is_rect = 1;
}

Bool wxRectanglePathRgn::Install(long target, Bool reverse, Bool align)
{
  double xx, yy, ww, hh;
  cairo_matrix_p m;

  PrepareScale(target, TRUE, align, &m);

  xx = XFormXB(x, align);
  yy = XFormYB(y, align);
  ww = XFormW(width, x, align);
  hh = XFormH(height, y, align);

#ifdef WX_USE_CAIRO
  cairo_move_to(CAIRO_DEV, xx, yy);
  if (reverse) {
    cairo_rel_line_to(CAIRO_DEV, 0, hh);
    cairo_rel_line_to(CAIRO_DEV, ww, 0);
    cairo_rel_line_to(CAIRO_DEV, 0, -hh);
  } else {
    cairo_rel_line_to(CAIRO_DEV, ww, 0);
    cairo_rel_line_to(CAIRO_DEV, 0, hh);
    cairo_rel_line_to(CAIRO_DEV, -ww, 0);
  }
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  CGPathMoveToPoint(CGPATH, CGXFORM, xx, yy);
  if (reverse) {
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy + hh);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy + hh);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy);
  } else {
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy + hh);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy + hh);
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  if (reverse) {
    wxGPathAddLine(CURRENT_GP, xx, yy, xx, yy + hh);
    wxGPathAddLine(CURRENT_GP, xx, yy + hh, xx + ww, yy + hh);
    wxGPathAddLine(CURRENT_GP, xx + ww, yy + hh, xx + ww, yy);
  } else {
    wxGPathAddLine(CURRENT_GP, xx, yy, xx + ww, yy);
    wxGPathAddLine(CURRENT_GP, xx + ww, yy, xx + ww, yy + hh);
    wxGPathAddLine(CURRENT_GP, xx + ww, yy + hh, xx, yy + hh);
  }
  wxGPathCloseFigure(CURRENT_GP);
#endif

  RestoreScale(target, align, &m);
  
  return FALSE;
}

Bool wxRectanglePathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  double xx, yy, ww, hh;

  xx = dc->FsLogicalToDeviceX(x, ox, sx);
  yy = dc->FsLogicalToDeviceY(y, oy, sy);
  ww = dc->FsLogicalToDeviceXRel(width, ox, sx);
  hh = dc->FsLogicalToDeviceYRel(height, oy, sy);

  s->Out(xx); s->Out(" "); s->Out(yy); s->Out(" moveto\n");
  s->Out(xx + ww); s->Out(" "); s->Out(yy); s->Out(" lineto\n");
  s->Out(xx + ww); s->Out(" "); s->Out(yy - hh); s->Out(" lineto\n");
  s->Out(xx); s->Out(" "); s->Out(yy - hh); s->Out(" lineto\n");
  s->Out("closepath\n");

  return FALSE;
}

wxRoundedRectanglePathRgn::wxRoundedRectanglePathRgn(wxDC *dc_for_scale, 
						     double _x, double _y, double _width, double _height, double _radius)
: wxPathRgn(dc_for_scale)
{
  x = _x;
  y = _y;
  width = _width;
  height = _height;
  radius = _radius;

  if (radius < 0) {
    radius = -radius;
    if (width > height)
      radius = radius * height;
    else
      radius = radius * width;
  }
}

Bool wxRoundedRectanglePathRgn::Install(long target, Bool reverse, Bool align)
{
  double xx, yy, ww, hh, rr, rr2;
  cairo_matrix_p m;
  
  PrepareScale(target, TRUE, align, &m);

  xx = XFormXB(x, align);
  yy = XFormYB(y, align);
  ww = XFormW(width, x, align);
  hh = XFormH(height, y, align);
  
  rr = XFormW(radius, 0, align);
  rr2 = XFormH(radius, 0, align);
  if (rr2 < rr)
    rr = rr2;

#ifdef WX_USE_CAIRO
  {
    if (reverse) {
      cairo_move_to(CAIRO_DEV, xx, yy + rr);
      cairo_line_to(CAIRO_DEV, xx, yy + hh - rr);
      cairo_arc_negative(CAIRO_DEV, xx + rr, yy + hh - rr, rr, wxPI, 0.5 * wxPI);
      cairo_line_to(CAIRO_DEV, xx + ww - rr, yy + hh);
      cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + hh - rr, rr, 0.5 * wxPI, 0);
      cairo_line_to(CAIRO_DEV, xx + ww, yy + rr);
      cairo_arc_negative(CAIRO_DEV, xx + ww - rr, yy + rr, rr, 2 * wxPI, 1.5 * wxPI);
      cairo_line_to(CAIRO_DEV, xx + rr, yy);
      cairo_arc_negative(CAIRO_DEV, xx + rr, yy + rr, rr, 1.5 * wxPI, wxPI);
      cairo_line_to(CAIRO_DEV, xx, yy + rr);
    } else {
      cairo_move_to(CAIRO_DEV, xx, yy + rr);
      cairo_arc(CAIRO_DEV, xx + rr, yy + rr, rr, wxPI, 1.5 * wxPI);
      cairo_line_to(CAIRO_DEV, xx + ww - rr, yy);
      cairo_arc(CAIRO_DEV, xx + ww - rr, yy + rr, rr, 1.5 * wxPI, 2 * wxPI);
      cairo_line_to(CAIRO_DEV, xx + ww, yy + hh - rr);
      cairo_arc(CAIRO_DEV, xx + ww - rr, yy + hh - rr, rr, 0, 0.5 * wxPI);
      cairo_line_to(CAIRO_DEV, xx + rr, yy + hh);
      cairo_arc(CAIRO_DEV, xx + rr, yy + hh - rr, rr, 0.5 * wxPI, wxPI);
      cairo_line_to(CAIRO_DEV, xx, yy + rr);
    }
    cairo_close_path(CAIRO_DEV);
  }
#endif
#ifdef wx_mac
  if (reverse) {
    CGPathMoveToPoint(CGPATH, CGXFORM, xx + rr, yy);
    CGPathAddArc(CGPATH, CGXFORM, xx + rr, yy + rr, rr, 1.5 * wxPI, 1.0 * wxPI, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy + hh - rr);
    CGPathAddArc(CGPATH, CGXFORM, xx + rr, yy + hh - rr, rr, 1.0 * wxPI, 0.5 * wxPI, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww - rr, yy + hh);
    CGPathAddArc(CGPATH, CGXFORM, xx + ww - rr, yy + hh - rr, rr, 0.5 * wxPI, 0, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy + rr);
    CGPathAddArc(CGPATH, CGXFORM, xx + ww - rr, yy + rr, rr, 2 * wxPI, 1.5 * wxPI, TRUE);
  } else {
    CGPathMoveToPoint(CGPATH, CGXFORM, xx + rr, yy);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww - rr, yy);
    CGPathAddArc(CGPATH, CGXFORM, xx + ww - rr, yy + rr, rr, 1.5 * wxPI, 2 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + ww, yy + hh - rr);
    CGPathAddArc(CGPATH, CGXFORM, xx + ww - rr, yy + hh - rr, rr, 0, 0.5 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx + rr, yy + hh);
    CGPathAddArc(CGPATH, CGXFORM, xx + rr, yy + hh - rr, rr, 0.5 * wxPI, 1.0 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy + rr);
    CGPathAddArc(CGPATH, CGXFORM, xx + rr, yy + rr, rr, 1.0 * wxPI, 1.5 * wxPI, FALSE);
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  if (reverse) {
    wxGPathAddArc(CURRENT_GP, xx, yy, rr * 2, rr * 2, 270, -90);
    wxGPathAddLine(CURRENT_GP, xx, yy + rr, xx, yy + hh - rr);
    wxGPathAddArc(CURRENT_GP, xx, yy + hh - 2 * rr, 2 * rr, 2 * rr, 180, -90);
    wxGPathAddLine(CURRENT_GP, xx + rr, yy + hh, xx + ww - rr, yy + hh);
    wxGPathAddArc(CURRENT_GP, xx + ww - 2 * rr, yy + hh - 2 * rr, 2 * rr, 2 * rr, 90, -90);
    wxGPathAddLine(CURRENT_GP, xx + ww, yy + hh - rr, xx + ww, yy + rr);
    wxGPathAddArc(CURRENT_GP, xx + ww - 2 * rr, yy, rr * 2, rr * 2, 360, -90);
  } else {
    wxGPathAddArc(CURRENT_GP, xx, yy, rr * 2, rr * 2, 180, 90);
    wxGPathAddLine(CURRENT_GP, xx + rr, yy, xx + ww - rr, yy);
    wxGPathAddArc(CURRENT_GP, xx + ww - 2 * rr, yy, rr * 2, rr * 2, 270, 90);
    wxGPathAddLine(CURRENT_GP, xx + ww, yy + rr, xx + ww, yy + hh - rr);
    wxGPathAddArc(CURRENT_GP, xx + ww - 2 * rr, yy + hh - 2 * rr, 2 * rr, 2 * rr, 0, 90);
    wxGPathAddLine(CURRENT_GP, xx + ww - rr, yy + hh, xx + rr, yy + hh);
    wxGPathAddArc(CURRENT_GP, xx, yy + hh - 2 * rr, 2 * rr, 2 * rr, 90, 90);
  }
  wxGPathCloseFigure(CURRENT_GP);
#endif

  RestoreScale(target, align, &m);

  return FALSE;
}

Bool wxRoundedRectanglePathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  double xx, yy, ww, hh, rr;

  xx = dc->FsLogicalToDeviceX(x, ox, sx);
  yy = dc->FsLogicalToDeviceY(y, oy, sy);
  ww = dc->FsLogicalToDeviceXRel(width, ox, sx);
  hh = dc->FsLogicalToDeviceYRel(height, oy, sy);
  if (sx > sy)
    rr = dc->FsLogicalToDeviceYRel(radius, oy, sy);
  else
    rr = dc->FsLogicalToDeviceXRel(radius, ox, sx);

  hh = -hh;

  s->Out(xx + rr); s->Out(" "); 
  s->Out(yy); s->Out(" moveto\n");
  
  s->Out(xx + rr); s->Out(" ");
  s->Out(yy - rr); s->Out(" "); 
  s->Out(rr); s->Out(" 90 180 arc\n");
  
  s->Out(xx + rr); s->Out(" ");
  s->Out(yy + hh + rr); s->Out(" "); 
  s->Out(rr); s->Out(" 180 270 arc\n");
  
  s->Out(xx + ww - rr); s->Out(" ");
  s->Out(yy + hh + rr); s->Out(" "); 
  s->Out(rr); s->Out(" 270 0 arc\n");
  
  s->Out(xx + ww - rr); s->Out(" ");
  s->Out(yy - rr); s->Out(" "); 
  s->Out(rr); s->Out(" 0 90 arc\n");

  s->Out("closepath\n");

  return FALSE;
}

wxPolygonPathRgn::wxPolygonPathRgn(wxDC *dc_for_scale,
				   int _n, wxPoint _points[], double _xoffset, double _yoffset, int _fillStyle)
: wxPathRgn(dc_for_scale)
{
  n = _n;
  points = _points;
  xoffset = _xoffset;
  yoffset = _yoffset;
  fillStyle = _fillStyle;
}

Bool wxPolygonPathRgn::Install(long target, Bool reverse, Bool align)
{
#if defined(WX_USE_CAIRO) || defined(wx_msw) || defined(wx_mac)
  double xx, yy;
#endif
  cairo_matrix_p m;

  PrepareScale(target, fillStyle == wxODDEVEN_RULE, align, &m);

#ifdef WX_USE_CAIRO
  if (reverse) {
    int i;
    xx = XFormX(points[n-1].x + xoffset, align);
    yy = XFormY(points[n-1].y + yoffset, align);
    cairo_move_to(CAIRO_DEV, xx, yy);
    for (i = n-1; i--; ) {
      xx = XFormX(points[i].x + xoffset, align);
      yy = XFormY(points[i].y + yoffset, align);
      cairo_line_to(CAIRO_DEV, xx, yy);
    }
  } else {
    int i;
    xx = XFormX(points[0].x + xoffset, align);
    yy = XFormY(points[0].y + yoffset, align);
    cairo_move_to(CAIRO_DEV, xx, yy);
    for (i = 1; i < n; i++) {
      xx = XFormX(points[i].x + xoffset, align);
      yy = XFormY(points[i].y + yoffset, align);
      cairo_line_to(CAIRO_DEV, xx, yy);
    }
  }
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  if (reverse) {
    int i;
    xx = XFormX(points[n-1].x + xoffset, align);
    yy = XFormY(points[n-1].y + yoffset, align);
    CGPathMoveToPoint(CGPATH, CGXFORM, xx, yy);
    for (i = n-1; i--; ) {
      xx = XFormX(points[i].x + xoffset, align);
      yy = XFormY(points[i].y + yoffset, align);
      CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy);
    }
  } else {
    int i;
    xx = XFormX(points[0].x + xoffset, align);
    yy = XFormY(points[0].y + yoffset, align);
    CGPathMoveToPoint(CGPATH, CGXFORM, xx, yy);
    for (i = 1; i < n; i++) {
      xx = XFormX(points[i].x + xoffset, align);
      yy = XFormY(points[i].y + yoffset, align);
      CGPathAddLineToPoint(CGPATH, CGXFORM, xx, yy);
    }
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  if (reverse) {
    int i;
    double xx2, yy2;
    for (i = n - 1; i--; ) {
      xx = XFormX(points[i+1].x + xoffset, align);
      yy = XFormY(points[i+1].y + yoffset, align);
      xx2 = XFormX(points[i].x + xoffset, align);
      yy2 = XFormY(points[i].y + yoffset, align);
      wxGPathAddLine(CURRENT_GP, xx, yy, xx2, yy2);
    }
  } else {
    int i;
	double xx2, yy2;
    for (i = 0; i < n - 1; i++) {
      xx = XFormX(points[i].x + xoffset, align);
      yy = XFormY(points[i].y + yoffset, align);
      xx2 = XFormX(points[i+1].x + xoffset, align);
      yy2 = XFormY(points[i+1].y + yoffset, align);
      wxGPathAddLine(CURRENT_GP, xx, yy, xx2, yy2);
    }
  }
  wxGPathCloseFigure(CURRENT_GP);
#endif

  RestoreScale(target, align, &m);

  return (fillStyle == wxODDEVEN_RULE);
}

Bool wxPolygonPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  double xx, yy;
  int i;

  xx = dc->FsLogicalToDeviceX(points[0].x + xoffset, ox, sx);
  yy = dc->FsLogicalToDeviceY(points[0].y + yoffset, oy, sy);
  s->Out(xx); s->Out(" "); 
  s->Out(yy); s->Out(" moveto\n");

  for (i = 1; i < n; i++) {
    xx = dc->FsLogicalToDeviceX(points[i].x + xoffset, ox, sx);
    yy = dc->FsLogicalToDeviceY(points[i].y + yoffset, oy, sy);
    s->Out(xx); s->Out(" "); 
    s->Out(yy); s->Out(" lineto\n");
  }
  s->Out("closepath\n");

  return (fillStyle == wxODDEVEN_RULE);
}


wxPathPathRgn::wxPathPathRgn(wxDC *dc_for_scale,
			     wxPath *_p, double _xoffset, double _yoffset, int _fillStyle)
: wxPathRgn(dc_for_scale)
{
  p = new WXGC_PTRS wxPath();
  p->AddPath(_p);
  p->Translate(_xoffset, _yoffset);
  fillStyle = _fillStyle;
}

Bool wxPathPathRgn::Install(long target, Bool reverse, Bool align)
{
  wxPath *q;
  cairo_matrix_p m;

  PrepareScale(target, fillStyle == wxODDEVEN_RULE, align, &m);

  if (reverse) {
    q = new WXGC_PTRS wxPath();
    q->AddPath(p);
    q->Reverse();
  } else
    q = p;

  if (align) {
#ifdef WX_USE_CAIRO
    q->Install(target, 0, 0, ox, oy, sx, sy, TRUE, 0.5, 0.5);
#endif
#ifdef wx_mac
    q->Install((long)CGPATH, 0, 0, ox, oy, sx, sy, TRUE, 0.5, 0.5);
#endif
#ifdef wx_msw
    q->Install((long)CURRENT_GP, 0, 0, ox, oy, sx, sy, TRUE, 0, 0);
#endif
  } else {
#ifdef WX_USE_CAIRO
    q->Install(target, 0, 0, 0, 0, 1, 1, FALSE, 0, 0);
#endif
#ifdef wx_mac
    q->Install((long)CGPATH, 0, 0, 0, 0, 1, 1, FALSE, 0, 0);
#endif
#ifdef wx_msw
    q->Install((long)CURRENT_GP, 0, 0, 0, 0, 1, 1, FALSE, 0, 0);
#endif
  }

  RestoreScale(target, align, &m);

  return (fillStyle == wxODDEVEN_RULE);
}

Bool wxPathPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  p->InstallPS(dc, s, 0, 0);
  return (fillStyle == wxODDEVEN_RULE);
}

wxArcPathRgn::wxArcPathRgn(wxDC *dc_for_scale,
			   double _x, double _y, double _w, double _h, double _start, double _end)
: wxPathRgn(dc_for_scale)
{
  x = _x;
  y = _y;
  w = _w;
  h = _h;
  start = _start;
  end = _end;
}

Bool wxArcPathRgn::Install(long target, Bool reverse, Bool align)
{
  double xx, yy, ww, hh;
  cairo_matrix_p m;

  PrepareScale(target, TRUE, align, &m);

  xx = XFormXB(x, align);
  yy = XFormYB(y, align);
  ww = XFormW(w, x, align);
  hh = XFormH(h, y, align);  

#ifdef WX_USE_CAIRO
  {
    cairo_translate(CAIRO_DEV, xx, yy);
    cairo_scale(CAIRO_DEV, ww, hh);
    if ((start != 0.0) || (end != (2 * wxPI)))
      cairo_move_to(CAIRO_DEV, 0.5, 0.5);
    if (!reverse)
      cairo_arc(CAIRO_DEV, 0.5, 0.5, 0.5, -end, -start);
    else
      cairo_arc_negative(CAIRO_DEV, 0.5, 0.5, 0.5, -start, -end);
    cairo_close_path(CAIRO_DEV);
  }
#endif
#ifdef wx_mac
  {
    CGAffineTransform xform;
    xform = CGAffineTransformTranslate(*CGXFORM, xx, yy);
    xform = CGAffineTransformScale(xform, ww, hh);
    if ((start != 0.0) || (end != (2 * wxPI)))
      CGPathMoveToPoint(CGPATH, &xform, 0.5, 0.5);
    if (!reverse)
      CGPathAddArc(CGPATH, &xform, 0.5, 0.5, 0.5, (2 * wxPI) - end, (2 * wxPI) - start, FALSE);
    else
      CGPathAddArc(CGPATH, &xform, 0.5, 0.5, 0.5, (2 * wxPI) - start, (2 * wxPI) - end, TRUE);
    CGPathCloseSubpath(CGPATH);
  }
#endif
#ifdef wx_msw
  {
    double init, span;
    if ((start == 0.0) && (end == 2 * wxPI)) {
      if (reverse) {
	wxGPathAddArc(CURRENT_GP, xx, yy, ww, hh, 360.0, -360.0);
      } else {
	wxGPathAddArc(CURRENT_GP, xx, yy, ww, hh, 0.0, 360.0);
      }
    } else {
      init = (2 * wxPI - start) * 180 / wxPI;
      init = fmod(init, 360.0);
      if (init < 0.0)
	init += 360.0;
      
      span = (start - end) * 180 / wxPI;
      span = fmod(span, 360.0);
      if (span > 0)
	span -= 360.0;
      if (reverse) {
	wxGPathAddPie(CURRENT_GP, xx, yy, ww, hh, init + span, -span);
      } else {
	wxGPathAddPie(CURRENT_GP, xx, yy, ww, hh, init, span);
      }
    }
    wxGPathCloseFigure(CURRENT_GP);
  }
#endif

  RestoreScale(target, align, &m);

  return FALSE;
}

Bool wxArcPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  double xx, yy, ww, hh;

  xx = dc->FsLogicalToDeviceX(x, ox, sx);
  yy = dc->FsLogicalToDeviceY(y, oy, sy);
  ww = dc->FsLogicalToDeviceXRel(w, ox, sx);
  hh = dc->FsLogicalToDeviceYRel(h, oy, sy);

  s->Out("matrix currentmatrix ");
  s->Out(xx + ww/2); s->Out(" "); s->Out(yy - hh/2); s->Out(" translate ");
  s->Out(ww); s->Out(" "); s->Out(hh); s->Out(" scale\n");
  if ((start != 0) || (end != 2 * wxPI)) {
    s->Out("0 0 moveto\n");
  }
  s->Out("0 0 0.5 "); 
  s->Out(start * 180 / wxPI); s->Out(" "); 
  s->Out(end * 180 / wxPI); s->Out(" arc setmatrix closepath\n");
  
  return FALSE;
}


wxUnionPathRgn::wxUnionPathRgn(wxPathRgn *_f, wxPathRgn *_s)
: wxPathRgn(NULL)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

Bool wxUnionPathRgn::Install(long target, Bool reverse, Bool align)
{
  Bool aoe, boe;

  aoe = a->Install(target, reverse, align);
  boe = b->Install(target, reverse, align);

  return aoe || boe;
}

Bool wxUnionPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  Bool aoe, boe;

  aoe = a->InstallPS(dc, s);
  boe = b->InstallPS(dc, s);

  return aoe || boe;
}

wxIntersectPathRgn::wxIntersectPathRgn(wxPathRgn *_f, wxPathRgn *_s)
: wxPathRgn(NULL)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

Bool wxIntersectPathRgn::Install(long target, Bool reverse, Bool align)
{
  Bool aoe;

  aoe = a->Install(target, reverse, align);

#ifdef WX_USE_CAIRO
  if (aoe)
    cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_EVEN_ODD);
  cairo_clip(CAIRO_DEV);
  if (aoe)
    cairo_set_fill_rule(CAIRO_DEV, CAIRO_FILL_RULE_WINDING);
  cairo_new_path(CAIRO_DEV);
#endif
#if defined(wx_mac)
  CGContextBeginPath(CGCG);
  CGContextAddPath(CGCG, CGPATH);
  if (aoe)
    CGContextEOClip(CGCG);
  else
    CGContextClip(CGCG);
  CGPathRelease(CGPATH);
  {
    CGMutablePathRef p;
    p = CGPathCreateMutable();
    CGPATH = p;
  }
#endif
#if defined(wx_msw)
  wxGSetClip(GP_G, GP, GP_DID_ONE ? CombineModeIntersect : CombineModeReplace);
  GP_DID_ONE = 1;
  wxGPathRelease(GP);
  {
    GraphicsPath *p;
    p = wxGPathNew(FillModeAlternate);
    GP = p;
  }
#endif

  return b->Install(target, reverse, align);
}

Bool wxIntersectPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  Bool aoe;

  aoe = a->InstallPS(dc, s);
  if (aoe)
    s->Out("eoclip\n");
  else
    s->Out("clip\n");
  
  return b->InstallPS(dc, s);
}
  

wxDiffPathRgn::wxDiffPathRgn(wxPathRgn *_f, wxPathRgn *_s)
: wxPathRgn(NULL)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

Bool wxDiffPathRgn::Install(long target, Bool reverse, Bool align)
{
  Bool aoe, boe;

  aoe = a->Install(target, reverse, align);
  boe = b->Install(target, !reverse, align);

  return aoe || boe;
}

Bool wxDiffPathRgn::InstallPS(wxPostScriptDC *dc, wxPSStream *s)
{
  Bool aoe, boe;

  aoe = a->InstallPS(dc, s);
  s->Out("reversepath\n");
  boe = b->InstallPS(dc, s);
  s->Out("reversepath\n");

  return aoe || boe;
}

/********************************************************/
/*                        Paths                         */
/********************************************************/

#define CMD_CLOSE        1.0
#define CMD_MOVE         2.0
#define CMD_LINE         3.0
#define CMD_CURVE        4.0

#define ROTATE_XY(x, y) { xtmp1 = x; ytmp1 = y;             \
                          xtmp2 = (xx * xtmp1) + (xy * ytmp1); \
                          ytmp2 = (yy * ytmp1) + (yx * xtmp1); \
                          x = xtmp2; y = ytmp2; }

wxPath::wxPath()
{
  Reset();
}

wxPath::~wxPath()
{
  Reset();
}

void wxPath::Reset()
{
  ClearCache();
  cmd_size = 0;
  alloc_cmd_size = 0;
  cmds = NULL;
  last_cmd = -1;
}

void wxPath::ClearCache()
{
  poly_pts = NULL;
}

void wxPath::MakeRoom(int n)
{
  ClearCache();
  if (cmd_size + n > alloc_cmd_size) {
    double *a;
    int s;
    s = 2 * (alloc_cmd_size + n);
    a = new WXGC_ATOMIC double[s];
    memcpy(a, cmds, sizeof(double) * cmd_size);
    cmds = a;
    alloc_cmd_size = s;
  }
}

Bool wxPath::IsOpen()
{
  return ((last_cmd > -1) && (cmds[last_cmd] != CMD_CLOSE));
}

void wxPath::Close()
{
  if ((last_cmd > -1) && (cmds[last_cmd] != CMD_CLOSE)) {
    MakeRoom(1);
    last_cmd = cmd_size;
    cmds[cmd_size++] = CMD_CLOSE;
  }
}

void wxPath::MoveTo(double x, double y)
{
  Close();

  MakeRoom(3);
  last_cmd = cmd_size;
  cmds[cmd_size++] = CMD_MOVE;
  cmds[cmd_size++] = x;
  cmds[cmd_size++] = y;
}

void wxPath::LineTo(double x, double y)
{
  MakeRoom(3);
  last_cmd = cmd_size;
  cmds[cmd_size++] = CMD_LINE;
  cmds[cmd_size++] = x;
  cmds[cmd_size++] = y;
}

void wxPath::Arc(double x, double y, double w, double h, double start, double end, Bool ccw)
{
  double delta, angle, rotate;
  double x0, y0, x1, y1, x2, y2, x3, y3;
  double xx, xy, yy, yx, xtmp1, ytmp1, xtmp2, ytmp2;
  int did_one = 0, start_cmd = cmd_size, start_open;

  start_open = IsOpen();

  /* The arc below is backwards from the GRacket API.... */
  {
    double s;
    s = start;
    start = end;
    end = s;
  }

  if (ccw) {
    double s;
    s = start;
    start = end;
    end = s;
  }

  delta = end - start;
  if (delta > 2 * wxPI)
    delta = 2 * wxPI;
  else if (delta < 0) {
    delta = fmod(delta, 2 * wxPI);
    delta += 2 * wxPI;
  }

  /* At this point, delta is between 0 and 2pi */
  
  if (delta == 2 * wxPI)
    start = 0;

  /* Change top-left to center: */
  x += w/2;
  y += h/2;

  /* Make up to 4 curves to represent the arc. */
  do {
    if (delta > (wxPI / 2))
      angle = (wxPI / 2);
    else
      angle = delta;

    /* First generate points for an arc
       of `angle' length from -angle/2 to
       +angle/2. */

    x0  = cos(angle / 2);
    y0  = sin(angle / 2);
    x1 = (4 - x0) / 3;
    y1 = ((1 - x0) * (3 - x0)) / (3 * y0);
    x2 = x1;
    y2 = -y1;
    x3 = x0;
    y3 = -y0;
    
    /* Rotate to start: */
    rotate = start + (angle / 2);
    xx = cos(rotate);
    xy = sin(rotate);
    yy = xx;
    yx = -xy;
    ROTATE_XY(x0, y0);
    ROTATE_XY(x1, y1);
    ROTATE_XY(x2, y2);
    ROTATE_XY(x3, y3);

    /* Scale and move to match ellipse: */
    x0 = (x0 * w/2) + x;
    x1 = (x1 * w/2) + x;
    x2 = (x2 * w/2) + x;
    x3 = (x3 * w/2) + x;

    y0 = (y0 * h/2) + y;
    y1 = (y1 * h/2) + y;
    y2 = (y2 * h/2) + y;
    y3 = (y3 * h/2) + y;

    if (!did_one) {
      if (IsOpen()) {
	LineTo(x0, y0);
      } else {
	MoveTo(x0, y0);
      }
    }

    if (angle)
      CurveTo(x1, y1, x2, y2, x3, y3);
    else
      LineTo(x3, y3);
    
    start += angle;
    delta -= angle;
    did_one = 1;
  } while (delta > 0);

  if (!ccw) {
    Reverse(start_cmd, start_open);
  }
}

void wxPath::CurveTo(double x1, double y1, double x2, double y2, double x3, double y3)
{
  MakeRoom(7);
  last_cmd = cmd_size;
  cmds[cmd_size++] = CMD_CURVE;
  cmds[cmd_size++] = x1;
  cmds[cmd_size++] = y1;
  cmds[cmd_size++] = x2;
  cmds[cmd_size++] = y2;
  cmds[cmd_size++] = x3;
  cmds[cmd_size++] = y3;
}

void wxPath::Rectangle(double x, double y, double width, double height)
{
  MoveTo(x, y);
  LineTo(x + width, y);
  LineTo(x + width, y + height);
  LineTo(x, y + height);
  Close();
}

void wxPath::RoundedRectangle(double x, double y, double width, double height, double radius)
{
  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    double smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (double)(- radius * smallest);
  }
    
  Close();
  Arc(x, y, radius * 2, radius * 2, wxPI, 0.5 * wxPI, FALSE);
  LineTo(x + width - radius, y);
  Arc(x + width - 2 * radius, y, radius * 2, radius * 2, 0.5 * wxPI, 0 * wxPI, FALSE);
  LineTo(x + width, y + height - radius);
  Arc(x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius, 0 * wxPI, 1.5 * wxPI, FALSE);
  LineTo(x + radius, y + height);
  Arc(x, y + height - 2 * radius, 2 * radius, 2 * radius, 1.5 * wxPI, 1.0 * wxPI, FALSE);
  Close();
}

void wxPath::Ellipse(double x, double y, double width, double height)
{
  Close();
  Arc(x, y, width, height, 0, 2 * wxPI, TRUE);
  Close();
}

void wxPath::Lines(int n, wxPoint points[], double xoffset, double yoffset)
{
  int i;
  for (i = 0; i < n; i++) {
    LineTo(points[i].x + xoffset, points[i].y + yoffset);
  }
}

void wxPath::Translate(double x, double y)
{
  int i = 0;
  while (i < cmd_size) {
    if (cmds[i] == CMD_CLOSE) {
      i += 1;
    } else if ((cmds[i] == CMD_MOVE)
	       || (cmds[i] == CMD_LINE)) {
      cmds[i+1] += x;
      cmds[i+2] += y;
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      cmds[i+1] += x;
      cmds[i+2] += y;
      cmds[i+3] += x;
      cmds[i+4] += y;
      cmds[i+5] += x;
      cmds[i+6] += y;
      i += 7;
    }
  }
}

void wxPath::Scale(double x, double y)
{
  int i = 0;
  while (i < cmd_size) {
    if (cmds[i] == CMD_CLOSE) {
      i += 1;
    } else if ((cmds[i] == CMD_MOVE)
	       || (cmds[i] == CMD_LINE)) {
      cmds[i+1] *= x;
      cmds[i+2] *= y;
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      cmds[i+1] *= x;
      cmds[i+2] *= y;
      cmds[i+3] *= x;
      cmds[i+4] *= y;
      cmds[i+5] *= x;
      cmds[i+6] *= y;
      i += 7;
    }
  }
}

void wxPath::Rotate(double a)
{
  double xx, xy, yy, yx, xtmp1, ytmp1, xtmp2, ytmp2;
  int i = 0;

  xx = cos(a);
  xy = sin(a);
  yy = xx;
  yx = -xy;

  while (i < cmd_size) {
    if (cmds[i] == CMD_CLOSE) {
      i += 1;
    } else if ((cmds[i] == CMD_MOVE)
	       || (cmds[i] == CMD_LINE)) {
      ROTATE_XY(cmds[i+1], cmds[i+2]);
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      ROTATE_XY(cmds[i+1], cmds[i+2]);
      ROTATE_XY(cmds[i+3], cmds[i+4]);
      ROTATE_XY(cmds[i+5], cmds[i+6]);
      i += 7;
    }
  }
}

void wxPath::Reverse(int start_cmd, Bool start_with_line)
{
  int e, i, j, pos, n, *cs, controls;
  double *a;

  while (start_cmd < cmd_size) {
    /* Find next starting point: */
    if (cmds[start_cmd] == CMD_CLOSE) {
      start_cmd += 1;
    }

    i = start_cmd;
    n = 0;
    while (i < cmd_size) {
      if (cmds[i] == CMD_CLOSE) {
	break;
      } else {
	n++;
	if (cmds[i] == CMD_MOVE) {
	  i += 3;
	} else if (cmds[i] == CMD_LINE) {
	  i += 3;
	} else if (cmds[i] == CMD_CURVE) {
	  i += 7;
	}
      }
    }
    e = i;

    /* Reverse sub-path in [start_cmd, e) */

    a = new WXGC_ATOMIC double[e - start_cmd];
    cs = new WXGC_ATOMIC int[n];

    /* Traverse again to find command starts: */
    n = 0;
    i = start_cmd;
    while (i < e) {
      cs[n++] = i;
      if (cmds[i] == CMD_MOVE) {
	i += 3;
      } else if (cmds[i] == CMD_LINE) {
	i += 3;
      } else if (cmds[i] == CMD_CURVE) {
	i += 7;
      }
    }

    /* Reverse */
    controls = -1;
    pos = 0;
    for (j = n; j--; ) {
      i = cs[j];
      if (!start_with_line && (j == n - 1)) {
	a[pos++] = CMD_MOVE;
      } else if (controls >= 0) {
	a[pos++] = CMD_CURVE;
	a[pos++] = cmds[controls+3];
	a[pos++] = cmds[controls+4];
	a[pos++] = cmds[controls+1];
	a[pos++] = cmds[controls+2];
      } else {
	a[pos++] = CMD_LINE;
      }

      if ((cmds[i] == CMD_MOVE)
	  || (cmds[i] == CMD_LINE)) {
	a[pos++] = cmds[i+1];
	a[pos++] = cmds[i+2];
	controls = -1;
      } else if (cmds[i] == CMD_CURVE) {
	a[pos++] = cmds[i+5];
	a[pos++] = cmds[i+6];
	controls = i;
      }
    }

    memcpy(cmds + start_cmd, a, (e - start_cmd) * sizeof(double));

    start_cmd = e;
  }
}

void wxPath::AddPath(wxPath *p)
{
  int i, closed_n;

  if (!IsOpen()) {
    /* Simple case: this path is closed, so just append p */
    MakeRoom(p->cmd_size);
    last_cmd = cmd_size + p->last_cmd;
    for (i = 0; i < p->cmd_size; i++) {
      cmds[cmd_size++] = p->cmds[i];
    }
  } else {
    /* Put closed paths in p on the front of this path,
       and add unclosed paths to this path's unclosed
       path. */
    if (p->IsOpen()) {
      for (i = 0; i < p->cmd_size; i++) {
	if (p->cmds[i] == CMD_CLOSE)
	  break;
	else if (cmds[i] == CMD_CURVE)
	  i += 7;
	else
	  i += 3;
      }
      
      if (i < p->cmd_size) {
	closed_n = i + 1;
      } else {
	closed_n = 0;
      }
    } else {
      /* No open path in p */
      closed_n = p->cmd_size;
    }
    
    MakeRoom(p->cmd_size);
    memmove(cmds + closed_n, cmds, cmd_size * sizeof(double));
    memcpy(cmds, p->cmds, closed_n * sizeof(double));
    if (closed_n  < p->cmd_size) {
      /* There was an open path in p... */
      memcpy(cmds + cmd_size + closed_n, p->cmds + closed_n, (p->cmd_size - closed_n) * sizeof(double));

      /* p's open path must start with CMD_MOVE; change it to CMD_LINE */
      cmds[closed_n + cmd_size] = CMD_LINE;
    
      last_cmd = cmd_size + p->last_cmd;
    } else {
      /* No open path in p, so just adjust last_cmd */
      last_cmd += closed_n;
    }
    cmd_size += p->cmd_size;
  }
}

void wxPath::Install(long target, double dx, double dy, 
		     double ox, double oy, double sx, double sy,
		     Bool align, double pox, double poy)
{
  int i = 0;
  double lx = 0.0, ly = 0.0, lxx = 0.0, lyy = 0.0;

#ifdef WX_USE_CAIRO
  cairo_new_path(CAIRO_DEV);
#endif

  while (i < cmd_size) {
    if (cmds[i] == CMD_CLOSE) {
#ifdef WX_USE_CAIRO
      cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
      CGPathCloseSubpath(PATHPATH);;
#endif
#ifdef wx_msw
      wxGPathCloseFigure(PATH_GP);
#endif
      i += 1;
    } else if (cmds[i] == CMD_MOVE) {
      double xx, yy;
      xx = (cmds[i+1]+dx) * sx + ox;
      yy = (cmds[i+2]+dy) * sy + oy;
      if (align) {
	xx = floor(xx) + pox;
	yy = floor(yy) + poy;
      }

#ifdef WX_USE_CAIRO
      cairo_move_to(CAIRO_DEV, xx, yy);
#endif
#ifdef wx_mac
      CGPathMoveToPoint(PATHPATH, NULL, xx, yy);
#endif

      lx = cmds[i+1];
      ly = cmds[i+2];
      lxx = xx;
      lyy = yy;

      i += 3;
    } else if (cmds[i] == CMD_LINE) {
      if ((cmds[i+1] != lx) || (cmds[i+2] != ly)) {
	double xx, yy;
	xx = (cmds[i+1]+dx) * sx + ox;
	yy = (cmds[i+2]+dy) * sy + oy;
	if (align) {
	  xx = floor(xx) + pox;
	  yy = floor(yy) + poy;
	}

#ifdef WX_USE_CAIRO
	cairo_line_to(CAIRO_DEV, xx, yy);
#endif
#ifdef wx_mac
	CGPathAddLineToPoint(PATHPATH, NULL, xx, yy);
#endif
#ifdef wx_msw
	wxGPathAddLine(PATH_GP, lxx, lyy, xx, yy);
#endif

	lx = cmds[i+1];
	ly = cmds[i+2];
	lxx = xx;
	lyy = yy;
      }
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      if ((cmds[i+5] != lx) || (cmds[i+6] != ly)) {
	double xx, yy, xx1, yy1, xx2, yy2;
	xx = (cmds[i+5]+dx) * sx + ox;
	yy = (cmds[i+6]+dy) * sy + oy;
	if (align) {
	  xx = floor(xx) + pox;
	  yy = floor(yy) + poy;
	}

	xx1 = (cmds[i+1]+dx) * sx + ox;
	yy1 = (cmds[i+2]+dy) * sy + oy;
	xx2 = (cmds[i+3]+dx) * sx + ox;
	yy2 = (cmds[i+4]+dy) * sy + oy;

#ifdef WX_USE_CAIRO
	cairo_curve_to(CAIRO_DEV, xx1, yy1, xx2, yy2, xx, yy);
#endif
#ifdef wx_mac
	CGPathAddCurveToPoint(PATHPATH, NULL, xx1, yy1, xx2, yy2, xx, yy);
#endif
#ifdef wx_msw
	wxGPathAddBezier(PATH_GP, lxx, lyy, xx1, yy1, xx2, yy2, xx, yy);
#endif
	lx = cmds[i+5];
	ly = cmds[i+6];
	lxx = xx;
	lyy = yy;
      }
      i += 7;
    }
  }
}

void wxPath::InstallPS(wxPostScriptDC *dc, wxPSStream *s, double dx, double dy)
{
  int i = 0;

  while (i < cmd_size) {
    if (cmds[i] == CMD_CLOSE) {
      s->Out("closepath\n");
      i += 1;
    } else if ((cmds[i] == CMD_MOVE) 
	       || (cmds[i] == CMD_LINE)) {
      double x, y;
      x = dc->FLogicalToDeviceX(cmds[i+1]+ dx);
      y = dc->FLogicalToDeviceY(cmds[i+2]+ dy);
      s->Out(x); s->Out(" "); s->Out(y);
      if (cmds[i] == CMD_LINE)
	s->Out(" lineto\n");
      else
	s->Out(" moveto\n");
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      double x1, y1, x2, y2, x3, y3;
      x1 = dc->FLogicalToDeviceX(cmds[i+1] + dx);
      y1 = dc->FLogicalToDeviceY(cmds[i+2] + dy);
      x2 = dc->FLogicalToDeviceX(cmds[i+3] + dx);
      y2 = dc->FLogicalToDeviceY(cmds[i+4] + dy);
      x3 = dc->FLogicalToDeviceX(cmds[i+5] + dx);
      y3 = dc->FLogicalToDeviceY(cmds[i+6] + dy);
      s->Out(x1); s->Out(" "); s->Out(y1); s->Out(" "); 
      s->Out(x2); s->Out(" "); s->Out(y2); s->Out(" "); 
      s->Out(x3); s->Out(" "); s->Out(y3); s->Out(" "); 
      s->Out("curveto\n");
      i += 7;
    }
  }
}

static double my_round(double d)
{
  double i, frac;

  if (d < 0) {
    frac = modf(d, &i);
    if (frac < -0.5)
      return i - 1;
    else
      return i;
  } else {
    frac = modf(d, &i);
    if (frac < 0.5)
      return i;
    else
      return i + 1;
  }
}

int wxPath::ToPolygons(int **_lens, double ***_ptss, double sx, double sy)
{
  int i, cnt, *lens, len, alloc_len, need_len;
  double lx, ly, **ptss, *pts, *naya;

  cnt = 0;
  for (i = 0; i < cmd_size; ) {
    if (cmds[i] == CMD_CLOSE) {
      cnt++;
      i += 1;
    } else if (cmds[i] == CMD_MOVE) {
      i += 3;
    } else if (cmds[i] == CMD_LINE) {
      i += 3;
    } else if (cmds[i] == CMD_CURVE) {
      i += 7;
    }
  }

  if (IsOpen())
    cnt++;

  ptss = new WXGC_PTRS double*[cnt];
  lens = new WXGC_ATOMIC int[cnt];
  cnt = 0;

  pts = NULL;
  len = 0;
  alloc_len = 0;
  lx = ly = 0;

  for (i = 0; i < cmd_size; ) {
    if (cmds[i] == CMD_CLOSE) {
      ptss[cnt] = pts;
      lens[cnt] = len;
      cnt++;

      len = 0;
      alloc_len = 0;
      pts = NULL;
      lx = ly = 0;

      i += 1;
    } else {
      if ((cmds[i] == CMD_MOVE)
	  || (cmds[i] == CMD_LINE)) {
	need_len = 1;
      } else if (cmds[i] == CMD_CURVE) {
	double dx, dy;
	dx = sx * (lx - cmds[i + 5]);
	dy = sy * (ly - cmds[i + 6]);
	if (dx < 0) dx = -dx;
	if (dy < 0) dy = -dy;
	if (dx > dy)
	  need_len = (int)ceil(dx);
	else
	  need_len = (int)ceil(dy);
	need_len += 1;
      } else {
	need_len = 0;
      }

      if (len + (2 * need_len) > alloc_len) {
	int l;
	l = (len + (2 * need_len)) * 2;
	naya = new WXGC_ATOMIC double[l];
	memcpy(naya, pts, len * sizeof(double));
	pts = naya;
	alloc_len = l;
      }

      if ((cmds[i] == CMD_MOVE)
	  || (cmds[i] == CMD_LINE)) {
	lx = cmds[i+1];
	ly = cmds[i+2];
	pts[len++] = lx;
	pts[len++] = ly;
	i += 3;
      } else if (cmds[i] == CMD_CURVE) {
	int d;
	double x0 = lx, x1 = cmds[i+1], x2 = cmds[i+3], x3 = cmds[i+5];
	double y0 = ly, y1 = cmds[i+2], y2 = cmds[i+4], y3 = cmds[i+6];
	double ax = (((x3 - (x2 * 3)) + (x1 * 3)) - x0);
	double ay = (((y3 - (y2 * 3)) + (y1 * 3)) - y0);
	double bx = (((x2 * 3) - (x1 * 6)) + (x0 * 3));
	double by = (((y2 * 3) - (y1 * 6)) + (y0 * 3));
	double cx = ((x1 * 3) - (x0 * 3));
	double cy = ((y1 * 3) -  (y0 * 3));
	double dx = x0, dy = y0, tt, x, y;

	for (d = 0; d < need_len; d++) {
	  tt = ((double)d / (double)(need_len - 1));
	  x = ((((((tt * ax) + bx) * tt) + cx) * tt) + dx);
	  y = ((((((tt * ay) + by) * tt) + cy) * tt) + dy);
	  if ((d > 0) && (d < need_len-1)) {
	    /* We've generating points to map to pixels
	       after scaling, so round intermediate points.
	       End point have to be floored, for consistency
	       with everything else, so leave them alone. */
	    x = my_round(x * sx) / sx;
	    y = my_round(y * sy) / sy;
	  }
	  pts[len++] = x;
	  pts[len++] = y;
	}

	lx = x3;
	ly = y3;

	i += 7;
      }
    }
  }

  if (IsOpen()) {
    ptss[cnt] = pts;
    lens[cnt] = len;
    cnt++;
  }

  *_lens = lens;
  *_ptss = ptss;

  return cnt;
}

void wxPath::BoundingBox(double *_x1, double *_y1, double *_x2, double *_y2)
{
  double x1, x2, y1, y2;
  int i;

  if (cmd_size) {
    /* First command must be move-to: */
    x1 = cmds[1];
    y1 = cmds[2];
    x2 = x1;
    y2 = y1;
    for (i = 3; i < cmd_size; ) {
      if (cmds[i] == CMD_CLOSE) {
	i += 1;
      } else if ((cmds[i] == CMD_MOVE)
		 || (cmds[i] == CMD_LINE)) {
	if (cmds[i+1] < x1)
	  x1 = cmds[i+1];
	if (cmds[i+1] > x2)
	  x2 = cmds[i+1];
	if (cmds[i+2] < y1)
	  y1 = cmds[i+2];
	if (cmds[i+2] > y2)
	  y2 = cmds[i+2];
	i += 3;
      } else if (cmds[i] == CMD_CURVE) {
	int j;
	for (j = 0; j < 6; j += 2) {
	  if (cmds[i+j+1] < x1)
	    x1 = cmds[i+j+1];
	  if (cmds[i+j+1] > x2)
	    x2 = cmds[i+j+1];
	  if (cmds[i+j+2] < y1)
	    y1 = cmds[i+j+2];
	  if (cmds[i+j+2] > y2)
	    y2 = cmds[i+j+2];
	}
	i += 7;
      }
    }
  } else {
    x1 = y1 = x2 = y2 = 0.0;
  }

  *_x1 = x1;
  *_x2 = x2;
  *_y1 = y1;
  *_y2 = y2;
}
