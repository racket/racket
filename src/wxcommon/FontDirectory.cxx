/*								-*- C++ -*-
 *
 * Purpose: wxWindows font name handling
 *
 * Authors: Markus Holzem, Julian Smart, and Matthew Flatt
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 * USA.
 */

#ifdef __GNUG__
#pragma implementation
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#ifdef wx_xt
# define  Uses_wxApp
# define  Uses_wxFontNameDirectory
# include "wx.h"
# include <string.h>
#endif

char *font_defaults[] = {
  "PostScriptMediumStraight", "",
  "PostScriptMediumItalic", "-Oblique",
  "PostScriptMediumSlant", "-Oblique",
  "PostScriptLightStraight", "",
  "PostScriptLightItalic", "-Oblique",
  "PostScriptLightSlant", "-Oblique",
  "PostScriptBoldStraight", "-Bold",
  "PostScriptBoldItalic", "-BoldOblique",
  "PostScriptBoldSlant", "-BoldOblique",

  "PostScript___", "${PostScript$[family],$[weight],$[style]}",

  "PostScriptSystem__", "${PostScriptTimes,$[weight],$[style]}",
  "PostScriptRoman__", "${PostScriptTimes,$[weight],$[style]}",
  "PostScriptScript__", "ZapfChancery-MediumItalic",

  "PostScriptTimesMedium", "",
  "PostScriptTimesLight", "",
  "PostScriptTimesBold", "Bold",

  "PostScriptTimes__", "Times${PostScript$[weight]$[style]}",
  "PostScriptTimesMediumStraight", "Times-Roman",
  "PostScriptTimesLightStraight", "Times-Roman",
  "PostScriptTimes_Slant", "Times-${PostScriptTimes$[weight]}Italic",
  "PostScriptTimes_Italic", "Times-${PostScriptTimes$[weight]}Italic",

  "PostScriptDefault__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptSwiss__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptDecorative__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptModern__", "Courier${PostScript$[weight]$[style]}",
  "PostScriptSymbol__", "Symbol",

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#ifdef wx_x
  "ScreenMedium", "medium",
  "ScreenBold", "bold",
  "ScreenLight", "light",
  "ScreenStraight", "r",
  "ScreenItalic", "i",
  "ScreenSlant", "o",

  "ScreenSystemBase", "*-lucida",
  "ScreenDefaultBase", "*-lucida",
  "ScreenRomanBase", "*-times",
  "ScreenDecorativeBase", "*-helvetica",
  "ScreenModernBase", "*-courier",
  "ScreenTeletypeBase", "*-lucidatypewriter",
  "ScreenSwissBase", "*-lucida",
  "ScreenScriptBase", "*-zapf chancery",
  "ScreenSymbolBase", "*-symbol",

  "ScreenStdSuffix", "-${Screen$[weight]}-${Screen$[style]}"
    "-normal-*-*-%d-*-*-*-*-*-*",

  "ScreenSystem__",
  "+-${ScreenSystemBase}${ScreenStdSuffix}",
  "ScreenDefault__",
  "+-${ScreenDefaultBase}${ScreenStdSuffix}",
  "ScreenRoman__",
  "+-${ScreenRomanBase}${ScreenStdSuffix}",
  "ScreenDecorative__",
  "+-${ScreenDecorativeBase}${ScreenStdSuffix}",
  "ScreenModern__",
  "+-${ScreenModernBase}${ScreenStdSuffix}",
  "ScreenTeletype__",
  "+-${ScreenTeletypeBase}${ScreenStdSuffix}",
  "ScreenSwiss__",
  "+-${ScreenSwissBase}${ScreenStdSuffix}",
  "ScreenScript__",
  "+-${ScreenScriptBase}-medium-i-normal-*-*-%d-*-*-*-*-*-*",
  "ScreenSymbol__",
  "+-${ScreenSymbolBase}-medium-r-normal-*-*-%d-*-*-*-*-*-*",
#endif

#ifdef wx_msw
  "ScreenSystem__", "MS Sans Serif", /* maybe changed by Adjust */ 
  "ScreenDefault__", "MS Sans Serif", /* maybe changed by Adjust */ 
  "ScreenRoman__", "Times New Roman",
  "ScreenDecorative__", "Arial",
  "ScreenModern__", "Courier New",
  "ScreenTeletype__", "${ScreenModern$[weight];$[style]}",
  "ScreenSwiss__", "Arial",
  "ScreenScript__", "Arial",
  "ScreenSymbol__", "Symbol",
#endif

#ifdef wx_mac
# ifdef OS_X
  "ScreenDefault__", "Lucida Grande",
  "ScreenSystem__", "Lucida Grande",
  "ScreenDecorative__", "Arial", /* "Geneva" looks bad at 12pt in Quartz with QD spacing */
  "ScreenModern__", "Courier New", /* "Courier" is worse without Quartz */
  "ScreenScript__", "Apple Chancery",
# else
  "ScreenDefault__", "applicationfont",
  "ScreenSystem__", "systemfont",
  "ScreenDecorative__", "Geneva",
  "ScreenModern__", "Monaco", /* "courier" is also good */
  "ScreenScript__", "Zapf Chancery",
# endif
  "ScreenRoman__", "Times",
  "ScreenTeletype__", "${ScreenModern,$[weight],$[style]}",
  "ScreenSwiss__", "Helvetica",
  "ScreenSymbol__", "Symbol",
#endif

  NULL
};

#ifdef wx_msw
static int CALLBACK check_font_here(ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme,
				       DWORD FontType, LPARAM lParam)
{
  *(int *)lParam = 1;
  return 0;
}

int check_avail(char *name)
{
  int present = 0;
  LOGFONT lf;
  HDC dc;
  
  lf.lfCharSet = DEFAULT_CHARSET;
  strcpy(lf.lfFaceName, name);
  lf.lfPitchAndFamily = 0;
  dc = GetDC(NULL);
  EnumFontFamiliesEx(dc, &lf, (FONTENUMPROC)check_font_here, (LPARAM)&present, 0);
  ReleaseDC(NULL, dc);

  return present;
}
#endif

#ifdef WX_USE_XFT
extern int wxXRenderHere(void);
#endif

static void AdjustFontDefaults(void)
{
#ifdef wx_xt
# ifdef WX_USE_XFT
  if (wxXRenderHere()) {
    int i;  
    
    for (i = 0; font_defaults[i]; i += 2) {
      if (!strcmp(font_defaults[i], "ScreenSystem__")) {
	font_defaults[i + 1] = " Sans";
      } else if (!strcmp(font_defaults[i], "ScreenDefault__")) {
	font_defaults[i + 1] = " Sans";
      } else if (!strcmp(font_defaults[i], "ScreenRoman__")) {
	font_defaults[i + 1] = " Serif";
      } else if (!strcmp(font_defaults[i], "ScreenDecorative__")) {
	font_defaults[i + 1] = " Nimbus Sans L";
      } else if (!strcmp(font_defaults[i], "ScreenModern__")) {
	font_defaults[i + 1] = " Monospace";
      } else if (!strcmp(font_defaults[i], "ScreenTeletype__")) {
	font_defaults[i + 1] = " Monospace";
      } else if (!strcmp(font_defaults[i], "ScreenSwiss__")) {
	font_defaults[i + 1] = " Nimbus Sans L";
      } else if (!strcmp(font_defaults[i], "ScreenScript__")) {
	font_defaults[i + 1] = " URW Chancery L";
      } else if (!strcmp(font_defaults[i], "ScreenSymbol__")) {
	font_defaults[i + 1] = " Standard Symbols L,Nimbus Sans L";
      }
    }
  }
# endif
#endif
#ifdef wx_msw
  int i;

  for (i = 0; font_defaults[i]; i += 2) {
    if (!strcmp(font_defaults[i], "ScreenDefault__")) {
      /* We'd prefer to use "Microsoft Sans Serif", instead,
	 in XP, because that's an outline font. */
      if (check_avail("Microsoft Sans Serif")) {
	font_defaults[i + 1] = "Microsoft Sans Serif";
      }
    } else if (!strcmp(font_defaults[i], "ScreenSystem__")) {
      /* 2000 and XP use Tahoma by default for controls */
      static int known = 0, present = 0;
      if (check_avail("Tahoma")) {
	font_defaults[i+1] = "Tahoma";
      }
    }
  }
#endif
}

wxFontNameDirectory *wxTheFontNameDirectory;

enum {
  wxWEIGHT_NORMAL,
  wxWEIGHT_BOLD,
  wxWEIGHT_LIGHT,
  wxNUM_WEIGHTS
  };

enum {
  wxSTYLE_NORMAL,
  wxSTYLE_ITALIC,
  wxSTYLE_SLANT,
  wxNUM_STYLES
  };

class wxSuffixMap {
 public:
  char *map[wxNUM_WEIGHTS][wxNUM_STYLES];
  void Initialize(const char *, const char *, int weight, int style, int fam);

  wxSuffixMap();

#ifdef MZ_PRECISE_GC
  void gcMark();
  void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
void wxSuffixMap::gcMark() {
  int i, j;
  for (i = 0; i < wxNUM_WEIGHTS; i++)
    for (j = 0; j < wxNUM_STYLES; j++) {
      gcMARK_TYPED(char *, map[i][j]);
    }
}
void wxSuffixMap::gcFixup() {
  int i, j;
  for (i = 0; i < wxNUM_WEIGHTS; i++)
    for (j = 0; j < wxNUM_STYLES; j++) {
      gcFIXUP_TYPED(char *, map[i][j]);
    }
}
END_XFORM_SKIP;
#endif

wxSuffixMap::wxSuffixMap() {
  int i, j;
  for (i = 0; i < wxNUM_WEIGHTS; i++) {
    for (j = 0; j < wxNUM_STYLES; j++) {
      map[i][j] = NULL;
    }
  }
}

class wxFontNameItem : public wxObject
{
 public:
  int id;
  int family;
  char *name;
  wxSuffixMap *screen, *printing;
  Bool isfamily;
  wxFontNameItem();
};

wxFontNameItem::wxFontNameItem()
: wxObject(WXGC_NO_CLEANUP)
{
  screen = new WXGC_PTRS wxSuffixMap;
  printing = new WXGC_PTRS wxSuffixMap;
}

static int WCoordinate(int w)
{
  switch (w) {
  case wxBOLD:
    return wxWEIGHT_BOLD;
  case wxLIGHT:
    return wxWEIGHT_LIGHT;
  case wxNORMAL:
  default:
    return wxWEIGHT_NORMAL;
  }
}

static int SCoordinate(int s)
{
  switch (s) {
  case wxITALIC:
    return wxSTYLE_ITALIC;
  case wxSLANT:
    return wxSTYLE_SLANT;
  case wxNORMAL:
  default:
    return wxSTYLE_NORMAL;
  }
}

wxFontNameDirectory::wxFontNameDirectory(void)
{
  wxHashTable *ht;
  ht = new WXGC_PTRS wxHashTable(wxKEY_INTEGER, 20);
  table = ht;
  nextFontId = 100; /* Larger than all family ids */
}

wxFontNameDirectory::~wxFontNameDirectory()
{
  DELETE_OBJ table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId++);
}

int wxGetPreference(const char *name, char *res, long len);

static char pref_buf[1024];

static void SearchResource(const char *prefix, const char **names, int count, char **v)
{
  int k, i, j;
  char resource[1024], *internal;
  GC_CAN_IGNORE char **defaults;

  k = 1 << count;

  *v = NULL;
  internal = NULL;

  for (i = 0; i < k; i++) {
    strcpy(resource, prefix);
    for (j = 0; j < count; j++) {
      if (!(i & (1 << j)))
	strcat(resource, names[j]);
      else
	strcat(resource, "_");
    }

    if (wxGetPreference((char *)resource, pref_buf, 1024) && *pref_buf) {
      *v = pref_buf;
      return;
    }

    if (!internal) {
      defaults = font_defaults;
      while (*defaults) {
	if (!strcmp(*defaults, resource)) {
	  internal = defaults[1];
	  break;
	}
	defaults += 2;
      }
    }
  }

  if (internal) {
    char *s;
    s = copystring(internal);
    *v = s;
  }
}

void wxInitializeFontNameDirectory(void)
{
  AdjustFontDefaults();

  wxREGGLOB(wxTheFontNameDirectory);
  wxTheFontNameDirectory = new WXGC_PTRS wxFontNameDirectory;
  wxTheFontNameDirectory->Initialize(wxSYSTEM, wxSYSTEM, "System");
  wxTheFontNameDirectory->Initialize(wxDEFAULT, wxDEFAULT, "Default");
  wxTheFontNameDirectory->Initialize(wxDECORATIVE, wxDECORATIVE, "Decorative");
  wxTheFontNameDirectory->Initialize(wxROMAN, wxROMAN, "Roman");
  wxTheFontNameDirectory->Initialize(wxMODERN, wxMODERN, "Modern");
  wxTheFontNameDirectory->Initialize(wxTELETYPE, wxTELETYPE, "Teletype");
  wxTheFontNameDirectory->Initialize(wxSWISS, wxSWISS, "Swiss");
  wxTheFontNameDirectory->Initialize(wxSCRIPT, wxSCRIPT, "Script");
  wxTheFontNameDirectory->Initialize(wxSYMBOL, wxSYMBOL, "Symbol");
}

void wxSuffixMap::Initialize(const char *resname, const char *devresname,
			     int wt, int st, int fam)
{
  const char *weight, *style;
  char *v = NULL;
  int i, drn;

  {
    switch (wt) {
    case wxWEIGHT_NORMAL:
      weight = "Medium";
      break;
    case wxWEIGHT_LIGHT:
      weight = "Light";
      break;
    case wxWEIGHT_BOLD:
      default:
      weight = "Bold";
    }
    {
      int len, closer = 0, startpos = 0;
      const char *rnames[3];

      switch (st) {
      case wxSTYLE_NORMAL:
	style = "Straight";
	break;
      case wxSTYLE_ITALIC:
	style = "Italic";
	break;
      case wxSTYLE_SLANT:
      default:
	style = "Slant";
      }

      rnames[0] = resname;
      rnames[1] = weight;
      rnames[2] = style;
      
      SearchResource(devresname, rnames, 3, &v);

      /* Expand macros in the found string: */
    found:
      len = (v ? strlen(v) : 0);
      for (i = 0; i < len; i++) {
	if (v[i] == '$' && ((v[i+1] == '[') || (v[i+1] == '{'))) {
	  startpos = i;
	  if (v[i+1] == '[')
	    closer = ']';
	  else
	    closer = '}';
	  i++;
	} else if (v[i] == closer) {
	  int newstrlen, noff;
	  const char *r = NULL;
	  char *naya, *name;
	  
	  noff = startpos + 2;
	  name = v;
	  v[i] = 0;

	  if (closer == '}') {
	    int i, count, len;
	    char **names;

	    for (i = 0, count = 1; name[i + noff]; i++) {
	      if (name[i + noff] == ',') {
		count++;
		name[i + noff] = 0;
	      }
	    }
	    
	    len = i;

	    names = new WXGC_PTRS char*[count];

	    {
	      char *cs;
	      cs = COPYSTRING_TO_ALIGNED(name, noff);
	      names[0] = cs;
	    }
	    for (i = 0, count = 1; i < len; i++) {
	      if (!name[i + noff]) {
		{
		  char *cs;
		  cs = COPYSTRING_TO_ALIGNED(name, i + 1 + noff);
		  names[count++] = cs;
		}
	      }
	    }

	    SearchResource("", (const char **)names, count, (char **)&r);

	    if (!r) {
	      for (i = 0; i < len; i++) {
		if (!name[i + noff])
		  name[i + noff] = ',';
	      }
	      r = "";
	      printf("Bad resource name \"%s\" in font lookup\n", name + noff);
	    }
	  } else if (!strcmp(name + noff, "weight")) {
	    r = weight;
	  } else if (!strcmp(name + noff, "style")) {
	    r = style;
	  } else if (!strcmp(name + noff, "family")) {
	    switch (fam) {
	    case wxSYSTEM:
	      r = "System";
	      break;
	    case wxDECORATIVE:
	      r = "Decorative";
	      break;
	    case wxROMAN:
	      r = "Roman";
	      break;
	    case wxMODERN:
	      r = "Modern";
	      break;
	    case wxTELETYPE:
	      r = "Teletype";
	      break;
	    case wxSWISS:
	      r = "Swiss";
	      break;
	    case wxSCRIPT:
	      r = "Script";
	      break;
	    case wxSYMBOL:
	      r = "Symbol";
	      break;
	    default:
	      r = "Default";
	    }
	  } else {
	    r = "";
	    printf("Bad font macro name \"%s\"\n", name + noff);
	  }
	  newstrlen = strlen(r);

	  naya = new WXGC_ATOMIC char[len + newstrlen + 1];
	  memcpy(naya, v, startpos);
	  memcpy(naya + startpos, r, newstrlen);
	  memcpy(naya + startpos + newstrlen, v + i + 1, len - i + 1);

	  v = naya;

	  goto found;
	}
      }

      drn = ((resname[0] == '@') ? 1 : 0);

#if defined(wx_msw) || defined(wx_mac)
      if (!v)
	v = copystring(resname + drn);
#endif
#ifdef wx_x
      if (!strcmp(devresname, "Screen")) {
	if (v && (v[0] == '+')) {
	  memmove(v, v + 1, strlen(v));
	} else {
	  int len, ds;
	  char *src;
	  char *normalcy;
	  /* Build name using special heuristics:
	     -([^-]*) => -*-\1-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     -([^-]*)-(.*) => -\1-\2-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     ([^-].*[^-]) => \1
	     */

	  if (v) {
	    src = (char *)v;
	    ds = 0;
	  } else {
	    src = (char *)resname;
	    ds = drn;
	  }

	  len = strlen(src XFORM_OK_PLUS ds);
	  if (src[ds] == '-') {
	    char *prefix;
	    int c = 0;
	    for (i = 0; i < len; i++) {
	      if (src[ds + i] == '-')
		c++;
	    }
	    
	    v = new WXGC_ATOMIC char[len + 40];
	    if (c < 2)
	      prefix = "-*";
	    else
	      prefix = "";
	    
	    if (c < 3) {
	      switch (wt) {
	      case wxWEIGHT_NORMAL:
		weight = "-medium";
		break;
	      case wxWEIGHT_LIGHT:
		weight = "-light";
		break;
	      case wxWEIGHT_BOLD:
	      default:
		weight = "-bold";
	      }
	    } else
	      weight = "";
	    
	    if (c < 4) {
	      switch (st) {
	      case wxSTYLE_NORMAL:
		style = "-r";
	      break;
	      case wxSTYLE_ITALIC:
		style = "-i";
		break;
	      case wxSTYLE_SLANT:
	      default:
		style = "-o";
	      }
	    } else
	      style = "";
	    
	    if (c < 5)
	      normalcy = "-normal";
	    else
	      normalcy = "";
	    
	    sprintf(v, "%s%s%s%s%s-*-*-%%d-*-*-*-*-*-*",
		    prefix, src + ds, weight, style, normalcy);
	  } else
	    v = COPYSTRING_TO_ALIGNED(src, ds);
	}
      }
#endif

      /* We have a final value: */
      map[wt][st] = v;
    }
  }
}

void wxFontNameDirectory::Initialize(int fontid, int family, const char *resname)
{
  wxFontNameItem *item;
  
  item = new WXGC_PTRS wxFontNameItem;

  item->id = fontid;
  item->family = family;
  item->isfamily = (resname[0] != '@');
  item->name = copystring(resname);

  table->Put(fontid, item);
}

int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;
  char *s;

  if ((id = GetFontId(name, family)))
    return id;

  id = GetNewFontId();
  s = new WXGC_ATOMIC char[strlen(name) + 2];
  strcpy(s + 1, name);
  s[0] = '@';
  Initialize(id, family, s);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  int wt, st;
  wxFontNameItem *item;

  item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  wt = WCoordinate(weight);
  st = SCoordinate(style);

  /* Check for init */
  if (!item->screen->map[wt][st])
    item->screen->Initialize(item->name, "Screen", wt, st, item->family);

  return item->screen->map[wt][st];
}

void wxFontNameDirectory::SetScreenName(int fontid, int weight, int style, char *s)
{
  int wt, st;
  wxFontNameItem *item;

  item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return;

  wt = WCoordinate(weight);
  st = SCoordinate(style);

#ifdef wx_x
  {
    /* Safety: name must be less than 500 chars, and must not contain %
       except maybe one instance of %d. */
    int i, found_d = 0;
    for (i = 0; s[i]; i++) {
      if (i > 500) {
	s = NULL;
	break;
      }
      if (s[i] == '%') {
	if (found_d || (s[i+1] != 'd')) {
	  s = NULL;
	  break;
	} else
	  found_d = 1;
      }
    }
  }

  if (!s)
    return;
#endif

  item->screen->map[wt][st] = s;
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  int wt, st;
  wxFontNameItem *item;

  item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  wt = WCoordinate(weight);
  st = SCoordinate(style);

  /* Check for init */
  if (!item->printing->map[wt][st])
    item->printing->Initialize(item->name, "PostScript", wt, st, item->family);

  return item->printing->map[wt][st];
}

void wxFontNameDirectory::SetPostScriptName(int fontid, int weight, int style, char *s)
{
  int wt, st;
  wxFontNameItem *item;

  item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  wt = WCoordinate(weight);
  st = SCoordinate(style);

  item->printing->map[wt][st] = s;
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item;
  item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  if (item->isfamily)
    return NULL;

  return item->name + 1;
}

int wxFontNameDirectory::GetFontId(const char *name, int family)
{
  wxNode *node;

  table->BeginFind();

  while ((node = table->Next())) {
    wxFontNameItem *item;
    item = (wxFontNameItem *)node->Data();
    if (!item->isfamily 
	&& !strcmp(name, item->name+1)
	&& item->family == family)
      return item->id;
  }

  return 0;
}

int wxFontNameDirectory::GetFamily(int fontid)
{
  wxFontNameItem *item;
  item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return wxDEFAULT;

  return item->family;
}


