#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxFont(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxFont(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFont(class wxFont *realobj);
class wxFont *objscheme_unbundle_wxFont(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
#endif
void objscheme_setup_wxFontList(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxFontList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFontList(class wxFontList *realobj);
class wxFontList *objscheme_unbundle_wxFontList(Scheme_Object *obj, const char *where, int nullOK);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
#endif
void objscheme_setup_wxColour(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxColour(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxColour(class wxColour *realobj);
class wxColour *objscheme_unbundle_wxColour(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
#endif
void objscheme_setup_wxColourDatabase(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxColourDatabase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxColourDatabase(class wxColourDatabase *realobj);
class wxColourDatabase *objscheme_unbundle_wxColourDatabase(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxPoint(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPoint(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPoint(class wxPoint *realobj);
class wxPoint *objscheme_unbundle_wxPoint(Scheme_Object *obj, const char *where, int nullOK);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxBrush(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBrush(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBrush(class wxBrush *realobj);
class wxBrush *objscheme_unbundle_wxBrush(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBrush(class wxBrush *);
#endif
void objscheme_setup_wxBrushList(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBrushList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBrushList(class wxBrushList *realobj);
class wxBrushList *objscheme_unbundle_wxBrushList(Scheme_Object *obj, const char *where, int nullOK);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPen(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPen(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPen(class wxPen *realobj);
class wxPen *objscheme_unbundle_wxPen(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxPen(class wxPen *);
#endif
void objscheme_setup_wxPenList(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPenList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPenList(class wxPenList *realobj);
class wxPenList *objscheme_unbundle_wxPenList(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxCursor(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxCursor(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxCursor(class wxCursor *realobj);
class wxCursor *objscheme_unbundle_wxCursor(Scheme_Object *obj, const char *where, int nullOK);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxPath *objscheme_unbundle_wxPath(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxDC(class wxDC *);
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxRegion(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxRegion(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxRegion(class wxRegion *realobj);
class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *obj, const char *where, int nullOK);
extern class wxPath *objscheme_unbundle_wxPath(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPath(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPath(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPath(class wxPath *realobj);
class wxPath *objscheme_unbundle_wxPath(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxFontNameDirectory(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxFontNameDirectory(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFontNameDirectory(class wxFontNameDirectory *realobj);
class wxFontNameDirectory *objscheme_unbundle_wxFontNameDirectory(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxFontNameDirectory(class wxFontNameDirectory *);
extern Scheme_Object *objscheme_bundle_wxFontList(class wxFontList *);
extern Scheme_Object *objscheme_bundle_wxPenList(class wxPenList *);
extern Scheme_Object *objscheme_bundle_wxBrushList(class wxBrushList *);
extern Scheme_Object *objscheme_bundle_wxColourDatabase(class wxColourDatabase *);
#endif
void objscheme_setup_wxGDIGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
