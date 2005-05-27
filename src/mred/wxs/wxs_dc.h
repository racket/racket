#ifndef WXS_SETUP_ONLY
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxGL(class wxGL *);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern Scheme_Object *objscheme_bundle_wxPen(class wxPen *);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
extern Scheme_Object *objscheme_bundle_wxBrush(class wxBrush *);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxBrush(Scheme_Object *, const char *, int);
extern class wxBrush *objscheme_unbundle_wxBrush(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPen(Scheme_Object *, const char *, int);
extern class wxPen *objscheme_unbundle_wxPen(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxRegion(class wxRegion *);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxPath *objscheme_unbundle_wxPath(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxDC(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxDC(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxDC(class wxDC *realobj);
class wxDC *objscheme_unbundle_wxDC(Scheme_Object *obj, const char *where, int nullOK);
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxDCGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMemoryDC(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMemoryDC(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMemoryDC(class wxMemoryDC *realobj);
class wxMemoryDC *objscheme_unbundle_wxMemoryDC(Scheme_Object *obj, const char *where, int nullOK);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPostScriptDC(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPostScriptDC(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPostScriptDC(class wxPostScriptDC *realobj);
class wxPostScriptDC *objscheme_unbundle_wxPostScriptDC(Scheme_Object *obj, const char *where, int nullOK);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_basePrinterDC(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_basePrinterDC(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_basePrinterDC(class basePrinterDC *realobj);
class basePrinterDC *objscheme_unbundle_basePrinterDC(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxGL(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxGL(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxGL(class wxGL *realobj);
class wxGL *objscheme_unbundle_wxGL(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxGLConfig(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxGLConfig(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxGLConfig(class wxGLConfig *realobj);
class wxGLConfig *objscheme_unbundle_wxGLConfig(Scheme_Object *obj, const char *where, int nullOK);
#endif
