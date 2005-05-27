#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxGLConfig(class wxGLConfig *);
extern class wxGLConfig *objscheme_unbundle_wxGLConfig(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxBitmap(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBitmap(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *realobj);
class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *obj, const char *where, int nullOK);
#endif
