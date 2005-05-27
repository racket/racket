#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxScrollEvent(class wxScrollEvent *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxScrollEvent *objscheme_unbundle_wxScrollEvent(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxDC(class wxDC *);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxGLConfig *objscheme_unbundle_wxGLConfig(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxCanvas(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxCanvas(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxCanvas(class wxCanvas *realobj);
class wxCanvas *objscheme_unbundle_wxCanvas(Scheme_Object *obj, const char *where, int nullOK);
#endif
