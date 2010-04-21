#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxMenuBar(class wxMenuBar *);
extern class wxMenuBar *objscheme_unbundle_wxMenuBar(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxFrame(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxFrame(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFrame(class wxFrame *realobj);
class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *obj, const char *where, int nullOK);
#endif
