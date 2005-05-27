#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern class wxCursor *objscheme_unbundle_wxCursor(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxCursor(class wxCursor *);
#endif
void objscheme_setup_wxWindow(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxWindow(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *realobj);
class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *obj, const char *where, int nullOK);
#endif
