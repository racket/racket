#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxListBox(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxListBox(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxListBox(class wxListBox *realobj);
class wxListBox *objscheme_unbundle_wxListBox(Scheme_Object *obj, const char *where, int nullOK);
#endif
