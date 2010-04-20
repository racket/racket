#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxChoice(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxChoice(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxChoice(class wxChoice *realobj);
class wxChoice *objscheme_unbundle_wxChoice(Scheme_Object *obj, const char *where, int nullOK);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxChoiceGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
