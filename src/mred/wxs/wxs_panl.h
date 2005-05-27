#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxDialogBox(Scheme_Object *, const char *, int);
extern class wxDialogBox *objscheme_unbundle_wxDialogBox(Scheme_Object *, const char *, int);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPanel(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPanel(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPanel(class wxPanel *realobj);
class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxDialogBox(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxDialogBox(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxDialogBox(class wxDialogBox *realobj);
class wxDialogBox *objscheme_unbundle_wxDialogBox(Scheme_Object *obj, const char *where, int nullOK);
#endif
