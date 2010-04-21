#ifndef WXS_SETUP_ONLY
extern class wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxItem(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxItem(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxItem(class wxItem *realobj);
class wxItem *objscheme_unbundle_wxItem(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
extern int objscheme_istype_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxBitmap(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMessage(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMessage(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMessage(class wxMessage *realobj);
class wxMessage *objscheme_unbundle_wxMessage(Scheme_Object *obj, const char *where, int nullOK);
#endif
