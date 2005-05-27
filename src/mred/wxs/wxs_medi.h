#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxSnip(class wxSnip *);
extern class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *, const char *, int);
extern class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxCursor *objscheme_unbundle_wxCursor(Scheme_Object *, const char *, int);
extern class wxStyleList *objscheme_unbundle_wxStyleList(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxStyleList(class wxStyleList *);
extern Scheme_Object *objscheme_bundle_wxKeymap(class wxKeymap *);
extern class wxKeymap *objscheme_unbundle_wxKeymap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxDC(class wxDC *);
extern class wxMediaAdmin *objscheme_unbundle_wxMediaAdmin(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxMediaAdmin(class wxMediaAdmin *);
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
extern class wxSnip *objscheme_unbundle_wxSnip(Scheme_Object *, const char *, int);
extern class wxStyle *objscheme_unbundle_wxStyle(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMediaBuffer(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaBuffer(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaBuffer(class wxMediaBuffer *realobj);
class wxMediaBuffer *objscheme_unbundle_wxMediaBuffer(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxBufferDataClassList(class wxBufferDataClassList *);
extern Scheme_Object *objscheme_bundle_wxSnipClassList(class wxSnipClassList *);
extern class wxKeymap *objscheme_unbundle_wxKeymap(Scheme_Object *, const char *, int);
extern class wxKeymap *objscheme_unbundle_wxKeymap(Scheme_Object *, const char *, int);
extern class wxKeymap *objscheme_unbundle_wxKeymap(Scheme_Object *, const char *, int);
extern class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *, const char *, int);
extern class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *, const char *, int);
extern class wxMediaStreamIn *objscheme_unbundle_wxMediaStreamIn(Scheme_Object *, const char *, int);
extern class wxMediaStreamIn *objscheme_unbundle_wxMediaStreamIn(Scheme_Object *, const char *, int);
extern class wxMediaStreamIn *objscheme_unbundle_wxMediaStreamIn(Scheme_Object *, const char *, int);
extern class wxMediaStreamInBase *objscheme_unbundle_wxMediaStreamInBase(Scheme_Object *, const char *, int);
extern class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *, const char *, int);
extern class wxMediaStreamOutBase *objscheme_unbundle_wxMediaStreamOutBase(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMediaGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
