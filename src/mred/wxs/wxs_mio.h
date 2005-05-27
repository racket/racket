#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxMediaStreamInBase(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamInBase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamInBase(class wxMediaStreamInBase *realobj);
class wxMediaStreamInBase *objscheme_unbundle_wxMediaStreamInBase(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxMediaStreamOutBase(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamOutBase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamOutBase(class wxMediaStreamOutBase *realobj);
class wxMediaStreamOutBase *objscheme_unbundle_wxMediaStreamOutBase(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxMediaStreamInStringBase(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamInStringBase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamInStringBase(class wxMediaStreamInStringBase *realobj);
class wxMediaStreamInStringBase *objscheme_unbundle_wxMediaStreamInStringBase(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxMediaStreamOutStringBase(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamOutStringBase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamOutStringBase(class wxMediaStreamOutStringBase *realobj);
class wxMediaStreamOutStringBase *objscheme_unbundle_wxMediaStreamOutStringBase(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxMediaStreamIn(class wxMediaStreamIn *);
extern Scheme_Object *objscheme_bundle_wxMediaStreamIn(class wxMediaStreamIn *);
extern class wxMediaStreamInBase *objscheme_unbundle_wxMediaStreamInBase(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMediaStreamIn(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamIn(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamIn(class wxMediaStreamIn *realobj);
class wxMediaStreamIn *objscheme_unbundle_wxMediaStreamIn(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxMediaStreamOut(class wxMediaStreamOut *);
extern Scheme_Object *objscheme_bundle_wxMediaStreamOut(class wxMediaStreamOut *);
extern class wxMediaStreamOutBase *objscheme_unbundle_wxMediaStreamOutBase(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMediaStreamOut(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMediaStreamOut(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMediaStreamOut(class wxMediaStreamOut *realobj);
class wxMediaStreamOut *objscheme_unbundle_wxMediaStreamOut(Scheme_Object *obj, const char *where, int nullOK);
#endif
