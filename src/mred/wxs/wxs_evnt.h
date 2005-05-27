#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxEvent(class wxEvent *realobj);
class wxEvent *objscheme_unbundle_wxEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxCommandEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxCommandEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxCommandEvent(class wxCommandEvent *realobj);
class wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxPopupEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPopupEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPopupEvent(class wxPopupEvent *realobj);
class wxPopupEvent *objscheme_unbundle_wxPopupEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxScrollEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxScrollEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxScrollEvent(class wxScrollEvent *realobj);
class wxScrollEvent *objscheme_unbundle_wxScrollEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxKeyEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxKeyEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *realobj);
class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxMouseEvent(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMouseEvent(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *realobj);
class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *obj, const char *where, int nullOK);
#endif
