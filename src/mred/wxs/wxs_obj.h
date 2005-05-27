#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxObject(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxObject(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxObject(class wxObject *realobj);
class wxObject *objscheme_unbundle_wxObject(Scheme_Object *obj, const char *where, int nullOK);
#endif
