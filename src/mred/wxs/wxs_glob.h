#ifndef WXS_SETUP_ONLY
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxsGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
