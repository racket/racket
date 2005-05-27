#ifndef WXS_SETUP_ONLY
extern class wxMenuBar *objscheme_unbundle_wxMenuBar(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
extern int objscheme_istype_wxMenu(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMenu(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMenu(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMenu(class wxMenu *realobj);
class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *obj, const char *where, int nullOK);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMenuBar(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMenuBar(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMenuBar(class wxMenuBar *realobj);
class wxMenuBar *objscheme_unbundle_wxMenuBar(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxsMenuItem(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxsMenuItem(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxsMenuItem(class wxsMenuItem *realobj);
class wxsMenuItem *objscheme_unbundle_wxsMenuItem(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxsMenuItem(class wxsMenuItem *);
#endif
void objscheme_setup_wxsMenuItemGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
