#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxTimer(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxTimer(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxTimer(class wxTimer *realobj);
class wxTimer *objscheme_unbundle_wxTimer(Scheme_Object *obj, const char *where, int nullOK);
extern class wxClipboardClient *objscheme_unbundle_wxClipboardClient(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxClipboardClient *objscheme_unbundle_wxClipboardClient(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxClipboard(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxClipboard(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxClipboard(class wxClipboard *realobj);
class wxClipboard *objscheme_unbundle_wxClipboard(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxClipboard(class wxClipboard *);
extern Scheme_Object *objscheme_bundle_wxClipboard(class wxClipboard *);
#endif
void objscheme_setup_wxClipboardGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxClipboardClient(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxClipboardClient(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxClipboardClient(class wxClipboardClient *realobj);
class wxClipboardClient *objscheme_unbundle_wxClipboardClient(Scheme_Object *obj, const char *where, int nullOK);
extern class wxPrintSetupData *objscheme_unbundle_wxPrintSetupData(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPrintSetupData(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPrintSetupData(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPrintSetupData(class wxPrintSetupData *realobj);
class wxPrintSetupData *objscheme_unbundle_wxPrintSetupData(Scheme_Object *obj, const char *where, int nullOK);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPrintSetupGlobal(Scheme_Env *env);
#ifndef WXS_SETUP_ONLY
#endif
