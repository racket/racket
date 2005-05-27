
#ifndef MRED_EXTERN
# define MRED_EXTERN extern
#endif

extern int mred_eventspace_param;
extern int mred_event_dispatch_param;
extern int mred_ps_setup_param;

extern Scheme_Type mred_eventspace_type;

extern Scheme_Object *MrEdGetFrameList(void);
extern int MrEdCheckForBreak(void);
extern Scheme_Object *MrEdEventspaceConfig(Scheme_Object *);
extern Scheme_Object *MrEdEventspaceThread(Scheme_Object *e);

extern Scheme_Object *MrEdMakeEventspace();
extern int wxsIsContextShutdown(void *cx);

extern Scheme_Object *wxsBundlePSSetup(wxPrintSetupData *d);
extern wxPrintSetupData *wxsUnbundlePSSetup(Scheme_Object *s);

extern void MrEd_add_q_callback(char *who, int argc, Scheme_Object **argv);
void MrEdQueueInEventspace(void *context, Scheme_Object *thunk);
extern Scheme_Object *MrEd_mid_queue_key;

extern Scheme_Object *wxs_app_file_proc;
extern Scheme_Object *wxs_app_quit_proc;
extern Scheme_Object *wxs_app_about_proc;
extern Scheme_Object *wxs_app_pref_proc;

extern void *wxSchemeYield(void *sema);

extern wxWindow *wxLocationToWindow(int x, int y);

MRED_EXTERN void mred_wait_eventspace(void);

MRED_EXTERN Scheme_Object *wxSchemeFindDirectory(int argc, Scheme_Object **argv);

extern int wxGetPreference(const char *name, int *res);

extern int wxIsUserMainEventspace(Scheme_Object *o);

void wxscheme_early_gl_init(void);

#ifdef MPW_CPLUS
# define CAST_SP (Scheme_Prim *)
#else
# define CAST_SP 
#endif
