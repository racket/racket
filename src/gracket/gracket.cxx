/*
 * File:        mred.cc
 * Purpose:     GRacket main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1995-2000, Matthew Flatt
 */

/* wx_xt: */
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif

/* wx_motif, for wxTimer: */
#ifdef __GNUG__
# pragma implementation "wx_timer.h"
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif

#include "common.h"

#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include "wx_messg.h"
#include "wx_timer.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_menu.h"
#include "wx_dcps.h"
#include "wx_clipb.h"
#include "wx_types.h"
#ifdef wx_mac
# include "simpledrop.h"
#endif
#ifdef wx_msw
# include "wx_wmgr.h"
#endif
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

/* Solaris: getdtablesize sometimes not available */
#if !defined(USE_ULIMIT) && defined(sun) && defined(__svr4__)
# define USE_ULIMIT
#endif

#if defined(wx_xt)
# include <X11/Xlib.h>
# include <X11/keysymdef.h>
#endif

#ifdef wx_x
# include <sys/types.h>
# include <sys/time.h>
# include <unistd.h>
#  if defined(_IBMR2)
#   include <sys/select.h>
#  endif
# include <signal.h>
#endif

#ifdef wx_msw
# ifdef _MSC_VER
#  include <direct.h>
# else
#  include <dir.h>
# endif
#endif

#ifdef wx_mac
# ifndef WX_CARBON
#  include <Events.h>
# endif
# ifdef OS_X
int wx_in_terminal;
# else
#  define wx_in_terminal 0
# endif
#else
# ifdef wx_msw
static int wx_in_terminal = 0;
# else
#  define wx_in_terminal 0
# endif
#endif

#ifdef OS_X
extern "C" void _signal_nobind(...);
#endif

#if defined(wx_x) || defined(wx_msw)
# define ADD_OBJ_DUMP 0
#else
# define ADD_OBJ_DUMP 0
#endif

#define INTERRUPT_CHECK_ON 0

# include "wxs/wxscheme.h"
# include "wxs/wxsgracket.h"
# include "wxs/wxs_fram.h"
# include "wxs/wxs_obj.h"

wxFrame *mred_real_main_frame;

#if defined(wx_xt) || defined(OS_X)
# define mred_BREAK_HANDLER
#endif

static Scheme_Thread *user_main_thread;

extern void wxMouseEventHandled(void);
#ifdef wx_xt
extern int wx_single_instance;
#endif

#include "gracket.h"

#ifdef MPW_CPLUS
extern "C" {
  typedef void (*GC_F_PTR)(void *, void *);
  typedef void (*ON_KILL_PTR)(struct Scheme_Thread *p);
  typedef Scheme_Object *(*MK_PTR)(void);
# if !defined(USE_SENORA_GC) && !defined(MZ_PRECISE_GC)
  typedef void (*IGNORE_PTR)(char *, GC_word);
# endif
  typedef void (*CONSOLE_PRINTF_PTR)(char *str, ...);
  typedef void (*CONSOLE_OUTPUT_PTR)(char *str, long len);
  typedef void (*EXIT_PTR)(int);
  typedef void (*DW_PRE_PTR)(void *);
  typedef Scheme_Object *(*DW_RUN_PTR)(void *);
  typedef void (*DW_POST_PTR)(void *);
  typedef void (*ON_SUSPEND_PTR)(void);
}
# define CAST_SCP (Scheme_Closed_Prim *)
# define CAST_GCP (GC_F_PTR)
# define CAST_SCCC (Scheme_Close_Custodian_Client *)
# define CAST_BLKCHK (Scheme_Ready_Fun)
# define CAST_WU (Scheme_Needs_Wakeup_Fun)
# define CAST_TOK (ON_KILL_PTR)
# define CAST_GS (Scheme_Get_String_Fun)
# define CAST_IREADY (Scheme_In_Ready_Fun)
# define CAST_ICLOSE (Scheme_Close_Input_Fun)
# define CAST_WS (Scheme_Write_String_Fun)
# define CAST_MK (MK_PTR)
# define CAST_SLEEP (SLEEP_PROC_PTR)
# define CAST_IGNORE (IGNORE_PTR)
# define CAST_PRINTF (CONSOLE_PRINTF_PTR)
# define CAST_OUTPUT (CONSOLE_OUTPUT_PTR)
# define CAST_EXIT (EXIT_PTR)
# define CAST_DW_PRE (DW_PRE_PTR)
# define CAST_DW_RUN (DW_RUN_PTR)
# define CAST_DW_POST (DW_POST_PTR)
# define CAST_SUSPEND (ON_SUSPEND_PTR)
# define CAST_EXT (Scheme_Custodian_Extractor)
#else
# define CAST_SCP /* empty */
# define CAST_GCP /* empty */
# define CAST_SCCC  /* empty */
# define CAST_BLKCHK /* empty */
# define CAST_WU /* empty */
# define CAST_TOK /* empty */
# define CAST_GS /* empty */
# define CAST_IREADY /* empty */
# define CAST_ICLOSE /* empty */
# define CAST_WS /* empty */
# define CAST_MK /* empty */
# define CAST_SLEEP /* empty */
# define CAST_IGNORE /* empty */
# define CAST_PRINTF /* empty */
# define CAST_OUTPUT /* empty */
# define CAST_EXIT /* empty */
# define CAST_DW_PRE /* empty */
# define CAST_DW_RUN /* empty */
# define CAST_DW_POST /* empty */
# define CAST_SUSPEND /* empty */
# define CAST_EXT /* empty */
#endif

/* Set by mrmain.cxx: */
/* (The indirection is needed to avoid mutual .dll dependencies.) */
MrEd_Finish_Cmd_Line_Run_Proc mred_finish_cmd_line_run;
void mred_set_finish_cmd_line_run(MrEd_Finish_Cmd_Line_Run_Proc p) { mred_finish_cmd_line_run = p; }
MrEd_Run_From_Cmd_Line_Proc mred_run_from_cmd_line;
void mred_set_run_from_cmd_line(MrEd_Run_From_Cmd_Line_Proc p) { mred_run_from_cmd_line = p; }

#if 0
/* Force initialization of the garbage collector (currently needed
   only when supporting Irix sprocs) */
class GCInit {
public:
  GCInit() {
    GC_INIT();
  }
};
static GCInit _gcinit;
#endif

static Scheme_Env *global_env;

class MrEdApp: public wxApp
{
public:
  Bool initialized;
  int xargc;
  char **xargv;

  MrEdApp();
  wxFrame *OnInit(void);
  void RealInit(void);
#ifdef wx_mac
  char *GetDefaultAboutItemName();
  void DoDefaultAboutItem();
#endif
  int OnExit(void);
};

MrEdApp *TheMrEdApp;

static int exit_val = 0;

#ifdef LIBGPP_REGEX_HACK
/* Fixes weirdness with libg++ and the compiler: it tries to
   destroy global regexp objects that were never created. Calling
   the constructor forces the other global values to be initialized. */
# include <Regex.h>
#endif

/****************************************************************************/
/*                               Contexts                                   */
/****************************************************************************/

MrEdContext *mred_contexts;
static MrEdContext *mred_main_context;
static MrEdContext *mred_only_context;
static int only_context_just_once = 0;
static MrEdContext *user_main_context;
static MrEdContextFramesRef mred_frames; /* list of all frames (weak link to invisible ones) */
static Scheme_Hash_Table *timer_contexts;
int mred_eventspace_param;
int mred_event_dispatch_param;
Scheme_Type mred_eventspace_type;
Scheme_Type mred_nested_wait_type;
static Scheme_Type mred_eventspace_hop_type;
static Scheme_Object *def_dispatch;
int mred_ps_setup_param;
#ifdef NEED_HET_PARAM
Scheme_Object *mred_het_key;
#endif

typedef struct Nested_Wait {
  Scheme_Object so;
  Scheme_Object *wait_on;
} Nested_Wait;

typedef struct Context_Custodian_Hop {
  Scheme_Object so;
  MrEdContext *context;
} Context_Custodian_Hop;

#ifdef MZ_PRECISE_GC
# define WEAKIFY(x) ((MrEdContext *)GC_malloc_weak_box(x, NULL, 0))
# define WEAKIFIED(x) ((MrEdContext *)GC_weak_box_val(x))
#else
# define WEAKIFY(x) x
# define WEAKIFIED(x) x
# define HIDE_FROM_XFORM(x) x
#endif

static MrEdContext *check_q_callbacks(int hi, int (*test)(MrEdContext *, MrEdContext *),
				      MrEdContext *tdata, int check_only);
static void remove_q_callbacks(MrEdContext *c);

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

static int size_eventspace_val(void *)
{
  return gcBYTES_TO_WORDS(sizeof(MrEdContext));
}

static int mark_eventspace_val(void *p)
{
  MrEdContext *c = (MrEdContext *)p;

  gcMARK_TYPED(Scheme_Thread *, c->handler_running);
  gcMARK_TYPED(MrEdFinalizedContext *, c->finalized);

  gcMARK_TYPED(wxChildList *, c->topLevelWindowList);
  gcMARK_TYPED(wxWindow *, c->modal_window);
  gcMARK_TYPED(MrEd_Saved_Modal *, c->modal_stack);

  gcMARK_TYPED(Scheme_Config *, c->main_config);
  gcMARK_TYPED(Scheme_Thread_Cell_Table *, c->main_cells);
  gcMARK_TYPED(Scheme_Thread_Cell_Table *, c->main_break_cell);

  gcMARK_TYPED(wxTimer *, c->timer);
  gcMARK_TYPED(wxTimer **, c->timers);

  gcMARK_TYPED(void *, c->alt_data);

  gcMARK_TYPED(MrEdContext *, c->next);

#ifdef wx_msw
  gcMARK_TYPED(LeaveEvent *, c->queued_leaves);
#endif

  gcMARK_TYPED(Context_Custodian_Hop *, c->mr_hop);
  gcMARK_TYPED(Scheme_Custodian_Reference *, c->mref);

  return gcBYTES_TO_WORDS(sizeof(MrEdContext));
}

static int fixup_eventspace_val(void *p)
{
  MrEdContext *c = (MrEdContext *)p;

  gcFIXUP_TYPED(Scheme_Thread *, c->handler_running);
  gcFIXUP_TYPED(MrEdFinalizedContext *, c->finalized);

  gcFIXUP_TYPED(wxChildList *, c->topLevelWindowList);
  gcFIXUP_TYPED(wxWindow *, c->modal_window);
  gcFIXUP_TYPED(MrEd_Saved_Modal *, c->modal_stack);

  gcFIXUP_TYPED(Scheme_Config *, c->main_config);
  gcFIXUP_TYPED(Scheme_Thread_Cell_Table *, c->main_cells);
  gcFIXUP_TYPED(Scheme_Thread_Cell_Table *, c->main_break_cell);

  gcFIXUP_TYPED(wxTimer *, c->timer);
  gcFIXUP_TYPED(wxTimer **, c->timers);

  gcFIXUP_TYPED(void *, c->alt_data);

  gcFIXUP_TYPED(MrEdContext *, c->next);

#ifdef wx_msw
  gcFIXUP_TYPED(LeaveEvent *, c->queued_leaves);
#endif

  gcFIXUP_TYPED(Context_Custodian_Hop *, c->mr_hop);
  gcFIXUP_TYPED(Scheme_Custodian_Reference *, c->mref);

  return gcBYTES_TO_WORDS(sizeof(MrEdContext));
}

static int size_nested_wait_val(void *)
{
  return gcBYTES_TO_WORDS(sizeof(Nested_Wait));
}

static int mark_nested_wait_val(void *p)
{
  Nested_Wait *c = (Nested_Wait *)p;

  gcMARK_TYPED(MrEdContext *, c->wait_on);

  return gcBYTES_TO_WORDS(sizeof(Nested_Wait));
}

static int fixup_nested_wait_val(void *p)
{
  Nested_Wait *c = (Nested_Wait *)p;

  gcFIXUP_TYPED(MrEdContext *, c->wait_on);

  return gcBYTES_TO_WORDS(sizeof(Nested_Wait));
}

static int size_eventspace_hop_val(void *)
{
  return gcBYTES_TO_WORDS(sizeof(Context_Custodian_Hop));
}

static int mark_eventspace_hop_val(void *p)
{
  Context_Custodian_Hop *c = (Context_Custodian_Hop *)p;

  gcMARK_TYPED(MrEdContext *, c->context);

  return gcBYTES_TO_WORDS(sizeof(Context_Custodian_Hop));
}

static int fixup_eventspace_hop_val(void *p)
{
  Context_Custodian_Hop *c = (Context_Custodian_Hop *)p;

  gcFIXUP_TYPED(MrEdContext *, c->context);

  return gcBYTES_TO_WORDS(sizeof(Context_Custodian_Hop));
}

END_XFORM_SKIP;

#endif

MrEdContext *MrEdGetContext(wxObject *w)
{
  if (w) {
#if !defined(wx_xt) && !defined(wx_mac)
    if (wxSubType(w->__type, wxTYPE_FRAME)) {
#endif
      MrEdContext *c;
      c = (MrEdContext *)((wxFrame *)w)->context;
      if (c)
	return c;
#if !defined(wx_xt) && !defined(wx_mac)
    } else {
      MrEdContext *c;
      c = (MrEdContext *)((wxDialogBox *)w)->context;
      if (c)
	return c;
    }
#endif
  }

  if (mred_only_context) {
    if (only_context_just_once) {
      MrEdContext *c = mred_only_context;
      mred_only_context = NULL;
      only_context_just_once = 0;
      return c;
    } else
      return mred_only_context;
  } else
    return (MrEdContext *)scheme_get_param(scheme_current_config(), mred_eventspace_param);
}

void *MrEdGetWindowContext(wxWindow *w)
{
  while (1) {
    if (wxSubType(w->__type, wxTYPE_FRAME))
      return MrEdGetContext(w);
#if !defined(wx_xt) && !defined(wx_mac)
    if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
      return MrEdGetContext(w);
#endif

    w = w->GetParent();
  }
}

void *wxGetContextForFrame()
{
  if (!TheMrEdApp)
    return NULL;
  else
    return (void *)MrEdGetContext();
}

wxChildList *wxGetTopLevelWindowsList(wxObject *w)
{
  MrEdContext *c;
  c = MrEdGetContext(w);

  return c->topLevelWindowList;
}

wxWindow *wxGetModalWindow(wxObject *w)
{
  MrEdContext *c;
  c = MrEdGetContext(w);

  return c->modal_window;
}

class MrEd_Saved_Modal {
public:
  wxWindow *win;
  MrEd_Saved_Modal *next;
};

void wxPushModalWindow(wxObject *w, wxWindow *win)
{
  MrEdContext *c;
  c = MrEdGetContext(w);

  if (c->modal_window) {
    MrEd_Saved_Modal *save;
    save = new WXGC_PTRS MrEd_Saved_Modal;

    save->next = c->modal_stack;
    save->win = c->modal_window;
    c->modal_stack = save;
  }

  c->modal_window = win;
}

void wxPopModalWindow(wxObject *w, wxWindow *win)
{
  MrEdContext *c;
  MrEd_Saved_Modal *save, *prev;
  c = MrEdGetContext(w);

  if (c->modal_window == win)
    c->modal_window = NULL;

  prev = NULL;
  for (save = c->modal_stack; save; save = save->next) {
    if ((save->win == win) || !c->modal_window) {
      if (prev)
	prev->next = save->next;
      else
	c->modal_stack = save->next;

      if (save->win != win)
	c->modal_window = save->win;
    } else
      prev = save;
  }
}

int wxGetBusyState(void)
{
  MrEdContext *c;
  c = MrEdGetContext();

  return c->busyState;
}

void wxSetBusyState(int state)
{
  MrEdContext *c;
  c = MrEdGetContext();

  c->busyState = state;
}

extern int MrEdGetDoubleTime(void);
static int doubleClickThreshold = -1;

int wxMrEdGetDoubleTime(void)
{
  if (doubleClickThreshold < 0) {
    if (!wxGetPreference("doubleClickTime", &doubleClickThreshold)) {
      doubleClickThreshold = MrEdGetDoubleTime();
    }
  }

  return doubleClickThreshold;
}

#ifdef wx_xt
/* For widgets: */
extern "C" {
  int wxGetMultiClickTime(Display *d)
  {
    return wxMrEdGetDoubleTime();
  }
}
#endif

Bool wxIsPrimEventspace()
{
  return MrEdGetContext() == mred_main_context;
}

int wxIsUserMainEventspace(Scheme_Object *o)
{
  return o == (Scheme_Object *)user_main_context;
}

int wxsIsContextShutdown(void *cx)
{
  MrEdContext *c;
  c = (MrEdContext *)cx;

  return c->killed;
}

void *wxsCheckEventspace(char *who)
{
  MrEdContext *c;
  c = (MrEdContext *)wxGetContextForFrame();

  if (c->killed)
    scheme_signal_error("%s: the current eventspace has been shutdown", who);

  return (void *)c;
}

static int ps_ready = 0;
static wxPrintSetupData *orig_ps_setup;

wxPrintSetupData *wxGetThePrintSetupData()
{
  if (ps_ready) {
    Scheme_Object *o;
    o = scheme_get_param(scheme_current_config(), mred_ps_setup_param);
    if (o && SCHEME_TRUEP(o))
      return wxsUnbundlePSSetup(o);
  }
  return orig_ps_setup;
}

void wxSetThePrintSetupData(wxPrintSetupData *d)
{
  if (ps_ready) {
    Scheme_Object *o;
    o = wxsBundlePSSetup(d);
    scheme_set_param(scheme_current_config(), mred_ps_setup_param, o);
  }
  orig_ps_setup = d;
}


/* Forward decl: */
static int MrEdSameContext(MrEdContext *c, MrEdContext *testc);

static void destroy_wxObject(wxWindow *w, void *)
{
  if (w->__gc_external) {
    objscheme_destroy(w, (Scheme_Object *)w->__gc_external);
    ((Scheme_Class_Object *)w->__gc_external)->primflag = -2; /* -2 => shutdown */
    w->__gc_external = NULL;
  }
}

static void kill_eventspace(Scheme_Object *ec, void *)
{
  MrEdContext *c;
  c = WEAKIFIED(((Context_Custodian_Hop *)ec)->context);

  if (!c)
    return; /* must not have had any frames, timers, etc. */

  {
    wxClipboardClient *clipOwner;
    clipOwner = wxTheClipboard->GetClipboardClient();
    if (clipOwner && (clipOwner->context == c))
      wxTheClipboard->SetClipboardString("", 0);
  }

  c->killed = 1;

  {
    wxChildNode *node, *next;
    for (node = c->topLevelWindowList->First(); node; node = next) {
      wxWindow *w;
      w = (wxWindow *)node->Data();
      next = node->Next();
      if (w) {
	w->ForEach(destroy_wxObject, NULL);
	if (node->IsShown())
	  w->Show(FALSE);
      }
    }
  }

  {
    wxTimer *t;
    while (c->timers) {
      t = c->timers;
      t->Stop();
    }
  }

  remove_q_callbacks(c);
}

static Scheme_Object *extract_eventspace_from_hop(Scheme_Object *ec)
{
  return (Scheme_Object *)WEAKIFIED(((Context_Custodian_Hop *)ec)->context);
}

static void CollectingContext(void *cfx, void *)
{
  wxChildNode *cnode, *next;
  MrEdFinalizedContext *cf;
  cf = (MrEdFinalizedContext *)gcPTR_TO_OBJ(cfx);

  if (cf->frames->next)
    FRAMES_REF(cf->frames->next)->prev = cf->frames->prev;
  if (cf->frames->prev)
    FRAMES_REF(cf->frames->prev)->next = cf->frames->next;
  else
    mred_frames = cf->frames->next;

  cf->frames->next = NULL;
  cf->frames->prev = NULL;

  /* Must explicitly delete frames now because their context
     is going away. (The frame would certainly have been finalized
     later during this set of finalizations, but that would be
     too late.) */
  for (cnode = cf->frames->list->First(); cnode; cnode = next) {
    wxFrame *fr;
    next = cnode->Next();
    fr = (wxFrame *)cnode->Data();
    if (fr) {
      DELETE_OBJ fr;
    }
  }

  MrEdDestroyContext(cf);

  DELETE_OBJ cf->frames->list;
  cf->frames = NULL;
}

static MrEdContext *MakeContext(MrEdContext *c)
{
  MrEdContextFrames *frames;
  Context_Custodian_Hop *mr_hop;
  Scheme_Object *break_cell;
  Scheme_Config *config;
  Scheme_Thread_Cell_Table *cells;

  scheme_custodian_check_available(NULL, "make-eventspace", "eventspace");

  if (!c) {
    wxChildList *tlwl;
    MrEdFinalizedContext *fc;

    c = (MrEdContext *)scheme_malloc_tagged(sizeof(MrEdContext));
    c->so.type = mred_eventspace_type;

    tlwl = new WXGC_PTRS wxChildList();
    c->topLevelWindowList = tlwl;
    fc = new WXGC_PTRS MrEdFinalizedContext;
    c->finalized = fc;
  }

  c->ready = 1;

  c->handler_running = NULL;

  c->busyState = 0;
  c->killed = 0;

  frames = new WXGC_PTRS MrEdContextFrames;
  c->finalized->frames = frames;
  frames->next = mred_frames;
  frames->prev = NULL;
  frames->list = c->topLevelWindowList;
  {
    MrEdContextFramesRef r;
    r = MAKE_FRAMES_REF(frames);
    if (mred_frames)
      FRAMES_REF(mred_frames)->prev = r;
    mred_frames = r;
  }

  c->modal_window = NULL;

  config = scheme_extend_config(scheme_current_config(), 
				mred_eventspace_param, 
				(Scheme_Object *)c);

  c->main_config = config;
  cells = scheme_inherit_cells(NULL);
  c->main_cells = cells;
  break_cell = scheme_current_break_cell();
  c->main_break_cell = break_cell;

#ifdef MZ_PRECISE_GC
  /* Override destructor-based finalizer: */
  GC_set_finalizer(gcOBJ_TO_PTR(c->finalized),
		   0, 3,
		   CollectingContext, NULL,
		   NULL, NULL);
#else
  scheme_register_finalizer(gcOBJ_TO_PTR(c->finalized),
			    CAST_GCP CollectingContext, NULL,
			    NULL, NULL);
#endif
  WXGC_IGNORE(c, c->finalized);

#ifdef MZ_PRECISE_GC
  mr_hop = (Context_Custodian_Hop *)GC_malloc_one_tagged(sizeof(Context_Custodian_Hop));
#else
  mr_hop = (Context_Custodian_Hop *)scheme_malloc_atomic(sizeof(Context_Custodian_Hop));
#endif
  mr_hop->so.type = mred_eventspace_hop_type;
  {
    MrEdContext *ctx;
    ctx = WEAKIFY(c);
    mr_hop->context = ctx;
  }
  c->mr_hop = mr_hop;
#ifndef MZ_PRECISE_GC
  scheme_weak_reference((void **)(void *)&mr_hop->context);
#endif

  {
    Scheme_Custodian_Reference *mr;
    mr = scheme_add_managed(NULL, (Scheme_Object *)mr_hop,
			    CAST_SCCC kill_eventspace,
			    NULL, 0);
    c->mref = mr;
  }

  return c;
}

static void ChainContextsList()
{
  MrEdContextFrames *f;
  MrEdContextFramesRef fr = mred_frames;
  wxChildNode *first;

  mred_contexts = NULL;

  while (fr) {
    f = FRAMES_REF(fr);
    first = f->list->First();

#if 0
    while (first && !first->IsShown())
      first = first->Next();
#endif

    if (first) {
      wxObject *o;
      MrEdContext *c;
      o = first->Data();
      c = MrEdGetContext(o);
      c->next = mred_contexts;
      mred_contexts = c;
    }
    fr = f->next;
  }
}

static void UnchainContextsList()
{
  while (mred_contexts) {
    MrEdContext *next = mred_contexts->next;
    mred_contexts->next = NULL;
    mred_contexts = next;
  }
}

static wxTimer *GlobalFirstTimer()
{
  wxTimer *timer = NULL;
  int i;
  for (i = timer_contexts->size; i--; ) {
    if (timer_contexts->vals[i]) {
      MrEdContext *c = (MrEdContext *)timer_contexts->keys[i];
      if (c->ready && c->timers) {
        if (!timer)
          timer = c->timers;
        else if (c->timers->expiration < timer->expiration)
          timer = c->timers;
      }
    }
  }
  return timer;
}

#ifdef wx_xt
void wxUnhideAllCursors()
{
  MrEdContextFrames *f;
  MrEdContextFramesRef fr = mred_frames;
  wxChildNode *first;
  int v;

  if (wxCheckHiddenCursors()) {
    while (fr) {
      f = FRAMES_REF(fr);
      first = f->list->First();
      
      if (first) {
	wxObject *o;
	MrEdContext *c;
	o = first->Data();
	c = MrEdGetContext(o);
	v = wxUnhideCursorInFrame(o, c->busyState);
	c->busyState = v;
      }
      fr = f->next;
    }
  }
}
#endif

Scheme_Object *MrEdMakeEventspace()
{
  MrEdContext *c;

  c = MakeContext(NULL);

  MrEdInitNewContext(c);

  return (Scheme_Object *)c;
}

Scheme_Object *MrEdEventspaceThread(Scheme_Object *e)
{
  return (Scheme_Object *)((MrEdContext *)e)->handler_running;
}

Scheme_Object *MrEdGetFrameList(void)
{
  MrEdContext *c;
  Scheme_Object *l = scheme_null;
  c = MrEdGetContext();

  if (c) {
    wxChildNode *node;
    for (node = c->topLevelWindowList->First(); node; node = node->Next()) {
      wxObject *o;
      o = node->Data();
      if (node->IsShown()) {
#ifdef wx_mac
	/* Mac: some frames really represent dialogs. Any modal frame is
	   a dialog, so extract its only child. */
	if (((wxFrame *)o)->IsModal()) {
	  wxChildNode *node2;
	  wxChildList *cl;
	  cl = ((wxFrame *)o)->GetChildren();
	  node2 = cl->First();
	  if (node2)
	    o = node2->Data();
	}
#endif
	l = scheme_make_pair(objscheme_bundle_wxObject(o), l);
      }
    }
  }

  return l;
}

void *MrEdForEachFrame(ForEachFrameProc fp, void *data)
{
  MrEdContextFrames *f;
  MrEdContextFramesRef fr = mred_frames;
  wxChildNode *node;

  while (fr) {
    f = FRAMES_REF(fr);
    node = f->list->First();

    while (node) {
      if (node->IsShown()) {
	wxObject *o;
	o = node->Data();
#ifdef wx_mac
	/* Mac: some frames really represent dialogs. Any modal frame is
	   a dialog, so extract its only child. */
	if (((wxFrame *)o)->IsModal()) {
	  wxChildNode *node2;
	  wxChildList *cl;
	  cl = ((wxFrame *)o)->GetChildren();
	  node2 = cl->First();
	  if (node2)
	    o = node2->Data();
	}
#endif
	data = fp(o, data);
      }
      node = node->Next();
    }

    fr = f->next;
  }

  return data;
}

static int check_eventspace_inactive(void *_c)
{
  MrEdContext *c = (MrEdContext *)_c;

  if (c->nested_avail)
    return 0;

  /* Any callbacks prepared for this eventspace? */
  if (check_q_callbacks(0, MrEdSameContext, c, 1)
      || check_q_callbacks(1, MrEdSameContext, c, 1)
      || check_q_callbacks(2, MrEdSameContext, c, 1))
    return 0;

  /* Any running timers for the eventspace? */
  if (c->timers)
    return 0;

  /* Any top-level windows visible in this eventspace */
  {
    MrEdContextFrames *f = c->finalized->frames;
    wxChildNode *node;

    node = f->list->First();

    while (node) {
      if (node->IsShown()) {
	return 0;
      }
      node = node->Next();
    }
  }

  return 1;
}

void mred_wait_eventspace(void)
{
  MrEdContext *c;
  Scheme_Thread *thread;
  c = MrEdGetContext();
  thread = scheme_get_current_thread();
  if (c && (c->handler_running == thread)) {
    wxDispatchEventsUntilWaitable(check_eventspace_inactive, c, NULL);
  }
}

int mred_current_thread_is_handler(void *ctx)
{
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!ctx)
    ctx = MrEdGetContext();

  return (((MrEdContext *)ctx)->handler_running == thread);
}

int mred_in_restricted_context()
{
#ifdef NEED_HET_PARAM
  /* see wxHiEventTrampoline for info on mred_het_key: */
  Scheme_Object *v;
  if (!scheme_get_current_thread()) 
    return 1;
  
  if (mred_het_key)
    v = scheme_extract_one_cc_mark(NULL, mred_het_key);
  else
    v = NULL;

  if (v && SCHEME_BOX_VAL(v))
    return 1;
#endif
  return 0;
}

/****************************************************************************/
/*                               Events                                     */
/****************************************************************************/

static wxTimer *TimerReady(MrEdContext *c)
{
  wxTimer *timer;

  if (c) {
    timer = c->timers;
  } else {
    timer = GlobalFirstTimer();
  }

  if (timer) {
    double now;
    double goal = timer->expiration;

    now = scheme_get_inexact_milliseconds();

    return ((now >= goal)
	    ? timer
	    : (wxTimer *)NULL);
  } else
    return NULL;
}

static void DoTimer(wxTimer *timer)
{
  int once;
  mz_jmp_buf *save, newbuf;
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (timer->interval == -1)
    return;

  once = timer->one_shot;
  timer->one_shot = -1;

  save = thread->error_buf;
  thread->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf))
    timer->Notify();
  scheme_clear_escape();
  thread = scheme_get_current_thread();
  thread->error_buf = save;
  thread = NULL;

  if (!once && (timer->one_shot == -1) && (timer->interval != -1)
      && !((MrEdContext *)timer->context)->killed)
    timer->Start(timer->interval, FALSE);
}

static int do_check_for_nested_event(Scheme_Object *cx)
{
  MrEdContext *c = (MrEdContext *)cx;

  if (!c->waiting_for_nested)
    return 1;

  if (c->alternate) {
    if (c->alternate(c->alt_data))
      return 1;

    return 0;
  } else
    return 0;
}

static int check_for_nested_event(Scheme_Object *cx)
{
  return do_check_for_nested_event(((Nested_Wait *)cx)->wait_on);
}

static int MrEdSameContext(MrEdContext *c, MrEdContext *testc)
{
  return (c == testc);
}

static void GoAhead(MrEdContext *c)
{
  c->ready_to_go = 0;

  if (c->q_callback) {
    int hi = (c->q_callback - 1);
    c->q_callback = 0;
    (void)check_q_callbacks(hi, MrEdSameContext, c, 0);
  } else if (c->timer) {
    wxTimer *timer;
    timer = c->timer;
    c->timer = NULL;
    DoTimer(timer);
  } else {
    GC_CAN_IGNORE MrEdEvent e;
    mz_jmp_buf *save, newbuf;
    Scheme_Thread *thread;
    thread = scheme_get_current_thread();

    memcpy(&e, &c->event, sizeof(MrEdEvent));

    save = thread->error_buf;
    thread->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf))
      MrEdDispatchEvent(&e);
    scheme_clear_escape();
    thread = scheme_get_current_thread();
    thread->error_buf = save;
    thread = NULL;
  }
}

static Scheme_Object *def_event_dispatch_handler(int argc, Scheme_Object *argv[])
{
  MrEdContext *c;

  c = (MrEdContext *)argv[0];
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), mred_eventspace_type)
      || !c->ready_to_go) {
    scheme_wrong_type("default-event-dispatch-handler",
		      "eventspace (with ready event)",
		      0, argc, argv);
    return NULL;
  }

  GoAhead(c);

  return scheme_void;
}

static void DoTheEvent(MrEdContext *c)
{
  Scheme_Object *p;

  c->ready_to_go = 1;

  p = scheme_get_param(scheme_current_config(), mred_event_dispatch_param);
  if (p != def_dispatch) {
    Scheme_Object *a[1];
    mz_jmp_buf *save, newbuf;
    Scheme_Thread *thread;
    thread = scheme_get_current_thread();

    a[0] = (Scheme_Object *)c;

    save = thread->error_buf;
    thread->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf))
      scheme_apply_multi(p, 1, a);
    scheme_clear_escape();
    thread = scheme_get_current_thread();
    thread->error_buf = save;
    thread = NULL;

#if 0
    if (c->ready_to_go)
      printf("Bad dispatcher\n");
#endif
  }

  if (c->ready_to_go)
    GoAhead(c);
}

static void reset_nested_wait(MrEdContext *c)
{
  c->ready = 0;
  c->waiting_for_nested = 0;
  c->alternate = NULL;
  c->alt_data = NULL;
}

static Scheme_Object *MrEdDoNextEvent(MrEdContext *c, wxDispatch_Check_Fun alt, void *altdata, Scheme_Object *alt_wait)
{
  wxTimer *timer;
  GC_CAN_IGNORE MrEdEvent evt;
  int restricted = 0;

#ifdef NEED_HET_PARAM
  /* see wxHiEventTrampoline for info on mred_het_key: */
  if (mred_in_restricted_context())
    restricted = 1;
#endif

  if (alt) {
    if (alt(altdata)) {
      /* Do nothing, since alt fired. */
      return scheme_void;
    }
  }
  if (alt_wait) {
    Scheme_Object *a[2], *r;
    a[0] = scheme_make_integer(0);
    a[1] = alt_wait;
    r = scheme_sync_timeout(2, a);

    if (r) {
      /* Do nothing, since alt fired. */
      return r;
    }
  }

  if (c->nested_avail) {
    c->nested_avail = 0;
    DoTheEvent(c);
  } else if (check_q_callbacks(2, MrEdSameContext, c, 1)) {
    c->q_callback = 3;
    DoTheEvent(c);
  } else if ((timer = TimerReady(c))) {
    timer->Dequeue();
    c->timer = timer;
    DoTheEvent(c);
  } else if (check_q_callbacks(1, MrEdSameContext, c, 1)) {
    c->q_callback = 2;
    DoTheEvent(c);
  } else if (!restricted && MrEdGetNextEvent(0, 1, &evt, NULL)) {
    memcpy(&c->event, &evt, sizeof(MrEdEvent));
    DoTheEvent(c);
#ifdef wx_mac
    /* MrEdGetNextEvent might enqueue */
  } else if (check_q_callbacks(1, MrEdSameContext, c, 1)) {
    c->q_callback = 2;
    DoTheEvent(c);
#endif
  } else if (!restricted && check_q_callbacks(0, MrEdSameContext, c, 1)) {
    c->q_callback = 1;
    DoTheEvent(c);
  } else if (c != mred_main_context) {
    Scheme_Object *result = NULL;

    c->ready = 1;
    c->waiting_for_nested = 1;

    c->alternate = alt;
    c->alt_data = altdata;

    if (alt_wait) {
      Nested_Wait *nw;
      Scheme_Object *a[2], *v = NULL;

      nw = (Nested_Wait *)scheme_malloc_tagged(sizeof(Nested_Wait));
      nw->so.type = mred_nested_wait_type;
      nw->wait_on = (Scheme_Object *)c;

      a[0] = alt_wait;
      a[1] = (Scheme_Object *)nw;

      /* Running arbitrary Scheme code here. */
      BEGIN_ESCAPEABLE(reset_nested_wait, c);
      v = scheme_sync(2, a);
      END_ESCAPEABLE();

      if (!SAME_OBJ(v, a[1]))
	result = v;
    } else {
      scheme_block_until((Scheme_Ready_Fun)do_check_for_nested_event, NULL,
			 (Scheme_Object *)c, 0.0);
    }

    c->alternate = NULL;
    c->alt_data = NULL;

    if (c->waiting_for_nested) {
      /* Alternate condition fired. Clear waiting flag. */
      c->ready = 0;
      c->waiting_for_nested = 0;
      if (!result)
	result = scheme_void;
    }

    return result;
  }

  return NULL;
}

void wxDoNextEvent()
{
  MrEdContext *c;
  Scheme_Thread *thread;
  c = MrEdGetContext();
  thread = scheme_get_current_thread();

  if (!c->ready_to_go)
    if (c->handler_running == thread)
      MrEdDoNextEvent(c, NULL, NULL, NULL);
}

int MrEdEventReady(MrEdContext *c)
{
  int restricted = 0;

#ifdef NEED_HET_PARAM
  /* see wxHiEventTrampoline for info on mred_het_key: */
  if (mred_in_restricted_context())
    restricted = 1;
#endif

  return (c->nested_avail
	  || TimerReady(c)
	  || (!restricted && MrEdGetNextEvent(1, 1, NULL, NULL))
	  || (!restricted && check_q_callbacks(2, MrEdSameContext, c, 1))
	  || check_q_callbacks(1, MrEdSameContext, c, 1)
	  || check_q_callbacks(0, MrEdSameContext, c, 1));
}

int wxEventReady()
{
  MrEdContext *c;
  Scheme_Thread *thread;
  c = MrEdGetContext();
  thread = scheme_get_current_thread();

  return (!c->ready_to_go
	  && (c->handler_running == thread)
	  && MrEdEventReady(c));
}

static void WaitForAnEvent_OrDie(MrEdContext *c)
{
  c->ready = 1;
  c->waiting_for_nested = 1;
  c->alternate = NULL;
  c->alt_data = NULL;

  /* Suspend the thread. If another event is found for the eventspace, the
     thread will be resumed. */
  c->suspended = 1;
  while (1) {
    scheme_weak_suspend_thread(c->handler_running); /* suspend self */

    if (c->waiting_for_nested) {
      /* we were resumed for a break signal, or some such: */
      c->suspended = 0;
      c->ready = 0;
      c->waiting_for_nested = 0;

      scheme_thread_block(0);
      scheme_set_current_thread_ran_some();

      /* Go back to sleep: */
      c->ready = 1;
      c->waiting_for_nested = 1;
      c->suspended = 1;
    } else
      break;
  }

  /* An event has been found. Do it. */
  c->nested_avail = 0;
  DoTheEvent(c);

  /* Return to loop and look for more events... */
}

static void on_handler_killed(Scheme_Thread *p)
{
  MrEdContext *c = (MrEdContext *)p->kill_data;

  p->on_kill = NULL;
  p->kill_data = NULL;

  /* The thread is forever not ready: */
  c->handler_running = NULL;
  c->ready = 0;
  c->waiting_for_nested = 0;
  c->nested_avail = 0;
  c->q_callback = 0;
  c->timer = NULL;
  c->alternate = NULL;
  c->alt_data = NULL;
  c->ready_to_go = 0;
}

static Scheme_Object *handle_events(void *cx, int, Scheme_Object **)
{
  MrEdContext *c = (MrEdContext *)cx;
  Scheme_Thread *this_thread;
  mz_jmp_buf newbuf;

#if SGC_STD_DEBUGGING
  fprintf(stderr, "new thread\n");
#endif

  this_thread = scheme_get_current_thread();
  if (!this_thread->name) {
    Scheme_Object *tn;
    tn = scheme_intern_symbol("handler");
    this_thread->name = tn;
  }
  c->handler_running = this_thread;
  this_thread->on_kill = CAST_TOK on_handler_killed;
  this_thread->kill_data = c;
  c->suspended = 0;
  c->ready = 0;

  this_thread->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {
    if (!TheMrEdApp->initialized)
      TheMrEdApp->RealInit();
    else {
      DoTheEvent(c);

      while(1) {
	while (MrEdEventReady(c)) {
	  MrEdDoNextEvent(c, NULL, NULL, NULL);
	}

	WaitForAnEvent_OrDie(c);
      }
    }
  }

  /* We should never get here. */
#if 0
  c->ready = 1;
  c->handler_running = NULL;
  this_thread->on_kill = NULL;
  this_thread->kill_data = NULL;
#endif

  return scheme_void;
}

static int MrEdContextReady(MrEdContext *, MrEdContext *c)
{
  return ((MrEdContext *)c)->ready;
}

static void event_found(MrEdContext *c)
{
  if (c->killed)
    return;

  c->ready = 0;

  if (c->waiting_for_nested) {
    c->waiting_for_nested = 0;
    c->nested_avail = 1;
    if (c->suspended) {
      c->suspended = 0;
      scheme_weak_resume_thread(c->handler_running);
    }
  } else {
    Scheme_Object *cp, *cust;

    cp = scheme_make_closed_prim(CAST_SCP handle_events, c);
    cust = scheme_get_thread_param(c->main_config, c->main_cells, MZCONFIG_CUSTODIAN);
    scheme_thread_w_details(cp, c->main_config, c->main_cells, c->main_break_cell, (Scheme_Custodian *)cust, 0);
  }
}

static int try_q_callback(Scheme_Object *do_it, int hi)
{
  MrEdContext *c;

  if ((c = check_q_callbacks(hi, MrEdContextReady, NULL, 1))) {
    if (!do_it)
      return 1;

    if (SCHEME_FALSEP(do_it))
      scheme_set_current_thread_ran_some();

    if (c == mred_main_context)
      check_q_callbacks(hi, MrEdSameContext, c, 0);
    else {
      c->q_callback = 1 + hi;
      event_found(c);
    }

    return 1;
  }

  return 0;
}

static int try_dispatch(Scheme_Object *do_it)
{
  MrEdContext *c;
  GC_CAN_IGNORE MrEdEvent e;
  wxTimer *timer;
  int got_one;

  if (try_q_callback(do_it, 2))
    return 1;

  timer = TimerReady(NULL);

  if (timer) {
    if (!do_it)
      return 1;
    if (SCHEME_FALSEP(do_it))
      scheme_set_current_thread_ran_some();

    c = (MrEdContext *)timer->context;

    timer->Dequeue();

    if (c == mred_main_context)
      timer->Notify();
    else {
      c->timer = timer;
      event_found(c);
    }

    return 1;
  }

  if (try_q_callback(do_it, 1))
    return 1;

  ChainContextsList();

  got_one = MrEdGetNextEvent(!do_it, 0, &e, &c);

  UnchainContextsList();

#ifdef wx_mac
  /* MrEdGetNextEvent might enqueue */
  if (try_q_callback(do_it, 1))
    return 1;
#endif

  if (got_one) {
    if (!do_it)
      return 1;

    if (SCHEME_FALSEP(do_it))
      scheme_set_current_thread_ran_some();

    if (c) {
      memcpy(&c->event, &e, sizeof(MrEdEvent));
      event_found(c);
    } else {
      /* Event with unknown context: */
      MrEdDispatchEvent(&e);
    }

    return 1;
  }

  if (try_q_callback(do_it, 0))
    return 1;

  return 0;
}

static void wakeup_on_dispatch(Scheme_Object *, void *fds)
{
#ifdef wx_x
  Display *d = XtDisplay(mred_main_context->finalized->toplevel);
  int fd;

  fd = ConnectionNumber(d);

  MZ_FD_SET(fd, (fd_set *)fds);
#endif
}

static int check_initialized(Scheme_Object *)
{
  return TheMrEdApp->initialized;
}

# define KEEP_GOING wxTheApp->keep_going

void wxDoEvents()
{
  /* When we get here, we are in the main dispatcher thread */
  if (!TheMrEdApp->initialized) {
    MrEdContext *c;

    /* Create the user's main thread: */

    c = (MrEdContext *)MrEdMakeEventspace();

    wxREGGLOB(user_main_context);
    user_main_context = c;

    {
      Scheme_Object *cp;
      cp = scheme_make_closed_prim(CAST_SCP handle_events, c);
      wxREGGLOB(user_main_thread);
      user_main_thread = (Scheme_Thread *)scheme_thread_w_details(cp, 
								  c->main_config,
								  c->main_cells,
								  c->main_break_cell,
								  NULL, 0);
      scheme_set_break_main_target(user_main_thread);
      cp = scheme_intern_symbol("mred");
      user_main_thread->name = cp;
    }

    /* Block until the user's main thread is initialized: */
    scheme_block_until(CAST_BLKCHK check_initialized, NULL, NULL, 0.0);
  }

  if (!try_dispatch(scheme_true)) {
    do {
      Scheme_Thread *thread;
      thread = scheme_get_current_thread();
      thread->block_descriptor = -1;
      thread->blocker = NULL;
      thread->block_check = CAST_BLKCHK try_dispatch;
      thread->block_needs_wakeup = CAST_WU wakeup_on_dispatch;

      scheme_thread_block(0);

      thread = scheme_get_current_thread();
      thread->block_descriptor = 0;
      /* Sets ran_some if it succeeds: */
      if (try_dispatch(scheme_false))
	break;
    } while (KEEP_GOING);
  }
}

Scheme_Object *wxDispatchEventsUntilWaitable(wxDispatch_Check_Fun f, void *data, Scheme_Object *w)
{
  MrEdContext *c;
  Scheme_Object *result = scheme_void;
  Scheme_Thread *thread;

  c = MrEdGetContext();
#ifdef wx_mac
  wxMouseEventHandled();
#endif

  thread = scheme_get_current_thread();
  if (c->ready_to_go
      || (c->handler_running != thread)) {
    /* This is not the handler thread or an event still hasn't been
       dispatched. Wait. */
    if (w) {
      Scheme_Object *a[1];
      a[0] = w;
      result = scheme_sync(1, a);
    } else {
      scheme_block_until((Scheme_Ready_Fun)f, NULL, (Scheme_Object *)data, 0.0);
    }
  } else {
    /* This is the main thread. Handle events */
    do {
      result = MrEdDoNextEvent(c, f, data, w);
      if (result)
	break;
    } while (1);
  }

  return result;
}

void wxDispatchEventsUntil(wxDispatch_Check_Fun f, void *data)
{
  wxDispatchEventsUntilWaitable(f, data, NULL);
}

void wxBlockUntil(wxDispatch_Check_Fun f, void *data)
{
  scheme_block_until((Scheme_Ready_Fun)f, NULL, (Scheme_Object *)data, 0.0);
}

void wxBlockUntilTimeout(wxDispatch_Check_Fun f, void *data, float timeout)
{
  scheme_block_until((Scheme_Ready_Fun)f, NULL, (Scheme_Object *)data, timeout);
}

static SLEEP_PROC_PTR mzsleep;

static void MrEdSleep(float secs, void *fds)
{
  double now;

#ifdef NEVER_EVER_SLEEP
  return;
#endif

  if (!(KEEP_GOING))
    return;

  now = scheme_get_inexact_milliseconds();
  {
    wxTimer *timer;

    timer = GlobalFirstTimer();

    if (timer) {
      double done = timer->expiration;
      double diff = done - now;

      diff /= 1000;
      if (diff <= 0)
	secs = (float)0.00001;
      else if (!secs || (secs > diff))
	secs = (float)diff;
    }
  }

#ifdef wx_msw
  MrEdMSWSleep(secs, fds, mzsleep);
#else
# ifdef wx_mac
  MrEdMacSleep(secs, fds, mzsleep);
# else
  mzsleep(secs, fds);
# endif
#endif
}

#ifdef mred_BREAK_HANDLER
static void *break_handle;
static void *signal_handle;
static void user_break_hit(int ignore)
{
  scheme_break_main_thread_at(break_handle);
  scheme_signal_received_at(signal_handle);

#  ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, user_break_hit);
#  endif
#  ifdef MZ_PRECISE_GC
#   ifndef GC_STACK_CALLEE_RESTORE
  /* Restore variable stack. */
  GC_variable_stack = (void **)__gc_var_stack__[0];
#   endif
#  endif
}
#endif

/****************************************************************************/
/*                                wxTimer                                   */
/****************************************************************************/

wxTimer::wxTimer(void *ctx)
#ifdef wx_xt
 : wxObject(WXGC_NO_CLEANUP)
#endif
{
  __type = wxTYPE_TIMER;

  next = prev = NULL;

  if (!ctx)
    ctx = (void *)MrEdGetContext();

  context = ctx;
}

wxTimer::~wxTimer(void)
{
}

void wxTimer::SetContext(void *ctx)
{
  context = ctx;
}

Bool wxTimer::Start(int millisec, Bool _one_shot)
{
  double now;

  if (prev || next || (((MrEdContext *)context)->timers == this))
    return FALSE;

  if (((MrEdContext *)context)->killed)
    scheme_signal_error("start in timer%%: the current eventspace has been shutdown");
  
  interval = millisec;
  if (interval <= 0)
    interval = 1;
  one_shot = !!_one_shot;

  now = scheme_get_inexact_milliseconds();
  expiration = now + interval;

  if (((MrEdContext *)context)->timers) {
    wxTimer *t = ((MrEdContext *)context)->timers;

    while (1) {
      int later;

      later = (expiration >= t->expiration);

      if (!later) {
	prev = t->prev;
	t->prev = this;
	next = t;
	if (prev)
	  prev->next = this;
	else
	  ((MrEdContext *)context)->timers = this;
	return TRUE;
      }

      if (!t->next) {
	t->next = this;
	prev = t;

	return TRUE;
      }
      t = t->next;
    }
  } else {
    ((MrEdContext *)context)->timers = this;
    scheme_hash_set(timer_contexts, (Scheme_Object *)context, scheme_true);
  }

  return TRUE;
}

void wxTimer::Dequeue(void)
{
  if (!prev) {
    if (((MrEdContext *)context)->timers == this) {
      ((MrEdContext *)context)->timers = next;
      if (!next)
        scheme_hash_set(timer_contexts, (Scheme_Object *)context, NULL);
    }
  }

  if (prev)
    prev->next = next;
  if (next)
    next->prev = prev;

  next = prev = NULL;
}

void wxTimer::Stop(void)
{
  Dequeue();

  interval = -1;
}

/****************************************************************************/
/*                               Callbacks                                  */
/****************************************************************************/

typedef struct Q_Callback {
  /* MZ_PRECISE_GC: allocation relies on this struct as the same as
     array of pointers: */
  MrEdContext *context;
  Scheme_Object *callback;
  struct Q_Callback *prev;
  struct Q_Callback *next;
} Q_Callback;

typedef struct {
  /* Collection relies on this struct as the same as array of
     pointers: */
  Q_Callback *first;
  Q_Callback *last;
} Q_Callback_Set;

static Q_Callback_Set q_callbacks[3];

static void insert_q_callback(Q_Callback_Set *cs, Q_Callback *cb)
{
  /* This can happen under Windows, for example,
     due to an on-paint queue attempt: */
  if (cb->context->killed)
    return;

  cb->next = NULL;
  cb->prev = cs->last;
  cs->last = cb;
  if (cb->prev)
    cb->prev->next = cb;
  else
    cs->first = cb;
}

static void remove_q_callback(Q_Callback_Set *cs, Q_Callback *cb)
{
  if (cb->prev)
    cb->prev->next = cb->next;
  else
    cs->first = cb->next;
  if (cb->next)
    cb->next->prev = cb->prev;
  else
    cs->last = cb->prev;

  cb->next = NULL;
  cb->prev = NULL;
}

static void call_one_callback(Q_Callback * volatile  cb)
{
  mz_jmp_buf *save, newbuf;
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  save = thread->error_buf;
  thread->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf))
    scheme_apply_multi(cb->callback, 0, NULL);
  scheme_clear_escape();
  thread = scheme_get_current_thread();
  thread->error_buf = save;
}

static MrEdContext *check_q_callbacks(int hi, int (*test)(MrEdContext *, MrEdContext *),
				      MrEdContext *tdata, int check_only)
{
  Q_Callback_Set *cs = q_callbacks + hi;
  Q_Callback *cb;

  cb = cs->first;
  while (cb) {
    if (test(tdata, cb->context)) {
      if (check_only)
	return cb->context;

      remove_q_callback(cs, cb);

      call_one_callback(cb);

      return cb->context;
    }
    cb = cb->next;
  }

  return NULL;
}

static void remove_q_callbacks(MrEdContext *c)
{
  Q_Callback_Set *cs;
  Q_Callback *cb, *next;
  int i;

  for (i = 0; i < 3; i++) {
    cs = q_callbacks + i;
    for (cb = cs->first; cb; cb = next) {
      next = cb->next;
      if (cb->context == c)
	remove_q_callback(cs, cb);
    }
  }
}

Scheme_Object *MrEd_mid_queue_key;

void MrEd_add_q_callback(char *who, int argc, Scheme_Object **argv)
{
  MrEdContext *c;
  Q_Callback_Set *cs;
  Q_Callback *cb;
  int hi;

  scheme_check_proc_arity(who, 0, 0, argc, argv);
  c = (MrEdContext *)wxsCheckEventspace("queue-callback");

  if (argc > 1) {
    if (argv[1] == MrEd_mid_queue_key)
      hi = 1;
    else
      hi = (SCHEME_TRUEP(argv[1]) ? 2 : 0);
  } else
    hi = 2;

  cs = q_callbacks + hi;

  cb = (Q_Callback*)scheme_malloc(sizeof(Q_Callback));
  cb->context = c;
  cb->callback = argv[0];

  insert_q_callback(cs, cb);
}

#if defined(wx_msw) || defined(wx_mac)

static void MrEdQueueWindowCallback(wxWindow *wx_window, Scheme_Closed_Prim *scp, void *data)
{
  MrEdContext *c;
  Q_Callback *cb;
  Scheme_Object *p;

  if (!scheme_get_current_thread()) {
    /* Scheme hasn't started yet, so call directly.
       We might get here for an update to the stdio
       window, for example. */
    scp(data, 0, NULL);
    return;
  }

#ifdef wx_mac
  c = MrEdGetContext(wx_window->GetRootFrame());
#else
  c = MrEdGetContext();
#endif

  /* Search for existing queued on-paint: */
  cb = q_callbacks[1].last;
  while (cb) {
    if (cb->context == c) {
      if (SCHEME_CLSD_PRIMP(cb->callback)) {
	Scheme_Closed_Primitive_Proc *prim;
	prim = (Scheme_Closed_Primitive_Proc *)cb->callback;
	if ((prim->data == wx_window)
	    && (prim->prim_val == scp)) {
	  /* on-paint already queued */
	  return;
	}
      }
    }
    cb = cb->prev;
  }

  p = scheme_make_closed_prim(scp, data);

  cb = (Q_Callback*)scheme_malloc(sizeof(Q_Callback));
  cb->context = c;
  cb->callback = p;

  insert_q_callback(q_callbacks + 1, cb);

#ifdef wx_mac
  WakeUpMrEd();
#endif
}

static Scheme_Object *call_on_paint(void *d, int, Scheme_Object **argv)
{
  wxWindow *w = (wxWindow *)d;

  ((wxCanvas *)w)->DoPaint();

  return scheme_void;
}

void MrEdQueuePaint(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_on_paint, wx_window);
}

static Scheme_Object *call_close(void *d, int, Scheme_Object **argv)
{
  wxFrame *w = (wxFrame *)d;

  if (w->OnClose())
    w->Show(FALSE);

  return scheme_void;
}

void MrEdQueueClose(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_close, wx_window);
}

static Scheme_Object *call_zoom(void *d, int, Scheme_Object **argv)
{
  wxFrame *w = (wxFrame *)d;

  w->Maximize(2);

  return scheme_void;
}

void MrEdQueueZoom(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_zoom, wx_window);
}

static Scheme_Object *call_toolbar(void *d, int, Scheme_Object **argv)
{
  wxFrame *w = (wxFrame *)d;

  w->OnToolbarButton();

  return scheme_void;
}

void MrEdQueueToolbar(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_toolbar, wx_window);
}

static Scheme_Object *call_on_size(void *d, int, Scheme_Object **argv)
{
  wxWindow *w = (wxWindow *)d;
  w->OnSize(-1, -1);
  return scheme_void;
}

void MrEdQueueOnSize(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_on_size, wx_window);
}

# ifdef wx_mac
static Scheme_Object *call_unfocus(void *d, int, Scheme_Object **argv)
{
  wxFrame *w = (wxFrame *)d;
  w->Unfocus();
  return scheme_void;
}

void MrEdQueueUnfocus(wxWindow *wx_window)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_unfocus, wx_window);
}

static Scheme_Object *call_drop(void *d, int, Scheme_Object **argv)
{
  wxWindow *w = (wxWindow *)SCHEME_CAR((Scheme_Object *)d);
  char *s = (char *)SCHEME_CDR((Scheme_Object *)d);
  w->OnDropFile(s);
  return scheme_void;
}

void MrEdQueueDrop(wxWindow *wx_window, char *s)
{
  MrEdQueueWindowCallback(wx_window, CAST_SCP call_drop, 
			  scheme_make_pair((Scheme_Object *)wx_window, (Scheme_Object *)s));
}
# endif

#endif

static Scheme_Object *call_being_replaced(void *d, int, Scheme_Object **argv)
{
  wxClipboardClient *clipOwner = (wxClipboardClient *)d;
  clipOwner->BeingReplaced();
  return scheme_void;
}

void MrEdQueueBeingReplaced(wxClipboardClient *clipOwner)
{
  Scheme_Object *p;
  MrEdContext *c = (MrEdContext *)clipOwner->context;
  Q_Callback *cb;

  if (c) {
    clipOwner->context = NULL;

    p = scheme_make_closed_prim(CAST_SCP call_being_replaced, clipOwner);

    cb = (Q_Callback*)scheme_malloc(sizeof(Q_Callback));
    cb->context = c;
    cb->callback = p;

    insert_q_callback(q_callbacks + 1, cb);
  }
}

void MrEdQueueInEventspace(void *context, Scheme_Object *thunk)
{
  Q_Callback *cb;

  cb = (Q_Callback*)scheme_malloc(sizeof(Q_Callback));
  cb->context = (MrEdContext *)context;
  cb->callback = thunk;

  insert_q_callback(q_callbacks + 1, cb);
}

/****************************************************************************/
/*                        Redirected Standard I/O                           */
/****************************************************************************/

#if REDIRECT_STDIO || WCONSOLE_STDIO
static void MrEdSchemeMessages(char *, ...);
static Scheme_Object *stdin_pipe;
#endif

#if WCONSOLE_STDIO

static HANDLE console_out;
static HANDLE console_in;
static Scheme_Object *console_inport;
static HWND console_hwnd;
static int has_stdio, stdio_kills_prog;
static HANDLE waiting_sema;
static void *break_handle;
static void *signal_handle;

typedef HWND (WINAPI* gcw_proc)();

static void init_console_in()
{
  if (!console_in) {
    console_in = GetStdHandle(STD_INPUT_HANDLE);
    wxREGGLOB(console_inport);
    console_inport = scheme_make_fd_input_port((int)console_in, scheme_intern_symbol("stdin"), 0, 0);
  }
}

static BOOL WINAPI ConsoleHandler(DWORD op)
{
  if (stdio_kills_prog) {
    ReleaseSemaphore(waiting_sema, 1, NULL);
  } else {
    scheme_break_main_thread();
    scheme_signal_received();
  }
  return TRUE;
}

static void WaitOnConsole()
{
  DWORD wrote;

  stdio_kills_prog = 1;
  if (console_hwnd) {
    AppendMenu(GetSystemMenu(console_hwnd, FALSE), 
	       MF_STRING,
	       SC_CLOSE,
	       "Close");
    /* Un-gray the close box: */
    RedrawWindow(console_hwnd, NULL, NULL, 
		 RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW);
  }

  WriteConsole(console_out, "\n[Exited. Close box or Ctrl-C closes the console.]\n", 51, &wrote, NULL);

  WaitForSingleObject(waiting_sema, INFINITE);
}

#else  /* !WCONSOLE_STDIO */

#if REDIRECT_STDIO
static FILE *mrerr = NULL;
#else
#define mrerr stderr
#endif

#endif /* WCONSOLE_STDIO */

#if REDIRECT_STDIO || WCONSOLE_STDIO
static void MrEdSchemeMessages(char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  
  scheme_start_atomic();

  HIDE_FROM_XFORM(va_start(args, msg));

#if WCONSOLE_STDIO
  if (!console_out) {
    AllocConsole();
    console_out = GetStdHandle(STD_OUTPUT_HANDLE);

    if (!wx_in_terminal) {
      has_stdio = 1;
      waiting_sema = CreateSemaphore(NULL, 0, 1, NULL);
      break_handle = scheme_get_main_thread_break_handle();
      signal_handle = scheme_get_signal_handle();
      SetConsoleCtrlHandler(ConsoleHandler, TRUE);


      {
	HMODULE hm;
	gcw_proc gcw;

	hm = LoadLibrary("kernel32.dll");
	if (hm)
	  gcw = (gcw_proc)GetProcAddress(hm, "GetConsoleWindow");
	else
	  gcw = NULL;
    
	if (gcw)
	  console_hwnd = gcw();
      }

      if (console_hwnd) {
	EnableMenuItem(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE,
		       MF_BYCOMMAND | MF_GRAYED);
	RemoveMenu(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE, MF_BYCOMMAND);
      }
    }
  }
#endif
#if REDIRECT_STDIO
  if (!mrerr)
    mrerr = fopen("mrstderr.txt", "w");
  if (!mrerr) {
    scheme_end_atomic_no_swap();
    HIDE_FROM_XFORM(va_end(args));
    return;
  }
#endif

#if WCONSOLE_STDIO
  if (!msg) {
    char *s;
    long l, d;
    DWORD wrote;

    s = va_arg(args, char*);
    d = va_arg(args, long);
    l = va_arg(args, long);

    WriteConsole(console_out, s XFORM_OK_PLUS d, l, &wrote, NULL);
  } else {
    long sz, wrt;
    char *buffer;
    DWORD wrote;
    /* FIXME: multiplying by 5 and adding 80 works for
       all the cases where printf mode is currently used 
       for the function, but it's completely a hack. */
    buffer = (char *)malloc((5 * strlen(msg)) + 80);
    wrt = vsprintf(buffer, msg, args);
    WriteConsole(console_out, buffer, strlen(buffer), &wrote, NULL);
    free(buffer);
  }
#endif
#if !WCONSOLE_STDIO
  vfprintf(mrerr, msg, args);
  fflush(mrerr);
#endif

  scheme_end_atomic_no_swap();

  HIDE_FROM_XFORM(va_end(args));
}

static void MrEdSchemeMessagesOutput(char *s, long l)
{
  if (l)
    MrEdSchemeMessages(NULL, s, 0, l);
}
#endif

#if REDIRECT_STDIO || WCONSOLE_STDIO

static Scheme_Object *console_reading;

static void add_console_reading()
{
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!console_reading) {
    wxREGGLOB(console_reading);
    console_reading = scheme_null;
  }

  console_reading = scheme_make_pair((Scheme_Object *)thread,
				     console_reading);
}

static void remove_console_reading()
{
  Scheme_Object *p, *prev = NULL;
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!console_reading)
    return;

  p = console_reading;
  while (SCHEME_PAIRP(p)) {
    if (SAME_OBJ(SCHEME_CAR(p), (Scheme_Object *)thread)) {
      if (prev)
	SCHEME_CDR(prev) = SCHEME_CDR(p);
      else
	console_reading = SCHEME_CDR(p);
      return;
    }
    prev = p;
    p = SCHEME_CDR(p);
  }
}

static void break_console_reading_threads()
{
  Scheme_Object *p;

  if (!console_reading)
    return;

  for (p = console_reading; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
    scheme_break_thread((Scheme_Thread *)SCHEME_CAR(p));
  }
}

static long mrconsole_get_string(Scheme_Input_Port *ip,
				 char *buffer, long offset, long size,
				 int nonblock, Scheme_Object *unless)
{
  long result;
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

#if WCONSOLE_STDIO
  init_console_in();
  pipe = console_inport;
#endif

  add_console_reading();
  result = scheme_get_byte_string_unless("console get-string", pipe, 
					 buffer, offset, size, 
					 nonblock, 0, NULL,
					 unless);
  remove_console_reading();
  return result;
}

static Scheme_Object *mrconsole_progress_evt(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

#if WCONSOLE_STDIO
  init_console_in();
  pipe = console_inport;
#endif

  return scheme_progress_evt(pipe);
}

static int mrconsole_peeked_read(Scheme_Input_Port *ip,
					    long amount,
					    Scheme_Object *unless,
					    Scheme_Object *target_ch)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

#if WCONSOLE_STDIO
  init_console_in();
  pipe = console_inport;
#endif

  return scheme_peeked_read(pipe, amount, unless, target_ch);
}

static int mrconsole_char_ready(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

#if WCONSOLE_STDIO
  init_console_in();
  pipe = console_inport;
#endif

  return scheme_char_ready(pipe);
}

static void mrconsole_close(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
#if WCONSOLE_STDIO
  init_console_in();
  pipe = console_inport;
#endif
  scheme_close_input_port(pipe);
}

static Scheme_Object *MrEdMakeStdIn(void)
{
  Scheme_Object *readp;
  Scheme_Input_Port *ip;

  wxREGGLOB(stdin_pipe);

  scheme_pipe(&readp, &stdin_pipe);

  ip = scheme_make_input_port(scheme_make_port_type("mred-console-input-port"),
			      readp,
			      scheme_intern_symbol("mred-console"),
			      CAST_GS mrconsole_get_string,
			      NULL,
			      mrconsole_progress_evt,
			      mrconsole_peeked_read,
			      CAST_IREADY mrconsole_char_ready,
			      CAST_ICLOSE mrconsole_close,
			      NULL,
			      0);

  return (Scheme_Object *)ip;
}

static long stdout_write(Scheme_Output_Port*, const char *s, long d, long l, 
			 int rarely_block, int enable_break)
{
#if WCONSOLE_STDIO
  if (l)
    MrEdSchemeMessages(NULL, s, d, l);
#else
  static FILE *out = NULL;

  if (!out)
    out = fopen("mrstdout.txt", "w");

  if (out)
    fwrite(s + d, l, 1, out);
#endif
  return l;
}

static Scheme_Object *MrEdMakeStdOut(void)
{
  Scheme_Object *outtype;

  outtype = scheme_make_port_type("stdout");

  return (Scheme_Object *)scheme_make_output_port(outtype, NULL,
						  scheme_intern_symbol("mred-console"),
						  scheme_write_evt_via_write,
						  CAST_WS stdout_write,
						  NULL, NULL, NULL, NULL, NULL, 0);
}

static long stderr_write(Scheme_Output_Port*, const char *s, long d, long l, 
			 int rarely_block, int enable_break)
{
#if WCONSOLE_STDIO
  if (l)
    MrEdSchemeMessages(NULL, s, d, l);
#else
  if (!mrerr)
    mrerr = fopen("mrstderr.txt", "w");

  if (mrerr)
    fwrite(s + d, l, 1, mrerr);
#endif
  return l;
}

static Scheme_Object *MrEdMakeStdErr(void)
{
  Scheme_Object *errtype;

  errtype = scheme_make_port_type("stderr");

  return (Scheme_Object *)scheme_make_output_port(errtype, NULL,
						  scheme_intern_symbol("mred-console"),
						  scheme_write_evt_via_write,
						  CAST_WS stderr_write,
						  NULL, NULL, NULL, NULL, NULL, 0);
}
#endif

void wxmeError(const char *e)
{
  scheme_signal_error("%s", e);
}

/****************************************************************************/
/*                               Debugging                                  */
/****************************************************************************/

#if ADD_OBJ_DUMP
extern int wx_object_count;

# ifndef USE_SENORA_GC
extern "C" GC_PTR GC_changing_list_start, GC_changing_list_current;
# else
# define GC_word int
# endif
extern "C" GC_word GC_dl_entries;
extern "C" GC_word GC_fo_entries;

Scheme_Object *OBJDump(int, Scheme_Object *[])
{
# if REDIRECT_STDIO || WCONSOLE_STDIO
# define PRINT_IT MrEdSchemeMessages
# else
# define PRINT_IT scheme_console_printf
# endif
  int c;

  PRINT_IT("Objects: %d\n", wx_object_count);
# ifndef USE_SENORA_GC
  PRINT_IT("Memory: %d\n", GC_get_heap_size());
# endif
  PRINT_IT("FO: %d\n", GC_fo_entries);
  PRINT_IT("DL: %d\n", GC_dl_entries);
# ifndef USE_SENORA_GC
  PRINT_IT("Changing: %d\n",
	 (long)GC_changing_list_current - (long)GC_changing_list_start);
# endif

  Scheme_Thread *p;
  for (c = 0, p = scheme_first_thread; p; p = p->next)
    c++;

  PRINT_IT("Threads: %d\n", c);

  return scheme_make_integer(wx_object_count);
}
#endif

#ifdef SGC_STD_DEBUGGING
extern "C" {
  extern void (*scheme_external_dump_info)(void);
  extern void (*scheme_external_dump_arg)(Scheme_Object *);
  extern char *(*scheme_external_dump_type)(void *);
};
extern void GC_cpp_for_each(void (*f)(void *, int, void *), void *data);
extern int GC_is_wx_object(void *v);

#define NUM_OBJ_KIND (wxTYPE_SNIP_CLASS_LIST + 1)
static int cpp_count[NUM_OBJ_KIND], cpp_sch_count[NUM_OBJ_KIND], cpp_size[NUM_OBJ_KIND];
static int cpp_actual_count[NUM_OBJ_KIND], cpp_actual_size[NUM_OBJ_KIND];
static unsigned long cpp_lo[NUM_OBJ_KIND], cpp_hi[NUM_OBJ_KIND];

static int trace_path_type;

#if SGC_STD_DEBUGGING
# define USE_WXOBJECT_TRACE_COUNTER
#endif

#ifdef USE_WXOBJECT_TRACE_COUNTER

void wxTraceCount(void *o, int size)
{
  wxObject *obj = (wxObject *)o;
  int type = obj->__type;

  if ((type >= 0) && (type < NUM_OBJ_KIND)) {
    cpp_actual_count[type]++;
    cpp_actual_size[type] += size;

    unsigned long s = (unsigned long)o;
    if (!cpp_lo[type] || (s < cpp_lo[type]))
      cpp_lo[type] = s;
    if (!cpp_hi[type] || (s > cpp_hi[type]))
      cpp_hi[type] = s;
  }
}

void wxTracePath(void *o, unsigned long src, void *pd)
{
  if (trace_path_type > 0) {
    wxObject *obj = (wxObject *)o;
    int type = obj->__type;

    if (type == trace_path_type)
      GC_store_path(o, src, pd);
  }
}

void wxTraceInit(void)
{
  int i;

  for (i = 0; i < NUM_OBJ_KIND; i++) {
    cpp_actual_count[i] = cpp_actual_size[i] = 0;
    cpp_lo[i] = cpp_hi[i] = 0;
  }
}

void wxTraceDone(void)
{
  /* nothing */
}

void wxObjectFinalize(void *o)
{
#if 0
  /* Not every gc instance is a wxObject instance, now: */
  if (((wxObject *)o)->__type != -1) {
# if 0
    /* New non-cleanup flag makes this incorrect: */
    fprintf(stderr, "ERROR: free wxObject had non-deleted type value!");
# else
    ((wxObject *)o)->__type = -1;
# endif
  }
#endif
}

static void set_trace_arg(Scheme_Object *a)
{
  trace_path_type = -1;
  if (a && SCHEME_SYMBOLP(a)) {
    char *s = SCHEME_SYM_VAL(a);
    int i;

    for (i = 0; i < NUM_OBJ_KIND; i++) {
      char *tn = wxGetTypeName(i);
      if (tn && !strcmp(tn, s)) {
	trace_path_type = i;
	return;
      }
    }
  }
}

static char *object_type_name(void *v)
{
  if (GC_is_wx_object(v)) {
    int t = ((wxObject *)v)->__type;
    if ((t >= 0) && (t < NUM_OBJ_KIND)) {
      char *c;
      c = wxGetTypeName(t);
      if (c) {
	if (wxSubType(t, wxTYPE_WINDOW)) {
	  char *lbl;
	  lbl = ((wxWindow *)v)->GetLabel();
	  if (!lbl)
	    lbl = ((wxWindow *)v)->GetTitle();
	  if (!lbl)
	    lbl = ((wxWindow *)v)->GetName();

	  if (lbl) {
	    int l1, l2;
	    char *r;
	    l1 = strlen(c);
	    l2 = strlen(lbl);
	    r = new WXGC_ATOMIC char[l1+l2+2];
	    memcpy(r, c, l1);
	    r[l1] = '=';
	    memcpy(r + l1 + 1, lbl, l2 + 1);

	    return r;
	  }
	}
	return c;
      } else
	return "wxUNKNOWN";
    } else
      return "wxBAD";
  } else
    return "";
}

#endif

static void count_obj(void *o, int s, void *)
{
  wxObject *obj = (wxObject *)o;
  int type = obj->__type;

  if ((type >= 0) && (type < NUM_OBJ_KIND)) {
    cpp_count[type]++;
    if (obj->__gc_external)
      cpp_sch_count[type]++;
#ifdef MEMORY_USE_METHOD
    cpp_size[type] += s + (obj->MemoryUse());
#endif
  }
}

static void dump_cpp_info()
{
  int i, total_count = 0, total_size = 0, total_actual_size = 0;

  for (i = 0; i < NUM_OBJ_KIND; i++)
    cpp_count[i] = cpp_sch_count[i] = cpp_size[i] = 0;

  GC_cpp_for_each(count_obj, NULL);

  scheme_console_printf("\nBegin wxWindows\n");

  for (i = 0; i < NUM_OBJ_KIND; i++) {
    if (cpp_count[i] || cpp_actual_count[i]) {
      char buffer[50];
      char *name = wxGetTypeName(i);

      if (!name) {
	sprintf(buffer, "#%d", i);
	name = buffer;
      }

      scheme_console_printf("%30.30s %4ld %5ld %10ld %10ld %8lx - %8lx\n",
			    name,
			    cpp_sch_count[i],
			    cpp_count[i],
			    cpp_size[i],
			    cpp_actual_size[i],
			    cpp_lo[i],
			    cpp_hi[i]);
#ifdef USE_WXOBJECT_TRACE_COUNTER
      if (cpp_count[i] != cpp_actual_count[i])
	scheme_console_printf("%30.30s actual count: %10ld\n",
			      "", cpp_actual_count[i]);
#endif
      total_count += cpp_count[i];
      total_size += cpp_size[i];
      total_actual_size += cpp_actual_size[i];
    }
  }

  scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			"total", total_count, total_size, total_actual_size);

  scheme_console_printf("End wxWindows\n");

#if ADD_OBJ_DUMP
  scheme_console_printf("\n");
  OBJDump(0, NULL);
#endif
}

#endif

/****************************************************************************/
/*                           AIX DANGER signal                              */
/****************************************************************************/

#if defined(_IBMR2)
#define DANGER_ALARM
#endif

#ifdef DANGER_ALARM

static int danger_signal_received = 0;
static wxDialogBox *dangerFrame = NULL;

class DangerThreadTimer : public wxTimer
{
 public:
  void Notify(void);
};

void DismissDanger(wxObject &o, wxEvent &e)
{
  dangerFrame->Show(FALSE);
  dangerFrame = NULL;
  danger_signal_received = 0;
}

void DangerThreadTimer::Notify(void)
{
  if (danger_signal_received) {
    if (!dangerFrame) {
      wxREGGLOB(dangerFrame);
      dangerFrame = new WXGC_PTRS wxDialogBox((wxWindow *)NULL, "Danger", FALSE, 0, 0, 300, 200);

      (void) new WXGC_PTRS wxMessage(dangerFrame, "Warning: Paging space is low.");

      dangerFrame->NewLine();

      wxButton *b = new WXGC_PTRS wxButton(dangerFrame, (wxFunction)DismissDanger, "Ok");

      dangerFrame->Fit();
      b->Centre(wxHORIZONTAL);

      dangerFrame->Centre(wxBOTH);
      dangerFrame->Show(TRUE);
    }
  }
}

#endif

/****************************************************************************/
/*                             Application                                  */
/****************************************************************************/

MrEdApp::MrEdApp()
{
#ifndef wx_xt
  if (!wx_class)
    wx_class = "mred";
#endif
}

extern "C" {
  MZ_EXTERN void (*GC_out_of_memory)(void);
};

static void MrEdOutOfMemory(void)
{
  /* Hopefully we have enough memory for a message dialog under
     Windows and Mac OS X: */
#ifdef wx_mac
  Alert(101, NULL);
#endif
#ifdef wx_msw
  wxNoMoreCallbacks();
  MessageBox(NULL, 
             "Racket virtual machine is out of memory. Aborting.",
             "Out of Memory",
             MB_OK);
#endif
  /* For X, mzscheme already writes to stderr (and maybe syslog). */
}

void *wxOutOfMemory()
{
  scheme_out_of_memory_abort();
  return NULL;
}

extern "C" {
  typedef void (*OOM_ptr)(void);
}

static OOM_ptr mr_save_oom;
static mz_jmp_buf oom_buf;

static void not_so_much_memory(void)
{
  scheme_longjmp(oom_buf, 1);
}

void *wxMallocAtomicIfPossible(size_t s)
{
  void *v;

  if (s < 5000)
    return scheme_malloc_atomic(s);

  mr_save_oom = GC_out_of_memory;
  if (!scheme_setjmp(oom_buf)) {
    GC_out_of_memory = (OOM_ptr)not_so_much_memory;
    v = scheme_malloc_atomic(s);
  } else {
    v = NULL;
  }
  GC_out_of_memory = mr_save_oom;

  return v;
}

#if !defined(USE_SENORA_GC) && !defined(MZ_PRECISE_GC)
static void MrEdIgnoreWarnings(char *, GC_word)
{
}
#endif

void wxDoMainLoop()
{
  TheMrEdApp->MainLoop();
}

static Scheme_Env *setup_basic_env()
{
  scheme_set_banner(BANNER);

  wxREGGLOB(global_env);
  global_env = scheme_basic_env();

#ifdef DANGER_ALARM
  {
    DangerThreadTimer *t = new WXGC_PTRS DangerThreadTimer();
    t->Start(10000);
  }
#endif

  scheme_add_evt(mred_eventspace_type,
		 (Scheme_Ready_Fun)check_eventspace_inactive,
		 NULL,
		 NULL, 0);
  scheme_add_evt(mred_nested_wait_type,
		 CAST_BLKCHK check_for_nested_event,
		 NULL,
		 NULL, 0);

  scheme_add_custodian_extractor(mred_eventspace_hop_type,
				 CAST_EXT extract_eventspace_from_hop);

  wxsScheme_setup(global_env);

  scheme_set_param(scheme_current_config(), mred_eventspace_param, (Scheme_Object *)mred_main_context);

  wxREGGLOB(def_dispatch);
  def_dispatch = scheme_make_prim_w_arity(CAST_SP def_event_dispatch_handler,
					  "default-event-dispatch-handler",
					  1, 1);
  scheme_set_param(scheme_current_config(), mred_event_dispatch_param, def_dispatch);

  /* Make sure ps-setup is installed in the parameterization */
  ps_ready = 1;
  /* wxSetThePrintSetupData(wxGetThePrintSetupData()); */

  MakeContext(mred_main_context);

  mred_only_context = NULL;

  /* This handler_running pointer gets reset later. Do
     we really need to set it now? */
  {
    Scheme_Thread *thread;
    thread = scheme_get_current_thread();
    mred_main_context->handler_running = thread;
  }

  mzsleep = scheme_sleep;
  scheme_sleep = CAST_SLEEP MrEdSleep;

#if ADD_OBJ_DUMP
  scheme_add_global("dump-object-stats",
		    scheme_make_prim(OBJDump), global_env);
#endif

  return global_env;
}

#if WCONSOLE_STDIO
static void MrEdExit(int v)
{
  if (has_stdio) {
    WaitOnConsole();
  }

#ifdef wx_msw
  mred_clean_up_gdi_objects();
#endif
  scheme_immediate_exit(v);
}
#endif

wxFrame *MrEdApp::OnInit(void)
{
  MrEdContext *mmc;

  initialized = 0;

#ifdef wx_mac
  {
    TSMDocumentID doc;
    OSType itfs[1];
    itfs[0] = kUnicodeDocumentInterfaceType;
    NewTSMDocument(1, itfs, &doc, 0);
    UseInputWindow(NULL, TRUE);
    ActivateTSMDocument(doc);
  }
#endif

  wxREGGLOB(mred_frames);
  wxREGGLOB(timer_contexts);
  timer_contexts = scheme_make_hash_table(SCHEME_hash_ptr);

#ifdef LIBGPP_REGEX_HACK
  new WXGC_PTRS Regex("a", 0);
#endif

#if REDIRECT_STDIO || WCONSOLE_STDIO
  if (!wx_in_terminal) {
    scheme_make_stdin = CAST_MK MrEdMakeStdIn;
    scheme_make_stdout = CAST_MK MrEdMakeStdOut;
    scheme_make_stderr = CAST_MK MrEdMakeStdErr;
  }
#endif

#if !defined(USE_SENORA_GC) && !defined(MZ_PRECISE_GC)
  GC_set_warn_proc(CAST_IGNORE MrEdIgnoreWarnings);
#endif
  scheme_set_report_out_of_memory(MrEdOutOfMemory);

#ifdef SGC_STD_DEBUGGING
  scheme_external_dump_info = dump_cpp_info;
# ifdef USE_WXOBJECT_TRACE_COUNTER
  scheme_external_dump_type = object_type_name;
  scheme_external_dump_arg = set_trace_arg;
# endif
#endif

#if REDIRECT_STDIO || WCONSOLE_STDIO
  scheme_console_printf = CAST_PRINTF MrEdSchemeMessages;
  if (!wx_in_terminal) {
    scheme_console_output = CAST_OUTPUT MrEdSchemeMessagesOutput;
  }
#endif

  mred_eventspace_param = scheme_new_param();
  mred_event_dispatch_param = scheme_new_param();
  mred_ps_setup_param = scheme_new_param();

  mred_eventspace_type = scheme_make_type("<eventspace>");
  mred_nested_wait_type = scheme_make_type("<eventspace-nested-wait>");
  mred_eventspace_hop_type = scheme_make_type("<internal:eventspace-hop>");
#ifdef MZ_PRECISE_GC
  GC_register_traversers(mred_eventspace_type,
			 size_eventspace_val,
			 mark_eventspace_val,
			 fixup_eventspace_val,
			 1, 0);
  GC_register_traversers(mred_nested_wait_type,
			 size_nested_wait_val,
			 mark_nested_wait_val,
			 fixup_nested_wait_val,
			 1, 0);
  GC_register_traversers(mred_eventspace_hop_type,
			 size_eventspace_hop_val,
			 mark_eventspace_hop_val,
			 fixup_eventspace_hop_val,
			 1, 0);
#endif

#ifdef NEED_HET_PARAM
  wxREGGLOB(mred_het_key);
  mred_het_key = scheme_make_symbol("het"); /* uninterned */
#endif

#ifdef MZ_PRECISE_GC
  mmc = (MrEdContext *)GC_malloc_one_tagged(sizeof(MrEdContext));
#else
  mmc = new WXGC_PTRS MrEdContext;
#endif
  mmc->so.type = mred_eventspace_type;
  wxREGGLOB(mred_main_context);
  mred_main_context = mmc;
  {
    wxChildList *cl;
    cl = new WXGC_PTRS wxChildList();
    mmc->topLevelWindowList = cl;
  }
  {
    MrEdFinalizedContext *fc;
    fc = new WXGC_PTRS MrEdFinalizedContext;
    mmc->finalized = fc;
  }

  wxREGGLOB(mred_only_context);
  mred_only_context = mred_main_context;

  MrEdInitFirstContext(mred_main_context);

  /* Just in case wxWindows needs an initial frame: */
  /* (Windows needs it for the clipboard.) */
  wxREGGLOB(mred_real_main_frame);
  mred_real_main_frame = new WXGC_PTRS wxFrame(NULL, "MrEd");
#ifdef wx_msw
  TheMrEdApp->wx_frame = mred_real_main_frame;
#endif

  wxInitClipboard();

  wxscheme_early_gl_init();

#ifdef mred_BREAK_HANDLER
  break_handle = scheme_get_main_thread_break_handle();
  signal_handle = scheme_get_signal_handle();
# ifdef OS_X
  _signal_nobind(SIGINT, user_break_hit);
# else
  MZ_SIGSET(SIGINT, user_break_hit);
# endif
#endif

#ifdef wx_mac
# ifdef OS_X
  /* Hack to make sure it's referenced, so that xform doesn't throw it away. */
  wx_in_terminal = wx_in_terminal;
# endif
#endif

  mred_run_from_cmd_line(argc, argv, setup_basic_env);

#if WCONSOLE_STDIO
  if (!wx_in_terminal) {
    /* The only reason we get here is that a command-line error or
       -h occured. In either case, stick around for the sake of the
       console. */
    MrEdExit(1);
  }
#endif

  return NULL;
}

static void on_main_killed(Scheme_Thread *p)
{
  on_handler_killed(p);

  if (scheme_exit)
    scheme_exit(exit_val);
  else {
#ifdef wx_msw
    mred_clean_up_gdi_objects();
#endif
    scheme_immediate_exit(exit_val);
  }
}

void MrEdApp::RealInit(void)
{
  Scheme_Thread *thread;
  int skip = 0;

  thread = scheme_get_current_thread();

  initialized = 1;

  thread->on_kill = CAST_TOK on_main_killed;
#if WCONSOLE_STDIO
  if (!wx_in_terminal)
    scheme_exit = CAST_EXIT MrEdExit;
#endif

#ifdef wx_xt
  if (wx_single_instance) {
    skip = wxCheckSingleInstance(global_env);
  }
#endif

  if (!exit_val && !skip)
    exit_val = mred_finish_cmd_line_run();

  scheme_kill_thread(thread);
}

#ifdef wx_mac
char *wx_original_argv_zero;
static char *about_label;
extern "C" char *scheme_get_exec_path();
char *MrEdApp::GetDefaultAboutItemName()
{
# ifdef OS_X
  if (!about_label) {
    char *p;
    int i, len;

    p = wx_original_argv_zero;
    len = strlen(p);
    for (i = len - 1; i; i--) {
      if (p[i] == '/') {
	i++;
	break;
      }
    }

    wxREGGLOB(about_label);
    about_label = new WXGC_ATOMIC char[len - i + 20];
    sprintf(about_label, "About %s...", p + i);
  }

  return about_label;
# else
  return "About...";
# endif
}

void MrEdApp::DoDefaultAboutItem()
{
  DialogPtr dial;
  short hit;
  CGrafPtr port;
  GDHandle device;

  dial = GetNewDialog(129, NULL, (WindowRef)-1);
  GetGWorld(&port,&device);

  SetGWorld(GetDialogPort(dial),GetGDevice());

  TextFont(kFontIDGeneva);
  TextSize(10);
  SetGWorld(port,device);

  ModalDialog(NULL, &hit);

  DisposeDialog(dial);
}

#ifdef OS_X
extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
#endif

int wxGetOriginalAppFSSpec(FSSpec *spec)
{
  char *s = wx_original_argv_zero;

#ifdef OS_X
  /* Need the folder of the exe, three levels up: */
  {
    char *p;
    int i, len, c = 0;

    p = s;
    len = strlen(s);
    for (i = len - 1; i; i--) {
      if (p[i] == '/') {
	c++;
	if (c == 3) {
	  i++;
	  break;
	}
      }
    }

    if (i) {
      char *s2;
      s2 = new WXGC_ATOMIC char[i + 1];
      memcpy(s2, s, i);
      s2[i] = 0;
      s = s2;
    }
  }
#endif

  return scheme_mac_path_to_spec(s, spec);
}

#endif

int MrEdApp::OnExit(void)
{
  return 0;
}

void wxCreateApp(void)
{
  if (!TheMrEdApp) {
#ifdef wx_mac
    wxmac_reg_globs();
#endif
#ifdef wx_msw
  {
    HANDLE h;
    h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h && (h != INVALID_HANDLE_VALUE)
        && (GetFileType(h) != FILE_TYPE_UNKNOWN)) {
      wx_in_terminal = 1;
    }
  }
#endif

    wxREGGLOB(orig_ps_setup);
    wxREGGLOB(q_callbacks);

    wxREGGLOB(TheMrEdApp);
    TheMrEdApp = new WXGC_PTRS MrEdApp;
  }
}

/****************************************************************************/
/*                              wxFlushDisplay                              */
/****************************************************************************/

void wxFlushDisplay(void)
{
#ifdef wx_x
  Display *d;

  d = XtDisplay(wxAPP_TOPLEVEL);

  XFlush(d);
  XSync(d, FALSE);
  XFlush(d);
  XSync(d, FALSE);
#endif
#ifdef wx_mac
  wxFlushMacDisplay();
#endif
}

#ifdef DEFINE_DUMMY_PURE_VIRTUAL
/* Weird hack to avoid linking to libg++ */
extern "C" {
 void __pure_virtual(void) {  }
}
#endif

/****************************************************************************/
/*                            wxHiEventTrampoline                           */
/****************************************************************************/

#ifdef NEED_HET_PARAM

/* In certain Windows and Mac OS modes (e.g., to implement scrolling),
   we run Scheme code atomically to avoid copying part of the stack
   that belongs to the system. We run arbitrary code, however, the
   code does not run to completion. Instead, we suspend the
   continuation after a while, and then try to continue on the next OS
   stop point (e.g., an WM_XSCROLL message). Hopefully, a timer
   ensures that a suspended continuation gets to continue soon when
   nothing else is going on.  During this special mode, other messages
   that can call into Scheme are ignored (e.g., WM_ACTIVATE). After
   the OS mode ends (e.g., the scroller returns), any pending
   continuation is finished, but in non-atomic mode, and things are
   generally back to normal.

   The call process is
    wxHiEventTrampoline(f, data)
     -> f(data) in ht mode
         -> ... mred_het_run_some(g, data2)        \
             -> Scheme code, may finish or may not  | maybe loop
         het->in_progress inicates whether done    /
     -> continue scheme if not finished

   In this process, it's the call stack between f(data)
   and the call to mred_het_run_some() that won't be copied
   in or out until f(data) returns. 

   Nesting wxHiEventTrampoline() calls should be safe, but it won't
   achieve the goal, which is to limit the amount of work done before
   returning (because the inner wxHiEventTrampoline will have to run
   to completion). */

static unsigned long get_deeper_base();

int wxHiEventTrampoline(int (*_wha_f)(void *), void *wha_data)
{
  HiEventTramp *het;
  HiEventTrampProc wha_f = (HiEventTrampProc)_wha_f;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Object *bx;

  het = new WXGC_PTRS HiEventTramp;

  bx = scheme_make_raw_pair((Scheme_Object *)het, NULL);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(mred_het_key, bx);

  het->progress_cont = scheme_new_jmpupbuf_holder();

  scheme_init_jmpup_buf(&het->progress_cont->buf);

  scheme_start_atomic();
  het->val = wha_f(wha_data);

  if (het->timer_on) {
    het->timer_on = 0;
# ifdef wx_msw
    KillTimer(NULL, het->timer_id);
# endif
  }

  if (het->in_progress) {
    /* We have leftover work; jump and finish it (non-atomically).
       But don't swap until we've jumped back in, because the jump-in
       point might be trying to suspend the thread (and that should
       complete before any swap). */
    scheme_end_atomic_no_swap();
    SCHEME_CAR(bx) = NULL;
    het->in_progress = 0;
    het->progress_is_resumed = 1;
    if (!scheme_setjmp(het->progress_base)) {
#ifdef MZ_PRECISE_GC
      het->fixup_var_stack_chain = &__gc_var_stack__;
#endif
      scheme_longjmpup(&het->progress_cont->buf);
    }
  } else {
    scheme_end_atomic();
  }

  scheme_pop_continuation_frame(&cframe);

  het->old_param = NULL;
  het->progress_cont = NULL;
  het->do_data = NULL;

  return het->val;
}

static void suspend_het_progress(void)
{
  HiEventTramp * volatile het;
  double msecs;

  {
    Scheme_Object *v;
    v = scheme_extract_one_cc_mark(NULL, mred_het_key);
    het = (HiEventTramp *)SCHEME_CAR(v);
  }

  msecs = scheme_get_inexact_milliseconds();
  if (msecs < het->continue_until)
    return;

  scheme_on_atomic_timeout = NULL;

  het->yielding = 0;
  het->in_progress = 1;
  if (scheme_setjmpup(&het->progress_cont->buf, (void*)het->progress_cont, het->progress_base_addr)) {
    /* we're back */
    scheme_reset_jmpup_buf(&het->progress_cont->buf);
    het->yielding = 0;
#ifdef MZ_PRECISE_GC
    /* Base addr points to the last valid gc_var_stack address.
       Fixup that link to skip over the part of the stack we're
       not using right now. */
    ((void **)het->progress_base_addr)[0] = het->fixup_var_stack_chain;
    ((void **)het->progress_base_addr)[1] = NULL;
#endif
  } else {
    /* we're leaving */
    scheme_longjmp(het->progress_base, 1);
  }
}

#define HET_RUN_MSECS 200

static void het_run_new(HiEventTramp * volatile het)
{
  double msecs;

  /* We're willing to start new work that is specific to this thread */
  het->progress_is_resumed = 0;

  msecs = scheme_get_inexact_milliseconds();
  het->continue_until = msecs + HET_RUN_MSECS;
  
  if (!scheme_setjmp(het->progress_base)) {
    scheme_start_atomic();
    scheme_on_atomic_timeout = CAST_SUSPEND suspend_het_progress;
    /* Due to het param, yield work will be restricted: */
    het->yielding = 1;
    if (het->do_f) {
      HiEventTrampProc do_f = het->do_f;
      do_f(het->do_data);
    } else
      wxYield();
    het->yielding = 0;
  }

  if (het->progress_is_resumed) {
    /* we've already returned once; jump out to new progress base */
    scheme_longjmp(het->progress_base, 1);
  } else {
    scheme_on_atomic_timeout = NULL;
    scheme_end_atomic_no_swap();
  }
}

static void het_do_run_new(HiEventTramp * volatile het, int *iteration)
{
  /* This function just makes room on the stack, eventually calling
     het_run_new(). */
  int new_iter[32];

  if (iteration[0] == 3) {
#ifdef MZ_PRECISE_GC
    het->progress_base_addr = (void *)&__gc_var_stack__;
#else
    het->progress_base_addr = (void *)new_iter;
#endif
    het_run_new(het);
  } else {
    new_iter[0] = iteration[0] + 1;
    het_do_run_new(het, new_iter);
  }
}

int mred_het_run_some(HiEventTrampProc do_f, void *do_data)
{
  HiEventTramp * volatile het;
  int more = 0;

  {
    Scheme_Object *v;
    v = scheme_extract_one_cc_mark(NULL, mred_het_key);
    if (v)
      het = (HiEventTramp *)SCHEME_CAR(v);
    else
      het = NULL;
  }

  if (het) {
    if (het->in_progress) {
      /* We have work in progress. */
      if ((unsigned long)het->progress_base_addr < get_deeper_base()) {
	/* We have stack space to resume the old work: */
        double msecs;
	het->in_progress = 0;
	het->progress_is_resumed = 1;
        msecs = scheme_get_inexact_milliseconds();
        het->continue_until = msecs + HET_RUN_MSECS;
	scheme_start_atomic();
	scheme_on_atomic_timeout = CAST_SUSPEND suspend_het_progress;
	if (!scheme_setjmp(het->progress_base)) {
#ifdef MZ_PRECISE_GC
	  het->fixup_var_stack_chain = &__gc_var_stack__;
#endif
	  scheme_longjmpup(&het->progress_cont->buf);
	} else {
	  scheme_on_atomic_timeout = NULL;
	  scheme_end_atomic_no_swap();
	}
      }
    } else {
      int iter[1];
      iter[0] = 0;
      het->do_f = do_f;
      het->do_data = do_data;
      het_do_run_new(het, iter);
    }

    more = het->in_progress;
  }

  return more;
}

// Disable warning for returning address of local variable.
#ifdef _MSC_VER
#pragma warning (disable:4172)
#endif

static unsigned long get_deeper_base()
{
  long here;
  return (unsigned long)&here;
}

// re-enable warning
#ifdef _MSC_VER
#pragma warning (default:4172)
#endif

#endif

/****************************************************************************/
/*                              AE-like support                             */
/****************************************************************************/

static void wxDo(Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  mz_jmp_buf * volatile save, newbuf;
  volatile int block_descriptor;
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!proc) {
    /* Oops --- too early. */
    return;
  }

  /* wxDo might be called when GRacket is sleeping (i.e.,
     blocked on WNE in OS X). Since we're hijacking the
     thread, save an restore block information. */
  block_descriptor = thread->block_descriptor;
  thread->block_descriptor = 0;

  scheme_start_atomic();

  save = thread->error_buf;
  thread->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    scheme_clear_escape();
  } else {
    scheme_apply(proc, argc, argv);
  }

  thread = scheme_get_current_thread();
  thread->error_buf = save;
  thread->block_descriptor = block_descriptor;

  scheme_end_atomic_no_swap();
}

void wxDrop_Runtime(char **argv, int argc)
{
  int i;

  for (i = 0; i < argc; i++) {
    Scheme_Object *p[1];
#ifdef wx_xt
    p[0] = scheme_char_string_to_path(scheme_make_utf8_string(argv[i]));
#else
    p[0] = scheme_make_path(argv[i]);
#endif
    wxDo(wxs_app_file_proc, 1, p);
  }
}

#if defined(wx_mac) || defined(wx_msw)
void wxDrop_Quit()
{
  wxDo(wxs_app_quit_proc, 0, NULL);
}
#endif

#ifdef wx_mac
void wxDo_About()
{
  wxDo(wxs_app_about_proc, 0, NULL);
}

void wxDo_Pref()
{
  if (!SCHEME_FALSEP(wxs_app_pref_proc))
    wxDo(wxs_app_pref_proc, 0, NULL);
}

int wxCan_Do_Pref()
{
  return SCHEME_TRUEP(wxs_app_pref_proc);
}
#endif
