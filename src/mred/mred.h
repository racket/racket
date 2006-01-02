
#define BREAKING_REQUIRES_SHIFT 1

#ifndef MRED_EXTERN
# define MRED_EXTERN extern
#endif

#ifdef wx_x
typedef XEvent MrEdEvent;
#else
#ifdef wx_msw
typedef MSG MrEdEvent;
#else
typedef EventRecord MrEdEvent;
#endif
#endif

class wxTimer;
class MrEdContextFrames;

#if defined(MZ_PRECISE_GC) || defined(SGC_STD_DEBUGGING)
typedef Scheme_Object *MrEdContextFramesRef;
# define MAKE_FRAMES_REF(x) scheme_make_weak_box((Scheme_Object *)x)
# define FRAMES_REF(p) ((MrEdContextFrames *)SCHEME_WEAK_BOX_VAL(p))
#else
typedef MrEdContextFrames *MrEdContextFramesRef;
# define MAKE_FRAMES_REF(x) x
# define FRAMES_REF(p) p
#endif

class MrEdContextFrames {
 public:
  wxChildList *list;
  MrEdContextFramesRef next, prev;
};

class MrEdFinalizedContext;
class MrEd_Saved_Modal;

#include "wxs/mrdispatch.h"

#ifdef wx_msw
class LeaveEvent;
#endif

typedef struct MrEdContext {
  Scheme_Object so;

  Scheme_Thread *handler_running;
  int suspended;

  MrEdFinalizedContext *finalized;

  wxChildList *topLevelWindowList;
  wxStandardSnipClassList *snipClassList;
  wxBufferDataClassList *bufferDataClassList;
  wxWindow *modal_window;
  MrEd_Saved_Modal *modal_stack;

  Scheme_Config *main_config;
  Scheme_Thread_Cell_Table *main_cells;
  Scheme_Object *main_break_cell;

  short ready_to_go;

  short ready, waiting_for_nested, nested_avail;
  short q_callback;
  wxTimer *timer;
  MrEdEvent event;

  /* Alternate condition for nested event loop pending some condition */
  wxDispatch_Check_Fun alternate;
  void *alt_data;

  /* Used to chain active contexts while reading events: */
  struct MrEdContext *next;

  int busyState;
  int killed;

#ifdef wx_msw
  LeaveEvent *queued_leaves;
#endif

  struct Context_Custodian_Hop *mr_hop;
  Scheme_Custodian_Reference *mref;
} MrEdContext;

class MrEdFinalizedContext {
 public:
#ifdef wx_xt
  Widget toplevel;
#endif
  MrEdContextFrames *frames;
};

extern MrEdContext *mred_contexts;

MrEdContext *MrEdGetContext(wxObject *w = NULL);

extern int MrEdGetNextEvent(int peek, int current_only, MrEdEvent *, MrEdContext **);
void MrEdDispatchEvent(MrEdEvent *);

void MrEdInitFirstContext(MrEdContext *c);
void MrEdInitNewContext(MrEdContext *c);
void MrEdDestroyContext(MrEdFinalizedContext *c);

extern "C" {
  typedef void (*SLEEP_PROC_PTR)(float seconds, void *fds);
}

#ifdef wx_msw
void MrEdMSWSleep(float secs, void *fds);
MRED_EXTERN void mred_clean_up_gdi_objects(void);
#endif

#ifdef wx_mac
void MrEdMacSleep(float secs, void *fds, SLEEP_PROC_PTR mzsleep);
void wxmac_reg_globs(void);
#endif

#if defined(wx_msw) || defined(wx_mac)
# define NEED_HET_PARAM
#endif

#ifdef NEED_HET_PARAM
# ifdef wx_msw
#  define HET_TIMER_T UINT
# else
#  define HET_TIMER_T long
# endif

typedef int (*HiEventTrampProc)(void *);

class HiEventTramp {
public:
  HiEventTrampProc wrap_het_around_f;
  void *wha_data;
  HiEventTrampProc do_f;
  void *do_data;
  int val;
  int in_progress;
  int progress_is_resumed;
  int yielding;
  Scheme_Object *old_param;
  Scheme_Config *config;
  void *progress_base_addr;
  mz_jmp_buf progress_base;
  Scheme_Jumpup_Buf_Holder *progress_cont;
  int timer_on;
  HET_TIMER_T timer_id;
#ifdef MZ_PRECISE_GC
  void *fixup_var_stack_chain;
#endif
};

int mred_het_run_some(HiEventTrampProc do_f, void *do_data);

extern int mred_het_param;

int wxHiEventTrampoline(HiEventTrampProc wha_f, void *wha_data);

#endif // NEED_HET_PARAM

typedef void *(*ForEachFrameProc)(wxObject *, void *);
void *MrEdForEachFrame(ForEachFrameProc fp, void *data);

/* Startup: */
MRED_EXTERN void wxCreateApp(void);
MRED_EXTERN void wxDoMainLoop();

typedef int (*MrEd_Finish_Cmd_Line_Run_Proc)(void);
typedef void (*MrEd_Run_From_Cmd_Line_Proc)(int argc, char **argv, Scheme_Env *(*mk_basic_env)(void));

MRED_EXTERN MrEd_Finish_Cmd_Line_Run_Proc mred_finish_cmd_line_run;
MRED_EXTERN void mred_set_finish_cmd_line_run(MrEd_Finish_Cmd_Line_Run_Proc);
MRED_EXTERN MrEd_Run_From_Cmd_Line_Proc mred_run_from_cmd_line;
MRED_EXTERN void mred_set_run_from_cmd_line(MrEd_Run_From_Cmd_Line_Proc);

# include "../mzscheme/src/schvers.h"

#ifdef MZ_PRECISE_GC
# define MRED3M "3m"
#else
# define MRED3M ""
#endif
#define BANNER "MrEd" MRED3M " version " MZSCHEME_VERSION ", Copyright (c) 2004-2006 PLT Scheme Inc.\n"

#ifndef WINDOW_STDIO
/* Removing "|| defined(wx_msw)" below uses the Windows console.
   The danger is that closing that console kills MrEd without
   any chance of cancelling the kill. */
# if defined(wx_mac) || defined(wx_msw)
#  define WINDOW_STDIO 1
# else
#  define WINDOW_STDIO 0
# endif
#endif

#ifndef WCONSOLE_STDIO
# if defined(wx_msw) && !WINDOW_STDIO
#  define WCONSOLE_STDIO 1
# else
#  define WCONSOLE_STDIO 0
# endif
#endif

#ifndef REDIRECT_STDIO
# if (defined(wx_msw) || defined(wx_mac)) && !WINDOW_STDIO && !WCONSOLE_STDIO
#  define REDIRECT_STDIO 1
# else
#  define REDIRECT_STDIO 0
# endif
#endif

#define MRED_GUID "B2261834-D535-44dd-8511-A26FC8F97DD0"

#if defined(wx_mac) || defined(wx_msw)
MRED_EXTERN void wxDrop_Runtime(char **argv, int argc);
MRED_EXTERN void wxDrop_Quit();
#endif

#if defined(wx_mac)
extern void WakeUpMrEd();
#endif

#if defined(wx_xt)
extern void wxUnhideAllCursors();
#endif
