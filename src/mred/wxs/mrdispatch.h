
#ifndef MR_DISPATCH_H
#define MR_DISPATCH_H

typedef int (*wxDispatch_Check_Fun)(void *);
typedef void (*wxDispatch_Needs_Wakeup_Fun)(void *, void *);

extern void wxDispatchEventsUntil(wxDispatch_Check_Fun f, void *data);
extern Scheme_Object *wxDispatchEventsUntilWaitable(wxDispatch_Check_Fun f, void *data, Scheme_Object *w);

#endif
