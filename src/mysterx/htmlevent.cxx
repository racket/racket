// event.cpp -- event-related functions

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "bstr.h"
#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

// number of elts should be same as in EVENT_TYPE enumeration
WCHAR *eventNames[11];

static BOOL html_event_available(MX_Browser_Object *browser) {
  VARIANT_BOOL val;

  val = 0;
  browser->pIEventQueue->get_EventAvailable(&val);

  return val;
}

static void html_event_sem_fun(MX_Browser_Object *browser,void *fds) {
  scheme_add_fd_eventmask(fds,QS_ALLINPUT);
  scheme_add_fd_handle(browser->readSem,fds,TRUE);
}

Scheme_Object *mx_block_until_event(int argc,Scheme_Object **argv) {
  GUARANTEE_BROWSER ("block-until-event", 0);
  scheme_block_until((int (*)(Scheme_Object *))html_event_available,
  		     (void (*)(Scheme_Object *,void *))html_event_sem_fun,
  		     argv[0],0.0F);

  return scheme_void;
}

void initEventNames(void) {
  eventNames[click] = L"click";
  eventNames[dblclick] = L"dblclick";
  eventNames[error] = L"error";
  eventNames[keydown] = L"keydown";
  eventNames[keypress] = L"keypress";
  eventNames[keyup] = L"keyup";
  eventNames[mousedown] = L"mousedown";
  eventNames[mousemove] = L"mousemove";
  eventNames[mouseout] = L"mouseout";
  eventNames[mouseover] = L"mouseover";
  eventNames[mouseup] = L"mouseup";
}

IEvent *getEventInterface(Scheme_Object *ev,char *fname) {
  if (MX_EVENTP(ev) == FALSE) {
    scheme_wrong_type(fname,"com-event",-1,0,&ev) ;
  }

  return MX_EVENT_VAL(ev);
}


Scheme_Object * mx_event_tag (int argc, Scheme_Object **argv)
{
  BSTR tag;

  getEventInterface(argv[0],"mx-event-tag")->get_srcTag(&tag);

  return unmarshalBSTR (tag);
}

Scheme_Object * mx_event_id (int argc, Scheme_Object **argv)
{
  BSTR id;

  getEventInterface(argv[0],"mx-event-id")->get_srcId(&id);

  return unmarshalBSTR (id);
}

Scheme_Object * mx_event_from_tag (int argc, Scheme_Object **argv)
{
  BSTR tag;

  getEventInterface(argv[0],"mx-event-from-tag")->get_fromTag(&tag);

  return unmarshalBSTR (tag);
}

Scheme_Object * mx_event_from_id (int argc, Scheme_Object **argv)
{
  BSTR id;

  getEventInterface (argv[0],"mx-event-from-id")->get_fromId (&id);

  return unmarshalBSTR (id);
}

Scheme_Object * mx_event_to_tag (int argc, Scheme_Object **argv)
{
  BSTR tag;

  getEventInterface (argv[0],"mx-event-to-tag")->get_toTag (&tag);

  return unmarshalBSTR (tag);
}

Scheme_Object * mx_event_to_id (int argc, Scheme_Object **argv)
{
  BSTR id;

  getEventInterface(argv[0],"mx-event-to-id")->get_toId(&id);

  return unmarshalBSTR (id);
}

Scheme_Object *mx_event_keycode(int argc,Scheme_Object **argv) {
  long code;

  getEventInterface(argv[0],"mx-event-keycode")->get_keyCode(&code);

  return scheme_make_integer(code);
}

Scheme_Object *mx_event_shiftkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;

  getEventInterface(argv[0],"mx-event-shiftkey")->get_shiftPressed(&vb);

  return (vb == VARIANT_FALSE) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_altkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;

  getEventInterface(argv[0],"mx-event-altkey")->get_altPressed(&vb);

  return (vb == VARIANT_FALSE) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_ctrlkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;

  getEventInterface(argv[0],"mx-event-ctrlkey")->get_ctrlPressed(&vb);

  return (vb == VARIANT_FALSE) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_x(int argc,Scheme_Object **argv) {
  long x;

  getEventInterface(argv[0],"mx-event-x")->get_x(&x);

  return scheme_make_integer(x);
}

Scheme_Object *mx_event_y(int argc,Scheme_Object **argv) {
  long y;

  getEventInterface(argv[0],"mx-event-y")->get_y(&y);

  return scheme_make_integer(y);
}

Scheme_Object *mx_event_type_pred(int argc,Scheme_Object **argv,WCHAR *evType) {
  EVENT_TYPE actualType;

  getEventInterface(argv[0],"event-<event-type>?")->get_eventType(&actualType);

  return (wcscmp(evType,eventNames[actualType]) == 0)
      ? scheme_true
      : scheme_false;
}

Scheme_Object *mx_event_pred(int argc,Scheme_Object **argv) {
  return MX_EVENTP(argv[0]) ? scheme_true : scheme_false;
}

Scheme_Object *mx_event_keypress_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keypress");
}

Scheme_Object *mx_event_keydown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keydown");
}

Scheme_Object *mx_event_keyup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keyup");
}

Scheme_Object *mx_event_mousedown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousedown");
}

Scheme_Object *mx_event_mouseover_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseover");
}

Scheme_Object *mx_event_mousemove_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousemove");
}

Scheme_Object *mx_event_mouseout_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseout");
}

Scheme_Object *mx_event_mouseup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseup");
}

Scheme_Object *mx_event_click_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"click");
}

Scheme_Object *mx_event_dblclick_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"dblclick");
}

Scheme_Object *mx_event_error_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"error");
}


Scheme_Object *mx_get_event(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IEvent *pEvent;
  IEventQueue *pEventQueue;
  MX_Event *event_object;

  pEventQueue = MX_BROWSER_EVENTQUEUE (GUARANTEE_BROWSER ("mx-get-event", 0));

  pEvent = NULL; // DCOM requires this for some reason

  hr = pEventQueue->GetEvent(&pEvent);  // blocking call

  if (hr != S_OK || pEvent == NULL) {
    codedComError("Error retrieving event",hr);
  }

  event_object = (MX_Event *)scheme_malloc(sizeof(MX_Event));

  event_object->type = mx_event_type;
  event_object->released = FALSE;
  event_object->pEvent = pEvent;

  mx_register_simple_com_object((Scheme_Object *)event_object,pEvent);

  return (Scheme_Object *)event_object;
}
