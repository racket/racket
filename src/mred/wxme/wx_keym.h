
#ifndef wx_keym
#define wx_keym

#include "wx_stdev.h"
#include "wx_hash.h"

#define UNKNOWN_OBJ void*

typedef Bool (*wxKMFunction)(UNKNOWN_OBJ media, wxEvent *event, void *data);

typedef Bool (*wxGrabKeyFunction)(char *str, class wxKeymap *km,
				  UNKNOWN_OBJ media, wxKeyEvent *event, 
				  void *data);
typedef Bool (*wxGrabMouseFunction)(char *str, class wxKeymap *km,
				    UNKNOWN_OBJ media, wxMouseEvent *event, 
				    void *data);

typedef void (*wxBreakSequenceFunction)(void *data);

enum {
  wxKEY_FINAL = 0,
  wxKEY_PREFIX
};

#define WXK_MOUSE_RIGHT         (-1)
#define WXK_MOUSE_LEFT          (-2)
#define WXK_MOUSE_MIDDLE        (-3)
#define WXK_MOUSE_RIGHT_DOUBLE  (-4)
#define WXK_MOUSE_LEFT_DOUBLE   (-5)
#define WXK_MOUSE_MIDDLE_DOUBLE (-6)
#define WXK_MOUSE_RIGHT_TRIPLE  (-7)
#define WXK_MOUSE_LEFT_TRIPLE   (-8)
#define WXK_MOUSE_MIDDLE_TRIPLE (-9)

#define WXK_CLICK_ADDER         (-3)

class wxKeymap : public wxObject
{
  wxHashTable *functions;
  wxHashTable *keys;

  int chainCount;
  wxKeymap **chainTo;

  long doubleInterval;

  int lastButton;
  int clickCount;
  long lastTime;
  double lastX, lastY;

  class wxKeycode *prefix;

  char *active_mouse_function;

  wxGrabKeyFunction grabKeyFunction;
  void *grabKeyData;
  wxGrabMouseFunction grabMouseFunction;
  void *grabMouseData;

  wxBreakSequenceFunction onBreak;
  void *onBreakData;

  class wxKeycode *FindKey(long, Bool, Bool, Bool, Bool, class wxKeycode *);
  int HandleEvent(long code, Bool shift, Bool ctrl, Bool alt, Bool meta,
		  int score, char **fname, int *fullset);
  int GetBestScore(long code, Bool shift, Bool ctrl, Bool alt, Bool meta);

  Bool CycleCheck(wxKeymap *km);

  int ChainHandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event,
			  wxGrabKeyFunction grab, void *grabData,
			  int try_state, int score);
  int ChainHandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event,
			    wxGrabMouseFunction grab, void *grabData,
			    int try_state, int score);

  int OtherHandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event,
			  wxGrabKeyFunction grab, void *grabData,
			  int try_state, int score);
  int OtherHandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event,
			    wxGrabMouseFunction grab, void *grabData,
			    int try_state, int score);

  int GetBestScore(wxKeyEvent *event);
  int GetBestScore(wxMouseEvent *event);

  void Reset(void);

 public:
  wxKeymap();
  ~wxKeymap();
  
  virtual Bool HandleKeyEvent(UNKNOWN_OBJ media, wxKeyEvent *event);
  virtual Bool HandleMouseEvent(UNKNOWN_OBJ media, wxMouseEvent *event);

  void SetGrabKeyFunction(wxGrabKeyFunction grab, void *grabData);
  void RemoveGrabKeyFunction(void);
  void SetGrabMouseFunction(wxGrabMouseFunction grab, void *grabData);
  void RemoveGrabMouseFunction(void);

  void BreakSequence(void);
  void SetBreakSequenceCallback(wxBreakSequenceFunction f, void *data);

  class wxKeycode *MapFunction(long code, int shift, int ctrl, 
			       int alt, int meta, 
			       char *fname, class wxKeycode *prevkey=NULL, 
			       int keytype = wxKEY_FINAL);
  void MapFunction(char *keyname, char *fname);

  void AddFunction(char *name, wxKMFunction func, void *data);
  Bool CallFunction(char *name, UNKNOWN_OBJ media, wxEvent *event, 
		    Bool try_chained = FALSE);

  void ChainToKeymap(wxKeymap *, Bool prefix);
  void RemoveChainedKeymap(wxKeymap *);

  long GetDoubleClickInterval();
  void SetDoubleClickInterval(long);
};

#endif
