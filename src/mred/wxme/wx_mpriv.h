
#include "wx_mline.h"

#define MAX_COUNT_FOR_SNIP 500

enum {
  wxSTREAK_EXCEPT_DELAYED = 1,
  wxSTREAK_EXCEPT_KEY_SEQUENCE = 2,
  wxSTREAK_EXCEPT_CURSOR = 4
  };

class wxClickback : public wxObject
{
 public:
  wxClickback();

  long start, end;
  wxClickbackFunc f;
  void *data;
  Bool callOnDown;

  wxStyleDelta *delta;

  Bool hilited;
  wxList *unhilite;
};

#include "wx_timer.h"

class wxMediaFlashTimer : public wxTimer
{
 public:
  wxMediaEdit *media;
  void Notify(void);
};

#include "wx_clipb.h"

#include "wx_ptreq.h"
