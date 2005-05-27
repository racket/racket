#ifndef NO_USER_BREAK_HANDLER
# ifdef MAC_MZ_GUI_ENABLED

extern void (*GC_out_of_memory)(void);

static void MacOutOfMemory(void)
{
  Alert(101, NULL);
  ExitToShell();
}

static int WeAreFront()
{
  static int inited;
  static ProcessSerialNumber us;
  ProcessSerialNumber front;
  Boolean r;
  
  if (!inited) {
    GetCurrentProcess(&us);
    inited = 1;
  }
  GetFrontProcess(&front);
  SameProcess(&us, &front, &r);
  
  return r;
}

static int is_break_event(EventRecord *e)
{
   if ((e->what == keyDown)
       && ((((e->message & charCodeMask) == '.') 
            && (e->modifiers & cmdKey))
           || (((e->message & charCodeMask) == 3)
	           && (e->modifiers & controlKey))))
	 return 1;
  else
    return 0;
}

static int check_break_flag()
{
# ifdef MACINTOSH_GIVE_TIME
  static long last_time;
  static int front = 1;

  if (TickCount() > last_time + (front ? 30 : 0)) {
      EventRecord e;
      front = WeAreFront();
      while (WaitNextEvent(everyEvent, &e, front ? 0 : 30, NULL)) {
        if (is_break_event(&e)) {
	       return 1;
	     }
#  ifdef MACINTOSH_SIOUX
        SIOUXHandleOneEvent(&e);
#  endif
       }
      last_time = TickCount();
    }
# endif
    return 0;
}

static void handle_one(EventRecord *e)
{
  if (is_break_event(e))
    scheme_break_thread(NULL);
  
#  ifdef MACINTOSH_SIOUX
  SIOUXHandleOneEvent(e);
#  endif
}

static void MacSleep(float secs, void *fds)
{
   EventRecord e;
   if (WaitNextEvent(everyEvent, &e, secs * 60, NULL)) {
     if (is_break_event(&e))
       scheme_break_thread(NULL);
     
     handle_one(&e);
   }
}

# endif

#endif

#ifdef MAC_MZ_GUI_ENABLED

void Drop_Runtime(char **argv, int argc)
{
  int i;
	
  for (i = 0; i < argc; i++) {
    printf("(load \"%s\") ", argv[i]);
  }
  if (argc) printf("\n");
}

void Drop_Quit()
{
  ExitToShell();
}

#endif

#ifndef DONT_PARSE_COMMAND_LINE

# ifdef MACINTOSH_SIOUX
static void SetSIOUX(void)
{
  SIOUXSettings.initializeTB = 0;
}
# endif

#endif /* DONT_PARSE_COMMAND_LINE */
