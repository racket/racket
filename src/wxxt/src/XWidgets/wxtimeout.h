
void wxRemoveTimeOut(long timer);
long wxAppAddTimeOut(XtAppContext c, unsigned long interval, 
		     XtTimerCallbackProc callback, 
		     XtPointer data,
		     Widget w);
