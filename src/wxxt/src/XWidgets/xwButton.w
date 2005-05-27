# Button -- Button widget
# Bert Bos <bert@let.rug.nl>
# Version 2.0 for FWF V4.0

@class XfwfButton(XfwfLabel)  @file=xwButton

@ The |XfwfButton| widget is a simple button with a
single callback |activate_callback|. Except for that
callback, all resources are inherited from the
|XfwfLabel| widget. Subclasses provide buttons of
other, more specialized types.  |XfwfButton| inherits
its 3D frame from |XfwfFrame|, the location
specification resources from |XfwfBoard| and the
multi-line label from |XfwfLabel|.

The implementation is also very simple, since nearly
all code is inherited from |XfwfLabel|. There are no
new or redefined methods. In fact, just six lines of
actual code suffice to implement the widget: Object
Oriented Programming at its best!


@public

@ The |activate| is invoked from the |activate|
action, which is normally bound to a mouse click. The
|call_data| argument of the callbacks routines is
filled with a pointer to the event that triggered the
action.

	@var <Callback> XtCallbackList activate = NULL

@ The |enter| callback is invoked on an EnterNotify event.

	@var <Callback> XtCallbackList enter = NULL

@ The |leave| callback is invoked on an LeaveNotify event.

	@var <Callback> XtCallbackList leave = NULL

@ The default |frameWidth| is set to 2 pixels.

	@var Dimension frameWidth = 2

@ In contrast to its superclass XfwfLabel, a button
usually takes part in keyboard traversal.

	@var traversalOn = True


@translations

@ By default, the |activate| action is bound to a
mouse click and to the Enter key.

	@trans <Btn1Down>: set_shadow("sunken")
	@trans <Btn1Up>: activate() set_shadow()
	@trans Button1<Leave>: set_shadow() leave()
	@trans Button1<Enter>: set_shadow("sunken") enter()
	@trans <Key>Return: set_shadow("sunken") activate() set_shadow()
	@trans <EnterNotify>: enter()
	@trans <LeaveNotify>: leave()


@actions

@ The |activate| action just calls the |activate|
callback functions, passing the |XEvent| pointer in
the |call_data| argument, if the button is sunken.

@proc activate
{
    if ($frameType == XfwfSunken)
      XtCallCallbackList($, $activate, event);
}

@proc enter
{
    XtCallCallbackList($, $enter, event);
}

@proc leave
{
    XtCallCallbackList($, $leave, event);
}
