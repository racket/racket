#lang scribble/doc
@(require "common.rkt")

@defclass/title[keymap% object% ()]{

A @racket[keymap%] object is used by @racket[editor<%>] objects to
 map keyboard and mouse sequences to arbitrary functions in an
 extensible way. Keymaps can be used without editors, as well.  A
 @racket[keymap%] object contains

@itemize[

 @item{a mapping from function names to event-handling procedures; and}

 @item{a mapping from key and mouse sequences to function names.}

]

A handler procedure in a keymap is invoked with a @racket[key-event%]
 object or a @racket[mouse-event%] object. It is also given another
 value that depends on the context in which the keymap is used (or,
 more specifically, the arguments to @method[keymap% handle-key-event]
 or @method[keymap% handle-mouse-event]). For keymaps associated with
 @racket[editor<%>] objects, the extra parameter is generally the
 @racket[editor<%>] object that received the keyboard or mouse event.


@defconstructor[()]{

Creates an empty keymap.

}

@defmethod[(add-function [name string?]
                         [func (any/c (is-a?/c event%) . -> . any/c)])
           void?]{

Names a new function to handle events, called in response to
 @method[keymap% handle-key-event], @method[keymap%
 handle-mouse-event], or @method[keymap% call-function]. The return
 value is of the procedure is ignored.

If there was already a function mapped to this name, it will be
 replaced with the given function.

When the function is called, it gets the arguments that were passed to
 @method[keymap% handle-key-event], @method[keymap%
 handle-mouse-event], or @method[keymap% call-function]. For keymaps
 associated with an editor, this is normally the target editor.

}


@defmethod[(break-sequence)
           void?]{

Clears the state of the keymap if it is in the middle of a key
 sequence. For example, the user may have hit escape, and then changed
 to another window; if escape is part of a keyboard sequence, the
 keymap state needs to be cleared because the user is not going to
 complete the sequence.

A break callback function can be installed with @method[keymap%
 set-break-sequence-callback].

}

@defmethod[(call-function [name string?]
                          [in any/c]
                          [event (is-a?/c event%)]
                          [try-chain? any/c #f])
           boolean?]{

Calls a named event handler directly. If the function cannot be found
 or the found handler did not want to handle the event, @racket[#f] is
 returned. Otherwise, the return value is the boolean return value of
 the event handler.

The @racket[in] and @racket[event] arguments are passed on to the keymap
 handler procedure if one is found.

If @racket[try-chain?] is not @racket[#f], keymaps chained to this one
 are searched for the function name.  If the function is not found and
 @racket[try-chain?] is @racket[#f]; an exception is also raised, but
 the exception handler cannot escape (see
 @secref["evtcontjump"]).

}


@defmethod[(chain-to-keymap [next (is-a?/c keymap%)]
                            [prefix? any/c])
           void?]{

Chains @racket[next] off @this-obj[] The @racket[next] keymap will be
 used to handle events which are not handled by @this-obj[]. If
 @racket[prefix?] is a true value, then @racket[next] will take
 precedence over other keymaps already chained to @this-obj[].

Multiple keymaps can be chained off one keymap using @method[keymap%
 chain-to-keymap]. When keymaps are chained off a main keymap, events
 not handled by the main keymap are passed to the chained keymaps
 until some chained keymap handles the events.  Keymaps can be chained
 together in an arbitrary acyclic graph.

Keymap chaining is useful because multiple-event sequences are handled
 correctly for chained groups. Without chaining, a sequence of events
 can produce state in a keymap that must be reset when a callback is
 invoked in one of the keymaps. This state can be manually cleared
 with @method[keymap% break-sequence], though calling the
 @method[keymap% break-sequence] method also invokes the handler
 installed by @method[keymap% set-break-sequence-callback].

}


@defmethod[(get-double-click-interval)
           (integer-in 0 1000000)]{

Returns the maximum number of milliseconds that can separate the
 clicks of a double-click.

The default interval is determined in a platform-specific way, but it
 can be overridden globally though the
 @ResourceFirst{doubleClickTime}; see @|mrprefsdiscuss|.

}

@defmethod[(handle-key-event [in any/c]
                             [event (is-a?/c key-event%)])
           boolean?]{

Attempts to handle a keyboard event, returning @racket[#t] if the event
 was handled (i.e., a handler was found and it returned a true value),
 @racket[#f] otherwise.

See also @method[keymap% call-function].

}


@defmethod[(handle-mouse-event [in any/c]
                               [event (is-a?/c mouse-event%)])
           boolean?]{

Attempts to handle a mouse event, returning @racket[#t] if the event
 was handled (i.e., a handler was found and it returned a true value),
 @racket[#f] otherwise.

See also @method[keymap% call-function].

}


@defmethod[(map-function [keyname string?]
                         [fname string?])
           void?]{

Maps an input state sequence to a function name using a string-encoded
 sequence in @racket[keyname]. The format of @racket[keyname] is a
 sequence of semicolon-delimited input states; each state is made up
 of a sequence of modifier identifiers followed by a key identifier.

The modifier identifiers are:

@itemize[

 @item{@litchar{s:} --- All platforms: Shift}

 @item{@litchar{c:} --- All platforms: Control}

 @item{@litchar{a:} --- Mac OS X: Option}

 @item{@litchar{m:} --- Windows: Alt; Unix: Meta; Mac OS X: Command, when
 @racket[map-command-as-meta-key] produces @racket[#t]}

 @item{@litchar{d:} --- Mac OS X: Command}

 @item{@litchar{l:} --- All platforms: Caps Lock}

 @item{@litchar{?:} --- All platforms: allow match to character produced by opposite 
                  use of Shift, AltGr/Option, and/or Caps Lock, when available; see
@xmethod[key-event% get-other-shift-key-code]} 
]

If a particular modifier is not mentioned in a state string, it
 matches states whether that modifier is pressed or not pressed. A
 @litchar{~} preceding a modifier makes the string match only states
 where the corresponding modifier is not pressed. If the state string
 begins with @litchar{:}, then the string matches a state only if
 modifiers (other than Caps Lock) not mentioned in the string are not
 pressed.

A key identifier can be either a character on the keyboard (e.g.,
 @litchar{a}, @litchar{2}, @litchar{?}) or a special name. The
 special names are as follows:

@itemize[
#:style 'compact
@item{@litchar{leftbutton} (button down)}
@item{@litchar{rightbutton}}
@item{@litchar{middlebutton}}
@item{@litchar{leftbuttondouble} (button down for double-click)}
@item{@litchar{rightbuttondouble}}
@item{@litchar{middlebuttondouble}}
@item{@litchar{leftbuttontriple} (button down for triple-click)}
@item{@litchar{rightbuttontriple}}
@item{@litchar{middlebuttontriple}}
@item{@litchar{leftbuttonseq} (all events from button down through button up)}
@item{@litchar{rightbuttonseq}}
@item{@litchar{middlebuttonseq}}
@item{@litchar{wheelup}}
@item{@litchar{wheeldown}}
@item{@litchar{wheelleft}}
@item{@litchar{wheelright}}
@item{@litchar{esc}}
@item{@litchar{delete}}
@item{@litchar{del}  (same as @litchar{delete})}
@item{@litchar{insert}}
@item{@litchar{ins} (same as @litchar{insert})}
@item{@litchar{add}}
@item{@litchar{subtract}}
@item{@litchar{multiply}}
@item{@litchar{divide}}
@item{@litchar{backspace}}
@item{@litchar{back}}
@item{@litchar{return}}
@item{@litchar{enter} (same as @litchar{return})}
@item{@litchar{tab}}
@item{@litchar{space}}
@item{@litchar{right}}
@item{@litchar{left}}
@item{@litchar{up}}
@item{@litchar{down}}
@item{@litchar{home}}
@item{@litchar{end}}
@item{@litchar{pageup}}
@item{@litchar{pagedown}}
@item{@litchar{semicolon} (since @litchar{;} separates sequence steps)}
@item{@litchar{colon}  (since @litchar{:} separates modifiers)}
@item{@litchar{numpad0}}
@item{@litchar{numpad1}}
@item{@litchar{numpad2}}
@item{@litchar{numpad3}}
@item{@litchar{numpad4}}
@item{@litchar{numpad5}}
@item{@litchar{numpad6}}
@item{@litchar{numpad7}}
@item{@litchar{numpad8}}
@item{@litchar{numpad9}}
@item{@litchar{numpadenter}}
@item{@litchar{f1}}
@item{@litchar{f2}}
@item{@litchar{f3}}
@item{@litchar{f4}}
@item{@litchar{f5}}
@item{@litchar{f6}}
@item{@litchar{f7}}
@item{@litchar{f8}}
@item{@litchar{f9}}
@item{@litchar{f10}}
@item{@litchar{f11}}
@item{@litchar{f12}}
@item{@litchar{f13}}
@item{@litchar{f14}}
@item{@litchar{f15}}
@item{@litchar{f16}}
@item{@litchar{f17}}
@item{@litchar{f18}}
@item{@litchar{f19}}
@item{@litchar{f20}}
@item{@litchar{f21}}
@item{@litchar{f22}}
@item{@litchar{f23}}
@item{@litchar{f24}}
]

For a special keyword, the capitalization does not matter. However,
 capitalization is important for single-letter keynames. Furthermore,
 single-letter ASCII keynames are treated specially: @litchar{A} and
 @litchar{s:a} are both treated as @litchar{s:A}.  However, when
 @litchar{c:} is included on Windows without @litchar{m:}, or when
 @litchar{d:} is included on Mac OS X, then ASCII letters are not
 upcased with @litchar{s:}, since the upcasing behavior of the Shift key
 is cancelled by Control without Alt (on Windows) or by Command
 (on Mac OS X).

A state can match multiple state strings mapped in a keymap (or keymap
 chain); when a state matches multiple state strings, a mapping is
 selected by ranking the strings according to specificity. A state
 string that mentions more pressed modifiers ranks higher than other
 state strings, and if two strings mention the same number of pressed
 modifiers, the one that mentions more unpressed modifiers ranks
 higher. Finally, a state string that includes @litchar{?:} and
 matches only with the opposite use of Shift, AltGr/Option, and/or
 Caps Lock ranks below all matches that do not depend on @litchar{?:},
 and one that requires the opposite use of both Shift and AltGr/Option
 ranks even lower. In the case that multiple matching strings have the
 same rank, a match is selected arbitrarily.

Examples:

@itemize[

 @item{@racket["space"] --- matches whenever the space bar is pressed,
 regardless of the state of modifiers keys.}

 @item{@racket["~c:space"] --- matches whenever the space bar is pressed
 and the Control key is not pressed.}

 @item{@racket["a"] --- matches whenever @litchar{a} is typed, regardless of
 the state of modifiers keys (other than Shift).}

 @item{@racket[":a"] --- matches only when @litchar{a} is typed with no
 modifier keys pressed.}

 @item{@racket["~c:a"] --- matches whenever @litchar{a} is typed and neither
 the Shift key nor the Control key is pressed.}

 @item{@racket[":esc;:c:c"] --- matches an Escape key press (no
 modifiers) followed by a Control-C press (no modifiers other than
 Control).}

 @item{@racket["?:d:+"] --- matches when Command is pressed with key
  that produces @litchar{+}, even if producing @litchar{+} normally requires
  pressing Shift.}

]

A call to @method[keymap% map-function] that would map a particular
 key sequence both as a prefix and as a complete sequence raises an
 exception, but the exception handler cannot escape (see
 @secref["evtcontjump"]).

A function name does not have to be mapped to a handler before input
 states are mapped to the name; the handler is dispatched by name at
 the time of invocation. The event handler mapped to a function name
 can be changed without affecting the map from input states to
 function names.

}


@defmethod[(remove-chained-keymap [keymap (is-a?/c keymap%)])
           void?]{

If @racket[keymap] was previously chained from this keymap (through
 @method[keymap% chain-to-keymap]), then it is removed from the
 chain-to list.

}


@defmethod[(remove-grab-key-function)
           void?]{

Removes a callback installed with @method[keymap%
 set-grab-key-function].

}

@defmethod[(remove-grab-mouse-function)
           void?]{

Removes a callback installed with @method[keymap%
 set-grab-mouse-function].

}


@defmethod[(set-break-sequence-callback [f (-> any)])
           void?]{

Installs a callback procedure that is invoked when @method[keymap%
 break-sequence] is called. After it is invoked once, the callback is
 removed from the keymap. If another callback is installed before
 @method[keymap% break-sequence] is called, the old callback is
 invoked immediately before the new one is installed.

}


@defmethod[(set-double-click-interval [n (integer-in 0 1000000)])
           void?]{

Sets the maximum number of milliseconds that can separate the clicks
 of a double-click.

}

@defmethod[(set-grab-key-function [f ((or/c string? false?) 
                                      (is-a?/c keymap%) 
                                      any/c 
                                      (is-a?/c key-event%)
                                      . -> . any)])
           void?]{

Installs a callback procedure that is invoked after the keymap matches
 input to a function name or fails to match an input. Only one
 keyboard grab function can be installed at a time. When keymaps are
 chained to a keymap with a grab callback, the callback is invoked for
 matches in the chained keymap (when the chained keymap does not have
 its own grab callback).

If a grab callback returns a true value for a matching or non-matching
 callback, the event is considered handled. If the callback returns a
 true value for a matching callback, then the matching keymap function
 is not called by the keymap.

The callback procedure @racket[f] will be invoked as:

@racketblock[
(f _str _keymap _editor _event)
]

The @racket[_str] argument is the name of a function for a matching
 callback, or @racket[#f] for a non-matching callback.  The
 @racket[_keymap] argument is the keymap that matched (possibly a
 keymap chained to the one in which the callback was installed) or the
 keymap in which the callback was installed. The @racket[_editor] and
 @racket[_event] arguments are the same as passed on to the matching
 keymap function.

Key grab callback functions are de-installed with @method[keymap%
 remove-grab-key-function].

}


@defmethod[(set-grab-mouse-function [f ((or/c string? false?) 
                                        (is-a?/c keymap%)
                                        any/c 
                                        (is-a?/c mouse-event%)
                                        . -> . any)])
           void?]{

Like @method[keymap% set-grab-key-function], but for mouse events.

}}

