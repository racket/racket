#lang scribble/doc
@(require "common.rkt")

@defclass/title[control-event% event% ()]{

A @racket[control-event%] object contains information about a
 control event. An instance of @racket[control-event%] is always
 provided to a control or menu item callback procedure.

@defconstructor[([event-type (or/c 'button 'check-box 'choice
                                   'list-box 'list-box-dclick 'list-box-column
                                   'text-field 'text-field-enter 
                                   'menu 'slider 'radio-box 'tab-panel
                                   'menu-popdown 'menu-popdown-none)]
                [time-stamp exact-integer? 0])]{

The @racket[event-type] argument is one of the following:
@itemize[
@item{@racket['button] --- for @racket[button%] clicks}
@item{@racket['check-box] --- for @racket[check-box%] toggles}
@item{@racket['choice] --- for @racket[choice%] item selections}
@item{@racket['list-box] --- for @racket[list-box%] selections and deselections}
@item{@racket['list-box-dclick] --- for @racket[list-box%] double-clicks}
@item{@racket['list-box-column] --- for @racket[list-box%] column clicks in
                                    a @racket[column-control-event%] instance}
@item{@racket['text-field] --- for @racket[text-field%] changes}
@item{@racket['text-field-enter] --- for single-line @racket[text-field%] Enter event}
@item{@racket['menu] --- for @racket[selectable-menu-item<%>] callbacks}
@item{@racket['slider] --- for @racket[slider%] changes}
@item{@racket['radio-box] --- for @racket[radio-box%] selection changes}
@item{@racket['tab-panel] --- for @racket[tab-panel%] tab changes}
@item{@racket['menu-popdown] --- for @racket[popup-menu%] callbacks (item selected)}
@item{@racket['menu-popdown-none] --- for @racket[popup-menu%] callbacks (no item selected)}
]

This value is extracted out of a @racket[control-event%] object with
 the
@method[control-event% get-event-type] method.

See @method[event% get-time-stamp] for information about
@racket[time-stamp].

}

@defmethod[(get-event-type)
           (or/c 'button 'check-box 'choice
                 'list-box 'list-box-dclick 'text-field 
                 'text-field-enter 'menu 'slider 'radio-box 
                 'menu-popdown 'menu-popdown-none 'tab-panel)]{
Returns the type of the control event. See
@racket[control-event%] for information about each event type symbol.

}

@defmethod[(set-event-type
            [type (or/c 'button 'check-box 'choice
                        'list-box 'list-box-dclick 'text-field 
                        'text-field-enter 'menu 'slider 'radio-box 
                        'menu-popdown 'menu-popdown-none 'tab-panel)])
           void?]{

Sets the type of the event. See
@racket[control-event%] for information about each event type symbol.

}}
