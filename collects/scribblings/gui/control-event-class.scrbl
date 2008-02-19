#lang scribble/doc
@(require "common.ss")

@defclass/title[control-event% event% ()]{

A @scheme[control-event%] object contains information about a
 control event. An instance of @scheme[control-event%] is always
 provided to a control or menu item callback procedure.

@defconstructor[([event-type (one-of/c 'button 'check-box 'choice
                                       'list-box 'list-box-dclick 'text-field 
                                       'text-field-enter 'slider 'radio-box 
                                       'menu-popdown 'menu-popdown-none 'tab-panel)]
                [time-stamp (and/c exact? integer?) 0])]{

The @scheme[event-type] argument is one of the following:
@itemize{
@item{@scheme['button] --- for @scheme[button%] clicks}
@item{@scheme['check-box] --- for @scheme[check-box%] toggles}
@item{@scheme['choice] --- for @scheme[choice%] item selections}
@item{@scheme['list-box] --- for @scheme[list-box%] selections and deselections}
@item{@scheme['list-box-dclick] --- for @scheme[list-box%] double-clicks}
@item{@scheme['text-field] --- for @scheme[text-field%] changes}
@item{@scheme['text-field-enter] --- for single-line @scheme[text-field%] Enter event}
@item{@scheme['menu] --- for @scheme[selectable-menu-item<%>] callbacks}
@item{@scheme['slider] --- for @scheme[slider%] changes}
@item{@scheme['radio-box] --- for @scheme[radio-box%] selection changes}
@item{@scheme['menu-popdown] --- for @scheme[popup-menu%] callbacks (item selected)}
@item{@scheme['menu-popdown-none] --- for @scheme[popup-menu%] callbacks (no item selected)}
@item{@scheme['tab-panel] --- for @scheme[tab-panel%] tab changes}
}

This value is extracted out of a @scheme[control-event%] object with
 the
@method[control-event% get-event-type] method.

See @method[event% get-time-stamp] for information about
@scheme[time-stamp].

}

@defmethod[(get-event-type)
           (one-of/c 'button 'check-box 'choice
                     'list-box 'list-box-dclick 'text-field 
                     'text-field-enter 'slider 'radio-box 
                     'menu-popdown 'menu-popdown-none 'tab-panel)]{
Returns the type of the control event. See
@scheme[control-event%] for information about each event type symbol.

}

@defmethod[(set-event-type
            [type (one-of/c 'button 'check-box 'choice
                            'list-box 'list-box-dclick 'text-field 
                            'text-field-enter 'slider 'radio-box 
                            'menu-popdown 'menu-popdown-none 'tab-panel)])
           void?]{

Sets the type of the event. See
@scheme[control-event%] for information about each event type symbol.

}}
