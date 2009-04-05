#lang scribble/doc
@(require "common.ss")

@definterface/title[control<%> (subwindow<%>)]{

The @scheme[control<%>] interface is implemented by the built-in
 control window classes:
@itemize[
@item{@scheme[message%]}
@item{@scheme[button%]}
@item{@scheme[check-box%]}
@item{@scheme[slider%]}
@item{@scheme[gauge%]}
@item{@scheme[text-field%]}
@item{@scheme[radio-box%]}
@item{@scheme[choice%]}
@item{@scheme[list-box%]}
]



@defmethod[(command [event (is-a?/c control-event%)])
           void?]{

Calls the control's callback function, passing on the given
 @scheme[control-event%] object.

}

@defmethod[(get-font)
           (is-a?/c font%)]{

Returns the font used for the control, which is optionally supplied
 when a control is created.

}}

