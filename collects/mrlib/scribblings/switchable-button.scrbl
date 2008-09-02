#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/switchable-button))

@title{Switchable Button}

@defmodule[mrlib/switchable-button]

@defclass[switchable-button% canvas% ()]{

A @scheme[switchable-button%] control displays
and icon and a string label. It toggles between
display of just the icon and a display with the 
label and the icon side-by-side. 

@defconstructor/auto-super[([label string?]
		            [callback (-> (is-a?/c switchable-button%) any/c)]
			    [bitmap (is-a?/c bitmap%)]
			    [alternate-bitmap (is-a?/c bitmap%) bitmap]	
			    [vertical-tight? boolean? #f])]{
The @scheme[callback] is called when the button
is pressed. The @scheme[string] and @scheme[bitmap] are
used as discussed above.

If @scheme[alternate-bitmap] is supplied, then it is used
when the button is switched to the view that just shows the bitmap.
If it is not supplied, both modes show the same bitmap.

If the @scheme[vertical-tight?] argument is @scheme[#t], then the button takes up
as little as possible vertical space.
}

@defmethod[(set-label-visible [visible? boolean?]) void?]{
  Sets the visibility of the string part of the label.
}

@defmethod[(command) void?]{

Calls the button's callback function.}

}

