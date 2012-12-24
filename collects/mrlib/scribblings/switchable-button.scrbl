#lang scribble/doc
@(require "common.rkt" (for-label mrlib/switchable-button framework))

@title{Switchable Button}

@defmodule[mrlib/switchable-button]

@defclass[switchable-button% canvas% ()]{
  
  A @racket[switchable-button%] control displays
  and icon and a string label. It toggles between
  display of just the icon and a display with the 
  label and the icon side-by-side. 
  
  The @racket[panel:discrete-sizes-mixin] explicitly
  uses @racket[switchable-button%]s via their
  @method[switchable-button% get-small-width],
  @method[switchable-button% get-large-width], and
  @method[switchable-button% get-without-label-small-width] methods.
  See @racket[panel:discrete-sizes-mixin] for more details.
  
  @defconstructor/auto-super[([label (or/c string? (is-a?/c bitmap%) #f)]
                              [bitmap (is-a?/c bitmap%)]
                              [callback (-> (is-a?/c switchable-button%) any/c)]
                              [alternate-bitmap (is-a?/c bitmap%) bitmap]
                              [vertical-tight? boolean? #f]
                              [min-width-includes-label? boolean? #f])]{
    The @racket[callback] is called when the button
    is pressed. The @racket[label] and @racket[bitmap] are
    used as discussed above.
    
    If @racket[alternate-bitmap] is supplied, then it is used
    when the label is not visible (via a call to @method[switchable-button% set-label-visible]).
    If it is not supplied, both modes show @racket[bitmap].
    
    If the @racket[vertical-tight?] argument is @racket[#t], then the button takes up
    as little as possible vertical space.
    
    If the @racket[min-width-includes-label?] is @racket[#t], then the minimum
    width includes both the bitmap and the label. Otherwise, it includes
    only the bitmap.
  }
  
  @defmethod[(set-label-visible [visible? boolean?]) void?]{
    Sets the visibility of the string part of the label.
  }
  
  @defmethod[(command) void?]{
    Calls the button's callback function.
  }
  
  @defmethod[(get-button-label) string?]{
    Returns the label of this button.
  }
  
  @defmethod[(get-large-width) exact-nonnegative-integer?]{
     Returns the width of the button when it would show both
     the label and the bitmap and when it is in label-visible
     mode (i.e., when @racket[set-label-visible] has been called
     with @racket[#t]).
  }

  @defmethod[(get-small-width) exact-nonnegative-integer?]{
     Returns the width of the button when it would show both
     just the bitmap (not the alternate bitmap),
     and when it is in label-visible
     mode (i.e., when @racket[set-label-visible] has been called
     with @racket[#t]).
  }
  
  @defmethod[(get-without-label-small-width) exact-nonnegative-integer?]{
     Returns the width of the button when
     it is not in label-visible
     mode (i.e., when @racket[set-label-visible] has been called
     with @racket[#f]).
  }
}
