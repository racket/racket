#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Panel}

@definterface[panel:single<%> (area-container<%>)]{
  See @racket[panel:single-mixin%].

  @defmethod*[(((active-child (child (is-a?/c area<%>))) void?) ((active-child) (is-a?/c area<%>)))]{
    Sets the active child to be @racket[child]

    Returns the current active child.
  }
}

@defmixin[panel:single-mixin (area-container<%>) (panel:single<%>)]{
  This mixin adds single panel functionality to an implementation of the
  @racket[area-container<%>] interface.

  Single panels place all of the children in the center of the panel, and allow
  make one child to be visible at a time. The
  @method[panel:single<%> active-child] method controls which panel is
  currently active.

  The @method[window<%> show] method is used to hide and show the children of a
  single panel.

  @defmethod*[#:mode override (((after-new-child (child (is-a?/c subarea<%>))) void?))]{
    Hides this child by calling @racket[(send child show #f)], unless this is
    the first child in which case it does nothing.
  }

  @defmethod*[#:mode override (((container-size) (values exact-integer? exact-integer?)))]{
    Returns the maximum width of all the children and the maximum height of all
    of the children.
  }

  @defmethod*[#:mode override (((place-children) (listof (list/c exact-integer? exact-integer? exact-integer? exact-integer?))))]{
    Returns the positions for single panels and panes.
  }
}

@definterface[panel:single-window<%> (panel:single<%> window<%>)]{
}

@defmixin[panel:single-window-mixin (panel:single<%> window<%>) (panel:single-window<%>)]{
  @defmethod*[#:mode override 
                     (((container-size (info (listof (list/c exact-integer? 
                                                             exact-integer? 
                                                             boolean? 
                                                             boolean?))))
                       (values exact-integer? exact-integer?)))]{
    Factors the border width into the size calculation.
  }
}

@defclass[panel:single% (panel:single-window-mixin (panel:single-mixin panel%)) ()]{}
@defclass[panel:single-pane% (panel:single-mixin pane%) ()]{}

@definterface[panel:dragable<%> (window<%> area-container<%>)]{
  Classes matching this interface implement a panel where the user can adjust
  the percentage of the space that each takes up. The user adjusts the size by
  clicking and dragging the empty space between the children.

  @defmethod*[(((after-percentage-change) void?))]{
    This method is called when the user changes the percentage by dragging the
    bar between the children, or when a new child is added to the frame, but
    not when @method[panel:dragable<%> set-percentages] is called.

    Use @method[panel:dragable<%> get-percentages] to find the current
    percentages.
  }
  
  @defmethod[(get-default-percentages [subwindow-count exact-positive-integer?])
             (listof (and/c real? (between/c 0 1)))]{
     Called when the number of children in the panel changes;
     the result is used as the initial percentages for each of the new
     windows. 
     
     The numbers in the result list must sum to @racket[1].
  }

  @defmethod[(right-click-in-gap [evt (is-a?/c mouse-event%)]
                                 [before (is-a?/c subarea<%>)]
                                 [after (is-a?/c subarea<%>)])
             void?]{
    This method is called when the user right-clicks in the space
    between two children. It receives the mouse event and the
    child before and after the gap where the user clicked.
  }

                                                    
  @defmethod*[(((set-percentages (new-percentages (listof number?))) void?))]{
    Call this method to set the percentages that each window takes up of the
    panel.

    The argument, @racket[new-percentages] must be a list of numbers that sums
    to 1. It's length must be equal to the number of children of the panel (see
    @method[area-container<%> get-children]) and each percentage must
    correspond to a number of pixels that is equal to or larger than the
    minimum with of the child, as reported by @method[area<%> min-width].
  }

  @defmethod*[(((get-percentages) (listof number?)))]{
    Return the current percentages of the children.
  }

  @defmethod*[(((get-vertical?) boolean?))]{
    This method controls the behavior of the other overridden methods in mixins
    that implement this interface.

    If it returns @racket[#t], the panel will be vertically aligned and if it
    returns @racket[#f], they will be horizontally aligned.
  }

  @defmethod[(set-orientation [horizontal? boolean?]) void?]{
    Sets the orientation of the panel, switching it from behaving like a
    @racket[panel:horizontal-dragable<%>] and
    @racket[panel:vertical-dragable<%>].
  }
}

@definterface[panel:vertical-dragable<%> (panel:dragable<%>)]{
  A panel that implements @racket[panel:vertical-dragable<%>]. It aligns its
  children vertically.
}

@definterface[panel:horizontal-dragable<%> (panel:dragable<%>)]{
  A panel that implements @racket[panel:horizontal-dragable<%>]. It aligns its
  children horizontally.
}

@defmixin[panel:dragable-mixin (window<%> area-container<%>) (panel:dragable<%>)]{
  This mixin adds the @racket[panel:dragable<%>] functionality to a
  @racket[panel%].

  It is not useful unless the @method[panel:dragable<%> get-vertical?]  method
  is overridden.

  @defmethod*[#:mode override (((after-new-child (child (is-a?/c subarea<%>))) void?))]{
    Updates the number of percentages to make sure that it matches the number
    of children and calls @method[panel:dragable<%> after-percentage-change].
  }

  @defmethod*[#:mode override (((on-subwindow-event (receiver (is-a?/c window<%>)) (event (is-a?/c mouse-event%))) boolean?))]{
    When the cursor is dragging the middle bar around, this method handles the
    resizing of the two panes.
  }

  @defmethod*[#:mode override
              (((place-children (info (listof (list/c exact-integer? exact-integer?)))
                                (w exact-integer?)
                                (h exact-integer?))
                (listof (list/c exact-integer? exact-integer? exact-integer? exact-integer?))))]{
    Places the children vertically in the panel, based on the percentages
    returned from @method[panel:dragable<%> get-percentages]. Also leaves a
    little gap between each pair of children.
  }

  @defmethod*[#:mode override (((container-size (info (listof (list/c exact-integer? exact-integer? any/c any/c))))
                                (values exact-integer? exact-integer?)))]{
    Computes the minimum size the panel would have to be in order to have the
    current percentages (see @method[panel:dragable<%> get-percentages]).
  }
}

@defmixin[panel:vertical-dragable-mixin (panel:dragable<%>) (panel:vertical-dragable<%>)]{
  This mixin merely overrides the @method[panel:dragable<%> get-vertical?]
  method of the @racket[panel:dragable-mixin] to return @racket[#t].

  @defmethod*[#:mode override (((get-vertical?) boolean?))]{
    Returns @racket[#t].
  }
}
@defmixin[panel:horizontal-dragable-mixin (panel:dragable<%>) (panel:vertical-dragable<%>)]{
  This mixin merely overrides the @method[panel:dragable<%> get-vertical?]
  method of the @racket[panel:dragable-mixin] to return @racket[#f].

  @defmethod*[#:mode override (((get-vertical?) boolean?))]{
    Returns @racket[#f].
  }
}

@defclass[panel:vertical-dragable% (panel:vertical-dragable-mixin (panel:dragable-mixin panel%)) ()]{}
@defclass[panel:horizontal-dragable% (panel:horizontal-dragable-mixin (panel:dragable-mixin panel%)) ()]{}

@definterface[panel:splitter<%> ()]{
  A panel that implements @racket[panel:splitter<%>]. Children can be split
  horizonally or vertically.
}

@defmixin[panel:splitter-mixin (area-container<%> panel:dragable<%>) (panel:splitter<%>)]{
  This mixin allows panels to split their children either horizontally or
  vertically. Children that are split can be further split independant of any
  other splitting.

  @defmethod[(split-vertical (canvas (is-a?/c canvas<%>))
                             (maker (-> (is-a?/c panel:splitter<%>)
                                        (is-a?/c canvas<%>))))
                             (is-a?/c canvas<%>)]{
    Splits the @racket[canvas] vertically by creating a new instance using
    @racket[maker]. This splitter object is passed as the argument to
    @racket[maker] and should be used as the @racket[parent] field of the newly
    created canvas.
  }

  @defmethod[(split-horizontal (canvas (is-a?/c canvas<%>))
                             (maker (-> (is-a?/c panel:splitter<%>)
                                        (is-a?/c canvas<%>))))
                             (is-a?/c canvas<%>)]{
    Similar to @racket[split-vertical] but splits horizontally.
  }

  @defmethod[(collapse (canvas (is-a?/c canvas<%>))) void]{
    Removes the given @racket[canvas] from the splitter hierarchy and collapses
    any split panes as necessary.
  }

}

@(include-previously-extracted "main-extracts.rkt" #rx"^panel:")
