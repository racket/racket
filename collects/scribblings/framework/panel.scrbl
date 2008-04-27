#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework/framework))
@(require (for-label scheme/gui))
@title{Panel}

@definterface[panel:single<%> (area-container<%>)]{
  See 
  @scheme[panel:single-mixin%].
  @defmethod*[(((active-child (child (is-a?/c area<%>))) void) ((active-child) (is-a?/c area<%>)))]{

    Sets the active child to be \var{child}


    Returns the current active child.
  }
}
@defmixin[panel:single-mixin (area-container<%>) (panel:single<%>)]{
  This mixin adds single panel functionality to an implementation of the 
  \iscmintf{area-container} interface.

  Single panels place all of the children in the center of the panel,
  and allow make one child to be visible at a time. The 
  @method[panel:single<%> active-child]
  method controls which panel is currently active.

  The 
  @method[window<%> show]
  method is used to hide and show the children of a single panel.
  @defmethod*[#:mode override (((after-new-child (child subarea<%>)) void))]{

    Hides this child by calling \scmline{(send child show \#f)}, unless
    this is the first child in which case it does nothing.
  }
  @defmethod*[#:mode override (((container-size) (values exact-integer exact-integer)))]{

    Returns the maximum width of all the children and the maximum height
    of all of the children.
  }
  @defmethod*[#:mode override (((place-children) (listof (list exact-integer exact-integer exact-integer exact-integer))))]{

    Returns the positions for single panels and panes.
  }
}
@definterface[panel:single-window<%> (panel:single<%> window<%>)]{

}
@defmixin[panel:single-window-mixin (panel:single<%> window<%>) (panel:single-window<%>)]{

  @defmethod*[#:mode override (((container-size (info (list-of (list exact-integer exact-integer boolean boolean)))) (values exact-integer exact-integer)))]{

    Factors the border width into the size calculation.
  }
}
@defclass[panel:single% (panel:single-window-mixin (panel:single-mixin panel%)) ()]{}
@defclass[panel:single-pane% (panel:single-mixin pane%) ()]{}
@definterface[panel:dragable<%> (window<%> area-container<%>)]{
  Classes matching this interface implement a panel where the
  user can adjust the percentage of the space that each takes
  up. The user adjusts the size by clicking and dragging the
  empty space between the children.
  @defmethod*[(((after-percentage-change) void))]{
    This method is called when the user changes the percentage
    by dragging the bar between the children, or when a new
    child is added to the frame, but not when
    @method[panel:dragable<%> set-percentages]
    is called.

    Use
    @method[panel:dragable<%> get-percentages]
    to find the current percentages.


  }
  @defmethod*[(((set-percentages (new-percentages (listof number))) void))]{
    Call this method to set the percentages that each window
    takes up of the panel.


    The argument, \var{new-percentages} must be a list of
    numbers that sums to 1. It's length must be equal to the
    number of children of the panel (see
    @method[area-container<%> get-children]) and each percentage must correspond to a number of pixels
    that is equal to or larger than the 
    minimum with of the child, as reported by
    @method[area<%> min-width].
  }
  @defmethod*[(((get-percentages) (listof numbers)))]{
    Return the current percentages of the children.

  }
  @defmethod*[(((get-vertical?) boolean))]{
    This method controls the behavior of the other overridden
    methods in mixins that implement this interface.

    If it returns \scheme|#t|, the panel will be vertically
    aligned and if it returns \scheme|#f|, they will be
    horizontally aligned.

  }
}
@definterface[panel:vertical-dragable<%> (panel:dragable<%>)]{
  A panel that implements
  @scheme[panel:vertical-dragable<%>]. It aligns its children vertically.
}
@definterface[panel:horizontal-dragable<%> (panel:dragable<%>)]{
  A panel that implements
  @scheme[panel:horizontal-dragable<%>]. It aligns its children horizontally.
}
@defmixin[panel:dragable-mixin (window<%> area-container<%>) (panel:dragable<%>)]{
  This mixin adds the 
  @scheme[panel:dragable<%>]
  functionality to a
  @scheme[panel%].

  It is not useful unless the 
  @method[panel:dragable<%> get-vertical?]
  method is overridden.


  @defmethod*[#:mode override (((after-new-child (child (instance-of (is-a?/c area<%>)))) void))]{

    Updates the number of percentages to make sure that it
    matches the number of children and calls
    @method[panel:dragable<%> after-percentage-change].
  }
  @defmethod*[#:mode override (((on-subwindow-event (receiver (instanceof window<%>)) (event (instanceof mouse-event%))) boolean))]{

    When the cursor is dragging the middle bar around, this
    method handles the resizing of the two panes.
  }
  @defmethod*[#:mode override (((place-children (info (list-of (list exact-int exact-int))) (w exact-int) (h exact-int)) (list-of (list exact-int exact-int exact-int exact-int))))]{

    Places the children vertically in the panel, based on the percentages
    returned from
    @method[panel:dragable<%> get-percentages]. Also leaves a little gap between each pair of children.
  }
  @defmethod*[#:mode override (((container-size (info list)) two))]{

    Computes the minimum size the panel would have to be in
    order to have the current percentages (see 
    @method[panel:dragable<%> get-percentages]).

  }
}
@defmixin[panel:vertical-dragable-mixin (panel:dragable<%>) (panel:vertical-dragable<%>)]{
  This mixin merely overrides the
  @method[panel:dragable<%> get-vertical?]
  method of the 
  @scheme[panel:dragable-mixin]
  to return \scheme|#t|.
  @defmethod*[#:mode override (((get-vertical?) boolean))]{

    Returns \scheme|#t|.
  }
}
@defmixin[panel:horizontal-dragable-mixin (panel:dragable<%>) (panel:vertical-dragable<%>)]{
  This mixin merely overrides the
  @method[panel:dragable<%> get-vertical?]
  method of the 
  @scheme[panel:dragable-mixin]
  to return \scheme|#f|.
  @defmethod*[#:mode override (((get-vertical?) boolean))]{

    Returns \scheme|#f|.
  }
}
@defclass[panel:vertical-dragable% (panel:vertical-dragable-mixin (panel:dragable-mixin vertical-panel%)) ()]{}
@defclass[panel:horizontal-dragable% (panel:horizontal-dragable-mixin (panel:dragable-mixin horizontal-panel%)) ()]{}
@(require framework/framework-docs)
@(def-fw-procs panel)
