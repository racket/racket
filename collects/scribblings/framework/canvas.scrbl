#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Canvas}

@definterface[canvas:basic<%> (editor-canvas%)]{

}
@defmixin[canvas:basic-mixin (editor-canvas%) (canvas:basic<%>)]{
}
@definterface[canvas:color<%> (canvas:basic<%>)]{
  @index{background color}
  Mixins that implement this interface initialize the
  background color of the canvas to the value of the
  @index{'framework:basic-canvas-background}
  @racket['framework:basic-canvas-background] preference.
  Adds a callback so that when that preference is modified,
  the background color changes.

}
@defmixin[canvas:color-mixin (canvas:basic<%>) (canvas:color<%>)]{
}
@definterface[canvas:delegate<%> (canvas:basic<%>)]{
  This class is part of the delegate window implementation. 
}
@defmixin[canvas:delegate-mixin (canvas:basic<%>) (canvas:delegate<%>)]{
  Provides an implementation of
  @racket[canvas:delegate<%>].
  @defmethod*[#:mode override (((on-superwindow-show (shown? boolean?)) void?))]{

    Notifies the delegate window when the original window is
    visible. When invisible, the blue highlighting is erased.
  }
}
@definterface[canvas:info<%> (canvas:basic<%>)]{

}
@defmixin[canvas:info-mixin (canvas:basic<%>) (canvas:info<%>)]{

  @defmethod*[#:mode override (((on-focus) void?))]{

    sets the canvas that the frame displays info about.
  }
  @defmethod*[#:mode override (((set-editor) void?))]{

    Calls 
    @method[frame:info<%> update-info]
    to update the frame's info panel.
  }
}
@definterface[canvas:wide-snip<%> (canvas:basic<%>)]{
  Any
  @racket[canvas%]
  that matches this interface will automatically
  resize selected snips when its size changes. Use
  @method[canvas:wide-snip<%> add-tall-snip]
  and
  @method[canvas:wide-snip<%> add-wide-snip]
  to specify which snips should be resized.
  @defmethod*[(((recalc-snips) void?))]{
    Recalculates the sizes of the wide snips.

  }
  @defmethod*[(((add-wide-snip (snip (is-a?/c snip%))) void?))]{
    Snips passed to this method will be resized when the canvas's size
    changes. Their width will be set so they take up all of the space
    from their lefts to the right edge of the canvas.
  }
  @defmethod*[(((add-tall-snip (snip (is-a?/c snip%))) void?))]{
    Snips passed to this method will be resized when the canvas's size
    changes. Their height will be set so they take up all of the space
    from their tops to the bottom of the canvas.
  }
}
@defmixin[canvas:wide-snip-mixin (canvas:basic<%>) (canvas:wide-snip<%>)]{
  This canvas maintains a list of wide and tall snips and adjusts their
  heights and widths when the canvas's size changes.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.
  @defmethod*[#:mode override (((on-size (width (integer-in 0 10000)) (height (integer-in 0 10000))) void?))]{

    Adjusts the sizes of the marked snips.

    See
    @method[canvas:wide-snip<%> add-wide-snip]
    and 
    @method[canvas:wide-snip<%> add-tall-snip].

  }
}
@defclass[canvas:basic% (canvas:basic-mixin editor-canvas%) ()]{}
@defclass[canvas:color% (canvas:color-mixin canvas:basic%) ()]{}
@defclass[canvas:info% (canvas:info-mixin canvas:basic%) ()]{}
@defclass[canvas:delegate% (canvas:delegate-mixin canvas:basic%) ()]{}
@defclass[canvas:wide-snip% (canvas:wide-snip-mixin canvas:basic%) ()]{}

@(include-previously-extracted "main-extracts.rkt" #rx"^canvas:")
