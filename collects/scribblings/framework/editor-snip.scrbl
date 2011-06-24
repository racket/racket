#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title[#:tag "editor-snip"]{Editor Snip}

@definterface[editor-snip:decorated<%> (editor-snip%)]{
  @defmethod[(get-corner-bitmap) (or/c false/c (is-a?/c bitmap%))]{
    Returns a bitmap that is drawn in the upper-right corner of this snip.
  }
  @defmethod[(get-color) (or/c string? (is-a?/c color%))]{
    Returns the color used to draw the background part of the snip.
  }
  @defmethod[(get-menu) (or/c false/c (is-a?/c popup-menu%))]{
    Returns a popup menu that is used when clicking on the top part of the snip.
  }
  @defmethod[(get-position) (symbols 'top-right 'left-top)]{
    Returns the location of the image and the clickable
     region. The symbol @racket['top-right] indicates top portion is clickable
     and icon on right. The symbol @racket['left-top] means left portion is
     clickable and icon on top.
  }
  @defmethod[(reset-min-sizes) void?]{
    Sets the minimum sizes based on the result of 
    @method[editor-snip:decorated<%> get-corner-bitmap].
  }
}


@defmixin[editor-snip:decorated-mixin (editor-snip%) (editor-snip:decorated<%>)]{
  @defmethod[(get-corner-bitmap) (or/c false/c (is-a?/c bitmap%))]{
    Returns @racket[#f].
  }
  @defmethod[(get-color) (or/c string? (is-a?/c color%))]{
     Returns @racketblock[
       (if (preferences:get 'framework:white-on-black?)
           "white" 
           "black")]
  }
  @defmethod[(get-menu) (or/c false/c (is-a?/c popup-menu%))]{
    Returns @racket[#f].
 }
  @defmethod[(get-position) (symbols 'top-right 'left-top)]{
    Returns @racket['top-right].
  }
}

@defclass[editor-snip:decorated% (editor-snip:decorated-mixin editor-snip%) ()]{
  @defconstructor/auto-super[()]{
     Invokes the super constructor with the keyword @racket[editor] as a call to
     @method[editor-snip:decorated% make-editor].
  }

  @defmethod[(make-snip) (is-a?/c editor-snip:decorated%)]{
    This method should return an instance of the class it is invoked in.
    If you create a subclass of this class, be sure to override this method and
    have it create instances of the subclass.
  }
  @defmethod[(make-editor) (is-a?/c editor<%>)]{
   Creates an editor to be used in this snip.
 }

  @defmethod[(copy) (is-a?/c editor-snip:decorated%)]{
    Uses the @method[editor-snip:decorated% make-editor] and
    @method[editor-snip:decorated% make-snip] methods to create a
    copy of this snip, as follows:
    @racketmod[
      (let ([snip (make-snip)])
        (send snip set-editor (send (get-editor) copy-self))
        (send snip set-style (get-style))
        snip)]
  }
}

@defclass[editor-snip:decorated-snipclass% snip-class% ()]{
  @defmethod[(make-snip [stream-in (is-a?/c editor-stream-in%)]) (is-a?/c editor-snip:decorated<%>)]{
   Returns an instance of @racket[editor-snip:decorated%].
  }
  @defmethod[(read [stream-in (is-a?/c editor-stream-in%)]) (is-a?/c editor-snip:decorated<%>)]{
   Calls @method[editor-snip:decorated-snipclass% make-snip] to get an object and
   then invokes its @racket[editor<%>]'s @method[editor<%> read-from-file] method
   in order to read a snip from @racket[stream-in], eg:
   @racketblock[
      (let ([snip (make-snip stream-in)])
        (send (send snip get-editor) read-from-file stream-in #f)
        snip)
   ]
  }
}
