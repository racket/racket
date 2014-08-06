#lang scribble/doc
@(require 
   scribble/manual
   (for-label
    pict/snip
    pict
    racket
    racket/snip
    racket/contract))


@title{Pict Snip: Build Snips from Picts}

@defmodule[pict/snip]{ 
  The @racketmodname[pict/snip] library
  constructs @racket[snip%] instances that
  draw based on a given @racket[pict?] object.
}

@defclass[pict-snip% snip% ()]{
  @defconstructor[([pict pict?])]{
    Creates a @racket[pict-snip%] object, using
    @racket[pict] to draw.
  }
  @defmethod[(get-pict) pict?]{
    Returns the pict passed to the constructor.
  }
  
  @defmethod[(get-extent [dc (is-a?/c dc<%>)] [x real?] [y real?]
                         [w (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [h (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [descent (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [lspace (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [rspace (or/c (box/c (and/c real? (not/c negative?))) #f)])
             void?]{
    Updates the arguments based on the 
    size of the pict returned from @racket[get-pict].
  }

  @defmethod[#:mode 
             override
             (draw [dc (is-a?/c dc<%>)] [x real?] [y real?]
                   [left real?]
                   [top real?]
                   [right real?]
                   [bottom real?]
                   [dx real?]
                   [dy real?]
                   [draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret
                                     (cons/c exact-nonnegative-integer?
                                             exact-nonnegative-integer?))])
             void?]{
    Draws the pict returned from @racket[get-pict].
  }
  
  @defmethod[(write [f (is-a?/c editor-stream-out%)]) void?]{
    Uses a @racket[record-dc%] to write the way that the
           result of @racket[get-pict] draws and saves that
           to @racket[f].
  }
                                                                            
  @defmethod[(copy) (is-a?/c pict-snip%)]{
    Returns a snip that has the same pict as @racket[this] one.
 }
  
}

@defthing[snipclass (is-a?/c snip-class%)]{
  The @racket[snip-class%] instance used by
      instances of @racket[pict-snip%].
}
@defthing[reader (is-a?/c snip-reader<%>)]{
  The @racket[snip-reader<%>] instance used
      by the @racketmodname[wxme] library.
}
