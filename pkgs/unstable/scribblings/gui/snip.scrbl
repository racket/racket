#lang scribble/manual

@(require "../utils.rkt"
          (for-label racket/base
                     racket/class
                     racket/gui/base
                     unstable/gui/snip
                     racket/contract))

@title[#:tag "snip"]{Snip Utilities}
@unstable[@author+email["Neil Toronto" "neil.toronto@gmail.com"]]

@defmodule[unstable/gui/snip]

@defclass[snip-canvas% editor-canvas% ()]{
A canvas that contains a single snip.

Snips cannot be placed directly on dialogs, frames and panels.
To use an interactive snip in a GUI,
it must be inserted into an editor, which itself must be placed on a special canvas, which can be placed in a GUI container.
To provide a seamless user experience, the editor should be enabled but not writable,
not be able to receive focus, not have scrollbars, and other small details.

The @racket[snip-canvas%] class handles these details, making it easy to use interactive snips as normal GUI elements.

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [make-snip ((integer-in 0 10000) (integer-in 0 10000) . -> . snip%)]
                 [style (listof (one-of/c 'no-border 'control-border 'combo
                                          'resize-corner 'no-focus 'deleted
                                          'transparent)) null]
                 [label (or/c label-string? false/c) #f]
                 [horizontal-inset (integer-in 0 1000) 5]
                 [vertical-inset (integer-in 0 1000) 5]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [min-width (integer-in 0 10000) 0]
                 [min-height (integer-in 0 10000) 0]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{
Unlike instances of @racket[editor-canvas%], each instance of this class creates and manages its own editor.
The editor contains one object: a @racket[snip%] instance created by @racket[make-snip].

The @racket[make-snip] function receives the requested width and height of the snip, which are calculated from the size of the snip canvas.
It is called the first time the snip canvas is resized, which most likely coincides with the first time the snip canvas is shown.
The snip is thus created @italic{lazily}: only when needed, at the size needed.
See @method[snip-canvas% on-size] for more details and an example.

The @racket[style] list is prepended with @racket['no-hscroll] and @racket['no-vscroll] before being passed to the @racket[editor-canvas%] constructor.
The other constructor arguments are passed untouched.
}

@defmethod[(get-snip) (or/c (is-a?/c snip%) #f)]{
Returns the wrapped snip, or @racket[#f] if @racket[make-snip] has not been called yet.
}

@defmethod[#:mode override 
           (on-size [width (integer-in 0 10000)]
                    [height (integer-in 0 10000)])
           void?]{
This is called when the snip canvas is resized.

On the first call, @method[snip-canvas% on-size] calls @racket[make-snip] with width and height arguments
respectively @racket[(max 0 (- width (* 2 horizontal-inset)))] and @racket[(max 0 (- height (* 2 vertical-inset)))].
It then inserts the resulting snip into its editor.

On subsequent calls, @method[snip-canvas% on-size] calls the snip's @method[snip% resize] method, calculating the width and height arguments the same way.

When a @racket[snip-canvas%] instance is intended to wrap an existing @racket[snip%] instance, @racket[make-snip] should simply resize it and return it.

Example: functions from @racketmodname[plot #:indirect]
create snips and call a function similar to the following to place plots in a frame:
@racketblock[
(define (make-snip-frame snip w h label)
  (define (make-snip width height)
    (send snip resize width height)
    snip)
  
  (define frame
    (new frame%
         [label label]
         [width (+ 5 5 5 5 w)]
         [height (+ 5 5 5 5 h)]))
  
  (new snip-canvas%
       [parent frame]
       [make-snip make-snip]
       [horiz-margin 5] [vert-margin 5]
       [horizontal-inset 5] [vertical-inset 5])
  
  frame)]
}

} @; defclass snip-canvas%
