#lang scribble/manual

@(require scribble/eval
          (for-label images/compile-time
                     images/icons/control
                     images/icons/style
                     images/logos
                     racket racket/draw))

@(define ctime-eval (make-base-eval))
@interaction-eval[#:eval ctime-eval (require (for-syntax racket/base))]

@(define (author-email) "neil.toronto@gmail.com")

@title{Embedding Bitmaps in Compiled Files}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[images/compile-time]

Producing computed bitmaps can take time.
To reduce the startup time of programs that use computed bitmaps, use the macros exported by @racketmodname[images/compile-time] to @italic{compile} them: to embed the computed bitmaps in fully expanded, compiled modules.

@margin-note*{This is a form of constant folding, or equivalently a form of @italic{safe} ``3D'' values.}
The macros defined here compute bitmaps at expansion time, and expand to the bitmap's @racket[bytes] and a simple wrapper that converts @racket[bytes] to a @racket[bitmap%].
Thus, fully expanded, compiled modules contain (more or less) literal bitmap values, which do not need to be computed again when the module is @racket[require]d by another.

The literal bitmap values are encoded in @link["http://en.wikipedia.org/wiki/Portable_Network_Graphics"]{PNG} or @link["http://en.wikipedia.org/wiki/JPEG"]{JPEG} format, so they are compressed in the compiled module.

To get the most from compiled bitmaps during development, it is best to put them in files that are changed infrequently.
For example, for games, we suggest having a separate module called something like @tt{images.rkt} or @tt{resources.rkt} that @racket[provide]s all the game's images.

@defform[(compiled-bitmap expr [quality])
         #:contracts ([expr (is-a?/c bitmap%)]
                      [quality (integer-in 0 100)])]{
Evaluates @racket[expr] at expansion time, which must return a @racket[bitmap%], and returns to the bitmap at run time.
Keep in mind that @racket[expr] has access only to expansion-time values, not run-time values.

If @racket[quality] is @racket[100], the bitmap is stored as a PNG.
If @racket[quality] is between @racket[0] and @racket[99] inclusive, it is stored as a JPEG with quality @racket[quality].
(See @method[bitmap% save-file].)
If the bitmap has an alpha channel, its alpha channel is stored as a separate JPEG.
The default value is @racket[100].

Generally, to use this macro, wrap a @racket[bitmap%]-producing expression with it and move any identifiers it depends on into the expansion phase.
For example, suppose we are computing a large PLT logo at run time:
@codeblock|{#lang racket}|
@racketblock+eval[#:eval ctime-eval
(require images/logos)

(define the-logo (plt-logo #:height 384))
]
Running this takes several seconds. It produces
@interaction[#:eval ctime-eval the-logo]

To move the cost to expansion time, we change the program to
@codeblock|{
#lang racket

(require images/compile-time
         (for-syntax images/logos))
         
(define the-logo (compiled-bitmap (plt-logo #:height 384)))
}|
The logo is unchanged, but now @italic{expanding} (and thus compiling) the program takes several seconds, and running it takes a few milliseconds.
Note that @racketmodname[images/logos] is now required @racket[for-syntax], so that the expansion-phase expression @racket[(plt-logo #:height 384)]
has access to the identifier @racket[plt-logo].
}

@defform[(compiled-bitmap-list expr [quality])
         #:contracts ([expr (listof (is-a?/c bitmap%))]
                      [quality (integer-in 0 100)])]{
Like @racket[compiled-bitmap], but it expects @racket[expr] to return a @racket[list] of @racket[bitmap%]s, and it returns the list at run time.
The @racket[quality] argument works as in @racket[compiled-bitmap], but is applied to all the images in the list.

Use this for animations. For example,
@codeblock|{#lang racket}|
@racketblock+eval[#:eval ctime-eval
(require images/compile-time
         (for-syntax images/icons/stickman))

(begin-for-syntax
  (define num-stickman-frames 12))

(define running-stickman-frames
  (compiled-bitmap-list
   (for/list ([t  (in-range 0 1 (/ 1 num-stickman-frames))])
     (running-stickman-icon t #:height 32
                            #:body-color "red"
                            #:arm-color "white"
                            #:head-color "red"))
   50))
]
This computes
@interaction[#:eval ctime-eval running-stickman-frames]
at expansion time.
}
