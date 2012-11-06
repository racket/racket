#lang scribble/manual

@(require scribble/eval
          unstable/latent-contract/defthing
          (for-label images/logos
                     racket racket/draw)
          images/logos)

@(define (author-email) "neil.toronto@gmail.com")

@title{Logos}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[images/logos]

@(define logos-eval (make-base-eval))
@interaction-eval[#:eval logos-eval (require images/logos images/icons/style)]

@doc-apply[plt-logo]{
Returns the PLT logo, rendered in tinted glass and azure metal by the ray tracer that renders icons.
@examples[#:eval logos-eval (plt-logo)]
The default height is the size used for DrRacket splash screen.
}

@doc-apply[planet-logo]{
Returns an unofficial PLaneT logo. This is used as the PLaneT icon when DrRacket downloads PLaneT packages.
@examples[#:eval logos-eval
                 (planet-logo)
                 (planet-logo #:height (default-icon-height))]
}

@doc-apply[stepper-logo]{
Returns the algebraic stepper logo.
@examples[#:eval logos-eval (stepper-logo)]
}

@doc-apply[macro-stepper-logo]{
Returns the macro stepper logo.
@examples[#:eval logos-eval (macro-stepper-logo)]
}


@close-eval[logos-eval]
