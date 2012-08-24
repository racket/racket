#lang scribble/doc
@(require (for-label racket/math
                     racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          racket/sandbox
          racket/math)

@(define math-eval 
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket #:requires '(racket/math))))
@;(interaction-eval #:eval math-eval (require racket/math))

@title[#:tag "number-theory" #:style '(toc)]{Number Theory}

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "congruences"]{Congruences and Modular Arithmetic}



@defproc[(divides? [m Integer] [n Integer]) boolean?]{
   Returns @racket[#t] if @racket[m] divides @racket[n],
   @racket[#f] otherwise.

   Note: That a non-zero integer @racket[m] divides an integer @racket[n]
   means there exists an integer @racket[k] such that m*k=n.

   Test whether 2 divides 9:
   @interaction[(divides? 2 9)]
}
