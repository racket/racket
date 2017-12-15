#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@(define the-eval (make-sp-eval))

@title[#:tag "state"]{Unwindable State}

@declare-exporting[syntax/parse]

@deftogether[[
@defproc[(syntax-parse-state-ref [key any/c]
                                 [default default/c (lambda () (error ....))])
         any/c]
@defproc[(syntax-parse-state-set! [key any/c] [value any/c]) void?]
@defproc[(syntax-parse-state-update! [key any/c]
                                     [update (-> any/c any/c)]
                                     [default default/c (lambda () (error ....))])
         void?]
@defproc[(syntax-parse-state-cons! [key any/c]
                                   [value any/c]
                                   [default default/c null])
         void?]
]]{

Get or update the current @racket[syntax-parse] state. Updates to the
state are unwound when @racket[syntax-parse] backtracks. Keys are
compared using @racket[eq?].

The state can be updated only within @racket[~do] patterns (or
@racket[#:do] blocks). In addition, @racket[syntax-parse]
automatically adds identifiers that match literals (from
@racket[~literal] patterns and literals declared with
@racket[#:literals], but not from @racket[~datum] or
@racket[#:datum-literals]) under the key @racket['literals].

@examples[#:eval the-eval
(define-syntax-class cond-clause
  #:literals (=> else)
  (pattern [test:expr => ~! answer:expr ...])
  (pattern [else answer:expr ...])
  (pattern [test:expr answer:expr ...]))
(syntax-parse #'(cond [A => B] [else C])
  [(_ c:cond-clause ...) (syntax-parse-state-ref 'literals null)])
]

@history[#:added "6.11.0.4"]
}

@(close-eval the-eval)
