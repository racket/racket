#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "../common.rkt"
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

@defproc[(syntax-parse-track-literals [stx syntax?] [#:introduce? introduce? any/c #t]) syntax?]{

Add a @racket['disappeared-use] @tech[#:doc refman]{syntax property} to
@racket[stx] containing the information stored in the current
@racket[syntax-parse] state under the key @racket['literals]. If
@racket[stx] already has a @racket['disappeared-use] property, the
added information is @racket[cons]ed onto the property’s current value.

Due to the way @racket[syntax-parse] automatically adds identifiers that match
literals to the state under the key @racket['literals], as described in the
documentation for @racket[syntax-parse-state-ref],
@racket[syntax-parse-track-literals] can be used to automatically add any
identifiers used as literals to the @racket['disappeared-use] property.

If @racket[syntax-parse-track-literals] is called within the dynamic
extent of a @tech[#:doc refman]{syntax transformer} (see
@racket[syntax-transforming?]), @racket[introduce?] is not @racket[#f], and the
value in the current @racket[syntax-parse] state under the key
@racket['literals] is a list, then @racket[syntax-local-introduce] is applied to
any identifiers in the list before they are added to @racket[stx]’s
@racket['disappeared-use] property.

Most of the time, it is unnecessary to call this function directly. Instead, the
@racket[#:track-literals] option should be provided to @racket[syntax-parse],
which will automatically call @racket[syntax-parse-track-literals] on
syntax-valued results.

@examples[#:eval the-eval
(define-syntax-class cond-clause
  #:literals (=> else)
  (pattern [test:expr => ~! answer:expr ...])
  (pattern [else answer:expr ...])
  (pattern [test:expr answer:expr ...]))
(syntax-property
 (syntax-parse #'(cond [A => B] [else C])
   [(_ c:cond-clause ...) (syntax-parse-track-literals #'#f)])
 'disappeared-use)
]

@history[#:added "6.90.0.29"]}

@(close-eval the-eval)
