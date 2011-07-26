#lang scribble/base

@(require "shared.rkt")

@title{Units of Code}

@; -----------------------------------------------------------------------------
@section{Module Interfaces}

The purpose of a module is to provide some services. @margin-note{The
modules we discuss in this section coincide with files.}

@centerline{Equip a module with a short purpose statement.}

Its interface describes which services it provides, and its body implements
 the services. At least in principle others shouldn't have to read the
 implementation ever, but it is quite likely that they have to read the
 interface.

@centerline{Place the interface at the top of the module.}

@compare[
@;%
@(begin
#reader scribble/comment-reader
(racketmod #:file
 @tt{good}
 racket

 ;; This module implements
 ;; several game strategies.

 (require "game-basics.rkt")

 (provide
  ;; Strtgy = GameState -> Action

  ;; Strtgy
  ;; a person's strategy
  human-strategy

  ;; Strtgy
  ;; a complete tree traversal
  ai-1-strategy

  ;; Strtgy
  ;; alpha-beta pruning traversal
  ai-2-strategy)

;; ------------------------------------------------------------------
 (define (general-strategy p)
   ... )

;; ------------------------------------------------------------------
 ... some 100 lines ...
 (define human-strategy
   (general-strategy create-gui))

;; ------------------------------------------------------------------
 ... some 100 lines ...
 (define ai-1-strategy
   (general-strategy traversal))

;; ------------------------------------------------------------------
 ... some 100 lines ...
 (define ai-2-strategy
   (general-strategy alpha-beta))))
@(begin
#reader scribble/comment-reader
(racketmod #:file
 @tt{bad}
 racket

 ;; This module implements
 ;; several game strategies.

 (require "game-basics.rkt")

 ;; Strtgy = GameState -> Action

;; ------------------------------------------------------------------
 (define (general-strategy p)
   ... )
 ... some 100 lines ...

;; ------------------------------------------------------------------
 (provide
  ;; Strtgy
  ;; a person's strategy
  human-strategy)

 (define human-strategy
   (general-strategy create-gui))
 ... some 100 lines ...

;; ------------------------------------------------------------------
 (provide
  ;; Strtgy
  ;; a complete tree traversal
  ai-1-strategy)

 (define ai-1-strategy
   (general-strategy traversal))
 ... some 100 lines ...

;; ------------------------------------------------------------------
 (provide
  ;; Strtgy
  ;; alpha-beta pruning traversal
  ai-2-strategy)

 (define ai-2-strategy
   (general-strategy alpha-beta))))
]

As you can see from this comparison, an interface shouldn't just be a
@scheme[provide] with a list of names. Each identifier should come with a
purpose statement.

@bold{Note} Following this documentation guideline is most applicable to
modules that are a component of a large application. It is less relevant
for a module in the Racket that comes with a full-fledged description in
the guide.

While a one-line purpose statement for a function is usually enough, syntax
should come with a description of the grammar clause it introduces
@emph{and} its meaning.

@codebox[
@(begin
#reader scribble/comment-reader
(racketmod #:file
@tt{good}
racket

(provide
 ;; (define-strategy (s:id a:id b:id c:id d:id)
 ;;   action:definition-or-expression)
 ;;
 ;; (define-strategy (s board tiles available score) ...)
 ;; defines a function from an instance of player to a placement
 ;; The four identifier denote the state of the board,
 ;; the player's hand, the places where a tile can be
 ;; placed, and the player's current score.
 define-strategy)
))]

If the performance of your module doesn't suffer too much from contracts,
 consider using @scheme[provide/contract]. The use of type-like contracts
 (constructor predicates that check only a tag) impose a tolerable overhead
 and still discover simple mistakes.

Finally, a module consists of sections. It is good practice to separate the
 sections with comment lines. You may want to write down purpose statements
 for sections so that readers can easily understand which part of a module
 implements which service. Alternatively, consider using the large letter
 chapter headings in DrRacket to label the sections of a module.

@; -----------------------------------------------------------------------------
@section{Functions & Methods, Classes & Units}

@; -----------------------------------------------------------------------------
@section{Size Matters}

Keep functions small. Keep classes small. Keep units small. Keep modules small.

Anytime a unit of code looks incomprehensible, it is probably too
large. Break it up into smaller units. To bring across what these smaller
units compute, implement or serve, use meaningful names; see
@secref{names}.  Conversely, if you can't come up with a good name for such
units, you are probably looking at the wrong kind of division; consider
alternatives.
