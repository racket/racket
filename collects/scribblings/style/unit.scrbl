#lang scribble/base

@(require "shared.rkt")

@title{Units of Code}

@; -----------------------------------------------------------------------------
@section{Size Matters}

Keep units of code small. Keep modules, classes, functions and methods small.

A module of 10,000 lines of code is too large. A module of 1,000 lines is
 tolerable. A module of 500 lines of code has the right size.

One module should usually a class and its auxiliary functions, which in
 turn determines the length of a good-sized class.

And a function (method) of more than 66 lines is barely acceptable. For
 many years we had a limited syntax transformation language that forced
 people to create @emph{huge} functions. This is no longer the case, so
 consider this rule universal.

If a unit of code looks incomprehensible, it is probably too large. Break
 it up. To bring across what the pieces compute, implement or serve, use
 meaningful names; see @secref{names}.  If you can't come up with a good
 name for such pieces, you are probably looking at the wrong kind of
 division; consider alternatives.

@; -----------------------------------------------------------------------------
@section{Module Interfaces}

The purpose of a module is to provide some services.

@centerline{Equip a module with a short purpose statement.}
@;
Often ``short'' means one line; occasionally you may need several lines.

A module's interface describes the services it provides; its body
 implements these services. Others have to read the interface if the
 external documentation doesn't suffice:

@centerline{Place the interface at the top of the module.}
@;
This helps people find the relevant information quickly.

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
  ;; Stgy = State -> Action

  ;; Stgy
  ;; people's strategy
  human-strategy

  ;; Stgy
  ;; complete tree traversal
  ai-strategy)

 (define (general p)
   ... )

 ... some 100 lines ...
 (define human-strategy
   (general create-gui))

 ... some 100 lines ...
 (define ai-strategy
   (general traversal))))

@(begin
#reader scribble/comment-reader
(racketmod #:file
 @tt{bad}
 racket

 ;; This module implements
 ;; several game strategies.

 (require "game-basics.rkt")

 ;; Stgy = State -> Action

 (define (general p)
   ... )
 ... some 100 lines ...

 (provide
  ;; Stgy
  ;; a person's strategy
  human-strategy)

 (define human-strategy
   (general create-gui))
 ... some 100 lines ...

 (provide
  ;; Stgy
  ;; a complete tree traversal
  ai-strategy)

 (define ai-strategy
   (general traversal))
 ... some 100 lines ...
))
]

As you can see from this comparison, an interface shouldn't just
@scheme[provide] a list of names. Each identifier should come with a
purpose statement. Type-like explanations of data also belong into a
@scheme[provide] specification.

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

Consider using @scheme[provide/contract] for module interfaces.
 Although contracts affect the performance of your module's services, they
 provide a specification and they will help you with the inevitable bug
 reports you will receive. You should definitely consider type-like
 contracts (constructor predicates that check only a tag); they tend to
 cost relatively little.

Finally, a module consists of sections. It is good practice to separate the
 sections with comment lines. You may want to write down purpose statements
 for sections so that readers can easily understand which part of a module
 implements which service. Alternatively, consider using the large letter
 chapter headings in DrRacket to label the sections of a module.

@; -----------------------------------------------------------------------------
@section{Classes & Units}

@; -----------------------------------------------------------------------------
@section{Functions & Methods}


