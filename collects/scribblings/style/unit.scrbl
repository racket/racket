#lang scribble/base

@(require "shared.rkt")

@title{Units of Code}

@; -----------------------------------------------------------------------------
@section{Organization Matters}

We often develop units of code in a bottom-up fashion with some top-down
planning. There is nothing surprising about this strategy because we build
code atop of existing libraries, which takes some experimentation, which in
turn is done in the REPL. We also want testable code quickly, meaning we
tend to write down those pieces of code first for which we can develop and
run tests. Readers don't wish to follow our development, however; they wish
to understand what the code computes without necessarily understanding all
the details.

So, please take the time to present each unit of code in a top-down
manner. This starts with the implementation part of a module. Put the
important functions close to the top, right below any code and comments as
to what kind of data you use. The rule also applies to classes, where you
want to expose @racket[public] methods before you tackle @racket[private]
methods. And the rule applies to units, too.

@; -----------------------------------------------------------------------------
@section{Size Matters}

Keep units of code small. Keep modules, classes, functions and methods small.

A module of 10,000 lines of code is too large. A module of 1,000 lines is
 tolerable. A module of 500 lines of code has the right size.

One module should usually contain a class and its auxiliary functions, which in
 turn determines the length of a good-sized class.

And a function (method) of more than roughly 66 lines is usually
 acceptable. The 66 is based on the length of a screen with small font. It
 really means "a screen length." Yes, there are exceptions where functions
 are more than 1,000 lines long and extremely readable. Nesting levels and
 nested loops may look fine to you when you write code, but readers will
 not appreciate it keeping implicit and tangled dependencies in their
 mind. It really helps the reader to separate functions (with what you may
 call manual lambda lifting) into a reasonably flat organization of units
 that fit on a (laptop) screen and explicit dependencies.

For many years we had a limited syntax transformation language that forced
 people to create @emph{huge} functions. This is no longer the case, so we
 should try to stick to the rule whenever possible.

If a unit of code looks incomprehensible, it is probably too large. Break
 it up. To bring across what the pieces compute, implement or serve, use
 meaningful names; see @secref{names}.  If you can't come up with a good
 name for such pieces, you are probably looking at the wrong kind of
 division; consider alternatives.

@; -----------------------------------------------------------------------------
@section{Modules and their Interfaces}

The purpose of a module is to provide some services:
@;
@centerline{Equip a module with a short purpose statement.}
@;
Often ``short'' means one line; occasionally you may need several lines.

In order to understand a module's services, organize the module in three
sections below the purpose statement: its imports, its exports, and its
implementation:
@;%
@(begin
#reader scribble/comment-reader
 (racketmod #:file
 @tt{good}
 racket/base

;; the module implements a tv server

(require 2htdp/universe htdp/image)

(provide
  ;; launch the tv server function
  tv-launch
  ;; set up a tv client to receive messages from the tv server
  tv-client)

(define (tv-launch)
  (universe ...))

(define (tv-client)
  (big-bang ...))
))
@;%

If you choose to use @racket[provide/contract], define auxiliary concepts
 related to the contracts between the @racket[require] and the
 @racket[provide] sections:
@;%
@(begin
#reader scribble/comment-reader
 (racketmod #:file
 @tt{good}
 racket/base

;; the module implements a tv server

(require 2htdp/universe htdp/image xml)

(define player# 3)
(define plain-board/c
  (instanceof/c (and/c admin-board%/c board%-contracts/c)))

(define placement/c
  (flat-named-contract "placement" ...))

(provide/contract
  ;; initialize the game board for the specified number of players
  [board-init        (-> player#/c plain-board/c)]
  ;; initialize a board for some players and place the specified tiles
  [create-board      (-> player#/c (listof placement/c)
                         (or/c plain-board/c string?))]
  ;; create a board from an X-expression representation
  [board-deserialize (-> xexpr? plain-board/c)])

; implementation:
(define (board-init n)
  (new board% ...))

(define (create-board n lop)
  (define board (board-init n))
  ...)

(define board%
  (class ... some 900 lines ...))
))
@;%

Avoid @racket[(provide (all-defined-out))] except for general-purpose
 libraries.

A test suite section---if located within the module---should come at the
 every end, including its specific dependencies, i.e., @racket[require]
 specifications.

@; -----------------------------------------------------------------------------
@subsection{Require}

With @racket[require] specifications at the top of the module, you let
 every reader know what is needed to understand the module. The
 @racket[require] specification nails down the external dependencies.

@; -----------------------------------------------------------------------------
@subsection{Provide}

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
purpose statement. Type-like explanations of data may also show up in a
@scheme[provide] specification so that readers understand what kind of data
your public functions work on.

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

@subsection{Uniformity of Interface}

Pick a consistency rule for naming your functions, classes, and
 methods. Stick to it. For example, you may wish to prefix all exported
 names with the name of the data type that they deal with, say
 @racket[syntax-local].

Pick a consistency rule for naming and ordering the parameters of your
 functions and methods. Stick to it. For example, if your module implements
 an abstract data type (ADT), all functions on the ADT should consume the
 ADT-argument first or last.

@subsection{Sections}

Finally, a module consists of sections. It is good practice to separate the
 sections with comment lines. You may want to write down purpose statements
 for sections so that readers can easily understand which part of a module
 implements which service. Alternatively, consider using the large letter
 chapter headings in DrRacket to label the sections of a module.

With @racketmodname[rackunit], test suites can be defined within the
 module using @racket[define/provide-test-suite]. If you do so, locate the
 test section at the end of the module and @racket[require] the necessary
 pieces for testing specifically for the test suites.

@; -----------------------------------------------------------------------------
@section{Classes & Units}

@; -----------------------------------------------------------------------------
@section{Functions & Methods}

If your function or method consumes more than two parameters, consider
keyword arguments so that call sites can easily be understood.  In
addition, keyword arguments also ``thin'' out calls because function calls
don't need to refer to default values of arguments that are considered
optional.

Similarly, if your function or method consumers two (or more)
@emph{optional} parameters, keyword arguments are a must.

Please write a purpose statement for your function.
If you can, add an informal type and/or contract statement.

