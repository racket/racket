#lang scribble/base

@(require "shared.rkt" (for-label rackunit))

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

And a function/method/syntax-case of roughly 66 lines is usually
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
@(define line
   @t{---------------------------------------------------------------------------------------------------})
@section{Modules and their Interfaces}

The purpose of a module is to provide some services:
@;
@centerline{Equip a module with a short purpose statement.}
@;
Often ``short'' means one line; occasionally you may need several lines.

In order to understand a module's services, organize the module in three
sections below the purpose statement: its exports, its imports, and its
implementation:
@;%
@codebox[
@(begin
#reader scribble/comment-reader
 (racketmod0 #:file
 @tt{good}
 racket/base

;; the module implements a tv server

(provide
  ;; launch the tv server function
  tv-launch
  ;; set up a tv client to receive messages from the tv server
  tv-client)

(code:comment #, @line)
(code:comment #, @t{import and implementation section})

(require 2htdp/universe htdp/image)

(define (tv-launch)
  (universe ...))

(define (tv-client)
  (big-bang ...))
))]
@;%

If you choose to use @racket[provide] with @racket[contract-out], you
 may wish to have two @racket[require] sections:
@itemlist[
@item{the first one, placed with the @racket[provide] section, imports the
 values needed to formulate the contracts and}
@item{the second one, placed below the @racket[provide] section, imports
 the values needed to implement the services.}
]
 If your contracts call for additional concepts, define those right below
 the @racket[provide] specification:
@;%
@codebox[
@(begin
#reader scribble/comment-reader
 (racketmod0 #:file
 @tt{good}
 racket/base

;; the module implements a tv server

(provide
  (contract-out
    ;; initialize the board for the given number of players
    [board-init        (-> player#/c plain-board/c)]
    ;; initialize a board and place the tiles
    [create-board      (-> player#/c (listof placement/c)
			   (or/c plain-board/c string?))]
    ;; create a board from an X-expression representation
    [board-deserialize (-> xexpr? plain-board/c)]))

(require xml)

(define player# 3)
(define plain-board/c
  (instanceof/c (and/c admin-board%/c board%-contracts/c)))

(define placement/c
  (flat-named-contract "placement" ...))

(code:comment #, @line)
(code:comment #, @t{import and implementation section})

(require 2htdp/universe htdp/image)

; implementation:
(define (board-init n)
  (new board% ...))

(define (create-board n lop)
  (define board (board-init n))
  ...)

(define board%
  (class ... some 900 lines ...))
))]
@;%
 In the preceding code snippet, @xml[] imports the @racket[xexpr?]
 predicate. Since the latter is needed to articulate the contract for
 @racket[board-deserialize], the @racket[require] line for @xml[] is a part
 of the @racket[provide] section. In contrast, the @racket[require] line
 below the lines imports an event-handling mechanism plus a simple image
 manipulation library, and these tools are needed only for the
 implementation of the provided services.

Prefer specific export specifications over @racket[(provide (all-defined-out))].

A test suite section---if located within the module---should come at the
 very end, including its specific dependencies, i.e., @racket[require]
 specifications.

@; -----------------------------------------------------------------------------
@subsection{Require}

With @racket[require] specifications at the top of the implementation
 section, you let every reader know what is needed to understand the
 module.

@; -----------------------------------------------------------------------------
@subsection{Provide}

@(define 1/2-line
   @t{---------------------------------})


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
(racketmod0 #:file
 @tt{good}
 racket

 ;; This module implements
 ;; several strategies.

 (provide
  ;; Stgy = State -> Action

  ;; Stgy
  ;; people's strategy
  human-strategy

  ;; Stgy
  ;; tree traversal
  ai-strategy)

 (code:comment #, @1/2-line)
 (code:comment #, @t{implementation})

 (require "basics.rkt")

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
(racketmod0 #:file
 @tt{bad}
 racket

 ;; This module implements
 ;; several strategies.

 (code:comment #, @1/2-line)
 (code:comment #, @t{implementation})

 (require "basics.rkt")

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
  ;; a tree traversal
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
(racketmod0 #:file
@tt{good}
racket

(provide
 ;; (define-strategy (s:id a:id b:id c:id d:id)
 ;;   action:definition-or-expression)
 ;;
 ;; (define-strategy (s board tiles available score) ...)
 ;; defines a function from an instance of player to a
 ;; placement. The four identifier denote the state of
 ;; the board, the player's hand, the places where a
 ;; tile can be placed, and the player's current score.
 define-strategy)
))]

Use @scheme[provide] with @racket[contract-out] for module interfaces.
 Contracts often provide the right level of specification for first-time
 readers.

At a minimum, you should use type-like contracts, i.e., predicates that
 check for the constructor of data. They cost almost nothing, especially
 because exported functions tend to check such constraints internally
 anyway and contracts tend to render such checks superfluous.

If you discover that contracts create a performance bottleneck, please
 report the problem to the Racket developer mailing list.

@subsection{Uniformity of Interface}

Pick a rule for consistently naming your functions, classes, and
 methods. Stick to it. For example, you may wish to prefix all exported
 names with the name of the data type that they deal with, say
 @racket[syntax-local].

Pick a rule for consistently naming and ordering the parameters of your
 functions and methods. Stick to it. For example, if your module implements
 an abstract data type (ADT), all functions on the ADT should consume the
 ADT-argument first or last.

Finally pick the same name for all function/method arguments in a module
 that refer to the same kind of data---regardless of whether the module
 implements a common data structure. For example, in
 @filepath{collects/setup/scribble}, all functions use @racket[latex-dest]
 to refer to the same kind of data, even those that are not exported.

@subsection{Sections and Sub-modules}

Finally, a module consists of sections. It is good practice to separate the
 sections with comment lines. You may want to write down purpose statements
 for sections so that readers can easily understand which part of a module
 implements which service. Alternatively, consider using the large letter
 chapter headings in DrRacket to label the sections of a module.

With @racketmodname[rackunit], test suites can be defined within the
 module using @racket[define/provide-test-suite]. If you do so, locate the
 test section at the end of the module and @racket[require] the necessary
 pieces for testing specifically for the test suites.

As of version 5.3, Racket supports sub-modules. Use sub-modules to
 formulate sections, especially test sections. With sub-modules it is now
 possible to break up sections into distinct parts (labeled with the same
 name) and leave it to the language to stitch pieces together.

@;%
@codebox[
@(begin
#reader scribble/comment-reader
 (racketmod0 #:file
 @tt{fahrenheit.rkt}
 racket

 (provide
   (contract-out
     (code:comment #, @t{convert a fahrenheit temperature to a celsius})
     [fahrenheit->celsius (-> number? number?)]))

 (define (fahrenheit->celsius f)
   (/ (* 5 (- f 32)) 9))

 (module+ test
   (require rackunit)
   (check-equal? (fahrenheit->celsius -40) -40)
   (check-equal? (fahrenheit->celsius 32) 0)
   (check-equal? (fahrenheit->celsius 212) 100))
))]
@;%
 If you develop your code in DrRacket, it will run the test sub-module
 every time you click ``run'' unless you explicitly disable this
 functionality in the language selection menu. If you have a file and you
 just wish to run the tests, use @tt{raco} to do so:
@verbatim[#:indent 2]{
$ raco test fahrenheit.rkt
}
 Running this command in a shell will require and evaluate the test
 sub-module from the @tt{fahrenheit.rkt}.

@; -----------------------------------------------------------------------------
@section{Classes & Units}

(I will write something here sooner or later.)

@; -----------------------------------------------------------------------------
@section{Functions & Methods}

If your function or method consumes more than two parameters, consider
keyword arguments so that call sites can easily be understood.  In
addition, keyword arguments also ``thin'' out calls because function calls
don't need to refer to default values of arguments that are considered
optional.

Similarly, if your function or method consumers two (or more)
@emph{optional} parameters, keyword arguments are a must.

Write a purpose statement for your function.  If you can, add an informal
type and/or contract statement.

@; -----------------------------------------------------------------------------
@section{Contracts}

A contract establishes a boundary between a service provider and a service
consumer aka @defterm{server} and @defterm{client}. Due to historical
reasons, we tend to refer to this boundary as a @defterm{module boundary},
but the use of "module" in this phrase does @emph{not} only refer to
file-based or physical Racket modules. Clearly, @defterm{contract boundary}
is better than module boundary because it separates the two concepts.

When you use @racket[provide] with @racket[contract-out] at the module
level, the boundary of the physical module and the contract boundary
coincide.

When a module becomes too large to manage without contracts but you do not
wish to distribute the source over several files, you may wish to use one
of the following two constructs to erect contract boundaries internal to
the physical module:
@itemlist[
@item{@racket[define/contract]}
@item{@racket[module], as in submodule.}
]

Using the first one, @racket[define/contract], is like using
@racket[define] except that it is also possible to add a contract between
the header of the definition and its body. The following code display shows
a file that erects three internal contract boundaries: two for plain
constants and one for a function.

@;%
@codebox[
@(begin
#reader scribble/comment-reader
 (racketmod0 #:file
 @tt{celsius.rkt}
 racket

(define/contract AbsoluteC real? -273.15)
(define/contract AbsoluteF real? -459.67)

(define/contract (celsius->fahrenheit c)
  (code:comment #, @t{convert a celsius temperature to a fahrenheit temperature})
  (-> (and/c real? (>=/c AbsoluteC))
      (and/c real? (>=/c AbsoluteF)))
  ;; -- IN --
  (+ (* 9/5 c) 32))

(module+ test
  (require rackunit)
  (check-equal? (celsius->fahrenheit -40) -40)
  (check-equal? (celsius->fahrenheit 0) 32)
  (check-equal? (celsius->fahrenheit 100) 212))
))]
@;%

To find out how these contract boundaries work, you may wish to conduct
some experiments:
@itemlist[#:style 'ordered
@item{Add the following line to the bottom of the file:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(celsius->fahrenheit -300)
))
@;%
Save to file and observe how the contract system blames this line and what
the blame report tells you.}

@item{Replace the body of the @racket[celsius->fahrenheit] function with
@racketblock[(sqrt c)]
Once again, run the program and study the contract
exceptions, in particular observe which party gets blamed.}

@item{Change the right-hand side of @racket[AbsoluteC] to
@racket[-273.15i], i.e., a complex number. This time a different contract
party gets blamed.}
]
The screen shot below shows that @racket[define/contract] works for
mutually recursive functions with modules. This capability is unique to
@racket[define/contract].

@image["mut-rec-contracts.png" #:scale .8]{Mutually recursive functions with contracts}

In contrast, submodules act exactly like plain modules when it comes to
contract boundaries. Like @racket[define/contract], a submodue establishes
a contract boundary between itself and the rest of the module. Any value
flow between a client module and the submodule is governed by
contracts. Any value flow within the submodule is free of any constraints.

@codebox[
@(begin
#reader scribble/comment-reader
 (racketmod0 #:file
 @tt{graph-traversal.rkt}
 racket
 ...
 (module traversal racket
   (provide
    (contract-out
     (find-path (-> graph? node? node? (option/c path?)))))

   (require (submod ".." graph) (submod ".." contract))

   (define (find-path G s d (visited history0))
     (cond
       [(node=? s d) '()]
       [(been-here? s visited) #f]
       [else (define neighbors (node-neighbors G s))
	     (define there (record s visited))
	     (define path (find-path* G neighbors d there))
	     (if path (cons s path) #f)]))

   (define (find-path* G s* d visited)
     (cond
       [(empty? s*) #f]
       [else (or (find-path G (first s*) d visited)
		 (find-path* G (rest s*) d visited))]))

   (define (node-neighbors G n)
     (rest (assq n G))))

 (module+ test
   (require (submod ".." traversal) (submod ".." graph))
   (find-path G 'a 'd))
))]
@;%

Since modules and submodules cannot refer to each other in a mutual
recursive fashion, submodule contract boundaries cannot enforce constraints
on mutually recursive functions. It would thus be impossible to distribute
the @racket[find-path] and @racket[find-path*] functions from the preceding
code display into two distinct submodules.
