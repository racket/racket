#lang scribble/base

@(require "shared.rkt"
          (for-label syntax/parse))

@; -----------------------------------------------------------------------------

@title{Language and Performance}

When you write a module, you first pick a language. In Racket you can
 choose a lot of languages. The most important choice concerns @rkt/base[]
 vs @rkt[].

For scripts, use @rkt/base[]. The @rkt/base[] language loads significantly
 faster than the @rkt[] language because it is much smaller than the
 @rkt[].

If your module is intended as a library, stick to @rkt/base[]. That way
 script writers can use it without incurring the overhead of loading all of
 @rkt[] unknowingly.

Conversely, you should use @rkt[] (or even @rkt/gui[]) when you just want a
 convenient language to write some program. The @rkt[] language comes with
 almost all the batteries, and @rkt/gui[] adds the rest of the GUI base.

@; -----------------------------------------------------------------------------
@section{Library Interfaces}

Imagine you are working on a library. You start with one module, but before
you know it the set of modules grows to a decent size. Client programs are
unlikely to use all of your library's exports and modules. If, by default,
your library includes all features, you may cause unnecessary mental stress
and run-time cost that clients do not actually use. 

In building the Racket language, we have found it useful to factor
libraries into different layers so that client programs can selectively
import from these bundles. The specific Racket practice is to use the most
prominent name as the default for the module that includes everything. When
it comes to languages, this is the role of @rkt[]. A programmer who wishes
to depend on a small part of the language chooses to @rkt/base[] instead;
this name refers to the basic foundation of the language. Finally, some of
Racket's constructs are not even included in @rkt[]---consider
@racketmodname[racket/require] for example---and must be required
explicitly in programs.

Other Racket libraries choose to use the default name for the small
core. Special names then refer to the complete library. 

We encourage library developers to think critically about these
decisions and decide on a practice that fits their taste and
understanding of the users of their library. We encourage developers
to use the following names for different places on the "size"
hierarchy:

@itemlist[

@item{@racket[library/kernel], the bare minimal conceivable for the
library to be usable;}

@item{@racket[library/base], a basic set of functionality.}

@item{@racket[library], an appropriate "default" of functionality
corresponding to either @racket[library/base] or @racket[library/full].}

@item{@racket[library/full], the full library functionality.}  
] 
Keep two considerations in mind as you decide which parts of your library
should be in which files: dependency and logical ordering.  The smaller
files should depend on fewer dependencies. Try to organize the levels so
that, in principle, the larger libraries can be implemented in terms of the
public interfaces of the smaller ones. 

Finally, the advice of the previous section, to use @rkt/base[] when
building a library, generalizes to other libraries: by being more
specific in your dependencies, you are a responsible citizen and
enable others to have a small (transitive) dependency set.

@; -----------------------------------------------------------------------------
@section{Macros: Space and Performance}

Macros copy code. Also, Racket is really a tower of macro-implemented
 languages. Hence, a single line of source code may expand into a rather
 large core expression. As you and others keep adding macros, even the
 smallest functions generate huge expressions and consume a lot of space.
 This kind of space consumption may affect the performance of your project
 and is therefore to be avoided.

When you design your own macro with a large expansion, try to factor it
 into a function call that consumes small thunks or procedures.

@compare0[
@racketmod0[
racket
...
(define-syntax (search s)
  (syntax-parse s
    [(_ x (e:expr ...)
        (~datum in)
        b:expr)
     #'(sar/λ (list e ...)
              (λ (x) b))]))

(define (sar/λ l p)
  (for ([a '()]) ([y l])
    (unless (bad? y)
      (cons (p y) a))))

(define (bad? x)
  ... many lines ...)
]
@; -----------------------------------------------------------------------------
[racketmod0
racket
...
(define-syntax (search s)
  (syntax-parse s
    [(_ x (e:expr ...)
        (~datum in)
        b:expr)
     #'(begin
         (define (bad? x)
           ... many lines ...)
         (define l
	   (list e ...))
         (for ([a '()]) ([x l])
           (unless (bad? x)
             (cons b a))))]))
]
]

As you can see, the macro on the left calls a function with a list of the
searchable values and a function that encapsulates the body. Every
expansion is a single function call. In contrast, the macro on the right
expands to many nested definitions and expressions every time it is used.

@; -----------------------------------------------------------------------------
@section{No Contracts}

Adding contracts to a library is good. 

On some occasions, contracts impose a significant performance penalty. 
For such cases, we recommend organizing the module into a main module as
usual and a submodule called @tt{no-contract} so that
@itemlist[

@item{the @tt{no-contract} submodule @racket[provide]s the functionality @emph{without} contracts,}
@item{the main module @racket[provide]s the functionality @emph{with} contracts.}
]
This section explains three strategies for three different situations and
levels of implementation complexity. 

@margin-note*{We will soon supply a Reference section in the Evaluation Model chapter that
explains the basics of our understanding of ``safety'' and link to it.}
@; 
@bold{Warning} Splitting contracted functionality into two modules in
this way renders the code in the @tt{no-contract} module @bold{unsafe}. The
creator of the original code might have assumed certain constraints on some
functions' arguments, and the contracts checked these constraints. While
the documentation of the @tt{no-contract} submodule is likely to state
these constraints, it is left to the client to check them.  If the client
code doesn't check the constraints and the arguments don't satisfy them,
the code in the @tt{no-contract} submodule may go wrong in various ways.

The @emph{first} and simplest way to create a @tt{no-contract} submodule is to use
the @racket[#:unprotected-submodule] functionality of @racket[contract-out].

@compare0[#:right "fast"
@;%
(racketmod0
 racket

 (define state? zero?)
 (define action? odd?)
 (define strategy/c
   (-> state? action?))

 (provide
  (contract-out
   [human strategy/c]
   [ai strategy/c]))


 (code:comment2 #, @1/2-line[])
 (code:comment2 #, @t{implementation})

 (define (general p)
   (lambda (_) pi))

 (define (human x)
   ((general 'gui) x))

 (define (ai x)
   ((general 'tra) x)))

(racketmod0
 racket

 (define state? zero?)
 (define action? odd?)
 (define strategy/c
   (-> state? action?))

 (provide
  (contract-out (code:hilite (code:line
   #:unprotected-submodule no-contract))
   [human strategy/c]
   [ai strategy/c]))

 (code:comment2 #, @1/2-line[])
 (code:comment2 #, @t{implementation})

 (define (general s)
   (lambda (_) pi))

 (define (human x)
   ((general 'gui) x))

 (define (ai x)
   ((general 'tra) x)))
]

The module called @tt{good} illustrates what the code might look
like originally. Every exported functions come with contracts, and the
definitions of these functions can be found below the @racket[provide]
specification in the module body. The @tt{fast} module on the right
requests the creation of a submodule named @tt{no-contract}, which exports
the same identifiers as the original module but without contracts. 

Once the submodule exists, using the library with or without contracts is
straightforward:
@compare0[#:left "needs-goodness" #:right "needs-speed"
@;%
(racketmod0
 racket

 (require "fast.rkt")

 human
 (code:comment2 #, @elem{comes with contracts})
 (code:comment2 #, @elem{as if we had required })
 (code:comment2 #, @elem{"good.rkt" itself})

 (define state1 0)
 (define state2 
   (human state1)))

@(begin
#reader scribble/comment-reader
(racketmod0
 racket

 (require (submod "fast.rkt" no-contract))

 human
 (code:comment2 #, @elem{comes without})
 (code:comment2 #, @elem{a contract})

 (define state* 
   (build-list 0 1))
 (define action*
   (map human state*))))
]
Both modules @racket[require] the @tt{fast} module, but @tt{needs-goodness}
on the left goes through the contracted @racket[provide] while
@tt{needs-speed} on the right uses the @tt{no-contract} submodule. Tchnically,
the left module imports @racket[human] with contracts; the right one
imports the same function without contract and thus doesn't have to pay the
performance penalty.

Notice, however, that when you run these two client modules---assuming you
saved them with the correct names in some folder---the left one raises a
contract error while the right one binds @racket[action*] to

@;%
@(begin
#reader scribble/comment-reader
(racketresult
'(3.141592653589793 3.141592653589793)
))
@;%

The @tt{no-contract} submodule generated by this first, easy approach
retains the dependency on @racketmodname[#, 'racket/contract] at both compile and run time. 
Here is a variant of the above module that demonstrates this point: 
@;%
@(racketmod0 #:file
 @tt{problems-with-unprotected-submodule}
 racket

(define state? zero?)
(define action? odd?)
(define strategy/c (-> state? action?))

(provide
 (contract-out
  #:unprotected-submodule no-contract
  [human strategy/c]
  [ai strategy/c]))

(define (general p) pi)

(define human (general 'gui))

(define ai (general 'tra)))
@;%
Even though the @racket[contract-out] specification seems to remove the
contracts, requiring the @tt{no-contract} still raises a contract error: 
@;%
@(racketblock
(require (submod "." server no-contract))
)
@;%
@bold{Explanation} The @tt{no-contract} submodule depends on the main
module, so the require runs the body of the main module, and doing so
checks the first-order properties of the exported values. Because
@racket[human] is not a function, this evaluation raises a contract error.

The @emph{second} way to create a @tt{no-contract} submodule requires
systematic work from the developer and eliminates the run-time dependency
on @racketmodname[#, 'racket/contract]. Here are the two modules from
above, with the right one derived manually from the one on the left: 
@compare0[#:left "good2" #:right "fast2"
@;%
(racketmod0
 racket

 (define state? zero?)
 (define action? odd?)
 (define strategy/c
   (-> state? action?))

 (provide
  (contract-out
   [human strategy/c]
   [ai strategy/c]))

 (code:comment2 #, @1/2-line[])
 (code:comment2 #, @t{implementation})

 (define (general p)
   (lambda (_) pi))

 (define (human x)
   ((general 'gui) x))

 (define (ai x)
   ((general 'tra) x)))

(racketmod0
 racket 

 (define state? zero?)
 (define action? odd?)
 (define strategy/c
   (-> state? action?))

 (provide
  (contract-out
   [human strategy/c]
   [ai strategy/c]))

 (code:comment2 #, @1/2-line[])
 (code:comment2 #, @t{implementation})

 (module no-contract racket 
   (provide 
    human
    ai)

   (define (general s)
     (lambda (_) pi))

   (define (human x)
     ((general 'gui) x))

   (define (ai x)
     ((general 'tra) x)))

 (require 'no-contract)
)
]
The @tt{fast2} module on the right encapsulates the
definitions in a submodule called @tt{no-contract}; the @racket[provide] in
this submodule exports the exact same identifiers as the @tt{good2} module
on the left.  The main module @racket[require]s the submodule immediately,
making the identifiers available in the outer scope so that the contracted
@code{provide} can re-export them.

While this second way of creating a @tt{no-contract} submodule eliminates
the run-time dependency on @racketmodname[#, 'racket/contract], its
compilation---as a part of the outer module---still depends on this
library, which is problematic in a few remaining situations. 

The @emph{third} and last way to create a @tt{no-contract} submodule is
useful when contracts prevents a module from being used in a context where
contracts aren't available at all---neither at compile nor at run time. One
example is @rkt/base[]; another is the contracts library itself. Again, you
may wish you had the same library without contracts. For these cases, we
recommend a file-based strategy one. Assuming the library is located at
@tt{a/b/c}, we recommend

@itemlist[#:style 'ordered

@item{creating a @tt{c/} sub-directory with the file @tt{no-contract.rkt},}

@item{placing the functionality into @tt{no-contract.rkt},}

@item{adding @racket[(require "c/no-contract.rkt")] to @tt{c.rkt}, and}

@item{exporting the functionality from there with contracts.}

]

Once this arrangement is set up, a client module in a special context
@rkt/base[] or for @racketmodname[#, 'racket/contract] can use @racket[(require
a/b/c/no-contract)]. In a regular module, though, it would suffice
to write @racket[(require a/b/c)] and doing so would import contracted
identifiers. 

@; -----------------------------------------------------------------------------
@section{Unsafe: Beware}

Racket provides a number of unsafe operations that behave
like their related, safe variants but only when given valid inputs.
They differ in that they eschew checking for performance reasons
and thus behave unpredictably on invalid inputs.

As one example, consider @racket[fx+] and @racket[unsafe-fx+].
When @racket[fx+] is applied to a non-@racket[fixnum?], it raises
an error. In contrast, when @racket[unsafe-fx+] is applied to a non-@racket[fixnum?],
it does not raise an error. Instead it either returns a strange result
that may violate invariants of the run-time system and may cause
later operations (such as printing out the value) to crash Racket itself.

Do not use unsafe operations in your programs unless you are writing
software that builds proofs that the unsafe operations receive only
valid inputs (e.g., a type system like Typed Racket) or you are building
an abstraction that always inserts the right checks very close to
the unsafe operation (e.g., a macro like @racket[for]). And even in these
situations, avoid unsafe operations unless you have done a careful performance
analysis to be sure that the performance improvement outweighs
the risk of using the unsafe operations.
