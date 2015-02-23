#lang scribble/doc
@(require scribble/manual scribble/eval racket/class "guide-utils.rkt")

@title[#:tag "reflection" #:style 'toc]{Reflection and Dynamic Evaluation}

Racket is a @italic{dynamic} language. It offers numerous facilities
for loading, compiling, and even constructing new code at run
time.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "eval"]{@racket[eval]}

@margin-note{This example will not work within a module or in DrRacket's definitions window,
             but it will work in the interactions window, for reasons that are
             explained by the end of @secref["namespaces"].}

The @racket[eval] function takes a representation of an expression or definition
(as a ``quoted'' form or @tech{syntax object}) and evaluates it:

@interaction[
(eval '(+ 1 2))
]

The power of @racket[eval] is that an expression can be
constructed dynamically:

@interaction[
(define (eval-formula formula)
  (eval `(let ([x 2]
               [y 3])
           ,formula)))
(eval-formula '(+ x y))
(eval-formula '(+ (* x y) y))
]

Of course, if we just wanted to evaluate expressions with given values
for @racket[x] and @racket[y], we do not need @racket[eval]. A more
direct approach is to use first-class functions:

@interaction[
(define (apply-formula formula-proc)
  (formula-proc 2 3))
(apply-formula (lambda (x y) (+ x y)))
(apply-formula (lambda (x y) (+ (* x y) y)))
]

However, if expressions like @racket[(+ x y)] and @racket[(+ (* x y)
y)] are read from a file supplied by a user, for example, then
@racket[eval] might be appropriate. Similarly, the @tech{REPL} reads
expressions that are typed by a user and uses @racket[eval] to
evaluate them.

Also, @racket[eval] is often used directly or indirectly on whole
modules. For example, a program might load a module on demand using
@racket[dynamic-require], which is essentially a wrapper around
@racket[eval] to dynamically load the module code.

@; ----------------------------------------

@subsection{Local Scopes}

The @racket[eval] function cannot see local bindings in the context
where it is called. For example, calling @racket[eval] inside an
unquoted @racket[let] form to evaluate a formula does not make values
visible for @racket[x] and @racket[y]:

@interaction[
(define (broken-eval-formula formula)
  (let ([x 2]
        [y 3])
    (eval formula)))
(broken-eval-formula '(+ x y))
]

The @racket[eval] function cannot see the @racket[x] and @racket[y]
bindings precisely because it is a function, and Racket is a lexically
scoped language. Imagine if @racket[eval] were implemented as

@racketblock[
(define (eval x)
  (eval-expanded (macro-expand x)))
]

then at the point when @racket[eval-expanded] is called, the most
recent binding of @racket[x] is to the expression to evaluate, not the
@racket[let] binding in @racket[broken-eval-formula]. Lexical scope
prevents such confusing and fragile behavior, and consequently
prevents @racket[eval] from seeing local bindings in the context where
it is called.

You might imagine that even though @racket[eval] cannot see the local
bindings in @racket[broken-eval-formula], there must actually be a
data structure mapping @racket[x] to @racket[2] and @racket[y] to
@racket[3], and you would like a way to get that data structure. In
fact, no such data structure exists; the compiler is free to replace
every use of @racket[x] with @racket[2] at compile time, so that the
local binding of @racket[x] does not exist in any concrete sense at
run-time. Even when variables cannot be eliminated by
constant-folding, normally the names of the variables can be
eliminated, and the data structures that hold local values do not
resemble a mapping from names to values.

@; ----------------------------------------

@subsection[#:tag "namespaces"]{Namespaces}

Since @racket[eval] cannot see the bindings from the context where it
is called, another mechanism is needed to determine dynamically
available bindings. A @deftech{namespace} is a first-class value that
encapsulates the bindings available for dynamic evaluation.

@margin-note{Informally, the term @defterm{namespace} is sometimes
 used interchangeably with @defterm{environment} or
 @defterm{scope}. In Racket, the term @defterm{namespace} has the
 more specific, dynamic meaning given above, and it should not be
 confused with static lexical concepts.}

Some functions, such as @racket[eval], accept an optional namespace
argument. More often, the namespace used by a dynamic operation is the
@deftech{current namespace} as determined by the
@racket[current-namespace] @tech{parameter}.

When @racket[eval] is used in a @tech{REPL}, the current namespace is the one
that the @tech{REPL} uses for evaluating expressions. That's why the
following interaction successfully accesses @racket[x] via
@racket[eval]:

@interaction[
(define x 3)
(eval 'x)
]

In contrast, try the following simple module and running it directly
in DrRacket or supplying the file as a command-line argument to
@exec{racket}:

@racketmod[
racket

(eval '(cons 1 2))
]

This fails because the initial current namespace is empty. When you
run @exec{racket} in interactive mode (see
@secref["start-interactive-mode"]), the initial namespace is
initialized with the exports of the @racket[racket] module, but when
you run a module directly, the initial namespace starts empty.

In general, it's a bad idea to use @racket[eval] with whatever
namespace happens to be installed. Instead, create a namespace
explicitly and install it for the call to eval:

@racketmod[
racket

(define ns (make-base-namespace))
(eval '(cons 1 2) ns) (code:comment @#,t{works})
]

The @racket[make-base-namespace] function creates a namespace that is
initialized with the exports of @racket[racket/base]. The later
section @secref["mk-namespace"] provides more information on creating
and configuring namespaces.

@; ----------------------------------------

@subsection{Namespaces and Modules}

As with @racket[let] bindings, lexical scope means that @racket[eval]
cannot automatically see the definitions of a @racket[module] in which
it is called. Unlike @racket[let] bindings, however, Racket provides a
way to reflect a module into a @tech{namespace}.

The @racket[module->namespace] function takes a quoted @tech{module
path} and produces a namespace for evaluating expressions and
definitions as if they appeared in the @racket[module] body:

@interaction[
(module m racket/base
  (define x 11))
(require 'm)
(define ns (module->namespace ''m))
(eval 'x ns)
]

@margin-note{The double quoting in @racket[''m] is because @racket['m]
is a module path that refers to an interactively declared module, and
so @racket[''m] is the quoted form of the path.}

The @racket[module->namespace] function is mostly useful from outside
a module, where the module's full name is known. Inside a
@racket[module] form, however, the full name of a module may not be
known, because it may depend on where the module source is located
when it is eventually loaded.

From within a @racket[module], use @racket[define-namespace-anchor] to
declare a reflection hook on the module, and use
@racket[namespace-anchor->namespace] to reel in the module's
namespace:

@racketmod[
racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define x 1)
(define y 2)

(eval '(cons x y) ns) (code:comment @#,t{produces @racketresult[(1 . 2)]})
]


@; ----------------------------------------------------------------------

@section[#:tag "mk-namespace"]{Manipulating Namespaces}

A @tech{namespace} encapsulates two pieces of information:

@itemize[

 @item{A mapping from identifiers to bindings. For example, a
       namespace might map the identifier @racketidfont{lambda} to the
       @racket[lambda] form. An ``empty'' namespace is one that maps
       every identifier to an uninitialized top-level variable.}

 @item{A mapping from module names to module declarations and
       instances.}

]

The first mapping is used for evaluating expressions in a top-level
context, as in @racket[(eval '(lambda (x) (+ x 1)))]. The second
mapping is used, for example, by @racket[dynamic-require] to locate a
module. The call @racket[(eval '(require racket/base))] normally uses
both pieces: the identifier mapping determines the binding of
@racketidfont{require}; if it turns out to mean @racket[require], then
the module mapping is used to locate the @racketmodname[racket/base]
module.

From the perspective of the core Racket run-time system, all
evaluation is reflective. Execution starts with an initial namespace
that contains a few primitive modules, and that is further populated
by loading files and modules as specified on the command line or as
supplied in the @tech{REPL}. Top-level @racket[require] and
@racket[define] forms adjusts the identifier mapping, and module
declarations (typically loaded on demand for a @racket[require] form)
adjust the module mapping.

@; ----------------------------------------

@subsection{Creating and Installing Namespaces}

The function @racket[make-empty-namespace] creates a new, empty
@tech{namespace}. Since the namespace is truly empty, it cannot at
first be used to evaluate any top-level expression---not even
@racket[(require racket)]. In particular,

@racketblock[
(parameterize ([current-namespace (make-empty-namespace)])
  (namespace-require 'racket))
]

fails, because the namespace does not include the primitive modules on
which @racket[racket] is built.

To make a namespace useful, some modules must be @deftech{attached}
from an existing namespace. Attaching a module adjusts the mapping of
module names to instances by transitively copying entries (the module
and all its imports) from an existing namespace's mapping. Normally,
instead of just attaching the primitive modules---whose names and
organization are subject to change---a higher-level module is
attached, such as @racketmodname[racket] or
@racketmodname[racket/base].

The @racket[make-base-empty-namespace] function provides a namespace
that is empty, except that @racketmodname[racket/base] is
attached. The resulting namespace is still ``empty'' in the sense that
the identifiers-to-bindings part of the namespace has no mappings;
only the module mapping has been populated. Nevertheless, with an
initial module mapping, further modules can be loaded.

A namespace created with @racket[make-base-empty-namespace] is
suitable for many basic dynamic tasks. For example, suppose that a
@racketmodfont{my-dsl} library implements a domain-specific language
in which you want to execute commands from a user-specified file. A
namespace created with @racket[make-base-empty-namespace] is enough to
get started:

@racketblock[
(define (run-dsl file)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require 'my-dsl)
    (load file)))
]

Note that the @racket[parameterize] of @racket[current-namespace] does
not affect the meaning of identifiers like @racket[namespace-require]
within the @racket[parameterize] body. Those identifiers obtain their
meaning from the enclosing context (probably a module). Only
expressions that are dynamic with respect to this code, such as the
content of @racket[load]ed files, are affected by the
@racket[parameterize].

Another subtle point in the above example is the use of
@racket[(namespace-require 'my-dsl)] instead of @racket[(eval
'(require my-dsl))]. The latter would not work, because @racket[eval]
needs to obtain a meaning for @racket[require] in the namespace, and
the namespace's identifier mapping is initially empty. The
@racket[namespace-require] function, in contrast, directly imports the
given module into the current namespace.  Starting with
@racket[(namespace-require 'racket/base)] would introduce a binding
for @racketidfont{require} and make a subsequent @racket[(eval
'(require my-dsl))] work. The above is better, not only because it is
more compact, but also because it avoids introducing bindings that are
not part of the domain-specific languages.

@; ----------------------------------------

@subsection{Sharing Data and Code Across Namespaces}

Modules not attached to a new namespace will be loaded and
instantiated afresh if they are demanded by evaluation. For example,
@racketmodname[racket/base] does not include
@racketmodname[racket/class], and loading @racketmodname[racket/class]
again will create a distinct class datatype:

@interaction[
(require racket/class)
(class? object%)
(class?
 (parameterize ([current-namespace (make-base-empty-namespace)])
   (namespace-require 'racket/class) (code:comment @#,t{loads again})
   (eval 'object%)))
]

For cases when dynamically loaded code needs to share more code and
data with its context, use the @racket[namespace-attach-module]
function. The first argument to @racket[namespace-attach-module] is a
source namespace from which to draw a module instance; in some cases,
the current namespace is known to include the module that needs to be
shared:

@interaction[
(require racket/class)
(class?
 (let ([ns (make-base-empty-namespace)])
   (namespace-attach-module (current-namespace)
                            'racket/class
                            ns)
   (parameterize ([current-namespace ns])
     (namespace-require 'racket/class) (code:comment @#,t{uses attached})
     (eval 'object%))))
]

Within a module, however, the combination of
@racket[define-namespace-anchor] and
@racket[namespace-anchor->empty-namespace] offers a more reliable
method for obtaining a source namespace:

@racketmod[
racket/base

(require racket/class)

(define-namespace-anchor a)

(define (load-plug-in file)
  (let ([ns (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace a)
                             'racket/class
                              ns)
    (parameterize ([current-namespace ns])
      (dynamic-require file 'plug-in%))))
]

The anchor bound by @racket[namespace-attach-module] connects the
run time of a module with the namespace in which a module is loaded
(which might differ from the current namespace).  In the above
example, since the enclosing module requires
@racketmodname[racket/class], the namespace produced by
@racket[namespace-anchor->empty-namespace] certainly contains an
instance of @racketmodname[racket/class]. Moreover, that instance is
the same as the one imported into the module, so the class datatype is
shared.

@; ----------------------------------------------------------------------

@section[#:tag "load"]{Scripting Evaluation and Using @racket[load]}

Historically, Lisp implementations did not offer module
systems. Instead, large programs were built by essentially scripting
the @tech{REPL} to evaluate program fragments in a particular order.
While @tech{REPL} scripting turns out to be a bad way to structure
programs and libraries, it is still sometimes a useful capability.

@margin-note{Describing a program via @racket[load] interacts
especially badly with macro-defined language extensions
@cite["Flatt02"].}

The @racket[load] function runs a @tech{REPL} script by
@racket[read]ing S-expressions from a file, one by one, and passing
them to @racket[eval]. If a file @filepath{place.rkts} contains

@racketblock[
(define city "Salt Lake City")
(define state "Utah")
(printf "~a, ~a\n" city state)
]

then it can be loaded in a @tech{REPL}:

@interaction[
(eval:alts (load "place.rkts") (begin (define city "Salt Lake City")
                                     (printf "~a, Utah\n" city)))
city
]

Since @racket[load] uses @racket[eval], however, a module like the
following generally will not work---for the same reasons described in
@secref["namespaces"]:

@racketmod[
racket

(define there "Utopia")

(load "here.rkts")
]

The current namespace for evaluating the content of
@filepath{here.rkts} is likely to be empty; in any case, you cannot get
@racket[there] from @filepath{here.rkts}. Also, any definitions in
@filepath{here.rkts} will not become visible for use within the module;
after all, the @racket[load] happens dynamically, while references to
identifiers within the module are resolved lexically, and therefore
statically.

Unlike @racket[eval], @racket[load] does not accept a namespace
argument. To supply a namespace to @racket[load], set the
@racket[current-namespace] @tech{parameter}. The following example evaluates
the expressions in @filepath{here.rkts} using the bindings of the
@racketmodname[racket/base] module:

@racketmod[
racket

(parameterize ([current-namespace (make-base-namespace)])
  (load "here.rkts"))
]

You can even use @racket[namespace-anchor->namespace] to make the
bindings of the enclosing module accessible for dynamic evaluation. In
the following example, when @filepath{here.rkts} is @racket[load]ed, it
can refer to @racket[there] as well as the bindings of
@racketmodname[racket]:

@racketmod[
racket

(define there "Utopia")

(define-namespace-anchor a)
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (load "here.rkts"))
]

Still, if @filepath{here.rkts} defines any identifiers, the definitions
cannot be directly (i.e., statically) referenced by in the enclosing
module.

The @racketmodname[racket/load] module language is different from
@racketmodname[racket] or @racketmodname[racket/base]. A module using
@racketmodname[racket/load] treats all of its content as dynamic,
passing each form in the module body to @racket[eval] (using a
namespace that is initialized with @racketmodname[racket]). As a
result, uses of @racket[eval] and @racket[load] in the module body see
the same dynamic namespace as immediate body forms. For example, if
@filepath{here.rkts} contains

@racketblock[
(define here "Morporkia")
(define (go!) (set! here there))
]

then running

@racketmod[
racket/load

(define there "Utopia")

(load "here.rkts")

(go!)
(printf "~a\n" here)
]

prints ``Utopia''.

Drawbacks of using @racketmodname[racket/load] include reduced
error checking, tool support, and performance. For example, with the
program

@racketmod[
racket/load

(define good 5)
(printf "running\n")
good
bad
]

DrRacket's @onscreen{Check Syntax} tool cannot tell that the second
@racket[good] is a reference to the first, and the unbound reference
to @racket[bad] is reported only at run time instead of rejected
syntactically.
