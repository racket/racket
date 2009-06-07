#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scheme/class
          "guide-utils.ss")

@title[#:tag "reflection" #:style 'toc]{Reflection and Dynamic Evaluation}

Scheme is a @italic{dynamic} language. It offers numerous facilities
for loading, compiling, and even constructing new code at run
time.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "eval"]{@scheme[eval]}

The @scheme[eval] function takes a ``quoted'' expression or definition
and evaluates it:

@interaction[
(eval '(+ 1 2))
]

The power of @scheme[eval] that is that an expression can be
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
for @scheme[x] and @scheme[y], we do not need @scheme[eval]. A more
direct approach is to use first-class functions:

@interaction[
(define (apply-formula formula-proc)
  (formula-proc 2 3))
(apply-formula (lambda (x y) (+ x y)))
(apply-formula (lambda (x y) (+ (* x y) y)))
]

However, if expressions like @scheme[(+ x y)] and @scheme[(+ (* x y)
y)] are read from a file supplied by a user, for example, then
@scheme[eval] might be appropriate. Simialrly, the @tech{REPL} reads
expressions that are typed by a user and uses @scheme[eval] to
evaluate them.

Also, @scheme[eval] is often used directly or indirectly on whole
modules. For example, a program might load a module on demand using
@scheme[dynamic-require], which is essentially a wrapper around
@scheme[eval] to dynamically load the module code.

@; ----------------------------------------

@subsection{Local Scopes}

The @scheme[eval] function cannot see local bindings in the context
where it is called. For example, calling @scheme[eval] inside an
unquoted @scheme[let] form to evaluate a formula does not make values
visible for @scheme[x] and @scheme[y]:

@interaction[
(define (broken-eval-formula formula)
  (let ([x 2]
        [y 3])
    (eval formula)))
(broken-eval-formula '(+ x y))
]

The @scheme[eval] function cannot see the @scheme[x] and @scheme[y]
bindings precisely because it is a function, and Scheme is a lexically
scoped language. Imagine if @scheme[eval] were implemented as

@schemeblock[
(define (eval x)
  (eval-expanded (macro-expand x)))
]

then at the point when @scheme[eval-expanded] is called, the most
recent binding of @scheme[x] is to the expression to evaluate, not the
@scheme[let] binding in @scheme[broken-eval-formula]. Lexical scope
prevents such confusing and fragile behavior, and consequently
prevents @scheme[eval] from seeing local bindings in the context where
it is called.

You might imagine that even though @scheme[eval] cannot see the local
bindings in @scheme[broken-eval-formula], there must actually be a
data structure mapping @scheme[x] to @scheme[2] and @scheme[y] to
@scheme[3], and you would like a way to get that data structure. In
fact, no such data structure exists; the compiler is free to replace
every use of @scheme[x] with @scheme[2] at compile time, so that the
local binding of @scheme[x] does not exist in any concrete sense at
run-time. Even when variables cannot be eliminated by
constant-folding, normally the names of the variables can be
eliminated, and the data structures that hold local values do not
resemble a mapping from names to values.

@; ----------------------------------------

@subsection[#:tag "namespaces"]{Namespaces}

Since @scheme[eval] cannot see the bindings from the context where it
is called, another mechanism is needed to determine dynamically
available bindings. A @deftech{namespace} is a first-class value that
encapsulates the bindings available for dynamic evaluation.

@margin-note{Informally, the term @defterm{namespace} is sometimes
 used interchangeably with @defterm{environment} or
 @defterm{scope}. In PLT Scheme, the term @defterm{namespace} has the
 more specific, dynamic meaning given above, and it should not be
 confused with static lexical concepts.}

Some functions, such as @scheme[eval], accept an optional namespace
argument. More often, the namespace used by a dynamic operation is the
@deftech{current namespace} as determined by the
@scheme[current-namespace] parameter.

When @scheme[eval] is used in a @tech{REPL}, the current is the one
that the @tech{REPL} uses for evaluating expressions. That's why the
following interaction successfully accesses @scheme[x] via
@scheme[eval]:

@interaction[
(define x 3)
(eval 'x)
]

In contrast, try the following a simple module and running in directly
in DrScheme's @onscreen{Module} language or supplying the file as a
command-line argument to @exec{mzscheme}:

@schememod[
scheme

(eval '(cons 1 2))
]

This fails because the initial current namespace is empty. When you
run @exec{mzscheme} in interactive mode (see
@secref["start-interactive-mode"]), the initial namespace is
initialized with the exports of the @scheme[scheme] module, but when
you run a module directly, the initial namespace starts empty.

In general, it's a bad idea to use @scheme[eval] with whatever
namespace happens to be installed. Instead, create a namespace
explicitly and install it for the call to eval:

@schememod[
scheme

(define ns (make-base-namespace))
(eval '(cons 1 2) ns) (code:comment @#,t{works})
]

The @scheme[make-base-namespace] function creates a namespace that is
initialized with the exports of @scheme[scheme/base]. The later
section @secref["mk-namespace"] provides more information on creating
and configuring namespaces.

@; ----------------------------------------

@subsection{Namespaces and Modules}

As with @scheme[let] bindings, lexical scope means that @scheme[eval]
cannot automatically see the definitions of a @scheme[module] in which
it is called. Unlike @scheme[let] bindings, however, Scheme provides a
way to reflect a module into a @tech{namespace}.

The @scheme[module->namespace] function takes a quoted @tech{module
path} and produces a namespace for evaluating expressions and
definitions as if they appears in the @scheme[module] body:

@interaction[
(module m scheme/base
  (define x 11))
(require 'm)
(define ns (module->namespace ''m))
(eval 'x ns)
]

@margin-note{The double quoting in @scheme[''m] is because @scheme['m]
is a module path that refers to an interactively declared module, and
so @scheme[''m] is the quoted form of the path.}

The @scheme[module->namespace] function is mostly useful from outside
a module, where the module's full name is known. Inside a
@scheme[module] form, however, the full name of a module may not be
known, because it may depend on where the module source is location
when it is eventually loaded.

From within a @scheme[module], use @scheme[define-namespace-anchor] to
declare a reflection hook on the module, and use
@scheme[namespace-anchor->namespace] to reel in the module's
namespace:

@schememod[
scheme

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define x 1)
(define y 2)

(eval '(cons x y) ns) (code:comment @#,t{produces @schemeresult[(1 . 2)]})
]


@; ----------------------------------------------------------------------

@section[#:tag "mk-namespace"]{Manipulating Namespaces}

A @tech{namespace} encapsulates two pieces of information:

@itemize[

 @item{A mapping from identifiers to bindings. For example, a
       namespace might map the identifier @schemeidfont{lambda} to the
       @scheme[lambda] form. An ``empty'' namespace is one that maps
       every identifier to an uninitialized top-level variable.}

 @item{A mapping from module names to module declarations and
       instances.}

]

The first mapping is used for evaluating expressions in a top-level
context, as in @scheme[(eval '(lambda (x) (+ x 1)))]. The second
mapping is used, for example, by @scheme[dynamic-require] to locate a
module. The call @scheme[(eval '(require scheme/base))] normally uses
both pieces: the identifier mapping determines the binding of
@schemeidfont{require}; if it turns out to mean @scheme[require], then
the module mapping is used to locate the @schememodname[scheme/base]
module.

From the perspective of the core Scheme run-time system, all
evaluation is reflective. Execution starts with an initial namespace
that contains a few primitive modules, and that is further populated
by loading files and modules as specified on the command line or as
supplied in the @tech{REPL}. Top-level @scheme[require] and
@scheme[define] forms adjusts the identifier mapping, and module
declarations (typically loaded on demand for a @scheme[require] form)
adjust the module mapping.

@; ----------------------------------------

@subsection{Creating and Installing Namespaces}

The function @scheme[make-empty-namespace] creates a new, empty
@tech{namespace}. Since the namespace is truly empty, it cannot at
first be used to evaluate any top-level expression---not even
@scheme[(require scheme)]. In particular,

@schemeblock[
(parameterize ([current-namespace (make-empty-namespace)])
  (namespace-require 'scheme))
]

fails, because the namespace does not include the primitive modules on
which @scheme[scheme] is built.

To make a namespace useful, some modules much be @deftech{attached}
from an existing namespace. Attaching a module adjusts the mapping of
module names to instances by transitively copying entries (the module
and all its imports) from an existing namespace's mapping. Normally,
instead of just attaching the primitive modules---whose names and
organization are subject to change---a higher-level module is
attached, such as @schememodname[scheme] or
@schememodname[scheme/base].

The @scheme[make-base-empty-namespace] function provides a namespace
that is empty, except that @schememodname[scheme/base] is
attached. The resulting namespace is still ``empty'' in the sense that
the identifiers-to-bindings part of the namespace has no mappings;
only the module mapping has been populated. Nevertheless, with an
initial module mapping, further modules can be loaded.

A namespace created with @scheme[make-base-empty-namespace] is
suitable for many basic dynamic tasks. For example, suppose that a
@schememodfont{my-dsl} library implements a domain-specific language
in which you want to execute commands from a user-specified file. A
namespace created with @scheme[make-base-empty-namespace] is enough to
get started:

@schemeblock[
(define (run-dsl file)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require 'my-dsl)
    (load file)))
]

Note that the @scheme[parameterize] of @scheme[current-namespace] does
not affect the meaning of identifiers like @scheme[namespace-require]
within the @scheme[parameterize] body. Those identifiers obtain their
meaning from the enclosing context (probably a module). Only
expressions that are dynamic with respect to this code, such as the
content of @scheme[load]ed files, are affected by the
@scheme[parameterize].

Another subtle point in the above example is the use of
@scheme[(namespace-require 'my-dsl)] instead of @scheme[(eval
'(require my-dsl))]. The latter would not work, because @scheme[eval]
needs to obtain a meaning for @scheme[require] in the namespace, and
the namespace's identifier mapping is initially empty. The
@scheme[namespace-require] function, in contrast, directly imports the
given module into the current namespace.  Starting with
@scheme[(namespace-require 'scheme/base)] would introduce a binding
for @schemeidfont{require} and make a subsequent @scheme[(eval
'(require my-dsl))] work. The above is better, not only because it is
more compact, but also because it avoids introducing bindings that are
not part of the domain-specific languages.

@; ----------------------------------------

@subsection{Sharing Data and Code Across Namespaces}

Modules not attached to a new namespace will be loaded and
instantiated afresh if they are demanded by evaluation. For example,
@schememodname[scheme/base] does not include
@schememodname[scheme/class], and loading @schememodname[scheme/class]
again will create a distinct class datatype:

@interaction[
(require scheme/class)
(class? object%)
(class?
 (parameterize ([current-namespace (make-base-empty-namespace)])
   (namespace-require 'scheme/class) (code:comment @#,t{loads again})
   (eval 'object%)))
]

For cases when dynamically loaded code needs to share more code and
data with its context, use the @scheme[namespace-attach-module]
function. The first argument to @scheme[namespace-attach-module] is a
source namespace from which to draw a module instance; in some cases,
the current namespace is known to include the module that needs to be
shared:

@interaction[
(require scheme/class)
(class?
 (let ([ns (make-base-empty-namespace)])
   (namespace-attach-module (current-namespace)
                            'scheme/class
                            ns)
   (parameterize ([current-namespace ns])
     (namespace-require 'scheme/class) (code:comment @#,t{uses attached})
     (eval 'object%))))
]

Within a module, however, the combination of
@scheme[define-namespace-anchor] and
@scheme[namespace-anchor->empty-namespace] offers a more reliable
method for obtaining a source namespace:

@schememod[
scheme/base

(require scheme/class)

(define-namespace-anchor a)

(define (load-plug-in file)
  (let ([ns (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace a)
                             'scheme/class
                              ns)
    (parameterize ([current-namespace ns])
      (dynamic-require file 'plug-in%))))
]

The anchor bound by @scheme[namespace-attach-module] connects the
run time of a module with the namespace in which a module is loaded
(which might differ from the current namespace).  In the above
example, since the enclosing module requires
@schememodname[scheme/class], the namespace produced by
@scheme[namespace-anchor->empty-namespace] certainly contains an
instance of @schememodname[scheme/class]. Moreover, that instance is
the same as the one imported into the module, so the class datatype is
shared.

@; ----------------------------------------------------------------------

@section[#:tag "load"]{Scripting Evaluation and Using @scheme[load]}

Historically, Scheme and Lisp systems did not offer module
systems. Instead, large programs were built by essentially scripting
the @tech{REPL} to evaluate program fragments in a particular order.
While @tech{REPL} scripting turns out to be a bad way to structure
programs and libraries, it is still sometimes a useful capability.

@margin-note{Describing a program via @scheme[load] interacts
especially badly with macro-defined language extensions
@cite["Flatt02"].}

The @scheme[load] function runs a @tech{REPL} script by
@scheme[read]ing S-expressions from a file, one by one, and passing
them to @scheme[eval]. If a file @filepath{place.scm} contains

@schemeblock[
(define city "Salt Lake City")
(define state "Utah")
(printf "~a, ~a\n" city state)
]

then it can be loaded in a @tech{REPL}:

@interaction[
(eval:alts (load "place.scm") (begin (define city "Salt Lake City")
                                     (printf "~a, Utah\n" city)))
city
]

Since @scheme[load] uses @scheme[eval], however, a module like the
following generally will not work---for the same reasons described in
@secref["namespaces"]:

@schememod[
scheme

(define there "Utopia")

(load "here.scm")
]

The current namespace for evaluating the content of
@filepath{here.scm} is likely to be empty; in any case, you cannot get
@scheme[there] from @filepath{here.scm}. Also, any definitions in
@filepath{here.scm} will not become visible for use within the module;
after all, the @scheme[load] happens dynamically, while references to
identifiers within the module are resolved lexically, and therefore
statically.

Unlike @scheme[eval], @scheme[load] does not accept a namespace
argument. To supply a namespace to @scheme[load], set the
@scheme[current-namespace] parameter. The following example evaluates
the expressions in @filepath{here.scm} using the bindings of the
@schememodname[scheme/base] module:

@schememod[
scheme

(parameterize ([current-namespace (make-base-namespace)])
  (load "here.scm"))
]

You can even use @scheme[namespace-anchor->namespace] to make the
bindings of the enclosing module accessible for dynamic evaluation. In
the following example, when @filepath{here.scm} is @scheme[load]ed, it
can refer to @scheme[there] as well as the bindings of
@schememodname[scheme]:

@schememod[
scheme

(define there "Utopia")

(define-namespace-anchor a)
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (load "here.scm"))
]

Still, if @filepath{here.scm} defines any identifiers, the definitions
cannot be directly (i.e., statically) referenced by in the enclosing
module.

The @schememodname[scheme/load] module language is different from
@schememodname[scheme] or @schememodname[scheme/base]. A module using
@schememodname[scheme/load] treats all of its content as dynamic,
passing each form in the module body to @scheme[eval] (using a
namespace that is initialized with @schememodname[scheme]). As a
result, uses of @scheme[eval] and @scheme[load] in the module body see
the same dynamic namespace as immediate body forms. For example, if
@filepath{here.scm} contains

@schemeblock[
(define here "Morporkia")
(define (go!) (set! here there))
]

then running

@schememod[
scheme/load

(define there "Utopia")

(load "here.scm")

(go!)
(printf "~a\n" here)
]

prints ``Utopia''.

Drawbacks of using @schememodname[scheme/load] include reduced
error checking, tool support, and performance. For example, with the
program

@schememod[
scheme/load

(define good 5)
(printf "running\n")
good
bad
]

DrScheme's @onscreen{Check Syntax} tool cannot tell that the second
@scheme[good] is a reference to the first, and the unbound reference
to @scheme[bad] is reported only at run time instead of rejected
syntactically.
