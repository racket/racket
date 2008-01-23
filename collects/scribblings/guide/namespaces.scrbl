#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require[scheme/class]
@require["guide-utils.ss"]

@title[#:tag "reflection"]{Reflection and Dynamic Evaluation}

Scheme is a @italic{dynamic} language. It offers numerous facilities
for loading, compiling, and even constructing new code at run
time.

@; ----------------------------------------------------------------------

@section{Namespaces}

Dynamic evaluation requires a @deftech{namespace}, which encapsulates
two pieces of information:

@itemize{

 @item{A mapping from identifiers to bindings. For example, a
       namespace might map the identifier @schemeidfont{lambda} to the
       @scheme[lambda] form. An ``empty'' namespace is one that maps
       every identifier to an uninitialized top-level variable.}

 @item{A mapping from module names to module declarations and
       instances.}

}

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

Informally, the term @defterm{namespace} is sometimes used
interchangeably with @defterm{environment} or @defterm{scope}. In PLT
Scheme, the term @defterm{namespace} has the more specific, dynamic
meaning given above, and it should not be confused with static lexical
concepts.

@; ----------------------------------------------------------------------

@section{Creating and Installing Namespaces}

A @tech{namespace} is a first-class value.  Some functions, such as
@scheme[eval], accept an optional namespace argument. More often, the
namespace used by a dynamic operation is the @deftech{current
namespace} as determined by the @scheme[current-namespace] parameter.

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
from an existing namespace. Attaching a module adjust the mapping of
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

@; ----------------------------------------------------------------------

@section{Sharing Data and Code Across Namespaces}

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
   (namespace-require 'scheme/class) (code:comment #, @t{loads again})
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
     (namespace-require 'scheme/class) (code:comment #, @t{uses attached})
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

The anchor bound by @scheme[namespace-attach-module] connects the the
run time of a module with the namespace in which a module is loaded
(which might differ from the current namespace).  In the above
example, since the enclosing module requires
@schememodname[scheme/class], the namespace produced by
@scheme[namespace-anchor->empty-namespace] certainly contains an
instance of @schememodname[scheme/class]. Moreover, that instance is
the same as the one imported into the module, so the class datatype is
shared.

@;{

@; ----------------------------------------------------------------------

@section{The Top Level is Hopeless}



@; ----------------------------------------------------------------------

@section{Guidelines on @scheme[eval], @scheme[load], and @scheme[dynamic-require]}

These dynamic features are powerful tools, but they are also easily
misused. This section provides some general guidelines on using
dynamic features.

@itemize{

 @item{If you find a use for @scheme[eval] or @scheme[load], then it's
       probably an abuse. In any case, don't expect the environment
       for dynamically evaluated code to have anything to do with the
       environment of a call to @scheme[eval] or @scheme[load].}

 @item{If you find a use for @scheme[dynamic-require], such as for a
      plug-in architecture or to delay loading code, then it's likely
      a fine use.}

 @item{When using functions like @scheme[eval], @scheme[load], or
       @scheme[dynamic-require], take care to install an appropriate
       @tech{namespace}.}

}

}
