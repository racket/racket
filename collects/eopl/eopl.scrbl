#lang scribble/doc
@(require scribble/manual
          (for-label eopl/eopl
                     scheme/contract
                     (only-in scheme printf pretty-print)))

@(define-syntax-rule (def-mz id)
   (begin
     (require (for-label mzscheme))
     (define id (scheme provide))))
@(def-mz mzscheme-provide)

@title{@italic{Essentials of Programming Languages} Language}

The @italic{Essentials of Programming Languages} language in DrScheme
provides all of the functions of R5RS (see @schememodname[r5rs]), plus
the forms and procedures described below. It is intended for use with
the textbook @cite["EoPL"].

Differences from the book:

@itemize{

  @item{Datatypes must be defined before they are used in
   @scheme[cases] expressions. This constraint enables better and
   earlier error reporting.

   Some examples in the book's code (or at least the code distributed
   for the book) must be changed by moving datatype definitions
   earlier.}

  @item{The @scheme[sllgen:] functions have been changed to syntactic
   forms. This change is also related to better error reporting.
  
   All examples in the book work with the @scheme[sllgen:] forms.}
}

@defmodule[eopl/eopl]

@defform[(define-datatype id predicate-id 
           (variant-id (field-id predicate-expr) ...) 
           ...)]{

  Defines the datatype @scheme[id] and a function
  @scheme[predicate-id] that returns @scheme[#t] for instances of the
  datatype, and @scheme[#f] for any other value.

  Each @scheme[variant-id] is defined as a constructor function that
  creates an instance of the datatype; the constructor takes as many
  arguments as the variant's @scheme[field-id]s, and each argument is
  checked by applying the function produced by the variant's
  @scheme[predicate-expr].

  In DrScheme v209 and older, when constructor-based printing was
  used, variant instances were printed with a @scheme[make-] prefix
  before the variant name.  Thus, for compatibility, in addition to
  @scheme[variant-id], @scheme[make-variant-id] is also defined for
  each @scheme[variant-id] (to the same constructor as
  @scheme[variant-id]).}

@defform*[#:literals (else)
         [(cases datatype-id expr 
            (variant-id (field-id ...) result-expr ...) 
            ...)
          (cases datatype-id expr 
            (variant-id (field-id ...) result-expr ...) 
            ... 
            (else result-expr ...))]]{

  Branches on the datatype instance produced by @scheme[expr], which
  must be an instance of the specified @scheme[datatype-id]
  (previously defined with @scheme[define-datatype]).}

@deftogether[(
@defidform[sllgen:make-string-scanner]
@defidform[sllgen:make-string-parser]
@defidform[sllgen:make-stream-parser]
@defidform[sllgen:make-define-datatypes]
@defidform[sllgen:show-define-datatypes]
@defidform[sllgen:list-define-datatypes])]{

  Defined in the textbook's Appendix A @cite["EoPL"]. However, the
  DrScheme versions are syntactic forms, instead of procedures, and
  the arguments must be either quoted literal tables or identifiers
  that are defined (at the top level) to quoted literal tables.}

@defthing[sllgen:make-rep-loop procedure?]{

  Defined in the @italic{EoPL} textbook's Appendix A @cite["EoPL"]
  (and still a function).}

@defthing[eopl:error procedure?]{

  As in the book.}

@deftogether[(
  @defproc[(eopl:printf (form string?) (v any/c) ...) void?]
  @defproc[(eopl:pretty-print (v any/c) (port output-port? (current-output-port))) void?])]{

  Same as PLT Scheme's @scheme[printf] and @scheme[pretty-print].}

@deftogether[(
  @defproc[((list-of (pred (any/c . -> . any)) ...+) (x any/c)) boolean?]
  @defproc[(always? (x any/c)) boolean?])]{

  As in the book @cite["EoPL"].}

@defthing[empty empty?]{

  The empty list.}

@defform[(time expr)]{

  Evaluates @scheme[expr], and prints timing information before returning the
  result.}

@defproc[(collect-garbage) void?]{

  Performs a garbage collection (useful for repeatable timings).}

@deftogether[(
  @defform[(trace id ...)]
  @defform[(untrace id ...)])]{

  For debugging: @scheme[trace] redefines each @scheme[id] at the top
  level (bound to a procedure) so that it prints arguments on entry
  and results on exit. The @scheme[untrace] form reverses the action
  of @scheme[trace] for the given @scheme[id]s.

  Tracing a function causes tail-calls in the original function to
  become non-tail calls.}

@defform[(provide provide-spec ...)]{

  Useful only with a module that uses @schememodname[eopl/eopl] as a
  language: exports identifiers from the module. See @mzscheme-provide
  from @schememodname[mzscheme] for more information.}

@defthing[eopl:error-stop (-> any/c)]{

  Defined only in the top-level namespace (i.e., not in a module);
  mutate this variable to install an exception-handling
  thunk. Typically, the handler thunk escapes through a continuation.

  The @schememodname[eopl/eopl] library sets this variable to
  @scheme[#f] in the current namespace when it executes.}

@defproc[(install-eopl-exception-handler) void?]{

  Sets an exception handler to one that checks
  @scheme[eopl:error-stop].

  The @schememodname[eopl/eopl] library calls this function when it
  executes.}


@(bibliography

  (bib-entry #:key "EoPL"
             #:title @elem{@italic{Essentials of Programming Languages}, Second Edition}
             #:location "MIT Press"
             #:date "2001")

)
