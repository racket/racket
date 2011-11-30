#lang scribble/doc
@(require scribble/manual "common.rkt"
          (for-label racket/base
                     compiler/decompile
                     (only-in compiler/zo-parse compilation-top? req)
                     compiler/zo-marshal))

@title[#:tag "decompile"]{@exec{raco decompile}: Decompiling Bytecode}

The @exec{raco decompile} command takes a bytecode file (which usually
 has the file extension @filepath{.zo}) or a source file with an
 associated bytecode file (usually created with @exec{raco make}) and
 converts it back to an approximation of Racket code. Decompiled
 bytecode is mostly useful for checking the compiler's transformation
 and optimization of the source program.

Many forms in the decompiled code, such as @racket[module],
 @racket[define], and @racket[lambda], have the same meanings as
 always. Other forms and transformations are specific to the rendering
 of bytecode, and they reflect a specific execution model:

@itemize[

@item{Top-level variables, variables defined within the module, and
 variables imported from other modules are prefixed with @litchar{_},
 which helps expose the difference between uses of local variables
 versus other variables. Variables imported from other modules,
 moreover, have a suffix that indicates the source module.

 Non-local variables are always accessed indirectly though an implicit
 @racketidfont{#%globals} or @racketidfont{#%modvars} variable that
 resides on the value stack (which otherwise contains local
 variables). Variable accesses are further wrapped with
 @racketidfont{#%checked} when the compiler cannot prove that the
 variable will be defined before the access.

 Uses of core primitives are shown without a leading @litchar{_}, and
 they are never wrapped with @racketidfont{#%checked}.}

@item{Local-variable access may be wrapped with
 @racketidfont{#%sfs-clear}, which indicates that the variable-stack
 location holding the variable will be cleared to prevent the
 variable's value from being retained by the garbage collector.
 Variables whose name starts with @racketidfont{unused} are never
 actually stored on the stack, and so they never have
 @racketidfont{#%sfs-clear} annotations. (The bytecode compiler
 normally eliminates such bindings, but sometimes it cannot, either
 because it cannot prove that the right-hand side produces the right
 number of values, or the discovery that the variable is unused
 happens too late with the compiler.)

 Mutable variables are converted to explicitly boxed values using
 @racketidfont{#%box}, @racketidfont{#%unbox}, and
 @racketidfont{#%set-boxes!} (which works on multiple boxes at once).
 A @racketidfont{set!-rec-values} operation constructs
 mutually-recursive closures and simultaneously updates the
 corresponding variable-stack locations that bind the closures.  A
 @racketidfont{set!}, @racketidfont{set!-values}, or
 @racketidfont{set!-rec-values} form is always used on a local
 variable before it is captured by a closure; that ordering reflects
 how closures capture values in variable-stack locations, as opposed
 to stack locations.}

@item{In a @racket[lambda] form, if the procedure produced by the
 @racket[lambda] has a name (accessible via @racket[object-name])
 and/or source-location information, then it is shown as a quoted
 constant at the start of the procedure's body. Afterward, if the
 @racket[lambda] form captures any bindings from its context, those
 bindings are also shown in a quoted constant. Neither constant
 corresponds to a computation when the closure is called, though the
 list of captured bindings corresponds to a closure allocation when
 the @racket[lambda] form itself is evaluated.

 A @racket[lambda] form that closes over no bindings is wrapped with
 @racketidfont{#%closed} plus an identifier that is bound to the
 closure. The binding's scope covers the entire decompiled output, and
 it may be referenced directly in other parts of the program; the
 binding corresponds to a constant closure value that is shared, and
 it may even contain cyclic references to itself or other constant
 closures.}

@item{A form @racket[(#%apply-values _proc _expr)] is equivalent to
 @racket[(call-with-values (lambda () _expr) _proc)], but the run-time
 system avoids allocating a closure for @racket[_expr].}

@item{A @racket[define-values] form may have @racket[(begin
 '%%inline-variant%% _expr1 _expr2)] for its expression, in which case
 @racket[_expr2] is the normal result, but @racket[_expr1] may be
 inlined for calls to the definition from other modules. Definitions
 of functions without an @racket['%%inline-variant%%] are never
 inlined across modules.}

@item{Some applications of core primitives are annotated with
 @racketidfont{#%in}, which indicates that the JIT compiler will
 inline the operation. (Inlining information is not part of the
 bytecode, but is instead based on an enumeration of primitives that
 the JIT is known to handle specially.) Operations from
 @racketmodname[racket/flonum] and @racketmodname[racket/unsafe/ops]
 are always inlined, so @racketidfont{#%in} is not shown for them.}

@item{Some applications of flonum operations from @racketmodname[racket/flonum] 
 and @racketmodname[racket/unsafe/ops] are annotated with
 @racketidfont{#%flonum}, indicating a place where the JIT compiler
 might avoid allocation for intermediate flonum results. A single
 @racketidfont{#%flonum} by itself is not useful, but a
 @racketidfont{#%flonum} operation that consumes a
 @racketidfont{#%flonum} or @racketidfont{#%from-flonum} argument
 indicates a potential performance improvement. A
 @racketidfont{#%from-flonum} wraps an identifier that is bound by
 @racket[let] with a @racketidfont{#%as-flonum} around its value,
 which indicates a local binding that can avoid boxing (when used as
 an argument to an operation that can work with unboxed values).}

@item{A @racketidfont{#%decode-syntax} form corresponds to a syntax
 object.}

]

@; ------------------------------------------------------------

@section{API for Decompiling}

@defmodule[compiler/decompile]

@defproc[(decompile [top compilation-top?]) any/c]{

Consumes the result of parsing bytecode and returns an S-expression
(as described above) that represents the compiled code.}

@; ------------------------------------------------------------

@include-section["zo-parse.scrbl"]

@; ------------------------------------------------------------

@section{API for Marshaling Bytecode}

@defmodule[compiler/zo-marshal]

@defproc[(zo-marshal-to [top compilation-top?] [out output-port?]) void?]{

Consumes a representation of bytecode and writes it to @racket[out].}

@defproc[(zo-marshal [top compilation-top?]) bytes?]{

Consumes a representation of bytecode and generates a byte string for
the marshaled bytecode.}

@; ------------------------------------------------------------

@include-section["zo-struct.scrbl"]

