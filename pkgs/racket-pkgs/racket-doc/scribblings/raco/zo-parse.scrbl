#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     racket/contract
                     compiler/zo-parse
                     racket/set))

@(define-syntax-rule (defstruct+ id fields . rest)
   (defstruct id fields #:prefab . rest))

@title{API for Parsing Bytecode}

@defmodule[compiler/zo-parse]

The @racketmodname[compiler/zo-parse] module re-exports
@racketmodname[compiler/zo-structs] in addition to
@racket[zo-parse].

@defproc[(zo-parse [in input-port? (current-input-port)]) compilation-top?]{
  Parses a port (typically the result of opening a @filepath{.zo} file)
  containing bytecode.  Beware that the structure types used to
  represent the bytecode are subject to frequent changes across Racket
  versons.

  The parsed bytecode is returned in a @racket[compilation-top]
  structure.  For a compiled module, the @racket[compilation-top]
  structure will contain a @racket[mod] structure.  For a top-level
  sequence, it will normally contain a @racket[seq] or @racket[splice]
  structure with a list of top-level declarations and expressions.

  The bytecode representation of an expression is closer to an
  S-expression than a traditional, flat control string.  For example, an
  @racket[if] form is represented by a @racket[branch] structure that
  has three fields: a test expression, a ``then'' expression, and an
  ``else'' expression.  Similarly, a function call is represented by an
  @racket[application] structure that has a list of argument
  expressions.

  Storage for local variables or intermediate values (such as the
  arguments for a function call) is explicitly specified in terms of a
  stack.  For example, execution of an @racket[application] structure
  reserves space on the stack for each argument result.  Similarly, when
  a @racket[let-one] structure (for a simple @racket[let]) is executed,
  the value obtained by evaluating the right-hand side expression is
  pushed onto the stack, and then the body is evaluated.  Local
  variables are always accessed as offsets from the current stack
  position.  When a function is called, its arguments are passed on the
  stack.  A closure is created by transferring values from the stack to
  a flat closure record, and when a closure is applied, the saved values
  are restored on the stack (though possibly in a different order and
  likely in a more compact layout than when they were captured).

  When a sub-expression produces a value, then the stack pointer is
  restored to its location from before evaluating the sub-expression.
  For example, evaluating the right-hand size for a @racket[let-one]
  structure may temporarily push values onto the stack, but the stack is
  restored to its pre-@racket[let-one] position before pushing the
  resulting value and continuing with the body.  In addition, a tail
  call resets the stack pointer to the position that follows the
  enclosing function's arguments, and then the tail call continues by
  pushing onto the stack the arguments for the tail-called function.

  Values for global and module-level variables are not put directly on
  the stack, but instead stored in ``buckets,'' and an array of
  accessible buckets is kept on the stack.  When a closure body needs to
  access a global variable, the closure captures and later restores the
  bucket array in the same way that it captured and restores a local
  variable.  Mutable local variables are boxed similarly to global
  variables, but individual boxes are referenced from the stack and
  closures.

  Quoted syntax (in the sense of @racket[quote-syntax]) is treated like
  a global variable, because it must be instantiated for an appropriate
  phase.  A @racket[prefix] structure within a @racket[compilation-top]
  or @racket[mod] structure indicates the list of global variables and
  quoted syntax that need to be instantiated (and put into an array on
  the stack) before evaluating expressions that might use them.}
