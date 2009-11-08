#lang scribble/manual
@(require scribble/eval
          (for-label unstable/struct
                     scheme/contract
                     scheme/base))

@title[#:tag "struct"]{Structs}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/struct))

@defmodule[unstable/struct]

@defform[(make struct-id expr ...)]{

Creates an instance of @scheme[struct-id], which must be bound as a
struct name. The number of @scheme[expr]s is statically checked
against the number of fields associated with @scheme[struct-id]. If
they are different, or if the number of fields is not known, an error
is raised at compile time.

@examples[#:eval the-eval
  (define-struct triple (a b c))
  (make triple 3 4 5)
  (make triple 2 4)
]

}
