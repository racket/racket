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

@defproc[(struct->list [v any/c]
                       [#:false-on-opaque? false-on-opaque? boolean? #f])
         (or/c list? #f)]{

Returns a list containing the struct instance @scheme[v]'s
fields. Unlike @scheme[struct->vector], the struct name itself is not
included.

The struct instance @scheme[v] must be fully accessible using the
current inspector. If any fields are inaccessible, either an error is
raised or @scheme[#f] is returned, depending on the value of
@scheme[false-on-opaque?]. The default is to raise an error.

@examples[#:eval the-eval
(define-struct open (u v) #:transparent)
(struct->list (make-open 'a 'b))
(struct->list #s(pre 1 2 3))
(define-struct secret (x y))
(struct->list (make-secret 17 22))
(struct->list (make-secret 17 22) #:false-on-opaque? #t)
(struct->list 'not-a-struct #:false-on-opaque? #t)
]
}
