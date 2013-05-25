#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base unstable/struct racket/contract))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/struct))

@title[#:tag "struct"]{Structs}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/struct]

@defform[(make struct-id expr ...)]{

Creates an instance of @racket[struct-id], which must be bound as a
struct name. The number of @racket[expr]s is statically checked
against the number of fields associated with @racket[struct-id]. If
they are different, or if the number of fields is not known, an error
is raised at compile time.

@examples[#:eval the-eval
  (define-struct triple (a b c))
  (make triple 3 4 5)
  (make triple 2 4)
]
}

@defproc[(struct->list [v any/c]
                       [#:on-opaque on-opaque (or/c 'error 'return-false 'skip) 'error])
         (or/c list? #f)]{

Returns a list containing the struct instance @racket[v]'s
fields. Unlike @racket[struct->vector], the struct name itself is not
included.

If any fields of @racket[v] are inaccessible via the current inspector
the behavior of @racket[struct->list] is determined by
@racket[on-opaque]. If @racket[on-opaque] is @racket['error] (the
default), an error is raised. If it is @racket['return-false],
@racket[struct->list] returns @racket[#f]. If it is @racket['skip],
the inaccessible fields are omitted from the list.

@examples[#:eval the-eval
(define-struct open (u v) #:transparent)
(struct->list (make-open 'a 'b))
(struct->list #s(pre 1 2 3))
(define-struct (secret open) (x y))
(struct->list (make-secret 0 1 17 22))
(struct->list (make-secret 0 1 17 22) #:on-opaque 'return-false)
(struct->list (make-secret 0 1 17 22) #:on-opaque 'skip)
(struct->list 'not-a-struct #:on-opaque 'return-false)
(struct->list 'not-a-struct #:on-opaque 'skip)
]
}

@close-eval[the-eval]
