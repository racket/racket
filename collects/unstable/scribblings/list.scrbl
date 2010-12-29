#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  "utils.rkt"
          (for-label racket/dict
                     unstable/list
                     syntax/id-table
                     racket/contract
                     racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/list))

@title[#:tag "list"]{Lists}

@defmodule[unstable/list]

@unstable-header[]

@defproc[(list-prefix? [l list?]
                       [r list?])
         boolean?]{
 True if @racket[l] is a prefix of @racket[r].
@examples[#:eval the-eval
(list-prefix? '(1 2) '(1 2 3 4 5))
]
}

@addition{Sam Tobin-Hochstadt}

@defproc[(filter-multiple [l list?] [f procedure?] ...) (values list? ...)]{
Produces @racket[(values (filter f l) ...)].

@examples[#:eval the-eval
(filter-multiple (list 1 2 3 4 5) even? odd?)
]
}

@defproc[(extend [l1 list?] [l2 list?] [v any/c]) list?]{
Extends @racket[l2] to be as long as @racket[l1] by adding @racket[(-
(length l1) (length l2))] copies of @racket[v] to the end of
@racket[l2].   

@examples[#:eval the-eval
(extend '(1 2 3) '(a) 'b)
]}


@addition{Ryan Culpepper}

@defproc[(check-duplicate [lst list?]
                          [#:key extract-key (-> any/c any/c) (lambda (x) x)]
                          [#:same? same?
                                   (or/c (any/c any/c . -> . any/c)
                                         dict?)
                                   equal?])
         (or/c any/c #f)]{

Returns the first duplicate item in @racket[lst]. More precisely, it
returns the first @racket[_x] such that there was a previous
@racket[_y] where @racket[(same? (extract-key _x) (extract-key _y))].

The @racket[same?] argument can either be an equivalence predicate
such as @racket[equal?] or @racket[eqv?] or a dictionary. In the
latter case, the elements of the list are mapped to @racket[#t] in the
dictionary until an element is discovered that is already mapped to a
true value. The procedures @racket[equal?], @racket[eqv?], and
@racket[eq?] automatically use a dictionary for speed.

@(the-eval '(require syntax/id-table racket/dict))
@examples[#:eval the-eval
(check-duplicate '(1 2 3 4))
(check-duplicate '(1 2 3 2 1))
(check-duplicate '((a 1) (b 2) (a 3)) #:key car)
(define id-t (make-free-id-table))
(check-duplicate (syntax->list #'(a b c d a b))
                 #:same? id-t)
(dict-map id-t list)
]
}

                         
@addition{Carl Eastlund}

@defproc[(map/values [n natural-number/c]
                     [f (-> A ... (values B_1 ... B_n))]
                     [lst (listof A)]
                     ...)
         (values (listof B_1) ... (listof B_n))]{

Produces lists of the respective values of @racket[f] applied to the elements in
@racket[lst ...] sequentially.

@defexamples[
#:eval the-eval
(map/values
 3
 (lambda (x)
   (values (+ x 1) x (- x 1)))
 (list 1 2 3))
]

}

@addition{David Van Horn}

@defproc[(remf [pred procedure?]
               [lst list?])
         list?]{
Returns a list that is like @racket[lst], omitting the first element of @racket[lst] 
for which @racket[pred] produces a true value.

@defexamples[
#:eval the-eval
(remf negative? '(1 -2 3 4 -5))
]

}

@close-eval[the-eval]
