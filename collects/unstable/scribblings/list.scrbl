#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  "utils.ss"
          (for-label scheme/dict
                     unstable/list
                     syntax/id-table
                     scheme/contract
                     scheme/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/list))

@title[#:tag "list"]{Lists}

@defmodule[unstable/list]

@unstable-header[]

@defproc[(list-prefix? [l list?]
                       [r list?])
         boolean?]{
 True if @scheme[l] is a prefix of @scheme[r].
@examples[#:eval the-eval
(list-prefix? '(1 2) '(1 2 3 4 5))
]
}

@addition{Sam Tobin-Hochstadt}

@defproc[(filter-multiple [l list?] [f procedure?] ...) (values list? ...)]{
Produces @scheme[(values (filter f l) ...)].

@examples[#:eval the-eval
(filter-multiple (list 1 2 3 4 5) even? odd?)
]
}

@defproc[(extend [l1 list?] [l2 list?] [v any/c]) list?]{
Extends @scheme[l2] to be as long as @scheme[l1] by adding @scheme[(-
(length l1) (length l2))] copies of @scheme[v] to the end of
@scheme[l2].   

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

Returns the first duplicate item in @scheme[lst]. More precisely, it
returns the first @scheme[_x] such that there was a previous
@scheme[_y] where @scheme[(same? (extract-key _x) (extract-key _y))].

The @scheme[same?] argument can either be an equivalence predicate
such as @scheme[equal?] or @scheme[eqv?] or a dictionary. In the
latter case, the elements of the list are mapped to @scheme[#t] in the
dictionary until an element is discovered that is already mapped to a
true value. The procedures @scheme[equal?], @scheme[eqv?], and
@scheme[eq?] automatically use a dictionary for speed.

@(the-eval '(require syntax/id-table scheme/dict))
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

Produces lists of the respective values of @scheme[f] applied to the elements in
@scheme[lst ...] sequentially.

@defexamples[
#:eval the-eval
(map/values
 3
 (lambda (x)
   (values (+ x 1) x (- x 1)))
 (list 1 2 3))
]

}

