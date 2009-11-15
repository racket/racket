#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  "utils.ss"
         (for-label unstable/list
                    scheme/contract
                    scheme/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/list))

@title[#:tag "list"]{Lists}

@defmodule[unstable/list]

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

@examples[#:eval the-eval]
(extend '(1 2 3) '(a) 'b)
}