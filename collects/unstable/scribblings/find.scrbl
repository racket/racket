#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label unstable/find racket/contract racket/shared racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/find racket/shared))

@title[#:tag "find"]{Find}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/find]

@defproc[(find [pred (-> any/c any/c)]
               [x any/c]
               [#:stop-on-found? stop-on-found? any/c #f]
               [#:stop stop (or/c #f (-> any/c any/c)) #f]
               [#:get-children get-children (or/c #f (-> any/c (or/c #f list?))) #f])
         list?]{

Returns a list of all values satisfying @racket[pred] contained in
@racket[x] (possibly including @racket[x] itself). 

If @racket[stop-on-found?] is true, the children of values satisfying
@racket[pred] are not examined. If @racket[stop] is a procedure, then
the children of values for which @racket[stop] returns true are not
examined (but the values themselves are; @racket[stop] is applied
after @racket[pred]). Only the current branch of the search is
stopped, not the whole search.

The search recurs through pairs, vectors, boxes, and the accessible
fields of structures. If @racket[get-children] is a procedure, it can
override the default notion of a value's children by returning a list
(if it returns false, the default notion of children is used).

No cycle detection is done, so @racket[find] on a cyclic graph may
diverge. To do cycle checking yourself, use @racket[stop] and a
mutable table.

@examples[#:eval the-eval
(find symbol? '((all work) and (no play)))
(find list? '#((all work) and (no play)) #:stop-on-found? #t)
(find negative? 100 
      #:stop-on-found? #t
      #:get-children (lambda (n) (list (- n 12))))
(find symbol? (shared ([x (cons 'a x)]) x)
      #:stop (let ([table (make-hasheq)])
               (lambda (x)
                 (begin0 (hash-ref table x #f)
                         (hash-set! table x #t)))))
]
}

@defproc[(find-first [pred (-> any/c any/c)]
                     [x any/c]
                     [#:stop stop (or/c #f (-> any/c any/c)) #f]
                     [#:get-children get-children (or/c #f (-> any/c (or/c #f list?))) #f]
                     [#:default default any/c (lambda () (error ....))])
         any/c]{

Like @racket[find], but only returns the first match. If no
matches are found, @racket[default] is applied as a thunk if it is a
procedure or returned otherwise.

@examples[#:eval the-eval
(find-first symbol? '((all work) and (no play)))
(find-first list? '#((all work) and (no play)))
(find-first negative? 100 
            #:get-children (lambda (n) (list (- n 12))))
(find-first symbol? (shared ([x (cons 'a x)]) x))
]
}

@close-eval[the-eval]
