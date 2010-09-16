#lang scribble/manual
@(require scribble/eval
          (for-label data/skip-list
                     racket/contract
                     racket/dict
                     racket/base))

@title[#:tag "skip-list"]{Skip Lists}

@(define the-eval (make-base-eval))
@(the-eval '(require data/skip-list))
@(the-eval '(require racket/dict))

@defmodule[data/skip-list]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Skip lists are a simple, efficient data structure for mutable
dictionaries with totally ordered keys. They were described in the
paper ``Skip Lists: A Probabilistic Alternative to Balanced Trees'' by
William Pugh in Communications of the ACM, June 1990, 33(6) pp668-676.

A skip-list is a dictionary (@racket[dict?] from
@racketmodname[racket/dict]). It also supports extensions of the
dictionary interface for iterator-based search and mutation.

@defproc[(make-skip-list [=? (any/c any/c . -> . any/c)]
                         [<? (any/c any/c . -> . any/c)]
                         [#:key-contract key-contract contract? any/c]
                         [#:value-contract value-contract contract? any/c])
         skip-list?]{

Makes a new empty skip-list. The skip-list uses @racket[=?] and
@racket[<?] to order keys.

@examples[#:eval the-eval
(define skip-list (make-skip-list = <))
(skip-list-set! skip-list 3 'apple)
(skip-list-set! skip-list 6 'cherry)
(dict-map skip-list list)
(skip-list-ref skip-list 3)
(skip-list-remove! skip-list 6)
(skip-list-count skip-list)
]
}

@defproc[(skip-list? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a skip-list, @racket[#f]
otherwise.

}

@deftogether[[
@defproc[(skip-list-ref [skip-list skip-list?]
                        [key any/c]
                        [default any/c (lambda () (error ....))])
         any/c]
@defproc[(skip-list-set! [skip-list skip-list?]
                         [key any/c]
                         [value any/c])
         void?]
@defproc[(skip-list-remove! [skip-list skip-list?]
                            [key any/c])
         void?]
@defproc[(skip-list-count [skip-list skip-list?])
         exact-nonnegative-integer?]
@defproc[(skip-list-iterate-first [skip-list skip-list?])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-next [skip-list skip-list?]
                                 [iter skip-list-iter?])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-key [skip-list skip-list?]
                                [iter skip-list-iter?])
         any/c]
@defproc[(skip-list-iterate-value [skip-list skip-list?]
                                  [iter skip-list-iter?])
         any/c]]]{

Implementations of @racket[dict-ref], @racket[dict-set!],
@racket[dict-remove!], @racket[dict-count],
@racket[dict-iterate-first], @racket[dict-iterate-next],
@racket[dict-iterate-key], and @racket[dict-iterate-value],
respectively.
}

@deftogether[[
@defproc[(skip-list-iterate-greatest/<? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-greatest/<=? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-least/>? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-least/>=? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]]]{

Return the position of, respectively, the greatest key less than
@racket[key], the greatest key less than or equal to @racket[key], the
least key greater than @racket[key], and the least key greater than or
equal to @racket[key].
}

@deftogether[[
@defproc[(skip-list-iterate-set-key! [skip-list skip-list?]
                                     [iter skip-list-iter?]
                                     [key any/c])
         void?]
@defproc[(skip-list-iterate-set-value! [skip-list skip-list?]
                                       [iter skip-list-iter?]
                                       [value any/c])
         void?]]]{

Set the key and value, respectively, at the position @racket[iter] in
@racket[skip-list].

@bold{Warning:} Changing a position's key to be less than its
predecessor's key or greater than its successor's key results in an
out-of-order skip-list, which may cause comparison-based operations to
behave incorrectly.
}

@defproc[(skip-list-iter? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] represents a position in a
skip-list, @racket[#f] otherwise.
}
