#lang scribble/manual
@(require scribble/eval
          "utils.ss"
          (for-label unstable/skip-list
                     scheme/contract
                     scheme/dict
                     scheme/base))

@title[#:tag "skip-list"]{Skip Lists}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/skip-list))
@(the-eval '(require scheme/dict))

@defmodule[unstable/skip-list]

@unstable[@author+email["Ryan Culpepper" "ryanc@plt-scheme.org"]]

Skip lists are a simple, efficient data structure for mutable
dictionaries with totally ordered keys. They were described in the
paper ``Skip Lists: A Probabilistic Alternative to Balanced Trees'' by
William Pugh in Communications of the ACM, June 1990, 33(6) pp668-676.

A skip-list is a dictionary (@scheme[dict?] from
@schememodname[scheme/dict]). It also supports extensions of the
dictionary interface for iterator-based search and mutation.

@defproc[(make-skip-list [=? (any/c any/c . -> . any/c)]
                         [<? (any/c any/c . -> . any/c)])
         skip-list?]{

Makes a new empty skip-list. The skip-list uses @scheme[=?] and @scheme[<?] to order keys.

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

Returns @scheme[#t] if @scheme[v] is a skip-list, @scheme[#f]
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

Implementations of @scheme[dict-ref], @scheme[dict-set!],
@scheme[dict-remove!], @scheme[dict-count],
@scheme[dict-iterate-first], @scheme[dict-iterate-next],
@scheme[dict-iterate-key], and @scheme[dict-iterate-value],
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
@scheme[key], the greatest key less than or equal to @scheme[key], the
least key greater than @scheme[key], and the least key greater than or
equal to @scheme[key].
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

Set the key and value, respectively, at the position @scheme[iter] in
@scheme[skip-list].

@bold{Warning:} Changing a position's key to be less than its
predecessor's key or greater than its successor's key results in an
out-of-order skip-list, which may cause comparison-based operations to
behave incorrectly.
}

@defproc[(skip-list-iter? [v any/c])
         boolean?]{

Returns @scheme[#t] if @scheme[v] represents a position in a
skip-list, @scheme[#f] otherwise.
}
