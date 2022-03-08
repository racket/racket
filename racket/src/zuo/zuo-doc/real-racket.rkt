#lang at-exp racket/base
(require scribble/manual
         (for-syntax racket/base)
         (for-label racket/base
                    racket/contract/base
                    racket/cmdline))

(provide realracket
         realracket*
         (for-label any/c
                    listof
                    ->))

(define-syntax (realracket stx)
  (syntax-case stx ()
    [(_ id) @#`racket[#,(datum->syntax #'here (syntax-e #'id))]]))

(define-syntax (realracket* stx)
  (syntax-case stx ()
    [(_ id) @#'realracket[id]]
    [(_ id1 id2) @#'elem{@realracket[id1] and @realracket[id2]}]
    [(_ id1 id2 id3) @#'elem{@realracket[id1], @realracket[id2], and @realracket[id3]}]
    [(_ id0 id ...) @#'elem{@realracket[id0], @realracket*[id ...]}]))


