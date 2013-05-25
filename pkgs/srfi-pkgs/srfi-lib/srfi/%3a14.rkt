#lang scheme
(require
 (rename-in srfi/14
            (list->char-set srfi14:list->char-set)
            (list->char-set! srfi14:list->char-set!)
            (char-set->list srfi14:char-set->list))
 scheme/mpair)

(provide char-set?

         char-set=
         char-set<=
         char-set-hash
         char-set-cursor
         char-set-ref
         char-set-cursor-next
         end-of-char-set?
         char-set-fold
         char-set-unfold
         char-set-unfold!
         char-set-for-each
         char-set-map
         char-set-copy
         char-set
         list->char-set
         list->char-set!
         string->char-set
         string->char-set!
         char-set-filter
         char-set-filter!
         ucs-range->char-set
         ucs-range->char-set!
         ->char-set
         char-set->list
         char-set->string
         char-set-size
         char-set-count
         char-set-contains?
         char-set-every
         char-set-any
         char-set-adjoin
         char-set-adjoin!
         char-set-delete
         char-set-delete!
         char-set-complement
         char-set-complement!
         char-set-union
         char-set-union!
         char-set-intersection
         char-set-intersection!
         char-set-difference
         char-set-difference!
         char-set-xor
         char-set-xor!
         char-set-diff+intersection
         char-set-diff+intersection!
         char-set:lower-case
         char-set:upper-case
         char-set:title-case
         char-set:letter
         char-set:digit
         char-set:letter+digit
         char-set:graphic
         char-set:printing
         char-set:whitespace
         char-set:iso-control
         char-set:punctuation
         char-set:symbol
         char-set:hex-digit
         char-set:blank
         char-set:ascii
         char-set:empty
         char-set:full)

(define list->char-set
  (case-lambda
    ((char-list base-cs)
     (srfi14:list->char-set (mlist->list char-list) base-cs))
    ((char-list)
     (srfi14:list->char-set (mlist->list char-list)))))

(define (list->char-set! char-list base-cs)
  (list->char-set char-list base-cs))

(define (char-set->list cs)
  (list->mlist (srfi14:char-set->list cs)))
