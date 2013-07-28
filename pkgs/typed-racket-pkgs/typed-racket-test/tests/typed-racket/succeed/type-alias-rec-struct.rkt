#lang typed/racket/base
;; Iteratee
(define-type (Alias A D) (U #f (Main A D)))

(struct: (A D) Main
  ([resume : (Alias A D)])) ;
