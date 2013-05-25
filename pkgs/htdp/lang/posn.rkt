#lang racket/base

(require lang/private/signature-syntax)

;; The posn struct for the teaching languages
(provide struct:posn make-posn posn? posn-x posn-y set-posn-x! set-posn-y!
         (rename-out (posn posn-id))
	 (rename-out (posn-signature posn)))

(struct posn (x y) #:mutable #:transparent)

;; We define a separate function so tha it has the 
;; name `make-posn':
(define (make-posn x y) (posn x y))

(define posn-signature (signature (predicate posn?)))
