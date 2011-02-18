#lang racket/base

;; Simple implementation by Eli Barzilay,
;;   (compatible names to the srfi version.)

(provide s:set! setter set-setter! getter-with-setter)

(require (for-syntax racket/base))

(define setters (make-weak-hasheq)) ; weak => usable for local functions

(define (setter proc)
  (hash-ref setters proc
            (lambda () (error 'setter "could not find a setter for ~e" proc))))

(define (set-setter! proc setter)
  ;; it seems better to throw an error if a setter already exists, but I
  ;; didn't do that to keep it compatible with the original srfi code.
  (hash-set! setters proc setter))

(define-syntax (s:set! stx)
  (syntax-case stx ()
    [(s:set! (E0 E1 ...) V) #'((setter E0) E1 ... V)]
    [(s:set! x V) (identifier? #'x) #'(set! x V)]))

(define (getter-with-setter getter setter)
  ;; I don't see any reason why the sample version returns a wrapped
  ;; getter function, it seems like it would kill any chance of inlining
  ;; with any compiler (eg, the resulting arity can be different).  In
  ;; fact, I don't see any reason for this thing at all...  (Keeping it
  ;; just to be compatible...)
  (set-setter! getter setter)
  getter)

;; Initialize the table
(for ([x (in-list `([,setter     ,set-setter!]
                    [,vector-ref ,vector-set!]
                    [,string-ref ,string-set!]
                    [,mcar       ,set-mcar!]
                    [,mcdr       ,set-mcdr!]
                    [,hash-ref   ,hash-set!]))])
  (set-setter! (car x) (cadr x)))
