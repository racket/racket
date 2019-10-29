#lang racket/base
(require "check.rkt")

(provide current-plumber
         make-plumber
         plumber?
         plumber-flush-all
         plumber-flush-all/wrap
         plumber-add-flush!
         plumber-flush-handle?
         plumber-flush-handle-remove!
         plumber-callbacks)

(struct plumber (callbacks ; hash table of handles -> callbacks
                 weak-callbacks) ; same, but weak references
  #:authentic)

(define (make-plumber)
  (plumber (make-hasheq)
           (make-weak-hasheq)))

(define/who current-plumber
  (make-parameter (make-plumber)
                  (lambda (v)
                    (check who plumber? v)
                    v)
                  'current-plumber))

(struct plumber-flush-handle (plumber))

(define/who (plumber-add-flush! p proc [weak? #f])
  (check who plumber? p)
  (check who (procedure-arity-includes/c 1) proc)
  (define h (plumber-flush-handle p))
  (hash-set! (if weak?
                 (plumber-weak-callbacks p)
                 (plumber-callbacks p))
             h
             proc)
  h)

(define/who (plumber-flush-all p)
  (check who plumber? p)
  (plumber-flush-all/wrap p (lambda (proc h) (proc h))))

(define (plumber-flush-all/wrap p app)
  ;; Spec requires getting all callbacks before running any
  (define procs+hs
    (for*/list ([cbs (in-list (list (plumber-callbacks p)
                                    (plumber-weak-callbacks p)))]
                [(h proc) (in-hash cbs)])
      (cons proc h)))
  (for ([proc+h (in-list procs+hs)])
    (app (car proc+h) (cdr proc+h))))

(define/who (plumber-flush-handle-remove! h)
  (check who plumber-flush-handle? h)
  (define p (plumber-flush-handle-plumber h))
  (hash-remove! (plumber-callbacks p) h)
  (hash-remove! (plumber-weak-callbacks p) h))
