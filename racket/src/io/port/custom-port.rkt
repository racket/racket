#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "input-port.rkt"
         "output-port.rkt")

;; Common functionality for "custom-input-port.rkt" and
;; "custom-output-port.rkt"

(provide make-get-location

         check-init-position
         make-init-offset+file-position

         check-buffer-mode
         make-buffer-mode)


;; in atomic mode
(define (make-get-location user-get-location)
  (lambda (self)
    (end-atomic)
    (call-with-values
     (lambda () (user-get-location))
     (case-lambda
       [(line col pos) 
        (unless (or (not line) (exact-positive-integer? line))
          (raise-result-error '|user port get-location| "(or/c #f exact-positive-integer?)" line))
        (unless (or (not line) (exact-nonnegative-integer? col))
          (raise-result-error '|user port get-location| "(or/c #f exact-nonnegative-integer?)" col))
        (unless (or (not line) (exact-positive-integer? pos))
          (raise-result-error '|user port get-location| "(or/c #f exact-positive-integer?)" pos))
        (start-atomic)
        (values line col pos)]
       [args
        (apply raise-arity-error '|user port get-location return| 3 args)]))))

(define (check-init-position who user-init-position)  
  (check who (lambda (p) (or (exact-positive-integer? p)
                             (input-port? p)
                             (output-port? p)
                             (not p)
                             (and (procedure? p) (procedure-arity-includes? p 0))))
         #:contract "(or/c exact-positive-integer? port? #f (procedure-arity-includes/c 0))"
         user-init-position))

(define (make-init-offset+file-position user-init-position)
  (define init-offset
    (if (or (procedure? user-init-position)
            (input-port? user-init-position)
            (output-port? user-init-position)
            (not user-init-position))
        #f
        (sub1 user-init-position)))

  (define file-position
    (cond
      [(input-port? user-init-position) user-init-position]
      [(output-port? user-init-position) user-init-position]
      [(procedure? user-init-position)
       (lambda (self)
         (define pos (user-init-position))
         (unless (or (not pos) (exact-positive-integer? pos))
           (raise-result-error '|user port init-position| "(or/c exact-positive-integer? #f)" pos))
         (and pos (sub1 pos)))]
      [else #f]))

  (values init-offset file-position))

(define (check-buffer-mode who user-buffer-mode)
  (check who (lambda (p) (or (not p)
                             (and (procedure? p)
                                  (procedure-arity-includes? p 0)
                                  (procedure-arity-includes? p 1))))
         #:contract (string-append "(or/c #f (and/c (procedure-arity-includes/c 0)\n"
                                   "                (procedure-arity-includes/c 1)))")
         user-buffer-mode))

(define (make-buffer-mode user-buffer-mode #:output? [output? #f])
  (case-lambda
    [(self)
     (end-atomic)
     (define m (user-buffer-mode))
     (cond
       [(or (not m) (eq? m 'block) (eq? m 'none) (and output? (eq? m 'line)))
        (start-atomic)
        m]
       [else
        (raise-result-error '|user port buffer-mode|
                            (if output?
                                "(or/c 'block 'line 'none #f)"
                                "(or/c 'block 'none #f)")
                            m)])]
    [(self m)
     (non-atomically
      (user-buffer-mode m))]))
