#lang racket

(require ffi/unsafe)

(provide make-transient-box transient-box?
         (rename-out [transient-box-value* transient-box-value])
         transient-box-touch!)

(define (register-gc-callback f)
  (define v (make-vector 0))
  (register-finalizer v (λ (x) (when (f) (register-gc-callback f)))))

(struct transient-box (value counter max-counter touched?) #:mutable #:transparent)

(struct no-value-struct ())
(define no-value (no-value-struct))

(define (make-transient-box value)
  (define bx (transient-box value 1 1 #f))
  (register-gc-callback
   (λ ()
     (define cnt (transient-box-counter bx))
     (cond [(cnt . <= . 0)
            (cond [(transient-box-touched? bx)
                   (define max-cnt (* 2 (transient-box-max-counter bx)))
                   (set-transient-box-counter! bx max-cnt)
                   (set-transient-box-max-counter! bx max-cnt)
                   (set-transient-box-touched?! bx #f)
                   #t]
                  [else
                   (set-transient-box-value! bx no-value)
                   #f])]
           [else
            (set-transient-box-counter! bx (- cnt 1))
            #t])))
  bx)

(define (transient-box-value* bx [gced-value #f])
  (define value (transient-box-value bx))
  (if (eq? value no-value) gced-value value))

(define (transient-box-touch! bx)
  (set-transient-box-touched?! bx #t))

#|
(define bx (make-transient-box (make-vector 0)))
(transient-box-value* bx)
bx
(collect-garbage)
bx
(transient-box-value* bx)
bx
(collect-garbage)
bx
(collect-garbage)
bx
|#
