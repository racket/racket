#lang racket/base
(require racket/class
         "interfaces.rkt"
         "../util/stxobj.rkt")
(provide new-bound-partition
         identifier=-choices)

(define (new-bound-partition)
  (new bound-partition%))

;; bound-partition%
(define bound-partition%
  (class* object% (partition<%>)

    ;; simplified : hash[(listof nat) => nat]
    (define simplified (make-hash))

    ;; next-number : nat
    (define next-number 0)

    (define/public (get-partition stx)
      (let ([marks (get-marks stx)])
        (or (hash-ref simplified marks #f)
            (let ([n next-number])
              (hash-set! simplified marks n)
              (set! next-number (add1 n))
              n))))

    (define/public (same-partition? a b)
      (= (get-partition a) (get-partition b)))

    (define/public (count)
      next-number)

    (get-partition (datum->syntax #f 'nowhere))
    (super-new)))

;; ==== Identifier relations ====

(define identifier=-choices
  (make-parameter
   `(("<nothing>" . #f)
     ("bound-identifier=?"  . ,bound-identifier=?)
     ("free-identifier=?" . ,free-identifier=?))))
