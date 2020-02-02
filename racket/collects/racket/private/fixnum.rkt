(module fixnum '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%require '#%flfxnum)
  (#%provide fixnum-for-every-system?)

  ;; Smallest number of bits used for a fixnum across Racket
  ;; implementation is 30 bits.

  (define-values (fixnum-for-every-system?)
    (lambda (v)
      (if (fixnum? v)
          (if (fx>= v -536870912)
              (fx<= v 536870911)
              #f)
          #f))))
