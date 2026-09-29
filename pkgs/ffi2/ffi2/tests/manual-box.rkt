#lang racket/base
(require ffi2
         rackunit)

(let ()
  (define mbx (ffi2-malloc-manual-box 'a))
  (check-equal? (ffi2-manual-box-ref mbx) 'a)
  (check-equal? (ffi2-manual-box-set! mbx 'b) (void))
  (check-equal? (ffi2-manual-box-ref mbx) 'b)
  (check-equal? (ffi2-free-manual-box mbx) (void)))

(let ()
  (define mbx (ffi2-malloc-manual-box #f))
  (check-equal? (ffi2-manual-box-ref mbx) #f)
  (check-equal? (ffi2-manual-box-set! mbx #t) (void))
  (check-equal? (ffi2-manual-box-ref mbx) #t)
  (check-equal? (ffi2-free-manual-box mbx) (void)))

(let ()
  (define mbx (ffi2-malloc-manual-box #f))
  (collect-garbage 'minor)
  (define pr (cons 1 2))
  (check-equal? (ffi2-manual-box-set! mbx pr) (void))
  (check-eq? (ffi2-manual-box-ref mbx) pr)
  (collect-garbage 'minor)
  (check-eq? (ffi2-manual-box-ref mbx) pr)
  (check-equal? (ffi2-free-manual-box mbx) (void)))
