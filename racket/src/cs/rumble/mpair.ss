(define-record mpair (car cdr))

(define (mcons a b)
  (make-mpair a b))

(define/who (mcar m)
  (check who mpair? m)
  (mpair-car m))

(define/who (mcdr m)
  (check who mpair? m)
  (mpair-cdr m))

(define/who (set-mcar! m v)
  (check who mpair? m)
  (set-mpair-car! m v))

(define/who (set-mcdr! m v)
  (check who mpair? m)
  (set-mpair-cdr! m v))

(define (unsafe-mcar m)
  (mpair-car m))

(define (unsafe-mcdr m)
  (mpair-cdr m))

(define (unsafe-set-mcar! m v)
  (set-mpair-car! m v))

(define (unsafe-set-mcdr! m v)
  (set-mpair-cdr! m v))

(define (set-mpair-hash!)
  (record-type-equal-procedure (record-type-descriptor mpair)
                               (lambda (a b eql?)
                                 (and (eql? (mcar a) (mcar b))
                                      (eql? (mcdr a) (mcdr b)))))
  (record-type-hash-procedure (record-type-descriptor mpair)
                              (lambda (a hc)
                                (hash-code-combine (hc (mcar a))
                                                   (hc (mcar a))))))
