#lang racket/base
(require racket/unsafe/undefined
         "config.rkt")

(define-values (prop:mine mine? mine-ref)
  (make-struct-type-property 'mine))

(struct posn (x [y #:mutable])
  #:property prop:mine #t)

(struct posn3D posn ([z #:mutable]))

(struct posn/u (x [y #:mutable])
  #:property prop:mine #t
  #:property prop:chaperone-unsafe-undefined '(y x))

(struct posn3D/u posn/u ([z #:mutable])
  #:property prop:chaperone-unsafe-undefined '(z y x))

(define pt* (posn 1 2))
(define pt3D* (posn3D 1 2 3))

(define pt*/u (posn/u 1 2))
(define pt3D*/u (posn3D/u 1 2 3))

(define pt (chaperone-struct pt*
                             posn-x (lambda (self v) v)
                             set-posn-y! (lambda (self v) v)
                             mine-ref (lambda (self v) v)))

(define pt3D (chaperone-struct pt3D*
                               posn-x (lambda (self v) v)
                               posn3D-z (lambda (self v) v)
                               set-posn-y! (lambda (self v) v)
                               set-posn3D-z! (lambda (self v) v)
                               mine-ref (lambda (self v) v)))

(define pt/u (chaperone-struct pt*/u
                               posn/u-x (lambda (self v) v)
                               set-posn/u-y! (lambda (self v) v)
                               mine-ref (lambda (self v) v)))

(define pt3D/u (chaperone-struct pt3D*/u
                                 posn/u-x (lambda (self v) v)
                                 posn3D/u-z (lambda (self v) v)
                                 set-posn/u-y! (lambda (self v) v)
                                 set-posn3D/u-z! (lambda (self v) v)
                                 mine-ref (lambda (self v) v)))

'----------------------------------------

'chaperone
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (chaperone-struct pt*
                      posn-x (lambda (self v) v)))))

'chaperone-more
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (chaperone-struct pt
                      posn-x (lambda (self v) v)))))

'chaperone-prop
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (chaperone-struct pt*
                      mine-ref (lambda (self v) v)))))

'baseline-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn-x pt*))))

'baseline-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn-y! pt* 8)))

'baseline-sub-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn3D-z pt3D*))))

'baseline-sub-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn3D-z! pt3D* 9)))

'baseline-prop-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (mine-ref pt*))))

'undefined-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn/u-x pt*/u))))

'undefined-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn/u-y! pt*/u 8)))

'undefined-prop-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (mine-ref pt*/u))))

'chaperoned-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn-x pt))))

'chaperoned-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn-y! pt 8)))

'chaperoned-sub-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn3D-z pt3D))))

'chaperoned-sub-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn3D-z! pt3D 9)))

'chaperoned-prop-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (mine-ref pt))))

'chaperoned-undefined-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (posn/u-x pt/u))))

'chaperoned-undefined-set!
(times
 (for/fold ([r #f]) ([i (in-range M)])
   (set-posn/u-y! pt/u 8)))

'chaperoned-undefined-prop-ref
(times
 (void
  (for/fold ([r #f]) ([i (in-range M)])
    (mine-ref pt/u))))
