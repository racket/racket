(module inexact-complex typed/scheme #:optimize
  (require racket/unsafe/ops)
  (let ((t1 1.0+2.0i)
        (t2 2.0+4.0i))
    (unsafe-make-flrectangular
     (unsafe-fl+ (unsafe-flreal-part t1)
                 (unsafe-flreal-part t2))
     (unsafe-fl+ (unsafe-flimag-part t1)
                 (unsafe-flimag-part t2))))
  (let ((t1 1.0+2.0i)
        (t2 2.0+4.0i))
    (unsafe-make-flrectangular
     (unsafe-fl- (unsafe-flreal-part t1)
                 (unsafe-flreal-part t2))
     (unsafe-fl- (unsafe-flimag-part t1)
                 (unsafe-flimag-part t2)))))
