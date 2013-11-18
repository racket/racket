#;#;
#<<END
TR opt: make-polar.rkt 28:6 (make-polar 1.0 1.0) -- make-polar
TR opt: make-polar.rkt 28:6 (make-polar 1.0 1.0) -- make-rectangular elimination
TR opt: make-polar.rkt 31:12 1.0+2.0i -- unboxed literal
TR opt: make-polar.rkt 31:21 (make-polar 2.0 4.0) -- make-rectangular elimination
TR opt: make-polar.rkt 31:6 (p (+ 1.0+2.0i (make-polar 2.0 4.0))) -- unboxed let bindings
TR opt: make-polar.rkt 31:9 (+ 1.0+2.0i (make-polar 2.0 4.0)) -- unboxed binary float complex
TR opt: make-polar.rkt 32:39 (real-part p) -- complex accessor elimination
TR opt: make-polar.rkt 32:50 p -- leave var unboxed
TR opt: make-polar.rkt 33:39 (imag-part p) -- complex accessor elimination
TR opt: make-polar.rkt 33:50 p -- leave var unboxed
END
#<<END
"-0.3070.486"

END



#lang typed/scheme
#:optimize




;; top level
(void (make-polar 1.0 1.0))

;; nested
(let ((p (+ 1.0+2.0i (make-polar 2.0 4.0))))
  (string-append (real->decimal-string (real-part p) 3)
                 (real->decimal-string (imag-part p) 3)))
