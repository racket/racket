#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; top level
(make-polar 1.0 1.0)

;; nested
(let ((p (+ 1.0+2.0i (make-polar 2.0 4.0))))
  (string-append (real->decimal-string (real-part p) 10)
                 (real->decimal-string (imag-part p) 10)))
