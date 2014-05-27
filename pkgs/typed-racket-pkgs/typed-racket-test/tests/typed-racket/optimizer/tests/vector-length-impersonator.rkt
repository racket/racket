#;#;
#<<END
END
#<<END
passed

END

#lang typed/racket

(: x (Vectorof Number))
(define x (make-vector 0 0))
(: y (Vectorof Float))
(define z (cast x '#()))
(define y (cast z (Vectorof Float)))

;; should error
(with-handlers ([exn:fail:contract?
                 (lambda: ([e : exn])
                   (when (regexp-match "index is out of range for empty vector"
                                       (exn-message e))
                     (display "passed\n")))])
  (vector-set! y 1 3.0))
