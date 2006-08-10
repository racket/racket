(define (gen ignored)
  (local ((define-struct local-struct (local-field))
          (define silly-def (+ 3 4)))
    (make-local-struct 13)))

(define p (gen 1))
(define q (gen 1))
