;; Use `define-thread-local` for an immutable variable containing a
;; mutable value where the value can be created lazily and isn't #f

(define-syntax-rule (define-thread-local id rhs)
  (begin
    (define cell (make-thread-parameter #f))
    (define (init) rhs)
    (define-syntax (id stx)
      (...
       (syntax-case stx ()
         [(id arg ...) #'((thread-local-ref cell init) arg ...)]
         [_ #'(thread-local-ref cell init)])))))
    
(define (thread-local-ref c init)
  (let ([v (c)])
    (or v
        (with-interrupts-disabled
         (let ([v (c)])
           (or v
               (let ([v (init)])
                 (c v)
                 v)))))))
