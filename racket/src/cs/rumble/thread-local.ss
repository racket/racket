;; Use `define-thread-local` for an immutable variable containing a
;; mutable value where the value can be created lazily and isn't #f.
;; Unlike place-local values, thread-local values are meant to work
;; form arbitrary Scheme threads (i.e., not just those created to
;; exist within some place).

(define-virtual-register thread-local-table #f)

(define NUM-THREAD-LOCALS 64)
(meta define thread-local-counter 64)

(define-syntax (define-thread-local stx)
  (syntax-case stx ()
    [(_ id rhs)
     (let ([pos (sub1 thread-local-counter)])
       (set! thread-local-counter pos)
       (when (negative? pos)
         (error 'define-thread-local "out of thread-local slots"))
       (with-syntax ([pos (#%datum->syntax #'here pos)])
         #'(begin
             (define (init) rhs)
             (define-syntax (id stx)
               (...
                (syntax-case stx ()
                  [(id arg ...) #'((thread-local-ref pos init) arg ...)]
                  [_ #'(thread-local-ref pos init)]))))))]))

(define (thread-local-ref i init)
  (let ([vec (thread-local-table)])
    (cond
     [(#%vector? vec)
      (let ([v (#%vector-ref vec i)])
        (or v
            (with-interrupts-disabled
             (let ([v (#%vector-ref vec i)])
               (or v
                   (let ([v (init)])
                     (#%vector-set! vec i v)
                     v))))))]
     [else
      (with-interrupts-disabled
       (thread-local-table (#%make-vector NUM-THREAD-LOCALS #f)))
      (thread-local-ref i init)])))
