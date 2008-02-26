#lang scheme/base

(provide with-exception-handler
         guard else =>
         (rename-out [r6rs:raise raise])
         raise-continuable)

(define-struct (exn:continuable exn:fail) (base continuation))

(define (with-exception-handler proc thunk)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-type-error 'with-exception-handler "procedure (arity 1)" proc))
  (unless (and (procedure? thunk)
               (procedure-arity-includes? thunk 0))
    (raise-type-error 'with-exception-handler "procedure (arity 0)" thunk))
  (call-with-exception-handler
   (lambda (exn)
     (let/ec esc
       (call-with-exception-handler
        (lambda (new-exn) 
          ;; Chain to enclosing handler by returning:
          (esc new-exn))
        (lambda () 
          (call-with-values (lambda () (proc (if (exn:continuable? exn)
                                                 (exn:continuable-base exn)
                                                 exn)))
            (if (continuable? exn)
                (lambda args
                  ((continuable-continuation exn) (lambda () (apply values args))))
                (lambda args
                  (error 'raise
                         "when handling a non-continuable exception, exception handler returned~a"
                         (if (null? args)
                             " (no values)"
                             (apply
                              string-append
                              ":"
                              (let loop ([args args][n 10])
                                (cond
                                 [(null? args) null]
                                 [(zero? n)
                                  (list " ...")]
                                 [else
                                  (cons (format " ~e" (car args))
                                        (loop (cdr args) (sub1 n)))]))))))))))))
   thunk))

(define (continuable? exn)
  (or (exn:break? exn)
      (exn:continuable? exn)))

(define (continuable-continuation exn)
  (if (exn:break? exn)
      (exn:break-continuation exn)
      (exn:continuable-continuation exn)))

(define-syntax-rule (guard (id cond-clause ...) body0 body ...)
  (with-handlers ([(lambda (x) #t)
                   (lambda (id)
                     (let ([id (if (exn:continuable? id)
                                   (exn:continuable-base id)
                                   id)])
                       (cond
                        cond-clause ...
                        [else (raise id)])))])
    body0 body ...))

(define (r6rs:raise exn)
  ;; No barrier
  (raise exn #t))

(define (raise-continuable exn)
  ((let/cc cont
     (r6rs:raise
      (make-exn:continuable
       (if (exn? exn) (exn-message exn) "continuable exception")
       (if (exn? exn) (exn-continuation-marks exn) (current-continuation-marks))
       exn
       cont)))))
