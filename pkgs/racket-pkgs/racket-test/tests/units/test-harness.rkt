(module test-harness mzscheme
  (require syntax/stx)
              
  (provide (all-defined))

  (define (lst-bound-id=? x y)
    (andmap bound-identifier=? x y))
  
  (define (stx-bound-id=? x y)
    (cond 
      ((and (syntax? x) (eq? '_ (syntax-e x)))
       #t)
      ((and (stx-pair? x)
            (not (syntax-e (stx-car x)))
            (identifier? (stx-cdr x)))
       (and (identifier? y)
            (not (module-identifier=? (stx-cdr x) y))))
      ((and (stx-null? x) (stx-null? y))
       #t)
      ((and (stx-pair? x) (stx-pair? y))
       (and (stx-bound-id=? (stx-car x) (stx-car y))
            (stx-bound-id=? (stx-cdr x) (stx-cdr y))))
      ((and (identifier? x) (identifier? y))
       (bound-identifier=? x y))
      ((and (syntax? x) (number? (syntax-e x))
            (syntax? y) (number? (syntax-e y)))
       (= (syntax-e x) (syntax-e y)))
      (else #f)))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_ err expr)
       (with-handlers ((exn:fail:syntax? (lambda (exn)
                                           (printf "get expected syntax error \"~a\"\n  got message \"~a\"\n\n"
                                                   err
                                                   (exn-message exn)))))
         (expand #'expr)
         (error 'test-syntax-error "expected syntax error \"~a\" on ~a, got none" err 'expr)))))
  
  (define-syntax test-runtime-error
    (syntax-rules ()
      ((_ err-pred err expr)
       (with-handlers ((err-pred (lambda (exn)
                                   (printf "got expected runtime error \"~a\"\n  got message \"~a\"\n\n"
                                           err
                                           (exn-message exn)))))
         expr
         (error 'test-runtime-error "expected runtime error \"~a\" on ~a, got none" err 'expr)))))
  
  (define-syntax test
    (syntax-rules ()
      ((_ expected-value expr)
       (test equal? expected-value expr))
      ((_ cmp expected-value expr)
       (let ((v expr))
         (unless (cmp expected-value v)
           (error 'test "expected ~a to evaluate to ~a, got ~a" 'expr 'expected-value v)))))))
