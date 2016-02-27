(module test-harness racket
  (require syntax/stx rackunit)
              
  (provide (all-defined-out))

  (define (lst-bound-id=? x y)
    (andmap bound-identifier=? x y))
  
  (define (stx-bound-id=? x y)
    (cond 
      ((and (syntax? x) (eq? '_ (syntax-e x)))
       #t)
      ((and (syntax? x)
            (vector? (syntax-e x))
            (= 2 (vector-length (syntax-e x))))
       (and (identifier? y)
            (eq? (syntax-e (vector-ref (syntax-e x) 0))
                 (free-identifier=? (vector-ref (syntax-e x) 1) y))))
      ((and (stx-null? x) (stx-null? y))
       #t)
      ((and (stx-pair? x) (stx-pair? y))
       (and (stx-bound-id=? (stx-car x) (stx-car y))
            (stx-bound-id=? (stx-cdr x) (stx-cdr y))))
      ((and (identifier? x) (identifier? y))
       (if (bound-identifier=? x y)
           #t
           (begin
             (log-error "Differ:\n  ~s\n  ~s" x y)
             #f)))
      ((and (syntax? x) (number? (syntax-e x))
            (syntax? y) (number? (syntax-e y)))
       (= (syntax-e x) (syntax-e y)))
      (else #f)))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_ err expr)
       (check-exn
        (lambda (e) (and (exn:fail:syntax? e)
                         (regexp-match? (regexp-quote err)
                                        (exn-message e))))
        (lambda () (expand #'expr))))))
  
  (define-syntax test-runtime-error
    (syntax-rules ()
      ((_ err-pred err expr)
       (check-exn
        (λ (exn) (and (err-pred exn)
                      (let ([msg (exn-message exn)])
                        (and (regexp-match? (regexp-quote err) msg)))))
        (λ () expr (void))))))
  
  (define-syntax test
    (syntax-rules ()
      ((_ expected-value expr)
       (check-equal? expected-value expr))
      ((_ cmp expected-value expr)
       (check cmp expected-value expr)))))
