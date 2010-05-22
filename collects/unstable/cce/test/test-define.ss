#lang scheme

(require scheme/sandbox
         "checks.ss"
         "../define.ss")

(provide define-suite)

(define define-suite
  (test-suite "define.ss"

    (test-suite "block"
      (test
       (block
        (define (f x y) (both x y))
        (define-match-expander both
          (syntax-rules () [(_ a b) (struct pair [a b])])
           (syntax-rules () [(_ a b) (make-pair a b)]))
        (define-struct pair [x y] #:transparent)
        (check-equal? (f 1 2) (make-pair 1 2)))))

    (test-suite "at-end")

    (test-suite "define-if-unbound"
      (test
       (let ()
         (define-if-unbound very-special-name 1)
         (define-if-unbound very-special-name 2)
         (check-equal? very-special-name 1)))
      (test
       (let ()
         (define-if-unbound (very-special-function) 1)
         (define-if-unbound (very-special-function) 2)
         (check-equal? (very-special-function) 1))))

    (test-suite "define-values-if-unbound"
      (test
       (let ()
         (define-values-if-unbound [very-special-name] 1)
         (define-values-if-unbound [very-special-name] 2)
         (check-equal? very-special-name 1))))

    (test-suite "define-syntax-if-unbound"
      (test
       (let ()
         (define-syntax-if-unbound very-special-macro
           (lambda (stx) #'(quote 1)))
         (define-syntax-if-unbound very-special-macro
           (lambda (stx) #'(quote 2)))
         (check-equal? (very-special-macro) 1)))
      (test
       (let ()
         (define-syntax-if-unbound (very-special-macro stx)
           #'(quote 1))
         (define-syntax-if-unbound (very-special-macro stx)
           #'(quote 2))
         (check-equal? (very-special-macro) 1))))

    (test-suite "define-syntaxes-if-unbound"
      (test
       (let ()
         (define-syntaxes-if-unbound [very-special-macro]
           (lambda (stx) #'(quote 1)))
         (define-syntaxes-if-unbound [very-special-macro]
           (lambda (stx) #'(quote 2)))
         (check-equal? (very-special-macro) 1))))

    (test-suite "define-renamings"
      (test
       (let ()
         (define-renamings [with define] [fun lambda])
         (with f (fun (x) (add1 x)))
         (check-equal? (f 7) 8))))

    (test-suite "declare-names"
      (test
       (let ()
         (declare-names x y z)
         (define-values [x y z] (values 1 2 3))
         (check-equal? x 1)
         (check-equal? y 2)
         (check-equal? z 3))))

    (test-suite "define-with-parameter"
      (test
       (let ()
         (define p (make-parameter 0))
         (define-with-parameter with-p p)
         (with-p 7 (check-equal? (p) 7)))))

    (test-suite "define-single-definition"
      (test
       (let ()
         (define-single-definition with define-values)
         (with x 0)
         (check-equal? x 0))))

    (test-suite "in-phase1")
    (test-suite "in-phase1/pass2")))
