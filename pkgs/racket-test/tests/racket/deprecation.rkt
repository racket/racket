#lang racket/base

(require (for-syntax racket/base
                     racket/deprecation/transformer)
         racket/deprecation
         rackunit
         syntax/macro-testing
         syntax/parse/define)


(module mod racket/base
  (require racket/deprecation)
  (provide a b)
  (define a 42)
  (define-deprecated-alias b a))


(require (prefix-in mod: 'mod))


(define-syntax-rule (catch e)
  (with-handlers ([(λ (_) #true) values])
    e
    #false))


(test-case "define-deprecated-alias"

  (test-case "deprecated alias of a constant"
    (define a 42)
    (define-deprecated-alias b a)
    (check-equal? b 42))

  (test-case "deprecated alias of a function"
    (define (f x y)
      (+ x y))
    (define-deprecated-alias g f)
    (check-equal? (g 1 2) 3))

  (test-case "deprecated alias of a macro"
    (define-syntax-parse-rule (m a b c)
      (list a b c))
    (define-deprecated-alias m2 m)
    (check-equal? (m2 1 2 3) (list 1 2 3)))

  (define-syntax-parse-rule (is-deprecated? id:id)
    #:do [(define-values (transformer _)
            (syntax-local-value/immediate #'id (λ () (values #false #false))))]
    #:with result #`'#,(deprecated-alias? transformer)
    result)
  
  (test-case "deprecated alias can be inspected at compile time"
    (define a 42)
    (define-deprecated-alias b a)
    (check-false (is-deprecated? a))
    (check-true (is-deprecated? b)))

  (test-case "deprecated alias provided by module can be inspected at compile time"
    (check-false (is-deprecated? mod:a))
    (check-true (is-deprecated? mod:b)))

  (test-case "deprecated alias of unbound identifier is a syntax error"
    (define thrown (catch (convert-syntax-error (let () (define-deprecated-alias a x) (void)))))
    (check-pred exn:fail:syntax? thrown)
    (with-check-info (['thrown thrown])
      (check-regexp-match #rx"define-deprecated-alias: " (exn-message thrown))
      (check-regexp-match #rx"target identifier" (exn-message thrown))
      (define expr-datums (map syntax->datum (exn:fail:syntax-exprs thrown)))
      (check-equal? expr-datums '(x (define-deprecated-alias a x)))))

  (test-case "module-level deprecated alias can be defined before target"
    (check-not-exn
     (λ ()
       (eval
        #'(module foo racket/base
            (require racket/deprecation)
            (define-deprecated-alias a x)
            (define x 42))))))

  (test-case "module-level deprecated alias can be defined after target"
    (check-not-exn
     (λ ()
       (eval
        #'(module foo racket/base
            (require racket/deprecation)
            (define x 42)
            (define-deprecated-alias a x))))))

  (test-case "top-level deprecated alias cannot be defined before target"
    (define thrown (catch (eval #'(begin (define-deprecated-alias a x) (define x 42)))))
    (check-pred exn:fail:syntax? thrown)
    (with-check-info (['thrown thrown])
      (check-regexp-match #rx"define-deprecated-alias: " (exn-message thrown))
      (check-regexp-match #rx"target identifier" (exn-message thrown))
      (define expr-datums (map syntax->datum (exn:fail:syntax-exprs thrown)))
      (check-equal? expr-datums '(x (define-deprecated-alias a x)))))

  (test-case "top-level deprecated alias can be defined after target"
    (check-not-exn (λ () (eval #'(begin (define x 42) (define-deprecated-alias a x))))))

  (test-case "deprecated alias in definition context within module can be defined before target"
    (check-not-exn
     (λ ()
       (eval
        #'(module foo racket/base
            (require racket/deprecation)
            (define (f)
              (define-deprecated-alias a x)
              (define x 42)
              a))))))

  (test-case "deprecated alias in definition context within module can be defined after target"
    (check-not-exn
     (λ ()
       (eval
        #'(module foo racket/base
            (require racket/deprecation)
            (define (f)
              (define x 42)
              (define-deprecated-alias a x)
              a))))))

  (test-case "deprecated alias in definition context at top-level can be defined before target"
    (check-not-exn
     (λ ()
       (eval
        #'(define (f)
            (define-deprecated-alias a x)
            (define x 42)
            a)))))

  (test-case "deprecated alias in definition context at top-level can be defined after target"
    (check-not-exn
     (λ ()
       (eval
        #'(define (f)
            (define x 42)
            (define-deprecated-alias a x)
            a))))))
