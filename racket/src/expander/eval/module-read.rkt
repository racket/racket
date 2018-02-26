#lang racket/base
(require "../syntax/api.rkt"
         "main.rkt"
         "reflect.rkt"
         "../namespace/api.rkt"
         "../read/primitive-parameter.rkt")

(provide with-module-reading-parameterization
         raise-wrong-module-name
         check-module-form)

(define (with-module-reading-parameterization thunk)
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #t]
                 [read-accept-compiled #t]
                 ;; Would be set by `call-with-default-reading-parameterization`,
                 ;; but we need to set them in our own reader, not the host's:
                 [read-case-sensitive #t]
                 [read-square-bracket-as-paren #t]
                 [read-curly-brace-as-paren #t]
                 [read-square-bracket-with-tag #f]
                 [read-curly-brace-with-tag #f]
                 [read-accept-box #t]
                 [read-accept-bar-quote #t]
                 [read-accept-graph #t]
                 [read-decimal-as-inexact #t]
                 [read-cdot #f]
                 [read-accept-dot #t]
                 [read-accept-infix-dot #t]
                 [read-accept-quasiquote #t]
                 [current-readtable #f])
    (thunk)))

(define (raise-wrong-module-name filename expected-name name)
  (error 'load-handler
         "expected a `module' declaration for `~a' in ~s, found: ~a"
         expected-name filename name))

(define (check-module-form exp filename)
  (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
         (and filename
              (error 'load-handler
                     (string-append "expected a `module' declaration, but found end-of-file\n"
                                    "  file: ~a")
                     filename))]
        [(compiled-module-expression? (syntax-e exp))
         ;; It's fine:
         exp]
        [(and (syntax? exp)
              (pair? (syntax-e exp))
              (eq? 'module (syntax-e (car (syntax-e exp))))
              (let* ([r (cdr (syntax-e exp))]
                     [r (if (syntax? r) (syntax-e r) r)])
                (and (pair? r)
                     (identifier? (car r)))))
         ;; It's ok; need to install a specific `module' binding:
         (datum->syntax exp
                        (cons (namespace-module-identifier)
                              (cdr (syntax-e exp)))
                        exp
                        exp)]
        [else
         (and filename
              (error 'default-load-handler
                     (string-append "expected a `module' declaration, but found something else\n"
                                    "  file: ~a")
                     filename))]))
