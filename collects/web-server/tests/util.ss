#lang scheme/base
(require (for-syntax scheme/base)
         (lib "connection-manager.ss" "web-server" "private")
         (only-in (planet "ssax.ss" ("lizorkin" "ssax.plt" 2 0))
                  ssax:xml->sxml)
         (lib "request-structs.ss" "web-server" "private")
         (lib "web-server-structs.ss" "web-server" "private")
         net/url
         mzlib/pretty
         mzlib/list
         (lib "timer.ss" "web-server" "private"))
(provide make-module-eval
         make-eval/mod-path
         make-mock-connection
         redact
         collect
         htxml
         call)

(define (call d u bs)
  (htxml (collect d (make-request 'get (string->url u) empty bs #"" "127.0.0.1" 80 "127.0.0.1"))))
(define (htxml bs)
  (define sx (ssax:xml->sxml (open-input-bytes (second (regexp-match #"^.+\r\n\r\n(.+)$" bs))) empty))
  (pretty-print sx)
  sx)

(define (collect d req)
  (define-values (c i o) (make-mock-connection #""))
  (parameterize ([current-server-custodian (current-custodian)])
    (d c req))
  (redact (get-output-bytes o)))

(define (make-mock-connection ib)
  (define ip (open-input-bytes ib))
  (define op (open-output-bytes))
  (values (make-connection 0 (make-timer never-evt +inf.0 (lambda () (void)))
                           ip op (current-custodian) #f)
          ip
          op))

(define (redact b)
  (regexp-replace 
   #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
   (regexp-replace
    #"Last-Modified: [a-zA-Z0-9:, ]+ GMT\r\n"
    b
    #"Last-Modified: REDACTED GMT\r\n")
   #"Date: REDACTED GMT\r\n"))

(define-syntax (make-module-eval m-expr)
  (syntax-case m-expr (module)
    [(_ (module m-id . rest))
     #'(let ([ns (make-base-empty-namespace)])
         (parameterize ([current-namespace ns])
           (namespace-require 'scheme/base)
           (namespace-require '(lib "abort-resume.ss" "web-server" "lang"))
           (namespace-require '(lib "serialize.ss"))
           (eval '(module m-id . rest))
           (eval '(require 'm-id)))
         
         (lambda (s-expr)
           (parameterize ([current-namespace ns])
             (eval s-expr))))]
    [else
     (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))

(define (make-eval/mod-path pth)
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'scheme/base)
      (namespace-require '(lib "abort-resume.ss" "web-server" "lang"))
      (namespace-require '(lib "serialize.ss"))
      (namespace-require pth))
    (lambda (expr)
      (parameterize ([current-namespace ns])
        (eval expr)))))
