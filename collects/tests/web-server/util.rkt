#lang racket
(require (for-syntax racket/base)
         web-server/private/connection-manager
         web-server/http
         web-server/private/web-server-structs
         net/url
         mzlib/pretty
         mzlib/list
         xml
         tests/eli-tester
         web-server/private/timer)
(provide make-module-eval
         make-eval/mod-path
         make-mock-connection
         redact
         collect
         htxml
         call
         bytes-sort)

(require xml/path)
(provide (rename-out
          [se-path* simple-xpath*]
          [se-path*/list simple-xpath*/list]))

(define (bytes-sort bs)
  (sort
   (with-input-from-bytes bs
     (λ () (port->bytes-lines #:line-mode 'return-linefeed)))
   bytes<?))

(define (call d u bs)
  (htxml (collect d (make-request #"GET" (string->url u) empty (delay bs) #"" "127.0.0.1" 80 "127.0.0.1"))))
(define (htxml bs)
  (match (regexp-match #"^.+\r\n\r\n(.*)$" bs)
    [(list _ #"")
     ""]
    [(list _ s)
     (string->xexpr (bytes->string/utf-8 s))]
    [_
     (error 'html "Given ~S\n" bs)]))

; This causes errors because s/s/d tries to jump the barrier, but I have no idea why
(define (collect d req)
  (define-values (c i o) (make-mock-connection #""))
  (parameterize ([current-server-custodian (make-custodian)])
    (call-with-continuation-barrier
     (lambda ()
       (d c req))))
  (redact (get-output-bytes o)))

(define (make-mock-connection ib)
  (define ip (open-input-bytes ib))
  (define op (open-output-bytes))
  (values (make-connection 0 (make-timer never-evt +inf.0 (lambda () (void)))
                           ip op (make-custodian) #t)
          ip
          op))

(define (redact b)
  (regexp-replace
   #"Connection: close\r\n"
   (regexp-replace 
    #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
    (regexp-replace
     #"Last-Modified: [a-zA-Z0-9:, ]+ GMT\r\n"
     b
     #"Last-Modified: REDACTED GMT\r\n")
    #"Date: REDACTED GMT\r\n")
   #""))

(define-syntax (make-module-eval m-expr)
  (syntax-case m-expr (module)
    [(_ (module m-id . rest))
     #'(let ([ns (make-base-empty-namespace)])
         (parameterize ([current-namespace ns])
           (namespace-require 'racket/base)
           (namespace-require 'web-server/http)
           (namespace-require 'web-server/lang/abort-resume)
           (namespace-require 'mzlib/serialize)
           (eval '(module m-id . rest))
           (eval '(require 'm-id)))
         
         (lambda (s-expr)
           (parameterize ([current-namespace ns]
                          [current-output-port (open-output-nowhere)])
             (eval s-expr))))]
    [else
     (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))

(define (make-eval/mod-path pth)
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/base)
      (namespace-require 'web-server/http)
      (namespace-require 'web-server/lang/abort-resume)
      (namespace-require 'mzlib/serialize)
      (namespace-require pth))
    (lambda (expr)
      (parameterize ([current-namespace ns]
                     [current-output-port (open-output-nowhere)])
        (eval expr)))))
