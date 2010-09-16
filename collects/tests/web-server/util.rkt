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
         call)

(define keyword->symbol (compose string->symbol keyword->string))
(define (simple-xpath/xexpr p x)
  (match p
    [(list)
     (list x)]
    [(list-rest (? symbol? s) r)
     (match x
       [(list-rest (? (curry equal? s)) rs)
        (simple-xpath/tag-body r rs)]
       [_
        empty])]
    [_
     empty]))
(define (simple-xpath/tag-body p x)
  (match p
    [(list)
     (match x
       [(list) empty]
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (simple-xpath/tag-body p rs)]
       [(? list?)
        x]
       [_ 
        empty])]
    [(list-rest (? symbol?) _)
     (match x
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (simple-xpath/tag-body p rs)]
       [(? list?)
        (append-map (curry simple-xpath/xexpr p) x)]
       [_
        empty])]
    [(list (? keyword? k))
     (match x
       [(list-rest (and attrs (list (list (? symbol?) (? string?)) ...)) rs)
        (simple-xpath/attr (keyword->symbol k) attrs)]
       [_
        empty])]
    [_
     empty]))
(define (simple-xpath/attr k attrs)
  (dict-ref attrs k empty))
(define (simple-xpath*/list p x)
  (append (simple-xpath/xexpr p x)
          (match x
            [(list-rest (list (cons (? symbol?) (? string?)) ...) rs)
             (simple-xpath*/list p rs)]
            [(? list?)
             (append-map (curry simple-xpath*/list p) x)]
            [_
             empty])))
(define (simple-xpath* p x)
  (match (simple-xpath*/list p x)
    [(list) #f]
    [(list-rest f rs) f]))

(test
 (simple-xpath*/list '(p) '(html (body (p "Hey") (p "Bar")))) => (list "Hey" "Bar")
 (simple-xpath* '(p) '(html (body (p "Hey")))) => "Hey"
 (simple-xpath* '(p #:bar) '(html (body (p ([bar "Zog"]) "Hey")))) => "Zog")    

(provide simple-xpath*
         simple-xpath*/list)

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

; This causes infinite loop. I will try putting it in a thread like on the real server
#;(define (collect d req)
    (define-values (c i o) (make-mock-connection #""))
    (parameterize ([current-server-custodian (current-custodian)])
      (d c req))
    (redact (get-output-bytes o)))

; This causes errors because s/s/d tries to jump the barrier, but I have no idea why
(define (collect d req)
  (define-values (c i o) (make-mock-connection #""))
  (parameterize ([current-server-custodian (current-custodian)])
    (call-with-continuation-barrier
     (lambda ()
       (d c req))))
  (redact (get-output-bytes o)))

; This causes a dead lock, even though the log shows that the channel should sync
(define (channel-put* c v)
  (printf "+CHAN ~S PUT: ~S\n" c v)
  (channel-put c v)
  (printf "-CHAN ~S PUT: ~S\n" c v))

(define (channel-get* c)
  (printf "+CHAN ~S GET\n" c)
  (let ([v (channel-get c)])
    (printf "-CHAN ~S GET: ~S\n" c v)
    v))

#;(define (collect d req)
    (define chan (make-channel))
    (define-values (c i o) (make-mock-connection #""))
    (parameterize ([current-server-custodian (current-custodian)])
      (thread 
       (lambda ()
         (d c req)
         (channel-put* chan (get-output-bytes o))
         )))
    (redact (channel-get* chan)))

; This causes an error, because the output bytes are #""
#;(define (collect d req)
    (define-values (c i o) (make-mock-connection #""))
    (parameterize ([current-server-custodian (current-custodian)])
      (thread-wait
       (thread 
        (lambda ()
          (d c req)))))
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
