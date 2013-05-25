#lang racket/base
(require web-server/servlet
         racket/stxparam
         racket/list
         racket/contract
         (for-syntax racket/base))

(define-syntax-parameter embed/url
  (λ (stx) (raise-syntax-error stx 'embed/url "Used outside page")))

(define-syntax-rule (page e ...)
  (send/suspend/dispatch
   (λ (this-embed/url)
     (syntax-parameterize ([embed/url (make-rename-transformer #'this-embed/url)])
                          e ...))))

(define current-request (make-parameter #f))

(define-syntax-rule (lambda/page formals e ...)
  (lambda (req . formals)
    (parameterize ([current-request req])
      (page e ...))))

(define-syntax-rule (define/page (id . formals) e ...)
  (define id
    (lambda/page formals e ...)))

(define binding-id/c (or/c bytes? string? symbol?))
(define (binding-id->bytes id)
  (cond [(bytes? id)
         id]
        [(string? id)
         (string->bytes/utf-8 id)]
        [(symbol? id)
         (binding-id->bytes (symbol->string id))]))

(define binding-format/c (symbols 'string 'bytes 'file 'binding))
(define (convert-binding format b)
  (case format
    [(string)
     (and (binding:form? b)
          (with-handlers ([exn:fail? (λ (x) #f)])
            (bytes->string/utf-8 (binding:form-value b))))]
    [(bytes)
     (and (binding:form? b)
          (binding:form-value b))]
    [(file)
     (and (binding:file? b)
          (binding:file-content b))]
    [(binding)
     b]))
          
(define (get-binding id [req (current-request)]
                     #:format [format 'string])
  (convert-binding
   format
   (bindings-assq 
    (binding-id->bytes id) 
    (request-bindings/raw req))))

(define (get-bindings id [req (current-request)]
                      #:format [format 'string])
  (define id-bs (binding-id->bytes id))
  (filter-map
   (λ (b)
     (and (bytes=? id-bs (binding-id b))
          (convert-binding format b)))
   (request-bindings/raw req)))

(provide embed/url
         page
         lambda/page
         define/page)
(provide/contract
 [current-request (parameter/c (or/c false/c request?))]
 [binding-id/c contract?]
 [binding-format/c contract?]
 [get-binding (->* (binding-id/c) (request? #:format binding-format/c)
                   (or/c false/c string? bytes? binding?))]
 [get-bindings (->* (binding-id/c) (request? #:format binding-format/c)
                    (listof (or/c string? bytes? binding?)))])
