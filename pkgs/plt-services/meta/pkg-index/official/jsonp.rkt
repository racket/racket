#lang racket/base
(require json
         web-server/http
         racket/match)

(define (response/jsonp callback o)
  (response/output
   (λ (op)
     (fprintf op "~a(" callback)
     (write-json o op)
     (fprintf op ");"))
   #:mime-type #"application/javascript"))

(define (request-jsonp-data req)
  (for/fold ([ht (hasheq)])
      ([b (in-list (request-bindings/raw req))])
    (match b
      [(binding:form id value)
       (define p (map string->symbol (parse-jsonp-path (bytes->string/utf-8 id))))
       (hash*-set ht p
                  (bytes->string/utf-8 value))]
      [_ ht])))

(define parse-jsonp-path
  (match-lambda
   [(regexp #rx"^([^[]+)\\[([^]]+)\\](.*)$"
            (list _ fst snd rst))
    (list* fst (parse-jsonp-path (format "~a~a" snd rst)))]
   [s
    (list s)]))

(define (hash*-set ht p v)
  (match p
    [(list k)
     (hash-set ht k v)]
    [(list-rest f r)
     (hash-update ht f (λ (fht) (hash*-set fht r v)) (hasheq))]))

(define (make-jsonp-responder f)
  (λ (req)
    (define og (request-jsonp-data req))
    (response/jsonp
     (hash-ref og 'callback)
     (f (hash-remove (hash-remove og 'callback) '_)))))

(define-syntax-rule (define-jsonp (f . pat) . body)
  (define f
    (make-jsonp-responder
     (match-lambda [(hash-table . pat) . body]
                   [x (error 'f "ill formatted request: ~v" x)]))))

(provide define-jsonp
         make-jsonp-responder)
