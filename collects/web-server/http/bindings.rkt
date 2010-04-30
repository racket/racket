#lang racket/base
(require racket/list
         racket/contract
         racket/match
         web-server/private/util
         web-server/http/request-structs)

(define (request-headers request)
  (map (match-lambda
         [(struct header (field value))
          (cons (lowercase-symbol! (bytes->string/utf-8 field))
                (bytes->string/utf-8 value))])
       (request-headers/raw request)))
(define (request-bindings request)
  (map (match-lambda
         [(struct binding:form (id value))
          (cons (lowercase-symbol! (bytes->string/utf-8 id))
                (bytes->string/utf-8 value))]
         [(struct binding:file (id fname headers value))
          (cons (lowercase-symbol! (bytes->string/utf-8 id))
                value)])
       (request-bindings/raw request)))

; extract-binding/single : sym (listof (cons str str)) -> str
(define (extract-binding/single name bindings)
  (define lst (extract-bindings name bindings))
  (cond
    [(empty? lst)
     (error 'extract-binding/single "~e not found in ~e" name bindings)]
    [(empty? (rest lst))
     (first lst)]
    [else 
     (error 'extract-binding/single "~e occurs multiple times in ~e" name bindings)]))

; extract-bindings : sym (listof (cons str str)) -> (listof str)
(define (extract-bindings name bindings)
  (map cdr (filter (lambda (x) (equal? name (car x))) bindings)))

; exists-binding? : sym (listof (cons sym str)) -> bool
; for checkboxes
(define (exists-binding? name bindings)
  (if (assq name bindings)
      #t
      #f))

(provide/contract
 [extract-binding/single (symbol? (listof (cons/c symbol? any/c)) . -> . any/c)]
 [extract-bindings (symbol? (listof (cons/c symbol? any/c)) . -> . (listof any/c))]
 [exists-binding? (symbol? (listof (cons/c symbol? any/c)) . -> . boolean?)]   
 [request-bindings (request? . -> . (listof (or/c (cons/c symbol? string?)
                                                  (cons/c symbol? bytes?))))]
 [request-headers (request? . -> . (listof (cons/c symbol? string?)))])
