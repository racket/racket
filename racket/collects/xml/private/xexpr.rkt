#lang racket/base
(require racket/contract
         racket/list
         racket/symbol
         "reader.rkt"
         "structures.rkt"
         "writer.rkt"
         "xexpr-core.rkt")

;; sorting is no longer necessary, since xt3d uses xml->zxexpr, which sorts.

;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
(define (assoc-sort to-sort)
  (sort to-sort (bcompose string<? (compose symbol->string car))))

(define xexpr-drop-empty-attributes (make-parameter #f))

;; xml->xexpr : Content -> Xexpr
(define (xml->xexpr x)
  (let* ([non-dropping-combine
          (lambda (atts body)
            (cons (assoc-sort (map attribute->srep atts))
                  body))]
         [combine (if (xexpr-drop-empty-attributes)
                      (lambda (atts body)
                        (if (null? atts)
                            body
                            (non-dropping-combine atts body)))
                      non-dropping-combine)])
    (let loop ([x x])
      (cond
        [(element? x)
         (let ([body (map loop (element-content x))]
               [atts (element-attributes x)])
           (cons (element-name x) (combine atts body)))]
        [(pcdata? x) (pcdata-string x)]
        [(entity? x) (entity-text x)]
        [(or (comment? x) (p-i? x) (cdata? x)) x]
        [(document? x) (error 'xml->xexpr "Expected content, given ~e\nUse document-element to extract the content." x)]
        [(permissive-xexprs) x]
        [else (error 'xml->xexpr "Expected content, given ~e" x)]))))

;; attribute->srep : Attribute -> Attribute-srep
(define (attribute->srep a)
  (list (attribute-name a) (attribute-value a)))

;; srep->attribute : Attribute-srep -> Attribute
(define (srep->attribute a)
  (unless (and (pair? a) (pair? (cdr a)) (null? (cddr a)) (symbol? (car a)) (string? (cadr a)))
    (error 'srep->attribute "expected (list Symbol String) given ~e" a))
  (make-attribute 'scheme 'scheme (car a) (cadr a)))

;; xexpr->xml : Xexpr -> Content
;; The contract is enforced.
(define (xexpr->xml x)
  (cond
    [(pair? x)
     (let ([f (lambda (atts body)
                (unless (list? body)
                  (error 'xexpr->xml
                         "expected a list of xexprs for the body in ~e"
                         x))
                (make-element 'racket 'racket (car x)
                              atts
                              (map xexpr->xml body)))])
       (if (and (pair? (cdr x))
                (or (null? (cadr x))
                    (and (pair? (cadr x)) (pair? (caadr x)))))
           (f (map srep->attribute (cadr x)) (cddr x))
           (f null (cdr x))))]
    [(string? x) (make-pcdata 'racket 'racket x)]
    [(or (symbol? x) (exact-nonnegative-integer? x))
     (make-entity 'racket 'racket x)]
    [(or (comment? x) (p-i? x) (cdata? x) (pcdata? x)) x]
    [else ;(error 'xexpr->xml "malformed xexpr ~e" x)
     x]))

;; xexpr->string : Xexpression -> String
(define (xexpr->string xexpr)
  (let ([port (open-output-string)])
    (write-xexpr xexpr port)
    (get-output-string port)))

(define (string->xexpr str)
  (xml->xexpr (document-element (read-xml (open-input-string str)))))

;; bcompose : (a a -> c) (b -> a) -> (b b -> c)
(define (bcompose f g)
  (lambda (x y) (f (g x) (g y))))

(provide xexpr? 
         validate-xexpr
         correct-xexpr?
         xexpr/c)

(provide/contract
 [exn:invalid-xexpr? (any/c . -> . boolean?)]
 [exn:invalid-xexpr-code (exn:invalid-xexpr? . -> . any/c)]
 [string->xexpr (string? . -> . xexpr/c)]
 [xexpr->string (xexpr/c . -> . string?)]
 [xml->xexpr (content/c . -> . xexpr/c)]
 [xexpr->xml (xexpr/c . -> . content/c)]
 [xexpr-drop-empty-attributes (parameter/c boolean?)]
 [xml-attribute-encode (string? . -> . string?)]
 [write-xexpr (->* (xexpr/c)
                   (output-port?
                    #:insert-newlines? any/c)
                   void?)])

(define (write-xexpr x [out (current-output-port)]
                     #:insert-newlines? [insert-newlines? #f])
  (define short (empty-tag-shorthand))
  (define unescaped (current-unescaped-tags))
  (let loop ([x x] [escape? #t])
    (cond
      ; Element
      [(cons? x)
       (define name (car x))
       (define-values (attrs content)
         (if (and (pair? (cdr x))
                  (or (null? (cadr x))
                      (and (pair? (cadr x)) (pair? (caadr x)))))
             (values (cadr x) (cddr x))
             (values null (cdr x))))
       ; Write opening tag
       (write-bytes #"<" out)
       (write-string (symbol->immutable-string name) out)
       ; Write attributes
       (for ([att (in-list attrs)])
         (write-bytes #" " out)
         (write-string (symbol->immutable-string (car att)) out)
         (write-bytes #"=\"" out)
         (write-string/escape (cadr att) escape-attribute-table out)
         (write-bytes #"\"" out))
       (when insert-newlines?
         (newline out))
       ; Write end of opening tag
       (cond
         [(and (null? content)
               (case short
                 [(always) #t]
                 [(never) #f]
                 [else (memq (lowercase-symbol name) short)]))
          (write-bytes #"/>" out)]
         [else
          (write-bytes #">" out)
          ; Write body
          (for ([xe (in-list content)])
            (loop xe (not (memq name unescaped))))
          ; Write closing tag
          (write-bytes #"</" out)
          (write-string (symbol->immutable-string name) out)
          (write-bytes #">" out)])]
      ; PCData
      [(string? x)
       (if escape?
           (write-string/escape x escape-table out)
           (write-string x out))]
      ; Entities
      [(symbol? x)
       (write-bytes #"&" out)
       (write-string (symbol->immutable-string x) out)
       (write-bytes #";" out)]
      [(valid-char? x)
       (write-bytes #"&#" out)
       (write-string (number->string x) out)
       (write-bytes #";" out)]
      ; Embedded XML
      [(cdata? x)
       (write-xml-cdata x 0 'none out)]
      [(comment? x)
       (write-xml-comment x 0 'none out)]
      [(p-i? x)
       (write-xml-p-i x 0 'none out)]))
  (void))

;; given a string, encode it in the style required for attributes. Specifically,
;; double-quote must be encoded as well as <, >, and &, because the double-quote
;; would otherwise end the attribute.
(define (xml-attribute-encode str)
  (escape str escape-attribute-table))
