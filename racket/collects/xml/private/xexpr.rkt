#lang racket/base
(require racket/pretty
         racket/list
         racket/contract
         "xexpr-core.rkt"
         "structures.rkt"
         "reader.rkt"
         "writer.rkt")

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
                (make-element 'scheme 'scheme (car x)
                              atts
                              (map xexpr->xml body)))])
       (if (and (pair? (cdr x))
                (or (null? (cadr x))
                    (and (pair? (cadr x)) (pair? (caadr x)))))
           (f (map srep->attribute (cadr x)) (cddr x))
           (f null (cdr x))))]
    [(string? x) (make-pcdata 'scheme 'scheme x)]
    [(or (symbol? x) (exact-nonnegative-integer? x))
     (make-entity 'scheme 'scheme x)]
    [(or (comment? x) (p-i? x) (cdata? x) (pcdata? x)) x]
    [else ;(error 'xexpr->xml "malformed xexpr ~e" x)
     x]))

;; xexpr->string : Xexpression -> String
(define (xexpr->string xexpr)
  (let ([port (open-output-string)])
    (write-xml/content (xexpr->xml xexpr) port)
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
 [write-xexpr (->* (xexpr/c)
                   (output-port?
                    #:insert-newlines? any/c)
                   void?)])

(define (write-xexpr x [out (current-output-port)]
                     #:insert-newlines? [insert-newlines? #f])
  (define short (empty-tag-shorthand))
  (let loop ([x x])
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
       (write-string "<" out)
       (display name out)
       ; Write attributes
       (for ([att (in-list attrs)])
         (write-string " " out)
         (display (car att) out)
         (write-string "=\"" out)
         (write-string/escape (cadr att) escape-attribute-table out)
         (write-string "\"" out))
       (when insert-newlines?
         (newline))
       ; Write end of opening tag
       (if (and (null? content)
                (case short
                    [(always) #t]
                    [(never) #f]
                    [else (memq (lowercase-symbol name) short)]))
           (write-string "/>" out)
           (begin
             (write-string ">" out)
             ; Write body
             (for ([xe (in-list content)])
               (loop xe))
             ; Write closing tag
             (write-string "</" out)
             (display name out)
             (write-string ">" out)))]
      ; PCData
      [(string? x)
       (write-string/escape x escape-table out)]
      ; Entities
      [(symbol? x)
       (write-string "&" out)
       (display x out)
       (write-string ";" out)]
      [(valid-char? x)
       (write-string "&#" out)
       (display x out)
       (write-string ";" out)]
      ; Embedded XML
      [(cdata? x)
       (write-xml-cdata x 0 void out)]
      [(comment? x)
       (write-xml-comment x 0 void out)]
      [(p-i? x)
       (write-xml-p-i x 0 void out)]))
  (void))
