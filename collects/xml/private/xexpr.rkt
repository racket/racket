#lang racket/base
(require racket/pretty
         racket/list
         racket/contract
         "structures.rkt"
         "reader.rkt"
         "writer.rkt")

;; Xexpr ::= String
;;        |  (list* Symbol (listof Attribute-srep) (listof Xexpr))
;;        |  (cons Symbol (listof Xexpr))
;;        |  Symbol
;;        |  Nat (WFC: Valid Char)
;;        |  Comment
;;        |  Processing-instruction
;;        |  Cdata
;; Attribute-srep ::= (list Symbol String)

;; sorting is no longer necessary, since xt3d uses xml->zxexpr, which sorts.

;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
(define (assoc-sort to-sort)
  (sort to-sort (bcompose string<? (compose symbol->string car))))

(define xexpr-drop-empty-attributes (make-parameter #f))

(define xexpr-datum/c
  (or/c string? symbol? valid-char?
        comment? p-i? cdata? pcdata?))

#;(define xexpr/c
    (flat-rec-contract xexpr
                       xexpr-datum/c
                       (cons/c symbol?
                               (or/c (cons/c (listof (list/c symbol? string?)) (listof xexpr))
                                     (listof xexpr)))))

(define (xexpr? x)
  (correct-xexpr? x (lambda () #t) (lambda (exn) #f)))

(define (validate-xexpr x)
  (correct-xexpr? x (lambda () #t) (lambda (exn) (raise exn))))

(define xexpr/c 
  (make-flat-contract
   #:name 'xexpr?
   #:projection
   (lambda (blame)
     (lambda (val)
       (with-handlers ([exn:invalid-xexpr?
                        (lambda (exn)
                          (raise-blame-error
                           blame
                           val
                           "Not an Xexpr. ~a\n\nContext:\n~a"
                           (exn-message exn)
                           (pretty-format val)))])
         (validate-xexpr val)
         val)))
   #:first-order xexpr?))

;; ;; ;; ;; ;; ;; ;
;; ; xexpr? helpers

(define-struct (exn:invalid-xexpr exn:fail) (code))

;; correct-xexpr? : any (-> a) (exn -> a) -> a
(define (correct-xexpr? x true false)
  (cond
    ((string? x) (true))
    ((symbol? x) (true))
    ((valid-char? x) (true))
    ((comment? x) (true))
    ((p-i? x) (true))
    ((cdata? x) (true))
    ((pcdata? x) (true))
    ((list? x)
     (or (null? x)
         (if (symbol? (car x))
             (if (has-attribute? x)
                 (and (attribute-pairs? (cadr x) true false)
                      (andmap (lambda (part)
                                (correct-xexpr? part true false))
                              (cddr x))
                      (true))
                 (andmap (lambda (part)
                           (correct-xexpr? part true false))
                         (cdr x)))
             (false (make-exn:invalid-xexpr
                     (format
                      "Expected a symbol as the element name, given ~s"
                      (car x))
                     (current-continuation-marks)
                     x)))))
    [(permissive-xexprs) (true)]
    (else (false
           (make-exn:invalid-xexpr
            (format (string-append
                     "Expected a string, symbol, number, comment, "
                     "processing instruction, or list, given ~s")
                    x)
            (current-continuation-marks)
            x)))))

;; has-attribute? : List -> Boolean
;; True if the Xexpr provided has an attribute list.
(define (has-attribute? x)
  (and (> (length x) 1)
       (list? (cadr x))
       (andmap (lambda (attr)
                 (pair? attr))
               (cadr x))))

;; attribute-pairs? : List (-> a) (exn -> a) -> a
;; True if the list is a list of pairs.
(define (attribute-pairs? attrs true false)
  (if (null? attrs)
      (true)
      (let ((attr (car attrs)))
        (if (pair? attr)
            (and (attribute-symbol-string? attr true false)
                 (attribute-pairs? (cdr attrs) true false )
                 (true))
            (false
             (make-exn:invalid-xexpr
              (format "Expected an attribute pair, given ~s" attr)
              (current-continuation-marks)
              attr))))))

;; attribute-symbol-string? : List (-> a) (exn -> a) -> a
;; True if the list is a list of String,Symbol pairs.
(define (attribute-symbol-string? attr true false)
  (if (symbol? (car attr))
      (if (pair? (cdr attr))
          (if (or (string? (cadr attr))
                  (permissive-xexprs))
              (true)
              (false (make-exn:invalid-xexpr
                      (format "Expected an attribute value string, given ~v" (cadr attr))
                      (current-continuation-marks)
                      (cadr attr))))
          (false (make-exn:invalid-xexpr
                  (format "Expected an attribute value string for attribute ~s, given nothing" attr)
                  (current-continuation-marks)
                  attr)))
      (false (make-exn:invalid-xexpr
              (format "Expected an attribute symbol, given ~s" (car attr))
              (current-continuation-marks)
              (cadr attr)))))

;; ; end xexpr? helpers
;; ;; ;; ;; ;; ;; ;; ;;


; : (a -> bool) tst -> bool
; To check if l is a (listof p?)
; Don't use (and (list? l) (andmap p? l)) because l may be improper.
(define (listof? p? l)
  (let listof-p? ([l l])
    (or (null? l)
        (and (cons? l) (p? (car l)) (listof-p? (cdr l))))))

; : tst -> bool
(define (xexpr-attribute? b)
  (and (pair? b)
       (symbol? (car b))
       (pair? (cdr b))
       (string? (cadr b))
       (null? (cddr b))))

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

(provide/contract
 [exn:invalid-xexpr? (any/c . -> . boolean?)]
 [exn:invalid-xexpr-code (exn:invalid-xexpr? . -> . any/c)]
 [xexpr/c contract?]
 [xexpr? (any/c . -> . boolean?)]
 [string->xexpr (string? . -> . xexpr/c)]
 [xexpr->string (xexpr/c . -> . string?)]
 [xml->xexpr (content/c . -> . xexpr/c)]
 [xexpr->xml (xexpr/c . -> . content/c)]
 [xexpr-drop-empty-attributes (parameter/c boolean?)]
 [write-xexpr (->* (xexpr/c) (output-port?) void)] 
 [validate-xexpr (any/c . -> . (one-of/c #t))]
 [correct-xexpr? (any/c (-> any/c) (exn:invalid-xexpr? . -> . any/c) . -> . any/c)])

(define (write-xexpr x [out (current-output-port)])
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
     (display "<" out)
     (display name out)
     ; Write attributes
     (for ([att (in-list attrs)])
       (fprintf out " ~a=\"~a\"" (car att)
                (escape (cadr att) escape-attribute-table)))
     ; Write end of opening tag
     (if (and (null? content)
              (let ([short (empty-tag-shorthand)])
                (case short
                  [(always) #t]
                  [(never) #f]
                  [else (memq (lowercase-symbol name) short)])))
         (display " />" out)
         (begin
           (display ">" out)
           ; Write body
           (for ([xe (in-list content)])
             (write-xexpr xe out))
           ; Write closing tag
           (display "</" out)
           (display name out)
           (display ">" out)))]
    ; PCData
    [(string? x)
     (display (escape x escape-table) out)]
    ; Entities
    [(symbol? x)
     (fprintf out "&~a;" x)]
    [(valid-char? x)
     (fprintf out "&#~a;" x)]
    ; Embedded XML
    [(cdata? x)
     (write-xml-cdata x 0 void out)]
    [(comment? x)
     (write-xml-comment x 0 void out)]
    [(p-i? x)
     (write-xml-p-i x 0 void out)]))
