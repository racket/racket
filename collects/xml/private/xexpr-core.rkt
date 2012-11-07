#lang racket/base
(require racket/list
         racket/contract/base
         racket/contract/combinator
         racket/pretty
         "core.rkt")


(provide
 (contract-out
  [xexpr/c contract?]
  [xexpr? (any/c . -> . boolean?)]
  [validate-xexpr (any/c . -> . (one-of/c #t))]
  [rename correct-xexpr/k? correct-xexpr? (any/c (-> any/c) (exn:invalid-xexpr? . -> . any/c) . -> . any/c)])
 (struct-out exn:invalid-xexpr))

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

(define xexpr-datum/c
  (or/c string? symbol? valid-char?
        comment? p-i? cdata? pcdata?))

(define (xexpr? x)
  (not (incorrect-xexpr? x)))

(define (validate-xexpr x)
  (define maybe-exn (incorrect-xexpr? x))
  (if maybe-exn
      (raise maybe-exn)
      #t))


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



;; correct-xexpr/k? : any (-> a) (exn -> a) -> a
(define (correct-xexpr/k? x true-k false-k)
  (define maybe-exn (incorrect-xexpr? x))
  (if maybe-exn
      (false-k maybe-exn)
      (true-k)))


;; incorrect-xexpr?: any -> (or/c #f exn:invalid-xexpr)
(define (incorrect-xexpr? x)
  (cond
    ((string? x) #f)
    ((symbol? x) #f)
    ((valid-char? x) #f)
    ((comment? x) #f)
    ((p-i? x) #f)
    ((cdata? x) #f)
    ((pcdata? x) #f)
    ((list? x)     
     (cond [(null? x) 
            (make-exn:invalid-xexpr
             "Expected a symbol as the element name, but none given"
             (current-continuation-marks)
             x)]
           [else
            (if (symbol? (car x))
                (cond [(has-attribute? x)
                       (define maybe-exn (erroneous-attribute-pairs? (cadr x)))
                       (cond [maybe-exn maybe-exn]
                             [else
                              (ormap
                               incorrect-xexpr?
                               (cddr x))])]
                      
                      [else 
                       (ormap incorrect-xexpr?
                              (cdr x))])
                (make-exn:invalid-xexpr
                 (format
                  "Expected a symbol as the element name, given ~s"
                  (car x))
                 (current-continuation-marks)
                 x))]))
    [(permissive-xexprs) #f]
    [else (make-exn:invalid-xexpr
           (format (string-append
                    "Expected a string, symbol, valid numeric entity, comment, "
                    "processing instruction, or list, given ~s")
                   x)
           (current-continuation-marks)
           x)]))

;; has-attribute? : List -> Boolean
;; True if the Xexpr provided has an attribute list.
(define (has-attribute? x)
  (and (> (length x) 1)
       (list? (cadr x))
       (for/and ([attr (in-list (cadr x))])
         (pair? attr))))


;; erroneous-attribute-pairs? : List -> (or/c #f exn:invalid-xexpr)
;; Returns exn:invalid-expr if the list is not a list of attribute pairs.
(define (erroneous-attribute-pairs? attrs)
  (cond [(null? attrs)
         #f]
        [else
         (define attr (car attrs))
         (cond [(pair? attr)
                (define maybe-exn (erroneous-attribute-symbol-string? attr))
                (cond
                  [maybe-exn maybe-exn]
                  [else
                   (erroneous-attribute-pairs? (cdr attrs))])]
               [else
                (make-exn:invalid-xexpr
                 (format "Expected an attribute pair, given ~s" attr)
                 (current-continuation-marks)
                 attr)])]))


;; erroneous-attribute-symbol-string? : List -> (or/c #f exn:invalid-xexpr)
;; Returns exn:invalid-expr if the list is not a (String, Symbol) pair.
(define (erroneous-attribute-symbol-string? attr)
  (if (symbol? (car attr))
      (if (pair? (cdr attr))
          (if (or (string? (cadr attr))
                  (permissive-xexprs))
              #f
              (make-exn:invalid-xexpr
               (format "Expected an attribute value string, given ~v" (cadr attr))
               (current-continuation-marks)
               (cadr attr)))
          (make-exn:invalid-xexpr
           (format "Expected an attribute value string for attribute ~s, given nothing" attr)
           (current-continuation-marks)
           attr))
      (make-exn:invalid-xexpr
       (format "Expected an attribute symbol, given ~s" (car attr))
       (current-continuation-marks)
       (cadr attr))))

;; ; end xexpr? helpers
;; ;; ;; ;; ;; ;; ;; ;;
