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
  [correct-xexpr? (any/c (-> any/c) (exn:invalid-xexpr? . -> . any/c) . -> . any/c)])
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
                     "Expected a string, symbol, valid numeric entity, comment, "
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
