#lang racket/base

(require racket/list racket/match racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TEXT DATATYPE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? pred? v)
  (and (syntax? v) (pred? (syntax-e v))))

(define (string-literal? v) (literal? string? v))
(define (bytes-literal? v) (literal? bytes? v))
(define (keyword-literal? v) (literal? keyword? v))

(define (text? v)
  (or (symbol? v)
      (string? v)
      (keyword? v)
      (bytes? v)
      (and (syntax? v) (text? (syntax-e v)))))

(define (text=? a b)
  (string=? (to-string a) (to-string b)))

(define (text>? a b)
  (string>? (to-string a) (to-string b)))

(define (text>=? a b)
  (string>=? (to-string a) (to-string b)))

(define (text<? a b)
  (string<? (to-string a) (to-string b)))

(define (text<=? a b)
  (string<=? (to-string a) (to-string b)))

(define (to-string t)
  (cond
   [(string? t) t]
   [(symbol? t) (symbol->string t)]
   [(keyword? t) (keyword->string t)]
   [(bytes? t) (bytes->string/utf-8 t)]
   [(syntax? t) (to-string (syntax-e t))]))

(define (combine-strings before between after strs)
  (apply
   string-append
   before
   (let loop ([strs strs])
     (match strs
       [(list) (list after)]
       [(list str) (list str after)]
       [(cons str strs) (list* str between (loop strs))]))))

(define ((to-text convert)
         #:before [before ""]
         #:between [between ""]
         #:after [after ""]
         . ts)
  (convert (combine-strings (to-string before)
                            (to-string between)
                            (to-string after)
                            (map to-string ts))))

(define text->string (to-text values))
(define text->symbol (to-text string->symbol))
(define text->keyword (to-text string->keyword))
(define text->bytes (to-text string->bytes/utf-8))

(define ((to-literal convert)
         #:stx [stx #f]
         #:before [before ""]
         #:between [between ""]
         #:after [after ""]
         . ts)
  (datum->syntax
   stx
   (convert (combine-strings (to-string before)
                             (to-string between)
                             (to-string after)
                             (map to-string ts)))
   stx
   stx
   stx))

(define text->string-literal (to-literal values))
(define text->identifier (to-literal string->symbol))
(define text->keyword-literal (to-literal string->keyword))
(define text->bytes-literal (to-literal string->bytes/utf-8))

(define text/c (flat-named-contract "text" text?))

(define (convert/c result/c)
  (->* []
       [#:before text/c #:between text/c #:after text/c]
       #:rest (listof text/c)
       result/c))

(define (convert-literal/c result/c)
  (->* []
       [#:before text/c
        #:between text/c
        #:after text/c
        #:stx (or/c false/c syntax?)]
       #:rest (listof text/c)
       result/c))

(provide/contract
 [text/c flat-contract?]
 [text? (-> any/c boolean?)]
 [string-literal? (-> any/c boolean?)]
 [keyword-literal? (-> any/c boolean?)]
 [bytes-literal? (-> any/c boolean?)]
 [text=? (-> text/c text/c boolean?)]
 [text>? (-> text/c text/c boolean?)]
 [text>=? (-> text/c text/c boolean?)]
 [text<? (-> text/c text/c boolean?)]
 [text<=? (-> text/c text/c boolean?)]
 [text->string (convert/c string?)]
 [text->symbol (convert/c symbol?)]
 [text->keyword (convert/c keyword?)]
 [text->bytes (convert/c bytes?)]
 [text->identifier (convert-literal/c identifier?)]
 [text->string-literal (convert-literal/c string-literal?)]
 [text->keyword-literal (convert-literal/c keyword-literal?)]
 [text->bytes-literal (convert-literal/c bytes-literal?)])
