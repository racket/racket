#lang racket/base
(require racket/dict
         racket/contract/base
         racket/string
         unstable/prop-contract)

(define-values (prop:ordered-dict ordered-dict? ordered-dict-ref)
  (make-struct-type-property 'ordered-dict #f))

(define extreme-contract
  (->i ([d ordered-dict?])
       [_ (d) (or/c #f (dict-iter-contract d))]))

(define search-contract
  (->i ([d ordered-dict?]
        [k (d) (dict-key-contract d)])
       [_ (d) (or/c #f (dict-iter-contract d))]))

(define prop:ordered-dict-contract
  (let ([e extreme-contract]
        [s search-contract])
    (vector-immutable/c e   ;; iterate-min
                        e   ;; iterate-max
                        s   ;; iterate-least/>?
                        s   ;; iterate-least/>=?
                        s   ;; iterate-greatest/<?
                        s)));; iterate-greatest/<=?

;; --------

(define-syntax-rule (appd d offset arg ...)
  (let ([dv d])
    ((vector-ref (ordered-dict-ref dv) offset) dv arg ...)))

(define (dict-iterate-min d)
  (appd d 0))
(define (dict-iterate-max d)
  (appd d 1))
(define (dict-iterate-least/>? d k)
  (appd d 2 k))
(define (dict-iterate-least/>=? d k)
  (appd d 3 k))
(define (dict-iterate-greatest/<? d k)
  (appd d 4 k))
(define (dict-iterate-greatest/<=? d k)
  (appd d 5 k))

(provide/contract
 [prop:ordered-dict
  (struct-type-property/c prop:ordered-dict-contract)]
 [ordered-dict? (-> any/c boolean?)]
 [dict-iterate-min extreme-contract]
 [dict-iterate-max extreme-contract]
 [dict-iterate-least/>? search-contract]
 [dict-iterate-least/>=? search-contract]
 [dict-iterate-greatest/<? search-contract]
 [dict-iterate-greatest/<=? search-contract])

;; ============================================================

(provide natural-cmp
         datum-cmp)

;; Comparator : any any -> (U '< '= '>)

#|
natural-cmp : Comparator
datum-cmp : Comparator

comparators for (most) built-in values
!! May diverge on cyclical input.

natural-cmp:
  * restriction to reals equiv to <,=

    real (exact and inexact, #e1 = #i1, +nan.0 not allowed!)
  < complex
  < Other

datum-cmp:
  * restriction to reals NOT EQUIV to <,= (separates exact, inexact)

    exact real
  < inexact real (+nan.0 > +inf.0)
  < complex
  < Other

Other:

    string
  < bytes
  < keyword
  < symbol
  < bool
  < char
  < null
  < pair
  < vector
  < box
  < prefab

;; FIXME: What else to add? regexps (4 kinds?), syntax, ...

|#

(define (natural-cmp x y)
  (gen-cmp x y #t))

(define (datum-cmp x y)
  (gen-cmp x y #f))

(define (gen-cmp x y natural?)
  (define-syntax-rule (recur x* y*)
    (gen-cmp x* y* natural?))
  #|
  (cond ...
        [(T? x) ...]
        ;; at this point, Type(x) > T
        [(T? y)
         ;; Type(x) > T = Type(y), so:
         '>])
  Assumes arguments are legal.
  |#
  (cond [(real? x)
         (if (real? y)
             (cond [natural?
                    (cmp* < = x y)]
                   [else ;; exact < inexact
                    (cond [(and (exact? x) (exact? y))
                           (cmp* < = x y)]
                          [(exact? x) ;; inexact y
                           '<]
                          [(exact? y) ;; inexact x
                           '>]
                          [(and (eqv? x +nan.0) (eqv? y +nan.0))
                           '=]
                          [(eqv? x +nan.0)
                           '>]
                          [(eqv? y +nan.0)
                           '<]
                          [else ;; inexact x, inexact y
                           (cmp* < = x y)])])
             '<)]
        [(real? y) '>]
        [(complex? x)
         (if (complex? y)
             (lexico (recur (real-part x) (real-part y))
                     (recur (imag-part x) (imag-part y)))
             '<)]
        [(complex? y) '>]
        [(string? x)
         (if (string? y)
             (cmp* string<? string=? x y)
             '<)]
        [(string? y) '>]
        [(bytes? x)
         (if (bytes? y)
             (cmp* bytes<? bytes=? x y)
             '<)]
        [(bytes? y) '>]
        [(keyword? x)
         (if (keyword? y)
             (cmp* keyword<? eq? x y)
             '<)]
        [(keyword? y) '>]
        [(symbol? x)
         (if (symbol? y)
             (cmp* symbol<? eq? x y)
             '<)]
        [(symbol? y) '>]
        [(boolean? x)
         (if (boolean? y)
             (cond [(eq? x y) '=]
                   [y '<]
                   [else '>])
             '<)]
        [(boolean? y) '>]
        [(char? x)
         (if (char? y)
             (cmp* char<? char=? x y)
             '<)]
        [(char? y)
         '>]
        [(null? x)
         (if (null? y)
             '=
             '<)]
        [(null? y) '>]
        [(pair? x)
         (if (pair? y)
             (lexico (recur (car x) (car y)) (recur (cdr x) (cdr y)))
             '<)]
        [(pair? y) '>]
        [(vector? x)
         (if (vector? y)
             (vector<? x y 0 natural?)
             '<)]
        [(vector? y) '>]
        [(box? x)
         (if (box? y)
             (recur (unbox x) (unbox y))
             '<)]
        [(box? y) '>]
        [(prefab-struct-key x)
         (if (prefab-struct-key y)
             (lexico (recur (prefab-struct-key x) (prefab-struct-key y))
                     (vector<? (struct->vector x) (struct->vector y) 1 natural?))
             '<)]
        [(prefab-struct-key y)
         '>]
        [else
         (raise-type-error
          (if natural? 'natural-cmp 'datum-cmp)
          (string-join '("number" "string" "bytes" "keyword" "symbol" "boolean" "character"
                         "null" "pair" "vector" "box"
                         "or prefab struct")
                       ", ")
          0 x y)]))

(define-syntax-rule (cmp* <? =? xe ye)
  (let ([x xe] [y ye])
    (if (=? x y) '= (if (<? x y) '< '>))))

(define-syntax-rule (lexico c1 c2)
  (case c1
    ((<) '<)
    ((=) c2)
    ((>) '>)))

(define (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))

(define (vector<? x y i natural?)
  (cond [(< i (vector-length x))
         (if (< i (vector-length y))
             (lexico (gen-cmp (vector-ref x i) (vector-ref y i) natural?)
                     (vector<? x y (add1 i) natural?))
             '>)]
        [(< i (vector-length y))
         '<]
        [else '=]))
