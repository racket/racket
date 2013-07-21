#lang racket/base
(require racket/dict
         racket/contract/base
         racket/string
         ffi/unsafe/atomic
         racket/private/generic)

(define ordering/c
  (or/c '= '< '>))

(provide ordering/c)

;; we use the private version here because we need to
;; provide a backwards compatible interface (just in case)
;; i.e., exporting prop:ordered-dict as opposed to using a
;;       generated hidden property.
(define-primitive-generics
  (ordered-dict gen:ordered-dict
                prop:ordered-dict
                ordered-methods
                ordered-dict?
                ordered-dict-implements?)
  #:fast-defaults ()
  #:defaults ()
  #:fallbacks ()
  #:derive-properties ()
  (dict-iterate-least ordered-dict)
  (dict-iterate-greatest ordered-dict)
  (dict-iterate-least/>? ordered-dict key)
  (dict-iterate-least/>=? ordered-dict key)
  (dict-iterate-greatest/<? ordered-dict key)
  (dict-iterate-greatest/<=? ordered-dict key))

(define extreme-contract
  (->i ([d ordered-dict?])
       [_r (d) (or/c #f (dict-iter-contract d))]))

(define search-contract
  (->i ([d ordered-dict?]
        [k (d) (dict-key-contract d)])
       [_r (d) (or/c #f (dict-iter-contract d))]))

(define prop:ordered-dict-contract
  (let ([e (or/c extreme-contract #f)] ;; generics initializes with #f,
                                        ; then sets the methods
        [s (or/c search-contract #f)])
    (vector/c e   ;; iterate-least
              e   ;; iterate-greatest
              s   ;; iterate-least/>?
              s   ;; iterate-least/>=?
              s   ;; iterate-greatest/<?
              s)));; iterate-greatest/<=?

;; --------

(provide gen:ordered-dict)
(provide/contract
 [prop:ordered-dict
  (struct-type-property/c prop:ordered-dict-contract)]
 [ordered-dict? (-> any/c boolean?)]
 [dict-iterate-least extreme-contract]
 [dict-iterate-greatest extreme-contract]
 [dict-iterate-least/>? search-contract]
 [dict-iterate-least/>=? search-contract]
 [dict-iterate-greatest/<? search-contract]
 [dict-iterate-greatest/<=? search-contract])

;; ============================================================

(struct order (name domain-contract comparator =? <?)
        #:property prop:procedure (struct-field-index comparator))

(define order*
  (let ([order
         (case-lambda
           [(name ctc cmp)
            (order name ctc cmp
                   (lambda (x y) (eq? (cmp x y) '=))
                   (lambda (x y) (eq? (cmp x y) '<)))]
           [(name ctc = <)
            (order name ctc
                   (lambda (x y)
                     (cond [(= x y) '=]
                           [(< x y) '<]
                           [(< y x) '>]
                           [else (incomparable name x y)]))
                   = <)]
           [(name ctc = < >)
            (order name ctc
                   (lambda (x y)
                     (cond [(= x y) '=]
                           [(< x y) '<]
                           [(> x y) '>]
                           [else (incomparable name x y)]))
                   = <)])])
    order))

(define (incomparable name x y)
  (error name "values are incomparable: ~e ~e" x y))

(provide/contract
 [rename order* order
  (->* (symbol? contract? procedure?) (procedure? procedure?)
       order?)]
 [order? (-> any/c boolean?)]
 [order-comparator
  (-> order? procedure?)]
 [order-<?
  (-> order? procedure?)]
 [order-=?
  (-> order? procedure?)]
 [order-domain-contract
  (-> order? contract?)])

;; ============================================================

(define (real/not-NaN? x) (and (real? x) (not (eqv? x +nan.0))))

(define real-order
  (order* 'real-order real/not-NaN? = < >))

(provide/contract
 [real-order order?])

;; ============================================================

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
  < prefab-struct
  < fully-transparent-struct

;; FIXME: What else to add? regexps (4 kinds?), syntax, ...

|#

;; not exported because I'm not sure it's a good idea and I'm not sure
;; how to characterize it
(define (natural-cmp x y)
  (gen-cmp x y #t))

(define (datum-cmp x y)
  (gen-cmp x y #f))

(define (gen-cmp x y natural?)
  (define-syntax-rule (recur x* y*)
    (gen-cmp x* y* natural?))
  (cond [(eq? x y) '=]
        #|
        [(T? x) ...]
         ;; at this point, Type(x) > T
        [(T? y)
         ;; Type(x) > T = Type(y), so:
         '>]
        Assumes arguments are legal.
        |#
        [(real? x)
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
             (vector-cmp x y 0 natural?)
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
                     ;; FIXME: use struct-ref to avoid allocation?
                     (vector-cmp (struct->vector x) (struct->vector y) 1 natural?))
             '<)]
        [(prefab-struct-key y)
         '>]
        [(fully-transparent-struct-type x)
         => (lambda (xtype)
              (cond [(fully-transparent-struct-type y)
                     => (lambda (ytype)
                          ;; could also do another lexico with object-name first
                          (lexico (object-cmp xtype ytype)
                                  ;; FIXME: use struct-ref to avoid allocation?
                                  (vector-cmp (struct->vector x) (struct->vector y)
                                              1 natural?)))]
                    [else '<]))]
        [(fully-transparent-struct-type y)
         '>]
        [else
         (raise-type-error
          (if natural? 'natural-cmp 'datum-cmp)
          (string-join '("number" "string" "bytes" "keyword" "symbol" "boolean" "character"
                         "null" "pair" "vector" "box"
                         "prefab struct" "or fully-transparent struct")
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
  ;; FIXME: need prim symbol<? to avoid allocation!
  (string<? (symbol->string x) (symbol->string y)))

(define (vector-cmp x y i natural?)
  (cond [(< i (vector-length x))
         (if (< i (vector-length y))
             (lexico (gen-cmp (vector-ref x i) (vector-ref y i) natural?)
                     (vector-cmp x y (add1 i) natural?))
             '>)]
        [(< i (vector-length y))
         '<]
        [else '=]))

;; fully-transparent-struct-type : any -> struct-type or #f
(define (fully-transparent-struct-type x)
  (parameterize ((current-inspector weak-inspector))
    (let-values ([(x-type x-skipped?) (struct-info x)])
      (and (not x-skipped?) x-type))))

;; weak inspector controls no struct types;
;; so if it can inspect, must be transparent
(define weak-inspector (make-inspector))

;; Impose an arbitrary (but consistent) ordering on eq?-compared
;; objects. Use eq? and eq-hash-code for common fast path. Fall back
;; to table when comparing struct-types *same eq-hash-code* but *not
;; eq?*. That should be rare.
(define object-order-table (make-weak-hasheq))
(define object-order-next 0)
(define (object-cmp x y)
  (cond [(eq? x y) '=]
        [else
         (lexico
          (cmp* < = (eq-hash-code x) (eq-hash-code y))
          (call-as-atomic
           (lambda ()
             (let ([xi (hash-ref object-order-table x #f)]
                   [yi (hash-ref object-order-table y #f)])
               (cond [(and xi yi)
                      ;; x not eq? y, so xi != yi
                      (if (< xi yi) '< '>)]
                     [xi '<]
                     [yi '>]
                     [else ;; neither one is in table; we only need to add one
                      (hash-set! object-order-table x object-order-next)
                      (set! object-order-next (add1 object-order-next))
                      '<])))))]))

(define datum-order
  (order* 'datum-order any/c datum-cmp))

(provide/contract
 [datum-order order?])
