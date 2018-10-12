#lang racket/base
(require "match.rkt"
         "id.rkt"
         "vehicle.rkt"
         "struct.rkt")

(provide inline-function
         extract-inline-predicate)

(define (inline-function rator n rands in-lam knowns #:can-gc? [can-gc? #t])
  (case rator
    [(car unsafe-car) (and (= n 1) 'c_pair_car)]
    [(cdr unsafe-cdr) (and (= n 1) 'c_pair_cdr)]
    [(cadr) (and (= n 1) 'c_pair_cadr)]
    [(cdar) (and (= n 1) 'c_pair_cdar)]
    [(cddr) (and (= n 1) 'c_pair_cddr)]
    [(caar) (and (= n 1) 'c_pair_caar)]
    [(cons) (and (= n 2) can-gc? 'scheme_make_pair)]
    [(list*) (and (= n 2) can-gc? 'scheme_make_pair)]
    [(list) (and (or (= n 1) (= n 2)) can-gc? (if (= n 1) 'c_make_list1 'c_make_list2))]
    [(box unsafe-make-place-local) (and (= n 1) can-gc? 'c_make_box)]
    [(unbox unsafe-unbox unbox* unsafe-unbox* unsafe-place-local-ref) (and (= n 1) 'c_box_ref)]
    [(weak-box-value) (and (or (= n 1) (= n 2)) 'c_weak_box_value)]
    [(set-box! unsafe-set-box!) (and (= n 2) can-gc? 'c_box_set)]
    [(set-box*! unsafe-set-box*! unsafe-place-local-set!) (and (= n 2) 'c_box_set)]
    [(vector-ref unsafe-vector-ref) (and (= n 2) can-gc? 'c_vector_ref)]
    [(vector*-ref unsafe-vector*-ref) (and (= n 2) 'c_authentic_vector_ref)]
    [(vector-set! unsafe-vector-set!) (and (= n 3) can-gc? 'c_vector_set)]
    [(vector*-set! unsafe-vector*-set!) (and (= n 3) 'c_vector_set)]
    [(vector-length unsafe-vector-length vector*-length unsafe-vector*-length) (and (= n 1) 'c_vector_length)]
    [(string-ref unsafe-string-ref) (and (= n 2) can-gc? 'c_string_ref)]
    [(bytes-ref unsafe-bytes-ref) (and (= n 2) 'c_bytes_ref)]
    [(fx+ unsafe-fx+) (and (= n 2) 'c_int_add)]
    [(fx- unsafe-fx-) (and (= n 2) 'c_int_sub)]
    [(fx* unsafe-fx*) (and (= n 2) 'c_int_mult)]
    [(fxrshift unsafe-fxrshift) (and (= n 2) 'c_int_rshift)]
    [(fxand unsafe-fxand) (and (= n 2) 'c_int_and)]
    [(add1) (and (= n 1) can-gc? 'c_number_add1)]
    [(sub1) (and (= n 1) can-gc? 'c_number_sub1)]
    [(hash-ref) (cond
                  [(= n 3) (and can-gc? (known-non-procedure? (caddr rands) knowns) 'c_hash_ref)]
                  [(= n 2) (and can-gc? 'c_hash_ref2)]
                  [else #f])]
    [(hash-set) (and (= n 3) can-gc? 'c_hash_set)]
    [(hash-count) (and (= n 1) can-gc? 'c_hash_count)]
    [(hash-iterate-first) (and (= n 1) can-gc? 'c_hash_iterate_first)]
    [(unsafe-immutable-hash-iterate-first) (and (= n 1) can-gc? 'c_unsafe_immutable_hash_iterate_first)]
    [(unsafe-immutable-hash-iterate-next) (and (= n 2) can-gc? 'c_unsafe_immutable_hash_iterate_next)]
    [(unsafe-immutable-hash-iterate-key) (and (= n 2) can-gc? 'c_unsafe_immutable_hash_iterate_key)]
    [(unsafe-immutable-hash-iterate-key+value) (and (= n 2) can-gc? 'c_unsafe_immutable_hash_iterate_key_value)]
    [(prefab-struct-key) (and (= n 1) 'c_prefab_struct_key)]
    [else
     (define-values (pred-exprs pred-gc? pred-inliner)
       (extract-inline-predicate (cons rator (for/list ([i (in-range n)]) 'c_unknown)) in-lam knowns))
     (cond
       [(and pred-inliner
             (or (not pred-gc?) can-gc?))
        (lambda (s) (format "(~a ? scheme_true : scheme_false)" (pred-inliner s)))]
       [else
        (define k (hash-ref knowns rator #f))
        (cond
          [(and (struct-accessor? k) (= n 1))
           (define authentic? (struct-info-authentic? (struct-accessor-si k)))
           (and (or can-gc? authentic?)
                (lambda (s)
                  (if authentic?
                      (format "c_authentic_struct_ref(~a, ~a)" s (struct-accessor-pos k))
                      (and can-gc? (format "c_struct_ref(~a, ~a)" s (struct-accessor-pos k))))))]
          [(and (struct-mutator? k) (= n 2))
           (define authentic? (struct-info-authentic? (struct-mutator-si k)))
           (and (or can-gc? authentic?)
                (lambda (s)
                  (if authentic?
                      (format "c_authentic_struct_set(~a, ~a)" s (struct-mutator-pos k))
                      (format "c_struct_set(~a, ~a)" s (struct-mutator-pos k)))))]
          [(and (struct-property-accessor? k) (= n 1))
           (and can-gc?
                (lambda (s top-ref)
                  (format "c_struct_property_ref(~a, ~a)" s (top-ref (struct-property-accessor-property-id k)))))]
          [else #f])])]))

(define (extract-inline-predicate e in-lam knowns #:compose? [compose? #f])
  (define (compose e gc? wrapper)
    (define-values (new-es new-gc? new-wrapper) (extract-inline-predicate e in-lam knowns #:compose? compose?))
    (values new-es (or gc? new-gc?) (lambda (s) (wrapper (new-wrapper s)))))
  (define (generic e)
    (if compose?
        (values (list e) #f (lambda (s) (format "c_scheme_truep(~a)" s)))
        (values #f #f #f)))
  ;; simple => no GC
  (define (simple template #:can-gc? [can-gc? #f] . args)
    (values args can-gc? (lambda (s) (format template s))))
  (match e
    [`(not ,e)
     (if compose?
         (compose e #f (lambda (s) (format "!~a" s)))
         (values (list e) #f (lambda (s) (format "c_scheme_falsep(~a)" s))))]
    [`(null? ,e) (simple "c_scheme_nullp(~a)" e)]
    [`(eof-object? ,e) (simple "c_scheme_eof_objectp(~a)" e)]
    [`(void? ,e) (simple "c_scheme_voidp(~a)" e)]
    [`(boolean? ,e) (simple "c_scheme_boolp(~a)" e)]
    [`(number? ,e) (simple "c_scheme_numberp(~a)" e)]
    [`(pair? ,e) (simple "c_scheme_pairp(~a)" e)]
    [`(list? ,e) (simple "c_scheme_listp(~a)" e)]
    [`(vector? ,e) (simple "c_scheme_chaperone_vectorp(~a)" e)]
    [`(box? ,e) (simple "c_scheme_chaperone_boxp(~a)" e)]
    [`(symbol? ,e) (simple "c_scheme_symbolp(~a)" e)]
    [`(keyword? ,e) (simple "c_scheme_keywordp(~a)" e)]
    [`(string? ,e) (simple "c_scheme_char_stringp(~a)" e)]
    [`(bytes? ,e) (simple "c_scheme_byte_stringp(~a)" e)]
    [`(path? ,e) (simple "c_scheme_pathp(~a)" e)]
    [`(char? ,e) (simple "c_scheme_charp(~a)" e)]
    [`(hash? ,e) (simple "c_scheme_hashp(~a)" e)]
    [`(eq? ,e1 ,e2) (simple "c_same_obj(~a)" e1 e2)]
    [`(eqv? ,e1 ,e2) (simple "scheme_eqv(~a)" e1 e2)]
    [`(equal? ,e1 ,e2) (simple #:can-gc? #t "scheme_equal(~a)"e1 e2)]
    [`(char=? ,e1 ,e2) (simple "c_scheme_char_eq(~a)" e1 e2)]
    [`(unsafe-char=? ,e1 ,e2) (simple "c_scheme_char_eq(~a)" e1 e2)]
    [`(char-whitespace? ,e) (simple "c_scheme_char_whitespacep(~a)" e)]
    [`(unsafe-fx< ,e1 ,e2) (simple "c_int_lt(~a)" e1 e2)]
    [`(fx< ,e1 ,e2) (simple "c_int_lt(~a)" e1 e2)]
    [`(unsafe-fx> ,e1 ,e2) (simple "c_int_gt(~a)" e1 e2)]
    [`(fx> ,e1 ,e2) (simple "c_int_gt(~a)" e1 e2)]
    [`(unsafe-fx>= ,e1 ,e2) (simple "!c_int_lt(~a)" e1 e2)]
    [`(fx>= ,e1 ,e2) (simple "!c_int_lt(~a)" e1 e2)]
    [`(unsafe-fx<= ,e1 ,e2) (simple "!c_int_gt(~a)" e1 e2)]
    [`(fx<= ,e1 ,e2) (simple "!c_int_gt(~a)" e1 e2)]
    [`(unsafe-fx= ,e1 ,e2) (simple "c_same_obj(~a)" e1 e2)]
    [`(fx= ,e1 ,e2) (simple "c_same_obj(~a)" e1 e2)]
    [`(= ,e1 ,e2) (simple #:can-gc? #t "c_number_eq(~a)" e1 e2)]
    [`(< ,e1 ,e2) (simple #:can-gc? #t "c_number_lt(~a)" e1 e2)]
    [`(> ,e1 ,e2) (simple #:can-gc? #t "c_number_gt(~a)" e1 e2)]
    [`(<= ,e1 ,e2) (simple #:can-gc? #t "c_number_lt_eq(~a)" e1 e2)]
    [`(>= ,e1 ,e2) (simple #:can-gc? #t "c_number_gt_eq(~a)" e1 e2)]
    [`(zero? ,e) (simple "c_number_zerop(~a)" e)]
    [`(,rator ,rand)
     (define k (and (symbol? rator)
                    (hash-ref knowns rator #f)))
     (cond
       [(struct-predicate? k)
        (define si (struct-predicate-si k))
        (define s-id (struct-info-struct-id si))
        (values (list rand)
                #f
                (cond
                  [(struct-info-authentic? si)
                   (lambda (s) (format "c_is_authentic_struct_instance(~a, ~a)" s (top-ref in-lam s-id)))]
                  [else
                   (lambda (s) (format "c_is_struct_instance(~a, ~a)" s (top-ref in-lam s-id)))]))]
       [else (generic e)])]
    [`,_ (generic e)]))

(define (known-non-procedure? e knowns)
  (or (boolean? e)
      (number? e)
      (eq? e 'null)
      (and (symbol? e)
           (let ([k (hash-ref knowns e #f)])
             (or (symbol? k)
                 (eq? k '#:non-procedure))))))
