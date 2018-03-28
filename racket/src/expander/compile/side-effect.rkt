#lang racket/base
(require "../common/set.rkt"
         "built-in-symbol.rkt"
         "self-quoting.rkt"
         "known.rkt"
         "../host/correlate.rkt")

;; To support extraction of a bootstrapped version of the expander, we
;; need to be able to prune unused module content. Pruning is usefully
;; improved by a simple analysis of whether a module body has any
;; side-effects.

;; See "known.rkt" for classifications of definitions and locals

(provide any-side-effects?)

(define (any-side-effects? e ; compiled expression
                           expected-results ; number of expected results, or #f if any number is ok
                           #:known-locals [locals #hasheq()] ; known local-variable bindings
                           #:known-defns [defns #hasheq()] ; other variables to known-value information
                           #:ready-variable? [ready-variable? (lambda (id) #f)]) ; other variables known to be ready
  (define (effects? e expected-results locals)
    (any-side-effects? e expected-results
                       #:known-locals locals
                       #:known-defns defns
                       #:ready-variable? ready-variable?))
  (define actual-results
    (let loop ([e e] [locals locals])
      (case (and (pair? (correlated-e e))
                 (correlated-e (car (correlated-e e))))
        [(quote lambda case-lambda #%variable-reference) 1]
        [(letrec-values let-values)
         (define-correlated-match m e '(_ ([ids rhs] ...) body))
         (and (not (for/or ([ids (in-list (m 'ids))]
                            [rhs (in-list (m 'rhs))])
                     (effects? rhs (correlated-length ids) locals)))
              (loop (m 'body) (add-binding-info locals (m 'ids) (m 'rhs))))]
        [(values)
         (define-correlated-match m e '(_ e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (effects? e 1 locals)))
              (length (m 'e)))]
        [(void)
         (define-correlated-match m e '(_ e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (effects? e 1 locals)))
              1)]
        [(begin)
         (define-correlated-match m e '(_ e ...))
         (let bloop ([es (m 'e)])
           (cond
            [(null? es) #f]
            [(null? (cdr es)) (loop (car es) locals)]
            [else (and (not (effects? (car es) #f locals))
                       (bloop (cdr es)))]))]
        [(begin0)
         (define-correlated-match m e '(_ e0 e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (effects? e #f locals)))
              (loop (m 'e0) locals))]
        [(make-struct-type)
         (and (ok-make-struct-type? e ready-variable? defns)
              5)]
        [(make-struct-field-accessor)
         (and (ok-make-struct-field-accessor/mutator? e locals 'general-accessor defns)
              1)]
        [(make-struct-field-mutator)
         (and (ok-make-struct-field-accessor/mutator? e locals 'general-mutator defns)
              1)]
        [(make-struct-type-property)
         (and (ok-make-struct-type-property? e defns)
              3)]
        [(gensym)
         (define-correlated-match m e #:try '(gs (quot datum)))
         (and (or (and (m)
                       (eq? 'quote (m 'quot))
                       (or (symbol? (m 'datum))
                           (string? (m 'datum))))
                  (null? (cdr (correlated-e e))))
              1)]
        [(if)
         (define-correlated-match m e #:try '(_ (id:rator id:arg) thn els))
         (cond
          [(m)
           (cond
            [(or (hash-ref locals (m 'id:rator) #f)
                 (lookup-defn defns (m 'id:rator)))
             => (lambda (d)
                  (and (known-predicate? d)
                       (not (effects? (m 'thn)
                                      expected-results
                                      (hash-set locals (m 'id:arg)
                                                (known-satisfies (known-predicate-key d)))))
                       (loop (m 'els) locals)))]
            [else #f])]
          [else
           (define-correlated-match m e #:try '(_ tst thn els))
           (and (m)
                (not (effects? (m 'tst) 1 locals))
                (not (effects? (m 'thn) expected-results locals))
                (loop (m 'els) locals))])]
        [else
         (define v (correlated-e e))
         (cond
          [(or (string? v) (number? v) (boolean? v) (char? v))
           1] ;; unquoted vals
          [(and (pair? v)
                (let ([rator (correlated-e (car v))])
                  (or (hash-ref locals rator #f)
                      (lookup-defn defns rator))))
           =>
           (lambda (d)
             (define-correlated-match m e '(_ e ...))
             (define n-args (length (m 'e)))
             (and (or (and (or (and (known-struct-op? d)
                                    (eq? 'constructor (known-struct-op-type d))
                                    (= (known-struct-op-field-count d) n-args))
                               (and (known-function? d)
                                    (known-function-pure? d)
                                    (arity-includes? (known-function-arity d) n-args)))
                           (for/and ([e (in-list (m 'e))])
                             (not (effects? e 1 locals))))
                      (and (known-function-of-satisfying? d)
                           (= n-args (length (known-function-of-satisfying-arg-predicate-keys d)))
                           (for/and ([e (in-list (m 'e))]
                                     [key (in-list (known-function-of-satisfying-arg-predicate-keys d))])
                             (and (not (effects? e 1 locals))
                                  (satisfies? e key defns locals)))))
                  1))]
          [else
           (and
            (or (self-quoting-in-linklet? v)
                (and (symbol? v)
                     (or (hash-ref locals v #f)
                         (lookup-defn defns v)
                         (built-in-symbol? v)
                         (ready-variable? v))))
            1)])])))
  (not (and actual-results
            (or (not expected-results)
                (= actual-results expected-results)))))

(define (satisfies? e key defns locals)
  (define d (or (hash-ref locals e #f)
                (lookup-defn defns e)))
  (and d
       (known-satisfies? d)
       (eq? key (known-satisfies-predicate-key d))))

;; ----------------------------------------

(define (add-binding-info locals idss rhss)
  (for/fold ([locals locals]) ([ids (in-list idss)]
                               [rhs (in-list rhss)])
    (let loop ([rhs rhs])
      (case (and (pair? (correlated-e rhs))
                 (correlated-e (car (correlated-e rhs))))
        [(make-struct-type)
         ;; Record result "types"
         (define field-count (extract-struct-field-count-lower-bound rhs))
         (for/fold ([locals locals]) ([id (in-list (correlated->list ids))]
                                      [type (in-list '(struct-type
                                                       constructor
                                                       predicate
                                                       general-accessor
                                                       general-mutator))])
           (hash-set locals (correlated-e id) (known-struct-op type field-count)))]
        [(let-values)
         (if (null? (correlated-e (correlated-cadr rhs)))
             (loop (caddr (correlated->list rhs)))
             (loop #f))]
        [else
         (for/fold ([locals locals]) ([id (in-list (correlated->list ids))])
           (hash-set locals (correlated-e id) #t))]))))

;; ----------------------------------------

(define (ok-make-struct-type-property? e defns)
  (define l (correlated->list e))
  (and (<= 2 (length l) 5)
       (for/and ([arg (in-list (cdr l))]
                 [pred (in-list
                        (list
                         (lambda (v) (quoted? symbol? v))
                         (lambda (v) (is-lambda? v 2 defns))
                         (lambda (v) (ok-make-struct-type-property-super? v defns))
                         (lambda (v) (any-side-effects? v 1 #:known-defns defns))))])
         (pred arg))))

(define (ok-make-struct-type-property-super? v defns)
  (or (quoted? null? v)
      (eq? 'null (correlated-e v))
      (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'list)
           (for/and ([prop+val (in-list (cdr (correlated->list v)))])
             (and (= (correlated-length prop+val) 3)
                  (let ([prop+val (correlated->list prop+val)])
                    (and (eq? 'cons (correlated-e (car prop+val)))
                         (or (memq (correlated-e (list-ref prop+val 1))
                                   '(prop:procedure prop:equal+hash prop:custom-write))
                             (known-property? (lookup-defn defns (correlated-e (list-ref prop+val 1)))))
                         (not (any-side-effects? (list-ref prop+val 2) 1 #:known-defns defns))))))
           ;; All properties must be distinct
           (= (sub1 (correlated-length v))
              (set-count (for/set ([prop+val (in-list (cdr  (correlated->list v)))])
                           (correlated-e (list-ref (correlated->list prop+val) 1))))))))

;; ----------------------------------------

(define (ok-make-struct-type? e ready-variable? defns)
  (define l (correlated->list e))
  (define init-field-count-expr (and ((length l) . > . 3)
                                     (list-ref l 3)))
  (define auto-field-count-expr (and ((length l) . > . 4)
                                     (list-ref l 4)))
  (define num-fields
    (maybe+ (field-count-expr-to-field-count init-field-count-expr)
            (field-count-expr-to-field-count auto-field-count-expr)))
  (define immutables-expr (or (and ((length l) . > . 9)
                                   (list-ref l 9))
                              'null))
  (define super-expr (and ((length l) . > . 2)
                          (list-ref l 2)))

  (and ((length l) . >= . 5)
       ((length l) . <= . 12)
       (for/and ([arg (in-list (cdr l))]
                 [pred (in-list (list
                                 (lambda (v) (quoted? symbol? v))
                                 (lambda (v) (super-ok? v defns))
                                 (lambda (v) (field-count-expr-to-field-count v))
                                 (lambda (v) (field-count-expr-to-field-count v))
                                 (lambda (v) (not (any-side-effects? v 1 #:ready-variable? ready-variable? #:known-defns defns)))
                                 (lambda (v) (known-good-struct-properties? v immutables-expr super-expr defns))
                                 (lambda (v) (inspector-or-false? v))
                                 (lambda (v) (procedure-spec? v num-fields))
                                 (lambda (v) (immutables-ok? v init-field-count-expr))))])
         (pred arg))))

(define (super-ok? e defns)
  (or (quoted? false? e)
      (let ([o (lookup-defn defns (correlated-e e))])
        (and o
             (known-struct-op? o)
             (eq? 'struct-type (known-struct-op-type o))))))

(define (extract-struct-field-count-lower-bound e)
  ;; e is already checked by `ok-make-struct-type?`
  (define l (correlated->list e))
  (+ (field-count-expr-to-field-count (list-ref l 3))
     (field-count-expr-to-field-count (list-ref l 4))))

(define (quoted? val? v)
  (or (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'quote)
           (val? (correlated-e (correlated-cadr v))))
      (val? (correlated-e v))))

(define (quoted-value v)
  (if (pair? (correlated-e v))
      (correlated-e (correlated-cadr v))
      (correlated-e v)))

(define (false? v)
  (eq? (correlated-e v) #f))

(define (field-count-expr-to-field-count v)
  (and (quoted? exact-nonnegative-integer? v)
       (quoted-value v)))

(define (inspector-or-false? v)
  (or (quoted? false? v)
      (and (quoted? symbol? v)
           (eq? 'prefab (quoted-value v)))
      (and (= 1 (correlated-length v))
           (eq? 'current-inspector (correlated-e (car (correlated-e v)))))))

(define (known-good-struct-properties? v immutables-expr super-expr defns)
  (or (quoted? null? v)
      (eq? 'null (correlated-e v))
      (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'list)
           (for/and ([prop+val (in-list (cdr (correlated->list v)))])
             (and (= (correlated-length prop+val) 3)
                  (let ([prop+val (correlated->list prop+val)])
                    (and (eq? 'cons (correlated-e (car prop+val)))
                         (known-good-struct-property+value? (list-ref prop+val 1)
                                                            (list-ref prop+val 2)
                                                            immutables-expr
                                                            super-expr
                                                            defns)))))
           ;; All properties must be distinct
           (= (sub1 (correlated-length v))
              (set-count (for/set ([prop+val (in-list (cdr  (correlated->list v)))])
                           (correlated-e (list-ref (correlated->list prop+val) 1))))))))

(define (known-good-struct-property+value? prop-expr val-expr immutables-expr super-expr defns)
  (define prop-name (correlated-e prop-expr))
  (case prop-name
    [(prop:evt) (or (is-lambda? val-expr 1 defns)
                    (immutable-field? val-expr immutables-expr))]
    [(prop:procedure) (or (is-lambda? val-expr 1 defns)
                          (immutable-field? val-expr immutables-expr))]
    [(prop:custom-write) (is-lambda? val-expr 3 defns)]
    [(prop:equal+hash)
     (define l (correlated->list val-expr))
     (and (eq? 'list (car l))
          (is-lambda? (list-ref l 1) 3 defns)
          (is-lambda? (list-ref l 2) 2 defns)
          (is-lambda? (list-ref l 3) 2 defns))]
    [(prop:method-arity-error prop:incomplete-arity)
     (not (any-side-effects? val-expr 1 #:known-defns defns))]
    [(prop:impersonator-of)
     (is-lambda? val-expr 1 defns)]
    [(prop:arity-string) (is-lambda? val-expr 1 defns)]
    [(prop:checked-procedure)
     (and (quoted? false? super-expr)
          ;; checking that we have at least 2 fields
          (immutable-field? 1 immutables-expr))]
    [else
     (define o (lookup-defn defns prop-name))
     (and o
          (known-property? o)
          (not (any-side-effects? val-expr 1 #:known-defns defns)))]))

;; is expr a procedure of specified arity? (arity irrelevant if #f)
(define (is-lambda? expr arity defns)
  (define lookup (lookup-defn defns expr))
  (or (and lookup (known-function? lookup) ;; is it a known procedure?
           (or (not arity) ;; arity doesn't matter
               (arity-includes? (known-function-arity lookup) arity))) ;; arity compatible
      (and (pair? (correlated-e expr))
           (eq? 'case-lambda (car (correlated-e expr)))
           (not arity))
      (and (pair? (correlated-e expr))
           (eq? 'lambda (car (correlated-e expr)))
           (or (not arity)
               (let loop ([args (cadr (correlated->list expr))]
                          [arity arity])
                 (cond
                   [(correlated? args) (loop (correlated-e args) arity)]
                   [(null? args) (zero? arity)]
                   [(pair? args) (loop (cdr args) (sub1 arity))]
                   [else (not (negative? arity))]))))))

(define (arity-includes? a n)
  (or (equal? a n)
      (and (list? a)
           (for/or ([a (in-list a)])
             (equal? a n)))))

(define (immutable-field? val-expr immutables-expr)
  (and (quoted? exact-nonnegative-integer? val-expr)
       (memv (quoted-value val-expr)
             (immutables-expr-to-immutables immutables-expr null))))

(define (immutables-expr-to-immutables e fail-v)
  (case (and (pair? (correlated-e e))
             (correlated-e (car (correlated-e e))))
    [(quote)
     (define v (correlated-cadr e))
     (or (and (correlated-length v)
              (let ([l (map correlated-e (correlated->list v))])
                (and (andmap exact-nonnegative-integer? l)
                     (= (length l) (set-count (list->set l)))
                     l)))
         fail-v)]
    [else fail-v]))

(define (procedure-spec? e field-count)
  (or (quoted? false? e)
      (and (quoted? exact-nonnegative-integer? e)
           field-count
           (< (quoted-value e) field-count))
      (is-lambda? e #f #hasheq())))

(define (immutables-ok? e init-field-count-expr)
  (define l (immutables-expr-to-immutables e #f))
  (define c (field-count-expr-to-field-count init-field-count-expr))
  (and l
       (for/and ([n (in-list l)])
         (n . < . c))))

;; ----------------------------------------

(define (ok-make-struct-field-accessor/mutator? e locals type defns)
  (define l (correlated->list e))
  (define a (and (or (= (length l) 3) (= (length l) 4))
                 (or (hash-ref locals (correlated-e (list-ref l 1)) #f)
                     (lookup-defn defns (correlated-e (list-ref l 1))))))
  (and (known-struct-op? a)
       (eq? (known-struct-op-type a) type)
       ((field-count-expr-to-field-count (list-ref l 2)) . < . (known-struct-op-field-count a))
       (or (= (length l) 3) (quoted? symbol? (list-ref l 3)))))

;; ----------------------------------------

(define (maybe+ x y)
  (and x y (+ x y)))

;; ----------------------------------------

(module+ test
  (define-syntax-rule (check expr result)
    (unless (equal? expr result)
      (error 'failed "~s" #'expr)))

  (define (any-side-effects?* e n)
    (define v1 (any-side-effects? e n))
    (define v2 (any-side-effects? (datum->correlated e) n))
    (unless (equal? v1 v2)
      (error "problem with correlated:" e))
    v1)

  (check (any-side-effects?* ''1 1)
         #f)

  (check (any-side-effects?* ''1 #f)
         #f)

  (check (any-side-effects?* '(lambda (x) x) 1)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '0
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list
                                (cons prop:evt '0)
                                (cons prop:evt '0)) ; duplicate
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #t)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(1)) ; <- too big
                             5)
         #t))
