#lang racket/base

(require racket/contract/private/guts
         racket/contract/private/blame
         racket/contract/private/prop
         racket/contract/private/opt
         racket/contract/private/misc
         "contract-arr-checks.rkt")
(require (for-syntax racket/base)
         (for-syntax racket/contract/private/opt-guts)
         (for-syntax racket/contract/private/helpers)
         (for-syntax "contract-arr-obj-helpers.rkt")
         (for-syntax syntax/stx)
         (for-syntax syntax/name))

(provide ->
         ->d
         ->*
         ->d*
         ->r
         ->pp
         ->pp-rest
         case->
         opt->
         opt->*)

(define-struct contracted-function (proc ctc)
  #:property prop:procedure 0
  #:property prop:contracted 1)

(define (build--> name doms doms-rest rngs kwds quoted-kwds rng-any? func)
  (let ([doms/c (map (λ (dom) (coerce-contract name dom)) doms)]
        [rngs/c (map (λ (rng) (coerce-contract name rng)) rngs)]
        [kwds/c (map (λ (kwd) (coerce-contract name kwd)) kwds)]
        [doms-rest/c (and doms-rest (coerce-contract name doms-rest))])
    (make--> rng-any? doms/c doms-rest/c rngs/c kwds/c quoted-kwds func)))
;; rng-any? : boolean
;; doms : (listof contract)
;; dom-rest : (or/c false/c contract)
;; rngs : (listof contract) -- may be ignored by the wrapper function in the case of any
;; kwds : (listof contract)
;; quoted-keywords : (listof keyword) -- must be sorted by keyword<
;; func : the wrapper function maker. It accepts a procedure for
;;        checking the first-order properties and the contracts
;;        and it produces a wrapper-making function.
(define-struct -> (rng-any? doms dom-rest rngs kwds quoted-kwds func)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc) 
      (let* ([doms/c (map contract-projection
                          (if (->-dom-rest ctc)
                            (append (->-doms ctc) (list (->-dom-rest ctc)))
                            (->-doms ctc)))]
             [rngs/c (map contract-projection (->-rngs ctc))]
             [kwds/c (map contract-projection (->-kwds ctc))]
             [mandatory-keywords (->-quoted-kwds ctc)]
             [func (->-func ctc)]
             [dom-length (length (->-doms ctc))]
             [has-rest? (and (->-dom-rest ctc) #t)])
        (lambda (blame)
          (let ([partial-doms (map (λ (dom) (dom (blame-swap blame)))
                                   doms/c)]
                [partial-ranges (map (λ (rng) (rng blame))
                                     rngs/c)]
                [partial-kwds (map (λ (kwd) (kwd (blame-swap blame)))
                                   kwds/c)])
            (apply func
                   (λ (val)
                      (if has-rest?
                        (check-procedure/more val dom-length '() mandatory-keywords blame)
                        (check-procedure val dom-length 0 '() mandatory-keywords blame)))
                   ctc
                   (append partial-doms partial-ranges partial-kwds))))))

   #:name
   (λ (ctc) (single-arrow-name-maker 
             (->-doms ctc)
             (->-dom-rest ctc)
             (->-kwds ctc)
             (->-quoted-kwds ctc)
             (->-rng-any? ctc)
             (->-rngs ctc)))
   #:first-order
   (λ (ctc)
      (let ([l (length (->-doms ctc))])
        (if (->-dom-rest ctc)
          (λ (x)
             (and (procedure? x) 
                  (procedure-accepts-and-more? x l)))
          (λ (x)
             (and (procedure? x) 
                  (procedure-arity-includes? x l)
                  (no-mandatory-keywords? x))))))
   #:stronger
   (λ (this that)
      (and (->? that)
           (= (length (->-doms that))
              (length (->-doms this)))
           (andmap contract-stronger?
                   (->-doms that)
                   (->-doms this))
           (= (length (->-rngs that))
              (length (->-rngs this)))
           (andmap contract-stronger?
                   (->-rngs this) 
                   (->-rngs that))))))

(define (single-arrow-name-maker doms/c doms-rest kwds/c kwds rng-any? rngs)
  (cond
    [doms-rest
     (build-compound-type-name 
      '->*
      (apply build-compound-type-name (append doms/c (apply append (map list kwds kwds/c))))
      doms-rest
      (cond
        [rng-any? 'any]
        [else (apply build-compound-type-name rngs)]))]
    [else
     (let ([rng-name
            (cond
              [rng-any? 'any]
              [(null? rngs) '(values)]
              [(null? (cdr rngs)) (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply build-compound-type-name 
              '->
              (append doms/c
                      (apply append (map list kwds kwds/c))
                      (list rng-name))))]))

(define-for-syntax (sort-keywords stx kwd/ctc-pairs)
  (define (insert x lst)
    (cond
      [(null? lst) (list x)]
      [else
       (let ([fst-kwd (syntax-e (car (car lst)))])
         #;(printf "comparing ~s to ~s\n" (car x) fst-kwd)
         (cond
           [(equal? (syntax-e (car x)) fst-kwd)
            (raise-syntax-error #f 
                                "duplicate keyword"
                                stx
                                (car x))]
           [(keyword<? (syntax-e (car x)) fst-kwd)
            (cons x lst)]
           [else (cons (car lst) (insert x (cdr lst)))]))]))
  
  (let loop ([pairs (map syntax->list kwd/ctc-pairs)])
    (cond
      [(null? pairs) null]
      [else (insert (car pairs) (loop (cdr pairs)))])))

(define-for-syntax (split-doms stx name raw-doms)
  (let loop ([raw-doms raw-doms]
             [doms '()]
             [kwd-doms '()])
    (syntax-case raw-doms ()
      [() (list (reverse doms)
                (sort-keywords stx kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (not (keyword? (syntax-e #'arg))))
       (loop #'rest
             doms
             (cons #'(kwd arg) kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (keyword? (syntax-e #'arg)))
       (raise-syntax-error name
                           "expected a keyword followed by a contract"
                           stx
                           #'kwd)]
      [(kwd)
       (keyword? (syntax-e #'kwd))
       (raise-syntax-error name
                           "expected a keyword to be followed by a contract"
                           stx
                           #'kwd)]
      [(x . rest)
       (loop #'rest (cons #'x doms) kwd-doms)])))

(define-for-syntax (->-helper stx)
  (syntax-case stx ()
    [(-> raw-doms ... last-one)
     (with-syntax ([((doms ...) ((dom-kwd dom-kwd-ctc) ...)) (split-doms stx '-> #'(raw-doms ...))])
       (with-syntax ([(dom-kwd-arg ...) (generate-temporaries (syntax (dom-kwd ...)))]
                     [(dom-kwd-ctc-id ...) (generate-temporaries (syntax (dom-kwd ...)))])
         (with-syntax ([(keyword-call/ctc ...) (apply append (map syntax->list (syntax->list #'((dom-kwd (dom-kwd-ctc-id dom-kwd-arg)) ...))))]
                       [(keyword-formal-parameters ...) (apply append (map syntax->list (syntax->list #'((dom-kwd dom-kwd-arg) ...))))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))])
           (syntax-case* #'last-one (-> any values) module-or-top-identifier=?
             [any
              (with-syntax ([(ignored) (generate-temporaries (syntax (rng)))])
                (values (syntax (dom-ctc ...))
                        (syntax (ignored))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (any/c))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) (val (dom-ctc args) ... keyword-call/ctc ...)))
                        #t))]
             [(values rngs ...)
              (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                            [(rng-ctc ...) (generate-temporaries (syntax (rngs ...)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc ...))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rngs ...))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) 
                                 (let-values ([(rng-x ...) (val (dom-ctc args) ... keyword-call/ctc ...)])
                                   (values (rng-ctc rng-x) ...))))
                        #f))]
             [rng
              (with-syntax ([(rng-ctc) (generate-temporaries (syntax (rng)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rng))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) (rng-ctc (val (dom-ctc args) ... keyword-call/ctc ...))))
                        #f))]))))]))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->/proc/main stx)
  (let-values ([(dom-names rng-names kwd-names dom-ctcs rng-ctcs kwd-ctcs kwds inner-args/body use-any?) (->-helper stx)])
    (with-syntax ([(args body) inner-args/body])
      (with-syntax ([(dom-names ...) dom-names]
                    [(rng-names ...) rng-names]
                    [(kwd-names ...) kwd-names]
                    [(dom-ctcs ...) dom-ctcs]
                    [(rng-ctcs ...) rng-ctcs]
                    [(kwd-ctcs ...) kwd-ctcs]
                    [(kwds ...) kwds]
                    [inner-lambda 
                     (add-name-prop
                      (syntax-local-infer-name stx)
                      (syntax (lambda args body)))]
                    [use-any? use-any?])
        (with-syntax ([outer-lambda
                       (syntax
                        (lambda (chk ctc dom-names ... rng-names ... kwd-names ...)
                          (lambda (val)
                            (chk val)
                            (make-contracted-function inner-lambda ctc))))])
          (values
           (syntax (build--> '->
                             (list dom-ctcs ...)
                             #f
                             (list rng-ctcs ...)
                             (list kwd-ctcs ...)
                             '(kwds ...)
                             use-any?
                             outer-lambda))
           inner-args/body
           (syntax (dom-names ... rng-names ...))))))))
  
(define-syntax (-> stx) 
  (let-values ([(stx _1 _2) (->/proc/main stx)])
    stx))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->*/proc/main stx)
  (syntax-case* stx (->* any) module-or-top-identifier=?
    [(->* (doms ...) any)
     (->/proc/main (syntax (-> doms ... any)))]
    [(->* (doms ...) (rngs ...))
     (->/proc/main (syntax (-> doms ... (values rngs ...))))]
    [(->* (raw-doms ...) rst rng)
     (with-syntax ([((doms ...) ((dom-kwd dom-kwd-ctc) ...)) (split-doms stx '-> #'(raw-doms ...))])
       (with-syntax ([(dom-kwd-arg ...) (generate-temporaries (syntax (dom-kwd ...)))]
                     [(dom-kwd-ctc-id ...) (generate-temporaries (syntax (dom-kwd ...)))])
         (with-syntax ([(keyword-formal-parameters ...) (apply append (map syntax->list (syntax->list #'((dom-kwd dom-kwd-arg) ...))))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))])
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (doms ...)))]
                         [(args ...) (generate-temporaries (syntax (doms ...)))]
                         [(rst-x) (generate-temporaries (syntax (rst)))]
                         [(rest-arg) (generate-temporaries (syntax (rst)))])
             (syntax-case #'rng (any)
               [(rngs ...)
                (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                              [(rng-args ...) (generate-temporaries (syntax (rngs ...)))])
                  
                  (let ([inner-args/body 
                         #`((args ... keyword-formal-parameters ... . rest-arg)
                            (let-values ([(rng-args ...)
                                          #,(if (null? (syntax-e #'(dom-kwd ...)))
                                                #'(apply val (dom-x args) ... (rst-x rest-arg))
                                                #'(keyword-apply val 
                                                                 '(dom-kwd ...)
                                                                 (list (dom-kwd-ctc-id dom-kwd-arg) ...) 
                                                                 (dom-x args) ...
                                                                 (rst-x rest-arg)))])
                              (values (rng-x rng-args) ...)))])
                    (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                                  (add-name-prop
                                                   (syntax-local-infer-name stx)
                                                   (syntax (lambda args body))))])
                      (with-syntax ([outer-lambda 
                                     (syntax
                                      (lambda (chk ctc dom-x ... rst-x rng-x ... dom-kwd-ctc-id ...)
                                        (lambda (val)
                                          (chk val)
                                          (make-contracted-function inner-lambda ctc))))])
                        (values (syntax (build--> '->*
                                                  (list doms ...)
                                                  rst
                                                  (list rngs ...)
                                                  (list dom-kwd-ctc ...)
                                                  '(dom-kwd ...)
                                                  #f 
                                                  outer-lambda))
                                inner-args/body
                                (syntax (dom-x ... rst-x rng-x ...)))))))]
               [any
                (let ([inner-args/body 
                       #`((args ... keyword-formal-parameters ... . rest-arg)
                          #,(if (null? (syntax-e #'(dom-kwd ...)))
                                #'(apply val (dom-x args) ... (rst-x rest-arg))
                                #'(keyword-apply val
                                                 '(dom-kwd ...) 
                                                 (list (dom-kwd-ctc-id dom-kwd-arg) ...)
                                                 (dom-x args) ...
                                                 (rst-x rest-arg))))])
                  (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                                (add-name-prop
                                                 (syntax-local-infer-name stx)
                                                 (syntax (lambda args body))))])
                    (with-syntax ([outer-lambda 
                                   (syntax
                                    (lambda (chk ctc dom-x ... rst-x ignored dom-kwd-ctc-id ...)
                                      (lambda (val)
                                        (chk val)
                                        (make-contracted-function inner-lambda ctc))))])
                      (values (syntax (build--> '->*
                                                (list doms ...)
                                                rst
                                                (list any/c)
                                                (list dom-kwd-ctc ...)
                                                '(dom-kwd ...)
                                                #t
                                                outer-lambda))
                              inner-args/body
                              (syntax (dom-x ... rst-x))))))])))))]))

(define-syntax (->* stx) 
  (let-values ([(stx _1 _2) (->*/proc/main stx)])
    stx))

(define-for-syntax (select/h stx err-name ctxt-stx)
  (syntax-case stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
    [(-> . args) ->/h]
    [(->* . args) ->*/h]
    [(->d . args) ->d/h]
    [(->d* . args) ->d*/h]
    [(->r . args) ->r/h]
    [(->pp . args) ->pp/h]
    [(->pp-rest . args) ->pp-rest/h]
    [(xxx . args) (raise-syntax-error err-name "unknown arrow constructor" ctxt-stx (syntax xxx))]
    [_ (raise-syntax-error err-name "malformed arrow clause" ctxt-stx stx)]))

(define-syntax (->d stx) (make-/proc #f ->d/h stx))
(define-syntax (->d* stx) (make-/proc #f ->d*/h stx))
(define-syntax (->r stx) (make-/proc #f ->r/h stx))
(define-syntax (->pp stx) (make-/proc #f ->pp/h stx))
(define-syntax (->pp-rest stx) (make-/proc #f ->pp-rest/h stx))
(define-syntax (case-> stx) (make-case->/proc #f stx stx select/h))
(define-syntax (opt-> stx) (make-opt->/proc #f stx select/h #'case-> #'->))
(define-syntax (opt->* stx) (make-opt->*/proc #f stx stx select/h #'case-> #'->))

