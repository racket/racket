#lang racket/base

(require "helpers.rkt"
         "blame.rkt"
         "prop.rkt"
         "rand.rkt"
         "generate-base.rkt"
         racket/pretty)

(require (for-syntax racket/base))

(provide coerce-contract
         coerce-contracts
         coerce-flat-contract
         coerce-flat-contracts
         coerce-chaperone-contract
         coerce-chaperone-contracts
         coerce-contract/f
         
         build-compound-type-name
         
         contract-stronger?

         contract-first-order
         contract-first-order-passes?
         
         prop:contracted
         impersonator-prop:contracted
         has-contract?
         value-contract
         
         ;; for opters
         check-flat-contract
         check-flat-named-contract
         
         ;; helpers for adding properties that check syntax uses
         define/final-prop
         define/subexpression-pos-prop
         
         make-predicate-contract
         
         eq-contract?
         eq-contract-val
         equal-contract?
         equal-contract-val)

(define (has-contract? v)
  (or (has-prop:contracted? v)
      (has-impersonator-prop:contracted? v)))

(define (value-contract v)
  (cond
    [(has-prop:contracted? v)
     (get-prop:contracted v)]
    [(has-impersonator-prop:contracted? v)
     (get-impersonator-prop:contracted v)]
    [else #f]))

(define-values (prop:contracted has-prop:contracted? get-prop:contracted)
  (let-values ([(prop pred get)
                (make-struct-type-property
                 'prop:contracted
                 (lambda (v si)
                   (if (number? v)
                       (let ([ref (cadddr si)])
                         (lambda (s) (ref s v)))
                       (lambda (s) v))))])
    (values prop pred (λ (v) ((get v) v)))))

(define-values (impersonator-prop:contracted has-impersonator-prop:contracted? get-impersonator-prop:contracted)
  (make-impersonator-property 'impersonator-prop:contracted))

(define (contract-first-order c)
  (contract-struct-first-order
   (coerce-contract 'contract-first-order c)))

(define (contract-first-order-passes? c v)
  ((contract-struct-first-order
    (coerce-contract 'contract-first-order-passes? c))
   v))

;; contract-stronger? : contract contract -> boolean
;; indicates if one contract is stronger (ie, likes fewer values) than another
;; this is not a total order.
(define (contract-stronger? a b)
  (contract-struct-stronger? (coerce-contract 'contract-stronger? a)
                             (coerce-contract 'contract-stronger? b)))

;; coerce-flat-contract : symbol any/c -> contract
(define (coerce-flat-contract name x)
  (let ([ctc (coerce-contract/f x)])
    (unless (flat-contract-struct? ctc)
      (error name 
             "expected a flat contract or a value that can be coerced into one, got ~e"
             x))
    ctc))

;; coerce-flat-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on flat-contracts
(define (coerce-flat-contracts name xs) 
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless (flat-contract-struct? ctc)
        (error name
               "expected all of the arguments to be flat contracts, but argument ~a was not, got ~e"
               i
               x))
      ctc)))

;; coerce-chaperone-contract : symbol any/c -> contract
(define (coerce-chaperone-contract name x)
  (let ([ctc (coerce-contract/f x)])
    (unless (chaperone-contract-struct? ctc)
      (error name
             "expected a chaperone contract or a value that can be coerced into one, got ~e"
             x))
    ctc))

;; coerce-chaperone-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on chaperone-contracts
(define (coerce-chaperone-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless (chaperone-contract-struct? ctc)
        (error name
               "expected all of the arguments to be chaperone contracts, but argument ~a was not, got ~e"
               i
               x))
      ctc)))

;; coerce-contract : symbol any/c -> contract
(define (coerce-contract name x)
  (or (coerce-contract/f x)
      (error name 
             "expected contract or a value that can be coerced into one, got ~e"
             x)))

;; coerce-contracts : symbols (listof any) -> (listof contract)
;; turns all of the arguments in 'xs' into contracts
;; the error messages assume that the function named by 'name'
;; got 'xs' as it argument directly
(define (coerce-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless ctc
        (error name
              "expected all of the arguments to be contracts, but argument ~a was not, got ~e"
              i
              x))
      ctc)))

;; coerce-contract/f : any -> (or/c #f contract?)
;; returns #f if the argument could not be coerced to a contract
(define (coerce-contract/f x)
  (cond
    [(contract-struct? x) x]
    [(and (procedure? x) (procedure-arity-includes? x 1)) 
     (make-predicate-contract (or (object-name x) '???) x (make-generate-ctc-fail))]
    [(or (symbol? x) (boolean? x) (char? x) (null? x) (keyword? x)) (make-eq-contract x)]
    [(or (bytes? x) (string? x)) (make-equal-contract x)]
    [(number? x) (make-=-contract x)]
    [(or (regexp? x) (byte-regexp? x)) (make-regexp/c x)]
    [else #f]))

(define-syntax (define/final-prop stx)
  (syntax-case stx ()
    [(_ header bodies ...)
     (with-syntax ([ctc 
                    (syntax-case #'header ()
                      [id
                       (identifier? #'id)
                       #'id]
                      [(id1 . rest)
                       (identifier? #'id1)
                       #'id1]
                      [_ 
                       (raise-syntax-error #f 
                                           "malformed header position"
                                           stx 
                                           #'header)])])
       (with-syntax ([ctc/proc (string->symbol (format "~a/proc" (syntax-e #'ctc)))])
         #'(begin
             (define ctc/proc
               (let ()
                 (define header bodies ...)
                 ctc))
             (define-syntax (ctc stx)
               (syntax-case stx ()
                 [x
                  (identifier? #'x)
                  (syntax-property 
                   #'ctc/proc
                   'racket/contract:contract 
                   (vector (gensym 'ctc) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (with-syntax ([app (datum->syntax stx '#%app)])
                    (syntax-property 
                     #'(app ctc/proc margs (... ...))
                     'racket/contract:contract 
                     (vector (gensym 'ctc) 
                             (list (car (syntax-e stx)))
                             '())))])))))]))

(define-syntax (define/subexpression-pos-prop stx)
  (syntax-case stx ()
    [(_ header bodies ...)
     (with-syntax ([ctc (if (identifier? #'header)
                            #'header
                            (car (syntax-e #'header)))])
       (with-syntax ([ctc/proc (string->symbol (format "~a/proc" (syntax-e #'ctc)))])
         #'(begin
             (define ctc/proc
               (let ()
                 (define header bodies ...)
                 ctc))
             (define-syntax (ctc stx)
               (syntax-case stx ()
                 [x
                  (identifier? #'x)
                  (syntax-property 
                   #'ctc/proc
                   'racket/contract:contract 
                   (vector (gensym 'ctc) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (let ([this-one (gensym 'ctc)])
                    (with-syntax ([(margs (... ...)) 
                                   (map (λ (x) (syntax-property x 'racket/contract:positive-position this-one))
                                        (syntax->list #'(margs (... ...))))]
                                  [app (datum->syntax stx '#%app)])
                      (syntax-property 
                       #'(app ctc/proc margs (... ...))
                       'racket/contract:contract 
                       (vector this-one 
                               (list (car (syntax-e stx)))
                               '()))))])))))]))

;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
(define (build-compound-type-name . fs)
  (for/list ([sub (in-list fs)])
    (if (contract-struct? sub) (contract-struct-name sub) sub)))


;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;            ;                      ;;;                                       ;                         ;         
;          ;;;                                                              ;;;                       ;;;         
;   ;;;;; ;;;;;   ;;;   ;;; ;; ;;;  ;;;   ;;;         ;;;     ;;;   ;;; ;; ;;;;; ;;; ;;;;;;;    ;;;  ;;;;;  ;;;;  
;  ;;;;;;;;;;;;  ;;;;;  ;;;;;;;;;;; ;;;  ;;;;;       ;;;;;   ;;;;;  ;;;;;;;;;;;; ;;;;;;;;;;;;  ;;;;; ;;;;; ;;; ;; 
;  ;;  ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;     ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;  ;;; ;;;  ;; ;;;  ;;;    
;    ;;;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;         ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;    ;;;;; ;;;     ;;;   ;;;;  
;  ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;     ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;     ;;; 
;  ;;; ;;; ;;;;  ;;;;;  ;;; ;;; ;;; ;;;  ;;;;;       ;;;;;   ;;;;;  ;;; ;;; ;;;; ;;;  ;;; ;;;  ;;;;;  ;;;; ;; ;;; 
;   ;;;;;;  ;;;   ;;;   ;;; ;;; ;;; ;;;   ;;;         ;;;     ;;;   ;;; ;;;  ;;; ;;;   ;;;;;;   ;;;    ;;;  ;;;;  
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 

(define-struct eq-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (eq? (eq-contract-val ctc) x)))
   #:name
   (λ (ctc) 
      (if (symbol? (eq-contract-val ctc))
        `',(eq-contract-val ctc)
        (eq-contract-val ctc)))
   #:generate
   (λ (ctc) (λ (fuel) (eq-contract-val ctc)))
   #:stronger
   (λ (this that)
      (and (eq-contract? that)
           (eq? (eq-contract-val this) (eq-contract-val that))))))

(define-struct equal-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (equal? (equal-contract-val ctc) x)))
   #:name (λ (ctc) (equal-contract-val ctc))
   #:stronger
   (λ (this that)
      (and (equal-contract? that)
           (equal? (equal-contract-val this) (equal-contract-val that))))
   #:generate
   (λ (ctc) (λ (fuel) (equal-contract-val ctc)))))

(define-struct =-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (and (number? x) (= (=-contract-val ctc) x))))
   #:name (λ (ctc) (=-contract-val ctc))
   #:stronger
   (λ (this that)
      (and (=-contract? that)
           (= (=-contract-val this) (=-contract-val that))))
   #:generate
   (λ (ctc) (λ (fuel) (=-contract-val ctc)))))

(define-struct regexp/c (reg)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order
   (λ (ctc)
     (define reg (regexp/c-reg ctc))
      (λ (x)
         (and (or (string? x) (bytes? x))
              (regexp-match? reg x))))
   #:name (λ (ctc) (regexp/c-reg ctc))
   #:stronger
   (λ (this that)
      (and (regexp/c? that) (eq? (regexp/c-reg this) (regexp/c-reg that))))))


(define-struct predicate-contract (name pred generate)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:stronger
   (λ (this that) 
      (and (predicate-contract? that)
           (procedure-closure-contents-eq? (predicate-contract-pred this)
                                           (predicate-contract-pred that))))
   #:name (λ (ctc) (predicate-contract-name ctc))
   #:first-order (λ (ctc) (predicate-contract-pred ctc))
   #:generate (λ (ctc)
                 (let ([generate (predicate-contract-generate ctc)])
                   (if (generate-ctc-fail? generate)
                     (let ([fn (predicate-contract-pred ctc)])
                       (find-generate fn (predicate-contract-name ctc)))
                     generate)))
   #:exercise (λ (ctc)
                 (λ (val fuel) 
                    ((predicate-contract-pred ctc) val)))))

(define (check-flat-named-contract predicate) (coerce-flat-contract 'flat-named-contract predicate))
(define (check-flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (build-flat-contract name pred [generate (make-generate-ctc-fail)])
  (make-predicate-contract name pred generate))
