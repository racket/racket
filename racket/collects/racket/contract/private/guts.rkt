#lang racket/base

(require "helpers.rkt"
         "blame.rkt"
         "prop.rkt"
         "rand.rkt"
         "generate-base.rkt"
         "collapsible-common.rkt"
         (submod "collapsible-common.rkt" properties)
         "../../private/math-predicates.rkt"
         racket/pretty
         racket/list
         (for-syntax racket/base
                     "helpers.rkt"))

(provide coerce-contract
         coerce-contracts
         coerce-flat-contract
         coerce-flat-contracts
         coerce-chaperone-contract
         coerce-chaperone-contracts
         coerce-contract/f
         
         build-compound-type-name
         
         contract-stronger?
         contract-equivalent?
         list-contract?
                  
         contract-first-order
         contract-first-order-passes?
         
         prop:contracted prop:blame
         impersonator-prop:contracted
         impersonator-prop:blame

         has-contract? value-contract
         has-blame? value-blame
         
         ;; for opters
         check-flat-contract
         check-flat-named-contract
         
         ;; helpers for adding properties that check syntax uses
         define/final-prop
         define/subexpression-pos-prop
         define/subexpression-pos-prop/name
         
         make-predicate-contract
         
         eq-contract?
         eq-contract-val
         equal-contract?
         equal-contract-val
         char-in/c

         contract?
         chaperone-contract?
         impersonator-contract?
         flat-contract?
         
         contract-continuation-mark-key
         with-contract-continuation-mark
         collapsible-contract-continuation-mark-key
         with-collapsible-contract-continuation-mark
         
         (struct-out wrapped-extra-arg-arrow)
         contract-custom-write-property-proc
         (rename-out [contract-custom-write-property-proc custom-write-property-proc])

         contract-projection
         contract-val-first-projection  ;; might return #f (if none)
         contract-late-neg-projection   ;; might return #f (if none)
         get/build-val-first-projection ;; builds one if necc., using contract-projection
         get/build-late-neg-projection
         get/build-collapsible-late-neg-projection
         warn-about-val-first?

         contract-name
         maybe-warn-about-val-first
         
         set-some-basic-list-contracts!
         set-some-basic-misc-contracts!
         set-some-basic-integer-in-contracts!

         contract-first-order-okay-to-give-up?
         contract-first-order-try-less-hard
         contract-first-order-only-try-so-hard

         raise-predicate-blame-error-failure

         n->th
         nth-argument-of
         nth-element-of
         nth-case-of

         false/c-contract
         true/c-contract

         contract-pos/neg-doubling
         contract-pos/neg-doubling.2)

(define (contract-custom-write-property-proc stct port mode)
  (define (write-prefix)
    (write-string "#<" port)
    (cond
      [(flat-contract-struct? stct) (write-string "flat-" port)]
      [(chaperone-contract-struct? stct) (write-string "chaperone-" port)])
    (write-string "contract: " port))
  (define (write-suffix)
    (write-string ">" port))
  (cond
    [(boolean? mode)
     (write-prefix)
     (write-string (format "~.s" (contract-struct-name stct)) port)
     (write-suffix)]
    [else
     (cond
       [(zero? mode)
        (print (contract-struct-name stct) port 1)]
       [else
        (write-prefix)
        (print (contract-struct-name stct) port 1)
        (write-suffix)])]))

(define (contract? x)
  (or (simple-flat-contract? x)
      (and (coerce-contract/f x) #t)))

(define (flat-contract? x)
  (or (simple-flat-contract? x)
      (let ([c (coerce-contract/f x)])
        (and c
             (flat-contract-struct? c)))))

(define (chaperone-contract? x)
  (or (simple-flat-contract? x)
      (let ([c (coerce-contract/f x)])
        (and c
             (chaperone-contract-struct? c)))))
  
(define (simple-flat-contract? x)
  (or (and (procedure? x) (procedure-arity-includes? x 1))
      (null? x)
      (boolean? x)
      (symbol? x)
      (keyword? x)
      (char? x)
      (bytes? x)
      (string? x)
      (number? x)
      (regexp? x)
      (byte-regexp? x)))

(define (impersonator-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (not (flat-contract-struct? c))
         (not (chaperone-contract-struct? c)))))


(define (has-contract? v)
  (or (has-prop:contracted? v)
      (has-impersonator-prop:contracted? v)
      ;; TODO: I think this is the right check, but I'm not positive
      (has-impersonator-prop:collapsible? v)))

(define (value-contract v)
  (cond
    [(has-prop:contracted? v)
     (get-prop:contracted v)]
    [(has-impersonator-prop:contracted? v)
     (get-impersonator-prop:contracted v)]
    [(get-impersonator-prop:collapsible v #f)
     =>
     (λ (p)
       (collapsible-ho/c-latest-ctc (collapsible-property-c-c p)))]
    [else #f]))

(define (has-blame? v)
  (or (has-prop:blame? v)
      (has-impersonator-prop:blame? v)
      ;; TODO: I think this check is ok, but I'm not sure ...
      (has-impersonator-prop:collapsible? v)))

(define (value-blame v)
  (define bv
    (cond
      [(has-prop:blame? v)
       (get-prop:blame v)]
      [(has-impersonator-prop:blame? v)
       (get-impersonator-prop:blame v)]
      [(get-impersonator-prop:collapsible v #f)
       =>
       (λ (p)
         (define c-c (collapsible-property-c-c p))
         (cons
          (collapsible-ho/c-latest-blame c-c)
          (or (collapsible-ho/c-missing-party c-c) (collapsible-property-neg-party p))))]
      [else #f]))
  (cond
    [(and (pair? bv) (blame? (car bv)))
     (blame-add-missing-party (car bv) (cdr bv))]
    [(blame? bv) bv]
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

(define-values (prop:blame has-prop:blame? get-prop:blame)
  (let-values ([(prop pred get)
                (make-struct-type-property
                 'prop:blame
                 (lambda (v si)
                   (if (number? v)
                       (let ([ref (cadddr si)])
                         (lambda (s) (ref s v)))
                       (lambda (s) v))))])
    (values prop pred (λ (v) ((get v) v)))))

(define-values (impersonator-prop:contracted 
                has-impersonator-prop:contracted? 
                get-impersonator-prop:contracted)
  (make-impersonator-property 'impersonator-prop:contracted))

(define-values (impersonator-prop:blame
                has-impersonator-prop:blame? 
                get-impersonator-prop:blame)
  (make-impersonator-property 'impersonator-prop:blame))

(define (contract-first-order c)
  (contract-struct-first-order
   (coerce-contract 'contract-first-order c)))

(define (contract-first-order-passes? c v)
  ((contract-struct-first-order
    (coerce-contract 'contract-first-order-passes? c))
   v))

(define (list-contract? raw-c)
  (define c (coerce-contract/f raw-c))
  (and c (contract-struct-list-contract? c)))

;; contract-stronger? : contract contract -> boolean
;; indicates if one contract is stronger (ie, likes fewer values) than another
;; this is not a total order.
(define (contract-stronger? a b)
  (contract-struct-stronger? (coerce-contract 'contract-stronger? a)
                             (coerce-contract 'contract-stronger? b)))

(define (contract-equivalent? a b)
  (contract-struct-equivalent? (coerce-contract 'contract-equivalent? a)
                               (coerce-contract 'contract-equivalent? b)))

;; coerce-flat-contract : symbol any/c -> contract
(define (coerce-flat-contract name x)
  (define ctc (coerce-contract/f x))
  (unless (flat-contract-struct? ctc)
    (raise-argument-error name "flat-contract?" x))
  ctc)

;; coerce-flat-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on flat-contracts
(define (coerce-flat-contracts name xs) 
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (define ctc (coerce-contract/f x))
    (unless (flat-contract-struct? ctc)
      (raise-argument-error name
                            "flat-contract?"
                            i 
                            xs))
    ctc))

;; coerce-chaperone-contract : symbol any/c -> contract
(define (coerce-chaperone-contract name x)
  (define ctc (coerce-contract/f x))
  (unless (chaperone-contract-struct? ctc)
    (raise-argument-error
     name
     "chaperone-contract?"
     x))
  ctc)

;; coerce-chaperone-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on chaperone-contracts
(define (coerce-chaperone-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (define ctc (coerce-contract/f x))
    (unless (chaperone-contract-struct? ctc)
      (apply raise-argument-error
             name
             "chaperone-contract?"
             i
             xs))
    ctc))

;; coerce-contract : symbol any/c -> contract
(define (coerce-contract name x)
  (or (coerce-contract/f x)
      (raise-argument-error name
                            "contract?"
                            x)))

;; coerce-contracts : symbol (listof any) -> (listof contract)
;; turns all of the arguments in 'xs' into contracts
;; the error messages assume that the function named by 'name'
;; got 'xs' as it argument directly
(define (coerce-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (define ctc (coerce-contract/f x))
    (unless ctc
      (apply raise-argument-error
             name
             "contract?"
             i
             xs))
    ctc))

;; coerce-contract/f : any -> (or/c #f contract?)
;; returns #f if the argument could not be coerced to a contract
(define-values (name-default name-default?)
  (let ()
    (struct name-default ())
    (values (name-default) name-default?)))

;; these definitions work around a cyclic
;; dependency. When we coerce a value to a contract,
;; we want to use (listof any/c) for list?, but
;; the files are not set up for that, so we just
;; bang it in here and use it only after it's been banged in.
;; ditto for: (cons/c any/c any/c), (list/c), and (between/c -inf.0 +inf.0)
;; the selectors and predicate for `between/c-s` are used
;; to get contract-stronger right for numeric constants
(define listof-any #f)
(define consc-anyany #f)
(define list/c-empty #f)
(define (set-some-basic-list-contracts! l p mt)
  (set! listof-any l)
  (set! consc-anyany p)
  (set! list/c-empty mt))
(define between/c-inf+inf-as-real? #f)
(define renamed-between/c #f)
(define between/c-s? #f)
(define between/c-s-low #f)
(define between/c-s-high #f)
(define (set-some-basic-misc-contracts! b r-b b/c-s? b/c-s-l b/c-s-h)
  (set! between/c-inf+inf-as-real? b)
  (set! renamed-between/c r-b)
  (set! between/c-s? b/c-s?)
  (set! between/c-s-low b/c-s-l)
  (set! between/c-s-high b/c-s-h))
(define integer-in-ff #f)
(define integer-in-0f #f)
(define integer-in-1f #f)
(define renamed-integer-in #f)
(define (set-some-basic-integer-in-contracts! r-ii ff 0f 1f)
  (set! renamed-integer-in r-ii)
  (set! integer-in-ff ff)
  (set! integer-in-0f 0f)
  (set! integer-in-1f 1f))

(define (coerce-contract/f x [name name-default])
  (cond
    [(coerce-simple-value name x) => values]
    [(name-default? name) (and (contract-struct? x) x)]
    [(predicate-contract? x)
     (struct-copy predicate-contract x [name name])]
    [(eq-contract? x) (make-eq-contract (eq-contract-val x) name)]
    [(equal-contract? x) (make-eq-contract (equal-contract-val x) name)]
    [(=-contract? x) (make-=-contract (=-contract-val x) name)]
    [(regexp/c? x) (make-regexp/c (regexp/c-reg x) name)]
    [else #f]))


(define (coerce-simple-value name x)
  (cond
    [(contract-struct? x) #f] ;; this has to come first, since some of these are procedure?.
    [(and (procedure? x) (procedure-arity-includes? x 1))
     (cond
       [(chaperone-of? x null?) list/c-empty]
       [(chaperone-of? x empty?) list/c-empty]
       [(chaperone-of? x list?)
        (unless listof-any
          (error 'coerce-contract/f::listof-any "too soon!"))
        listof-any]
       [(chaperone-of? x boolean?) boolean?/c]
       [(or (chaperone-of? x pair?)
            (chaperone-of? x cons?))
        (unless consc-anyany
          (error 'coerce-contract/f::consc-anyany "too soon!"))
        consc-anyany]
       [(chaperone-of? x real?)
        (unless between/c-inf+inf-as-real?
          (error 'coerce-contract/f::between/c-inf+inf "too soon!"))
        (if (name-default? name)
            between/c-inf+inf-as-real?
            (renamed-between/c -inf.0 +inf.0 name))]
       [(chaperone-of? x exact-positive-integer?)
        (if (name-default? name) integer-in-1f (renamed-integer-in 1 #f name))]
       [(chaperone-of? x exact-nonnegative-integer?)
        (if (name-default? name) integer-in-0f (renamed-integer-in 0 #f name))]
       [(chaperone-of? x natural?)
        (if (name-default? name) integer-in-0f (renamed-integer-in 0 #f name))]
       [(chaperone-of? x exact-integer?)
        (if (name-default? name) integer-in-ff (renamed-integer-in #f #f name))]
       [else
        (make-predicate-contract (if (name-default? name)
                                     (or (object-name x) '???)
                                     name)
                                 x
                                 #f
                                 (or (struct-predicate-procedure? x)
                                     (memq x the-known-good-contracts)))])]
    [(null? x)
     (unless list/c-empty
       (error 'coerce-contract/f::list/c-empty "too soon!"))
     list/c-empty]
    [(not x) false/c-contract]
    [(equal? x #t) true/c-contract]
    [(or (symbol? x) (boolean? x) (keyword? x))
     (make-eq-contract x
                       (if (name-default? name)
                           (if (or (null? x)
                                   (symbol? x))
                               `',x
                               x)
                           name))]
    [(char? x) (make-char-in/c x x)]
    [(or (bytes? x) (string? x) (equal? +nan.0 x) (equal? +nan.f x))
     (make-equal-contract x (if (name-default? name) x name))]
    [(number? x)
     (make-=-contract x (if (name-default? name) x name))]
    [(or (regexp? x) (byte-regexp? x)) (make-regexp/c x (if (name-default? name) x name))]
    [else #f]))

(define the-known-good-contracts
  (let-syntax ([m (λ (x) #`(list #,@(known-good-contracts)))])
    (m)))

(struct wrapped-extra-arg-arrow (real-func extra-neg-party-argument)
  #:property prop:procedure 0)

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

(define-syntax (define/subexpression-pos-prop/name stx)
  (syntax-case stx ()
    [(_ ctc/proc header bodies ...)
     (with-syntax ([ctc (if (identifier? #'header)
                            #'header
                            (car (syntax-e #'header)))])
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
                                 (map (λ (x) (syntax-property x
                                                              'racket/contract:positive-position
                                                              this-one))
                                      (syntax->list #'(margs (... ...))))]
                                [app (datum->syntax stx '#%app)])
                    (syntax-property
                     (syntax/loc stx (app ctc/proc margs (... ...)))
                     'racket/contract:contract
                     (vector this-one
                             (list (car (syntax-e stx)))
                             '()))))]))))]))

(define-syntax (define/subexpression-pos-prop stx)
  (syntax-case stx ()
    [(_ header bodies ...)
     (with-syntax ([ctc (if (identifier? #'header)
                            #'header
                            (car (syntax-e #'header)))])
       (with-syntax ([ctc/proc (string->symbol (format "~a/proc" (syntax-e #'ctc)))])
         #'(define/subexpression-pos-prop/name ctc/proc header bodies ...)))]))

;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
(define (build-compound-type-name . fs)
  (for/list ([sub (in-list fs)])
    (if (contract-struct? sub) (contract-struct-name sub) sub)))


;
;
;            ;                      ;;;
;          ;;;
;   ;;;;; ;;;;;   ;;;   ;;; ;; ;;;  ;;;   ;;;
;  ;;;;;;;;;;;;  ;;;;;  ;;;;;;;;;;; ;;;  ;;;;;
;  ;;  ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;
;    ;;;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;
;  ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;
;  ;;; ;;; ;;;;  ;;;;;  ;;; ;;; ;;; ;;;  ;;;;;
;   ;;;;;;  ;;;   ;;;   ;;; ;;; ;;; ;;;   ;;;
;
;
;
;
;
;                            ;                         ;
;                          ;;;                       ;;;
;    ;;;     ;;;   ;;; ;;  ;;;; ;;; ;;;;;;;    ;;;   ;;;;  ;;;;
;   ;;;;;   ;;;;;  ;;;;;;; ;;;; ;;;;;;;;;;;;  ;;;;;  ;;;; ;;; ;;
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;  ;;; ;;;  ;; ;;;  ;;;
;  ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;    ;;;;; ;;;     ;;;   ;;;;
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;     ;;;
;   ;;;;;   ;;;;;  ;;; ;;; ;;;; ;;;  ;;; ;;;  ;;;;;  ;;;; ;; ;;;
;    ;;;     ;;;   ;;; ;;;  ;;; ;;;   ;;;;;;   ;;;    ;;;  ;;;;
;
;
;
;

(define-struct eq-contract (val name)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:first-order (λ (ctc) (λ (x) (eq? (eq-contract-val ctc) x)))
   #:name (λ (ctc) (eq-contract-name ctc))
   #:generate
   (λ (ctc) 
     (define v (eq-contract-val ctc))
     (λ (fuel) (λ () v)))
   #:stronger
   (λ (this that)
     (define this-val (eq-contract-val this))
     (or (and (eq-contract? that)
              (eq? this-val (eq-contract-val that)))
         (and (predicate-contract? that)
              (predicate-contract-sane? that)
              ((predicate-contract-pred that) this-val))))
   #:equivalent
   (λ (this that)
     (define this-val (eq-contract-val this))
     (and (eq-contract? that)
          (eq? this-val (eq-contract-val that))))
   #:list-contract? (λ (c) (null? (eq-contract-val c)))))

(define false/c-contract (make-eq-contract #f #f))
(define true/c-contract (make-eq-contract #t #t))

(define-struct equal-contract (val name)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:first-order (λ (ctc) (λ (x) (equal? (equal-contract-val ctc) x)))
   #:name (λ (ctc) (equal-contract-name ctc))
   #:stronger
   (λ (this that)
     (define this-val (equal-contract-val this))
     (or (and (equal-contract? that)
              (equal? this-val (equal-contract-val that)))
         (and (predicate-contract? that)
              (predicate-contract-sane? that)
              ((predicate-contract-pred that) this-val))))
   #:equivalent
   (λ (this that)
     (define this-val (equal-contract-val this))
     (and (equal-contract? that)
          (equal? this-val (equal-contract-val that))))
   #:generate
   (λ (ctc) 
     (define v (equal-contract-val ctc))
     (λ (fuel) (λ () v)))))

(define-struct =-contract (val name)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:first-order (λ (ctc) (λ (x) (and (number? x) (= (=-contract-val ctc) x))))
   #:name (λ (ctc) (=-contract-name ctc))
   #:stronger
   (λ (this that)
     (define this-val (=-contract-val this))
     (or (and (=-contract? that)
              (= this-val (=-contract-val that)))
         (and (between/c-s? that)
              (<= (between/c-s-low that) this-val (between/c-s-high that)))
         (and (predicate-contract? that)
              (predicate-contract-sane? that)
              ((predicate-contract-pred that) this-val))))
   #:equivalent
   (λ (this that)
     (define this-val (=-contract-val this))
     (or (and (=-contract? that)
              (= this-val (=-contract-val that)))
         (and (between/c-s? that)
              (= (between/c-s-low that) this-val (between/c-s-high that)))))
   #:generate
   (λ (ctc) 
     (define v (=-contract-val ctc))
     (λ (fuel)
       (cond
         [(zero? v)
          ;; zero has a whole bunch of different numbers that
          ;; it could be, so just pick one of them at random
          (λ ()
            (oneof '(0
                     -0.0 0.0 0.0f0 -0.0f0
                     0.0+0.0i 0.0f0+0.0f0i 0+0.0i 0.0+0i)))]
         [else
          (λ ()
            (case (random 10)
              [(0)
               (define inf/nan '(+inf.0 -inf.0 +inf.f -inf.f +nan.0 +nan.f))
               ;; try the inexact/exact variant (if there is one)
               (cond
                 [(exact? v)
                  (define iv (exact->inexact v))
                  (if (= iv v) iv v)]
                 [(and (inexact? v) (not (memv v inf/nan)))
                  (define ev (inexact->exact v))
                  (if (= ev v) ev v)]
                 [else v])]
              [(1)
               ;; try to add an inexact imaginary part
               (define c (+ v 0+0.0i))
               (if (= c v) c v)]
              [else
               ;; otherwise, just stick with the original number (80% of the time)
               v]))])))))

(define-struct char-in/c (low high)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:first-order
   (λ (ctc)
     (define low (char-in/c-low ctc))
     (define high (char-in/c-high ctc))
     (λ (x)
         (and (char? x)
              (char<=? low x high))))
   #:name (λ (ctc)
            (define low (char-in/c-low ctc))
            (define high (char-in/c-high ctc))
            (if (equal? low high)
                low
                `(char-in ,low ,high)))
   #:stronger
   (λ (this that)
     (cond
       [(char-in/c? that)
        (define this-low (char-in/c-low this))
        (define this-high (char-in/c-high this))
        (define that-low (char-in/c-low that))
        (define that-high (char-in/c-high that))
        (and (char<=? that-low this-low)
             (char<=? this-high that-high))]
       [else #f]))
   #:equivalent
   (λ (this that)
     (cond
       [(char-in/c? that)
        (define this-low (char-in/c-low this))
        (define this-high (char-in/c-high this))
        (define that-low (char-in/c-low that))
        (define that-high (char-in/c-high that))
        (and (char=? that-low this-low)
             (char=? this-high that-high))]
       [else #f]))
   #:generate
   (λ (ctc)
     (define low (char->integer (char-in/c-low ctc)))
     (define high (char->integer (char-in/c-high ctc)))
     (define delta (+ (- high low) 1))
     (λ (fuel)
       (λ ()
         (integer->char (+ low (random delta))))))))

(define (regexp/c-equivalent this that)
  (and (regexp/c? that)
       (equal? (regexp/c-reg this) (regexp/c-reg that))))

(define-struct regexp/c (reg name)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:first-order
   (λ (ctc)
     (define reg (regexp/c-reg ctc))
      (λ (x)
         (and (or (string? x) (bytes? x))
              (regexp-match? reg x))))
   #:name (λ (ctc) (regexp/c-reg ctc))
   #:stronger regexp/c-equivalent
   #:equivalent regexp/c-equivalent))

(define (predicate-contract-equivalent this that)
  (and (predicate-contract? that)
       (procedure-closure-contents-eq? (predicate-contract-pred this)
                                       (predicate-contract-pred that))))

;; sane? : boolean -- indicates if we know that the predicate is well behaved
;; (for now, basically amounts to trusting primitive procedures)
(define-struct predicate-contract (name pred generate sane?)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:stronger predicate-contract-equivalent
   #:equivalent predicate-contract-equivalent
   #:name (λ (ctc) (predicate-contract-name ctc))
   #:first-order (λ (ctc) (predicate-contract-pred ctc))
   #:late-neg-projection
   (λ (ctc)
     (define p? (predicate-contract-pred ctc))
     (define name (predicate-contract-name ctc))
     (λ (blame)
       (procedure-specialize
        (λ (v neg-party)
          (if (p? v)
              v
              (raise-predicate-blame-error-failure blame v neg-party name))))))
   #:generate (λ (ctc)
                 (let ([generate (predicate-contract-generate ctc)])
                   (cond
                     [generate generate]
                     [else
                      (define built-in-generator
                        (find-generate (predicate-contract-pred ctc)
                                       (predicate-contract-name ctc)))
                      (λ (fuel)
                        (and built-in-generator
                             (λ () (built-in-generator fuel))))])))
   #:list-contract? (λ (ctc) (or (equal? (predicate-contract-pred ctc) null?)
                                 (equal? (predicate-contract-pred ctc) empty?)))))

(define (raise-predicate-blame-error-failure blame v neg-party predicate-name)
  (raise-blame-error blame v #:missing-party neg-party
                     '(expected: "~s" given: "~e")
                     predicate-name
                     v))

(define (check-flat-named-contract predicate) (coerce-flat-contract 'flat-named-contract predicate))
(define (check-flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (build-flat-contract name pred [generate #f])
  (make-predicate-contract name pred generate #f))

(define boolean?/c (make-predicate-contract 'boolean? boolean? #f #t))

(define (contract-name ctc)
  (contract-struct-name
   (coerce-contract 'contract-name ctc)))

(define (contract-projection ctc)
  (get/build-projection
   (coerce-contract 'contract-projection ctc)))
(define (contract-val-first-projection ctc)
  (get/build-val-first-projection
   (coerce-contract 'contract-projection ctc)))
(define (contract-late-neg-projection ctc)
  (get/build-late-neg-projection
   (coerce-contract 'contract-projection ctc)))

(define-logger racket/contract)

(define (get/build-collapsible-late-neg-projection ctc)
  (cond
    [(contract-struct-collapsible-late-neg-projection ctc) => values]
    [else
     (define lnp (get/build-late-neg-projection ctc))
     (λ (blame)
       (define proj (lnp blame))
       (values proj
               (build-collapsible-leaf proj ctc blame)))]))

(define (get/build-late-neg-projection ctc)
  (cond
    [(contract-struct-late-neg-projection ctc) => values]
    [else
     (log-racket/contract-info "no late-neg-projection for ~s" ctc)
     (cond
       [(contract-struct-collapsible-late-neg-projection ctc) =>
        (lambda (f)
          (lambda (blame)
            (define-values (proj _) (f blame))
            proj))]
       [(contract-struct-projection ctc)
        =>
        (λ (projection)
          (projection->late-neg-projection projection))]
       [(contract-struct-val-first-projection ctc)
        =>
        (λ (val-first-projection)
          (val-first-projection->late-neg-projection val-first-projection))]
       [else
        (first-order->late-neg-projection (contract-struct-first-order ctc)
                                          (contract-struct-name ctc))])]))
     
(define (projection->late-neg-projection proj)
  (λ (b)
    (λ (x neg-party)
      ((proj (blame-add-missing-party b neg-party)) x))))
(define (val-first-projection->late-neg-projection vf-proj)
  (λ (b)
    (define vf-val-accepter (vf-proj b))
    (λ (x neg-party)
      ((vf-val-accepter x) neg-party))))
(define (first-order->late-neg-projection p? name)
  (λ (b)
    (λ (x neg-party)
      (if (p? x)
          x
          (raise-blame-error
           b x #:missing-party neg-party
           '(expected: "~a" given: "~e")
           name
           x)))))

(define warn-about-val-first? (make-parameter #t))
(define (maybe-warn-about-val-first ctc)
  (when (warn-about-val-first?)
    (log-racket/contract-info
     "building val-first-projection of contract ~s for~a"
     ctc
     (build-context))))

(define (get/build-val-first-projection ctc)
  (cond
    [(contract-struct-val-first-projection ctc) => values]
    [else
     (maybe-warn-about-val-first ctc)
     (late-neg-projection->val-first-projection
      (get/build-late-neg-projection ctc))]))
(define (late-neg-projection->val-first-projection lnp)
  (λ (b)
    (define val+neg-party-accepter (lnp b))
    (λ (x)
      (λ (neg-party)
        (val+neg-party-accepter x neg-party)))))

(define (get/build-projection ctc)
  (cond
    [(contract-struct-projection ctc) => values]
    [else
     (log-racket/contract-warning
      "building projection of contract ~s for~a"
      ctc
      (build-context))
     (late-neg-projection->projection
      (get/build-late-neg-projection ctc))]))
(define (late-neg-projection->projection lnp)
  (λ (b)
    (define val+np-acceptor (lnp b))
    (λ (x)
      (val+np-acceptor x #f))))


(define contract-first-order-okay-to-give-up-key (gensym 'contract-first-order-okay-to-give-up-key))
(define (contract-first-order-okay-to-give-up?)
  (zero? (continuation-mark-set-first #f
                                      contract-first-order-okay-to-give-up-key
                                      1)))
(define-syntax-rule
  (contract-first-order-try-less-hard e)
  (contract-first-order-try-less-hard/proc (λ () e)))
(define (contract-first-order-try-less-hard/proc th)
  (define cv (continuation-mark-set-first #f contract-first-order-okay-to-give-up-key))
  (if cv
      (with-continuation-mark contract-first-order-okay-to-give-up-key (if (= cv 0) 0 (- cv 1))
        (th))
      (th)))
(define-syntax-rule
  (contract-first-order-only-try-so-hard n e)
  (with-continuation-mark contract-first-order-okay-to-give-up-key n e))

;; Key used by the continuation mark that holds blame information for the current contract.
;; That information is consumed by the contract profiler.
(define contract-continuation-mark-key
  (make-continuation-mark-key 'contract))

;; Instrumentation strategy:
;; - add instrumentation at entry points to the contract system:
;;   - `contract` (`apply-contract`, really)
;;   - `contract-out` (`do-partial-app`, really)
;;   - all others go through one of the above
;;   that instrumentation picks up "top-level" flat contracts (i.e., not part of
;;   some higher-order contract) and the "eager" parts of higher-order contracts
;; - add instrumentation inside chaperones/impersonators created by projections
;;   that instrumentation picks up the deferred work of higher-order contracts
;; - add instrumentation to `plus-one-arity-functions`
;;   those perform checking, but don't rely on chaperones
;;   they exist for -> and ->*, and are partially implemented for ->i
;;   TODO once they're fully implemented for ->i, will need to instrument them
(define-syntax-rule (with-contract-continuation-mark payload code ...)
  (begin
    ;; ;; When debugging a missing blame party error, turn this on, then run
    ;; ;; the contract test suite. It should find the problematic combinator.
    ;; (unless (or (pair? payload) (not (blame-missing-party? payload)))
    ;;   (error "internal error: missing blame party" payload))
    (with-continuation-mark contract-continuation-mark-key payload
                            (let () code ...))))

(define collapsible-contract-continuation-mark-key
  (make-continuation-mark-key 'collapsible-contract))

(define-syntax-rule (with-collapsible-contract-continuation-mark code ...)
  (with-continuation-mark collapsible-contract-continuation-mark-key #t
    (let () code ...)))
  
(define (n->th n)
  (string-append 
   (number->string n)
   (case (modulo n 10)
     [(1) "st"]
     [(2) "nd"]
     [(3) "rd"]
     [else "th"])))

(define (nth-element-of/alloc n)
  (format "the ~a element of" (n->th n)))
(define (nth-argument-of/alloc n)
  (format "the ~a argument of" (n->th n)))
(define (nth-case-of/alloc n)
  (format "the ~a case of" (n->th n)))

(define-syntax (define-precompute/simple stx)
  (syntax-case stx ()
    [(_ fn fn/alloc lower-bound-stx upper-bound-stx)
     (let ()
       (define lower-bound (syntax-e #'lower-bound-stx))
       (define upper-bound (syntax-e #'upper-bound-stx))
       (define (n->id n)
         (string->symbol (format "precomputed-~a" n)))
     #`(begin
         #,@(for/list ([i (in-range lower-bound (+ upper-bound 1))])
              #`(define #,(n->id i) (fn/alloc #,i)))
         (define (fn n)
           (case n
             #,@(for/list ([i (in-range lower-bound (+ upper-bound 1))])
                  #`[(#,i) #,(n->id i)])
             [else (fn/alloc n)]))))]))

(define-precompute/simple nth-element-of nth-element-of/alloc 0 10)
(define-precompute/simple nth-argument-of nth-argument-of/alloc 1 7)
(define-precompute/simple nth-case-of nth-case-of/alloc 1 2)

(define-syntax-rule
  (contract-pos/neg-doubling e1 e2)
  (contract-pos/neg-doubling/proc (λ () e1) (λ () e2)))
(define-syntax-rule
  (contract-pos/neg-doubling.2 e1 e2)
  (contract-pos/neg-doubling.2/proc (λ () e1) (λ () e2)))
(define doubling-cm-key (gensym 'racket/contract-doubling-mark))
(define (contract-pos/neg-doubling/proc t1 t2)
  (define depth
    (or (continuation-mark-set-first (current-continuation-marks)
                                     doubling-cm-key)
        0))
  (cond
    [(> depth 5)
     (values #f t1 t2)]
    [else
     (with-continuation-mark doubling-cm-key (+ depth 1)
       (values #t (t1) (t2)))]))
(define (contract-pos/neg-doubling.2/proc t1 t2)
  (define depth
    (or (continuation-mark-set-first (current-continuation-marks)
                                     doubling-cm-key)
        0))
  (cond
    [(> depth 5)
     (values #f t1 #f t2 #f)]
    [else
     (with-continuation-mark doubling-cm-key (+ depth 1)
       (let ()
         (define-values (t11 t12) (t1))
         (define-values (t21 t22) (t2))
         (values #t t11 t12 t21 t22)))]))
