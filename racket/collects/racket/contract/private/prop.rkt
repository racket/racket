#lang racket/base

(require "blame.rkt"
         "generate-base.rkt"
         racket/private/performance-hint)

(provide prop:contract
         contract-struct?
         contract-struct-name
         contract-struct-first-order
         contract-struct-projection
         contract-struct-val-first-projection
         contract-struct-late-neg-projection
         contract-struct-stronger?
         contract-struct-generate
         contract-struct-exercise
         contract-struct-list-contract?
         
         prop:flat-contract
         flat-contract-struct?
         
         prop:chaperone-contract
         chaperone-contract-struct?

         contract-property?
         build-contract-property
         
         chaperone-contract-property?
         build-chaperone-contract-property

         flat-contract-property?
         build-flat-contract-property

         make-contract
         make-chaperone-contract
         make-flat-contract
         
         skip-projection-wrapper?
         
         prop:opt-chaperone-contract
         prop:opt-chaperone-contract?
         prop:opt-chaperone-contract-get-test
         
         prop:orc-contract
         prop:orc-contract?
         prop:orc-contract-get-subcontracts
         
         prop:recursive-contract
         prop:recursive-contract?
         prop:recursive-contract-unroll
         
         prop:arrow-contract 
         prop:arrow-contract?
         prop:arrow-contract-get-info
         (struct-out arrow-contract-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct contract-property [ name 
                                   first-order
                                   projection
                                   stronger
                                   generate
                                   exercise
                                   val-first-projection
                                   late-neg-projection
                                   list-contract? ]
  #:omit-define-syntaxes)

(define (contract-property-guard prop info)
  (unless (contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a contract property; got: ~e"
              'prop:contract
              prop)
      (current-continuation-marks))))
  prop)

(define-values [ prop:contract contract-struct? contract-struct-property ]
  (make-struct-type-property 'prop:contract contract-property-guard))

(define (contract-struct-name c)
  (let* ([prop (contract-struct-property c)]
         [get-name (contract-property-name prop)]
         [name (get-name c)])
    name))

(define (contract-struct-first-order c)
  (let* ([prop (contract-struct-property c)]
         [get-first-order (contract-property-first-order prop)]
         [first-order (get-first-order c)])
    first-order))

(define (contract-struct-projection c)
  (let* ([prop (contract-struct-property c)]
         [get-projection (contract-property-projection prop)]
         [projection (get-projection c)])
    projection))

(define (contract-struct-val-first-projection c)
  (define prop (contract-struct-property c))
  (define get-projection (contract-property-val-first-projection prop))
  (and get-projection 
       (get-projection c)))

(define (contract-struct-late-neg-projection c)
  (define prop (contract-struct-property c))
  (define get-projection (contract-property-late-neg-projection prop))
  (and get-projection 
       (get-projection c)))

(define trail (make-parameter #f))
(define (contract-struct-stronger? a b)
  (define prop (contract-struct-property a))
  (define stronger? (contract-property-stronger prop))
  (cond
    [(let ([th (trail)])
       (and th
            (for/or ([(a2 bs-h) (in-hash th)])
              (and (eq? a a2)
                   (for/or ([(b2 _) (in-hash bs-h)])
                     (eq? b b2))))))
     #t]
    [(or (prop:recursive-contract? a) (prop:recursive-contract? b))
     (parameterize ([trail (or (trail) (make-hasheq))])
       (define trail-h (trail))
       (let ([a-h (hash-ref trail-h a #f)])
         (cond
           [a-h
            (hash-set! a-h b #t)]
           [else
            (define a-h (make-hasheq))
            (hash-set! trail-h a a-h)
            (hash-set! a-h b #t)]))
       (contract-struct-stronger? (if (prop:recursive-contract? a)
                                      ((prop:recursive-contract-unroll a) a)
                                      a)
                                  (if (prop:recursive-contract? b)
                                      ((prop:recursive-contract-unroll b) b)
                                      b)))]
    [else
     (let loop ([b b])
       (cond
         [(stronger? a b) #t]
         [(prop:orc-contract? b)
          (define sub-contracts ((prop:orc-contract-get-subcontracts b) b))
          (for/or ([sub-contract (in-list sub-contracts)])
            (loop sub-contract))]
         [else #f]))]))

(define (contract-struct-generate c)
  (define prop (contract-struct-property c))
  (define generate (contract-property-generate prop))
  (if (procedure? generate)
      (generate c)
      #f))

(define (contract-struct-exercise c)
  (define prop (contract-struct-property c))
  (define exercise (contract-property-exercise prop))
  (if (procedure? exercise)
      (exercise c)
      (λ (fuel) (values void '()))))

(define (contract-struct-list-contract? c)
  (define prop (contract-struct-property c))
  ((contract-property-list-contract? prop) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Chaperone Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct chaperone-contract-property [implementation]
  #:omit-define-syntaxes)

(define (chaperone-contract-property-guard prop info)
  (unless (chaperone-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a chaperone contract property; got: ~e"
              'prop:chaperone-contract
              prop)
      (current-continuation-marks))))
  prop)

;; We check to make sure the contract projection actually resulted in
;; a chaperone (or chaperone-friendly) version of the value.
(define (chaperone-contract-property->contract-property fc)
  (let ([impl (chaperone-contract-property-implementation fc)])
    impl))

(define-values [ prop:chaperone-contract
                 chaperone-contract-struct?
                 chaperone-contract-struct-property ]
  (make-struct-type-property
   'prop:chaperone-contract
   chaperone-contract-property-guard
   (list (cons prop:contract chaperone-contract-property->contract-property))))

;; this property is so the opt'd contracts can
;; declare that they are chaperone'd; the property
;; is a function that extracts a boolean from the 
;; original struct
(define-values (prop:opt-chaperone-contract
                prop:opt-chaperone-contract? 
                prop:opt-chaperone-contract-get-test)
  (make-struct-type-property 'prop:opt-chaperone-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Flat Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct flat-contract-property [implementation]
  #:omit-define-syntaxes)

(define (flat-contract-property-guard prop info)
  (unless (flat-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a flat contract property; got: ~e"
              'prop:flat-contract
              prop)
      (current-continuation-marks))))
  prop)

(define (flat-contract-property->chaperone-contract-property fc)
  (let ([impl (flat-contract-property-implementation fc)])
    (make-chaperone-contract-property impl)))

(define (flat-contract-property->procedure-property prop)
  (let* ([impl (flat-contract-property-implementation prop)]
         [get-predicate (contract-property-first-order impl)])
    (lambda (c x) ((get-predicate c) x))))

(define-values [ prop:flat-contract
                 flat-contract-struct?
                 flat-contract-struct-property ]
  (make-struct-type-property
   'prop:flat-contract
   flat-contract-property-guard
   (list (cons prop:chaperone-contract flat-contract-property->chaperone-contract-property)
         (cons prop:procedure flat-contract-property->procedure-property))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Friendly Property Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define skip-projection-wrapper? (make-parameter #f))

(define ((build-property mk default-name projection-wrapper)
         #:name [get-name #f]
         #:first-order [get-first-order #f]
         #:projection [get-projection #f]
         #:val-first-projection [get-val-first-projection #f]
         #:late-neg-projection [get-late-neg-projection #f]
         #:stronger [stronger #f]
         #:generate [generate (λ (ctc) (λ (fuel) #f))]
         #:exercise [exercise (λ (ctc) (λ (fuel) (values void '())))]
         #:list-contract? [list-contract? (λ (c) #f)])
  
  ;; this code is here to help me find the combinators that
  ;; are still using only #:projection and not #:val-first-projection
  #;
  (when (and get-projection
             (not get-val-first-projection))
    (printf "missing val-first-projection ~s\n" 
            get-projection))
  
  (let* ([get-name (or get-name (lambda (c) default-name))]
         [get-first-order (or get-first-order get-any?)]
         [get-val-first-projection
          (or get-val-first-projection 
              (and (not get-projection)
                   (get-val-first-first-order-projection get-name get-first-order)))]
         [get-projection
          (cond
            [get-projection 
             (blame-context-projection-wrapper
              (if (skip-projection-wrapper?)
                  get-projection
                  (projection-wrapper get-projection)))]
            [else (val-first-projection->projection get-val-first-projection
                                                    get-name
                                                    get-first-order)])]
         [stronger (or stronger weakest)])

    (mk get-name get-first-order
        get-projection stronger 
        generate exercise 
        get-val-first-projection
        get-late-neg-projection
        list-contract?)))

(define build-contract-property
  (procedure-rename
   (build-property make-contract-property 'anonymous-contract values)
   'build-contract-property))

;; Here we'll force the projection to always return the original value,
;; instead of assuming that the provided projection does so appropriately.
(define (flat-projection-wrapper f)
  (λ (c)
    (let ([proj (f c)])
      (λ (b)
        (let ([p (proj b)])
          (λ (v) (p v) v))))))

(define build-flat-contract-property
  (procedure-rename
   (build-property (compose make-flat-contract-property make-contract-property)
                   'anonymous-flat-contract
                   flat-projection-wrapper)
   'build-flat-contract-property))

(define (chaperone-projection-wrapper f)
  (λ (c)
    (let ([proj (f c)])
      (λ (b)
        (let ([p (proj b)])
          (λ (v)
            (let ([v* (p v)])
              (unless (chaperone-of? v* v)
                (error 'prop:chaperone-contract (format "expected a chaperone of ~v, got ~v" v v*)))
              v*)))))))

(define (blame-context-projection-wrapper proj)
  (λ (ctc)
    (define c-proj (proj ctc))
    (λ (blame)
      (c-proj (blame-add-unknown-context blame)))))

(define build-chaperone-contract-property
  (procedure-rename
   (build-property (compose make-chaperone-contract-property make-contract-property)
                   'anonymous-chaperone-contract
                   chaperone-projection-wrapper)
   'build-chaperone-contract-property))

(define (get-any? c) any?)
(define (any? x) #t)

(define (weakest a b) #f)

(define ((val-first-projection->projection get-val-first-projection
                                           get-name
                                           get-first-order) c)
  (cond
    [(flat-contract-struct? c)
     (first-order-projection (get-name c) (get-first-order c))]
    [else
     (define vfp (get-val-first-projection c))
     (λ (blame)
       (define vp (vfp blame))
       (λ (val)
         ((vp val) #f)))]))

(begin-encourage-inline
  (define (first-order-projection name first-order)
    (λ (b)
      (λ (x)
        (if (first-order x)
            x
            (raise-blame-error b x
                               '(expected: "~s" given: "~e")
                               name 
                               x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Contract Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct make-contract [ name first-order projection
                                    val-first-projection late-neg-projection 
                                    stronger generate exercise list-contract? ]
  #:omit-define-syntaxes
  #:property prop:custom-write
  (λ (stct port display?)
    (write-string "#<contract: " port)
    (write-string (format "~.s" (make-contract-name stct)) port)
    (write-string ">" port))
  #:property prop:contract
  (build-contract-property
   #:name (lambda (c) (make-contract-name c))
   #:first-order (lambda (c) (make-contract-first-order c))
   #:projection (lambda (c) (make-contract-projection c))
   #:val-first-projection (lambda (c) (make-contract-val-first-projection c))
   #:late-neg-projection (lambda (c) (make-contract-late-neg-projection c))
   #:stronger (lambda (a b) ((make-contract-stronger a) a b))
   #:generate (lambda (c) (make-contract-generate c))
   #:exercise (lambda (c) (make-contract-exercise c))
   #:list-contract? (λ (c) (make-contract-list-contract? c))))

(define-struct make-chaperone-contract [ name first-order projection
                                              val-first-projection late-neg-projection
                                              stronger generate exercise list-contract? ]
  #:omit-define-syntaxes
  #:property prop:custom-write
  (λ (stct port display?)
    (write-string "#<chaperone-contract: " port)
    (write-string (format "~.s" (make-chaperone-contract-name stct)) port)
    (write-string ">" port))
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (lambda (c) (make-chaperone-contract-name c))
   #:first-order (lambda (c) (make-chaperone-contract-first-order c))
   #:projection (lambda (c) (make-chaperone-contract-projection c))
   #:val-first-projection (lambda (c) (make-chaperone-contract-val-first-projection c))
   #:late-neg-projection (lambda (c) (make-chaperone-contract-late-neg-projection c))
   #:stronger (lambda (a b) ((make-chaperone-contract-stronger a) a b))
   #:generate (lambda (c) (make-chaperone-contract-generate c))
   #:exercise (lambda (c) (make-chaperone-contract-exercise c))
   #:list-contract? (λ (c) (make-chaperone-contract-list-contract? c))))

(define-struct make-flat-contract [ name first-order projection
                                         val-first-projection late-neg-projection
                                         stronger generate exercise list-contract? ]
  #:omit-define-syntaxes
  #:property prop:custom-write
  (λ (stct port display?)
    (write-string "#<flat-contract: " port)
    (write-string (format "~.s" (make-flat-contract-name stct)) port)
    (write-string ">" port))
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (lambda (c) (make-flat-contract-name c))
   #:first-order (lambda (c) (make-flat-contract-first-order c))
   #:val-first-projection (λ (c) (make-flat-contract-val-first-projection c))
   #:late-neg-projection (λ (c) (make-flat-contract-late-neg-projection c))
   #:projection (lambda (c) (make-flat-contract-projection c))
   #:stronger (lambda (a b) ((make-flat-contract-stronger a) a b))
   #:generate (lambda (c) (make-flat-contract-generate c))
   #:exercise (lambda (c) (make-flat-contract-exercise c))
   #:list-contract? (λ (c) (make-flat-contract-list-contract? c))))

(define ((build-contract mk default-name)
         #:name [name #f]
         #:first-order [first-order #f]
         #:projection [projection #f]
         #:val-first-projection [val-first-projection #f]
         #:late-neg-projection [late-neg-projection #f]
         #:stronger [stronger #f]
         #:generate [generate (λ (ctc) (λ (fuel) #f))]
         #:exercise [exercise (λ (ctc) (λ (fuel) (values void '())))]
         #:list-contract? [list-contract? (λ (ctc) #f)])

  (let* ([name (or name default-name)]
         [first-order (or first-order any?)]
         [projection (or projection (first-order-projection name first-order))]
         [val-first-projection (or val-first-projection 
                                   (and (not projection)
                                        (val-first-first-order-projection name first-order)))]
         [stronger (or stronger as-strong?)])

    (mk name first-order 
        projection val-first-projection late-neg-projection
        stronger 
        generate exercise
        list-contract?)))

(define ((get-val-first-first-order-projection get-name get-first-order) c)
  (val-first-first-order-projection (get-name c) (get-first-order c)))

(define (val-first-first-order-projection name p?)
  (λ (b) 
    (λ (v) 
      (λ (neg-party) 
        (if (p? v)
            v
            (raise-blame-error 
             b #:missing-party neg-party
             v
             '(expected: "~s" given: "~e")
             name 
             v))))))

(define (as-strong? a b)
  (procedure-closure-contents-eq?
   (contract-struct-projection a)
   (contract-struct-projection b)))

(define make-contract
  (procedure-rename 
   (build-contract make-make-contract 'anonymous-contract)
   'make-contract))

(define make-chaperone-contract
  (procedure-rename
   (build-contract make-make-chaperone-contract 'anonymous-chaperone-contract)
   'make-chaperone-contract))

(define make-flat-contract
  (procedure-rename
   (build-contract make-make-flat-contract 'anonymous-flat-contract)
   'make-flat-contract))

;; property should be bound to a function that accepts the contract and
;; returns a list of contracts that were the original arguments to the or/c
(define-values (prop:orc-contract prop:orc-contract? prop:orc-contract-get-subcontracts)
  (make-struct-type-property 'prop:orc-contract))

;; property should be bound to a function that accepts the contract
;; and returns a new contract, after unrolling one time
(define-values (prop:recursive-contract
                prop:recursive-contract?
                prop:recursive-contract-unroll)
  (make-struct-type-property 'prop:recursive-contract))

;; get-info : (-> ctc arrow-contract-info?)
(define-values (prop:arrow-contract prop:arrow-contract? prop:arrow-contract-get-info)
  (make-struct-type-property 'prop:arrow-contract))

;; chaperone-procedure : 
;;   (-> any/c[val] blame? procedure[suitable for second argument to chaperone-procedure])
;; check-first-order : any/c[val] blame? -> void? 
;;     raises a blame error if val doesn't satisfy the first-order checks for the function
;; accepts-arglist : (-> (listof keyword?)[sorted by keyword<?] exact-nonnegative-integer? boolean?)
(struct arrow-contract-info (chaperone-procedure check-first-order accepts-arglist)
  #:transparent)
