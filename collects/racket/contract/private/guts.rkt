#lang racket/base

(require "helpers.rkt"
         "blame.rkt"
         "prop.rkt"
         racket/pretty)

(require (for-syntax racket/base
                     "helpers.rkt"))

(provide (except-out (all-from-out "blame.rkt") make-blame)
         (all-from-out "prop.rkt")
         
         coerce-contract
         coerce-contracts
         coerce-flat-contract
         coerce-flat-contracts
         coerce-contract/f
         
         flat-contract?
         flat-contract
         flat-contract-predicate
         flat-named-contract
         build-flat-contract
         
         build-compound-type-name
         
         and/c
         any/c
         none/c
         make-none/c 
         
         contract?
         contract-name
         contract-projection
         
         contract-stronger?

         contract-first-order
         contract-first-order-passes?
         
         prop:contracted
         has-contract?
         value-contract
         
         ;; for opters
         check-flat-contract
         check-flat-named-contract
         any
         
         ;; helpers for adding properties that check syntax uses
         define/final-prop
         define/subexpression-pos-prop)

(define-values (prop:contracted has-contract? value-contract)
  (let-values ([(prop pred get)
                (make-struct-type-property
                 'prop:contracted
                 (lambda (v si)
                   (if (number? v)
                       (let ([ref (cadddr si)])
                         (lambda (s) (ref s v)))
                       (lambda (s) v))))])
    (values prop pred (λ (v) (if (pred v) ((get v) v) #f)))))

(define-syntax (any stx)
  (raise-syntax-error 'any "use of 'any' outside of an arrow contract" stx))

(define (contract-first-order c)
  (contract-struct-first-order
   (coerce-contract 'contract-first-order-passes? c)))

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
  (let loop ([xs xs]
             [i 1])
    (cond
      [(null? xs) '()]
      [else
       (let ([fst (coerce-contract/f (car xs))])
         (unless (flat-contract-struct? fst)
           (error name 
                  "expected all of the arguments to be flat contracts, but argument ~a was not, got ~e" 
                  i
                  (car xs)))
         (cons fst (loop (cdr xs) (+ i 1))))])))

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
  (let loop ([xs xs]
             [i 1])
    (cond
      [(null? xs) '()]
      [(coerce-contract/f (car xs)) => (λ (x) (cons x (loop (cdr xs) (+ i 1))))]
      [else
       (error name 
              "expected all of the arguments to be contracts, but argument ~a was not, got ~e" 
              i
              (car xs))])))

;; coerce-contract/f : any -> (or/c #f contract?)
;; returns #f if the argument could not be coerced to a contract
(define (coerce-contract/f x)
  (cond
    [(contract-struct? x) x]
    [(and (procedure? x) (procedure-arity-includes? x 1)) 
     (make-predicate-contract (or (object-name x) '???) x)]
    [(or (symbol? x) (boolean? x) (char? x) (null? x)) (make-eq-contract x)]
    [(or (bytes? x) (string? x)) (make-equal-contract x)]
    [(number? x) (make-=-contract x)]
    [(or (regexp? x) (byte-regexp? x)) (make-regexp/c x)]
    [else #f]))
       
;                                                      
;                                                      
;                                                      
;                                                      
;                                                      
;                          ;                       ;   
;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                                      
;                                                      
;                                                      

(define (flat-contract-predicate x)
  (contract-struct-first-order
   (coerce-flat-contract 'flat-contract-predicate x)))

(define (flat-contract? x) 
  (let ([c (coerce-contract/f x)])
    (and c
         (flat-contract-struct? c))))

(define (contract-name ctc)
  (contract-struct-name
   (coerce-contract 'contract-name ctc)))

(define (contract? x) (and (coerce-contract/f x) #t))
(define (contract-projection ctc)
  (contract-struct-projection
   (coerce-contract 'contract-projection ctc)))

(define (check-flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (check-flat-named-contract predicate) (coerce-flat-contract 'flat-named-contract predicate))
(define (flat-named-contract name predicate)
  (cond
    [(and (procedure? predicate)
          (procedure-arity-includes? predicate 1))
     (make-predicate-contract name predicate)]
    [(flat-contract? predicate)
     (make-predicate-contract name (flat-contract-predicate predicate))]
    [else
     (error 'flat-named-contract 
            "expected a flat contract or procedure of arity 1 as second argument, got ~e" 
            predicate)]))

;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
(define (build-compound-type-name . fs)
  (let loop ([subs fs])
    (cond
      [(null? subs)
       '()]
      [else (let ([sub (car subs)])
              (cond
                [(contract-struct? sub)
                 (let ([mk-sub-name (contract-name sub)])
                   `(,mk-sub-name ,@(loop (cdr subs))))]
                [else `(,sub ,@(loop (cdr subs)))]))])))

(define (and-proj ctc)
  (let ([mk-pos-projs (map contract-projection (and/c-ctcs ctc))])
    (lambda (blame)
      (let ([projs (map (λ (c) (c blame)) mk-pos-projs)])
        (let loop ([projs (cdr projs)]
                   [proj (car projs)])
          (cond
            [(null? projs) proj]
            [else (loop (cdr projs)
                        (let ([f (car projs)])
                          (λ (v) (proj (f v)))))]))))))


(define-struct and/c (ctcs)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection and-proj
   #:name (λ (ctc) (apply build-compound-type-name 'and/c (and/c-ctcs ctc)))
   #:first-order
   (λ (ctc)
      (let ([tests (map contract-first-order (and/c-ctcs ctc))])
        (λ (x) 
           (andmap (λ (f) (f x)) tests))))
   #:stronger
   (λ (this that)
      (and (and/c? that)
           (let ([this-ctcs (and/c-ctcs this)]
                 [that-ctcs (and/c-ctcs that)])
             (and (= (length this-ctcs) (length that-ctcs))
                  (andmap contract-stronger?
                          this-ctcs
                          that-ctcs)))))))

(define (and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let* ([pred
               (let loop ([pred (flat-contract-predicate (car contracts))]
                          [preds (cdr contracts)])
                 (cond
                   [(null? preds) pred]
                   [else
                    (let* ([fst (flat-contract-predicate (car preds))])
                      (loop (let ([and/c-contract? (lambda (x) (and (pred x) (fst x)))])
                              and/c-contract?)
                            (cdr preds)))]))])
         (flat-named-contract (apply build-compound-type-name 'and/c contracts) pred))]
      [else (make-and/c contracts)])))

(define (get-any-projection c) any-projection)
(define (any-projection b) any-function)
(define (any-function x) x)

(define (get-any? c) any?)
(define (any? x) #t)

(define-struct any/c ()
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection get-any-projection
   #:stronger (λ (this that) (any/c? that))
   #:name (λ (ctc) 'any/c)
   #:first-order get-any?))

(define any/c (make-any/c))

(define (none-curried-proj ctc)
  (λ (blame)
    (λ (val) 
      (raise-blame-error
       blame
       val
       "~s accepts no values, given: ~e"
       (none/c-name ctc)
       val))))

(define-struct none/c (name)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection none-curried-proj
   #:stronger (λ (this that) #t)
   #:name (λ (ctc) (none/c-name ctc))
   #:first-order (λ (ctc) (λ (val) #f))))

(define none/c (make-none/c 'none/c))




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
           (equal? (equal-contract-val this) (equal-contract-val that))))))

(define-struct =-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (and (number? x) (= (=-contract-val ctc) x))))
   #:name (λ (ctc) (=-contract-val ctc))
   #:stronger
   (λ (this that)
      (and (=-contract? that)
           (= (=-contract-val this) (=-contract-val that))))))

(define-struct regexp/c (reg)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order
   (λ (ctc)
      (λ (x)
         (and (or (string? x) (bytes? x))
              (regexp-match (regexp/c-reg ctc) x)
              #t)))
   #:name (λ (ctc) (regexp/c-reg ctc))
   #:stronger
   (λ (this that)
      (and (regexp/c? that) (eq? (regexp/c-reg this) (regexp/c-reg that))))))


(define-struct predicate-contract (name pred)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:stronger
   (λ (this that) 
      (and (predicate-contract? that)
           (procedure-closure-contents-eq? (predicate-contract-pred this)
                                           (predicate-contract-pred that))))
   #:name (λ (ctc) (predicate-contract-name ctc))
   #:first-order (λ (ctc) (predicate-contract-pred ctc))))

(define (build-flat-contract name pred) (make-predicate-contract name pred))

(define-syntax (define/final-prop stx)
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
                   #'x
                   'racket/contract:contract 
                   (vector (gensym 'ctc) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (syntax-property 
                   #'(ctc/proc margs (... ...))
                   'racket/contract:contract 
                   (vector (gensym 'ctc) 
                           (list (car (syntax-e stx)))
                           '()))])))))]))

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
                   #'x
                   'racket/contract:contract 
                   (vector (gensym 'ctc) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (let ([this-one (gensym 'ctc)])
                    (with-syntax ([(margs (... ...)) 
                                   (map (λ (x) (syntax-property x 'racket/contract:positive-position this-one))
                                        (syntax->list #'(margs (... ...))))])
                      (syntax-property 
                       #'(ctc/proc margs (... ...))
                       'racket/contract:contract 
                       (vector this-one 
                               (list (car (syntax-e stx)))
                               '()))))])))))]))
