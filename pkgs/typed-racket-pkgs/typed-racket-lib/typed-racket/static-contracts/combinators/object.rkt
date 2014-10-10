#lang racket/base

;; Static contracts for class constructs.
;; Currently supports object/c and class/c.

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         racket/contract
         racket/syntax
         (for-template racket/base racket/class)
         (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [struct member-spec ([modifier symbol?] [id symbol?] [sc static-contract?])]
    [object/sc ((listof object-member-spec?) . -> . static-contract?)]
    [class/sc ((listof member-spec?) boolean? . -> . static-contract?)]
    [instanceof/sc (static-contract? . -> . static-contract?)]))



(struct member-spec (modifier id sc) #:transparent)

(define field-modifiers '(field init init-field inherit-field))
(define method-modifiers '(method inherit super inner override augment augride))

(struct object-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "object/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (object-combinator (member-seq-sc-map f (combinator-args v))))
     (define (sc-traverse v f)
       (member-seq-sc-map f (combinator-args v))
       (void))
     (define (sc->contract v f)
       (object/sc->contract v f))
     (define (sc->constraints v f)
       (merge-restricts* 'impersonator (map f (member-seq->list (combinator-args v)))))])

(struct class-combinator combinator (opaque)
  #:transparent
  #:property prop:combinator-name "class/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
         [(class-combinator args opaque)
          (class-combinator (member-seq-sc-map f args) opaque)]))
     (define (sc-traverse v f)
       (match v
         [(class-combinator args opaque)
          (member-seq-sc-map f args)
          (void)]))
     (define (sc->contract v f)
       (class/sc->contract v f))
     (define (sc->constraints v f)
       (merge-restricts* 'impersonator (map f (member-seq->list (combinator-args v)))))])

(struct instanceof-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "instanceof/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
         [(instanceof-combinator (list class))
          (instanceof-combinator (list (f class 'covariant)))]))
     (define (sc-traverse v f)
       (match v
         [(instanceof-combinator (list class))
          (f class 'covariant)
          (void)]))
     (define (sc->contract v f)
       (instance/sc->contract v f))
     (define (sc->constraints v f)
       (match v
         [(instanceof-combinator (list class))
          (f class)]))])


(define member-seq->list
  (match-lambda
    [(member-seq vals) 
     (filter-map member-spec-sc vals)]))

(struct member-seq (vals)
   #:transparent
   #:property prop:sequence member-seq->list)

(define (member-seq-sc-map f seq)
  (match seq
    [(member-seq vals)
     (member-seq
       (for/list ([v (in-list vals)])
          (match v
            [(member-spec mod id sc)
             (member-spec mod id (and sc (f sc 'invariant)))])))]))

;; TODO make this the correct subset
(define object-member-spec? member-spec?)

(define (object/sc specs)
  (object-combinator (member-seq specs)))
(define (class/sc specs opaque)
  (class-combinator (member-seq specs) opaque))
(define (instanceof/sc class)
  (instanceof-combinator (list class)))

(define ((member-spec->form f) v)
  (match v
    [(member-spec modifier id sc)
     (with-syntax ([ctc-stx (and sc (f sc) empty)]
                   [id-stx id])
       (define id/ctc
         (if sc #`(#,id #,(f sc)) id))
       (match modifier
         ['method id/ctc]
         ['augment #`(augment #,id/ctc)]
         ['init #`(init #,id/ctc)]
         ['field #`(field #,id/ctc)]))]))

(define (spec->id/ctc f modifier vals)
  (for/lists (_1 _2)
             ([spec vals]
              #:when (eq? modifier (member-spec-modifier spec)))
    (values (member-spec-id spec)
            (f (member-spec-sc spec)))))

(define (object/sc->contract v f) 
  (match v
   [(object-combinator (member-seq vals))
    #`(object/c #,@(map (member-spec->form f) vals))]))
(define (class/sc->contract v f) 
  (match v
   [(class-combinator (member-seq vals) opaque)
    (define-values (override-names override-ctcs)
      (spec->id/ctc f 'override vals))
    (define-values (pubment-names pubment-ctcs)
      (spec->id/ctc f 'pubment vals))
    (define/with-syntax (override-temp ...)
      (generate-temporaries override-ctcs))
    (define/with-syntax (pubment-temp ...)
      (generate-temporaries pubment-ctcs))
    (define/with-syntax (override-name ...) override-names)
    (define/with-syntax (pubment-name ...) pubment-names)
    (define/with-syntax (override-ctc ...) override-ctcs)
    (define/with-syntax (pubment-ctc ...) pubment-ctcs)
    (define vals-rest
      (filter (λ (spec)
                (not (memq (member-spec-modifier spec)
                           '(override pubment))))
              vals))
    #`(let ([override-temp override-ctc] ...
            [pubment-temp pubment-ctc] ...)
        (class/c #,@(if opaque (list '#:opaque) empty)
                 #,@(map (member-spec->form f) vals-rest)
                 [override-name override-temp] ...
                 (override [override-name override-temp] ...)
                 (super [override-name override-temp] ...)
                 (inherit [override-name override-temp] ...)
                 [pubment-name pubment-temp] ...
                 (inherit [pubment-name pubment-temp] ...)))]))
(define (instance/sc->contract v f)
  (match v
   [(instanceof-combinator (list class))
    #`(instanceof/c #,(f class))]))
