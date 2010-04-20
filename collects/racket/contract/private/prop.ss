#lang racket/base

(require "blame.ss")

(provide prop:contract
         contract-struct?
         contract-struct-name
         contract-struct-first-order
         contract-struct-projection
         contract-struct-stronger?

         prop:flat-contract
         flat-contract-struct?

         contract-property?
         build-contract-property

         flat-contract-property?
         build-flat-contract-property

         make-contract
         make-flat-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct contract-property [ name first-order projection stronger generator ]
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

(define (contract-struct-stronger? a b)
  (let* ([prop (contract-struct-property a)]
         [stronger (contract-property-stronger prop)])
    (stronger a b)))

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

(define flat-contract-property->contract-property
  flat-contract-property-implementation)

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
   (list (cons prop:contract flat-contract-property->contract-property)
         (cons prop:procedure flat-contract-property->procedure-property))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Friendly Property Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((build-property mk default-name)
         #:name [get-name #f]
         #:first-order [get-first-order #f]
         #:projection [get-projection #f]
         #:stronger [stronger #f]
         #:generator [generator #f])

  (let* ([get-name (or get-name (lambda (c) default-name))]
         [get-first-order (or get-first-order get-any?)]
         [get-projection (or get-projection
                             (get-first-order-projection
                              get-name get-first-order))]
         [stronger (or stronger weakest)])

    (mk get-name get-first-order get-projection stronger generator)))

(define build-contract-property
  (build-property make-contract-property 'anonymous-contract))

(define build-flat-contract-property
  (build-property (compose make-flat-contract-property make-contract-property)
                  'anonymous-flat-contract))

(define (get-any? c) any?)
(define (any? x) #t)

(define (weakest a b) #f)

(define ((get-first-order-projection get-name get-first-order) c)
  (first-order-projection (get-name c) (get-first-order c)))

(define (((first-order-projection name first-order) b) x)
  (if (first-order x)
    x
    (raise-blame-error b x "expected <~a>, given: ~e" name x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Contract Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct make-contract [ name first-order projection stronger ]
  #:omit-define-syntaxes
  #:property prop:contract
  (make-contract-property
   (lambda (c) (make-contract-name c))
   (lambda (c) (make-contract-first-order c))
   (lambda (c) (make-contract-projection c))
   (lambda (a b) ((make-contract-stronger a) a b))
   #f))

(define-struct make-flat-contract [ name first-order projection stronger ]
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (make-flat-contract-property
   (make-contract-property
    (lambda (c) (make-flat-contract-name c))
    (lambda (c) (make-flat-contract-first-order c))
    (lambda (c) (make-flat-contract-projection c))
    (lambda (a b) ((make-flat-contract-stronger a) a b))
    #f)))

(define ((build-contract mk default-name)
         #:name [name #f]
         #:first-order [first-order #f]
         #:projection [projection #f]
         #:stronger [stronger #f])

  (let* ([name (or name default-name)]
         [first-order (or first-order any?)]
         [projection (or projection (first-order-projection name first-order))]
         [stronger (or stronger as-strong?)])

    (mk name first-order projection stronger)))

(define (as-strong? a b)
  (procedure-closure-contents-eq?
   (contract-struct-projection a)
   (contract-struct-projection b)))

(define make-contract
  (build-contract make-make-contract 'anonymous-contract))

(define make-flat-contract
  (build-contract make-make-flat-contract 'anonymous-flat-contract))
