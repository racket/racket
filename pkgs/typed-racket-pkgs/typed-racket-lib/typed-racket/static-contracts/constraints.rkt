#lang racket/base

;; Manages the restrictions on what kind of contract is viable.
;; Some combinators cannot support arbitrary contracts. Ex: hash/c needs a flat contract on the key.
;; This module provides the functions for manipulating a set of such constraints.
;;
;; Constructors:
;;   simple-contract-restrict: kind? -> contract-restrict?
;;     This means that the generated contract will be contract of the supplied kind.
;;
;;   variable-contract-restrict: identifier? -> contract-restrict?
;;     This means that the generated contract will be of the same kind as the recursive contract
;;     referenced by the variable.
;;
;;   merge-restricts: kind? contract-restrict? ... -> contract-restrict?
;;   merge-restricts*: kind? (listof contracct-restrict?) -> contract-restrict?
;;     This means that the generated contract will be the max of kind and all of the other contract
;;     restricts.
;;
;;   add-constraint: contract-restrict? kind? -> contract-restrict
;;     This means the kind of the generated contract can not be greater than the supplied kind.
;;
;;   close-loop: (lisotf identifier?) (listof contract-restrict?) contract-restrict? -> contract-restrict?
;;     This takes a bunch of recursive contract identifiers, their corresponding contract-restricts,
;;     the contract restrict for a body and constructs the appropriate constract restrict.
;;
;; Other:
;;   validate-constraints: contract-restrict? -> void?
;;     This takes a contract-restrict and raises an exception if it has any violated constraints.
;;
;;   contract-restrict-recursive-values: contract-restrict? -> (dict/c identifier? kind?)
;;     Provides the kinds of all of the internal recursive contracts that are a part of the
;;     contract-restrict.
;;
;;
;;

(require
  racket/match
  racket/list
  racket/format
  racket/function
  racket/contract
  racket/dict
  racket/set
  syntax/id-table
  "kinds.rkt"
  "equations.rkt")

(provide
  simple-contract-restrict
  variable-contract-restrict
  merge-restricts*
  merge-restricts
  close-loop
  (contract-out
    [exn:fail:constraint-failure? predicate/c]
    [exn:fail:constraint-failure-reason (exn:fail:constraint-failure? . -> . string?)]
    [validate-constraints (contract-restrict? . -> . void?)]
    [add-constraint (contract-restrict? contract-kind? . -> . contract-restrict?)])
  contract-restrict-recursive-values

  contract-restrict?
  )

(module structs racket/base
  (require racket/contract
           racket/match
           racket/dict
           racket/list
           racket/set
           syntax/id-table
           "kinds.rkt")
  (provide
    (contract-out
      ;; constraint: value must be below max
      [struct constraint ([value kind-max?] [max contract-kind?])]
      ;; kind-max: represents the maximum kind across all of the variables and the specified kind
      [struct kind-max ([variables free-id-set?] [max contract-kind?])]
      ;; contract-restrict: represents a contract with value, recursive-values maps mentioned
      ;; recursive parts to kind-maxes, constraints are constraints that need to hold
      [struct contract-restrict ([value kind-max?]
                                 [recursive-values free-id-table?]
                                 [constraints (set/c constraint?)])]))
  (define free-id-set? free-id-table?)

  (struct constraint (value max) #:transparent)
  (struct kind-max (variables max) #:transparent
          #:methods gen:custom-write
          [(define (write-proc v port mode)
             (match-define (kind-max variables max) v)
             (define recur
               (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))]))
             (define-values (open close)
               (if (equal? mode 0)
                   (values "(" ")")
                   (values "#<" ">")))
             (display open port)
             (fprintf port "kind-max")
             (display " " port)
             (display (map syntax-e (dict-keys variables)) port)
             (display " " port)
             (recur max port)
             (display close port))])
  (struct contract-restrict (value recursive-values constraints)
          #:methods gen:custom-write
          [(define (write-proc v port mode)
             (match-define (contract-restrict value recursive-values constraints) v)
             (define recur
               (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))]))
             (define-values (open close)
               (if (equal? mode 0)
                   (values "(" ")")
                   (values "#<" ">")))
             (display open port)
             (fprintf port "contract-restrict")
             (display " " port)
             (recur value port)

             (display " (" port)
             (define (recur-pair name val)
               (fprintf port "(~a " (syntax->datum name))
               (recur val port)
               (display ")" port))
             (define-values (names vals)
                (let ((assoc (dict->list recursive-values)))
                  (values (map car assoc) (map cdr assoc))))
             (when (cons? names)
               (recur-pair (first names) (first vals))
               (for ((name (rest names))
                     (val (rest vals)))
                    (display " " port)
                    (recur-pair name val)))
             (display ") " port)
             (recur constraints port)
             (display close port))]
          #:transparent))
(require 'structs)
(provide (struct-out kind-max))

(struct exn:fail:constraint-failure exn:fail (reason))

(define (free-id-set . elems)
  (for/fold ([table (make-immutable-free-id-table)])
            ([e (in-list elems)])
    (dict-set table e #t)))

(define (free-id-set-union tables)
  (for*/fold ([table (make-immutable-free-id-table)])
             ([new-table (in-list tables)]
              [(k _) (in-dict new-table)])
    (dict-set table k #t)))

(define (free-id-table-union tables)
  (for*/fold ([table (make-immutable-free-id-table)])
             ([new-table (in-list tables)]
              [(k v) (in-dict new-table)])
    (dict-set table k v)))

(define (simple-contract-restrict kind)
  (contract-restrict (kind-max (free-id-set) kind) (make-immutable-free-id-table) (set)))
(define (variable-contract-restrict var)
  (contract-restrict (kind-max (free-id-set var) 'flat) (make-immutable-free-id-table) (set)))

(define (reason-string actual bound)
  (define (name k)
    (case k
      [(flat chaperone) (~a "a " k " contract")]
      [(impersonator) "an impersonator contract"]))
  (~a "required " (name bound) " but generated " (name actual)))


(define (trivial-constraint? con)
  (match con
    [(constraint _ 'impersonator)
     #t]
    [(constraint (kind-max (app dict-count 0) actual) bound)
     (contract-kind<= actual bound)]
    [else #f]))


(define (add-constraint cr max)
  (match cr
    [(contract-restrict v rec constraints)
     (define con (constraint v max))
     (if (trivial-constraint? con)
         cr
         (contract-restrict v rec (set-add constraints con)))]))

(define (add-recursive-values cr dict) 
  (match cr
    [(contract-restrict v rec constraints)
     (contract-restrict v (free-id-table-union (list rec dict)) constraints)]))

(define (merge-restricts* min crs)
  (apply merge-restricts min crs))

(define (merge-restricts min . crs)
  (match crs
    [(list (contract-restrict vs rec constraints) ...)
     (contract-restrict (merge-kind-maxes min vs)
                        (free-id-table-union rec)
                        (apply set-union (set) constraints))]))

(define (merge-kind-maxes min-kind vs)
  (match vs
    [(list (kind-max variables maxes) ...)
     (kind-max (free-id-set-union variables) (apply combine-kinds min-kind maxes))]))

(define (close-loop names crs body)
  (define eqs (make-equation-set))
  (define vars
    (for*/hash ((name (in-list names)))
      (values name 
              (add-variable! eqs (simple-contract-restrict 'flat)))))
  (define (variable-lookup name)
    (variable-ref (hash-ref vars name)))


  (define (instantiate-cr cr lookup-id)
    (define (instantiate-kind-max km)
      (match km
        [(kind-max ids actual)
         (define-values (bound-ids unbound-ids)
           (partition (lambda (id) (member id names)) (dict-keys ids)))
         (merge-kind-maxes 'flat (cons (kind-max (apply free-id-set unbound-ids) actual)
                                       (for/list ([id (in-list bound-ids)])
                                         (contract-restrict-value (lookup-id id)))))]))

    (define (instantiate-constraint con)
      (match con
        [(constraint km bound)
         (constraint (instantiate-kind-max km) bound)]))

    (match cr
      [(contract-restrict (kind-max ids max) rec constraints)
       (define-values (bound-ids unbound-ids)
         (partition (lambda (id) (member id names)) (dict-keys ids)))
       (merge-restricts* 'flat (cons
                                 (contract-restrict
                                   (kind-max (apply free-id-set unbound-ids) max) 
                                   rec
                                   (apply set
                                          (filter (negate trivial-constraint?)
                                                  (set-map constraints instantiate-constraint))))
                                 (map lookup-id bound-ids)))]))

  (for ([name names] [cr crs])
    (add-equation! eqs
      (hash-ref vars name)
      (lambda ()
        (instantiate-cr cr variable-lookup))))

  (define var-values (resolve-equations eqs))
  (define id-values
    (for/hash (((name var) vars))
      (values name (hash-ref var-values var))))

  (define new-rec-values
    (for/hash (((name value) id-values))
      (values name (contract-restrict-value value))))

  (for/fold ([cr (instantiate-cr body (lambda (id) (hash-ref id-values id)))])
            ([rec-values (cons new-rec-values (map contract-restrict-recursive-values
                                                   (hash-values id-values)))])
    (add-recursive-values cr rec-values)))



(define (validate-constraints cr)
  (match cr
    [(contract-restrict (kind-max (app dict-count 0) _) rec constraints)
     (for ([const (in-set constraints)])
       (match const
        [(constraint (kind-max (app dict-count 0) kind) bound)
         (unless (contract-kind<= kind bound)
           (define reason (reason-string kind bound))
           (raise (exn:fail:constraint-failure
                    (format "Violated constraint: ~a" reason)
                    (current-continuation-marks)
                    reason)))]))]))

