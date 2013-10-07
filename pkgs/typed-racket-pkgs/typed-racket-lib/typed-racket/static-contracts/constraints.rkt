#lang racket/base

(require
  racket/match
  racket/list
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
  add-constraint
  close-loop
  (contract-out
    [exn:fail:constraint-failure? predicate/c]
    [validate-constraints (contract-restrict? . -> . void?)])
  contract-restrict-recursive-values

  contract-restrict?
  )

(module structs racket/base
  (require racket/contract
           racket/set
           syntax/id-table
           "kinds.rkt")
  (provide
    (contract-out
      [struct constraint ([value kind-max?] [max contract-kind?])]
      [struct kind-max ([variables free-id-table?] [max contract-kind?])]
      [struct contract-restrict ([value kind-max?]
                                 [recursive-values free-id-table?]
                                 [constraints (set/c constraint?)])]))

  (struct constraint (value max) #:transparent)
  (struct kind-max (variables max) #:transparent)
  (struct contract-restrict (value recursive-values constraints) #:transparent))
(require 'structs)
(provide (struct-out kind-max))

(struct exn:fail:constraint-failure exn:fail ())

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


(define (add-constraint cr max) 
  (if (equal? 'impersonator max)
      cr
      (match cr
        [(contract-restrict v rec constraints)
         (contract-restrict v rec (set-add constraints (constraint v max)))])))

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
    (match cr
      [(contract-restrict (kind-max ids max) rec constraints)
       (define-values (bound-ids unbound-ids)
         (partition (lambda (id) (member id names)) (dict-keys ids)))
       (merge-restricts* 'flat (cons
                                 (contract-restrict
                                   (kind-max (apply free-id-set unbound-ids) max) 
                                   rec
                                   constraints)
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
           (raise (exn:fail:constraint-failure "Violated constraint ~a" (current-continuation-marks))))]))]))

