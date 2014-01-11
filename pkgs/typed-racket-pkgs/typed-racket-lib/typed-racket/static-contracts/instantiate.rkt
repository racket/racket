#lang racket/base

;; Provides functionality to take a static contract and turn it into a regular contract.

(require
  racket/function
  racket/match
  racket/dict
  racket/sequence
  racket/contract
  (for-template racket/base racket/contract)
  "kinds.rkt"
  "parametric-check.rkt"
  "structures.rkt"
  "constraints.rkt"
  "equations.rkt")

(provide
  (contract-out
    [instantiate
      (parametric->/c (a) ((static-contract? (-> #:reason (or/c #f string?) a))
                             (contract-kind?) . ->* . (or/c a syntax?)))]))

;; Providing these so that tests can work directly with them.
(module* internals #f
  (provide compute-constraints
           compute-recursive-kinds
           instantiate/inner))

;; kind is the greatest kind of contract that is supported, if a greater kind would be produced the
;; fail procedure is called.
(define (instantiate sc fail [kind 'impersonator])
  (if (parametric-check sc)
      (fail #:reason "multiple parametric contracts are not supported")
      (with-handlers [(exn:fail:constraint-failure?
                        (lambda (exn) (fail #:reason (exn:fail:constraint-failure-reason exn))))]
        (instantiate/inner sc
          (compute-recursive-kinds
            (contract-restrict-recursive-values (compute-constraints sc kind)))))))

(define (compute-constraints sc max-kind)
  (define (recur sc)
    (match sc
      [(recursive-sc names values body)
       (close-loop names (map recur values) (recur body))]
      [(? sc?)
       (sc->constraints sc recur)]))
  (define constraints (recur sc))
  (validate-constraints (add-constraint constraints max-kind))
  constraints)


(define (compute-recursive-kinds recursives)
  (define eqs (make-equation-set))
  (define vars
    (for/hash ([(name _) (in-dict recursives)])
      (values name (add-variable! eqs 'flat))))

  (define (lookup id)
    (variable-ref (hash-ref vars id)))

  (for ([(name v) (in-dict recursives)])
    (match v
      [(kind-max others max)
       (add-equation! eqs
          (hash-ref vars name)
          (lambda ()
            (apply combine-kinds max (map lookup (dict-keys others)))))]))
  (define var-values (resolve-equations eqs))
  (for/hash (((name var) (in-hash vars)))
    (values name (hash-ref var-values var))))


(define (instantiate/inner sc recursive-kinds)
  (define (recur sc)
    (match sc
      [(recursive-sc names values body)
       (define raw-names (generate-temporaries names))
       (define raw-bindings
          (for/list ([raw-name (in-list raw-names)]
                     [value (in-list values)])
            #`[#,raw-name #,(recur value)]))
       (define bindings
         (for/list ([name (in-list names)]
                    [raw-name (in-list raw-names)])
            #`[#,name (recursive-contract #,raw-name
                                            #,(kind->keyword
                                                (hash-ref recursive-kinds name)))]))
       #`(letrec (#,@bindings #,@raw-bindings) #,(recur body))]
      [(? sc? sc)
       (sc->contract sc recur)]))
  (recur sc))
