#lang racket/base

(require
  racket/function
  racket/match
  racket/dict
  racket/sequence
  (for-template racket/base (prefix-in c: racket/contract))
  "kinds.rkt"
  "structures.rkt"
  "constraints.rkt"
  "equations.rkt")
(require (prefix-in c: racket/contract))

(provide
  (c:contract-out
    [instantiate ((static-contract? (c:-> c:none/c)) (contract-kind?) . c:->* . syntax?)]))

(module* internals #f
  (provide compute-constraints
           compute-recursive-kinds
           instantiate/inner))


(define (instantiate sc fail [kind 'impersonator])
  (with-handlers [(exn:fail:constraint-failure? (lambda (exn) (fail)))]
    (instantiate/inner sc
      (compute-recursive-kinds
        (contract-restrict-recursive-values (compute-constraints sc kind))))))

(define (compute-constraints sc max-kind)
  (define (recur sc)
    (match sc
      [(recursive-contract names values body)
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
  (for/hash (((name var) vars))
    (values name (hash-ref var-values var))))

    
(define (instantiate/inner sc recursive-kinds)
  (define (recur sc)
    (match sc
      [(recursive-contract names values body)
       (define raw-names (generate-temporaries names))
       (define raw-bindings
          (for/list ([raw-name raw-names] [value values])
            #`[#,raw-name #,(recur value)]))
       (define bindings
         (for/list ([name names] [raw-name raw-names])
            #`[#,name (c:recursive-contract #,raw-name
                                            #,(kind->keyword
                                                (hash-ref recursive-kinds name)))]))
       #`(letrec (#,@bindings #,@raw-bindings) #,(recur body))]
      [(? sc? sc)
       (sc->contract sc recur)]))
  (recur sc))
