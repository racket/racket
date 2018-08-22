#lang racket/base
(require racket/list
         racket/port
         "match.rkt"
         "out.rkt"
         "prune.rkt"
         "unique.rkt"
         "sort.rkt"
         "id.rkt"
         "vehicle.rkt"
         "top-name.rkt"
         "prim-name.rkt"
         "ref.rkt"
         "function.rkt"
         "state.rkt"
         "generate.rkt"
         "lambda.rkt"
         "struct.rkt"
         "union.rkt"
         "debug.rkt")

(provide (rename-out [main-cify cify])
         re-unique)

(define (main-cify out-file exports in-e prim-knowns
                   #:debug? [debug? #f]
                   #:preamble [preamble '()]
                   #:postamble [postamble '()])
  (current-debug debug?)
  (call-with-output-file*
   out-file
   #:exists 'truncate/replace
   (lambda (out)
     (parameterize ([current-c-output-port out])
       (for-each out-exact preamble)
       (to-c exports in-e prim-knowns)
       (for-each out-exact postamble)))))

;; ----------------------------------------

(define (to-c exports in-e prim-knowns)
  (generate-header)

  ;; Inlining may have made some definitions useless:
  (define pruned-e (prune-unused in-e exports))

  ;; Make sure all names are unique:
  (define unique-e (re-unique pruned-e))

  ;; Find all `define`d names and `let[rec[*]]` names that are
  ;; flattend into the top sequence:
  (define top-names (extract-top-names #hasheq() unique-e))

  ;; Find all the primitives that we'll need to call:
  (define prim-names (extract-prim-names unique-e top-names))

  ;; Find mutable variables, which will need to be boxed:
  (define state (make-state))
  (extract-state! state unique-e)

  ;; Wrap `ref` around every local-variable reference. Also,
  ;; perform copy propagation:
  (define e (wrap-ref unique-e top-names prim-names state))

  ;; Find all `lambda`s and `case-lambda`s, mapping each to
  ;; a newly synthesized name:
  (define lambdas (make-hasheq))
  (extract-lambdas! lambdas e)

  (define struct-knowns (extract-structs e))

  ;; Find all functions that do not need to be kept in a closure.
  ;; Top-level functions and functions bound with `letrec` are in this
  ;; category:
  (define functions (extract-functions #hasheq() e lambdas))
  (for ([(id f) (in-sorted-hash functions symbol<?)])
    (define e (function-e f))
    (hash-set! lambdas e (make-lam id e)))

  (define knowns (hash-directed-union struct-knowns functions))

  ;; Generate top-level sequence just to set free-variable lists and
  ;; other state for each lambda:
  (define max-top-runstack-depth
    (parameterize ([current-c-output-port (open-output-nowhere)])
      (define vehicles (for/list ([lam (in-sorted-hash-values lambdas (compare symbol<? lam-id))])
                         (lam-vehicle lam)))
      (define max-top-runstack-depth
        (generate-tops e 0 exports knowns top-names state lambdas prim-names prim-knowns))
      (generate-vehicles vehicles lambdas knowns top-names state prim-names prim-knowns)
      (hash-set! state '#:done? #t)
      (reset-genid-counters! '(c_args))
      max-top-runstack-depth))

  ;; Now we know if a function that isn't already in `functions`
  ;; has zero free variables. If so, effectively lift it to
  ;; allocate the closure once
  (define closed-anonymous-functions
    (for/hash ([lam (in-hash-values lambdas)]
               #:when (and (null? (lam-free-var-refs lam))
                           ;; No need if it's only formed at the top, unless
                           ;; `c_self` isn't available for overflow handling:
                           (or (lam-under-lambda? lam)
                               (not (vehicle-closure? (lam-vehicle lam))))
                           (not (hash-ref functions (lam-id lam) #f))
                           (not (lam-unused? lam))))
      (set-lam-moved-to-top?! lam #t)
      (values (lam-id lam) lam)))

  ;; Generate prim record:
  (generate-struct "c_prims" prim-names)
  (out "static struct c_prims_t c_prims;")

  ;; Generate top-variable record:
  (generate-struct "startup_instance_top" (hash-union (hash-union top-names functions)
                                                      closed-anonymous-functions))
  (out "THREAD_LOCAL_DECL(static struct startup_instance_top_t *c_startup_instance_top);")
  (out "#define c_top c__startup_instance_top")
  
  (define vehicles (merge-vehicles! lambdas state))

  ;; Generate all the lambda bodies:
  (generate-prototypes vehicles)
  (generate-vehicles vehicles lambdas knowns top-names state prim-names prim-knowns)

  ;; Generate top-level sequence, this time to output:
  (hash-set! state '#:tops? #t)
  (generate-tops e max-top-runstack-depth exports knowns top-names state lambdas prim-names prim-knowns)

  (generate-footer))
