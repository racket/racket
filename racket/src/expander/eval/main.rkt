#lang racket/base
(require "../common/struct-star.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/api.rkt"
         (only-in "../syntax/taint.rkt"
                  [syntax-disarm raw:syntax-disarm]
                  [syntax-rearm raw:syntax-rearm])
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/core.rkt"
         "../common/phase.rkt"
         "../syntax/match.rkt"
         "../expand/context.rkt"
         (rename-in "../expand/main.rkt"
                    [expand expand-in-context])
         "../compile/main.rkt"
         "../compile/compiled-in-memory.rkt"
         "../compile/linklet.rkt"
         "top.rkt"
         "module.rkt"
         "../common/module-path.rkt"
         "../host/linklet.rkt"
         "../syntax/bulk-binding.rkt"
         "../common/contract.rkt"
         "../namespace/api.rkt"
         "../expand/lift-context.rkt"
         "../expand/require.rkt"
         "../expand/require+provide.rkt"
         "reflect.rkt"
         "../expand/log.rkt"
         "../expand/parsed.rkt"
         "../common/performance.rkt"
         "../compile/correlated-linklet.rkt")

(provide eval
         compile
         expand
         expand-once
         expand-to-top-form)

;; This `eval` is suitable as an eval handler that will be called by
;; the `eval` and `eval-syntax` of '#%kernel. 
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (eval s [ns (current-namespace)] [compile (lambda (s ns)
                                                    (compile s ns #f))])
  (cond
   [(or (compiled-in-memory? s)
        (linklet-directory? s)
        (linklet-bundle? s))
    (eval-compiled s ns)]
   [(and (syntax? s)
         (or (compiled-in-memory? (syntax-e s))
             (linklet-directory? (syntax-e s))
             (linklet-bundle? (syntax-e s))))
    (eval-compiled (syntax->datum s) ns)]
   [else
    (per-top-level s ns 
                   #:single (lambda (s ns tail?)
                              (eval-compiled (compile s ns) ns tail?))
                   #:observer #f)]))

(define (eval-compiled c ns [as-tail? #t])
  (cond
   [(compiled-module-expression? c)
    (eval-module c #:namespace ns)]
   [else
    (eval-top c ns eval-compiled as-tail?)]))

;; This `compile` is suitable as a compile handler that will be called
;; by the `compile` and `compile-syntax` of '#%kernel
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (compile s [ns (current-namespace)] [serializable? #t] [expand expand])
  (define to-correlated-linklet? (and serializable?
                                      (not (current-compile-target-machine))))
  ;; The given `s` might be an already-compiled expression because it
  ;; went through some strange path, such as a `load` on a bytecode
  ;; file, which would wrap `#%top-interaction` around the compiled
  ;; expression where the expansion just discards the wrapper
  (define cs
    (cond
     [(compiled-expression? s) (list s)]
     [(and (syntax? s)
           (compiled-expression? (syntax-e s)))
      (list (syntax-e s))]
     [else
      (per-top-level s ns
                     #:single (lambda (s ns as-tail?)
                                (list (compile-single s ns expand
                                                      #:serializable? serializable?
                                                      #:to-correlated-linklet? to-correlated-linklet?)))
                     #:combine append
                     #:observer #f)]))
  (if (and (= 1 (length cs))
           (not (compiled-multiple-top? (car cs))))
      (car cs)
      (compiled-tops->compiled-top cs
                                   #:to-correlated-linklet? to-correlated-linklet?
                                   #:merge-serialization? serializable?
                                   #:namespace ns)))

;; To communicate lifts from `expand-single` to `compile-single`:
(struct lifted-parsed-begin (seq last))

(define (compile-single s ns expand
                        #:serializable? serializable?
                        #:to-correlated-linklet? to-correlated-linklet?)
  (define exp-s (expand s ns #f #t serializable? to-correlated-linklet?))
  (let loop ([exp-s exp-s])
    (cond
      [(parsed-module? exp-s)
       (compile-module exp-s (make-compile-context #:namespace ns)
                       #:serializable? serializable?
                       #:to-correlated-linklet? to-correlated-linklet?)]
      [(lifted-parsed-begin? exp-s)
       ;; expansion must have captured lifts
       (compiled-tops->compiled-top
        #:to-correlated-linklet? to-correlated-linklet?
        (for/list ([e (in-list (append (lifted-parsed-begin-seq exp-s)
                                       (list (lifted-parsed-begin-last exp-s))))])
          (loop e)))]
      [else
       (compile-top exp-s (make-compile-context #:namespace ns)
                    #:serializable? serializable?
                    #:to-correlated-linklet? to-correlated-linklet?)])))

;; This `expand` is suitable as an expand handler (if such a thing
;; existed) to be called by `expand` and `expand-syntax`.
;; [Don't use keyword arguments here, because the function is
;;  exported for use by an embedding runtime system.]
(define (expand s
                [ns (current-namespace)] [observable? #f] [to-parsed? #f]
                [serializable? #f] [to-correlated-linklet? #f])
  (define observer (and observable? (current-expand-observe)))
  (when observer (...log-expand observer ['start-top]))
  (parameterize ([current-expand-observe #f])
    (per-top-level s ns
                   #:single (lambda (s ns as-tail?) (expand-single s ns observer to-parsed?
                                                                   #:serializable? serializable?
                                                                   #:to-correlated-linklet? to-correlated-linklet?))
                   #:combine cons
                   #:wrap re-pair
                   #:observer observer)))

(define (expand-single s ns observer to-parsed?
                       #:serializable? serializable?
                       #:to-correlated-linklet? [to-correlated-linklet? #f])
  (define rebuild-s (keep-properties-only s))
  (define ctx (make-expand-context ns
                                   #:to-parsed? to-parsed?
                                   #:for-serializable? serializable?
                                   #:to-correlated-linklet? to-correlated-linklet?
                                   #:observer observer))
  (define-values (require-lifts lifts exp-s) (expand-capturing-lifts s ctx))
  (cond
   [(and (null? require-lifts) (null? lifts)) exp-s]
   [to-parsed?
    (wrap-lifts-as-lifted-parsed-begin require-lifts
                                       lifts
                                       exp-s rebuild-s
                                       #:adjust-form (lambda (form)
                                                       (expand-single form ns observer to-parsed?
                                                                      #:serializable? serializable?)))]
   [else
    (log-top-lift-begin-before ctx require-lifts lifts exp-s ns)
    (define new-s
      (wrap-lifts-as-begin (append require-lifts lifts)
                           #:adjust-form (lambda (form)
                                           (log-expand ctx 'next)
                                           (expand-single form ns observer to-parsed?
                                                          #:serializable? serializable?))
                           #:adjust-body (lambda (form)
                                           (cond
                                             [to-parsed? form]
                                             [else
                                              (log-expand ctx 'next)
                                              ;; This re-expansion should be unnecessary, but we do it
                                              ;; for a kind of consistentcy with `expand/capture-lifts`
                                              ;; and for expansion observers
                                              (expand-single form ns observer to-parsed?
                                                             #:serializable? serializable?)]))
                           exp-s
                           (namespace-phase ns)))
    (log-top-begin-after ctx new-s)
    new-s]))

(define (expand-once s [ns (current-namespace)])
  (per-top-level s ns
                 #:single (lambda (s ns as-tail?) (expand-single-once s ns))
                 #:combine cons
                 #:wrap re-pair
                 #:just-once? #t
                 #:observer #f))

(define (expand-single-once s ns)
  (define-values (require-lifts lifts exp-s)
    (expand-capturing-lifts s (struct*-copy expand-context (make-expand-context ns)
                                            [just-once? #t])))
  (cond
   [(and (null? require-lifts) (null? lifts)) exp-s]
   [else
    (wrap-lifts-as-begin (append require-lifts lifts)
                         exp-s
                         (namespace-phase ns))]))

(define (expand-to-top-form s [ns (current-namespace)])
  ;; Use `per-top-level` for immediate expansion and lift handling,
  ;; but `#:single #f` makes it return immediately
  (define observer (current-expand-observe))
  (when observer (...log-expand observer ['start-top]))
  (parameterize ([current-expand-observe #f])
    (per-top-level s ns
                   #:single #f
                   #:quick-immediate? #f
                   #:observer observer)))

;; ----------------------------------------

;; Top-level compilation and evaluation, which involves partial
;; expansion to detect `begin` and `begin-for-syntax` to interleave
;; expansions
(define (per-top-level given-s ns
                       #:single single        ; handle discovered form; #f => stop after immediate
                       #:combine [combine #f] ; how to cons a recur result, or not
                       #:wrap [wrap #f]       ; how to wrap a list of recur results, or not
                       #:just-once? [just-once? #f] ; single expansion step
                       #:quick-immediate? [quick-immediate? #t]
                       #:serializable? [serializable? #f] ; for module+submodule expansion
                       #:observer observer)
  (define s (maybe-intro given-s ns))
  (define ctx (make-expand-context ns #:observer observer))
  (define phase (namespace-phase ns))
  (let loop ([s s] [phase phase] [ns ns] [as-tail? #t])
    (define tl-ctx (struct*-copy expand-context ctx
                                 [phase phase]
                                 [namespace ns]
                                 [just-once? just-once?]
                                 [for-serializable? serializable?]))
    (define wb-s (and just-once? s))
    (log-expand tl-ctx 'visit s)
    (define-values (require-lifts lifts exp-s)
      (expand-capturing-lifts s (struct*-copy expand-context tl-ctx
                                              [only-immediate? #t]
                                              [phase phase]
                                              [namespace ns])))
    (define disarmed-exp-s (raw:syntax-disarm exp-s))
    (cond
     [(or (pair? require-lifts) (pair? lifts))
      ;; Fold in lifted definitions and try again
      (define new-s (wrap-lifts-as-begin (append require-lifts lifts)
                                         exp-s
                                         phase))
      (log-expand tl-ctx 'lift-loop new-s)
      (if just-once?
          new-s
          (loop new-s phase ns as-tail?))]
     [(not single)
      (log-expand tl-ctx 'return exp-s)
      exp-s]
     [(and just-once? (not (eq? exp-s wb-s))) exp-s]
     [else
      (case (core-form-sym disarmed-exp-s phase)
        [(begin)
         (log-expand ctx 'prim-begin)
         (define-match m disarmed-exp-s '(begin e ...))
         ;; Map `loop` over the `e`s, but in the case of `eval`,
         ;; tail-call for last one:
         (define (begin-loop es)
           (cond
            [(null? es) (if combine null (void))]
            [(and (not combine) (null? (cdr es)))
             (loop (car es) phase ns as-tail?)]
            [else
             (log-expand tl-ctx 'next)
             (define a (if combine
                           (loop (car es) phase ns #f)
                           (begin
                             ;; Allow any number of results:
                             (loop (car es) phase ns #f)
                             (void))))
             (if combine
                 (combine a (begin-loop (cdr es)))
                 (begin-loop (cdr es)))]))
         (cond
           [wrap
            (define new-s (wrap (m 'begin) exp-s (begin-loop (m 'e))))
            (log-expand tl-ctx 'return new-s)
            new-s]
           [else (begin-loop (m 'e))])]
        [(begin-for-syntax)
         (log-expand tl-ctx 'prim-begin-for-syntax)
         (define-match m disarmed-exp-s '(begin-for-syntax e ...))
         (define next-phase (add1 phase))
         (define next-ns (namespace->namespace-at-phase ns next-phase))
         (log-expand tl-ctx 'prepare-env)
         (when quick-immediate?
           ;; In case `expand-capturing-lifts` didn't already:
           (namespace-visit-available-modules! ns))
         (namespace-visit-available-modules! next-ns) ; to match old behavior for empty body
         (define l
           (for/list ([s (in-list (m 'e))])
             (log-expand tl-ctx 'next)
             (loop s next-phase next-ns #f)))
         (cond
          [wrap
           (define new-s (wrap (m 'begin-for-syntax) exp-s l))
           (log-expand tl-ctx 'return new-s)
           new-s]
          [combine (apply append l)]
          [else (void)])]
        [else
         (single exp-s ns as-tail?)])])))

;; Add scopes to `s` if it's not syntax:
(define (maybe-intro s ns)
  (if (syntax? s)
      s
      (namespace-syntax-introduce (datum->syntax #f s) ns)))

(define (re-pair form-id s r)
  (raw:syntax-rearm
   (datum->syntax (raw:syntax-disarm s)
                  (cons form-id r)
                  s
                  s)
   s))

;; ----------------------------------------

(define (expand-capturing-lifts s ctx)
  (performance-region
   ['expand 'top]
   
   (define ns (expand-context-namespace ctx))
   (namespace-visit-available-modules! ns)
   
   (define lift-ctx (make-lift-context (make-top-level-lift ctx)))
   (define require-lift-ctx (make-require-lift-context
                             (namespace-phase ns)
                             (make-parse-top-lifted-require ns)))
   (define exp-s
     (expand-in-context s (struct*-copy expand-context ctx
                                        [lifts lift-ctx]
                                        [module-lifts lift-ctx]
                                        [require-lifts require-lift-ctx])))
   (values (get-and-clear-require-lifts! require-lift-ctx)
           (get-and-clear-lifts! lift-ctx)
           exp-s)))

(define (make-parse-top-lifted-require ns)
  (lambda (s phase)
    ;; We don't "hide" this require in the same way as
    ;; a top-level `#%require`, because it's already
    ;; hidden in the sense of having an extra scope
    (define-match m (raw:syntax-disarm s) '(#%require req))
    (parse-and-perform-requires! (list (m 'req)) s
                                 ns phase #:run-phase phase
                                 (make-requires+provides #f)
                                 #:who 'require)))

(define (wrap-lifts-as-lifted-parsed-begin require-lifts
                                           lifts
                                           exp-s rebuild-s
                                           #:adjust-form adjust-form)
  (lifted-parsed-begin (append
                        (for/list ([req (in-list require-lifts)])
                          (parsed-require req))
                        (for/list ([ids+syms+rhs (in-list (get-lifts-as-lists lifts))])
                          (define exp-rhs (adjust-form (caddr ids+syms+rhs)))
                          (define just-rhs (if (lifted-parsed-begin? exp-rhs)
                                               (lifted-parsed-begin-last exp-rhs)
                                               exp-rhs))
                          (define dv
                            (parsed-define-values rebuild-s
                                                  (car ids+syms+rhs)
                                                  (cadr ids+syms+rhs)
                                                  just-rhs))
                          (if (lifted-parsed-begin? exp-rhs)
                              (struct-copy lifted-parsed-begin exp-rhs
                                           [last dv])
                              dv)))
                       exp-s))

(define (log-top-lift-begin-before ctx require-lifts lifts exp-s ns)
  (log-expand...
   ctx
   (lambda (obs)
     (define new-s (wrap-lifts-as-begin (append require-lifts lifts)
                                        exp-s
                                        (namespace-phase ns)))
     (...log-expand obs ['lift-loop new-s])
     (log-top-begin-before ctx new-s))))

(define (log-top-begin-before ctx new-s)
  (log-expand...
   ctx
   (lambda (obs)
     (define-match m new-s '(begin e ...))
     (...log-expand obs
                    ['visit new-s] ['resolve (m 'begin)]
                    ['enter-prim new-s] ['prim-begin]
                    ['enter-list (datum->syntax #f (m 'e) new-s)]))))

(define (log-top-begin-after ctx new-s)
  (log-expand...
   ctx
   (lambda (obs)
     (define-match m new-s '(begin e ...))
     (log-expand* ctx
                  ['exit-list (datum->syntax #f (m 'e) new-s)]
                  ['exit-prim new-s]
                  ['return new-s]))))
