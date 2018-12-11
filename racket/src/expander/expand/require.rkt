#lang racket/base
(require "../common/set.rkt"
         "../common/performance.rkt"
         "../common/phase.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/provided.rkt"
         "../syntax/match.rkt"
         "module-path.rkt"
         "require+provide.rkt"
         "env.rkt"
         "../common/module-path.rkt"
         "../syntax/bulk-binding.rkt")

(provide parse-and-perform-requires!
         perform-initial-require!
         perform-require!
         require-spec-shift-for-syntax)

(struct adjust-only (syms))
(struct adjust-prefix (sym))
(struct adjust-all-except (prefix-sym syms))
(struct adjust-rename (to-id from-sym))

(define layers '(raw phaseless path))

(define (parse-and-perform-requires! reqs orig-s m-ns phase-shift
                                     requires+provides
                                     #:self [self #f]
                                     #:run-phase [run-phase (namespace-phase m-ns)]
                                     #:run? [run? #f]
                                     #:visit? [visit? #t]
                                     #:declared-submodule-names [declared-submodule-names #hasheq()]
                                     ;; For `namespace-require/copy` and `namespace-require/constant`:
                                     #:copy-variable-phase-level [copy-variable-phase-level #f]
                                     #:copy-variable-as-constant? [copy-variable-as-constant? #f]
                                     #:skip-variable-phase-level [skip-variable-phase-level #f]
                                     #:initial-require? [initial-require? #f]
                                     #:who who)
  (let loop ([reqs reqs]
             [top-req #f]
             [phase-shift phase-shift]
             [just-meta 'all]
             [adjust #f]
             [for-meta-ok? #t]
             [just-meta-ok? #t]
             [layer 'raw])
    (for/and ([req (in-list reqs)])
      (define (check-nested want-layer [ok? #t])
        (unless (and ok? (member want-layer (member layer layers)))
          (raise-syntax-error #f "invalid nesting" orig-s req)))
      (define fm (and (pair? (syntax-e req))
                      (identifier? (car (syntax-e req)))
                      (syntax-e (car (syntax-e req)))))
      (case fm
        [(for-meta)
         (check-nested 'raw for-meta-ok?)
         (define-match m req '(for-meta phase-level spec ...))
         (define p (syntax-e (m 'phase-level)))
         (unless (phase? p)
           (raise-syntax-error #f "bad phase" orig-s req))
         (loop (m 'spec) 
               (or top-req req)
               (phase+ phase-shift p)
               just-meta
               adjust
               #f just-meta-ok? 'raw)]
        [(for-syntax)
         (check-nested 'raw for-meta-ok?)
         (define-match m req '(for-syntax spec ...))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift 1)
               just-meta
               adjust
               #f just-meta-ok? 'raw)]
        [(for-template)
         (check-nested 'raw for-meta-ok?)
         (define-match m req '(for-template spec ...))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift -1)
               just-meta
               adjust
               #f just-meta-ok? 'raw)]
        [(for-label)
         (check-nested 'raw for-meta-ok?)
         (define-match m req '(for-label spec ...))
         (loop (m 'spec)
               (or top-req req)
               (phase+ phase-shift #f)
               just-meta
               adjust
               #f just-meta-ok? 'raw)]
        [(just-meta)
         (check-nested 'raw just-meta-ok?)
         (define-match m req '(just-meta phase-level spec ...))
         (define p (syntax-e (m 'phase-level)))
         (unless (phase? p)
           (raise-syntax-error #f "bad phase" orig-s req))
         (loop (m 'spec)
               (or top-req req)
               phase-shift
               p
               adjust
               for-meta-ok? #f 'raw)]
        [(only)
         (check-nested 'phaseless)
         (define-match m req '(only spec id ...))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-only (ids->sym-set (m 'id)))
               #f #f 'path)]
        [(prefix)
         (check-nested 'phaseless)
         (define-match m req '(prefix id:prefix spec))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-prefix (syntax-e (m 'id:prefix)))
               #f #f 'path)]
        [(all-except)
         (check-nested 'phaseless)
         (define-match m req '(all-except spec id ...))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-all-except '|| (ids->sym-set (m 'id)))
               #f #f 'path)]
        [(prefix-all-except)
         (check-nested 'phaseless)
         (define-match m req '(prefix-all-except id:prefix spec id ...))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-all-except (syntax-e (m 'id:prefix)) (ids->sym-set (m 'id)))
               #f #f 'path)]
        [(rename)
         (check-nested 'phaseless)
         (define-match m req '(rename spec id:to id:from))
         (loop (list (m 'spec))
               (or top-req req)
               phase-shift
               just-meta
               (adjust-rename (m 'id:to) (syntax-e (m 'id:from)))
               #f #f 'path)]
        [else
         (define maybe-mp (syntax->datum req))
         (unless (or (module-path? maybe-mp)
                     (resolved-module-path? maybe-mp))
           (raise-syntax-error #f "bad require spec" orig-s req))
         (when (or adjust (not (eq? just-meta 'all)))
           (set-requires+provides-all-bindings-simple?! requires+provides #f))
         (define mp (if (resolved-module-path? maybe-mp)
                        (resolved-module-path->module-path maybe-mp)
                        maybe-mp))
         (define mpi (module-path->mpi mp self
                                       #:declared-submodule-names declared-submodule-names))
         (perform-require! mpi req self
                           (or req top-req) m-ns
                           #:phase-shift phase-shift
                           #:run-phase run-phase
                           #:just-meta just-meta
                           #:adjust adjust
                           #:requires+provides requires+provides
                           #:run? run?
                           #:visit? visit?
                           #:copy-variable-phase-level copy-variable-phase-level
                           #:copy-variable-as-constant? copy-variable-as-constant?
                           #:skip-variable-phase-level skip-variable-phase-level
                           #:initial-require? initial-require?
                           #:who who)
         (set! initial-require? #f)]))))

(define (ids->sym-set ids)
  (for/set ([id (in-list ids)])
    (syntax-e id)))

;; ----------------------------------------

(define (perform-initial-require! mod-path self
                                  in-stx m-ns
                                  requires+provides
                                  #:bind? bind?
                                  #:who who)
  (perform-require! (module-path->mpi mod-path self) #f self
                    in-stx m-ns
                    #:phase-shift 0
                    #:run-phase 0
                    #:requires+provides requires+provides
                    #:can-be-shadowed? #t
                    #:initial-require? #t
                    #:bind? bind?
                    #:who who))

;; ----------------------------------------

(define (perform-require! mpi orig-s self
                          in-stx m-ns
                          #:phase-shift phase-shift
                          #:run-phase run-phase
                          #:just-meta [just-meta 'all]
                          #:adjust [adjust #f]
                          #:requires+provides [requires+provides #f]
                          #:visit? [visit? #t]
                          #:run? [run? #f]
                          #:can-be-shadowed? [can-be-shadowed? #f]
                          #:initial-require? [initial-require? #f]
                          ;; For `namespace-require/copy` and `namespace-require/constant`:
                          #:copy-variable-phase-level [copy-variable-phase-level #f]
                          #:copy-variable-as-constant? [copy-variable-as-constant? #f]
                          #:skip-variable-phase-level [skip-variable-phase-level #f]
                          #:bind? [bind? #t]
                          #:who who)
  (performance-region
   ['expand 'require]
   (define module-name (module-path-index-resolve mpi #t))
   (define bind-in-stx (if (adjust-rename? adjust)
                           (adjust-rename-to-id adjust)
                           in-stx))
   (define done-syms (and adjust (make-hash)))
   (define m (namespace->module m-ns module-name))
   (unless m (raise-unknown-module-error 'require module-name))
   (define interned-mpi
     (if requires+provides
         (add-required-module! requires+provides mpi phase-shift
                               (module-cross-phase-persistent? m))
         mpi))
   (when visit?
     (namespace-module-visit! m-ns interned-mpi phase-shift #:visit-phase run-phase))
   (when run?
     (namespace-module-instantiate! m-ns interned-mpi phase-shift #:run-phase run-phase))
   (when (not (or visit? run?))
     ;; make the module available:
     (namespace-module-make-available! m-ns interned-mpi phase-shift #:visit-phase run-phase))
   (define can-bulk-bind? (and (or (not adjust)
                                   (adjust-prefix? adjust)
                                   (adjust-all-except? adjust))
                               (not skip-variable-phase-level)))
   (define bulk-prefix (cond
                        [(adjust-prefix? adjust) (adjust-prefix-sym adjust)]
                        [(adjust-all-except? adjust) (adjust-all-except-prefix-sym adjust)]
                        [else #f]))
   (define bulk-excepts (cond
                         [(adjust-all-except? adjust) (adjust-all-except-syms adjust)]
                         [else #hasheq()]))
   (define update-nominals-box (and can-bulk-bind? (box null)))
   (bind-all-provides!
    m
    bind-in-stx phase-shift m-ns interned-mpi module-name
    #:in orig-s
    #:defines-mpi (and requires+provides (requires+provides-self requires+provides))
    #:only (cond
            [(adjust-only? adjust) (set->list (adjust-only-syms adjust))]
            [(adjust-rename? adjust) (list (adjust-rename-from-sym adjust))]
            [else #f])
    #:just-meta just-meta
    #:bind? bind?
    #:can-bulk? can-bulk-bind?
    #:bulk-prefix bulk-prefix
    #:bulk-excepts bulk-excepts
    #:bulk-callback (and
                     requires+provides
                     can-bulk-bind?
                     (lambda (provides provide-phase-level)
                       ;; Returns #t if any binding is already shadowed by a definition:
                       (add-bulk-required-ids! requires+provides
                                               bind-in-stx
                                               (module-self m) mpi phase-shift
                                               provides
                                               provide-phase-level
                                               #:prefix bulk-prefix
                                               #:excepts bulk-excepts
                                               #:symbols-accum (and (positive? (hash-count bulk-excepts))
                                                                    done-syms)
                                               #:can-be-shadowed? can-be-shadowed?
                                               #:check-and-remove? (not initial-require?)
                                               #:in orig-s
                                               #:accum-update-nominals update-nominals-box
                                               #:who who)))
    #:filter (and
              (or (not can-bulk-bind?)
                  copy-variable-phase-level)
              (lambda (binding as-transformer?)
                (define sym (module-binding-nominal-sym binding))
                (define provide-phase (module-binding-nominal-phase binding))
                (define adjusted-sym
                  (cond
                    [(not (symbol-interned? sym))
                     ;; Don't `require` non-interned symbols
                     #f]
                    [(and skip-variable-phase-level
                          (not as-transformer?)
                          (equal? provide-phase skip-variable-phase-level))
                     #f]
                    [(not adjust) sym]
                    [(adjust-only? adjust)
                     (and (set-member? (adjust-only-syms adjust) sym)
                          (hash-set! done-syms sym #t)
                          sym)]
                    [(adjust-prefix? adjust)
                     (string->symbol
                      (string-append (symbol->string (adjust-prefix-sym adjust))
                                     (symbol->string sym)))]
                    [(adjust-all-except? adjust)
                     (and (not (and (set-member? (adjust-all-except-syms adjust) sym)
                                    (hash-set! done-syms sym #t)))
                          (string->symbol
                           (string-append (symbol->string (adjust-all-except-prefix-sym adjust))
                                          (symbol->string sym))))]
                    [(adjust-rename? adjust)
                     (and (eq? sym (adjust-rename-from-sym adjust))
                          (hash-set! done-syms sym #t)
                          (adjust-rename-to-id adjust))]))
                (define skip-bind?
                  (cond
                    [(and adjusted-sym requires+provides)
                     (define s (datum->syntax bind-in-stx adjusted-sym))
                     (define bind-phase (phase+ phase-shift provide-phase))
                     (define skip-bind?
                       (cond
                         [initial-require? #f]
                         [else
                          (check-not-defined #:check-not-required? #t
                                             #:allow-defined? #t ; `define` shadows `require`
                                             requires+provides
                                             s bind-phase
                                             #:unless-matches binding
                                             #:in orig-s
                                             #:remove-shadowed!? #t
                                             #:who who)]))
                     (unless skip-bind?
                       (add-defined-or-required-id! requires+provides
                                                    s bind-phase binding
                                                    #:can-be-shadowed? can-be-shadowed?
                                                    #:as-transformer? as-transformer?))
                     skip-bind?]
                    [else #f]))
                (when (and copy-variable-phase-level
                           (not as-transformer?)
                           (equal? provide-phase copy-variable-phase-level))
                  (copy-namespace-value m-ns sym binding copy-variable-phase-level phase-shift
                                        copy-variable-as-constant?))
                (and (not skip-bind?) adjusted-sym))))
   ;; Now that a bulk binding is in place, update to merge nominals:
   (when update-nominals-box
     (for ([update! (in-list (unbox update-nominals-box))])
       (update!)))
   ;; check that we covered all expected ids:
   (define need-syms (cond
                      [(adjust-only? adjust)
                       (adjust-only-syms adjust)]
                      [(adjust-all-except? adjust)
                       (adjust-all-except-syms adjust)]
                      [(adjust-rename? adjust)
                       (set (adjust-rename-from-sym adjust))]
                      [else #f]))
   (when (and need-syms
              (not (= (set-count need-syms) (hash-count done-syms))))
     (for ([sym (in-set need-syms)])
       (unless (hash-ref done-syms sym #f)
         (raise-syntax-error who "not in nested spec" orig-s sym))))))

;; ----------------------------------------

(define (bind-all-provides! m in-stx phase-shift ns mpi module-name
                            #:in orig-s
                            #:defines-mpi defines-mpi
                            #:only only-syms
                            #:just-meta just-meta
                            #:bind? bind?
                            #:can-bulk? can-bulk?
                            #:bulk-prefix bulk-prefix
                            #:bulk-excepts bulk-excepts
                            #:filter filter
                            #:bulk-callback bulk-callback)
  (define self (module-self m))
  (for ([(provide-phase-level provides) (in-hash (module-provides m))]
        #:when (or (eq? just-meta 'all)
                   (eqv? just-meta provide-phase-level)))
    (define phase (phase+ phase-shift provide-phase-level))
    (define need-except?
      (and bulk-callback
           (bulk-callback provides provide-phase-level)))
    (when bind?
      (when filter
        (for ([sym (in-list (or only-syms (hash-keys provides)))])
          (define binding/p (hash-ref provides sym #f))
          (when binding/p
            (define b (provide-binding-to-require-binding binding/p sym
                                                          #:self self
                                                          #:mpi mpi
                                                          #:provide-phase-level provide-phase-level
                                                          #:phase-shift phase-shift))
            (let-values ([(sym) (filter b (provided-as-transformer? binding/p))])
              (when (and sym
                         (not can-bulk?)) ;; bulk binding added later
                ;; Add a non-bulk binding, since `filter` has checked/adjusted it
                (add-binding! (datum->syntax in-stx sym) b phase))))))
      ;; Add bulk binding after all filtering
      (when can-bulk?
        (define bulk-binding-registry (namespace-bulk-binding-registry ns))
        (add-bulk-binding! in-stx
                           (bulk-binding (or (and (not bulk-prefix)
                                                  (zero? (hash-count bulk-excepts))
                                                  provides)
                                             ;; During expansion, the submodules aren't be registered in
                                             ;; the bulk-binding registry for use by other submodules,
                                             ;; so do the work to compute bulk provides now if the module
                                             ;; isn't registered
                                             (and (not (registered-bulk-provide? bulk-binding-registry
                                                                                 module-name))
                                                  (bulk-provides-add-prefix-remove-exceptions
                                                   provides bulk-prefix bulk-excepts)))
                                         bulk-prefix bulk-excepts
                                         self mpi provide-phase-level phase-shift
                                         bulk-binding-registry)
                           phase
                           #:in orig-s
                           #:shadow-except (and need-except? defines-mpi))))))

;; ----------------------------------------

;; In certain lifting cases, we'd like to just throw a `for-syntax`
;; around a `require` specification, but that's not supported by our
;; `#%require` grammar. Instead, we have to adjust whatever phase
;; shift is present.
(define (require-spec-shift-for-syntax req)
  (define (rebuild-req req new-req)
    (datum->syntax req new-req req req))
  (define ((loop shifted?) req)
    (define fm (and (pair? (syntax-e req))
                    (identifier? (car (syntax-e req)))
                    (syntax-e (car (syntax-e req)))))
    (case fm
      [(for-meta)
       (define-match m req '(for-meta phase-level spec ...))
       (define p (syntax-e (m 'phase-level)))
       (unless (phase? p)
         (raise-syntax-error #f "bad phase" req))
       (rebuild-req req `(,(m 'for-meta) ,(phase+ p 1) ,@(map (loop #t) (m 'spec))))]
      [(for-syntax)
       (define-match m req '(for-syntax spec ...))
       (rebuild-req req `(for-meta 2 ,@(map (loop #t) (m 'spec))))]
      [(for-template)
       (define-match m req '(for-template spec ...))
       (rebuild-req req `(for-meta 0 ,@(map (loop #t) (m 'spec))))]
      [(for-label)
       (define-match m req '(for-label spec ...))
       (rebuild-req req `(,(m 'for-label) ,@(map (loop #t) (m 'spec))))]
      [(just-meta)
       (define-match m req '(just-meta phase-level spec ...))
       (rebuild-req req `(,(m 'just-meta) ,(m 'phase-level) ,@(map (loop #f) (m 'spec))))]
      [else
       (if shifted?
           req
           (datum->syntax #f `(for-syntax ,req)))]))
  ((loop #f) req))

;; ----------------------------------------

(define (copy-namespace-value m-ns adjusted-sym binding phase-level phase-shift as-constant?)
  (define i-ns (namespace->module-namespace m-ns
                                            (module-path-index-resolve (module-binding-module binding))
                                            (phase- (module-binding-phase binding) phase-level)
                                            #:complain-on-failure? #t))
  (define val (namespace-get-variable i-ns (module-binding-phase binding) (module-binding-sym binding)
                                      (lambda () (error 'namespace-require/copy
                                                   (format
                                                    (string-append "namespace mismatch;\n"
                                                                   " variable not found\n"
                                                                   "  module: ~a\n"
                                                                   "  variable name: ~s\n"
                                                                   "  phase level: ~s")
                                                    (module-binding-module binding)
                                                    (module-binding-sym binding)
                                                    (module-binding-phase binding))))))
  (namespace-set-variable! m-ns (phase+ phase-shift phase-level) adjusted-sym val as-constant?))
