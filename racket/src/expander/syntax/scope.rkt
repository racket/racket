#lang racket/base
(require racket/private/place-local
         ffi/unsafe/atomic
         "../common/set.rkt"
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/memo.rkt"
         "../common/inline.rkt"
         "syntax.rkt"
         "binding-table.rkt"
         "taint-object.rkt"
         "taint.rkt"
         "../common/phase.rkt"
         "fallback.rkt"
         "datum-map.rkt"
         "cache.rkt")

(provide new-scope
         make-interned-scope
         new-multi-scope
         add-scope
         add-scopes
         remove-scope
         remove-scopes
         flip-scope
         flip-scopes
         push-scope

         syntax-e ; handles lazy scope and taint propagation

         syntax-scope-set
         syntax-any-scopes?
         syntax-any-macro-scopes?

         syntax-shift-phase-level

         syntax-swap-scopes

         add-binding-in-scopes!
         add-bulk-binding-in-scopes!

         propagation-mpi-shift ; for use by "binding.rkt"

         resolve

         bound-identifier=?

         top-level-common-scope

         deserialize-scope
         deserialize-scope-fill!
         deserialize-representative-scope
         deserialize-representative-scope-fill!
         deserialize-multi-scope
         deserialize-shifted-multi-scope

         generalize-scope

         scope?
         scope<?
         shifted-multi-scope?
         shifted-multi-scope<?

         interned-scope-symbols
         interned-scopes
         syntax-has-interned-scope?

         scope-place-init!)

(module+ for-debug
  (provide (struct-out scope)
           (struct-out interned-scope)
           (struct-out multi-scope)
           (struct-out representative-scope)
           scope-set-at-fallback
           shifted-multi-scope-add-binding-phases))

;; A scope represents a distinct "dimension" of binding. We can attach
;; the bindings for a set of scopes to an arbitrary scope in the set;
;; we pick the most recently allocated scope to make a binding search
;; faster and to improve GC, since non-nested binding contexts will
;; generally not share a most-recent scope.

(struct scope (id             ; internal scope identity; used for sorting
               kind           ; 'macro for macro-introduction scopes, otherwise treated as debug info
               [binding-table #:mutable]) ; see "binding-table.rkt"
  #:authentic
  ;; Custom printer:
  #:property prop:custom-write
  (lambda (sc port mode)
    (write-string "#<scope:" port)
    (display (scope-id sc) port)
    (write-string ":" port)
    (display (scope-kind sc) port)
    (write-string ">" port))
  #:property prop:serialize
  (lambda (s ser-push! state)
    (unless (set-member? (serialize-state-reachable-scopes state) s)
      (error "internal error: found supposedly unreachable scope"))
    (cond
      [(eq? s top-level-common-scope)
       (ser-push! 'tag '#:scope)]
      [else
       (ser-push! 'tag '#:scope+kind)
       (ser-push! (scope-kind s))]))
  #:property prop:serialize-fill!
  (lambda (s ser-push! state)
    (cond
      [(binding-table-empty? (scope-binding-table s))
       (ser-push! 'tag #f)]
      [else
       (ser-push! 'tag '#:scope-fill!)
       (ser-push! (binding-table-prune-to-reachable (scope-binding-table s) state))]))
  #:property prop:reach-scopes
  (lambda (s extra-shifts reach)
    ;; the `bindings` field is handled via `prop:scope-with-bindings`
    (void))
  #:property prop:scope-with-bindings
  (lambda (s get-reachable-scopes extra-shifts reach register-trigger)
    (binding-table-register-reachable (scope-binding-table s)
                                      get-reachable-scopes
                                      extra-shifts
                                      reach
                                      register-trigger)))

(define deserialize-scope
  (case-lambda
    [() top-level-common-scope]
    [(kind)
     (scope (new-deserialize-scope-id!) kind empty-binding-table)]))

(define (deserialize-scope-fill! s bt)
  (set-scope-binding-table! s bt))

;; An "interned scope" is a scope identified by an interned symbol that is
;; consistent across both module instantiations and bytecode unmarshalling.
;; Creating an interned scope with the same symbol will always produce the
;; same scope.
(struct interned-scope scope (key)  ; symbolic key used for interning
  #:authentic
  #:property prop:custom-write
  (lambda (sc port mode)
    (write-string "#<scope:" port)
    (display (scope-id sc) port)
    (write-string ":" port)
    (display (scope-kind sc) port)
    (write-string " " port)
    (display (interned-scope-key sc) port)
    (write-string ">" port))
  #:property prop:serialize
  (lambda (s ser-push! state)
    (unless (set-member? (serialize-state-reachable-scopes state) s)
      (error "internal error: found supposedly unreachable scope"))
    (ser-push! 'tag '#:interned-scope)
    (ser-push! (interned-scope-key s))))

;; A "multi-scope" represents a group of scopes, each of which exists
;; only at a specific phase, and each in a distinct phase. This
;; infinite group of scopes is realized on demand. A multi-scope is
;; used to represent the inside of a module, where bindings in
;; different phases are distinguished by the different scopes within
;; the module's multi-scope.
;;
;; To compute a syntax's set of scopes at a given phase, the
;; phase-specific representative of the multi scope is combined with
;; the phase-independent scopes. Since a multi-scope corresponds to
;; a module, the number of multi-scopes in a syntax is expected to
;; be small.
(struct multi-scope (id       ; identity
                     name     ; for debugging
                     scopes   ; box of table: phase -> representative-scope
                     shifted  ; box of table: interned shifted-multi-scopes for non-label phases
                     label-shifted) ; box of table: interned shifted-multi-scopes for label phases
  #:authentic
  #:sealed
  #:property prop:serialize
  (lambda (ms ser-push! state)
    (ser-push! 'tag '#:multi-scope)
    (ser-push! (multi-scope-name ms))
    ;; Prune to reachable representative scopes
    (define multi-scope-tables (serialize-state-multi-scope-tables state))
    (ser-push! (or (hash-ref multi-scope-tables (multi-scope-scopes ms) #f)
                   (let ([ht (for/hasheqv ([(phase sc) (in-hash (unbox (multi-scope-scopes ms)))]
                                           #:when (set-member? (serialize-state-reachable-scopes state) sc))
                               (values phase sc))])
                     (hash-set! multi-scope-tables (multi-scope-scopes ms) ht)
                     ht))))
  #:property prop:reach-scopes
  (lambda (s extra-shifts reach)
    ;; the `scopes` field is handled via `prop:scope-with-bindings`
    (void))
  #:property prop:scope-with-bindings
  (lambda (ms get-reachable-scopes bulk-shifts reach register-trigger)
    ;; This scope is reachable via its multi-scope, but it only
    ;; matters if it's reachable through a binding (otherwise it
    ;; can be re-generated later). We don't want to keep a scope
    ;; that can be re-generated, because pruning it makes
    ;; compilation more deterministic relative to other
    ;; compilations that involve a shared module. If the scope
    ;; itself has any bindings, then we count it as reachable
    ;; through a binding (which is an approxmation, because
    ;; other scopes in the binding may be unreachable, but it
    ;; seems good enough for determinism).
    ;; To make that work, `binding-table-register-reachable`
    ;; needs to recognize representative scopes and treat
    ;; them differently, hence `prop:implicitly-reachable`.
    (for ([sc (in-hash-values (unbox (multi-scope-scopes ms)))])
      (unless (binding-table-empty? (scope-binding-table sc))
        (reach sc bulk-shifts)))))

(define (deserialize-multi-scope name scopes)
  (multi-scope (new-deserialize-scope-id!) name (box scopes) (box (hasheqv)) (box (hash))))

(struct representative-scope scope (owner   ; a multi-scope for which this one is a phase-specific identity
                                    phase)  ; phase of this scope
  #:authentic
  #:mutable ; to support serialization
  #:property prop:custom-write
  (lambda (sc port mode)
    (write-string "#<scope:" port)
    (display (scope-id sc) port)
    (when (representative-scope-owner sc)
      (write-string "=" port)
      (display (multi-scope-id (representative-scope-owner sc)) port))
    (write-string "@" port)
    (display (representative-scope-phase sc) port)
    (write-string ">" port))
  #:property prop:serialize
  (lambda (s ser-push! state)
    (ser-push! 'tag '#:representative-scope)
    (ser-push! (scope-kind s))
    (ser-push! (representative-scope-phase s)))
  #:property prop:serialize-fill!
  (lambda (s ser-push! state)
    (ser-push! 'tag '#:representative-scope-fill!)
    (ser-push! (binding-table-prune-to-reachable (scope-binding-table s) state))
    (ser-push! (representative-scope-owner s)))
  #:property prop:reach-scopes
  (lambda (s bulk-shifts reach)
    ;; the inherited `bindings` field is handled via `prop:scope-with-bindings`
    (reach (representative-scope-owner s) bulk-shifts))
  ;; Used by `binding-table-register-reachable`:
  #:property prop:implicitly-reachable #t)

(define (deserialize-representative-scope kind phase)
  (define v (representative-scope (new-deserialize-scope-id!) kind #f #f phase))
  v)

(define (deserialize-representative-scope-fill! s bt owner)
  (deserialize-scope-fill! s bt)
  (set-representative-scope-owner! s owner))

(struct shifted-multi-scope (phase        ; non-label phase shift or shifted-to-label-phase
                             multi-scope) ; a multi-scope
  #:authentic
  #:sealed
  #:property prop:custom-write
  (lambda (sms port mode)
    (write-string "#<scope:" port)
    (display (multi-scope-id (shifted-multi-scope-multi-scope sms)) port)
    (write-string "@" port)
    (display (shifted-multi-scope-phase sms) port)
    (write-string ">" port))
  #:property prop:serialize
  (lambda (sms ser-push! state)
    (ser-push! 'tag '#:shifted-multi-scope)
    (ser-push! (shifted-multi-scope-phase sms))
    (ser-push! (shifted-multi-scope-multi-scope sms)))
  #:property prop:reach-scopes
  (lambda (sms bulk-shifts reach)
    (reach (shifted-multi-scope-multi-scope sms) bulk-shifts)))

(define (deserialize-shifted-multi-scope phase multi-scope)
  (intern-shifted-multi-scope phase multi-scope))

(define (intern-shifted-multi-scope phase multi-scope)
  (define (transaction-loop boxed-table key make)
    (or (hash-ref (unbox boxed-table) phase #f)
        (let* ([val (make)]
               [current (unbox boxed-table)]
               [next (hash-set current key val)])
          (if (box-cas! boxed-table current next)
              val
              (transaction-loop boxed-table key make)))))
  (cond
   [(phase? phase)
    ;; `eqv?`-hashed by phase
    (or (hash-ref (unbox (multi-scope-shifted multi-scope)) phase #f)
        (transaction-loop (multi-scope-shifted multi-scope)
                          phase
                          (lambda () (shifted-multi-scope phase multi-scope))))]
   [else
    ;; `equal?`-hashed by shifted-to-label-phase
    (or (hash-ref (unbox (multi-scope-label-shifted multi-scope)) phase #f)
        (transaction-loop (multi-scope-label-shifted multi-scope)
                          phase
                          (lambda () (shifted-multi-scope phase multi-scope))))]))

;; A `shifted-to-label-phase` record in the `phase` field of a
;; `shifted-multi-scope` makes the shift reversible; when we're
;; looking up the label phase, then use the representative scope at
;; phase `from`; when we're looking up a non-label phase, there is no
;; corresponding representative scope
(struct shifted-to-label-phase (from) #:prefab)

;; Each new scope increments the counter, so we can check whether one
;; scope is newer than another.
(define-place-local id-counter 0)
(define (new-scope-id!)
  (set! id-counter (add1 id-counter))
  id-counter)

(define (new-deserialize-scope-id!)
  ;; negative scope ensures that new scopes are recognized as such by
  ;; having a larger id
  (- (new-scope-id!)))

(define (deserialized-scope-id? scope-id)
  (negative? scope-id))

;; A shared "outside-edge" scope for all top-level contexts
(define top-level-common-scope (scope 0 'module empty-binding-table))

(define (new-scope kind)
  (scope (new-scope-id!) kind empty-binding-table))

;; The intern table used for interned scopes. Access to the table must be
;; atomic so that the table is not left locked if the expansion thread is
;; killed.
(define-place-local interned-scopes-table (make-ephemeron-hasheq))

(define (interned-scope-symbols)
  (call-as-atomic
   (lambda ()
     (hash-keys interned-scopes-table))))

(define (interned-scopes)
  (call-as-atomic
   (lambda ()
     (for/seteq ([s (in-hash-values interned-scopes-table)])
       s))))

(define (scope-place-init!)
  (set! interned-scopes-table (make-ephemeron-hasheq)))

(define (make-interned-scope sym)
  (define (make)
    ;; since interned scopes are reused by unmarshalled code, and because they’re generally unlikely
    ;; to be a good target for bindings, always create them with a negative id
    (interned-scope (- (new-scope-id!)) 'interned empty-binding-table sym))
  (call-as-atomic
   (lambda ()
     (or (hash-ref! interned-scopes-table sym #f)
         (let ([new (make)])
           (hash-set! interned-scopes-table sym new)
           new)))))

(define (syntax-has-interned-scope? s)
  (for/or ([sc (in-set (syntax-scopes s))])
    (interned-scope? sc)))

(define (new-multi-scope [name #f])
  (intern-shifted-multi-scope 0 (multi-scope (new-scope-id!) name (box (hasheqv)) (box (hasheqv)) (box (hash)))))

(define (multi-scope-to-scope-at-phase ms phase)
  ;; Get the identity of `ms` at phase`
  (let ([scopes (unbox (multi-scope-scopes ms))])
    (or (hash-ref scopes phase #f)
        (let ([s (representative-scope (if (deserialized-scope-id? (multi-scope-id ms))
                                           (new-deserialize-scope-id!)
                                           (new-scope-id!))
                                       'module
                                       empty-binding-table
                                       ms phase)])
          (if (box-cas! (multi-scope-scopes ms) scopes (hash-set scopes phase s))
              s
              (multi-scope-to-scope-at-phase ms phase))))))

(define (scope>? sc1 sc2)
  ((scope-id sc1) . > . (scope-id sc2)))
(define (scope<? sc1 sc2)
  ((scope-id sc1) . < . (scope-id sc2)))

(define (shifted-multi-scope<? sms1 sms2)
  (define ms1 (shifted-multi-scope-multi-scope sms1))
  (define ms2 (shifted-multi-scope-multi-scope sms2))
  (if (eq? ms1 ms2)
      (let ([p1 (shifted-multi-scope-phase sms1)]
            [p2 (shifted-multi-scope-phase sms2)])
        (cond
         [(shifted-to-label-phase? p1)
          (cond
           [(shifted-to-label-phase? p2)
            (phase<? (shifted-to-label-phase-from p1) (shifted-to-label-phase-from p2))]
           [else #f])]
         [(shifted-to-label-phase? p2) #t]
         [else (phase<? p1 p2)]))
      ((multi-scope-id ms1) . < . (multi-scope-id ms2))))

;; Adding, removing, or flipping a scope is propagated
;; lazily to subforms
(define-inline (apply-scope s sc op prop-op)
  (define content* (syntax-content* s))
  (define content (if (modified-content? content*)
                      (modified-content-content content*)
                      content*))
  (if (shifted-multi-scope? sc)
      (struct-copy syntax s
                   [shifted-multi-scopes (fallback-update-first (syntax-shifted-multi-scopes s)
                                                                (lambda (smss)
                                                                  (op (fallback-first smss) sc)))]
                   [content* (if (datum-has-elements? content)
                                 (let ([prop (prop-op (and (modified-content? content*)
                                                           (modified-content-scope-propagations+taint content*))
                                                      sc
                                                      (syntax-scopes s)
                                                      (syntax-shifted-multi-scopes s)
                                                      (syntax-mpi-shifts s))])
                                   (if prop
                                       (modified-content content prop)
                                       content))
                                 content*)])
      (struct-copy syntax s
                   [scopes (op (syntax-scopes s) sc)]
                   [content* (if (datum-has-elements? content)
                                 (let ([prop (prop-op (and (modified-content? content*)
                                                           (modified-content-scope-propagations+taint content*))
                                                      sc
                                                      (syntax-scopes s)
                                                      (syntax-shifted-multi-scopes s)
                                                      (syntax-mpi-shifts s))])
                                   (if prop
                                       (modified-content content prop)
                                       content))
                                 content*)])))

(define (syntax-propagated-content* s)
  (define content* (syntax-content* s))
  (cond
    [(not (modified-content? content*))
     content*]
    [else
     (define prop (modified-content-scope-propagations+taint content*))
     (cond
       [(or (propagation? prop)
            (taint-needs-propagate? prop))
        (define content (modified-content-content content*))
        (define new-content
          (cond
            [(propagation? prop)
             (non-syntax-map content
                             (lambda (tail? x) x)
                             (lambda (sub-s)
                               (define sub-content* (syntax-content* sub-s))
                               (define sub-content (if (modified-content? sub-content*)
                                                       (modified-content-content sub-content*)
                                                       sub-content*))
                               (define scope-propagations+taint
                                 (propagation-merge
                                  sub-content
                                  prop
                                  (and (modified-content? sub-content*)
                                       (modified-content-scope-propagations+taint sub-content*))
                                  (syntax-scopes sub-s)
                                  (syntax-shifted-multi-scopes sub-s)
                                  (syntax-mpi-shifts sub-s)))
                               (struct-copy syntax sub-s
                                            [scopes (propagation-apply
                                                     prop
                                                     (syntax-scopes sub-s)
                                                     s)]
                                            [shifted-multi-scopes (propagation-apply-shifted
                                                                   prop
                                                                   (syntax-shifted-multi-scopes sub-s)
                                                                   s)]
                                            [mpi-shifts (propagation-apply-mpi-shifts
                                                         prop
                                                         (syntax-mpi-shifts sub-s)
                                                         s)]
                                            [inspector (propagation-apply-inspector
                                                        prop
                                                        (syntax-inspector sub-s))]
                                            [content* (if scope-propagations+taint
                                                          (modified-content sub-content scope-propagations+taint)
                                                          sub-content)])))]
            [else
             (non-syntax-map content
                             (lambda (tail? x) x)
                             (lambda (sub-s)
                               (struct-copy/t syntax sub-s
                                              [taint (tainted-for-content
                                                      (syntax-content sub-s))])))]))
        (define new-taint (taint-propagated (if (propagation? prop)
                                                (propagation-taint prop)
                                                prop)))
        (define new-content* (if new-taint
                                 (modified-content new-content new-taint)
                                 new-content))
        (if (syntax-content*-cas! s content* new-content*)
            new-content*
            ;; some other thread beat us to it?
            (syntax-propagated-content* s))]
       [else content*])]))

(define (syntax-e s)
  (define e (syntax-content* s))
  (cond
    ;; Shortcut for most common case:
    [(symbol? e) e]
    ;; General case:
    [else
     (define content* (syntax-propagated-content* s))
     (if (modified-content? content*)
         (modified-content-content content*)
         content*)]))

;; When a representative-scope is manipulated, we want to
;; manipulate the multi scope, instead (at a particular
;; phase shift)
(define (generalize-scope sc)
  (if (representative-scope? sc)
      (intern-shifted-multi-scope (representative-scope-phase sc)
                                  (representative-scope-owner sc))
      sc))

(define (add-scope s sc)
  (apply-scope s (generalize-scope sc) set-add propagation-add))

(define (add-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (add-scope s sc)))

(define (remove-scope s sc)
  (apply-scope s (generalize-scope sc) set-remove propagation-remove))

(define (remove-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (remove-scope s sc)))

(define (set-flip s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(define (flip-scope s sc)
  (apply-scope s (generalize-scope sc) set-flip propagation-flip))

(define (flip-scopes s scs)
  (for/fold ([s s]) ([sc (in-list scs)])
    (flip-scope s sc)))

;; Pushes a multi-scope to accommodate multiple top-level namespaces.
;; See "fallback.rkt".
(define (push-scope s sms)
  (define-memo-lite (push smss/maybe-fallbacks)
    (define smss (fallback-first smss/maybe-fallbacks))
    (cond
     [(set-empty? smss) (set-add smss sms)]
     [(set-member? smss sms) smss/maybe-fallbacks]
     [else (fallback-push (set-add smss sms)
                          smss/maybe-fallbacks)]))
  (syntax-map s
              (lambda (tail? x) x)
              (lambda (s d)
                (struct-copy syntax s
                             [content* (re-modify-content s d)]
                             [shifted-multi-scopes
                              (push (syntax-shifted-multi-scopes s))]))
              syntax-e))

;; ----------------------------------------

(struct propagation (prev-scs   ; owner's scopes before ops
                     prev-smss  ; owner's shifted multi scopes before ops
                     scope-ops  ; scope -> (or/c 'add 'remove 'flip)
                     ;; mpi-shifts and inspectors are mostly
                     ;; implemented at the "binding.rkt" layer,
                     ;; but we accommodate them here
                     prev-mss   ; owner's mpi-shifts before adds
                     add-mpi-shifts ; #f or (mpi-shifts -> mpi-shifts)
                     inspector  ; #f or inspector
                     taint)    ; see "taint-object.rkt"
  #:authentic
  #:sealed
  #:property prop:propagation syntax-e
  #:property prop:propagation-taint (lambda (p) (propagation-taint p))
  #:property prop:propagation-set-taint (lambda (p v) (propagation-set-taint p v)))

(define (propagation-add prop sc prev-scs prev-smss prev-mss)
  (if (propagation? prop)
      (struct-copy propagation prop
                   [scope-ops (hash-set (propagation-scope-ops prop)
                                        sc
                                        'add)])
      (propagation prev-scs prev-smss (hasheq sc 'add)
                   prev-mss #f #f
                   prop)))

(define (propagation-remove prop sc prev-scs prev-smss prev-mss)
  (if (propagation? prop)
      (struct-copy propagation prop
                   [scope-ops (hash-set (propagation-scope-ops prop)
                                        sc
                                        'remove)])
      (propagation prev-scs prev-smss (hasheq sc 'remove)
                   prev-mss #f #f
                   prop)))

(define (propagation-flip prop sc prev-scs prev-smss prev-mss)
  (if (propagation? prop)
      (let* ([ops (propagation-scope-ops prop)]
             [current-op (hash-ref ops sc #f)])
        (cond
         [(and (eq? current-op 'flip)
               (= 1 (hash-count ops))
               (not (propagation-inspector prop))
               (not (propagation-add-mpi-shifts prop)))
          ;; Nothing left to propagate, except maybe taint
          (propagation-taint prop)]
         [else
          (struct-copy propagation prop
                       [scope-ops
                        (if (eq? current-op 'flip)
                            (hash-remove ops sc)
                            (hash-set ops sc (case current-op
                                               [(add) 'remove]
                                               [(remove) 'add]
                                               [else 'flip])))])]))
      (propagation prev-scs prev-smss (hasheq sc 'flip)
                   prev-mss #f #f
                   prop)))

(define (propagation-mpi-shift prop add inspector prev-scs prev-smss prev-mss)
  (if (propagation? prop)
      (struct-copy propagation prop
                   [add-mpi-shifts (let ([base-add (propagation-add-mpi-shifts prop)])
                                     (if (and add base-add)
                                         (lambda (mss) (add (base-add mss)))
                                         (or add base-add)))]
                   [inspector (or (propagation-inspector prop)
                                  inspector)])
      (propagation prev-scs prev-smss #hasheq()
                   prev-mss add inspector
                   prop)))

(define (propagation-apply prop scs parent-s)
  (cond
   [(eq? (propagation-prev-scs prop) scs)
    (syntax-scopes parent-s)]
   [else
    (define new-scs
      (for/fold ([scs scs]) ([(sc op) (in-immutable-hash (propagation-scope-ops prop))]
                             #:when (not (shifted-multi-scope? sc)))
        (case op
          [(add) (set-add scs sc)]
          [(remove) (set-remove scs sc)]
          [else (set-flip scs sc)])))
    ;; Improve sharing if the result matches the parent:
    (if (set=? new-scs (syntax-scopes parent-s))
        (syntax-scopes parent-s)
        (cache-or-reuse-set new-scs))]))

(define (propagation-apply-shifted prop smss parent-s)
  (cond
   [(eq? (propagation-prev-smss prop) smss)
    (syntax-shifted-multi-scopes parent-s)]
   [else
    (define new-smss
      (for/fold ([smss smss]) ([(sms op) (in-immutable-hash (propagation-scope-ops prop))]
                               #:when (shifted-multi-scope? sms))
        (fallback-update-first
         smss
         (lambda (smss)
           (case op
             [(add) (set-add smss sms)]
             [(remove) (set-remove smss sms)]
             [else (set-flip smss sms)])))))
    ;; Improve sharing if the result clearly matches the parent:
    (define parent-smss (syntax-shifted-multi-scopes parent-s))
    (if (and (set? new-smss)
             (set? parent-smss)
             (set=? new-smss parent-smss))
        parent-smss
        (cache-or-reuse-hash new-smss))]))

(define (propagation-apply-mpi-shifts prop mss parent-s)
  (cond
   [(eq? (propagation-prev-mss prop) mss)
    (syntax-mpi-shifts parent-s)]
   [else
    (define add (propagation-add-mpi-shifts prop))
    (if add
        (add mss)
        mss)]))

(define (propagation-apply-inspector prop i)
  (or i (propagation-inspector prop)))

(define (propagation-set-taint prop t)
  (if (propagation? prop)
      (struct-copy propagation prop
                   [taint t])
      t))

(define (propagation-merge content prop base-prop prev-scs prev-smss prev-mss)
  (cond
   [(not (datum-has-elements? content))
    (if (propagation-taint prop)
        'tainted
        base-prop)]
   [(not (propagation? base-prop))
    (cond
     [(and (eq? (propagation-prev-scs prop) prev-scs)
           (eq? (propagation-prev-smss prop) prev-smss)
           (eq? (propagation-prev-mss prop) prev-mss)
           (eq? (propagation-taint prop) base-prop))
      prop]
     [else
      (propagation prev-scs
                   prev-smss
                   (propagation-scope-ops prop)
                   prev-mss
                   (propagation-add-mpi-shifts prop)
                   (propagation-inspector prop)
                   (if (propagation-taint prop)
                       'tainted/need-propagate
                       base-prop))])]
   [else
    (define new-ops
      ;; [could call `cache-or-reuse-hash` here (or a copy for propagations),
      ;;  but that doesn't seem to same time or space overall]
      (for/fold ([ops (propagation-scope-ops base-prop)]) ([(sc op) (in-immutable-hash (propagation-scope-ops prop))])
        (case op
          [(add) (hash-set ops sc 'add)]
          [(remove) (hash-set ops sc 'remove)]
          [else ; flip
           (define current-op (hash-ref ops sc #f))
           (case current-op
             [(add) (hash-set ops sc 'remove)]
             [(remove) (hash-set ops sc 'add)]
             [(flip) (hash-remove ops sc)]
             [else (hash-set ops sc 'flip)])])))
    (define add (propagation-add-mpi-shifts prop))
    (define base-add (propagation-add-mpi-shifts base-prop))
    (define new-taint
      (if (or (propagation-taint prop)
              (propagation-taint base-prop))
          'tainted/need-propagate
          (propagation-taint base-prop)))
    (if (and (zero? (hash-count new-ops))
             (not add)
             (not base-add)
             (not (propagation-inspector prop))
             (not (propagation-inspector base-prop)))
        new-taint
        (struct-copy propagation base-prop
                     [scope-ops new-ops]
                     [add-mpi-shifts (if (and add base-add)
                                         (lambda (mss) (add (base-add mss)))
                                         (or add base-add))]
                     [inspector (or (propagation-inspector base-prop)
                                    (propagation-inspector prop))]
                     [taint new-taint]))]))

;; ----------------------------------------

;; To shift a syntax's phase, we only have to shift the phase
;; of any phase-specific scopes. The bindings attached to a
;; scope must be represented in such a way that the binding
;; shift is implicit via the phase in which the binding
;; is resolved.
(define (shift-multi-scope sms delta)
  (cond
   [(zero-phase? delta)
    ;; No-op shift
    sms]
   [(label-phase? delta)
    (cond
     [(shifted-to-label-phase? (shifted-multi-scope-phase sms))
      ;; Shifting to the label phase moves only phase 0, so
      ;; drop a scope that is already collapsed to phase #f
      #f]
     [else
      ;; Move the current phase 0 to the label phase, which
      ;; means recording the negation of the current phase
      (intern-shifted-multi-scope (shifted-to-label-phase (phase- 0 (shifted-multi-scope-phase sms)))
                                  (shifted-multi-scope-multi-scope sms))])]
   [(shifted-to-label-phase? (shifted-multi-scope-phase sms))
    ;; Numeric shift has no effect on bindings in phase #f
    sms]
   [else
    ;; Numeric shift added to an existing numeric shift
    (intern-shifted-multi-scope (phase+ delta (shifted-multi-scope-phase sms))
                                (shifted-multi-scope-multi-scope sms))]))

;; Since we tend to shift rarely and only for whole modules, it's
;; probably not worth making this lazy
(define (syntax-shift-phase-level s phase)
  (if (eqv? phase 0)
      s
      (let ()
        (define-memo-lite (shift-all smss)
          (fallback-map
           smss
           (lambda (smss)
             (for*/seteq ([sms (in-set smss)]
                          [new-sms (in-value (shift-multi-scope sms phase))]
                          #:when new-sms)
               new-sms))))
        (syntax-map s
                    (lambda (tail? d) d)
                    (lambda (s d)
                      (struct-copy syntax s
                                   [content* (re-modify-content s d)]
                                   [shifted-multi-scopes
                                    (shift-all (syntax-shifted-multi-scopes s))]))
                    syntax-e))))


;; add each phase where `sms` might possibly have a binding
(define (shifted-multi-scope-add-binding-phases sms phases)
  (define ms (shifted-multi-scope-multi-scope sms))
  (define phase (shifted-multi-scope-phase sms))
  (cond
    [(shifted-to-label-phase? phase)
     (set-add phases #f)]
    [else
     (for/fold ([phases phases])
               ([ph (in-hash-keys (unbox (multi-scope-scopes ms)))])
       (if (label-phase? ph)
           (set-add phases #f)
           (set-add phases (phase- phase ph))))]))

;; ----------------------------------------

;; Scope swapping is used to make top-level compilation relative to
;; the top level. Each top-level environment has a set of scopes that
;; identify the environment; usually, it's a common outside-edge scope
;; and a namespace-specific inside-edge scope, but there can be
;; additional scopes due to `module->namespace` on a module that was
;; expanded multiple times (where each expansion adds scopes).
(define (syntax-swap-scopes s src-scopes dest-scopes)
  (if (equal? src-scopes dest-scopes)
      s
      (let-values ([(src-smss src-scs)
                    (set-partition (for/seteq ([sc (in-set src-scopes)])
                                     (generalize-scope sc))
                                   shifted-multi-scope?
                                   (seteq)
                                   (seteq))]
                   [(dest-smss dest-scs)
                    (set-partition (for/seteq ([sc (in-set dest-scopes)])
                                     (generalize-scope sc))
                                   shifted-multi-scope?
                                   (seteq)
                                   (seteq))])
        (define-memo-lite (swap-scs scs)
          (if (subset? src-scs scs)
              (set-union (set-subtract scs src-scs) dest-scs)
              scs))
        (define-memo-lite (swap-smss smss)
          (fallback-update-first
           smss
           (lambda (smss)
             (if (subset? src-smss smss)
                 (set-union (set-subtract smss src-smss) dest-smss)
                 smss))))
        (syntax-map s
                    (lambda (tail? d) d)
                    (lambda (s d)
                      (struct-copy syntax s
                                   [content* (re-modify-content s d)]
                                   [scopes (swap-scs (syntax-scopes s))]
                                   [shifted-multi-scopes
                                    (swap-smss (syntax-shifted-multi-scopes s))]))
                    syntax-e))))

;; ----------------------------------------

;; Assemble the complete set of scopes at a given phase by extracting
;; a phase-specific representative from each multi-scope.
(define (syntax-scope-set s phase)
  (scope-set-at-fallback s (fallback-first (syntax-shifted-multi-scopes s)) phase))

(define (scope-set-at-fallback s smss phase)
  (for*/fold ([scopes (syntax-scopes s)]) ([sms (in-set smss)]
                                           #:when (or (label-phase? phase)
                                                      (not (shifted-to-label-phase? (shifted-multi-scope-phase sms)))))
    (set-add scopes (multi-scope-to-scope-at-phase (shifted-multi-scope-multi-scope sms)
                                                   (let ([ph (shifted-multi-scope-phase sms)])
                                                     (if (shifted-to-label-phase? ph)
                                                         (shifted-to-label-phase-from ph)
                                                         (phase- ph phase)))))))

(define (find-max-scope scopes)
  (when (set-empty? scopes)
    (error "cannot bind in empty scope set"))
  (for/fold ([max-sc (set-first scopes)]) ([sc (in-set scopes)])
    (if (scope>? sc max-sc)
        sc
        max-sc)))

(define (add-binding-in-scopes! scopes sym binding #:just-for-nominal? [just-for-nominal? #f])
  (define max-sc (find-max-scope scopes))
  (define bt (binding-table-add (scope-binding-table max-sc) scopes sym binding just-for-nominal?))
  (set-scope-binding-table! max-sc bt)
  (clear-resolve-cache! sym))

(define (add-bulk-binding-in-scopes! scopes bulk-binding
                                     #:shadow-except [shadow-except #f])
  (define max-sc (find-max-scope scopes))
  (define bt (binding-table-add-bulk (scope-binding-table max-sc) scopes bulk-binding
                                     #:shadow-except shadow-except))
  (set-scope-binding-table! max-sc bt)
  (clear-resolve-cache!))

(define (syntax-any-scopes? s)
  (not (set-empty? (syntax-scopes s))))

(define (syntax-any-macro-scopes? s)
  (for/or ([sc (in-set (syntax-scopes s))])
    (eq? (scope-kind sc) 'macro)))

;; ----------------------------------------

;; Result is #f for no binding, `ambiguous-value` for an ambiguous binding,
;; or binding value
(define (resolve s phase
                 #:ambiguous-value [ambiguous-value #f]
                 #:exactly? [exactly? #f]
                 #:get-scopes? [get-scopes? #f] ; gets scope set instead of binding
                 ;; For resolving bulk bindings in `free-identifier=?` chains:
                 #:extra-shifts [extra-shifts null])
  (define sym (syntax-content s))
  (let fallback-loop ([smss (syntax-shifted-multi-scopes s)])
    (cond
     [(and (not exactly?)
           (not get-scopes?)
           (resolve-cache-get sym phase (syntax-scopes s) (fallback-first smss)))
      => (lambda (b)
           (cond
             [(eq? b '#:none)
              (if (fallback? smss)
                  (fallback-loop (fallback-rest smss))
                  #f)]
             [else b]))]
     [else
      (define scopes (scope-set-at-fallback s (fallback-first smss) phase))
      ;; As we look through all scopes, if we find two where neither
      ;; is a subset of the other, accumulate them into a list; maybe
      ;; we find a superset of both, later; if we end with a list,
      ;; then the binding is ambiguous. We expect that creating a list
      ;; of ambiguous scopes is rare relative to eventual success.
      (define-values (best-scopes best-binding)
        (for*/fold ([best-scopes #f] [best-binding #f])
                   ([sc (in-set scopes)]
                    [(b-scopes binding) (in-binding-table sym (scope-binding-table sc) s extra-shifts)]
                    #:when (and b-scopes binding (subset? b-scopes scopes)))
          (cond
           [(pair? best-scopes)
            ;; We have a list of scopes where none is a superset of the others
            (cond
             [(for/and ([amb-scopes (in-list best-scopes)])
                (subset? amb-scopes b-scopes))
              ;; Found a superset of all
              (values b-scopes binding)]
             [else
              ;; Accumulate another ambiguous set
              (values (cons b-scopes best-scopes) #f)])]
           [(not best-scopes)
            (values b-scopes binding)]
           [(subset? b-scopes best-scopes) ; can be `set=?` if binding is overridden
            (values best-scopes best-binding)]
           [(subset? best-scopes b-scopes)
            (values b-scopes binding)]
           [else
            ;; Switch to ambiguous mode
            (values (list best-scopes b-scopes) #f)])))
      (cond
       [(pair? best-scopes) ; => ambiguous
        (if (fallback? smss)
            (fallback-loop (fallback-rest smss))
            ambiguous-value)]
       [best-scopes
        (resolve-cache-set! sym phase (syntax-scopes s) (fallback-first smss) best-binding)
        (and (or (not exactly?)
                 (eqv? (set-count scopes)
                       (set-count best-scopes)))
             (if get-scopes?
                 best-scopes
                 best-binding))]
       [else
        (resolve-cache-set! sym phase (syntax-scopes s) (fallback-first smss) '#:none)
        (if (fallback? smss)
            (fallback-loop (fallback-rest smss))
            #f)])])))

;; ----------------------------------------

(define (bound-identifier=? a b phase)
  (and (eq? (syntax-e a)
            (syntax-e b))
       (equal? (syntax-scope-set a phase)
               (syntax-scope-set b phase))))
