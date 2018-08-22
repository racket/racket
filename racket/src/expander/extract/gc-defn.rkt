#lang racket/base
(require racket/list
         "../host/correlate.rkt"
         "../common/set.rkt"
         "../compile/side-effect.rkt"
         "../compile/known.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "symbol.rkt"
         "defn.rkt"
         "defn-known.rkt"
         "known-primitive.rkt")

(provide garbage-collect-definitions)

(define (garbage-collect-definitions linklet-expr
                                     #:disallows disallows)
  (log-status "Removing unused definitions...")

  (define disallow-ht (for/hasheq ([s (in-list disallows)])
                        (values s #t)))

  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define used-syms (make-hasheq))

  ;; See "../compile/known.rkt" for the meaning of
  ;; values in `seen-defns`
  (define seen-defns (make-hasheq))
  (register-known-primitives! seen-defns)

  ;; Map symbols to definition right-hand sides
  (define sym-to-rhs (make-hasheq))
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (for ([sym (in-list (defn-syms e))])
        (hash-set! sym-to-rhs sym (defn-rhs e)))]))

  ;; To track dependencies for reporting
  (define use-deps (make-hasheq))
  (define (track-and-check-disallowed! sym used-by)
    (when (hash-ref disallow-ht sym #f)
      (apply raise-arguments-error
             'flatten "disallowed identifier's definition preserved"
             "identifier" sym
             (let loop ([used-by used-by])
               (cond
                 [(not used-by) null]
                 [else
                  (or (and (list? used-by)
                           (for/or ([used-by (in-list used-by)])
                             (define next (hash-ref use-deps used-by #f))
                             (and next
                                  (list* "due to" used-by
                                         (loop next)))))
                      (list* "due to" used-by
                             (loop (hash-ref use-deps used-by #f))))]))))
    (hash-set! use-deps sym used-by))

  ;; A "mark"-like traversal of an expression:
  (define (set-all-used! e used-by)
    (for ([sym (in-set (all-used-symbols e))])
      (unless (hash-ref used-syms sym #f)
        (hash-set! used-syms sym #t)
        (track-and-check-disallowed! sym used-by)
        (set-all-used! (hash-ref sym-to-rhs sym #f) sym))))

  ;; Helper to check for side-effects at a definition
  (define (defn-side-effects? e)
    (any-side-effects? (defn-rhs e)
                       (length (defn-syms e))
                       #:known-defns seen-defns))

  ;; Mark each body form, delaying the righthand side of definitions
  ;; if the definition has no side-effect
  (let loop ([body body])
    (cond
     [(null? body) (void)]
     [(defn? (car body))
      (define defn (car body))
      (cond
        [(defn-side-effects? defn)
        ;; Right-hand side has an effect, so keep the
        ;; definition and mark everything as used:
        (for ([sym (in-list (defn-syms defn))])
          (unless (hash-ref used-syms sym #f)
            (track-and-check-disallowed! sym '#:rhs-effect)
            (hash-set! used-syms sym #t)))
        (set-all-used! (defn-rhs defn) (defn-syms defn))
        ;; Afterward, these identifiers are defined.
        ;; (It's ok if delayed types refer to these,
        ;; because they're apparently used later if they're
        ;; still delayed.)
        (for ([sym (in-list (defn-syms defn))])
          (hash-set! seen-defns sym (known-defined)))]
       [else
        ;; The definition itself doesn't have a side effect, so don't
        ;; mark it as used right away, and delay analysis to make it
        ;; independent of order within a group without side effects
        (define thunk
          (known-defined/delay
           (lambda ()
             (for ([sym (in-list (defn-syms defn))])
               (hash-set! seen-defns sym (known-defined)))
             (add-defn-known! seen-defns
                              (defn-syms defn)
                              (defn-rhs defn)))))
        (for ([sym (in-list (defn-syms defn))])
          (hash-set! seen-defns sym thunk))])
      (loop (cdr body))]
     [else
      (set-all-used! (car body) '#:effect)
      (loop (cdr body))]))

  ;; Mark each export:
  (for ([ex+sym (in-list (bootstrap:s-expr-linklet-exports+locals linklet-expr))])
    (set-all-used! (cdr ex+sym) '#:export))

  (define can-remove-count
    (for/sum ([e (in-list body)])
      (cond
       [(defn? e)
        (if (for/or ([sym (in-list (defn-syms e))])
              (hash-ref used-syms sym #f))
            0
            (length (defn-syms e)))]
       [else 0])))
  (log-status "Can remove ~s of ~s defined names, keeping ~s"
              can-remove-count
              (hash-count sym-to-rhs)
              (- (hash-count sym-to-rhs) can-remove-count))

  (define new-body
    (for/list ([e (in-list body)]
               #:when (or (not (defn? e))
                          (for/or ([sym (in-list (defn-syms e))])
                            (hash-ref used-syms sym #f))))
      e))

  (append (take linklet-expr 3)
          new-body))
