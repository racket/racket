#lang racket/base
(require compiler/zo-structs
         racket/match
         "run.rkt"
         "name.rkt"
         "import.rkt"
         "remap.rkt"
         "linklet.rkt")

(provide merge-linklets
         current-maximum-phase)

(define current-maximum-phase (make-parameter 1))

(define (merge-linklets phase-runs names phase-internals phase-lifts phase-imports)
  ;; Accumulate syntax objects, which span phases. If would be nice if we didn't
  ;; keep syntax objects in expressions that are later pruned,
  ;; but we'll leave that as a future improvement.
  (define stx-objs (make-hasheq))
  (define stx-obj-map (make-hasheqv))
  (define (remap-syntax-object! r i)
    (define stx-vec (run-stx-vec r))
    (cond
      [stx-vec
       (define stx (vector-ref (run-stx-vec r) i))
       (or (hash-ref stx-objs stx #f)
           (let ([j (hash-count stx-objs)])
             (hash-set! stx-objs stx j)
             (hash-set! stx-obj-map j stx)
             j))]
      [else 0]))
  
  (define phase-body (make-hasheqv))
  (define phase-first-internal-pos (make-hasheqv))
  (define phase-new-internals (make-hasheqv))
  (define phase-import-keys (make-hasheqv))
  (define phase-more (make-hasheqv))
  (define phase-defined-names (make-hasheqv))
  (define all-portal-stxes (make-hasheqv))

  (define linkl-mode #f)

  (define max-root-phase (current-maximum-phase))

  (for ([(root-phase runs) (in-hash phase-runs)]
        #:when (<= 0 root-phase (or max-root-phase +inf.0)))
    (define internals (hash-ref phase-internals root-phase))
    (define lifts (hash-ref phase-lifts root-phase))
    (define imports (hash-ref phase-imports root-phase))
    (define defined-names (or (hash-ref phase-defined-names root-phase #f)
                              (let ([ht (make-hasheq)])
                                (hash-set! phase-defined-names root-phase ht)
                                ht)))

    (define (syntax-literals-import? path/submod+phase)
      (eq? (cdr path/submod+phase) 'syntax-literals))
    (define (transformer-register-import? path/submod+phase)
      (eq? (cdr path/submod+phase) 'transformer-register))

    ;; Pick an order for the remaining imports:
    (define import-keys (for/list ([path/submod+phase (in-hash-keys imports)]
                                   ;; References to a 'syntax-literals "phase" are
                                   ;; references to the implicit syntax-literals
                                   ;; module; drop those:
                                   #:unless (or (syntax-literals-import? path/submod+phase)
                                                (transformer-register-import? path/submod+phase)))
                          path/submod+phase))
    
    (define any-syntax-literals?
      (for/or ([path/submod+phase (in-hash-keys imports)])
        (syntax-literals-import? path/submod+phase)))
    (define any-transformer-registers?
      (for/or ([path/submod+phase (in-hash-keys imports)])
        (transformer-register-import? path/submod+phase)))
    (define syntax-literals-pos 1)
    (define transformer-register-pos (+ (if any-syntax-literals? 1 0)
                                        syntax-literals-pos))
    (define import-counter (+ (if any-transformer-registers? 1 0)
                              transformer-register-pos))

    ;; Map each remaining import to its position, meanwhile getting a list
    ;; of lists of external--internal name lists
    (define ordered-importss
      (for/list ([key (in-list import-keys)])
        (define ordered-imports (hash-ref imports key))
        (for/list ([name (in-list ordered-imports)])
          (define i (hash-ref names (cons key name)))
          (set-import-pos! i import-counter)
          (set! import-counter (add1 import-counter))
          (list name (import-int-name i)))))
    ;; Keep all the same import shapes
    (define import-shapess
      (for/list ([key (in-list import-keys)])
        (for/list ([name (in-list (hash-ref imports key))])
          (import-shape (hash-ref names (cons key name))))))

    ;; Map all syntax-literal references to the same import.
    ;; We'll update each call to access a syntax object to use a suitable
    ;; vector index.
    (for ([(path/submod+phase imports) (in-hash imports)]
          #:when (syntax-literals-import? path/submod+phase)
          [name (in-list imports)])
      (define i (hash-ref names (cons path/submod+phase name)))
      (set-import-pos! i syntax-literals-pos))

    ;; Map the transformer-register import, if any
    (let* ([path/submod+phase '(#%transformer-register . transformer-register)]
           [imports (hash-ref imports path/submod+phase null)])
      (for ([name (in-list imports)])
        (define i (hash-ref names (cons path/submod+phase name)))
        (set-import-pos! i transformer-register-pos)))

    ;; Map internals and lifts to positions
    (define first-internal-pos import-counter)
    (define new-internals (make-hasheq))
    (define positions
      (for/hash ([name (in-list (append internals lifts))]
                 [i (in-naturals first-internal-pos)])
        (hash-set! new-internals name #t)
        (values name i)))

    ;; For each linklet that we merge, make a mapping from
    ;; the linklet's old position to new names (which can
    ;; then be mapped to new positions):
    (define (make-position-mapping r)
      (define h (make-hasheqv))
      (define p (make-hasheq))
      (define linkl (run-linkl r))
      (define importss (linklet*-importss linkl))
      (define internal-importss (linklet*-internal-importss linkl))
      (define pos 1)
      (for ([imports (in-list importss)]
            [internal-imports (in-list internal-importss)]
            [use (in-list (run-uses r))])
        (for ([name (in-list imports)]
              [internal-name (in-list internal-imports)])
          (hash-set! h pos (find-name names use name))
          (hash-set! p internal-name pos)
          (set! pos (add1 pos))))
      (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
      (define internal-names (append (linklet*-internals linkl)
                                     (linklet*-lifts linkl)))
      (for ([name (in-list (append (linklet*-exports linkl)
                                   internal-names))]
            [internal-name (in-list (append (linklet*-internal-exports linkl)
                                            internal-names))]
            [pos (in-naturals pos)])
        (hash-set! h pos (find-name names path/submod+phase name))
        (hash-set! p internal-name pos))
      (values h p))

    ;; Do we need the implicit initial variable for `(#%variable-reference)`?
    ;; The slot will be reserved whether we use it or not, but the
    ;; slot is not necessarily initialized if we don't need it.
    (define saw-zero-pos-toplevel? #f)
    
    (define body
      (apply
       append
       (for/list ([r (in-list runs)])
         (define-values (pos-to-name/import local-name-to-pos) (make-position-mapping r))
         (define linkl (run-linkl r))
         (cond
           [(linkl? linkl)
            (define (remap-toplevel-pos pos)
              (cond
                [(zero? pos)
                 ;; Implicit variable for `(#%variable-reference)` stays in place:
                 (set! saw-zero-pos-toplevel? #t)
                 0]
                [else
                 (define new-name/import (hash-ref pos-to-name/import pos))
                 (if (import? new-name/import)
                     (import-pos new-name/import)
                     (hash-ref positions new-name/import))]))
            (when (eq? linkl-mode 's-exp) (error 'demodularize "inconsistent linklet representations"))
            (set! linkl-mode 'linkl)
            (remap-positions (linkl-body linkl)
                             remap-toplevel-pos
                             #:application-hook
                             (lambda (rator rands remap)
                               ;; Check for a `(.get-syntax-literal! '<pos>)` call
                               ;; or a `(.set-transformer! '<sym> <expr>)` call
                               (cond
                                 [(and (toplevel? rator)
                                       (let ([i (hash-ref pos-to-name/import (toplevel-pos rator))])
                                         (and (import? i)
                                              i)))
                                  => (lambda (i)
                                       (cond
                                         [(and any-syntax-literals?
                                               (eqv? syntax-literals-pos (import-pos i)))
                                          ;; This is a `(.get-syntax-literal! '<pos>)` call
                                          (unless (and (= 1 (length rands))
                                                       (exact-nonnegative-integer? (car rands)))
                                            (error "unrecognized syntax-literal access"))
                                          (application (remap rator)
                                                       (list (remap-syntax-object! r (car rands))))]
                                         [(and any-transformer-registers?
                                               (eqv? transformer-register-pos (import-pos i)))
                                          ;; This is a `(.set-transformer! '<sym> <expr>)` call
                                          (void)]
                                         [else #f]))]
                                 [else #f])))]
           [(linklet*-body linkl)
            => (lambda (body)
                 (when (eq? linkl-mode 'linkl) (error 'demodularize "inconsistent linklet representations"))
                 (set! linkl-mode 's-exp)
                 #;
                 (when (eqv? root-phase 1)
                   (when (and (path? (run-path/submod r))
                              (regexp-match? #rx"rhombus/private/comparable.rkt" (run-path/submod r)))
                     (local-require racket/pretty)
                     (pretty-print linkl)))
                 ;; We can work in terms of names instead of positions
                 (define importss (linklet*-importss linkl))
                 (define (remap-name name)
                   (define pos (hash-ref local-name-to-pos name #f))
                   (cond
                     [(not pos) ; => primitive
                      name]
                     [else
                      (define n (hash-ref pos-to-name/import pos))
                      (cond
                        [(import? n) (import-int-name n)]
                        [else n])]))
                 (define (remap-defined-name name)
                   (define new-name (remap-name name))
                   (hash-set! defined-names new-name #t)
                   new-name)
                 (remap-names body
                              remap-name
                              #:remap-defined-name remap-defined-name
                              #:application-hook
                              (lambda (rator rands remap)
                                ;; Check for a `(.get-syntax-literal! '<pos>)` call
                                ;; or a `(.set-transformer! '<sym> <expr>)` call
                                (cond
                                  [(eq? rator '.get-syntax-literal!)
                                   (unless (and (= 1 (length rands))
                                                (exact-nonnegative-integer? (car rands)))
                                     (error "unrecognized syntax-literal access"))
                                   `(,(remap rator)
                                     ,(remap-syntax-object! r (car rands)))]
                                  [(eq? rator '.set-transformer!)
                                   (cond
                                     [(eqv? root-phase 0)
                                      `(void)]
                                     [else
                                      (match rands
                                        [`((quote ,name) ,rhs)
                                         (define path/submod+phase (cons (run-path/submod r) (sub1 (run-phase r))))
                                         (define new-name
                                           (or (hash-ref names (cons path/submod+phase name) #f)
                                               (error 'merge "cannot find name for transformer definition: ~s"
                                                      name)))
                                         `(,(remap rator)
                                           ',new-name
                                           ,(remap rhs))])])]
                                  [else #f]))))]))))

    (define portal-stxes
      (for/fold ([ht #hasheq()]) ([r (in-list runs)])
        (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
        (for/fold ([ht ht]) ([(name pos) (in-hash (run-portal-stxes r))])
          (define new-name (hash-ref names (cons path/submod+phase name)))
          (hash-set ht new-name (remap-syntax-object! r pos)))))

    (hash-set! phase-body root-phase body)
    (hash-set! phase-first-internal-pos root-phase first-internal-pos)
    (hash-set! phase-new-internals root-phase new-internals)
    (hash-set! phase-import-keys root-phase import-keys)
    (hash-set! all-portal-stxes root-phase portal-stxes)
    (hash-set! phase-more root-phase (list ordered-importss
                                           import-shapess
                                           any-syntax-literals?
                                           any-transformer-registers?
                                           saw-zero-pos-toplevel?)))

  (define new-stx-vec
    (for/vector ([i (in-range (hash-count stx-obj-map))])
      (hash-ref stx-obj-map i)))

  (values phase-body
          phase-first-internal-pos
          phase-new-internals
          linkl-mode
          phase-import-keys
          (for/hasheqv ([(phase ht) (in-hash all-portal-stxes)]) (values phase ht))
          phase-defined-names
          ;; Communicates into to `wrap-bundle`:
          (lambda ()
            (values phase-runs
                    phase-more
                    new-stx-vec))))
