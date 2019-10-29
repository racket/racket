#lang racket/base
(require compiler/zo-structs
         "run.rkt"
         "name.rkt"
         "import.rkt"
         "remap.rkt")

(provide merge-linklets)

(define (merge-linklets runs names internals lifts imports)
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

  ;; Map each remaining import to its position
  (define ordered-importss
    (for/list ([key (in-list import-keys)])
      (define ordered-imports (hash-ref imports key))
      (for ([name (in-list ordered-imports)])
        (define i (hash-ref names (cons key name)))
        (set-import-pos! i import-counter)
        (set! import-counter (add1 import-counter)))
      ordered-imports))
  ;; Keep all the same import shapes
  (define import-shapess
    (for/list ([key (in-list import-keys)])
      (for/list ([name (in-list (hash-ref imports key))])
        (import-shape (hash-ref names (cons key name))))))

  ;; Map all syntax-literal references to the same import.
  ;; We could update each call to the access to use a suitable
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
  (define positions
    (for/hash ([name (in-list (append internals lifts))]
               [i (in-naturals first-internal-pos)])
      (values name i)))

  ;; For each linklet that we merge, make a mapping from
  ;; the linklet's old position to new names (which can
  ;; then be mapped to new positions):
  (define (make-position-mapping r)
    (define h (make-hasheqv))
    (define linkl (run-linkl r))
    (define importss (linkl-importss linkl))
    (define pos 1)
    (for ([imports (in-list importss)]
          [use (in-list (run-uses r))])
      (for ([name (in-list imports)])
        (hash-set! h pos (find-name names use name))
        (set! pos (add1 pos))))
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
    (for ([name (in-list (append (linkl-exports linkl)
                                 (linkl-internals linkl)
                                 (linkl-lifts linkl)))]
          [pos (in-naturals pos)])
      (hash-set! h pos (find-name names path/submod+phase name)))
    h)

  ;; Do we need the implicit initial variable for `(#%variable-reference)`?
  ;; The slot will be reserved whether we use it or not, but the
  ;; slot is not necessarily initialized if we don't need it.
  (define saw-zero-pos-toplevel? #f)

  (define body
    (apply
     append
     (for/list ([r (in-list runs)])
       (define pos-to-name/import (make-position-mapping r))
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

       (remap-positions (linkl-body (run-linkl r))
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
                                     (application (remap rator)
                                                  ;; To support syntax objects, change the offset
                                                  rands)]
                                    [(and any-transformer-registers?
                                          (eqv? transformer-register-pos (import-pos i)))
                                     ;; This is a `(.set-transformer! '<sym> <expr>)` call
                                     (void)]
                                    [else #f]))]
                            [else #f]))))))

  (values body
          first-internal-pos
          ;; Communicates into to `wrap-bundle`:
          (lambda ()
            (values runs
                    import-keys
                    ordered-importss
                    import-shapess
                    any-syntax-literals?
                    any-transformer-registers?
                    saw-zero-pos-toplevel?))))
