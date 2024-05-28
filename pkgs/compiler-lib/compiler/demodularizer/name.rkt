#lang racket/base
(require racket/match
         "linklet.rkt"
         "run.rkt"
         "import.rkt"
         "remap.rkt"
         (only-in racket/linklet linklet-body-reserved-symbol?))

(provide select-names
         find-name)

(define (select-names phase-runs)
  (define (make-table make)
    (for/hasheqv ([phase (in-hash-keys phase-runs)])
      (values phase (make))))
  (define (phase-map table f)
    (for/hasheqv ([(phase v) (in-hash table)])
      (values phase (f v))))

  ;; Don't need a per-root-phase table of names, because
  ;; we'll pick a consistent name for a given module and phase-level
  ;; no matter which root phase it's in
  (define names (make-hash)) ; path/submod+phase+sym -> symbol
  (define used-names (make-hasheq))

  (define phase-internals (make-table (lambda () (box '()))))
  (define phase-lifts (make-table (lambda () (box '()))))
  (define phase-imports (make-table make-hash)) ; root-phase -> path/submod+phase -> list-of-sym
  (define phase-imports-done (make-table make-hash)) ; root-phase -> path/submod+phase -> set-of-sym

  (define done (make-hash))

  ;; choose names in phase 0, first, to bias name choice toward that phase
  (for ([root-phase (cons 0 (remv 0 (hash-keys phase-runs)))])
    (define runs (hash-ref phase-runs root-phase))
    (define lifts (hash-ref phase-lifts root-phase))
    (define internals (hash-ref phase-internals root-phase))
    (define imports (hash-ref phase-imports root-phase))
    (define imports-done (hash-ref phase-imports-done root-phase))
    
    ;; Reserve the syntax-literals and transformer-register names:
    (define reserved-names '(.get-syntax-literal!
                             .set-transformer!))
    (for ([name (in-list reserved-names)])
      (hash-set! used-names name #t))

    (define (pick-name name)
      (let loop ([try-name name] [i 0])
        (cond
          [(or (linklet-body-reserved-symbol? try-name)
               (hash-ref used-names try-name #f))
           (let ([i (add1 i)])
             (loop (string->symbol (format "~a_~a" name i)) i))]
          [else
           (hash-set! used-names try-name #t)
           try-name])))
    
    (for ([r (in-list (reverse runs))]) ; biases names to starting module
      (define linkl (run-linkl r))
      (define meta-linkl (run-meta-linkl r))
      (define portal-stxes (run-portal-stxes r))
      (define path/submod+phase (cons (run-path/submod r) (run-phase r)))

      ;; Same linklet can be used (as shifted) in multiple root phases,
      ;; so check that we haven't covered this one already:
      (unless (hash-ref done path/submod+phase #f)
        (hash-set! done path/submod+phase #t)
        
        ;; Process local definitions, first
        (define (select-names! name-list category)
          (for ([name (in-list name-list)])
            (unless (hash-ref names (cons path/submod+phase name) #f)
              (define new-name (pick-name name))
              (hash-set! names (cons path/submod+phase name) new-name)
              (when category
                (set-box! category (cons new-name (unbox category)))))))
        
        (select-names! (linklet*-exports linkl) internals)
        (select-names! (linklet*-internals linkl) internals)
        (select-names! (linklet*-lifts linkl) lifts)

        (when meta-linkl
          (remap-names (linklet*-body meta-linkl)
                       (lambda (name) name)
                       #:application-hook
                       (lambda (rator rands remap)
                         (cond
                           [(eq? rator '.set-transformer!)
                            (match rands
                              [`((quote ,name) ,_)
                               (select-names! (list name) #f)]
                              [_ (error "unrecognized transformer registration")])]))))

        (select-names! (hash-keys portal-stxes) #f)))

    ;; Record any imports that will remain as imports; anything
    ;; not yet mapped must be a leftover import
    (for ([r (in-list runs)])
      (define linkl (run-linkl r))
      (for ([import-names (in-list (linklet*-importss linkl))]
            [import-internal-names (in-list (linklet*-internal-importss linkl))]
            [import-shapes (in-list (linklet*-import-shapess linkl))]
            [use (in-list (run-uses r))])
        (for ([name (in-list import-names)]
              [internal-name (in-list import-internal-names)]
              [shape (in-list import-shapes)])
          (define n (hash-ref names (cons use name) #f))
          (unless (symbol? n)
            (define done-names (hash-ref imports-done use #hasheq()))
            (unless (hash-ref done-names name #f)
              (hash-set! imports use (cons name (hash-ref imports use null)))
              (hash-set! imports-done use (hash-set done-names name #t))))
          (unless n
            (define new-name ; used for S-expression mode
              (if (memq internal-name reserved-names)
                  internal-name
                  (pick-name internal-name)))
            (hash-set! names (cons use name) (import name shape new-name #f)))))))

  (values names (phase-map phase-internals unbox) (phase-map phase-lifts unbox) phase-imports))

(define (find-name names use name)
  (hash-ref names (cons use name)))
