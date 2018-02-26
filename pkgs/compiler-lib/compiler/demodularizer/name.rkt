#lang racket/base
(require compiler/zo-structs
         "run.rkt"
         "import.rkt")

(provide select-names
         find-name)

(define (select-names runs)
  (define names (make-hash)) ; path/submod+phase+sym -> symbol
  (define used-names (make-hasheq))
  (define internals (box '()))
  (define lifts (box '()))
  (define imports (make-hash)) ; path/submod+phase -> list-of-sym

  ;; Reserve the syntax-literals and transformer-register names:
  (hash-set! used-names '.get-syntax-literal! #t)
  (hash-set! used-names '.set-transformer! #t)

  (define (pick-name name)
    (let loop ([try-name name] [i 0])
      (cond
        [(hash-ref used-names try-name #f)
         (let ([i (add1 i)])
           (loop (string->symbol (format "~a_~a" name i)) i))]
        [else
         (hash-set! used-names try-name #t)
         try-name])))
  
  (for ([r (in-list (reverse runs))]) ; biases names to starting module
    (define linkl (run-linkl r))
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))

    ;; Process local definitions, first
    (define (select-names! name-list category)
      (for ([name (in-list name-list)])
        (define new-name (pick-name name))
        (hash-set! names (cons path/submod+phase name) new-name)
        (set-box! category (cons new-name (unbox category)))))

    (select-names! (linkl-exports linkl) internals)
    (select-names! (linkl-internals linkl) internals)
    (select-names! (linkl-lifts linkl) lifts))

  ;; Record any imports that will remain as imports; anything
  ;; not yet mapped must be a leftover import
  (for ([r (in-list runs)])
    (define linkl (run-linkl r))
    (for ([import-names (in-list (linkl-importss linkl))]
          [import-shapes (in-list (linkl-import-shapess linkl))]
          [use (in-list (run-uses r))])
      (for ([name (in-list import-names)]
            [shape (in-list import-shapes)])
        (unless (hash-ref names (cons use name) #f)
          (hash-set! imports use (cons name (hash-ref imports use null)))
          (hash-set! names (cons use name) (import name shape #f))))))

  (values names (unbox internals) (unbox lifts) imports))

(define (find-name names use name)
  (hash-ref names (cons use name)))
