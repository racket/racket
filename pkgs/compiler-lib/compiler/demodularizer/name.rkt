#lang racket/base
(require "linklet.rkt"
         "run.rkt"
         "import.rkt"
         (only-in racket/linklet linklet-body-reserved-symbol?))

(provide select-names
         find-name)
        
(define (select-names runs)
  (define names (make-hash)) ; path/submod+phase+sym -> symbol
  (define used-names (make-hasheq))
  (define internals (box '()))
  (define lifts (box '()))
  (define imports (make-hash)) ; path/submod+phase -> list-of-sym

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
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))

    ;; Process local definitions, first
    (define (select-names! name-list category)
      (for ([name (in-list name-list)])
        (define new-name (pick-name name))
        (hash-set! names (cons path/submod+phase name) new-name)
        (set-box! category (cons new-name (unbox category)))))

    (select-names! (linklet*-exports linkl) internals)
    (select-names! (linklet*-internals linkl) internals)
    (select-names! (linklet*-lifts linkl) lifts))

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
        (unless (hash-ref names (cons use name) #f)
          (hash-set! imports use (cons name (hash-ref imports use null)))
          (define new-name ; used for S-expression mode
            (if (memq internal-name reserved-names)
                internal-name
                (pick-name internal-name)))
          (hash-set! names (cons use name) (import name shape new-name #f))))))

  (values names (unbox internals) (unbox lifts) imports))

(define (find-name names use name)
  (hash-ref names (cons use name)))
