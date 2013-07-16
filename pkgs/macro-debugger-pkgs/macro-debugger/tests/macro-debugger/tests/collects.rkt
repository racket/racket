#lang racket/base
(require racket/list
         racket/path
         macro-debugger/model/debug
         rackunit)
(provide collects-tests
         modules-for-test
         trace-modules)

;; loadlib : module-path symbol -> Deriv
(define (loadlib mod)
  (trace-module mod))

(define (test-libs name mods)
  (test-suite name
    (test-suite "Trace & Parse"
      (for ([m mods]) (test-lib/deriv m)))
    #|
    (test-suite "Reductions"
      (for ([m mods]) (test-lib/hide m hide-none-policy)))
    (test-suite "Standard hiding"
      (for ([m mods]) (test-lib/hide m standard-policy)))
    |#))

(define (test-lib/deriv m)
  (test-case (format "~s" m)
    (let ([deriv (loadlib m)])
      (check-pred deriv? deriv "Not a deriv")
      (check-pred ok-node? deriv "Expansion error"))))

(define (test-lib/hide m policy)
  (test-case (format "~s" m)
    (let ([deriv (loadlib m)])
      (check-steps deriv policy))))

(define (check-steps deriv policy)
  (define-values (steps binders uses stx exn)
    (parameterize ((macro-policy policy)) (reductions+ deriv)))
  (check-pred syntax? stx)
  (check-eq? exn #f)
  (check-true (list? steps) "Expected list for steps")
  #|(check-reduction-sequence steps)|#)

(define (check-reduction-sequence steps)
  ;; FIXME: add remarkstep
  (cond [(null? steps) (void)]
        [(and (pair? steps) (step? (car steps)))
         (check-reduction-sequence (cdr steps))]
        [(and (pair? steps) (misstep? (car steps)))
         (check-eq? (cdr steps) '() "Stuff after misstep")]
        [else (fail "Bad reduction sequence")]))

;; ----

(define (make-tracing-module-name-resolver omnr table)
  (case-lambda
    [(mod rel stx load?)
     (when load?
       (when (not rel)
         (hash-set! table mod #t))
       (when rel
         (let ([abs (rel+mod->mod rel mod)])
           (when abs (hash-set! table abs #t)))))
     (omnr mod rel stx load?)]
    [args
     (apply omnr args)]))

(define (rel+mod->mod rel mod)
  (let* ([rel (resolved-module-path-name rel)]
         [rel (if (pair? rel) (car rel) rel)])
    (if (pair? mod)
        #f  ;; give up on submodules for now; FIXME
        (let-values ([(base file dir?) (split-path rel)])
          (path->mod (simplify-path (build-path base mod)))))))

(define (path->mod path)
  (cond [(for/or ([c (current-library-collection-paths)]) (path->mod* path c))
         => (lambda (l)
              (string->symbol
               (path->string
                (path-replace-suffix (apply build-path l) #""))))]
        [else #f]))

(define (path->mod* path base)
  (let loop ([path (explode-path path)] [base (explode-path base)])
    (cond [(null? base) path]
          [(and (pair? path) (pair? base) (equal? (car path) (car base)))
           (loop (cdr path) (cdr base))]
          [else #f])))

(define (trace-modules mods)
  (define table (make-hash))
  (parameterize ((current-module-name-resolver
                  (make-tracing-module-name-resolver
                   (current-module-name-resolver)
                   table))
                 (current-namespace (make-base-namespace)))
    (for ([mod mods])
      (dynamic-require mod (void)))
    (let* ([loaded
            (hash-map table (lambda (k v) k))]
           [syms
            (for/list ([l loaded] #:when (symbol? l)) l)]
           [libs
            (for/list ([l loaded] #:when (and (pair? l) (eq? (car l) 'lib))) l)]
           [conv-libs
            (for/list ([l libs])
                      (string->symbol
                       (string-append
                        (apply string-append
                               (for/list ([d (cddr l)]) (string-append d "/")))
                        (path->string (path-replace-suffix (cadr l) #"")))))])
      (sort (remove-duplicates (append syms conv-libs))
            string<?
            #:key symbol->string))))

;; ----

(define modules-for-test
  (trace-modules '(racket/main typed/racket framework)))

(define collects-tests
  (test-libs "Trace collections" modules-for-test))
