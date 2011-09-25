#lang racket/base
(require racket/path
         syntax/modcode
         syntax/modresolve
         macro-debugger/model/trace)

;; --------

(provide (struct-out ref)
         mode->nat
         (struct-out imp))

;; A Ref is (ref phase id/#f identifier-binding Mode)
;; the def-mod, def-sym, etc parts of identifier-binding may be #f (eg, provide)
(struct ref (phase id mode binding))

;; A Mode is one of '(reference syntax-local-value quote-syntax disappeared-use provide)
(define (mode->nat mode)
  (case mode
    ((reference) 0)
    ((provide) 1)
    ((syntax-local-value) 2)
    ((quote-syntax) 3)
    ((disappeared-use) 4)
    (else (error 'mode->nat "bad mode: ~s" mode))))

;; An Imp is (imp mpi phase symbol phase Ref)
(struct imp (mod reqphase sym exp-phase ref))
;; interpretation: reference ref could be satisfied by
;;   (require (only (for-meta reqphase (just-meta exp-phase mod)) sym))

;; --------

(provide get-module-code/trace
         here-mpi?
         mpi->key
         mpi->list)

;; get-module-derivation : module-path -> (values compiled Deriv)
(define (get-module-code/trace path)
  (get-module-code (resolve-module-path path #f)
                   #:choose (lambda _ 'src)
                   #:compile (lambda (stx)
                               (let-values ([(stx deriv) (trace/result stx expand)])
                                 (values (compile stx) deriv)))))

;; here-mpi? : any -> boolean
(define (here-mpi? x)
  (and (module-path-index? x)
       (let-values ([(rel base) (module-path-index-split x)])
         (and (eq? rel #f) (eq? base #f)))))

(define (mpi->key x)
  (let ([l (mpi->list x)])
    (if (and (pair? l) (null? (cdr l)))
        (car l)
        l)))

(define (mpi->list x)
  (cond [(module-path-index? x)
         (let-values ([(rel base) (module-path-index-split x)])
           (if rel
               (cons rel (mpi->list base))
               null))]
        [(eq? x #f)
         null]
        [else
         (list x)]))

;; --------

(provide get-module-imports
         get-module-exports
         get-module-var-exports
         get-module-stx-exports
         get-module-all-exports)

(struct modinfo (imports var-exports stx-exports) #:prefab)

;; cache : hash[path/symbol => modinfo]
(define cache (make-hash))

;; get-module-info/no-cache : path -> modinfo
(define (get-module-info/no-cache resolved)
  (let ([compiled (get-module-code resolved)])
    (let-values ([(imports) (module-compiled-imports compiled)]
                 [(var-exports stx-exports) (module-compiled-exports compiled)])
      (parameterize ((current-directory (path-only resolved)))
        (force-all-mpis (cons var-exports stx-exports)))
      (modinfo imports var-exports stx-exports))))

;; get-module-info : (or module-path module-path-index) -> modinfo
(define (get-module-info mod)
  (let ([resolved (resolve mod)])
    (hash-ref! cache resolved (lambda () (get-module-info/no-cache resolved)))))

;; resolve : (or module-path module-path-index) -> path
(define (resolve mod)
  (cond [(module-path-index? mod)
         (resolved-module-path-name (module-path-index-resolve mod))]
        [else (resolve-module-path mod #f)]))

(define (get-module-imports path)
  (modinfo-imports (get-module-info path)))
(define (get-module-var-exports path)
  (modinfo-var-exports (get-module-info path)))
(define (get-module-stx-exports path)
  (modinfo-stx-exports (get-module-info path)))
(define (get-module-exports path)
  (let ([info (get-module-info path)])
    (values (modinfo-var-exports info) (modinfo-stx-exports info))))
(define (get-module-all-exports path)
  (append (get-module-var-exports path)
          (get-module-stx-exports path)))

(define (force-all-mpis x)
  (let loop ([x x])
    (cond [(pair? x)
           (loop (car x))
           (loop (cdr x))]
          [(module-path-index? x)
           ;; uses current-directory, hopefully
           (module-path-index-resolve x)]
          [else (void)])))
