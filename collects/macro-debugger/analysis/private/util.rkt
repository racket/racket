#lang racket/base
(require syntax/modcode
         syntax/modresolve
         macro-debugger/model/trace)

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
         get-module-stx-exports)

(struct modinfo (imports var-exports stx-exports) #:prefab)

;; cache : hash[path/symbol => modinfo]
(define cache (make-hash))

(define (get-module-info/no-cache resolved)
  (let ([compiled (get-module-code resolved)])
    (let-values ([(imports) (module-compiled-imports compiled)]
                 [(var-exports stx-exports) (module-compiled-exports compiled)])
      (modinfo imports var-exports stx-exports))))

(define (get-module-info path)
  (let ([resolved (resolve-module-path path #f)])
    (hash-ref! cache resolved (lambda () (get-module-info/no-cache resolved)))))

(define (get-module-imports path)
  (modinfo-imports (get-module-info path)))
(define (get-module-var-exports path)
  (modinfo-var-exports (get-module-info path)))
(define (get-module-stx-exports path)
  (modinfo-stx-exports (get-module-info path)))
(define (get-module-exports path)
  (let ([info (get-module-info path)])
    (values (modinfo-var-exports info) (modinfo-stx-exports info))))

