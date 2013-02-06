#lang racket/base
(require racket/path
         racket/list
         racket/match
         syntax/modcode
         syntax/modresolve
         syntax/modcollapse
         macro-debugger/model/trace)

;; --------

(provide (struct-out ref)
         mode->nat
         (struct-out imp)
         ref->imp)

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

;; ref->imp : Ref -> Imp
;; Assumes id gotten from nom-mod, etc.
(define (ref->imp r)
  (match (ref-binding r)
    [(list _dm _ds nom-mod nom-sym _dp imp-shift nom-orig-phase)
     (imp nom-mod imp-shift nom-sym nom-orig-phase r)]))

;; --------

(provide get-module-code/trace
         here-mpi?
         mpi->key
         mpi->list
         mpi-list->module-path)

;; get-module-derivation : module-path -> (values compiled Deriv)
(define (get-module-code/trace modpath)
  (let-values ([(path subs)
                (match (resolve-module-path modpath #f)
                  [`(submod ,path . ,subs) (values path subs)]
                  [path (values path null)])])
    (get-module-code path
                     #:submodule-path subs
                     #:choose (lambda _ 'src)
                     #:compile (lambda (stx)
                                 (let-values ([(stx deriv) (trace/result stx expand)])
                                   (values (compile stx) deriv))))))

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

(define (mpi-list->module-path mpi-list)
  (let* ([mpi*
          (let loop ([mpi #f] [mpi-list mpi-list])
            (cond [mpi
                   (let-values ([(mod base) (module-path-index-split mpi)])
                     (cond [mod (module-path-index-join mod (loop base mpi-list))]
                           [else (loop #f mpi-list)]))]
                  [(pair? mpi-list)
                   (loop (car mpi-list) (cdr mpi-list))]
                  [else #f]))]
         [collapsed
          (let loop ([mpi mpi*])
            (cond [mpi 
                   (let-values ([(mod base) (module-path-index-split mpi)])
                     (cond [mod
                            (collapse-module-path mod (lambda () (loop base)))]
                           [else (build-path 'same)]))]
                  [else (build-path 'same)]))])
    (let simplify ([collapsed collapsed])
      (match collapsed
        [(list* 'submod base subs)
         (list* 'submod (simplify base) subs)]
        [(list 'lib str)
         (cond [(regexp-match? #rx"\\.rkt$" str)
                (let* ([no-suffix (path->string (path-replace-suffix str ""))]
                       [no-main
                        (cond [(regexp-match #rx"^([^/]+)/main$" no-suffix)
                               => cadr]
                              [else no-suffix])])
                  (string->symbol no-main))]
               [else collapsed])]
        [(? path?)
         (path->string (simplify-path collapsed #f))] ;; to get rid of "./" at beginning
        [_ collapsed]))))

;; --------

(provide get-module-imports
         get-module-exports
         get-module-var-exports
         get-module-stx-exports
         get-module-all-exports)

(struct modinfo (imports var-exports stx-exports imports/r var-exports/r stx-exports/r)
  #:prefab)

;; cache : hash[path/symbol/list => modinfo]
(define cache (make-hash))

;; get-module-code* : (U path (cons path (listof symbol))) -> compiled-module
(define (get-module-code* resolved)
  (let* ([path (if (pair? resolved) (car resolved) resolved)]
         [subs (if (pair? resolved) (cdr resolved) null)]
         [code (get-module-code path)])
    (for/fold ([code code]) ([submod-name (in-list subs)])
      (define (choose-submod? sub)
        (let* ([sub-name (module-compiled-name sub)])
          (equal? (last sub-name) submod-name)))
      (or (for/or ([sub (in-list (module-compiled-submodules code #t))])
            (and (choose-submod? sub) sub))
          (for/or ([sub (in-list (module-compiled-submodules code #f))])
            (and (choose-submod? sub) sub))
          (error 'get-module-code* "couldn't get code for: ~s" resolved)))))

;; get-module-info/no-cache : path -> modinfo
(define (get-module-info/no-cache resolved)
  (let ([compiled (get-module-code* resolved)]
        [resolved-base (if (pair? resolved) (car resolved) resolved)])
    (let-values ([(imports) (module-compiled-imports compiled)]
                 [(var-exports stx-exports) (module-compiled-exports compiled)]
                 [(dir) (path-only (if (pair? resolved) (car resolved) resolved))])
      (parameterize ((current-directory dir)
                     (current-load-relative-directory dir))
        (force-all-mpis imports)
        (force-all-mpis (cons var-exports stx-exports)))
      (modinfo imports var-exports stx-exports
               (resolve-all-mpis imports resolved-base)
               (resolve-all-mpis var-exports resolved-base)
               (resolve-all-mpis stx-exports resolved-base)))))

;; get-module-info : (or module-path module-path-index) -> modinfo
(define (get-module-info mod)
  (let ([resolved (resolve mod)])
    (when #f
      (eprintf "fetch ~s => ~s\n"
               (if (module-path-index? mod) (cons 'mpi (mpi->list mod)) mod)
               resolved))
    (hash-ref! cache resolved (lambda () (get-module-info/no-cache resolved)))))

;; resolve : (or module-path resolved-module-path module-path-index)
;;        -> (U (U path symbol) (cons (U path symbol) (listof symbol)))
(define (resolve mod)
  (cond [(module-path-index? mod)
         (resolved-module-path-name (module-path-index-resolve mod))]
        [(resolved-module-path? mod)
         (resolved-module-path-name mod)]
        [else (resolve-module-path mod #f)]))

(define (get-module-imports path #:resolve? [resolve? #f])
  ((if resolve? modinfo-imports/r modinfo-imports) (get-module-info path)))
(define (get-module-var-exports path #:resolve? [resolve? #f])
  ((if resolve? modinfo-var-exports/r modinfo-var-exports) (get-module-info path)))
(define (get-module-stx-exports path #:resolve? [resolve? #f])
  ((if resolve? modinfo-stx-exports/r modinfo-stx-exports) (get-module-info path)))
(define (get-module-exports path #:resolve? [resolve? #f])
  (values (get-module-var-exports path #:resolve? resolve?)
          (get-module-stx-exports path #:resolve? resolve?)))
(define (get-module-all-exports path #:resolve? [resolve? #f])
  (append (get-module-var-exports path #:resolve? resolve?)
          (get-module-stx-exports path #:resolve? resolve?)))

(define (resolve-all-mpis x base)
  (let loop ([x x])
    (cond [(pair? x)
           (cons (loop (car x)) (loop (cdr x)))]
          [(module-path-index? x)
           (resolve-module-path-index x base)]
          [else x])))

(define (force-all-mpis x)
  (let loop ([x x])
    (cond [(pair? x)
           (loop (car x))
           (loop (cdr x))]
          [(module-path-index? x)
           ;; uses current-directory, hopefully
           (module-path-index-resolve x)]
          [else (void)])))
