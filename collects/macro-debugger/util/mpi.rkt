#lang racket/base
(require racket/match
         racket/string)

(provide mpi->list
         mpi->string
         self-mpi?)

;; mpi->list : module-path-index -> list
(define (mpi->list mpi)
  (cond [(module-path-index? mpi)
         (let-values ([(path relto) (module-path-index-split mpi)])
           (cond [(not path) null]
                 [(not relto) (list path)]
                 [else (cons path (mpi->list relto))]))]
        [(not mpi) null]
        [else (list mpi)]))

;; mpi->string : module-path-index -> string
(define (mpi->string mpi)
  (if (module-path-index? mpi)
      (let ([mps (mpi->list mpi)])
        (cond [(pair? mps)
               (string-join (map (lambda (x) (format "~s" x)) mps)
                            " <= ")]
              [(null? mps) "this module"]))
      (format "~s" mpi)))

;; self-mpi? : module-path-index -> bool
(define (self-mpi? mpi)
  (let-values ([(path relto) (module-path-index-split mpi)])
    (eq? path #f)))

;; --

(provide mpi->mpi-sexpr
         mpi-sexpr->mpi
         rmp->rmp-sexpr
         rmp-sexpr->rmp)

;; mp = module-path
;; mpi = module-path-index
;; rmp = resolved-module-path

;; An mpi-sexpr is one of
;;   (cons mp-sexpr mpi-sexpr)
;;   (list rmp-sexpr)
;;   (list #f)  ;; "self" module
;;   null

;; An rmp-sexpr is
;;   (list 'resolved path/symbol)

;; mpi->mpi-sexpr : mpi -> mpi-sexpr
(define (mpi->mpi-sexpr mpi)
  (cond [(module-path-index? mpi)
         (let-values ([(mod next) (module-path-index-split mpi)])
           (cons (mp->mp-sexpr mod) (mpi->mpi-sexpr next)))]
        [(resolved-module-path? mpi)
         (list (rmp->rmp-sexpr mpi))]
        [else null]))

;; mp->mp-sexpr : mp -> mp-sexpr
(define (mp->mp-sexpr mp)
  (if (path? mp)
      (if (absolute-path? mp)
          `(file ,(path->string mp))
          (path->string mp))
      mp))

;; mpi-sexpr->mpi : mpi-sexpr -> mpi
(define (mpi-sexpr->mpi sexpr)
  (match sexpr
    ['() #f]
    [(list (list 'resolved path))
     (rmp-sexpr->rmp path)]
    [(cons first rest)
     (module-path-index-join first (mpi-sexpr->mpi rest))]))

;; rmp->rmp-sexpr : rmp -> rmp-sexpr
(define (rmp->rmp-sexpr rmp)
  (list 'resolved (resolved-module-path-name rmp)))

;; rmp-sexpr->rmp : rmp-sexpr -> rmp
(define (rmp-sexpr->rmp sexpr)
  (make-resolved-module-path (cadr sexpr)))

;; ----

(provide mpi-sexpr->expanded-mpi-sexpr
         expanded-mpi-sexpr->mpi-sexpr

         mpi-frame->expanded-mpi-frame
         expanded-mpi-frame->mpi-frame

         expanded-mpi-sexpr->library
         absolute-expanded-mpi-frame?
         library-expanded-mpi-frame?)

;; An expanded-mpi-sexpr is (listof expanded-mpi-frame)

;; An expanded-mpi-frame is one of:
;;   (list 'LIB (listof string))
;;   (list 'PLANET (listof string) PackageSpec)
;;   (list 'FILE string)
;;      absolute file path (not relative)
;;   (list 'QUOTE symbol)
;;   (list 'SELF)
;;   (list 'REL (listof string))
;; The first 5 variants are considered "absolute" frames.
;; The first 2 variants are consider "library" frames.

;; mpi-sexpr->expanded-mpi-sexpr
(define (mpi-sexpr->expanded-mpi-sexpr sexpr)
  (map mpi-frame->expanded-mpi-frame sexpr))

;; mpi-frame->expanded-mpi-frame
(define (mpi-frame->expanded-mpi-frame sexpr)
  (match sexpr
    [#f
     `(SELF)]
    [`(quote ,mod)
     `(QUOTE ,mod)]
    [`(lib ,path)
     (cond [(symbol? path)
            (mpi-frame->expanded-mpi-frame path)]
           [(regexp-match? #rx"/" path)
            `(LIB ,(split-mods path))]
           [else
            `(LIB ,(list "mzlib" path))])]
    [`(lib ,path . ,more)
     `(LIB ,(split-mods path more))]
    [`(planet ,(? symbol? spec))
     (mpi-frame->expanded-mpi-frame (parse-planet-spec spec))]
    [`(planet ,path ,package . ,more)
     `(PLANET ,(split-mods path more) ,package)]
    [(? symbol? mod)
     `(LIB ,(split-mods* (symbol->string mod)))]
    [`(file ,path)
     (cond [(absolute-path? path)
            `(FILE ,path)]
           [else
            `(REL (split-mods path))])]
    [(? string? path)
     `(REL ,(split-mods path))]
    [`(resolved ,(? path? path))
     `(FILE ,path)]
    [`(resolved ,(? symbol? symbol))
     `(QUOTE ,symbol)]))

;; expanded-mpi-sexpr->mpi-sexpr
(define (expanded-mpi-sexpr->mpi-sexpr sexpr)
  (map expanded-mpi-frame->mpi-frame sexpr))

;; expanded-mpi-frame->mpi-frame
(define (expanded-mpi-frame->mpi-frame sexpr)
  (match sexpr
    [`(SELF)
     #f]
    [`(QUOTE ,mod)
     `(quote ,mod)]
    [`(LIB ,paths)
     `(lib ,(apply string-append (intersperse "/" paths)))]
    [`(PLANET ,paths ,package)
     `(planet ,(apply string-append (intersperse "/" paths)) ,package)]
    [`(FILE ,path)
     `(file ,path)]
    [`(REL ,paths)
     (apply string-append (intersperse "/" paths))]))

(define (parse-planet-spec spec-sym)
  (define spec (symbol->string spec-sym))
  (let ([m (regexp-match #rx"([^/]*)/([^:/]*)(?:[:]([^/]*))?(?:/(.*))?" spec)])
    (unless m (error "bad planet symbol" spec-sym))
    (let ([owner (cadr m)]
          [package (string-append (caddr m) ".plt")]
          [version (and (cadddr m) (parse-version (cadddr m)))]
          [path (list-ref m 4)])
      `(planet ,(string-append (or path "main") ".rkt")
               (,owner ,package . ,version)))))

(define (parse-version str)
  ;; FIXME!!!
  '())

(define (split-mods* path)
  (let ([mods (split-mods path)])
    (if (and (pair? mods) (null? (cdr mods)))
        (append mods (list "main.rkt"))
        mods)))

(define (split-mods path [more null])
  (append (apply append (map split-mods more))
          (regexp-split #rx"/" path)))

(define (flatten-mods more path)
  (path->string (apply build-path (append more (list path)))))

;; expanded-mpi-sexpr->library : expanded-mpi-sexpr -> expanded-mpi-frame
(define (expanded-mpi-sexpr->library sexpr0)
  (define (abs? link)
    (and (pair? link) (memq (car link) '(LIB PLANET))))
  (define (loop stack stacks)
    (cond [(pair? (cdr stack))
           (cons (car stack) (loop (cdr stack) stacks))]
          [(pair? stacks)
           (unless (eq? 'REL (car (car stacks)))
             (error 'expanded-mpi-sexpr->library
                    "internal error: absolute frame"))
           (loop (cadr (car stacks)) (cdr stacks))]
          [else stack]))
  (define sexpr1 (reverse (cut-to-absolute sexpr0)))
  (and (library-expanded-mpi-frame? (car sexpr1))
       `(,(car (car sexpr1))
         ,(loop (cadr (car sexpr1)) (cdr sexpr1))
         . ,(cddr (car sexpr1)))))

;; cut-to-absolute : expanded-mpi-sexpr -> expanded-mpi-sexpr
(define (cut-to-absolute sexpr)
  (cond [(and (pair? sexpr)
              (absolute-expanded-mpi-frame? (car sexpr)))
         (list (car sexpr))]
        [(pair? sexpr)
         (cons (car sexpr) (cut-to-absolute (cdr sexpr)))]))

;; absolute-expanded-mpi-frame? : expanded-mpi-frame -> boolean
(define (absolute-expanded-mpi-frame? sexpr)
  (not (memq (car sexpr) '(REL))))

;; library-expanded-mpi-frame? : expanded-mpi-frame -> boolean
(define (library-expanded-mpi-frame? sexpr)
  (memq (car sexpr) '(LIB PLANET)))

;; intersperse : X (listof X) -> (listof X)
(define (intersperse sep items)
  (cond [(and (pair? items) (pair? (cdr items)))
         (cons (car items) (cons sep (intersperse sep (cdr items))))]
        [else items]))



#|
(provide mpi->path-list
         path-list->library-module)

(define (mpi->path-list mpi)
  (reverse-to-abs (mpi->mpi-sexpr mpi) null))

(define (reverse-to-abs paths acc)
  (match paths
    ['() 
     acc]
    [#f
     (cons (list 'SELF) acc)]
    [(cons `(quote ,mod) rest)
     (cons `(QUOTE ,mod) acc)]
    [(cons `(lib ,path) rest)
     (cond [(symbol? path)
            (reverse-to-abs (cons path rest) acc)]
           [(regexp-match? #rx"/" path)
            (cons `(LIB ,(split-mods path)) acc)]
           [else
            (cons `(LIB ,(list "mzlib" path)) acc)])]
    [(cons `(lib ,path . ,more) rest)
     (cons `(LIB ,(split-mods path more)) acc)]
    [(cons `(planet ,(? symbol? spec)) rest)
     (reverse-to-abs (cons (parse-planet-spec spec) rest) acc)]
    [(cons `(planet ,path ,package . ,more) rest)
     (cons `(PLANET ,(split-mods path more) ,package) acc)]
    [(cons (? symbol? mod) rest)
     (cons `(LIB ,(split-mods* (symbol->string mod))) acc)]
    [(cons `(file ,path) rest)
     (cond [(absolute-path? path)
            (cons `(FILE ,(split-mods path)) acc)]
           [else (reverse-to-abs rest (cons (split-mods path) acc))])]
    [(cons (? string? path) rest)
     (reverse-to-abs rest (cons (split-mods path) acc))]))

(provide parse-planet-spec)
|#
