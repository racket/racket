#lang racket/base
(require syntax/modresolve
         setup/path-to-relative
         racket/match)
(provide allow-bypass?
         allow-drop?
         bypass-ok-mpi?)

(define (allow-bypass? mod)
  (not (memq (lookup mod) '(no-bypass no-drop))))

(define (allow-drop? mod)
  (not (memq (lookup mod) '(no-drop))))

;; A ModuleDB = hash[path/symbol => (U 'no-drop 'no-bypass)]
;;   'no-drop = must not be dropped or bypassed because of, eg, side effects
;;   'no-bypass = don't bypass in favor of private component modules
;;                but if the module is unused, can drop it
;;                (FIXME: replace with component module calculation and checking)

(define (make-module-db no-drop-list no-bypass-list)
  (let ([mod+config-list
         (append (for/list ([no-drop (in-list no-drop-list)])
                   (list no-drop 'no-drop))
                 (for/list ([no-bypass (in-list no-bypass-list)])
                   (list no-bypass 'no-bypass)))])
    (for/hash ([mod+config (in-list mod+config-list)])
      (values (resolve-module-path (car mod+config) #f) (cadr mod+config)))))

(define (lookup mod)
  (let ([name (resolved-module-path-name (module-path-index-resolve mod))])
    (cond [(symbol? name) 'no-bypass]
          [(hash-ref module-db name #f)
           => values]
          [(path? name)
           (let ([str (path->relative-string/library name)])
             (for/or ([rx (in-list no-bypass-rxs)])
               (and (regexp-match? rx str) 'no-bypass)))]
          [else #f])))

;; module-db : ModuleDB
(define module-db
  (make-module-db
   ;; no-drop
   '('#%builtin
     errortrace
     scheme/mzscheme ;; introduced by mzscheme's #%module-begin; can't drop
     racket/contract/private/basic-opters
     racket/contract/private/opters
     typed-racket/private/base-env
     typed-racket/private/base-special-env
     typed-racket/private/base-env-numeric
     typed-racket/private/base-env-indexing)
   ;; no-bypass
   '(mred
     mzscheme
     openssl
     racket/gui/base
     racket/match
     scheme/gui/base
     slideshow/base
     string-constants
     syntax/parse
     wxme)))

(define no-bypass-rxs
  '(#rx"^<collects>/srfi/[0-9]+\\.rkt$"))

;; ========================================

;; bypass-ok-mpi? : mpi -> boolean
;; Okay to recommend mod as a replacement in bypass? (heuristic)
(define (bypass-ok-mpi? mpi)
  (define (no-private? s) (not (regexp-match? #rx"private" s)))
  (define legacy-rxs (list #rx"^mzlib" #rx"^texpict"))
  (define (ok? s)
    (and (no-private? s)
         (for/and ([rx (in-list legacy-rxs)])
           (not (regexp-match? rx s)))))
  (let-values ([(modpath relto) (module-path-index-split mpi)])
    (match modpath
      [(list 'quote name)
       (not (regexp-match? #rx"^#%" (symbol->string name)))]
      [(? string?)
       (ok? modpath)]
      [(list 'lib parts ...)
       (andmap ok? parts)]
      [(? symbol?)
       (ok? (symbol->string modpath))]
      [(list 'file part)
       (ok? part)]
      [(list 'planet part ...)
       #t])))
