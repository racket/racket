#lang racket/base
(require racket/list
         syntax/modread
         syntax/private/modresolve-noctc
         compiler/private/dep
         "cm-path.rkt"
         "cm-file.rkt"
         "cm-log.rkt")

(provide (all-defined-out))

;; Format in a ".dep" file is:
;;   (list <version>
;;         <machine> ; symbol or #f for machine-independent
;;         <sha1s>
;;         <dep> ...)
;; where <sha1> = (cons <src-sha1> <imports-sha1>)
;;              | (cons <src-sha1> (cons <imports-sha1> <assume-cmopiled-sha1>))
;; An <assume-compiled-sha1> is for the case where a machine-independent
;; bytecode file is recompiled, and the original machine-independent hash
;; should be preserved.

(define deps-has-version? pair?)
(define deps-version car)
(define (deps-has-machine? p) (and (pair? p) (pair? (cdr p))))
(define deps-machine cadr)
(define deps-sha1s caddr)
(define deps-src-sha1 caaddr)
(define (deps-imports-sha1 deps)
  (define p (cdaddr deps))
  (if (pair? p) (car p) p))
(define (deps-assume-compiled-sha1 deps)
  ;; Returns #f if ".dep" doesn't record a sha1 to assume for the compiled code
  (define p (cdaddr deps))
  (and (pair? p) (cdr p)))
(define deps-imports cdddr)

(define (get-deps code path)
  (define ht
    (let loop ([code code] [ht (hash)])
      (define new-ht
        (for*/fold ([ht ht]) ([imports (in-list (module-compiled-imports code))]
                              [x (in-list (cdr imports))])
          (let* ([r (resolve-module-path-index x path)]
                 [r (if (pair? r) (cadr r) r)])
            (if (and (path? r) 
                     (not (equal? path r))
                     (not (equal? path r))
                     (not (equal? path (rkt->ss r))))
                (hash-set ht (path->bytes r) #t)
                ht))))
      (for*/fold ([ht new-ht]) ([non-star? (in-list '(#f #t))]
                                [subcode (in-list (module-compiled-submodules code non-star?))])
        (loop subcode ht))))
  (for/list ([k (in-hash-keys ht)]) k))

(define (read-deps-file dep-path)
  (with-handlers ([exn:fail:filesystem? (lambda (ex)
                                          (trace-printf "failed reading ~a" dep-path)
                                          (list #f "none" '(#f . #f)))])
    (with-module-reading-parameterization
      (lambda ()
        (call-with-input-file* dep-path read)))))

