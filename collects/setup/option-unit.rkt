#lang scheme/base
(require scheme/unit
         racket/future
         "option-sig.rkt")

(provide setup:option@ set-flag-params)

;; a way to define a parameter that is set from an alist of names and values
(define defined-flag-params (make-parameter '()))
(define-syntax-rule (define-flag-param name default)
  (define name
    (let ([param (make-parameter default)])
      (defined-flag-params (cons (cons 'name param) (defined-flag-params)))
      param)))
(define (set-flag-params* flags flag-params)
  (for ([name+param flag-params])
    (cond [(assq (car name+param) flags)
           => (lambda (x) ((cdr name+param) (cadr x)))])))
;; this macro is used to actually do the setting, `more ...' is for additional
;; parameters to set
(define-syntax-rule (set-flag-params flags more ...)
  (set-flag-params* flags (list* (cons 'more more) ... (defined-flag-params))))

(define-unit setup:option@
  (import)
  (export setup-option^)

  (define setup-program-name (make-parameter "raco setup"))

  (define-flag-param parallel-workers (min (processor-count) 
                                           (if (fixnum? (arithmetic-shift 1 40))
                                               8    ; 64-bit machine
                                               4))) ; 32-bit machine
  (define-flag-param verbose #f)
  (define-flag-param make-verbose #f)
  (define-flag-param compiler-verbose #f)
  (define-flag-param clean #f)
  (define-flag-param compile-mode #f)
  (define-flag-param make-zo #t)
  (define-flag-param make-launchers #t)
  (define-flag-param make-info-domain #t)
  (define-flag-param make-docs #t)
  (define-flag-param make-user #t)
  (define-flag-param make-planet #t)
  (define-flag-param avoid-main-installation #f)
  (define-flag-param call-install #t)
  (define-flag-param call-post-install #t)
  (define-flag-param pause-on-errors #f)
  (define-flag-param force-unpacks #f)
  (define-flag-param doc-pdf-dest #f)

  (define specific-collections (make-parameter null))
  (define specific-planet-dirs (make-parameter null))

  (define archives (make-parameter null))
  (define archive-implies-reindex (make-parameter #t))

  (define current-target-directory-getter (make-parameter current-directory))
  (define current-target-plt-directory-getter 
    (make-parameter
     (lambda (preferred main-collects-parent-dir choices) preferred))))
