#lang racket/base
(require racket/future)

;; other params are provided by declaration
(provide call-with-flag-params
         set-flag-params
	 setup-program-name
	 specific-collections
	 specific-packages
	 specific-planet-dirs
	 archives
	 archive-implies-reindex
	 current-target-directory-getter
	 current-target-plt-directory-getter)

;; a way to define a parameter that is set from an alist of names and values
(define defined-flag-params (make-parameter '()))
(define-syntax-rule (define-flag-param name default)
  (begin 
    (provide name)
    (define name
      (let ([param (make-parameter default)])
	(defined-flag-params (cons (cons 'name param) (defined-flag-params)))
	param))))

;; this macro is used to actually do the setting, `more ...' is for additional
;; parameters to set
(define (call-with-flag-params flags k)
  (let loop ([flag-params (defined-flag-params)])
    (cond
     [(null? flag-params) (k)]
     [else 
      (define name+param (car flag-params))
      (define x (assq (car name+param) flags))
      (if x
          (parameterize ([(cdr name+param) (cadr x)])
            (loop (cdr flag-params)))
          (loop (cdr flag-params)))])))

;; Imperative version of `with-flag-params':
(define-syntax-rule (set-flag-params flags more ...)
  (set-flag-params* flags (list* (cons 'more more) ... (defined-flag-params))))
(define (set-flag-params* flags flag-params)
  (for ([name+param flag-params])
    (cond [(assq (car name+param) flags)
           => (lambda (x) ((cdr name+param) (cadr x)))])))

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
(define-flag-param make-only #f)
(define-flag-param make-zo #t)
(define-flag-param make-launchers #t)
(define-flag-param make-foreign-libs #t)
(define-flag-param make-info-domain #t)
(define-flag-param make-docs #t)
(define-flag-param make-user #t)
(define-flag-param make-planet #t)
(define-flag-param avoid-main-installation #f)
(define-flag-param force-user-docs #f)
(define-flag-param make-tidy #f)
(define-flag-param make-doc-index #f)
(define-flag-param check-dependencies #t)
(define-flag-param always-check-dependencies #f)
(define-flag-param fix-dependencies #f)
(define-flag-param check-unused-dependencies #f)
(define-flag-param call-install #t)
(define-flag-param call-post-install #t)
(define-flag-param pause-on-errors #f)
(define-flag-param force-unpacks #f)
(define-flag-param doc-pdf-dest #f)
(define-flag-param fail-fast #f)

(define specific-collections (make-parameter null))
(define specific-packages (make-parameter null))
(define specific-planet-dirs (make-parameter null))

(define archives (make-parameter null))
(define archive-implies-reindex (make-parameter #t))

(define current-target-directory-getter (make-parameter current-directory))
(define current-target-plt-directory-getter 
  (make-parameter
   (lambda (preferred main-collects-parent-dir choices) preferred)))
