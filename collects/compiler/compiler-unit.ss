;; Main compilation procedures
;; (c) 1997-2008 PLT

;; The various procedures provided by this library are implemented
;;  by dynamically linking to code supplied by the MzLib, dynext, and
;;  compiler collections.

;; The Scheme->C compiler is loaded as either sploadr.ss (link in
;;  real MrSpidey) or loadr.ss (link in trivial MrSpidey stubs).

#lang scheme/base

(require mzlib/unit

         "sig.ss"
         dynext/file-sig
         dynext/link-sig
         dynext/compile-sig

         make/make-sig
         make/collection-sig

         syntax/toplevel
         syntax/moddep

         mzlib/list
         scheme/file
         mzlib/compile ; gets compile-file
         mzlib/cm
         setup/getinfo
         setup/main-collects)

(provide compiler@)

(define-namespace-anchor anchor)
(define orig-namespace (namespace-anchor->empty-namespace anchor))

;; ;;;;;;;; ----- The main compiler unit ------ ;;;;;;;;;;
(define-unit compiler@
  (import compiler:option^
          dynext:compile^
          dynext:link^
          dynext:file^)
  (export compiler^)

  (define compile-notify-handler
    (make-parameter void))

  (define current-compiler-dynamic-require-wrapper
    (make-parameter (lambda (thunk)
                      (parameterize ([current-namespace orig-namespace])
                        (thunk)))))

  (define (c-dynamic-require path id)
    ((current-compiler-dynamic-require-wrapper)
     (lambda () (dynamic-require path id))))
  (define (c-get-info cp)
    ((current-compiler-dynamic-require-wrapper)
     (lambda () (get-info cp))))

  (define (make-extension-compiler mode prefix)
    (let ([u (c-dynamic-require 'compiler/private/base 'base@)]
          [init (unit (import compiler:inner^) (export)
		  (eval-compile-prefix prefix)
                  (case mode
                    [(compile-extension) compile-extension]
                    [(compile-extension-to-c) compile-extension-to-c]
                    [(compile-c-extension) compile-c-extension]))])
      (invoke-unit
       (compound-unit
        (import (COMPILE : dynext:compile^)
                (LINK : dynext:link^)
                (DFILE : dynext:file^)
                (OPTION : compiler:option^))
        (export)
        (link [((COMPILER : compiler:inner^)) u COMPILE LINK DFILE OPTION]
              [() init COMPILER]))
       (import dynext:compile^
               dynext:link^
               dynext:file^
               compiler:option^))))

  (define (make-compiler mode)
    (lambda (prefix)
      (let ([c (make-extension-compiler mode prefix)])
        (lambda (source-files destination-directory)
          (for ([source-file source-files])
            (c source-file (or destination-directory 'same)))))))

  (define (make-unprefixed-compiler mode)
    (let ([f #f])
      (lambda (source-files destination-directory)
        (unless f
          (set! f ((make-compiler mode) '(void))))
        (f source-files destination-directory))))

  (define compile-extensions
    (make-compiler 'compile-extension))
  (define compile-extensions-to-c
    (make-compiler 'compile-extension-to-c))
  (define compile-c-extensions
    (make-unprefixed-compiler 'compile-c-extension))

  (define (compile-to-zo src dest namespace eval?)
    ((if eval? 
         (lambda (t) (parameterize ([read-accept-reader #t])
                       (t)))
         with-module-reading-parameterization)
     (lambda ()
       (parameterize ([current-namespace namespace])
         (compile-file src dest
                       (if eval?
                         (lambda (expr)
                           (expand-syntax-top-level-with-compile-time-evals expr))
                         values)))))
    (printf " [output to \"~a\"]\n" dest))

  (define (compile-zos prefix)
    (define n (if prefix (make-base-namespace) (current-namespace)))
    (when prefix (eval prefix n))
    (lambda (source-files destination-directory)
      (define file-bases
        (map (lambda (file)
               (if destination-directory
                   (let-values ([(base file dir?) (split-path file)])
                     (build-path
                      (if (eq? destination-directory 'auto)
                          (let ([d (build-path (if (eq? base 'relative) 'same base)
                                               "compiled")])
                            (unless (directory-exists? d) (make-directory* d))
                            d)
                          destination-directory)
                      file))
                   file))
             source-files))
      (for ([f source-files] [b file-bases])
        (let ([zo (append-zo-suffix b)])
          (compile-to-zo f zo n prefix)))))

  (define (compile-directory dir info #:verbose [verbose? #t])
    (define info* (or info (lambda (key mk-default) (mk-default))))
    (define make (c-dynamic-require 'make/make-unit 'make@))
    (define coll (c-dynamic-require 'make/collection-unit 'make:collection@))
    (define init (unit (import make^ make:collection^) (export)
                       (values make-collection make-notify-handler)))
    (define-values (make-collection make-notify-handler)
      (invoke-unit
       (compound-unit
        (import (DFILE : dynext:file^)
                (OPTION : compiler:option^)
                (COMPILER : compiler^))
        (export)
        (link [((MAKE : make^)) make]
              [((COLL : make:collection^)) coll MAKE DFILE OPTION COMPILER]
              [() init MAKE COLL]))
       (import dynext:file^ compiler:option^ compiler^)))
    (define nothing (lambda () null))
    (define omit-paths (info* 'compile-omit-paths nothing))
    (define omit-files (info* 'compile-omit-files nothing))
    (unless (eq? 'all omit-paths)
      (parameterize ([current-directory dir]
                     [current-load-relative-directory dir]
                     ;; Verbose compilation manager:
                     [manager-trace-handler (if verbose?
                                                (lambda (s) (printf "~a\n" s))
                                                (manager-trace-handler))]
                     [manager-compile-notify-handler
                      (lambda (path) ((compile-notify-handler) path))])
        ;; Compile the collection files via make-collection
        (let* ([sses (append
                      ;; Find all .ss/.scm files:
                      (filter extract-base-filename/ss (directory-list))
                      ;; Add specified doc sources:
                      (map car (info* 'scribblings nothing)))]
               [sses (remove* (map string->path omit-paths) sses)]
               [sses (remove* (map string->path omit-files) sses)])
          (for-each (make-caching-managed-compile-zo) sses)))
      (when (compile-subcollections)
        (when (info* 'compile-subcollections (lambda () #f))
          (printf "Warning: ignoring `compile-subcollections' entry in info ~a\n"
                  dir))
        (for ([p (directory-list dir)])
          (let ([p* (build-path dir p)]
                [s  (path->string p)])
            (when (and
                   (directory-exists? p*)
                   (not
                    ;; this is the same check that setup/setup-unit is
                    ;; doing in `make-cc*'
                    (or (regexp-match? #rx"^[.]" s)
                        (equal? "compiled" s)
                        (and (equal? "doc" s)
                             (not (pair? (path->main-collects-relative p*))))
                        (and (pair? omit-paths) (member s omit-paths)))))
              (compile-directory p* (get-info/full p*))))))))

  (define (compile-collection-zos collection . cp)
    (compile-directory (apply collection-path collection cp)
                       (c-get-info (cons collection cp))
                       #:verbose #f))

  (define compile-directory-zos compile-directory)

  )
