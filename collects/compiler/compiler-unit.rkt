;; Main compilation procedures
;; (c) 1997-2010 PLT

;; The various procedures provided by this library are implemented
;;  by dynamically linking to code supplied by the MzLib, dynext, and
;;  compiler collections.

#lang scheme/base

(require scheme/unit

         "sig.ss"
         dynext/file-sig
         dynext/link-sig
         dynext/compile-sig

         syntax/toplevel
         syntax/moddep

         scheme/file
         mzlib/compile ; gets compile-file
         compiler/cm
         setup/getinfo
         setup/main-collects
         setup/private/omitted-paths)

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
  (define (c-get-info/full cp)
    ((current-compiler-dynamic-require-wrapper)
     (lambda () (get-info/full cp))))

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

  (define (compile-to-zo src dest namespace eval? verbose? mod?)
    ((if eval? 
         (lambda (t) (parameterize ([read-accept-reader #t])
                       (t)))
         with-module-reading-parameterization)
     (lambda ()
       (parameterize ([current-namespace namespace])
         (compile-file src dest
                       (compose
                        (if eval?
                            (lambda (expr)
                              (expand-syntax-top-level-with-compile-time-evals expr))
                            values)
                        (if mod?
                            (lambda (expr)
                              (check-module-form expr 
                                                 (let-values ([(base name dir?) (split-path src)])
                                                   (string->symbol 
                                                    (path-element->string (path-replace-suffix name #""))))
                                                 src))
                            values))))))
    (when verbose?
      (printf " [output to \"~a\"]\n" dest)))

  (define (compile-zos prefix #:verbose? [verbose? #f] #:module? [mod? #f])
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
          (compile-to-zo f zo n prefix verbose? mod?)))))

  (define (compile-directory dir info 
                             #:verbose [verbose? #t] 
                             #:skip-path [orig-skip-path #f]
                             #:skip-doc-sources? [skip-docs? #f])
    (define info* (or info (lambda (key mk-default) (mk-default))))
    (define omit-paths (omitted-paths dir c-get-info/full))
    (define skip-path (and orig-skip-path (path->bytes 
                                           (simplify-path (if (string? orig-skip-path)
                                                              (string->path orig-skip-path)
                                                              orig-skip-path)
                                                          #f))))
    (unless (eq? 'all omit-paths)
      (parameterize ([current-directory dir]
                     [current-load-relative-directory dir]
                     ;; Verbose compilation manager:
                     [manager-trace-handler (if verbose?
                                                (let ([op (current-output-port)])
                                                  (lambda (s) (fprintf op "~a\n" s)))
                                                (manager-trace-handler))]
                     [manager-compile-notify-handler
                      (lambda (path) ((compile-notify-handler) path))]
                     [manager-skip-file-handler
                      (lambda (path) (and skip-path
                                          (let ([b (path->bytes (simplify-path path #f))]
                                                [len (bytes-length skip-path)])
                                            (and ((bytes-length b) . > . len)
                                                 (bytes=? (subbytes b 0 len) skip-path)))
                                          (list -inf.0 "")))])
        (let* ([sses (append
                      ;; Find all .rkt/.ss/.scm files:
                      (filter extract-base-filename/ss (directory-list))
                      ;; Add specified doc sources:
                      (if skip-docs?
                          null
                          (map car (info* 'scribblings (lambda () null)))))]
               [sses (remove* omit-paths sses)])
          (for-each (make-caching-managed-compile-zo) sses)))
      (when (compile-subcollections)
        (when (info* 'compile-subcollections (lambda () #f))
          (printf "Warning: ignoring `compile-subcollections' entry in info ~a\n"
                  dir))
        (for ([p (directory-list dir)])
          (let ([p* (build-path dir p)])
            (when (and (directory-exists? p*) (not (member p omit-paths)))
              (compile-directory p* (c-get-info/full p*) 
                                 #:verbose verbose?
                                 #:skip-path skip-path
                                 #:skip-doc-sources? skip-docs?)))))))

  (define (compile-collection-zos collection 
                                  #:skip-path [skip-path #f]
                                  #:skip-doc-sources? [skip-docs? #f]
                                  . cp)
    (compile-directory (apply collection-path collection cp)
                       (c-get-info (cons collection cp))
                       #:verbose #f
                       #:skip-path skip-path
                       #:skip-doc-sources? skip-docs?))

  (define compile-directory-zos compile-directory)

  )
