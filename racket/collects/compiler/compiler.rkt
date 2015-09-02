#lang racket/base
;; Main compilation procedures
;; (c) 1997-2014 PLT Design Inc.

(require syntax/toplevel
         syntax/moddep
	 dynext/file
         racket/file
         compiler/compile-file
         compiler/cm
	 compiler/option
         setup/getinfo
         setup/main-collects
         setup/private/omitted-paths)

(provide compile-zos
	 
	 compile-collection-zos
	 compile-directory-zos
	 compile-directory-srcs
	 
	 current-compiler-dynamic-require-wrapper
	 compile-notify-handler)

(define-namespace-anchor anchor)
(define orig-namespace (namespace-anchor->empty-namespace anchor))

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
        (let ([zo (path-add-suffix b #".zo")])
          (compile-to-zo f zo n prefix verbose? mod?)))))

  (define (compile-directory-visitor dir info worker omit-root
                                     #:verbose verbose?
                                     #:has-module-suffix? has-module-suffix?
                                     #:skip-path orig-skip-path
                                     #:skip-paths orig-skip-paths
                                     #:skip-doc-sources? skip-docs?)
    (define info* (or info (lambda (key mk-default) (mk-default))))
    (define omit-paths (omitted-paths dir c-get-info/full omit-root))
    (define skip-paths (for/list ([p (in-list (if orig-skip-path
                                                  (cons orig-skip-path orig-skip-paths)
                                                  orig-skip-paths))])
                         (path->bytes (simplify-path p #f))))
    (unless (eq? 'all omit-paths)
      (let ([init (parameterize ([current-directory dir]
                                 [current-load-relative-directory dir]
                                 ;; Verbose compilation manager:
                                 [manager-trace-handler
                                  (if verbose?
                                      (let ([op (current-output-port)])
                                        (lambda (s) (fprintf op "~a\n" s)))
                                      (manager-trace-handler))]
                                 [manager-compile-notify-handler
                                  (lambda (path) ((compile-notify-handler) path))]
                                 [manager-skip-file-handler
                                  (lambda (path)
                                    (and (pair? skip-paths)
                                         (let* ([simp-path (simplify-path path #f)]
                                                [b (path->bytes simp-path)])
                                           (for/or ([skip-path (in-list skip-paths)])
                                             (let ([len (bytes-length skip-path)])
                                               (and ((bytes-length b) . > . len)
                                                    (bytes=? (subbytes b 0 len) skip-path)
                                                    (let-values ([(base name dir?) (split-path simp-path)])
                                                      (or (and (path? base)
                                                               ;; Compute the stamp:
                                                               (file-stamp-in-paths simp-path (list base)))
                                                          ;; This shouldn't happen, but just in case:
                                                          (cons -inf.0 "")))))))))])
                    (let* ([sses (append
                                  ;; Find all .rkt/.ss/.scm files:
                                  (filter has-module-suffix? (directory-list))
                                  ;; Add specified doc sources:
                                  (if skip-docs?
                                      null
                                      (map (lambda (s) (if (string? s) (string->path s) s))
                                           (map car (info* 'scribblings (lambda () null)))))
                                  ;; Add specified additional sources:
                                  (map (lambda (s) (if (string? s) (string->path s) s))
                                       (info* 'compile-include-files (lambda () null))))]
                           [sses (remove* omit-paths sses)])
                      (worker null sses)))])

        (if (compile-subcollections)
          (begin 
            (when (info* 'compile-subcollections (lambda () #f))
              (printf "Warning: ignoring `compile-subcollections' entry in info ~a\n"
                      dir))
            (for/fold ([init init]) ([p (directory-list dir)])
              (let ([p* (build-path dir p)])
                (if (and (directory-exists? p*) (not (member p omit-paths)))
                    (compile-directory-visitor p* (c-get-info/full p*) worker omit-root
                                               #:has-module-suffix? has-module-suffix?
                                               #:verbose verbose?
                                               #:skip-path orig-skip-path
                                               #:skip-paths orig-skip-paths
                                               #:skip-doc-sources? skip-docs?)
                    init))))
          init))))
  (define (compile-directory dir info
                             #:has-module-suffix? [has-module-suffix? extract-base-filename/ss]
                             #:verbose [verbose? #t]
                             #:skip-path [orig-skip-path #f]
                             #:skip-paths [orig-skip-paths null]
                             #:skip-doc-sources? [skip-docs? #f]
                             #:managed-compile-zo [managed-compile-zo
                                                   (make-caching-managed-compile-zo)]
                             #:omit-root [omit-root dir])
    (define (worker prev sses)
      (for-each managed-compile-zo sses))
    (compile-directory-visitor dir info worker omit-root
                               #:has-module-suffix? has-module-suffix?
                               #:verbose verbose?
                               #:skip-path orig-skip-path
                               #:skip-paths orig-skip-paths
                               #:skip-doc-sources? skip-docs?))
  
  (define (get-compile-directory-srcs dir info
                                      #:has-module-suffix? [has-module-suffix? extract-base-filename/ss]
                                      #:verbose [verbose? #t] 
                                      #:skip-path [orig-skip-path #f]
                                      #:skip-paths [orig-skip-paths null]
                                      #:skip-doc-sources? [skip-docs? #f]
                                      #:omit-root [omit-root dir])
    (compile-directory-visitor dir info append omit-root
                               #:has-module-suffix? has-module-suffix?
                               #:verbose verbose?
                               #:skip-path orig-skip-path
                               #:skip-paths orig-skip-paths
                               #:skip-doc-sources? skip-docs?))
  
  (define unspec (gensym))

  (define (compile-collection-zos collection
                                  #:has-module-suffix? [has-module-suffix? extract-base-filename/ss]
                                  #:skip-path [skip-path #f]
                                  #:skip-paths [skip-paths null]
                                  #:skip-doc-sources? [skip-docs? #f]
                                  #:managed-compile-zo [managed-compile-zo
                                                        (make-caching-managed-compile-zo)]
                                  #:omit-root [omit-root unspec]
                                  . cp)
    (define dir (apply collection-path collection cp))
    (compile-directory dir
                       (c-get-info (cons collection cp))
                       #:omit-root (if (eq? omit-root unspec)
                                       dir
                                       omit-root)
                       #:has-module-suffix? has-module-suffix?
                       #:verbose #f
                       #:skip-path skip-path
                       #:skip-paths skip-paths
                       #:skip-doc-sources? skip-docs?
                       #:managed-compile-zo managed-compile-zo))

  (define compile-directory-zos compile-directory)
  (define compile-directory-srcs get-compile-directory-srcs)

