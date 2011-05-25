#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform scheme/list
                     (only-in racket/syntax syntax-local-eval))
         "require-syntax.rkt")

(provide matching-identifiers-in)
(define-syntax matching-identifiers-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ rx spec)
        (regexp? (syntax-e #'rx))
        (let ([rx (syntax-e #'rx)])
          (define-values [imports sources] (expand-import #'spec))
          (values
           (filter (lambda (i)
                     (regexp-match? rx (symbol->string
                                        (syntax-e (import-local-id i)))))
                   imports)
           sources))]))))

(provide subtract-in)
(define-syntax subtract-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec specs ...)
        (let* ([subs (map (lambda (spec)
                            (let-values ([(imports srcs) (expand-import spec)])
                              imports))
                          (syntax->list #'(specs ...)))]
               [subs (map (lambda (i) (syntax-e (import-local-id i)))
                          (apply append subs))])
          (define-values [imports sources] (expand-import #'spec))
          (values (filter (lambda (i)
                            (not (memq (syntax-e (import-local-id i)) subs)))
                          imports)
                  sources))]))))

(provide filtered-in)
(define-syntax filtered-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ proc spec)
        (let ([proc (syntax-local-eval #'proc)])
          (define-values [imports sources] (expand-import #'spec))
          (values
           (filter-map
            (lambda (i)
              (let* ([id (import-local-id i)]
                     [s1 (symbol->string (syntax-e id))]
                     [s2 (proc s1)])
                (cond [(equal? s1 s2) i]
                      [(string? s2) (make-import (datum->syntax
                                                  id (string->symbol s2) id)
                                                 (import-src-sym i)
                                                 (import-src-mod-path i)
                                                 (import-mode i)
                                                 (import-req-mode i)
                                                 (import-orig-mode i)
                                                 (import-orig-stx i))]
                      [(not s2) #f]
                      [else (error 'filtered-in "bad result: ~e" s2)])))
            imports)
           sources))]))))

(provide path-up)
(define-require-syntax (path-up stx)
  (syntax-case stx ()    
    [(_ path-stx ...)
     (for/and ([ps (in-list (syntax->list #'(path-stx ...)))]) 
       (let ([s (syntax-e ps)]) (and (string? s) (module-path? s))))
     (let* ([src (syntax-source stx)]
            [dirname (lambda (path)
                       (let-values ([(dir name dir?) (split-path path)]) dir))]
            [srcdir (if (and (path-string? src) (complete-path? src))
                        (dirname src)
                        (or (current-load-relative-directory)
                            (current-directory)))])
       (with-syntax 
           ([(paths ...)
             (for/list ([ps (in-list (syntax->list #'(path-stx ...)))])
               (define path (syntax-e ps))
               (unless (complete-path? srcdir) (error 'path-up "internal error"))
               (parameterize ([current-directory srcdir])
                 (let loop ([dir srcdir] [path (string->path path)] [pathstr path])
                   (if (file-exists? path)
                       (datum->syntax stx pathstr stx stx)
                       (let ([dir (dirname dir)])
                         (if dir
                             (loop dir (build-path 'up path)
                                   (string-append "../" pathstr))
                             (raise-syntax-error 'path-up
                                                 "file not found in any parent directory"
                                                 stx ps)))))))])
         (syntax/loc stx (combine-in paths ...))))]))

(provide multi-in)
(define-require-syntax (multi-in stx)
  (syntax-case stx ()
    [(_ dir files ...)
     (or (andmap (lambda (f) ; directory + all files
                   (let ([s (syntax-e f)]) (and (string? s) (module-path? s))))
                 (syntax->list #'(files ...)))
         (andmap (lambda (f) (symbol? (syntax-e f))) ; collects path
                 (syntax->list #'(files ...))))
     (let ([dir (syntax-e #'dir)])
       (with-syntax
           ([(paths ...)
             (map (lambda (f)
                    (datum->syntax
                     stx (if (string? dir)
                             (string-append dir "/" (syntax-e f))
                             (string->symbol
                              (string-append (symbol->string dir) "/"
                                             (symbol->string (syntax-e f)))))
                     stx stx))
                  (syntax->list #'(files ...)))])
         (syntax/loc stx (combine-in paths ...))))]))
