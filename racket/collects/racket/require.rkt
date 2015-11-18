#lang racket/base

(require (for-syntax racket/base racket/require-transform racket/list syntax/stx
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


(define-for-syntax (multi xs)
  (define (loop xs)
    (if (stx-null? xs)
      '(())
      (let ([first (stx-car xs)]
            [rest (loop (stx-cdr xs))])
        (if (stx-list? first)
          (let ([bads (filter stx-list? (syntax->list first))])
            (if (null? bads)
              (append-map (λ (x) (map (λ (y) (cons x y)) rest)) (syntax->list first))
              (error 'multi-in "not a simple element" (car (syntax->datum bads)))))
          (map (λ (x) (cons first x)) rest)))))
  (define options (loop xs))
  (define (try pred? ->str str->)
    (and (andmap (λ (x) (andmap pred? (map syntax-e x))) options)
         (map (λ (x)
                (let* ([d (map syntax-e x)]
                       [r (apply string-append
                                 (add-between (if ->str (map ->str d) d)
                                              "/"))]
                       [ctxt (last x)])
                  (datum->syntax ctxt (if str-> (str-> r) r) ctxt ctxt)))
              options)))
  (or (try string? #f #f)
      (try symbol? symbol->string string->symbol)
      (error 'multi-in "only accepts all strings or all symbols")))

(provide multi-in)
(define-require-syntax (multi-in stx)
  (syntax-case stx ()
    [(_ elem0 elem ...)
     (quasisyntax/loc stx
       (combine-in #,@(multi #'(elem0 elem ...))))]))

(module+ for-testing
  (provide (for-syntax multi)))
