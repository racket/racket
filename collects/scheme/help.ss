#lang scheme/base

(require setup/xref
         scribble/xref
         scribble/manual-struct
         net/uri-codec
         net/sendurl
         scheme/path
         (for-syntax scheme/base))

(provide help)

(define-syntax (help stx)
  (if (identifier? stx)
      #'(open-help-start)
      (syntax-case stx ()
        [(help)
         #'(open-help-start)]
        [(help id)
         (identifier? #'id)
         #'(find-help (quote-syntax id))]
        [(help id #:from lib)
         (if (identifier? #'id)
             (if (module-path? (syntax->datum #'lib))
                 #'(find-help/lib (quote id) (quote lib))
                 (raise-syntax-error
                  #f
                  "expected a module path after #:from"
                  stx
                  #'lib))
             (raise-syntax-error
              #f
              "expected an identifier before #:from"
              stx
              #'id))]
        [(help #:search str ...)
         (with-syntax ([(str ...)
                        (map (lambda (e)
                               (if (string? (syntax-e e))
                                   e
                                   (format "~s"
                                           (syntax->datum e))))
                             (syntax->list #'(str ...)))])
           #'(search-for (list str ...)))]
        [_
         (raise-syntax-error #f
                             "expects a single identifer, a #:from clause, or a #:search clause; try just `help' for more information"
                             stx)])))

(define (open-help-start)
  (find-help #'help))

(define-namespace-anchor anchor)

(define (find-help/lib sym lib)
  (let ([id (parameterize ([current-namespace (namespace-anchor->empty-namespace
                                               anchor)])
              (namespace-require `(for-label ,lib))
              (namespace-syntax-introduce (datum->syntax #f sym)))])
    (if (identifier-label-binding id)
        (find-help id)
        (error 'help
               "no binding for identifier: ~a from module: ~a"
               sym
               lib))))

(define (find-help id)
  (let* ([lb (identifier-label-binding id)]
         [b (and (not lb) (identifier-binding id))]
         [xref (load-collections-xref
                (lambda ()
                  (printf "Loading help index...\n")))])
    (if (or lb b)
        (let ([tag (xref-binding->definition-tag
                    xref
                    (or lb b)
                    (if lb #f 0))])
          (if tag
              (go-to-tag xref tag)
              (error 'help
                     "no documentation found for: ~e provided by: ~a"
                     (syntax-e id)
                     (module-path-index-resolve (caddr (or lb b))))))
        (search-for-exports xref (syntax-e id)))))

(define (search-for-exports xref sym)
  (let ([idx (xref-index xref)]
        [libs null])
    (for-each (lambda (entry)
                (when (exported-index-desc? (entry-desc entry))
                  (when (eq? sym (exported-index-desc-name (entry-desc entry)))
                    (set! libs (append libs (exported-index-desc-from-libs (entry-desc entry)))))))
              idx)
    (if (null? libs)
        (printf "Not found in any library's documentation: ~a\n" sym)
        (begin
          (printf "No documentation for current binding, but provided by:\n")
          (let loop ([libs libs])
            (unless (null? libs)
              (unless (member (car libs) (cdr libs))
                (printf "  ~a\n" (car libs)))
              (loop (cdr libs))))))))

(define (go-to-tag xref t)
  (let-values ([(file anchor) (xref-tag->path+anchor xref t)])
    (printf "Sending to web browser...\n  file: ~a\n" file)
    (when anchor (printf "  anchor: ~a\n" anchor))
    (unless (send-url/file file #:fragment (and anchor (uri-encode anchor)))
      (error 'help "browser launch failed"))))

(define generate-search-results #f)

(define (search-for strs)
  (printf "Generating and opening search page...\n")
  (unless generate-search-results
    (parameterize ([current-namespace (namespace-anchor->empty-namespace
                                       anchor)])
      (set! generate-search-results
            (dynamic-require 'help/search 'perform-search))))
  (generate-search-results strs))

