#lang scheme/base

(require scribble/manual
         scribble/decode
         scribble/srcdoc
         (for-syntax scheme/base
                     scheme/path
                     syntax/path-spec))

(provide include-extracted)

(define-for-syntax (strip-context c)
  (cond
   [(syntax? c) (datum->syntax
                 #f
                 (strip-context (syntax-e c))
                 c)]
   [(pair? c) (cons (strip-context (car c))
                    (strip-context (cdr c)))]
   [else c]))

(define-syntax (include-extracted stx)
  (syntax-case stx ()
    [(_ orig-path) #'(include-extracted orig-path #rx"")] ;; this regexp matches everything
    [(_ orig-path regexp-s)
     (let ([path (resolve-path-spec #'orig-path #'orig-path stx)]
           [reg (syntax-e #'regexp-s)])
       (unless (regexp? reg)
         (raise-syntax-error #f "expected a literal regular expression as the second argument" stx #'regexp-s))
       (let ([s-exp 
              (parameterize ([current-namespace (make-base-namespace)]
                             [read-accept-reader #t]
                             [current-load-relative-directory
                              (path-only path)])
                (expand
                 (with-input-from-file path
                   (lambda ()
                     (port-count-lines! (current-input-port))
                     (read-syntax path)))))])
         (syntax-case s-exp ()
           [(mod name lang
                 (mod-beg
                  content ...))
            (with-syntax ([(content ...)
                           (apply
                            append
                            (map (lambda (c)
                                   (syntax-case c (#%plain-app void quote-syntax provide/doc)
                                     [(#%plain-app void (quote-syntax (provide/doc spec ...)))
                                      (map
                                       (λ (x) (syntax-case x () [(docs id) #'docs])) 
                                       (filter (λ (x)
                                                 (syntax-case x ()
                                                   [(stuff id)
                                                    (regexp-match reg (symbol->string (syntax-e #'id)))]))
                                               (syntax->list #'(spec ...))))]
                                     [_ null]))
                                 (syntax->list #'(content ...))))]
                          [(req ...)
                           (map
                            strip-context
                            (apply
                             append
                             (map (lambda (c)
                                    (syntax-case c (#%require #%plain-app void quote-syntax require/doc)
                                      [(#%require spec ...)
                                       (let loop ([specs (syntax->list #'(spec ...))])
                                         (cond
                                           [(null? specs) '()]
                                           [else (let ([spec (car specs)])
                                                   (syntax-case spec (for-syntax for-meta)
                                                     [(for-syntax . spec) (loop (cdr specs))]
                                                     [(for-meta . spec) (loop (cdr specs))]
                                                     [(for-template . spec) (loop (cdr specs))]
                                                     [(for-label . spec) (loop (cdr specs))]
                                                     [(just-meta . spec) (loop (cdr specs))]
                                                     [_ (cons #`(for-label #,spec) (loop (cdr specs)))]))]))]
                                      [(#%plain-app void (quote-syntax (require/doc spec ...)))
                                       (syntax->list #'(spec ...))]
                                      [_ null]))
                                  (syntax->list #'(content ...)))))]
                          [orig-tag (datum->syntax #f 'orig)])
              #`(begin
                  (#%require (for-label #,(strip-context #'lang))
                             (for-label #,(strip-context #'orig-path)) 
                             req ...)
                  (def-it orig-tag content) ...))])))]))

(define-for-syntax (revise-context c orig-tag new-tag tag)
  (cond
   [(syntax? c) 
    (datum->syntax
     (if (bound-identifier=? tag (datum->syntax c 'tag))
         new-tag
         orig-tag)
     (revise-context (syntax-e c) orig-tag new-tag tag)
     c)]
   [(pair? c) (cons (revise-context (car c) orig-tag new-tag tag)
                    (revise-context (cdr c) orig-tag new-tag tag))]
   [else c]))

(define-syntax (def-it stx)
  (syntax-local-introduce
   (syntax-case (syntax-local-introduce stx) ()
     [(_ orig-path (reqs doc tag))
      (let ([new-tag ((make-syntax-introducer) 
                      (datum->syntax #'orig-path 'new-tag))]
            [orig-tag #'orig-path])
        #`(begin
            (require . #,(revise-context #'reqs orig-tag new-tag #'tag))
            #,(revise-context #'doc orig-tag new-tag #'tag)))])))
