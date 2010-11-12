#lang scheme/base

(require scribble/manual
         scribble/decode
         scribble/srcdoc
         (for-syntax scheme/base
                     scheme/path
                     scheme/list
                     syntax/path-spec
                     syntax/modread
                     (for-syntax scheme/base)))

(provide include-extracted
         provide-extracted
         include-previously-extracted)

(define-for-syntax (strip-context c)
  (cond
   [(syntax? c) (datum->syntax
                 #f
                 (strip-context (syntax-e c))
                 c)]
   [(pair? c) (cons (strip-context (car c))
                    (strip-context (cdr c)))]
   [else c]))

(define-for-syntax (extract orig-path stx)
  (let* ([n-path (resolve-path-spec orig-path orig-path stx)]
         [path (if (regexp-match? #rx#"[.]rkt$" (path->bytes n-path))
                   (if (file-exists? n-path)
                       n-path
                       (let ([ss (path-replace-suffix n-path #".ss")])
                         (if (file-exists? ss)
                             ss
                             n-path)))
                   n-path)])
    (let ([s-exp 
           (parameterize ([current-namespace (make-base-namespace)]
                          [current-load-relative-directory
                           (path-only path)])
             (expand
              (with-module-reading-parameterization
               (lambda ()
                 (with-input-from-file path
                   (lambda ()
                     (port-count-lines! (current-input-port))
                     (read-syntax path)))))))])
      (syntax-case s-exp ()
        [(mod name lang
              (mod-beg
               content ...))
         (with-syntax ([((content id) ...)
                        (apply
                         append
                         (map (lambda (c)
                                (syntax-case c (#%plain-app void quote-syntax provide/doc)
                                  [(#%plain-app void (quote-syntax (provide/doc spec ...)))
                                   (syntax->list #'(spec ...))]
                                  [_ null]))
                              (syntax->list #'(content ...))))]
                       [(doc-req ...)
                        (map
                         strip-context
                         (append-map (lambda (c)
                                       (syntax-case c (#%plain-app void quote-syntax require/doc)
                                         [(#%plain-app void (quote-syntax (require/doc spec ...)))
                                          (syntax->list #'(spec ...))]
                                         [_ null]))
                                     (syntax->list #'(content ...))))]
                       [(req ...)
                        (map
                         strip-context
                         (append-map (lambda (c)
                                       (syntax-case c (#%require)
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
                                         [_ null]))
                                     (syntax->list #'(content ...))))]
                       [orig-tag (datum->syntax #f 'orig)])
           ;; This template is matched in `filter-info', below
           #`(begin
               (#%require (for-label #,(strip-context #'lang))
                          (for-label #,(strip-context orig-path)) 
                          req ...)
               (require doc-req ...)
               (drop-first (quote-syntax id) (def-it orig-tag content)) ...))]))))

(define-syntax (include-extracted stx)
  (syntax-case stx ()
    [(_ orig-path)
     (extract #'orig-path stx)]))

(define-syntax (provide-extracted stx)
  (syntax-case stx ()
    [(_ orig-path)
     (with-syntax ([(_begin reqs doc-reqs (_drop-first (_quote-syntax id) def) ...)
                    (extract #'orig-path stx)])
       #'(begin
           (require (for-label (only-in orig-path))) ;; creates build dependency
           (define-syntax (extracted stx)
             (syntax-case stx ()
               [(_ rx)
                (let-syntax ([quote-syntax/loc 
                              (lambda (stx)
                                (syntax-case stx ()
                                  [(_ s)
                                   (let loop ([stx #'s])
                                     (cond
                                      [(syntax? stx)
                                       (let ([ctx (datum->syntax stx 'ctx #f #f stx)])
                                         (let ([s 
                                                #`(datum->syntax (quote-syntax #,ctx)
                                                                 #,(loop (syntax-e stx))
                                                                 #,(and (syntax-position stx)
                                                                        (vector (let ([s (syntax-source stx)])
                                                                                  (if (path-string? s)
                                                                                      s
                                                                                      (format "~s" s)))
                                                                                (syntax-line stx)
                                                                                (syntax-column stx)
                                                                                (syntax-position stx)
                                                                                (syntax-span stx))))])
                                           (let ([p (syntax-property stx 'paren-shape)])
                                             (if p
                                                 #`(syntax-property #,s 'paren-shape '#,p)
                                                 s))))]
                                      [(pair? stx) #`(cons #,(loop (car stx)) #,(loop (cdr stx)))]
                                      [(vector? stx) #`(vector #,@(map loop (vector->list stx)))]
                                      [(box? stx) #`(box #,(loop (unbox stx)))]
                                      [else #`(quote #,stx)]))]))])
                  #`(begin #,(quote-syntax/loc reqs)
                           #,(quote-syntax/loc doc-reqs)
                           #,@(filter
                               values
                               (map (lambda (i d)
                                      (if (regexp-match (syntax-e #'rx) (symbol->string i))
                                          (d)
                                          #f))
                                    (list 'id ...)
                                    (list (lambda () (quote-syntax/loc def)) ...)))))]))
           (provide extracted)))]))

(define-syntax (include-previously-extracted stx)
  (syntax-case stx ()
    [(_ orig-path regexp-s)
     (unless (regexp? (syntax-e #'regexp-s))
       (raise-syntax-error #f "expected a literal regular expression as the second argument" stx #'regexp-s))
     #`(begin
         (require (only-in orig-path [#,(datum->syntax #'orig-path 'extracted) extracted]))
         (extracted regexp-s))]))

(define-for-syntax (revise-context c orig-tag new-tag tag)
  (cond
   [(syntax? c) 
    (datum->syntax
     (if (bound-identifier=? tag (datum->syntax c 'tag))
         new-tag
         orig-tag)
     (revise-context (syntax-e c) orig-tag new-tag tag)
     c
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

(define-syntax-rule (drop-first a b) b)
