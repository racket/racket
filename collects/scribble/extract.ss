#lang scheme/base

(require scribble/manual
         scribble/decode
         scribble/srcdoc
         (for-syntax scheme/base
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
    [(_ orig-path)
     (let ([path (resolve-path-spec #'orig-path #'orig-path stx)])
       (let ([s-exp 
              (parameterize ([current-namespace (make-base-namespace)]
                             [read-accept-reader #t])
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
                           (map
                            strip-context
                            (apply
                             append
                             (map (lambda (c)
                                    (syntax-case c (#%plain-app void quote-syntax provide/doc)
                                      [(#%plain-app void (quote-syntax (provide/doc spec ...)))
                                       (syntax->list #'(spec ...))]
                                      [_ null]))
                                  (syntax->list #'(content ...)))))]
                          [(req ...)
                           (map
                            strip-context
                            (apply
                             append
                             (map (lambda (c)
                                    (syntax-case c (#%require #%plain-app void quote-syntax require/doc)
                                      [(#%require spec ...)
                                       (syntax->list #'((for-label spec) ...))]
                                      [(#%plain-app void (quote-syntax (require/doc spec ...)))
                                       (syntax->list #'(spec ...))]
                                      [_ null]))
                                  (syntax->list #'(content ...)))))])
              #`(begin
                  (#%require (for-label #,(strip-context #'lang))
                             (for-label #,(strip-context #'orig-path)) 
                             req ...)
                  (def-it content) ...))])))]))

(define-syntax def-it
  (syntax-rules ()
    [(_ ((rename old-id id) contract desc))
     (def-it (id contract desc))]
    [(_ (id (-> arg ... result) desc))
     (defproc (id arg ...) result . desc)]))
