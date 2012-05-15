#lang racket/base
(require (for-syntax racket/base))

(provide include-extracted
         provide-extracted
         include-previously-extracted)

(define-for-syntax (do-include-extracted stx wraps)
  (syntax-case stx ()
    [(_ module-path)
     (with-syntax ([get-docs (syntax-local-lift-require 
                              #'(only (submod module-path srcdoc) get-docs)
                              (datum->syntax stx 'get-docs))]
                   [(wrap ...) wraps])
       #'(begin
           (define-syntax (docs stx)
             (define docs (get-docs))
             (if (identifier? docs)
                 ;; normal:
                 (with-syntax ([(_ xwrap (... ...)) stx]
                               [id docs])
                   #'(xwrap (... ...) id))
                 ;; delayed:
                 (with-syntax ([(_ xwrap (... ...)) stx]
                               [(reqs ((id d) (... ...))) (syntax-local-introduce
                                                           (datum->syntax #f (get-docs)))])
                   #`(begin
                       (require . reqs)
                       (xwrap (... ...) (list (cons 'id d) (... ...)))))))
           (docs wrap ...)))]))

(define-syntax (include-extracted stx)
  (do-include-extracted stx #'(map cdr)))
  
(define-syntax (provide-extracted stx)
  (syntax-case stx ()
    [(_ module-path)
     #`(begin
         #,(do-include-extracted stx #'(define exported))
         (provide exported))]))
  
(define-syntax-rule (include-previously-extracted module-path regexp)
  (let ()
    (local-require (rename-in module-path [exported exported]))
    (for/list ([p (in-list exported)]
               #:when (regexp-match regexp (symbol->string (car p))))
      (cdr p))))

