#lang racket/base

(require (for-syntax racket/base) racket/promise)

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
       (cond [(not (identifier? #'id))
              (raise-syntax-error
               #f "expected an identifier before #:from" stx #'id)]
             [(not (module-path? (syntax->datum #'lib)))
              (raise-syntax-error
               #f "expected a module path after #:from" stx #'lib)]
             [else #'(find-help/lib (quote id) (quote lib))])]
      [(help str0 str ...)
       (andmap (lambda (s) (string? (syntax-e s)))
               (syntax->list #'(str0 str ...)))
       #'(search-for (list str0 str ...))]
      [(help #:search str ...)
       (with-syntax ([(str ...)
                      (map (lambda (e)
                             (if (string? (syntax-e e))
                               e
                               (format "~s" (syntax->datum e))))
                           (syntax->list #'(str ...)))])
         #'(search-for (list str ...)))]
      [_
       (raise-syntax-error
        #f
        (string-append "expects any number of literal strings, a single"
                       " identifier, an identifier followed by a #:from clause,"
                       " or a #:search clause;"
                       " try `(help help)' for more information")
        stx)])))

(define (open-help-start)
  (go-to-main-page))

;; Autoload utilities from help/help-utils; if it does not exists,
;; suggest using docs.racket-lang.org.

(define-namespace-anchor anchor)
(define get-binding
  (let ([ns #f])
    (lambda (sym)
      (unless ns
        (set! ns (namespace-anchor->empty-namespace anchor)))
      (with-handlers ([exn:fail?
                       (lambda (exn)
                         ((error-display-handler) 
                          (if (exn? exn)
                              (exn-message exn)
                              (format "~s" exn))
                          exn)
                         (raise-user-error
                          'help
                          (string-append
                           "documentation system unavailable; "
                           "try http://docs.racket-lang.org/")))])
        (dynamic-require 'help/help-utils sym)))))

(define-syntax-rule (define-help-autoload id)
  (define id
    (let ([proc (delay (get-binding 'id))])
      (lambda args (apply (force proc) args)))))

(define-help-autoload find-help)
(define-help-autoload find-help/lib)
(define-help-autoload search-for)
(define-help-autoload go-to-main-page)
