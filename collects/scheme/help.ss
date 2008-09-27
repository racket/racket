#lang scheme/base

(require (for-syntax scheme/base) scheme/promise)

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
            #f "expected a module path after #:from" stx #'lib))
         (raise-syntax-error
          #f "expected an identifier before #:from" stx #'id))]
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
        (string-append "expects a single identifer, a #:from clause, or a"
                       " #:search clause; try just `help' for more information")
        stx)])))

(define (open-help-start)
  (find-help #'help))

(define-syntax-rule (define-help-autoload id)
  (begin
    (define auto (delay (dynamic-require 'scheme/private/help-autoload 'id)))
    (define (id . args) (apply (force auto) args))))

(define-help-autoload find-help)
(define-help-autoload find-help/lib)
(define-help-autoload search-for)
