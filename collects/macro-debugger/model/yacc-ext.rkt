#lang racket/base
(require (for-syntax racket/base)
         (prefix-in yacc: parser-tools/yacc))
(provide parser
         options
         productions
         definitions)

(define-syntax options
  (lambda (stx)
    (raise-syntax-error #f "options keyword used out of context" stx)))

(define-syntax productions
  (lambda (stx)
    (raise-syntax-error #f "productions keyword used out of context" stx)))

(define-syntax definitions
  (lambda (stx)
    (raise-syntax-error #f "definitions keyword used out of context" stx)))

(define-syntax (parser stx)
  (syntax-case stx ()
    [(parser form ...)
     (let ([stop-list (list #'begin #'options #'productions #'definitions)]
           [forms (syntax->list #'(form ...))])
       (define-values (opts prods defs)
         (let loop ([forms forms] [opts null] [prods null] [defs null])
           (if (pair? forms)
               (let ([eform0 (local-expand (car forms) 'expression stop-list)]
                     [forms (cdr forms)])
                 (syntax-case eform0 (begin options productions definitions)
                   [(begin subform ...)
                    (loop (append (syntax->list #'(subform ...)) forms) opts prods defs)]
                   [(options subform ...)
                    (loop forms (append (syntax->list #'(subform ...)) opts) prods defs)]
                   [(productions subform ...)
                    (loop forms opts (append (syntax->list #'(subform ...)) prods) defs)]
                   [(definitions subform ...)
                    (loop forms opts prods (append (syntax->list #'(subform ...)) defs))]
                   [else
                    (raise-syntax-error #f "bad parser subform" eform0)]))
               (values opts prods defs))))
       (with-syntax ([(opt ...) opts]
                     [(prod ...) prods]
                     [(def ...) defs])
         #'(let ()
             def ...
             (#%expression (yacc:parser opt ... (grammar prod ...))))))]))
