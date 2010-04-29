#lang scheme/base

(require (for-syntax scheme/base)
         framework/framework)

(provide (rename-out [-preferences:get preferences:get])
         preferences:get-drracket:large-letters-font)

(define (preferences:get-drracket:large-letters-font)
  (preferences:get 'drracket:large-letters-font))

(define-syntax (-preferences:get stx)
  (syntax-case stx (quote)
    [(_ (quote sym))
     (with-syntax ([nm (datum->syntax stx (string->symbol (string-append "preferences:get" "-" (symbol->string (syntax-e #'sym)))))])
       (syntax/loc stx (nm)))]))
