#lang scheme/base

(require (for-syntax scheme/base unstable/srcloc))

(provide quote-srcloc
         quote-source-file
         quote-line-number
         quote-column-number
         quote-character-position
         quote-character-span
         quote-module-path
         quote-module-source
         quote-module-name)

(define-syntax (quote-srcloc stx)
  (syntax-case stx ()
    [(_) #`(quote-srcloc #,stx)]
    [(_ loc)
     (with-syntax ([(arg ...) (build-source-location-list #'loc)])
       #'(make-srcloc (quote arg) ...))]
    [(_ loc #:module-source alt-src)
     (with-syntax ([(src arg ...) (build-source-location-list #'loc)])
       (with-syntax ([alt-src (if (syntax-source-module #'loc)
                                  #'alt-src
                                  #'(quote src))])
         #'(make-srcloc alt-src (quote arg) ...)))]))

(define-syntax (quote-source-file stx)
  (syntax-case stx ()
    [(_) #`(quote-source-file #,stx)]
    [(_ loc) #`(quote #,(source-location-source #'loc))]))

(define-syntax (quote-line-number stx)
  (syntax-case stx ()
    [(_) #`(quote-line-number #,stx)]
    [(_ loc) #`(quote #,(source-location-line #'loc))]))

(define-syntax (quote-column-number stx)
  (syntax-case stx ()
    [(_) #`(quote-column-number #,stx)]
    [(_ loc) #`(quote #,(source-location-column #'loc))]))

(define-syntax (quote-character-position stx)
  (syntax-case stx ()
    [(_) #`(quote-character-position #,stx)]
    [(_ loc) #`(quote #,(source-location-position #'loc))]))

(define-syntax (quote-character-span stx)
  (syntax-case stx ()
    [(_) #`(quote-character-span #,stx)]
    [(_ loc) #`(quote #,(source-location-span #'loc))]))

(define-syntax-rule (quote-module-name)
  (variable-reference->module-name (#%variable-reference)))

(define-syntax-rule (quote-module-path)
  (variable-reference->module-path (#%variable-reference)))

(define-syntax-rule (quote-module-source)
  (variable-reference->module-src (#%variable-reference)))

(define (variable-reference->module-path var)
  (module-name->module-path
   (variable-reference->module-name var)))

(define (variable-reference->module-name var)
  (let* ([rmp (variable-reference->resolved-module-path var)])
    (if (resolved-module-path? rmp)
      (resolved-module-path-name rmp)
      rmp)))

(define (module-name->module-path name)
  (cond
   [(path? name) `(file ,(path->string name))]
   [(symbol? name) `(quote ,name)]
   [else 'top-level]))

(define (variable-reference->module-src var)
  (let ([v (variable-reference->module-source var)])
    (if v
        (make-resolved-module-path v)
        'top-level)))
