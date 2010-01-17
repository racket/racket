#lang scheme/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide contract
         recursive-contract
         current-contract-region)

(require (for-syntax scheme/base)
         scheme/stxparam
         unstable/srcloc
         "guts.ss"
         "helpers.ss")

(define-syntax-parameter current-contract-region (λ (stx) #'(#%variable-reference)))

(define-syntax (contract stx)
  (syntax-case stx ()
    [(_ a-contract to-check pos-blame-e neg-blame-e srcloc-e name-e)
     (syntax/loc stx
       (let* ([c a-contract]
              [v to-check]
              [b (make-blame srcloc-e
                             name-e
                             (contract-name c)
                             (unpack-blame pos-blame-e)
                             (unpack-blame neg-blame-e)
                             #f)])
         (((contract-projection c) b) v)))]
    [(_ a-contract to-check pos-blame-e neg-blame-e)
     (quasisyntax/loc stx
       (contract a-contract
                 to-check
                 pos-blame-e
                 neg-blame-e
                 (build-source-location (quote-syntax #,stx))
                 '#f))]
    [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
     (syntax/loc stx
       (let* ([info src-info-e])
         (contract a-contract-e
                   to-check
                   pos-blame-e
                   neg-blame-e
                   (unpack-source info)
                   (unpack-name info))))]))

(define (unpack-source info)
  (cond
   [(syntax? info) (build-source-location info)]
   [(list? info)
    (let ([loc (list-ref info 0)])
      (if (syntax? (srcloc-source loc))
        (struct-copy
         srcloc loc
         [source
          (resolved-module-path-name
           (module-path-index-resolve
            (syntax-source-module
             (srcloc-source loc))))])
        loc))]
   [else
    (error 'contract
           "expected a syntax object or list of two elements, got: ~e"
           info)]))

(define (unpack-name info)
  (cond
   [(syntax? info) (and (identifier? info) (syntax-e info))]
   [(list? info) (list-ref info 1)]
   [else
    (error 'contract
           "expected a syntax object or list of two elements, got: ~e"
           info)]))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax
      (simple-contract
       #:name '(recursive-contract arg)
       #:projection
       (λ (blame)
          (let ([ctc (coerce-contract 'recursive-contract arg)])
            (let ([f (contract-projection ctc)])
              (λ (val)
                 ((f blame) val)))))))]))
