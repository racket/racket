#lang scheme/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide (rename-out [-contract contract])
         recursive-contract
         current-contract-region)

(require (for-syntax scheme/base)
         scheme/stxparam
         "contract-guts.ss"
         "contract-helpers.ss")

(define-syntax-parameter current-contract-region (λ (stx) #'(#%variable-reference)))

(define-syntax (-contract stx)
  (syntax-case stx ()
    [(_ a-contract to-check pos-blame-e neg-blame-e)
     (let ([s (syntax/loc stx here)])
       (quasisyntax/loc stx
         (contract/proc a-contract to-check pos-blame-e neg-blame-e 
                        (list (make-srcloc (quote-syntax #,s)
                                           #,(syntax-line s)
                                           #,(syntax-column s)
                                           #,(syntax-position s)
                                           #,(syntax-span s)) 
                              #f))))]
    [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
     (syntax/loc stx
       (begin
         (contract/proc a-contract-e to-check pos-blame-e neg-blame-e src-info-e)))]))

(define (contract/proc a-contract-raw name pos-blame neg-blame src-info)
  (let ([a-contract (coerce-contract 'contract a-contract-raw)])

    (unless (or (and (list? src-info)
                     (= 2 (length src-info))
                     (srcloc? (list-ref src-info 0))
                     (or (string? (list-ref src-info 1))
                         (not (list-ref src-info 1))))
                (syntax? src-info))
      (error 'contract "expected syntax or a list of two elements (srcloc and string or #f) as last argument, given: ~e, other args ~e ~e ~e ~e"
             src-info
             (unpack-blame neg-blame)
             (unpack-blame pos-blame)
             a-contract-raw
             name))
    (((contract-proc a-contract) pos-blame neg-blame src-info (contract-name a-contract) #t)
     name)))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax (make-proj-contract 
              '(recursive-contract arg) 
              (λ (pos-blame neg-blame src str positive-position?)
                (let ([ctc (coerce-contract 'recursive-contract arg)])
                  (let ([proc (contract-proc ctc)])
                    (λ (val)
                      ((proc pos-blame neg-blame src str positive-position?) val)))))
              #f))]))