#lang scheme/unit

(require "../utils/utils.rkt" 
	 "signatures.rkt"
         (utils tc-utils)
         (env type-environments)
         (types utils)
         (rep type-rep)
         syntax/kerncase
         scheme/match)

(require (for-template scheme/base))

(import tc-expr^ tc-app^)
(export tc-dots^)

;; form : syntax[expr]
;; returns two values : one is the type, the other the bound on the ... (always a symbol)
(define (tc/dots form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (map)
      [id
       (identifier? #'id)
       ;; we use tc-error directly instead of lookup-fail because we _don't_ want this
       ;; error to be delayed.  We usually catch it further up and decide that something
       ;; wasn't dotted after all because of it.
       (match (lookup (dotted-env) #'id (lambda (k) 
                                          (tc-error "unbound dotted identifier ~a" (syntax-e k))))
         [(cons dty dbound)
          (values dty dbound)])]
      [(#%plain-app map f l)
       (let-values ([(lty lbound) (tc/dots #'l)])
         (unless (Dotted? (lookup (current-tvars) lbound (lambda _ #f)))
           (int-err "tc/dots: ~a was not dotted" lbound))
         (parameterize ([current-tvars (extend-env (list lbound)
                                                   (list (make-DottedBoth (make-F lbound)))
                                                   (current-tvars))])
           (match-let* ([ft (single-value #'f)]
                        [(tc-result1: t) (tc/funapp #'f #'(l) ft (list (ret lty)) #f)])
             (values t lbound))))]
      [_
       (tc-error "form cannot be used where a term of ... type is expected")])))
