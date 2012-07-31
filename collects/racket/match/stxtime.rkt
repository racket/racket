#lang racket/base

(provide (all-defined-out))

(define match-...-nesting (make-parameter 0))

(struct acc-prop (n acc))

(define (make-struct-type-property/accessor name [guard #f] [supers null])
  (define-values (p pred? acc)
    (make-struct-type-property name
     (λ (pval sinfo)
	(cond [(exact-nonnegative-integer? pval)
	       (acc-prop pval (cadddr sinfo))]
	      [else (if (procedure? guard) 
			(guard pval sinfo)
			pval)]))
     supers))
  (values p pred? (lambda (v)
		    (define v* (acc v))
		    (if (acc-prop? v*)
			((acc-prop-acc v*) v (acc-prop-n v*))
			v*))))

(define-values (prop:match-expander match-expander? match-expander-proc) 
  (make-struct-type-property/accessor 'prop:match-expander))

(define-values (prop:legacy-match-expander legacy-match-expander? legacy-match-expander-proc)
  (make-struct-type-property/accessor 'prop:legacy-match-expander ))
