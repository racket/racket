
(htdp-syntax-test #'(let name ([x 12]) 10) "let: expected at least one binding (in parentheses) after let, but found something else")

(htdp-top (require "syntax.rkt"))

(htdp-syntax-test #'(let-syntax ([#%app (lambda (stx) (raise-syntax-error #f "app fail" stx))])
		    (local [(define (x a b) 1)]
			   (x 1 2)))
		  "app fail")

(htdp-top-pop 1)

