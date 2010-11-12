
;; Basic checks for the intermediate language. See also
;;  beginner.rktl

(load-relative "../racket/loadtest.rktl")

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
(require syntax/docprovide)
(let ([docs (lookup-documentation '(lib "htdp-intermediate-lambda.rkt" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "htdp-intermediate-lambda.rkt" "lang") (car doc))])
	  (when (procedure? v)
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(define current-htdp-lang 'lang/htdp-intermediate-lambda)
(load-relative "htdp-test.rktl")

(require (lib "htdp-intermediate-lambda.rkt" "lang"))

(load-relative "beg-adv.rktl")
(load-relative "beg-intml.rktl")
(load-relative "bega-adv.rktl")
(load-relative "intm-intml.rktl")
(load-relative "intm-adv.rktl")

(report-errs)
