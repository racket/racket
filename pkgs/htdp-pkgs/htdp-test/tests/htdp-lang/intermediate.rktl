
;; Basic checks for the intermediate language. See also
;;  beginner.rktl

(load-relative (collection-file-path "loadtest.rktl" "tests/racket"))

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
;; based on new docs: 
(require (submod lang/htdp-intermediate procedures))
(for ((s (docs)))
  (for ((rows (cdr s)))
    (for ((r rows))
      (define sy (syntax-e (car r)))
      (define vv (dynamic-require 'lang/htdp-intermediate sy))
      (when (procedure? vv) 
	(test sy object-name vv)))))

;; based on old docs: 
#;
(require syntax/docprovide)
#;
(let ([docs (lookup-documentation '(lib "htdp-intermediate.rkt" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "htdp-intermediate.rkt" "lang") (car doc))])
	  (when (procedure? v)
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(define current-htdp-lang 'lang/htdp-intermediate)
(load-relative "htdp-test.rktl")

(require (lib "htdp-intermediate.rkt" "lang"))

(load-relative "beg-adv.rktl")
(load-relative "beg-intml.rktl")
(load-relative "beg-intm.rktl")
(load-relative "bega-adv.rktl")
(load-relative "intm-intml.rktl")
(load-relative "intm-adv.rktl")

(htdp-syntax-test #'(local [(lambda (x) x)] 1) "lambda: found a lambda that is not a function definition")

(report-errs)
