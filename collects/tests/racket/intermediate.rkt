
;; Basic checks for the intermediate language. See also
;;  beginner.rkt

(load-relative "loadtest.rkt")

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
(require syntax/docprovide)
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
(load-relative "htdp-test.rkt")

(require (lib "htdp-intermediate.rkt" "lang"))

(load-relative "beg-adv.rkt")
(load-relative "beg-intml.rkt")
(load-relative "beg-intm.rkt")
(load-relative "bega-adv.rkt")
(load-relative "intm-intml.rkt")
(load-relative "intm-adv.rkt")

(report-errs)
