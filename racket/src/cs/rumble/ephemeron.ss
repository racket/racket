
;; A wrapper to hide the pairness of ephemeron pairs:
(define-record-type (ephemeron create-ephemeron ephemeron?)
  (fields p))

(define (make-ephemeron key val)
  (create-ephemeron (ephemeron-cons key val)))

(define/who ephemeron-value
  (case-lambda
   [(e) (ephemeron-value e #f)]
   [(e gced-v)
    (check who ephemeron? e)
    (let ([v (cdr (ephemeron-p e))])
      (if (eq? v #!bwp)
          gced-v
          v))]
   [(e gced-v keep-live)
    (let ([v (ephemeron-value e gced-v)])
      (#%$keep-live keep-live)
      v)]))
