;; A "variable" is a linklet import or export

(define undefined '#{undefined bjjxts6iq4xqtw8kz4eb1jxbs-0})

(define-record-type variable
  (fields (mutable val) name)
  (sealed #t))

(define (variable-set! var val)
  (variable-val-set! var val))

(define (variable-ref var)
  (define v (variable-val var))
  (if (eq? v undefined)
      (raise-undefined var)
      v))

(define (raise-undefined var)
  (raise
   (|#%app|
    exn:fail:contract:variable
    (error-message->adjusted-string
     (variable-name var) 'local
     "undefined;\n cannot reference undefined identifier"
     primitive-realm)
    (current-continuation-marks))))
