;; A "variable" is a linklet import or export

(define undefined (gensym "undefined"))

(define-record-type variable (fields (mutable val) name))

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
    (string-append (symbol->string (variable-name var))
                   ": undefined;\n cannot reference undefined identifier")
    (current-continuation-marks))))
