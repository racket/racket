
(define-struct srcloc (source line column position span)
  :guard (lambda (source line column position span who)
           (check who exact-positive-integer? :or-false line)
           (check who exact-nonnegative-integer? :or-false column)
           (check who exact-positive-integer? :or-false position)
           (check who exact-nonnegative-integer? :or-false span)
           (values source line column position span)))

(define-values (prop:exn:srclocs exn:srclocs? exn:srclocs-accessor)
  (make-struct-type-property 'exn:srclocs
                             (lambda (v info)
                               (check 'guard-for-prop:exn:srclocs (procedure-arity-includes/c 1) v)
                               v)))
