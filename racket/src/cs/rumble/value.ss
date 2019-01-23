
;; Declare/enforce that an expression has a single result, which can
;; enable the optimizer to infer that an enclosing procedure always
;; has a single result. Beware that the subexpression of `$value`
;; is not in tail position.
(define-syntax-rule ($value x)
  (#%$value x))
