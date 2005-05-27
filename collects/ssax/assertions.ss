(module assertions mzscheme

  (provide assert assure)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

  (define-syntax assert
    (syntax-rules (report:)
      ((assert "doit" (expr ...) (r-exp ...))
       (cond
	((and expr ...) => (lambda (x) x))
	(else
	 (error 'error "assertion failure: ~a" (list '(and expr ...) r-exp ...)))))
      ((assert "collect" (expr ...))
       (assert "doit" (expr ...) ()))
      ((assert "collect" (expr ...) report: r-exp ...)
       (assert "doit" (expr ...) (r-exp ...)))
      ((assert "collect" (expr ...) expr1 stuff ...)
       (assert "collect" (expr ... expr1) stuff ...))
      ((assert stuff ...)
       (assert "collect" () stuff ...))))

  (define-syntax assure
    (syntax-rules ()
      ((assure exp error-msg)
       (assert exp report: error-msg)))))