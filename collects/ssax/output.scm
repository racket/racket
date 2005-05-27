; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
		  (x (current-error-port))
		  (display x (current-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))
