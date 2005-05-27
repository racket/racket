; Look up a value associated with a symbolic key in alist 
; ((key value) ...) or ((key . value) ...)
; and return the associated value.
; If the association has the form
;   (key . value) where value is not a pair --> return value
;   (key   value)                           --> return value
;   (key value1 value2 value3 ...) -> return (value1 value2 value3 ...)
; that is, the procedure tries to do the right thing for
; both kinds of associative lists. 
;
; The form `lookup-def' is a special form rather than a regular
; procedure. Its first two arguments are evaluated exactly once. The
; default-value argument, if given, is evaluated only if the desired key
; is not found. I have not seen any need to pass `lookup-def' as an
; argument to other functions. If the latter is desired, it is not
; difficult to accomplish by explicitly wrapping `lookup-def' into a
; lambda form.
;
; We use a pseudo-keyword argument warn: as a modifier.
; This is not really a keyword argument (although it may be,
; if the Scheme system turns out DSSSL-compatible)
; 
; (lookup-def key alist)  -- lookup the key in the alist and return the
;                        associated value. Raise an error if the key is not
;                        found.
; (lookup-def key alist default-exp)
;                     -- lookup the key in the alist and return the associated
;                        value. If the the key is not found, evaluate
;                        the default-exp and return its result.
; (lookup-def key alist warn: default-exp)
;                     -- the same as above. In addition, write a warning
;                        (using cerr above) if the key is not found.

(define-syntax lookup-def 
  (syntax-rules (warn:)
    ((lookup-def key alist)
      (let ((nkey key) (nalist alist)) ; evaluate them only once
	(let ((res (assq nkey nalist)))
	  (if res
	    (let ((res (cdr res)))
	     (cond
	       ((not (pair? res)) res)
	       ((null? (cdr res)) (car res))
	       (else res)))
	    (error "Failed to find " nkey " in " nalist)))))
    ((lookup-def key alist default-exp)
      (let ((res (assq key alist)))
	(if res
	  (let ((res (cdr res)))
	    (cond
	      ((not (pair? res)) res)
	      ((null? (cdr res)) (car res))
	      (else res)))
	  default-exp)))
    ((lookup-def key alist warn: default-exp)
      (let ((nkey key) (nalist alist)) ; evaluate them only once
	(let ((res (assq nkey nalist)))
	  (if res
	    (let ((res (cdr res)))
	     (cond
	       ((not (pair? res)) res)
	       ((null? (cdr res)) (car res))
	       (else res)))
	    (begin
	      (cerr "Failed to find " nkey " in " nalist #\newline)
	      default-exp)))))
    ))
