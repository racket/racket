(module oleg-string-ports mzscheme

  (provide with-output-to-string
	   call-with-input-string
	   with-input-from-string)

  (begin
    (define (with-output-to-string thunk)
      (let ((port (open-output-string)))
	(parameterize ((current-output-port port))
	    (thunk)
	    (get-output-string port))))
    (define (call-with-input-string string proc)
      (proc (open-input-string string)))
    (define (with-input-from-string string thunk)
      (parameterize ((current-input-port (open-input-string string)))
	(thunk)))))

