
(namespace-variable-value 
 'quiet-load
 #f
 (lambda ()
   (namespace-set-variable-value! 'quiet-load "all.ss")))

(let ([p (make-output-port 'quiet 
			   always-evt
			   (lambda (str s e nonblock? breakable?) (- e s))
			   void)])
  (parameterize ([current-output-port p])
      (load-relative quiet-load))
  (report-errs))
