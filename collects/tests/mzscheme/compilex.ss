
; Tests simple compilation by setting the eval handler and
;  running all tests

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(orig (compile x)))))
   (lambda ()
     (load "all.ss"))
   (lambda ()
     (current-eval orig))))
