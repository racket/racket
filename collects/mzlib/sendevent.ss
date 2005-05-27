
(module sendevent mzscheme 
  (require "etc.ss")
  (provide send-event)
  
  (define send-event 
    (opt-lambda (who class msg [data (void)] [args null])
      (let ([send-event (with-handlers ([exn:fail? (lambda (x) #f)])
			  (dynamic-require '(lib "mred.ss" "mred") 
					   'send-event))])
	(if send-event
	    (send-event who class msg data args)
	    (raise
	     (make-exn:fail:unsupported
	      "send-event: only supported in MrEd"
	      (current-continuation-marks))))))))
