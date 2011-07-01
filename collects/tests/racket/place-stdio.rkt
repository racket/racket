#lang racket

(unless (getenv "IN_SUBPLACE")
  (putenv "IN_SUBPLACE" "yes")

  ;; Place that closes stdin shouldn't close stdin in the original
  ;; place:
  (place-wait (place ch 
		     (close-input-port (current-input-port))
		     (close-output-port (current-output-port))
		     (close-output-port (current-error-port))))
  (unless (and (equal? #f (port-closed? (current-input-port)))
	       (equal? #f (port-closed? (current-output-port)))
	       (equal? #f (port-closed? (current-error-port))))
    (error "sub-place port-close test failed"))
  
  ;; Closing only stdin should lead to closed stdin in
  ;; a sub-place:
  (close-input-port (current-input-port))
  (define p (place ch
		   (place-channel-put ch (port-closed? (current-input-port)))))
  (unless (equal? (place-channel-get p) #t)
    (error "closed-stdin test failed")))

