#lang racket

(module+ test 
  (main))

(define (main)

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
  
  ;; Closing only stdin shouldn't matter; the new place
  ;;  gets a finished stdin:
  (close-input-port (current-input-port))
  (define p (place ch
		   (place-channel-put ch (eof-object? (read-byte (current-input-port))))))
  (unless (equal? (place-channel-get p) #t)
    (error "closed-stdin test failed"))

  ;; Cosed current output port => fail creating place
  (define (try-closed mk)
    (with-handlers ([exn:fail:contract? (lambda (exn)
                                          (unless (regexp-match? #rx"port is closed"
                                                                 (exn-message exn))
                                            (raise exn)))])
      (let ([p (mk)])
        (close-output-port p)
        (parameterize ([current-output-port p])
          (place ch (void)))
        (error (format "closed-stdout test failed on ~e" mk)))))
  (let ([f (make-temporary-file)])
    (dynamic-wind
        void
        (lambda ()
          (try-closed (lambda () (open-output-file f #:exists 'truncate))))
        (lambda ()
          (delete-file f))))
  (try-closed open-output-bytes))
