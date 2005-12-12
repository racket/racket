
;; A few simple port functions are needed in pretty.ss, which is
;;  used by contract.ss, which is used by port.ss --- so we
;;  break the cycle with this module.

(module port mzscheme
  (require "../etc.ss")
  (provide open-output-nowhere
	   relocate-output-port
	   transplant-output-port
	   transplant-to-relocate)

  (define open-output-nowhere
    (opt-lambda ([name 'nowhere] [specials-ok? #t])
      (make-output-port 
       name
       always-evt
       (lambda (s start end non-block? breakable?) (- end start))
       void
       (and specials-ok?
	    (lambda (special non-block? breakable?) #t))
       (lambda (s start end) (wrap-evt
			      always-evt
			      (lambda (x)
				(- end start))))
       (and specials-ok?
	    (lambda (special) 
	      (wrap-evt always-evt (lambda (x) #t)))))))

  (define (transplant-to-relocate transplant p line col pos close?)
    (let-values ([(init-l init-c init-p) (port-next-location p)])
      (transplant
       p
       (lambda ()
	 (let-values ([(l c p) (port-next-location p)])
	   (values (and l init-l (+ l (- init-l) line))
		   (and c init-c (if (equal? l init-l)
				     (+ c (- init-c) col)
				     c))
		   (and p init-p (+ p (- init-p) pos)))))
       pos
       close?)))

  (define relocate-output-port
    (opt-lambda (p line col pos [close? #t])
      (transplant-to-relocate
       transplant-output-port
       p line col pos close?)))
  
  (define transplant-output-port
    (opt-lambda (p location-proc pos [close? #t] [count-lines!-proc void])
      (make-output-port
       (object-name p)
       p
       (lambda (s start end nonblock? breakable?)
	 (let ([v ((if nonblock? 
		       write-bytes-avail*
		       (if breakable?
			   write-bytes-avail/enable-break
			   write-bytes-avail))
		   s p start end)])
	   (if (and (zero? v) (not (= start end)))
	       (wrap-evt p (lambda (x) #f))
	       v)))
       (lambda ()
	 (when close?
	   (close-output-port p)))
       (and (port-writes-special? p)
	    (lambda (special nonblock? breakable?)
	      ((if nonblock? 
		   write-special-avail*
		   (if breakable?
		       (lambda (spec p)
			 (parameterize-break #t
			   (write-special spec p)))
		       write-special))
	       special p)))
       (and (port-writes-atomic? p)
	    (lambda (s start end)
	      (write-bytes-avail-evt s p start end)))
       (and (port-writes-atomic? p)
	    (port-writes-special? p)
	    (lambda (spec)
	      (write-special-evt spec p)))
       location-proc
       count-lines!-proc
       pos))))
