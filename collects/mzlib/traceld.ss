
(module traceld mzscheme

 (let ([load (current-load)]
       [load-extension (current-load-extension)]
       [ep (current-error-port)]
       [tab ""])
   (let ([mk-chain
	  (lambda (load)
	    (lambda (filename expected-module)
	      (fprintf ep
		       "~aloading ~a at ~a~n" 
		       tab filename (current-process-milliseconds))
	      (begin0
	       (let ([s tab])
		 (dynamic-wind
		  (lambda () (set! tab (string-append " " tab)))
		  (lambda () 
		    (if (regexp-match #rx#"_loader" (path->bytes filename))
			(let ([f (load filename #f)])
			  (lambda (sym)
			    (fprintf ep
				     "~atrying ~a's ~a~n" tab filename sym)
			    (let-values ([(loader provided-module) (f sym)])
                              (values
                               (and loader
                                    (lambda ()
                                      (fprintf ep
                                               "~astarting ~a's ~a at ~a~n" 
                                               tab filename sym
                                               (current-process-milliseconds))
                                      (let ([s tab])
                                        (begin0
                                          (dynamic-wind
                                           (lambda () (set! tab (string-append " " tab)))
                                           (lambda () (loader))
                                           (lambda () (set! tab s)))
                                          (fprintf ep
                                                   "~adone ~a's ~a at ~a~n"
                                                   tab filename sym
                                                   (current-process-milliseconds))))))
                               provided-module))))
			(load filename expected-module)))
		  (lambda () (set! tab s))))
	       (fprintf ep
			"~adone ~a at ~a~n"
			tab filename (current-process-milliseconds)))))])
     (current-load (mk-chain load))
     (current-load-extension (mk-chain load-extension)))))
