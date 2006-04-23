
(module name mzscheme
  (provide syntax-local-infer-name)
  
  (define (syntax-local-infer-name stx)
    (or (syntax-property stx 'inferred-name)
	(let ([n (syntax-local-name)])
	  (or n
	      (let ([s (syntax-source stx)])
		(and s
		     (let ([s (let ([s (format
					"~a"
					(cond
					 [(path? s) (path->string s)]
					 [else s]))])
				(if ((string-length s) . > . 20)
				    (string-append "..." (substring s (- (string-length s) 20)))
				    s))]
			   [l (syntax-line stx)]
			   [c (syntax-column stx)])
		       (if l
			   (string->symbol (format "~a:~a:~a" s l c))
			   (let ([p (syntax-position stx)])
			     (string->symbol (format "~a::~a" s p))))))))))))

				     
		
	  