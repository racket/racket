(module ((define-bitfields))
	
  (define-syntax define-bitfields
    (lambda (x)
      (define construct-name
	(lambda (template-identifier . args)
	  (datum->syntax template-identifier
	    (string->symbol
	      (apply string-append
		     (map (lambda (x) (format "~a" (syntax->datum x)))
			  args))))))
      (define extract
	(lambda (fld* bit def*)
	  (assert (< bit (fixnum-width)))
	  (if (null? fld*)
	      def*
	      (syntax-case (car fld*) (flag enumeration integer)
		[(flag name) (identifier? #'name)
		 (extract (cdr fld*) (+ bit 1)
		   #`((define name #,(fxsll 1 bit)) #,@def*))]
		[(enumeration name id ...)
		 (and (identifier? #'name) (for-all identifier? #'(id ...)))
		 (let ([width (bitwise-length (length #'(id ...)))])
		   (with-syntax ([name-shift (construct-name #'name #'name "-shift")]
				 [name-mask (construct-name #'name #'name "-mask")]
				 [name-members (construct-name #'name #'name "-members")])
		     (extract (cdr fld*) (+ bit width)
		       #`((define name-shift #,bit)
			  (define name-mask #,(fx- (fxsll 1 width) 1))
			  (define name-members (list 'id ...))
			  #,@(map (lambda (id val) #`(define #,id #,val))
				  #'(id ...)
				  (enumerate #'(id ...)))
			  #,@def*))))]
		[(integer name width) (identifier? #'name)
		 (let ([width (syntax->datum #'width)])
		   (with-syntax ([name-shift (construct-name #'name #'name "-shift")]
				 [name-mask (construct-name #'name #'name "-mask")])
		     (extract (cdr fld*) (+ bit width)
		       #`((define name-shift #,bit)
			  (define name-mask #,(fx- (fxsll 1 width) 1))
			  #,@def*))))]))))
      (syntax-case x ()
	[(_ fld ...)
	 #`(begin #,@(extract #'(fld ...) 0 #'()))]))))
