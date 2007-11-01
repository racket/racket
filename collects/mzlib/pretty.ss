;; Originally:
;; "genwrite.scm" generic write used by pp.scm
;; copyright (c) 1991, marc feeley

;; Pretty-printer for MzScheme
;;  Handles structures, cycles, and graphs

;; TO INSTALL this pretty-printer into a MzScheme's read-eval-print loop,
;; require this module and evaluate:
;;      (current-print pretty-print-handler)

(module pretty mzscheme
   (require (lib "port.ss" "mzlib" "private"))

   (provide pretty-print
	    pretty-display
	    pretty-print-columns
	    pretty-print-depth
	    pretty-print-handler
	    pretty-print-size-hook
	    pretty-print-print-hook
	    pretty-print-pre-print-hook
	    pretty-print-post-print-hook
	    pretty-print-print-line
	    pretty-print-show-inexactness
	    pretty-print-exact-as-decimal
	    pretty-print-.-symbol-without-bars
            pretty-print-abbreviate-read-macros
            
	    pretty-print-style-table?
	    pretty-print-current-style-table
	    pretty-print-extend-style-table
            pretty-print-remap-stylable
            
            pretty-format
	    pretty-printing
	    pretty-print-newline
	    make-tentative-pretty-print-output-port
	    tentative-pretty-print-port-transfer
	    tentative-pretty-print-port-cancel)

   (define-struct pretty-print-style-table (hash))

   (define pretty-print-extend-style-table
     (lambda (table symbols like-symbols)
       (let ([terr (lambda (kind which)
                     (raise-type-error
                      'pretty-print-extend-style-table
                      kind
                      which
                      table symbols like-symbols))])
         (unless (or (not table) (pretty-print-style-table? table))
           (terr "pretty-print style table or #f" 0))
         (unless (and (list? symbols)
                      (andmap symbol? symbols))
           (terr "list of symbols" 1))
         (unless (and (list? like-symbols)
                      (andmap symbol? like-symbols))
           (terr "list of symbols" 1))
         (unless (= (length symbols) (length like-symbols))
           (raise-mismatch-error
            'pretty-print-extend-style-table
            (format "length of first list (~a) doesn't match the length of the second list (~a): "
                    (length symbols) (length like-symbols))
            like-symbols)))
       (let ([ht (if table (pretty-print-style-table-hash table) (make-hash-table))]
             [new-ht (make-hash-table)])
         (hash-table-for-each
          ht
          (lambda (key val)
            (hash-table-put! new-ht key val)))
         (for-each
          (lambda (symbol like-symbol)
            (let ((s (hash-table-get ht
                                     like-symbol
                                     (lambda () #f))))
              (hash-table-put! new-ht symbol (or s like-symbol))))
          symbols like-symbols)
         (make-pretty-print-style-table new-ht))))

  (define pretty-print-abbreviate-read-macros (make-parameter #t))
  
   (define pretty-print-current-style-table 
     (make-parameter 
      (pretty-print-extend-style-table #f null null)
      (lambda (s)
	(unless (pretty-print-style-table? s)
	  (raise-type-error
	   'pretty-print-current-style-table 
	   "pretty-print style table"
	   s))
	s)))

   (define pretty-print-.-symbol-without-bars
     (make-parameter #f (lambda (x) (and x #t))))

   (define pretty-print-show-inexactness 
     (make-parameter #f
		     (lambda (x) (and x #t))))

   (define pretty-print-exact-as-decimal
     (make-parameter #f
		     (lambda (x) (and x #t))))

   (define pretty-print-columns 
     (make-parameter 79
		     (lambda (x)
		       (unless (or (eq? x 'infinity)
				   (integer? x))
			       (raise-type-error 
				'pretty-print-columns
				"integer or 'infinity"
				x))
		       x)))

   (define pretty-print-depth
     (make-parameter #f
		     (lambda (x)
		       (unless (or (not x) (number? x))
			       (raise-type-error 
				'pretty-print-depth
				"number or #f"
				x))
		       x)))

   (define can-accept-n?
     (lambda (n x)
       (procedure-arity-includes? x n)))
   
   (define pretty-print-size-hook
     (make-parameter (lambda (x display? port) #f)
		     (lambda (x)
		       (unless (can-accept-n? 3 x)
			       (raise-type-error 
				'pretty-print-size-hook
				"procedure of 3 arguments"
				x))
		       x)))

   (define pretty-print-print-hook
     (make-parameter void
		     (lambda (x)
		       (unless (can-accept-n? 3 x)
			       (raise-type-error 
				'pretty-print-print-hook
				"procedure of 3 arguments"
				x))
		       x)))

   (define pretty-print-print-line
     (make-parameter (lambda (line port offset width)
		       (when (and (number? width)
				  (not (eq? 0 line)))
			     (newline port))
		       0)
		     (lambda (x)
		       (unless (can-accept-n? 4 x)
			       (raise-type-error 
				'pretty-print-print-line
				"procedure of 4 arguments"
				x))
		       x)))

   (define pretty-print-pre-print-hook
     (make-parameter void
		     (lambda (x)
		       (unless (can-accept-n? 2 x)
			       (raise-type-error 
				'pretty-print-pre-print-hook
				"procedure of 2 arguments"
				x))
		       x)))

   (define pretty-print-post-print-hook
     (make-parameter void
		     (lambda (x)
		       (unless (can-accept-n? 2 x)
			       (raise-type-error 
				'pretty-print-post-print-hook
				"procedure of 2 arguments"
				x))
		       x)))

   (define pretty-printing
     (make-parameter #f (lambda (x) (and x #t))))
  
  (define pretty-print-remap-stylable
    (make-parameter (λ (x) #f) 
                    (λ (f) 
                      (unless (can-accept-n? 1 f)
                        (raise-type-error
                         'pretty-print-remap-stylable
                         "procedure of 1 argument"
                         f))
                      (λ (x)
                        (let ([res (f x)])
                          (unless (or (not res) (symbol? res))
                            (raise-type-error
                             'pretty-print-remap-stylable
                             "result of parameter function to be a symbol or #f"
                             res))
                          res)))))

   (define make-pretty-print
     (lambda (display?)
       (letrec ([pretty-print
		 (case-lambda 
		  [(obj port)
		   (let ([width (pretty-print-columns)]
			 [size-hook (pretty-print-size-hook)]
			 [print-hook (pretty-print-print-hook)]
			 [pre-hook (pretty-print-pre-print-hook)]
			 [post-hook (pretty-print-post-print-hook)])
		     (generic-write obj display?
				    width
				    (make-printing-port port 
							pre-hook
							post-hook
							print-hook
							(pretty-print-print-line))
				    (print-graph) (print-struct) (print-hash-table)
				    (and (not display?) (print-vector-length)) (print-box)
				    (pretty-print-depth)
				    (lambda (o display?)
				      (size-hook o display? port)))
		     (void))]
		  [(obj) (pretty-print obj (current-output-port))])])
	 pretty-print)))

   (define pretty-print (make-pretty-print #f))
   (define pretty-display (make-pretty-print #t))

   (define-struct mark (str def))
   (define-struct hide (val))

   (define (make-tentative-output-port pport width esc)
     (let* ([content null]
	    [special-ok? (port-writes-special? pport)]
	    ;; The null device counts for us:
	    [/dev/null (let-values ([(line col pos) (port-next-location pport)])
			 (relocate-output-port 
			  (let ([p (open-output-nowhere special-ok?)])
			    (port-count-lines! p)
			    p)
			  (or line 1) (or col 0) (or pos 1)))]
	    [check-esc (lambda ()
			 (let-values ([(l c p) (port-next-location /dev/null)])
			   (when (c . > . width)
			     (esc))))]
	    [p (make-output-port
		'tentative
		always-evt
		(lambda (s start end block? break?)
		  (write-bytes s /dev/null start end)
		  (check-esc)
		  (set! content (cons (subbytes s start end) content))
		  (- end start))
		void
		(and special-ok?
		     (lambda (special block break?)
		       (write-special special /dev/null)
		       (check-esc)
		       (set! content (cons (cons 'special special) content))
		       #t))
		#f #f
		(lambda ()
		  (port-next-location /dev/null)))]
	    [first-line? #t])
       (port-count-lines! /dev/null)
       (port-count-lines! p)
       (register-printing-port p 
			       (make-print-port-info
				(lambda () (reverse content))
				(box #t)
				(lambda (v)
				  (set! content (cons (cons 'pre v) content)))
				(lambda (v)
				  (set! content (cons (cons 'post v) content)))
				(lambda (v len display?)
				  (display (make-string len #\.) /dev/null)
				  (set! content (cons (list* 'hooked v len display?)
						      content)))
				(lambda (use-line? offset width)
				  (when (and (number? width)
					     (not first-line?))
				    (newline p))
				  (set! first-line? #f)
				  0)
				esc))
       p))

   (define (make-tentative-pretty-print-output-port pport width esc)
     (let ([p (make-tentative-output-port pport width esc)])
       (port-write-handler p (port-write-handler pport))
       (port-display-handler p (port-display-handler pport))
       (port-print-handler p (port-print-handler pport))
       p))
   
   (define (make-printing-port port pre-print post-print output-hooked print-line)
     (let-values ([(line col pos) (port-next-location port)])
       (let* ([orig-counts? (and line col pos)]
	      [p (if orig-counts?
		     (relocate-output-port port line col pos #f)
		     (transplant-output-port port #f 1 #f))]
	      [line -1])
	 (port-count-lines! p)
	 (register-printing-port p 
				 (make-print-port-info 
				  (lambda () null)
				  (box #t)
				  (lambda (v)
				    (pre-print v port))
				  (lambda (v)
				    (post-print v port))
				  (lambda (v len display?)
				    (output-hooked v display? p))
				  (lambda (use-line? offset width)
				    (set! line (add1 line))
				    (print-line (and use-line? line) p offset width))
				  void))
	 p)))

   (define printing-ports (make-hash-table 'weak))

   (define-struct print-port-info (get-content
				   def-box
				   pre-print
				   post-print
				   output-hooked
				   print-line
				   esc))

   (define (register-printing-port p info)
     (hash-table-put! printing-ports p (make-ephemeron p info)))

   (define (register-printing-port-like p pport)
     (hash-table-put! printing-ports p 
		      (make-ephemeron p
				      (ephemeron-value (hash-table-get printing-ports pport)))))

   (define (get pport selector)
     (let ([e (hash-table-get printing-ports pport (lambda () #f))])
       (selector (if e
		     (ephemeron-value e)
		     (make-print-port-info
		      (lambda () null)
		      (box #t)
		      void void void void void)))))

   (define (printing-port-pre-print pport)
     (get pport print-port-info-pre-print))
   (define (printing-port-post-print pport)
     (get pport print-port-info-post-print))
   (define (printing-port-def-box pport)
     (get pport print-port-info-def-box))
   (define (printing-port-output-hooked pport)
     (get pport print-port-info-output-hooked))
   (define (printing-port-print-line pport)
     (get pport print-port-info-print-line))
   (define (printing-port-esc pport)
     (get pport print-port-info-esc))

   (define orig-display (port-display-handler (open-output-string)))
   (define orig-write (port-write-handler (open-output-string)))
   
   (define (pretty-print-newline pport width)
     (let-values ([(l col p) (port-next-location pport)])
       ((printing-port-print-line pport) #t (or col 0) width)))

   (define (tentative-pretty-print-port-transfer a-pport pport)
     (let ([content ((get a-pport print-port-info-get-content))])
       (for-each (lambda (elem)
		   (if (bytes? elem)
		       (write-bytes elem pport)
		       (case (car elem)
			 [(special) (write-special (cdr elem) pport)]
			 [(pre) ((printing-port-pre-print pport) (cdr elem))]
			 [(post) ((printing-port-post-print pport) (cdr elem))]
			 [(hooked) ((printing-port-output-hooked pport) 
				    (cadr elem) (caddr elem) (cdddr elem))])))
		 content)))
   
   (define (tentative-pretty-print-port-cancel pport)
     (set-box! (get pport print-port-info-def-box) #f))

   (define (add-spaces n port)
     (if (> n 0)
	 (if (> n 7)
	     (begin
	       (write-string "        " port)
	       (add-spaces (- n 8) port))
	     (write-string "        " port 0 n))))

   (define (generic-write obj display? width pport
			  print-graph? print-struct? print-hash-table? print-vec-length? print-box?
			  depth size-hook)
     
     (define table (make-hash-table)) ; Hash table for looking for loops

     (define show-inexactness? (pretty-print-show-inexactness))
     (define exact-as-decimal? (pretty-print-exact-as-decimal))
     
     (define vector->repeatless-list
       (if print-vec-length?
	   (lambda (v)
	     (let ([len (vector-length v)])
               (if (zero? len)
                   null
                   (let ([last (vector-ref v (sub1 len))])
                     (let loop ([i (- len 2)])
                       (if (i . < . 0)
                           (list last)
                           (let ([e (vector-ref v i)])
                             (if (eq? e last)
                                 (loop (sub1 i))
                                 (let loop ([i (sub1 i)][r (list e last)])
                                   (if (i . < . 0)
                                       r
                                       (loop (sub1 i) (cons (vector-ref v i) r))))))))))))
           vector->list))

     (define (extract-sub-objects obj pport)
       (let ([p (open-output-nowhere 'null (port-writes-special? pport))]
	     [l null])
	 (let ([record (lambda (o p) (set! l (cons o l)))])
	   (port-write-handler p record)
	   (port-display-handler p record)
	   (port-print-handler p record))
	 (parameterize ([pretty-printing #f])
	   ((custom-write-accessor obj) obj p #f))
	 l))

     (define found-cycle
       (or print-graph?
	   (let loop ([obj obj])
	     (and (or (vector? obj)
		      (pair? obj)
		      (and (box? obj)
                           print-box?)
		      (and (custom-write? obj)
			   (not (struct-type? obj)))
		      (and (struct? obj) print-struct?)
		      (and (hash-table? obj) print-hash-table?))
		  (or (hash-table-get table obj (lambda () #f))
		      (begin
			(hash-table-put! table obj #t)
			(let ([cycle
			       (cond
				[(vector? obj)
				 (let ([len (vector-length obj)])
				   (let vloop ([i 0])
				     (if (= i len)
					 #f
					 (or (loop (vector-ref obj i))
					     (vloop (add1 i))))))]
				[(pair? obj)
				 (or (loop (car obj))
				     (loop (cdr obj)))]
				[(and (box? obj) print-box?) (loop (unbox obj))]
				[(and (custom-write? obj)
				      (not (struct-type? obj)))
				 (loop (extract-sub-objects obj pport))]
				[(struct? obj)
				 (ormap loop 
					(vector->list (struct->vector obj)))]
				[(hash-table? obj)
				 (let/ec esc
				   (hash-table-for-each
				    obj
				    (lambda (v k)
				      (when (or (loop v)
						(loop k))
					(esc #t))))
				   #f)])])
			  (hash-table-remove! table obj)
			  cycle)))))))

     (define __dummy__
       (when found-cycle
	 (let loop ([obj obj])
	   (if (or (vector? obj)
		   (pair? obj)
		   (and (box? obj)
                        print-box?)
		   (and (custom-write? obj)
			(not (struct-type? obj)))
		   (and (struct? obj) print-struct?)
		   (and (hash-table? obj) print-hash-table?))
	       ;; A little confusing: use #t for not-found
	       (let ([p (hash-table-get table obj (lambda () #t))])
		 (when (not (mark? p))
		   (if p
		       (begin
			 (hash-table-put! table obj #f)
			 (cond
			  [(vector? obj)
			   (let ([len (vector-length obj)])
			     (let vloop ([i 0])
			       (unless (= i len)
				 (loop (vector-ref obj i))
				 (vloop (add1 i)))))]
			  [(pair? obj)
			   (loop (car obj))
			   (loop (cdr obj))]
			  [(and (box? obj) print-box?) (loop (unbox obj))]
			  [(and (custom-write? obj)
				(not (struct-type? obj)))
			   (loop (extract-sub-objects obj pport))]
			  [(struct? obj)
			   (for-each loop 
				     (vector->list (struct->vector obj)))]
			  [(hash-table? obj)
			   (hash-table-for-each
			    obj
			    (lambda (k v)
			      (loop k)
			      (loop v)))]))
		       (begin
			 (hash-table-put! table obj 
					  (make-mark #f (box #f)))))))))))

     (define cycle-counter 0)

     (define found (if found-cycle
		       table 
		       #f))

     (define dsub1 (lambda (d)
		     (if d
			 (sub1 d)
			 #f)))

     (define (pre-print pport obj)
       ((printing-port-pre-print pport) obj))
     (define (post-print pport obj)
       ((printing-port-post-print pport)
	obj))
     (define (output-hooked pport obj len display?)
       ((printing-port-output-hooked pport)
	obj len display?))

     (define expr-found
       (lambda (pport ref)
	 (let ([n cycle-counter])
	   (set! cycle-counter (add1 cycle-counter))
	   (set-mark-str! ref 
			  (string-append "#"
					 (number->string n)
					 "#"))
	   (set-mark-def! ref (printing-port-def-box pport))
	   (display (string-append "#"
				   (number->string n)
				   "=")
		    pport))))
     
     (define check-expr-found
       (lambda (obj pport check? c-k d-k n-k)
	 (let ([ref (and check? 
			 found
			 (hash-table-get found obj (lambda () #f)))])
	   (if (and ref (unbox (mark-def ref)))
	       (if c-k
		   (c-k (mark-str ref))
		   (display (mark-str ref) pport))
	       (if (and ref d-k)
		   (d-k)
		   (begin
		     (when ref
		       (expr-found pport ref))
		     (n-k)))))))

     (define (write-custom recur obj pport depth display? width)
       (let-values ([(l c p) (port-next-location pport)])
	 (let ([p (relocate-output-port pport l c p)])
	   (port-count-lines! p)
	   (let ([writer (lambda (v port)
			   (recur port v (dsub1 depth) #f))]
		 [displayer (lambda (v port)
			      (recur port v (dsub1 depth) #t))])
	     (port-write-handler p writer)
	     (port-display-handler p displayer)
	     (port-print-handler p writer))
	   (register-printing-port-like p pport)
	   (parameterize ([pretty-printing #t]
			  [pretty-print-columns (or width 'infinity)])
	     ((custom-write-accessor obj) obj p (not display?))))))

     ;; ------------------------------------------------------------
     ;; wr: write on a single line
     (define (wr* pport obj depth display?)

       (define (out str)
	 (write-string str pport))
       
       (define (wr obj depth)
	 (wr* pport obj depth display?))

       (define (wr-expr expr depth)
	 (if (read-macro? expr)
	     (begin
	       (out (read-macro-prefix expr))
	       (wr (read-macro-body expr) depth))
	     (wr-lst expr #t depth)))

       (define (wr-lst l check? depth)
	 (if (pair? l)
	     (check-expr-found 
	      l pport check?
	      #f #f
	      (lambda ()
		(if (and depth (zero? depth))
		    (out "(...)")
		    (begin
		      (out "(")
		      (wr (car l) (dsub1 depth))
		      (let loop ([l (cdr l)])
			(check-expr-found
			 l pport (and check? (pair? l))
			 (lambda (s) (out " . ") (out s) (out ")"))
			 (lambda ()
			   (out " . ")
			   (wr-lst l check? (dsub1 depth))
			   (out ")"))
			 (lambda ()
			   (cond 
			    [(pair? l) 
			     (if (and (eq? (car l) 'unquote)
				      (pair? (cdr l))
				      (null? (cddr l)))
				 (begin
				   (out " . ,")
				   (wr (cadr l) (dsub1 depth))
				   (out ")"))
				 (begin
				   (out " ")
				   (wr (car l) (dsub1 depth))
				   (loop (cdr l))))]
			    [(null? l) (out ")")]
			    [else
			     (out " . ")
			     (wr l (dsub1 depth))
			     (out ")")]))))))))
	     (out "()")))

       (unless (hide? obj)
         (pre-print pport obj))
       (if (and depth 
                (negative? depth)
                (not (hide? obj)))
	   (out "...")
	   
	   (cond 
	    [(size-hook obj display?)
	     => (lambda (len)
		  (output-hooked pport obj len display?))]
	    
	    [(pair? obj) 
	     (wr-expr obj depth)]
	    [(null? obj)
	     (wr-lst obj #f depth)]
	    [(vector? obj)   
	     (check-expr-found
	      obj pport #t
	      #f #f
	      (lambda ()
		(out "#")
		(when print-vec-length?
		  (out (number->string (vector-length obj))))
		(wr-lst (vector->repeatless-list obj) #f depth)))]
	    [(and (box? obj)
                  print-box?)
	     (check-expr-found
	      obj pport #t
	      #f #f
	      (lambda ()
		(out "#&") 
		(wr (unbox obj) (dsub1 depth))))]
	    [(and (custom-write? obj) 
		  (not (struct-type? obj)))
	     (check-expr-found
	      obj pport #t
	      #f #f
	      (lambda ()
		(parameterize ([pretty-print-columns 'infinity])
		  (write-custom wr* obj pport depth display? width))))]
	    [(struct? obj)
	     (if (and print-struct?
		      (not (and depth
				(zero? depth))))
		 (check-expr-found
		  obj pport #t
		  #f #f
		  (lambda ()
		    (out "#")
		    (wr-lst (vector->list (struct->vector obj)) #f (dsub1 depth))))
		 (parameterize ([print-struct #f])
		   ((if display? orig-display orig-write) obj pport)))]
	    [(hash-table? obj)  
	     (if (and print-hash-table?
		      (not (and depth
				(zero? depth))))
		 (check-expr-found
		  obj pport #t
		  #f #f
		  (lambda ()
		    (out (if (hash-table? obj 'equal)
                             "#hash"
                             "#hasheq"))
		    (wr-lst (hash-table-map obj (lambda (k v)
                                                  (cons k (make-hide v))))
                            #f depth)))
		 (parameterize ([print-hash-table #f])
		   ((if display? orig-display orig-write) obj pport)))]
            [(hide? obj)
             (wr* pport (hide-val obj) depth display?)]
	    [(boolean? obj)
	     (out (if obj "#t" "#f"))]
	    [(number? obj)
	     (when (and show-inexactness?
			(inexact? obj))
	       (out "#i"))
	     (out ((if exact-as-decimal?
		       number->decimal-string
		       number->string)
		   obj))]
	    [(and (pretty-print-.-symbol-without-bars)
		  (eq? obj '|.|))
	     (out ".")]
	    [else
	     ((if display? orig-display orig-write) obj pport)]))
       (unless (hide? obj)
         (post-print pport obj)))

     ;; ------------------------------------------------------------
     ;; pp: write on (potentially) multiple lines
     (define (pp* pport obj depth display?)

       (define (pp obj depth)
	 (pp* pport obj depth display?))

       (define (out str)
	 (write-string str pport))
       
       (define (spaces n)
	 (add-spaces n pport))

       (define (ccol)
	 (let-values ([(l col p) (port-next-location pport)])
	   col))

       (define (indent to)
	 (let ([col (ccol)])
	   (if (< to col)
	       (begin
		 (let ([col ((printing-port-print-line pport) #t col width)])
		   (spaces (- to col))))
	       (spaces (max 0 (- to col))))))

       (define (pr obj extra pp-pair depth)
	 ;; may have to split on multiple lines
	 (let* ([can-multi (and width
                                (not (size-hook obj display?))
				(or (pair? obj)
                                    (vector? obj) 
				    (and (box? obj) print-box?)
				    (and (custom-write? obj)
					 (not (struct-type? obj)))
				    (and (struct? obj) print-struct?)
				    (and (hash-table? obj) print-hash-table?)))]
		[graph-ref (if can-multi
			       (and found (hash-table-get found obj (lambda () #f)))
			       #f)]
		[old-counter cycle-counter])
	   (if (and can-multi
		    (or (not graph-ref) 
			(not (unbox (mark-def graph-ref)))))
	       ;; It might be possible to split obj across lines.
	       ;; Try writing the obj, but accumulate the info that goes out
	       ;;  into a-pport
	       (let ([a-pport
		      (let/ec esc
			(letrec ([a-pport (make-tentative-output-port
					   pport 
					   (- width extra)
					   (lambda () (esc a-pport)))])
			  ;; Here's the attempt to write on one line:
			  (wr* a-pport obj depth display?)
			  a-pport))])
		 (let-values ([(l c p) (port-next-location a-pport)])
		   (if (<= c (- width extra))
		       ;; All can be printed on one line, so just dump the
		       ;;  accumulated text
		       (tentative-pretty-print-port-transfer a-pport pport)
		       ;; Doesn't fit on one line, so start over
		       (begin
			 (tentative-pretty-print-port-cancel a-pport)
			 (set! cycle-counter old-counter)
			 (when graph-ref
			   (expr-found pport graph-ref))
			 (pre-print pport obj)
			 (cond
			  [(pair? obj) (pp-pair obj extra depth)]
			  [(vector? obj)
			   (out "#")
			   (when print-vec-length?
			     (out (number->string (vector-length obj))))
			   (pp-list (vector->repeatless-list obj) extra pp-expr #f depth)]
			  [(and (custom-write? obj)
				(not (struct-type? obj)))
			   (write-custom pp* obj pport depth display? width)]
			  [(struct? obj) ; print-struct is on if we got here
			   (out "#")
			   (pp-list (vector->list (struct->vector obj)) extra pp-expr #f depth)]
			  [(hash-table? obj)
			   (out (if (hash-table? obj 'equal)
                                    "#hash"
                                    "#hasheq"))
			   (pp-list (hash-table-map obj cons) extra pp-expr #f depth)]
			  [(and (box? obj) print-box?)
			   (out "#&") 
			   (pr (unbox obj) extra pp-pair depth)])
			 (post-print pport obj)))))
	       ;; Not possible to split obj across lines; so just write directly
	       (wr* pport obj depth display?))))

       (define (pp-expr expr extra depth)
	 (if (read-macro? expr)
	     (begin
	       (out (read-macro-prefix expr))
	       (pr (read-macro-body expr)
		   extra
		   pp-expr
		   depth))
	     (let ((head (car expr)))
	       (if (or (and (symbol? head)
                            (not (size-hook head display?)))
                       ((pretty-print-remap-stylable) head))
		   (let ((proc (style head)))
		     (if proc
			 (proc expr extra depth)
			 (if (> (string-length 
                                 (symbol->string
                                  (if (symbol? head)
                                      head
                                      ((pretty-print-remap-stylable) head))))
				max-call-head-width)
			     (pp-general expr extra #f #f #f pp-expr depth)
			     (pp-list expr extra pp-expr #t depth))))
		   (pp-list expr extra pp-expr #t depth)))))

       (define (wr obj depth)
	 (wr* pport obj depth display?))

       ;; (head item1
       ;;       item2
       ;;       item3)
       (define (pp-call expr extra pp-item depth)
	 (out "(")
	 (wr (car expr) (dsub1 depth))
	 (let ([col (+ (ccol) 1)])
	   (pp-down ")" (cdr expr) col col extra pp-item #t #t depth)))

       ;; (head item1 item2
       ;;   item3
       ;;   item4)
       (define (pp-two-up expr extra pp-item depth)
	 (out "(")
	 (let ([col (ccol)])
	   (wr (car expr) (dsub1 depth))
	   (out " ")
	   (wr (cadr expr) (dsub1 depth))
	   (pp-down ")" (cddr expr) (+ (ccol) 1) (+ col 1) extra pp-item #t #t depth)))

       ;; (head item1
       ;;   item2
       ;;   item3)
       (define (pp-one-up expr extra pp-item depth)
	 (out "(")
	 (let ([col (ccol)])
	   (wr (car expr) (dsub1 depth))
	   (pp-down ")" (cdr expr) (+ (ccol) 1) (+ col 1) extra pp-item #t #t depth)))

       ;; (item1
       ;;  item2
       ;;  item3)
       (define (pp-list l extra pp-item check? depth)
	 (out "(")
	 (let ([col (ccol)])
	   (pp-down ")" l col col extra pp-item #f check? depth)))

       (define (pp-down closer l col1 col2 extra pp-item check-first? check-rest? depth)
	 (let loop ([l l] [icol col1] [check? check-first?])
	   (check-expr-found
	    l pport (and check? (pair? l))
	    (lambda (s) 
	      (indent col2)
	      (out ".")
	      (indent col2)
	      (out s)
	      (out closer))
	    (lambda ()
	      (indent col2)
	      (out ".")
	      (indent col2)
	      (pr l extra pp-item depth)
	      (out closer))
	    (lambda ()
	      (cond 
	       [(pair? l)
		(let ([rest (cdr l)])
		  (let ([extra (if (null? rest) (+ extra 1) 0)])
		    (indent icol)
		    (pr (car l) extra pp-item (dsub1 depth))
		    (loop rest col2 check-rest?)))]
	       [(null? l)
		(out closer)]
	       [else
		(indent col2)
		(out ".")
		(indent col2)
		(pr l (+ extra 1) pp-item (dsub1 depth))
		(out closer)])))))

       (define (pp-general expr extra named? pp-1 pp-2 pp-3 depth)

	 (define (tail1 rest col1 col3)
	   (if (and pp-1 (pair? rest))
	       (let* ((val1 (car rest))
		      (rest (cdr rest))
		      (extra (if (null? rest) (+ extra 1) 0)))
		 (indent col3)
		 (pr val1 extra pp-1 depth)
		 (tail2 rest col1 col3))
	       (tail2 rest col1 col3)))

	 (define (tail2 rest col1 col3)
	   (if (and pp-2 (pair? rest))
	       (let* ((val1 (car rest))
		      (rest (cdr rest))
		      (extra (if (null? rest) (+ extra 1) 0)))
		 (indent col3)
		 (pr val1 extra pp-2 depth)
		 (tail3 rest col1))
	       (tail3 rest col1)))

	 (define (tail3 rest col1)
	   (pp-down ")" rest col1 col1 extra pp-3 #f #t depth))

	 (let* ([head (car expr)]
		[rest (cdr expr)]
		[col (ccol)])
	   (out "(")
	   (wr head (dsub1 depth))
	   (if (and named? (pair? rest))
	       (let* ((name (car rest))
		      (rest (cdr rest)))
		 (out " ")
		 (wr name (dsub1 depth))
		 (tail1 rest (+ col indent-general) (+ (ccol) 1)))
	       (tail1 rest (+ col indent-general) (+ (ccol) 1)))))

       (define (pp-expr-list l extra depth)
	 (pp-list l extra pp-expr #t depth))

       (define (pp-lambda expr extra depth)
	 (pp-general expr extra #f pp-expr-list #f pp-expr depth))

       (define (pp-if expr extra depth)
	 (pp-general expr extra #f pp-expr #f pp-expr depth))

       (define (pp-cond expr extra depth)
	 (pp-list expr extra pp-expr-list #t depth))

       (define (pp-class expr extra depth)
	 (pp-two-up expr extra pp-expr-list depth))

       (define (pp-make-object expr extra depth)
	 (pp-one-up expr extra pp-expr-list depth))

       (define (pp-case expr extra depth)
	 (pp-general expr extra #f pp-expr #f pp-expr-list depth))

       (define (pp-and expr extra depth)
	 (pp-call expr extra pp-expr depth))

       (define (pp-let expr extra depth)
	 (let* ((rest (cdr expr))
		(named? (and (pair? rest) (symbol? (do-remap (car rest))))))
	   (pp-general expr extra named? pp-expr-list #f pp-expr depth)))

       (define (pp-begin expr extra depth)
	 (pp-general expr extra #f #f #f pp-expr depth))

       (define (pp-do expr extra depth)
	 (pp-general expr extra #f pp-expr-list pp-expr-list pp-expr depth))

       ;; define formatting style (change these to suit your style)

       (define indent-general 2)

       (define max-call-head-width 5)

       (define (style head)
         (case (look-in-style-table head)
	   ((lambda λ define define-macro define-syntax
		    syntax-rules
		    shared
		    unless when)
	    pp-lambda)
	   ((if set! set!-values)
	    pp-if)
	   ((cond case-lambda)
	    pp-cond)
	   ((case) 
	    pp-case)
	   ((and or import export 
		 require require-for-syntax require-for-template 
		 provide link
		 public private override rename inherit field init)
	    pp-and)
	   ((let letrec let*
	      let-values letrec-values let*-values
	      let-syntax letrec-syntax
	      let-syntaxes letrec-syntaxes)
	    pp-let)
	   ((begin begin0)
	    pp-begin)
	   ((do letrec-syntaxes+values)
	    pp-do)

	   ((send class syntax-case instantiate module)
	    pp-class)
	   ((make-object)
	    pp-make-object)

	   (else #f)))

       (pr obj 0 pp-expr depth))

     ;; ------------------------------------------------------------
     ;; This is where generic-write's body expressions start

     ((printing-port-print-line pport) #t 0 width)
     (let-values ([(l col p) (port-next-location pport)])
       (if (and width (not (eq? width 'infinity)))
	   (pp* pport obj depth display?)
	   (wr* pport obj depth display?)))
     (let-values ([(l col p) (port-next-location pport)])
       ((printing-port-print-line pport) #f col width)))
  
  (define (look-in-style-table raw-head)
    (let ([head (do-remap raw-head)])
      (or (hash-table-get (pretty-print-style-table-hash
                           (pretty-print-current-style-table))
                          head
                          #f)
          head)))
  
  (define (do-remap raw-head)
    (cond
      [((pretty-print-remap-stylable) raw-head)
       => 
       values]
      [else raw-head]))
   
   (define (read-macro? l)
     (define (length1? l) (and (pair? l) (null? (cdr l))))
     (and (pretty-print-abbreviate-read-macros)
          (let ((head (car l)) (tail (cdr l)))
            (case head
              ((quote quasiquote unquote unquote-splicing syntax)
               (length1? tail))
              (else #f)))))
   
   (define (read-macro-body l)
     (cadr l))
  
   (define (read-macro-prefix l)
     (let ((head (car l)))
       (case head
	 ((quote)             "'")
	 ((quasiquote)        "`")
	 ((unquote)           ",")
	 ((unquote-splicing)  ",@")
	 ((syntax)            "#'")
	 ((unsyntax)          "#,")
	 ((unsyntax-splicing) "#,@"))))
  
   (define pretty-print-handler
     (lambda (v)
       (unless (void? v)
	       (pretty-print v))))

   (define (number->decimal-string x)
     (cond
      [(or (inexact? x)
	   (integer? x))
       (number->string x)]
      [(not (real? x))
       (let ([r (real-part x)]
	     [i (imag-part x)])
	 (format "~a~a~ai"
		 (number->decimal-string r)
		 (if (negative? i)
		     ""
		     "+")
		 (number->decimal-string i)))]
      [else
       (let ([n (numerator x)]
	     [d (denominator x)])
	 ;; Count powers of 2 in denomintor
	 (let loop ([v d][2-power 0])
	   (if (and (positive? v)
		    (even? v))
	       (loop (arithmetic-shift v -1) (add1 2-power))
	       ;; Count powers of 5 in denominator
	       (let loop ([v v][5-power 0])
		 (if (zero? (remainder v 5))
		     (loop (quotient v 5) (add1 5-power))
		     ;; No more 2s or 5s. Anything left?
		     (if (= v 1)
			 ;; Denominator = (* (expt 2 2-power) (expt 5 5-power)).
			 ;; Print number as decimal.
			 (let* ([10-power (max 2-power 5-power)]
				[scale (* (expt 2 (- 10-power 2-power))
					  (expt 5 (- 10-power 5-power)))]
				[s (number->string (* (abs n) scale))]
				[orig-len (string-length s)]
				[len (max (add1 10-power) orig-len)]
				[padded-s (if (< orig-len len)
					      (string-append
					       (make-string (- len orig-len) #\0)
					       s)
					      s)])
			   (format "~a~a.~a"
				   (if (negative? n) "-" "")
				   (substring padded-s 0 (- len 10-power))
				   (substring padded-s (- len 10-power) len)))
			 ;; d has factor(s) other than 2 and 5.
			 ;; Print as a fraction.
			 (number->string x)))))))]))
  
  (define pretty-format
    (case-lambda
      [(t) (pretty-format t (pretty-print-columns))]
      [(t w)
       (parameterize ([pretty-print-columns w])
         (let ([op (open-output-string)])
           (pretty-print t op)
           (let ([s (get-output-string op)])
             (if (eq? w 'infinity)
                 s
                 (substring s 0 (- (string-length s) 1))))))]))

  
  )

