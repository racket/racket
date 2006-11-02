(module code mzscheme
  (require "mrpict.ss"
	   (lib "class.ss")
	   (lib "list.ss")
           (lib "mred.ss" "mred")
	   (lib "unitsig.ss"))

  (provide define-code code^ code-params^ code@)

  (define-struct (code-pict pict) (bottom-line))


  (define (to-code-pict p extension)
    (make-code-pict (pict-draw p)
		    (pict-width p)
		    (pict-height p)
		    (pict-ascent p)
		    (pict-descent p)
		    (pict-children p)
		    (pict-panbox p)
		    (if (code-pict? extension)
			(code-pict-bottom-line extension)
			extension)))

  (define (make-code-append htl-append)
    (case-lambda
     [(a b) (if (code-pict? a)
		(let ([extension (htl-append (ghost (code-pict-bottom-line a)) b)])
		  (let ([p (lt-superimpose
			    a
			    (let-values ([(x y) (lt-find a (code-pict-bottom-line a))])
			      (inset extension x y 0 0)))])
		    (to-code-pict p (if (code-pict? b)
					(code-pict-bottom-line b)
					extension))))
		(let ([p (htl-append a b)])
		  (if (code-pict? b)
		      (to-code-pict p (code-pict-bottom-line b))
		      p)))]
     [(a) a]
     [(a . rest)
      ((make-code-append htl-append)
       a
       (apply (make-code-append htl-append) rest))]))

  (define code-htl-append (make-code-append htl-append))
  (define code-hbl-append (make-code-append hbl-append))

  (define code-vl-append
    (case-lambda
     [(a b) (to-code-pict (vl-append a b) b)]
     [(a) a]
     [(a . rest)
      (code-vl-append a (apply code-vl-append rest))]))

  (define-syntax (define-code stx)
    (syntax-case stx ()
      [(_ code typeset-code uncode)
       (syntax/loc stx
	 (define-syntax (code stx)
	   (define (stx->loc-s-expr v)
	     (cond
	      [(syntax? v)
	       (let ([mk `(datum->syntax-object
			   #f
			   ,(syntax-case v (uncode)
			      [(uncode e) #'e]
			      [else (stx->loc-s-expr (syntax-e v))])
			   (list 'code
				 ,(syntax-line v)
				 ,(syntax-column v)
				 ,(syntax-position v)
				 ,(syntax-span v)))])
		 (let ([prop (syntax-property v 'paren-shape)])
		   (if prop
		       `(syntax-property ,mk 'paren-shape ,prop)
		       mk)))]
	      [(pair? v) `(cons ,(stx->loc-s-expr (car v))
				,(stx->loc-s-expr (cdr v)))]
	      [(vector? v) `(vector ,@(map
				       stx->loc-s-expr
				       (vector->list v)))]
	      [(box? v) `(box ,(stx->loc-s-expr (unbox v)))]
	      [(null? v) 'null]
	      [else `(quote ,v)]))
	   (define (cvt s)
	     (datum->syntax-object #'here (stx->loc-s-expr s)))
	   (syntax-case stx ()
	     [(_ expr) #`(typeset-code #,(cvt #'expr))]
	     [(_ expr (... ...))
	      #`(typeset-code #,(cvt #'(code:line expr (... ...))))])))]
      [(_ code typeset-code) #'(define-code code typeset-code unsyntax)]))
  
  (define-signature code^
    (typeset-code code-pict-bottom-line-pict pict->code-pict
     comment-color keyword-color id-color const-color literal-color
     code-align current-code-tt
     current-keyword-list current-const-list current-literal-list 
     code-colorize-enabled code-colorize-quote-enabled 
     code-italic-underscore-enabled code-scripts-enabled
     current-comment-color current-keyword-color 
     current-base-color current-id-color current-literal-color current-const-color
     current-reader-forms
     mzscheme-const-list))

  (define-signature code-params^
    (current-font-size 
     line-sep))

  (define-syntax (define-computed stx)
    (syntax-case stx ()
      [(_ id v)
       #'(begin
	   (define (get-val) v)
	   (define-syntax id
	     (syntax-id-rules (set!)
	       [(x (... ...)) ,illegal-use-of-once]
	       [x (get-val)])))]))
  
  (define code@
    (unit/sig code^
      (import code-params^)
      
      (define (default-tt s)
	(text s `(bold . modern) (current-font-size)))

      (define current-code-tt (make-parameter default-tt))

      (define (tt s)
	((current-code-tt) s))

      (define (code-align p)
	(lift (inset p 0 (pict-height p) 0 0) (pict-height p)))

      (define (code-pict-bottom-line-pict p)
	(if (code-pict? p)
	    (code-pict-bottom-line p)
	    #f))

      (define (pict->code-pict p bottom-line)
	(if bottom-line
	    (to-code-pict p bottom-line)
	    p))
      
      (define mzscheme-ns (let ([n (make-namespace 'empty)]
				[orig (current-namespace)])
			    (parameterize ([current-namespace n])
			      (namespace-attach-module orig 'mzscheme)
			      (namespace-require/copy 'mzscheme))
			    n))
      (define mzscheme-bindings (namespace-mapped-symbols mzscheme-ns))
      (define mzscheme-vars (filter (lambda (n)
				      (not (eq? 'nope
						(namespace-variable-value n #f (lambda () 'nope) mzscheme-ns))))
				    mzscheme-bindings))
      
      (define current-keyword-list 
	(make-parameter 
	 (let ([ht (make-hash-table)])
	   (for-each (lambda (n) (hash-table-put! ht n #f))
		     mzscheme-vars)
	   (map symbol->string
		(filter (lambda (n)
			  (hash-table-get ht n (lambda () #t)))
			mzscheme-bindings)))))
      (define current-const-list 
	(make-parameter '()))
      (define current-literal-list 
	(make-parameter '()))

      (define mzscheme-const-list
	(map symbol->string mzscheme-vars))

      (define code-colorize-enabled
	(make-parameter #t))

      (define code-colorize-quote-enabled
	(make-parameter #t))

      (define code-italic-underscore-enabled (make-parameter #t))
      (define code-scripts-enabled (make-parameter #t))

      (define (maybe-colorize p c)
	(if (code-colorize-enabled)
	    (colorize p c)
	    p))
      
      (define current-base-color (make-parameter "brown"))
      (define keyword-color "black")
      (define current-keyword-color (make-parameter keyword-color))
      (define id-color "navy")
      (define current-id-color (make-parameter id-color))
      (define literal-color (make-object color% 51 135 39))
      (define current-literal-color (make-parameter literal-color))
      (define const-color (make-object color% #x99 0 0))
      (define current-const-color (make-parameter const-color))
      (define comment-color (current-base-color))
      (define current-comment-color (make-parameter comment-color))
      (define current-reader-forms (make-parameter '(quote
						     quasiquote 
						     unquote unquote-splicing
						     syntax
						     quasisyntax
						     unsyntax unsyntax-splicing)))
      
      (define-computed open-paren-p (tt "("))
      (define-computed close-paren-p (tt ")"))
      (define-computed open-sq-p (tt "["))
      (define-computed close-sq-p (tt "]"))
      (define-computed open-curly-p (tt "{"))
      (define-computed close-curly-p (tt "}"))

      (define-computed quote-p (tt "'"))
      (define-computed unquote-p (tt ","))
      (define-computed unquote-splicing-p (tt ",@"))
      (define-computed quasiquote-p (tt "`"))
      (define-computed syntax-p (tt "#'"))
      (define-computed unsyntax-p (tt "#,"))
      (define-computed unsyntax-splicing-p (tt "#,@"))
      (define-computed quasisyntax-p (tt "#`"))
      (define-computed semi-p (tt "; "))

      (define (comment-mode? mode)
	(eq? mode 'comment))
      
      (define-computed dot-p (tt "."))

      (define (mode-colorize mode type p)
	(maybe-colorize
	 p
	 (case mode
	   [(literal) (current-literal-color)]
	   [(comment) (current-comment-color)]
	   [else (cond
		  [(number? mode) (current-literal-color)]
		  [(eq? type 'keyword) (current-keyword-color)]
		  [(eq? type 'literal) (current-literal-color)]
		  [(eq? type 'const) (current-const-color)]
		  [(eq? type 'id) (current-id-color)]
		  [else (current-base-color)])])))
	
      (define (get-open mode stx)
	(if (memq mode '(contract line))
	    (blank)
	    (mode-colorize 
	     mode #f
	     (case (syntax-property stx 'paren-shape)
	       [(#\[) open-sq-p]
	       [(#\{) open-curly-p]
	       [else open-paren-p]))))
	
      (define (get-close mode stx)
	(if (memq mode '(contract line))
	    (blank)
	    (mode-colorize
	     mode #f
	     (case (syntax-property stx 'paren-shape)
	       [(#\[) close-sq-p]
	       [(#\{) close-curly-p]
	       [else close-paren-p]))))
      
      (define (add-close p closes)
	(cond
	 [(null? closes) p]
	 [(memq (caar closes) '(contract line))
	  (add-close p (cdr closes))]
	 [else
	  (add-close (code-hbl-append p (get-close (caar closes) (cdar closes)))
		     (cdr closes))]))

      (define (pad-left space p)
	(if (= 0 space)
	    p
	    (code-htl-append (tt (make-string space #\space)) p)))

      (define (pad-bottom space p)
	(if (= 0 space)
	    p
	    (code-vl-append line-sep (tt " ") (pad-bottom (sub1 space) p))))

      (define (colorize-id str mode)
	(cond
	 [(and (code-italic-underscore-enabled)
	       ((string-length str) . > . 1)
	       (char=? #\_ (string-ref str 0))
	       (not (char=? #\_ (string-ref str 1))))
	  (mode-colorize
	   mode 'id
	   (text (substring str 1) `(bold italic . modern) (current-font-size)))]
	 [(and (code-scripts-enabled)
	       (regexp-match #rx"^(.+)_([0-9a-z]+)\\^([0-9a-z]+)$" str))
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (cc-superimpose
			    (text (caddr m) `(subscript bold . modern) (current-font-size))
			    (text (cadddr m) `(superscript bold . modern) (current-font-size)))))]
	 [(and (code-scripts-enabled)
	       (regexp-match #rx"^(.+)\\^([0-9a-z]+)_([0-9a-z]+)$" str))
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (cc-superimpose
			    (text (cadddr m) `(subscript bold . modern) (current-font-size))
			    (text (caddr m) `(superscript bold . modern) (current-font-size)))))]
	 [(and (code-scripts-enabled)
	       (regexp-match #rx"^(.+)\\^([0-9a-z]+)$" str))
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (text (caddr m) `(superscript bold . modern) (current-font-size))))]
	 [(and (code-scripts-enabled)
	       (regexp-match #rx"^(.+)_([0-9a-z]+)$" str))
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (text (caddr m) `(subscript bold . modern) (current-font-size))))]
	 [else
	  (mode-colorize
	   mode 
	   (cond
	    [(member str (current-keyword-list)) 'keyword]
	    [(member str (current-const-list)) 'const]
	    [(member str (current-literal-list)) 'literal]
	    [else 'id])
	   (tt str))]))

      (define (sub-mode mode)
	(case mode
	  [(line cond local) #f]
	  [(template-cond) 'template]
	  [(contract) 'comment]
	  [else mode]))

      (define (cond? s)
	(memq (syntax-e s) '(cond))) ;  syntax-rules syntax-case)))

      (define (local? s)
	(memq (syntax-e s) '(local)))

      (define (get-span stx)
        (syntax-case stx (code:blank)
          [code:blank 1]
          [_ (or (syntax-span stx) 1)]))
      
      (define (color-semi-p)
	(mode-colorize 'comment #f semi-p))

      (define (add-semis p)
	(let loop ([p p] [semis (color-semi-p)])
	  (if ((pict-height p) . > . (+ (pict-height semis) 1))
	      (loop p (vl-append line-sep (color-semi-p) semis))
	      (htl-append semis p))))

      (define (add-unquote unquote-p loop x closes mode)
	(let ([mode (cond
		     [(number? mode) (if (zero? mode)
					 #f
					 (sub1 mode))]
		     [else mode])])
	  (code-htl-append (mode-colorize mode 'keyword unquote-p) 
			   (loop x closes mode))))

      (define (typeset-code stx)
	(let loop ([stx stx][closes null][mode #f])
	  (syntax-case* stx (quote unquote unquote-splicing quasiquote
				   syntax-unquote syntax unsyntax
				   unsyntax-splicing quasisyntax
				   code:contract code:comment code:line
				   code:template code:blank $)
			(lambda (a b) (eq? (syntax-e a) (syntax-e b)))
	    [() (add-close (htl-append (get-open mode stx) (get-close mode stx))
			   closes)]
	    [code:blank (add-close (tt " ")
                                   closes)]
	    [$ (colorize-id "|" closes)]
	    [(quote x)
	     (memq 'quote (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'literal quote-p) 
			      (loop #'x closes (if (or (not (code-colorize-quote-enabled))
						       (comment-mode? mode))
						   mode
						   'literal)))]
	    [(unquote x)
	     (memq 'unquote (current-reader-forms))
	     (add-unquote unquote-p loop #'x closes mode)]
	    [(unquote-splicing x)
	     (memq 'unquote-splicing (current-reader-forms))
	     (add-unquote unquote-splicing-p loop #'x closes mode)]
	    [(quasiquote x)
	     (memq 'quasiquote (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'keyword quasiquote-p) 
			      (loop #'x closes (cond
						[(not (code-colorize-quote-enabled)) mode]
						[(comment-mode? mode) mode]
						[(number? mode) (add1 mode)]
						[else 0])))]
	    [(syntax x)
	     (memq 'syntax (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'literal syntax-p) 
			      (loop #'x closes mode))]
	    [(unsyntax x)
	     (memq 'unsyntax (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'literal unsyntax-p) 
			      (loop #'x closes mode))]
	    [(unsyntax-splicing x)
	     (memq 'unsyntax-splicing (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'literal unsyntax-splicing-p) 
			      (loop #'x closes mode))]
	    [(quasisyntax x)
	     (memq 'unsyntax-splicing (current-reader-forms))
	     (code-htl-append (mode-colorize mode 'literal quasisyntax-p) 
			      (loop #'x closes mode))]
	    [(code:contract i ...)
	     (add-semis (loop (datum->syntax-object #f (syntax->list #'(i ...)))
			      closes 'contract))]
	    [(code:line i ...)
	     (loop (datum->syntax-object #f (syntax->list #'(i ...)))
		   closes 'line)]
	    [(code:comment s ...)
	     (apply htl-append 
		    (color-semi-p)
		    (map (lambda (s)
			   (if (pict? (syntax-e s))
			       (syntax-e s)
			       (maybe-colorize (tt (syntax-e s)) (current-comment-color))))
			 (syntax->list #'(s ...))))]
	    [(code:template i ...)
	     (add-semis (loop #'(code:line i ...) closes 'template))]
	    [(i ...)
	     (let ([is (syntax->list #'(i ...))])
	       ;; Convert each i to a picture, include close paren in last item:
	       (let ([ips (let iloop ([is is][sub-mode (sub-mode mode)])
			    (cond
			     [(null? (cdr is)) (list (loop (car is) (cons (cons mode stx) closes) sub-mode))]
			     [else (cons (loop (car is) null sub-mode)
					 (iloop (cdr is) (cond
							  [(cond? (car is))
							   (if (eq? mode 'template)
							       'template-cond
							       'cond)]
							  [(local? (car is))
							   'local]
							  [(eq? sub-mode 'local)
							   #f]
							  [else
							   sub-mode])))]))])
		 ;; Combine the parts:
		 (let ([left (or (syntax-column stx) +inf.0)])
		   (let loop ([stxs is]
			      [ps ips]
			      [line-so-far (get-open mode stx)]
			      [col (+ left 1)]
			      [line (syntax-line stx)]
			      [always-space? #f]
                              [col->width (make-hash-table 'equal)])
		     (cond
		      [(null? ps) (blank)]
		      [(or (not line)
			   (= line (or (syntax-line (car stxs)) line)))
		       (let* ([space (if (syntax-column (car stxs))
					 (inexact->exact
					  (max (if always-space? 1 0) (- (syntax-column (car stxs)) col)))
					 (if always-space? 1 0))]
			      [p (code-htl-append
				  line-so-far
				  (pad-left space (car ps)))])
			 (unless (equal? +inf.0 (+ space col))
			   (hash-table-put! col->width 
					    (+ space col) 
					    (pict-width (code-htl-append line-so-far (pad-left space (blank))))))
			 (if (null? (cdr stxs))
			     p
			     (loop (cdr stxs)
				   (cdr ps)
				   p
				   (if (not (syntax-column (car stxs)))
				       +inf.0
				       (+ col space (get-span (car stxs))))
				   (or line (syntax-line (car stxs)))
				   #t
                                   col->width)))]
		      [else
		       ;; Start on next line:
		       (code-vl-append
			line-sep
			line-so-far
			(let* ([space (max 0 (- (or (syntax-column (car stxs)) 0) left))]
			       [p 
                                (let/ec k
                                  (code-htl-append
                                   (blank (hash-table-get col->width 
                                                          (+ space left)
                                                          (lambda ()
                                                            (k (pad-left space (car ps)))))
                                          0)
                                   (car ps)))])
			  (if (null? (cdr stxs))
			      p
			      (loop (cdr stxs)
				    (cdr ps)
				    p
				    (+ left space (get-span (car stxs)))
				    (or (syntax-line (car stxs)) (add1 line))
				    #t
                                    (make-hash-table 'equal)))))])))))]
	    [id
	     (identifier? stx)
	     (add-close (colorize-id (symbol->string (syntax-e stx)) mode) closes)]
	    [(a . b)
             ;; Build a list that makes the "." explicit.
             (let ([p (let loop ([a (syntax-e stx)])
                        (cond
                         [(pair? a) (cons (car a) (loop (cdr a)))]
                         [else (list (datum->syntax-object #f
                                                           (mode-colorize mode #f dot-p)
                                                           (list (syntax-source a)
                                                                 (syntax-line a)
                                                                 (- (syntax-column a) 2)
                                                                 (- (syntax-position a) 2)
                                                                 1))
                                     a)]))])
               (loop (datum->syntax-object stx
                                           p
                                           stx)
                     closes
                     mode))]
	    [else
	     (add-close (if (pict? (syntax-e stx))
			    (syntax-e stx)
			    (mode-colorize mode 'literal
					   (tt (format "~s" (syntax-e stx))) ))
			closes)])))
      
      )))
