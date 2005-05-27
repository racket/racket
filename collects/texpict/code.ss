(module code mzscheme
  (require "mrpict.ss"
	   (lib "class.ss")
           (lib "mred.ss" "mred")
	   (lib "unitsig.ss"))

  (provide define-code code^ code-params^ code@)

  (define-syntax (define-code stx)
    (syntax-case stx ()
      [(_ code typeset-code)
       (syntax/loc stx
	 (define-syntax (code stx)
	   (define (stx->loc-s-expr v)
	     (cond
	      [(syntax? v)
	       `(datum->syntax-object
		 #f
		 ,(syntax-case v (unsyntax)
		    [(unsyntax e) #'e]
		    [else (stx->loc-s-expr (syntax-e v))])
		 (list 'code
		       ,(syntax-line v)
		       ,(syntax-column v)
		       ,(syntax-position v)
		       ,(syntax-span v)))]
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
	      #`(typeset-code #,(cvt #'(code:line expr (... ...))))])))]))
  
  (define-signature code^
    (typeset-code 
     comment-color keyword-color id-color literal-color
     code-align current-code-tt
     current-keyword-list current-const-list code-colorize-enabled
     current-comment-color current-keyword-color 
     current-base-color current-id-color current-literal-color))

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
      
      (define current-keyword-list 
	(make-parameter '("define" "cond" "define-struct" "and" "or" "else"
			  "lambda" "require" "provide"  "require-for-syntax"
			  "define-syntax" "let" "letrec" "let*" "syntax-rules"
			  "syntax-case" "set!" "begin" "quote-syntax" "module")))
      (define current-const-list 
	(make-parameter '("null")))

      (define code-colorize-enabled
	(make-parameter #t))

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
      (define comment-color (current-base-color))
      (define current-comment-color (make-parameter comment-color))
      
      (define-computed open-paren-p (colorize (tt "(") (current-base-color)))
      (define-computed close-paren-p (colorize (tt ")") (current-base-color)))
      (define-computed open-sq-p (colorize (tt "[") (current-base-color)))
      (define-computed close-sq-p (colorize (tt "]") (current-base-color)))
      (define-computed quote-p (colorize (tt "'") (current-literal-color)))
      (define-computed syntax-p (colorize (tt "#'") (current-keyword-color)))
      (define-computed semi-p (colorize (tt "; ") (current-comment-color)))
      (define-computed open-paren/lit-p (colorize (tt "(") (current-literal-color)))
      (define-computed close-paren/lit-p (colorize (tt ")") (current-literal-color)))
      (define-computed open-paren/tmpl-p (colorize (tt "(") (current-comment-color)))
      (define-computed close-paren/tmpl-p (colorize (tt ")") (current-comment-color)))

      (define-computed dot-p (colorize (tt " . ") (current-base-color)))

      (define (get-close mode)
	(case mode
	  [(literal) close-paren/lit-p]
	  [(template comment) close-paren/tmpl-p]
	  [(cond template-cond local) close-sq-p]
	  [else close-paren-p]))

      (define (get-open mode)
	(case mode
	  [(literal) open-paren/lit-p]
	  [(template comment) open-paren/tmpl-p]
	  [(contract line) (blank)]
	  [(cond template-cond local) open-sq-p]
	  [else open-paren-p]))
      
      (define (add-close p closes)
	(cond
	 [(null? closes) p]
	 [(memq (car closes) '(contract line))
	  (add-close p (cdr closes))]
	 [else
	  (add-close (hbl-append p (get-close (car closes)))
		     (cdr closes))]))

      (define (pad-left space p)
	(if (= 0 space)
	    p
	    (htl-append (tt (make-string space #\space)) p)))

      (define (pad-bottom space p)
	(if (= 0 space)
	    p
	    (vl-append line-sep (tt " ") (pad-bottom (sub1 space) p))))

      (define (colorize-id str mode)
	(cond
	 [(and ((string-length str) . > . 1)
	       (char=? #\_ (string-ref str 0))
	       (not (char=? #\_ (string-ref str 1))))
	  (maybe-colorize (text (substring str 1) `(bold italic . modern) (current-font-size))
			  (current-id-color))]
	 [(regexp-match #rx"^(.+)_([0-9a-z]+)\\^([0-9a-z]+)$" str)
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (cc-superimpose
			    (text (caddr m) `(subscript bold . modern) (current-font-size))
			    (text (cadddr m) `(superscript bold . modern) (current-font-size)))))]
	 [(regexp-match #rx"^(.+)\\^([0-9a-z]+)_([0-9a-z]+)$" str)
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (cc-superimpose
			    (text (cadddr m) `(subscript bold . modern) (current-font-size))
			    (text (caddr m) `(superscript bold . modern) (current-font-size)))))]
	 [(regexp-match #rx"^(.+)\\^([0-9a-z]+)$" str)
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (text (caddr m) `(superscript bold . modern) (current-font-size))))]
	 [(regexp-match #rx"^(.+)_([0-9a-z]+)$" str)
	  => (lambda (m)
	       (hbl-append (colorize-id (cadr m) mode)
			   (text (caddr m) `(subscript bold . modern) (current-font-size))))]
	 [else
	  (maybe-colorize
	   (tt str)
	   (cond
	    [(eq? mode 'literal) (current-literal-color)]
	    [(memq mode '(comment template)) (current-comment-color)]
	    [(member str (current-keyword-list)) (current-keyword-color)]
	    [(member str (current-const-list)) (current-literal-color)]
	    [else (current-id-color)]))]))

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
      
      (define (add-semis p)
	(let loop ([p p] [semis semi-p])
	  (if ((pict-height p) . > . (+ (pict-height semis) 1))
	      (loop p (vl-append line-sep semi-p semis))
	      (htl-append semis p))))

      (define (typeset-code stx)
	(let loop ([stx stx][closes null][mode #f])
	  (syntax-case* stx (quote syntax-unquote syntax
				   code:contract code:comment code:line
				   code:template code:blank $)
			(lambda (a b) (eq? (syntax-e a) (syntax-e b)))
	    [() (add-close (htl-append (get-open mode) (get-close mode))
			   closes)]
	    [code:blank (tt " ")]
	    [$ (colorize-id "|" closes)]
	    [(quote x)
	     (htl-append quote-p (loop #'x closes 'literal))]
	    [(syntax x)
	     (htl-append syntax-p (loop #'x closes mode))]
	    [(code:contract i ...)
	     (add-semis (loop (datum->syntax-object #f (syntax->list #'(i ...)))
			      closes 'contract))]
	    [(code:line i ...)
	     (loop (datum->syntax-object #f (syntax->list #'(i ...)))
		   closes 'line)]
	    [(code:comment s ...)
	     (apply htl-append 
		    semi-p 
		    (map (lambda (s)
			   (if (pict? (syntax-e s))
			       (syntax-e s)
			       (maybe-colorize (tt (syntax-e s)) (current-comment-color))))
			 (syntax->list #'(s ...))))]
	    [(code:template i)
	     (add-semis (loop #'i closes 'template))]
	    [(i ...)
	     (let ([is (syntax->list #'(i ...))])
	       ;; Convert each i to a picture, include close paren in last item:
	       (let ([ips (let iloop ([is is][sub-mode (sub-mode mode)])
			    (cond
			     [(null? (cdr is)) (list (loop (car is) (cons mode closes) sub-mode))]
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
			      [line-so-far (get-open mode)]
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
			      [p (htl-append
				  line-so-far
				  (pad-left space (car ps)))])
			 (unless (equal? +inf.0 (+ space col))
			   (hash-table-put! col->width 
					    (+ space col) 
					    (pict-width (htl-append line-so-far (pad-left space (blank))))))
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
		       (vl-append
			line-sep
			line-so-far
			(let* ([space (max 0 (- (or (syntax-column (car stxs)) 0) left))]
			       [p 
                                (let/ec k
                                  (htl-append
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
	     (hbl-append open-paren-p (loop #'a null mode)
			 dot-p (loop #'b (cons mode closes) mode))]
	    [else
	     (add-close (if (pict? (syntax-e stx))
			    (syntax-e stx)
			    (maybe-colorize (tt (format "~s" (syntax-e stx))) (current-literal-color)))
			closes)])))
      
      )))
