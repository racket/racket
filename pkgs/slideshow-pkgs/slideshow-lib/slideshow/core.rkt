
(module core scheme/base
  (require scheme/class
           scheme/unit
           scheme/file
           racket/draw
           texpict/mrpict
           texpict/utils
           scheme/math
	   "sig.ss"
	   "private/utils.ss")

  (provide core@ 
	   zero-inset)

  ;; We create structs just once, so that all instances of the 
  ;; core share the types.
  (define/provide-struct sliderec (drawer        ; dc<%> x y -> void
				   title         ; string
				   comment       ; #f or just-a-comment
				   page          ; int
				   page-count    ; int
				   inset         ; sinset
				   [transitions  ; canvas% bitmap% -> 'went or delay-msecs
                                    #:mutable]
                                   timeout))     ; msecs
  (define/provide-struct just-a-comment (content)) ; content is list of strings and picts
  (define/provide-struct sinset (l t r b))
  (define/provide-struct click-region (left top right bottom thunk show-click?))
  (define/provide-struct interact (left top right bottom proc)
    ;; transparent for hashing:
    #:transparent)

  (define zero-inset (make-sinset 0 0 0 0))
      
  (define-unit core@
      (import config^ (prefix viewer: viewer^))
      (export (rename core^ 
                      (local:condense? condense?)
                      (local:printing? printing?)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                    Setup                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define local:condense? condense?)
      (define local:printing? printing?)

      (define font-size base-font-size)
      (define gap-size (* 3/4 font-size))
      (define current-gap-size (make-parameter gap-size))
      (define line-sep 2)
      (define title-size (+ font-size 4))
      (define main-font (if (and (not printing?)
				 (string=? (get-family-builtin-face 'default) " Sans"))
			    'default
			    'swiss))
      (define current-main-font (make-parameter main-font))

      (define current-line-sep (make-parameter line-sep))

      (define commentary-content-scale 0.8)

      (when (not (and (= use-screen-w screen-w)
		      (= use-screen-h screen-h)
		      (= pixel-scale 1)
		      (not commentary-on-slide?)))
	(let ([c-scale (if commentary-on-slide?
			   commentary-content-scale
			   1)])
	  (current-expected-text-scale (list (* (/ use-screen-w screen-w) pixel-scale c-scale)
					     (* (/ use-screen-h screen-h) pixel-scale c-scale)))))

      (define red "red")
      (define green "forest green")
      (define blue "blue")
      (define purple "purple")
      (define orange "orange")

      (define current-font-size (make-parameter 
				 font-size
				 (lambda (x)
				   (unless (and (number? x)
						(integer? x)
						(exact? x)
						(positive? x))
				     (raise-argument-error 'current-font-size "nonnegative-exact-integer?" x))
				   x)))

      (define current-title-color (make-parameter
				   green
				   (lambda (x)
				     (unless (or (string? x)
						 (x . is-a? . color%))
				       (raise-argument-error 'current-title-color
                                                             "(or/c string? (is-a?/c color%))"
                                                             x))
				     x)))

      (define (t s) (text s (current-main-font) (current-font-size)))
      (define (it s) (text s `(italic . ,(current-main-font)) (current-font-size)))
      (define (bt s) (text s `(bold . ,(current-main-font)) (current-font-size)))
      (define (bit s) (text s `(bold italic . ,(current-main-font)) (current-font-size)))
      (define (tt s) (text s '(bold . modern) (current-font-size)))
      (define (rt s) (text s 'roman (current-font-size)))

      (define current-titlet
        (make-parameter
         (lambda (s)
           (colorize (text s 
                           `(bold . ,(current-main-font)) 
                           title-size)
                     (current-title-color)))))
          
      (define (titlet s) ((current-titlet) s))

      (define (with-font f k)
	(parameterize ([current-main-font f])
	  (k)))

      (define (tt* . l) (apply vl-append (current-line-sep) (map tt l)))

      (define bullet (if (send (dc-for-text-size) glyph-exists? #\u2022)
			 (t "\u2022")
			 (baseless
			  (cc-superimpose (disk (/ gap-size 2)) 
					  (blank 0 gap-size)))))
      (define o-bullet (baseless
			(cc-superimpose (circle (/ gap-size 2)) 
					(blank 0 gap-size))))


      (define margin 20)
      (define-values (client-w client-h) (values (- screen-w (* margin 2))
						 (- screen-h (* margin 2))))
      (define full-page (blank client-w client-h))
      (define title-h (pict-height (titlet "Hi")))
      (define (mk-titleless-page)
	(inset full-page 0 (- 0 title-h (* 2 gap-size)) 0 0))
      (define titleless-page (mk-titleless-page))

      (define (set-margin! m)
	(set! margin m)
	(set! client-w (- screen-w (* 2 margin)))
	(set! client-h (- screen-h (* 2 margin)))
	(set! full-page (blank client-w client-h))
	(set! titleless-page (mk-titleless-page)))
      (define (get-margin) margin)
      (define (get-client-w) client-w)
      (define (get-client-h) client-h)
      (define (get-full-page) full-page)
      (define (get-titleless-page) titleless-page)

      (define (set-title-h! h)
	(set! title-h h)
	(set! titleless-page (mk-titleless-page)))
      (define (get-title-h) title-h)

      (define (set-use-background-frame! on?)
	(viewer:set-use-background-frame! (and on? #t)))

      (define (enable-click-advance! on?)
	(viewer:enable-click-advance! (and on? #t)))

      (define (set-page-numbers-visible! on?)
	(viewer:set-page-numbers-visible! (and on? #t)))

      (define (set-spotlight-style! #:size [size #f] #:color [color #f])
        (viewer:set-spotlight-style! #:size size #:color color))

      (define current-page-number-font 
	(make-parameter
	 (make-object font% 10 'default 'normal 'normal)
	 (lambda (f)
	   (unless (f . is-a? . font%)
	     (raise-argument-error 'current-page-number-font "(is-a?/c font%)" f))
	   f)))
      (define current-page-number-color 
	(make-parameter (make-object color% "black")
			(lambda (s)
			  (unless (s . is-a? . color%)
			    (raise-argument-error 'current-page-number-color "(is-a?/c color%)" s))
			  s)))
      (define current-page-number-adjust (make-parameter 
                                          (λ (n s) s)
                                          (lambda (f)
                                            (unless (procedure-arity-includes? f 2)
                                              (raise-argument-error 'current-page-number-adjust "(procedure-arity-includes/c 2)" f))
                                            f)))
      
      (define page-number 1)

      (define (add-commentary p comment)
	(if commentary-on-slide?
	    (let ([p (scale (frame
			     (inset (let ([tp (launder full-page)])
				      (refocus (lt-superimpose p tp) tp))
				    margin))
			    commentary-content-scale)]
		  [t (if comment
			 (let ([comments (let loop ([l (just-a-comment-content comment)]
						    [current-line null])
					   (cond
					    [(null? l) (list (reverse current-line))]
					    [(pict? (car l))
					     (loop (cdr l) (cons (car l) current-line))]
					    [else (let ([m (regexp-match #rx"^(.*?)(?:\n|\r\n|\r)[ \t]*(.*)$" (car l))])
						    (if m
							(cons
							 (reverse (cons (cadr m) current-line))
							 (loop (cons (caddr m) (cdr l))
							       null))
							(loop (cdr l) (cons (car l) current-line))))]))])
			   (parameterize ([current-font-size 9])
			     (apply vl-append
				    1
				    (map (lambda (l) 
					   (apply para (- (* screen-w (- 1 commentary-content-scale))
							  margin margin 2)
						  l))
					 comments))))
			 (blank))])
	      (ht-append 2 p t))
	    p))

      (define (add-slide! pict title comment page-count inset timeout)
	(viewer:add-talk-slide! 
	 (make-sliderec (make-pict-drawer (add-commentary pict
							  comment))
			title 
			comment
			page-number
			page-count
			inset
			null
                        timeout))
	(set! page-number (+ page-number page-count)))

      (define (skip-slides n)
	(set! page-number (+ page-number n)))

      (define (evenize-width p)
	(let ([w (pict-width p)])
	  ;; Force even size:
	  (inset p 0 0 (+ (- (ceiling w) w)
			  (modulo (ceiling w) 2)) 0)))

      (define (apply-slide-inset sinset pict)
	(inset pict 
	       (- (sinset-l sinset))
	       (- (sinset-t sinset))
	       (- (sinset-r sinset))
	       (- (sinset-b sinset))))

      (define (do-add-slide! content title comment page-count inset timeout)
	(add-slide!
	 (ct-superimpose
	  (apply-slide-inset inset full-page)
	  content)
	 title
	 comment
	 page-count
	 inset
         timeout))

      (define default-slide-assembler
	(lambda (s v-sep p)
	  (apply vc-append v-sep
		 (if s
		     (list (evenize-width (if (pict? s) s (titlet s))) p)
		     (list p)))))

      (define current-slide-assembler
	(make-parameter default-slide-assembler))

      (define-struct name-only (title))
      (define-struct name+title (name title))

      (define (one-slide/title/inset do-add-slide! use-assem? process v-sep skipped-pages s inset timeout a-gap-size . x) 
	(let-values ([(x c)
		      (let loop ([x x][c #f][r null])
			(cond
			 [(null? x) (values (reverse r) c)]
			 [(just-a-comment? (car x))
			  (loop (cdr x) (car x) r)]
			 [else
			  (loop (cdr x) c (cons (car x) r))]))])
	  (let ([content ((if use-assem?
			      (current-slide-assembler)
			      default-slide-assembler)
                          (if (name+title? s)
                              (name+title-title s)
                              (and (not (name-only? s)) 
                                   s))
			  v-sep
			  (apply vc-append 
				 a-gap-size
				 (map evenize-width (process x))))])
	    (do-add-slide!
	     content
	     (cond
              [(name-only? s) (name-only-title s)]
              [(name+title? s) (name+title-name s)]
              [else s])
	     c
	     (+ 1 skipped-pages)
	     inset
             timeout))))

      (define (slide-error nested string . args)
	(apply error
	       (let loop ([nested nested])
		 (if (null? nested)
		     'slide*
		     (string->symbol (format "~a of ~a" (car nested) (loop (cdr nested))))))
	       string
	       args))

      (define (do-slide/title/tall/inset do-add-slide! use-assem? skip-ok? skip-all? process v-sep s 
                                         inset timeout a-gap-size . x)
	;; Check slides:
	(let loop ([l x][nested null])
	  (or (null? l)
	      (cond
	       [(pict? (car l)) (loop (cdr l) nested)]
	       [(just-a-comment? (car l)) (loop (cdr l) nested)]
	       [(memq (car l) '(next next!)) (and (or (pair? l) 
						      (slide-error nested "argument sequence contains 'next at end"))
						  (loop (cdr l) nested))]
	       [(memq (car l) '(alts alts~)) (and (or (pair? (cdr l))
						      (slide-error nested "argument sequence contains '~a at end" (car l)))
						  (let ([a (cadr l)])
						    (and (or (list? a)
							     (slide-error nested "non-list after '~a: ~e" (car l) a))
							 (andmap (lambda (sl)
								   (unless (list? sl)
								     (slide-error nested "non-list in list after '~a: ~e" 
										  (car l) sl))
								   (loop sl (cons (car l) nested)))
								 a)))
						  (loop (cddr l) nested))]
	       [(eq? (car l) 'nothing) (loop (cdr l) nested)]
	       [else #f])
	      (slide-error nested "argument sequence contains a bad element: ~e" (car l))))

        (skip-slides
         (let loop ([l x][r null][comment #f][skip-all? skip-all?][skipped 0])
           (cond
            [(null? l) 
             (if skip-all?
                 (add1 skipped)
                 (begin
                   (apply one-slide/title/inset do-add-slide! use-assem? process v-sep skipped s 
                          inset timeout a-gap-size (reverse r))
                   0))]
            [(memq (car l) '(nothing))
             (loop (cdr l) r comment skip-all? skipped)]
            [(memq (car l) '(next next!))
             (let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'next)))])
               (let ([skipped (if skip?
                                  (add1 skipped)
                                  (begin
                                    (apply one-slide/title/inset do-add-slide! use-assem? process v-sep skipped s 
                                           inset timeout a-gap-size (reverse r))
                                    0))])
                 (loop (cdr l) r comment skip-all? skipped)))]
            [(memq (car l) '(alts alts~)) 
             (let ([rest (cddr l)])
               (let aloop ([al (cadr l)][skipped skipped])
                 (cond
                  [(null? al) ;; only happens when al starts out null
                   (loop rest r comment skip-all? skipped)]
                  [(null? (cdr al))
                   (loop (append (car al) rest) r comment skip-all? skipped)]
                  [else
                   (let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'alts~)))])
                     (let ([skipped (loop (car al) r comment skip? skipped)])
                       (aloop (cdr al) skipped)))])))]
            [else (loop (cdr l) (cons (car l) r) comment skip-all? skipped)]))))

      (define slide/kw 
        (let ([slide (lambda (#:title [s #f]
                                      #:name [name s]
                                      #:inset [inset zero-inset]
                                      #:timeout [timeout #f]
                                      #:layout [layout 'auto]
                                      #:condense? [condense-this? timeout]
                                      #:gap-size [a-gap-size (current-gap-size)]
                                      . body)
                       (let ([t (if s
                                    (if (equal? name s)
                                        (if (string? s)
                                            (decode s)
                                            s)
                                        (make-name+title name 
                                                         (if (string? s)
                                                             (decode s)
                                                             s)))
                                    (if name
                                        (make-name-only name)
                                        #f))])
                         (case layout
                           [(tall top)
                            (apply do-slide/title/tall/inset 
                                   do-add-slide! #t #t (and condense? condense-this?) values
                                   (if (eq? layout 'tall)
                                       a-gap-size 
                                       (* 2 a-gap-size))
                                   t
                                   inset
                                   timeout
                                   a-gap-size
                                   body)]
                           [else ; center, auto
                            (apply slide/title/center/inset/timeout/gap 
                                   (or (not s) (eq? layout 'center))
                                   (and condense? condense-this?)
                                   t
                                   inset 
                                   timeout 
                                   a-gap-size
                                   body)]))
                       (void))])
          slide))

      (define (make-slide-inset l t r b)
	(make-sinset l t r b))

      (define (slide/title/tall/inset/gap v-sep s inset . x)
	(apply do-slide/title/tall/inset do-add-slide! #t #t #f values v-sep s inset #f gap-size x))

      (define (slide/title/tall/inset s inset . x)
	(apply slide/title/tall/inset/gap gap-size s inset x))

      (define (slide/name/tall/inset s inset . x)
	(apply slide/title/tall/inset (make-name-only s) inset x))

      (define (slide/title/tall/gap v-sep s timeout . x)
	(apply do-slide/title/tall/inset do-add-slide! #t #t #f values v-sep s zero-inset timeout gap-size x))

      (define (slide/title/tall s . x)
	(apply slide/title/tall/gap gap-size s #f x))

      (define (slide/name/tall s . x)
	(apply slide/title/tall (make-name-only s) x))

      (define (slide/title s . x)
	(apply slide/title/tall/gap (* 2 gap-size) s #f x))

      (define (slide/title/timeout s timeout . x)
	(apply slide/title/tall/gap (* 2 gap-size) s timeout x))

      (define (slide/name s . x)
	(apply slide/title (make-name-only s) x))

      (define (slide . x)
	(apply slide/title #f x))

      (define (slide/title/inset s inset . x)
	(apply slide/title/tall/inset/gap (* 2 gap-size) s inset x))

      (define (slide/name/inset s inset . x)
	(apply slide/title/inset (make-name-only s) inset x))

      (define (slide/title/center/inset s inset . x)
        (apply slide/title/center/inset/timeout #t #f s inset #f x))

      (define (slide/title/center/inset/timeout always-center? skip-all? s inset timeout . x)
        (apply slide/title/center/inset/timeout/gap always-center? skip-all? s inset timeout gap-size x))

      (define (slide/title/center/inset/timeout/gap always-center? skip-all? s inset timeout a-gap-size . x)
	(let ([max-width 0]
	      [max-height 0]
	      [combine (lambda (x)
			 (apply vc-append a-gap-size
				(map
				 evenize-width
				 x)))])
	  ;; Run through all the slides once to measure (don't actually create slides):
	  (apply do-slide/title/tall/inset
		 (lambda (content title comment page-count inset timeout)
		   (set! max-width (max max-width (pict-width content)))
		   (set! max-height (max max-height (pict-height content))))
		 #f
		 #f
                 #f
		 (lambda (x) (list (combine x)))
		 0 #f inset timeout a-gap-size x)
          (let ([center? (or always-center?
                             (max-height . < . (- client-h 
                                                  (* 2
                                                     (+ (* 2 a-gap-size)
                                                        title-h)))))])
            (apply do-slide/title/tall/inset
                   do-add-slide!
                   #t
                   #t
                   skip-all?
                   (if center?
                       (lambda (x)
                         (list
                          (cc-superimpose
                           (apply-slide-inset inset (if (and s (not (name-only? s)))
                                                        titleless-page 
                                                        full-page))
                           (ct-superimpose
                            (blank max-width max-height)
                            (combine x)))))
                       values)
                   (if center? 0 (* 2 a-gap-size))
                   s inset timeout a-gap-size x))))

      (define (slide/name/center/inset s inset . x)
	(apply slide/title/center/inset (make-name-only s) inset x))

      (define (slide/title/center s . x)
	(apply slide/title/center/inset s zero-inset x))

      (define (slide/name/center s . x)
	(apply slide/title/center (make-name-only s) x))

      (define (slide/timeout timeout . x) (apply slide/title/timeout #f timeout x))
      (define (slide/inset inset . x) (apply slide/title/inset #f inset x))

      (define (slide/center . x) (apply slide/title/center #f x))
      (define (slide/center/inset inset . x) (apply slide/title/center/inset #f inset x))

      (define (slide/center/timeout t . x)
        (apply slide/title/center/inset/timeout #t #f #f zero-inset t x))

      (define (slide/title/center/timeout s t . x)
        (apply slide/title/center/inset/timeout #t #f s zero-inset t x))

      (define most-recent-slide
	(case-lambda
	 [() (most-recent-slide 0)]
	 [(n) (viewer:most-recent-talk-slide)]))

      (define retract-most-recent-slide
	(lambda ()
	  (let ([slide (viewer:most-recent-talk-slide)])
	    (when slide
	      (set! page-number (sliderec-page slide))
	      (viewer:retract-talk-slide!)
	      slide))))
      
      (define re-slide
	(lambda (s [addition #f])
	  (unless (sliderec? s)
	    (raise-argument-error 're-slide "slide?" s))
	  (viewer:add-talk-slide!
	   (make-sliderec
	    (let ([orig (sliderec-drawer s)]
		  [extra (if addition
			     (make-pict-drawer 
			      (add-commentary addition
					      #f))
			     void)])
	      (lambda (dc x y)
		(orig dc x y)
		(extra dc x y)))
	    (sliderec-title s)
	    (sliderec-comment s)
	    page-number
	    1
	    (sliderec-inset s)
	    null
            (sliderec-timeout s)))
	  (set! page-number (+ page-number 1))))
      
      (define (slide->pict s)
        (unless (sliderec? s)
          (raise-argument-error 'slide->pict "slide?" s))
        (let ([orig (sliderec-drawer s)])
          (dc orig client-w client-h)))

      (define (start-at-recent-slide)
	(viewer:set-init-page! (max 0 (- page-number 2))))

      (define (done-making-slides)
	(viewer:done-making-slides))

      (define (make-outline . l)
	(define ah (arrowhead gap-size 0))
	(define current-item (colorize (hc-append (- (/ gap-size 2)) ah ah) blue))
	(define other-item (rc-superimpose (ghost current-item) (colorize ah "light gray")))
        (define (to-next l)
          (let ([l (cdddr l)])
            (if (and (pair? l) (number? (car l)))
                (cdr l)
                l)))
	(lambda (which)
	  (slide/name
	   (format "--~a--"
		   (let loop ([l l])
		     (cond
		      [(null? l) "<unknown>"]
		      [(eq? (car l) which)
		       (cadr l)]
		      [else (loop (to-next l))])))
	   (blank (+ title-h gap-size))
	   (lc-superimpose
	    (blank (current-para-width) 0)
	    (let loop ([l l])
	      (cond
	       [(null? l) (blank)]
	       [else
		(let ([current? (or (eq? which (car l)) 
				    (and (list? (car l))
					 (memq which (car l))))])
		  (vc-append
                   gap-size
		   (page-para
		    (hbl-append
		     (quotient gap-size 2)
		     (if current?
			 current-item
			 other-item)
		     (let ([p (cadr l)])
		       (if (pict? p)
			   p
			   (bt p)))))
		   (let* ([rest (let ([p (loop (to-next l))]
                                      [l (cdddr l)])
                                  (if (and (pair? l) (number? (car l)))
                                      (inset p 0 (car l) 0 0)
                                      p))]
                          [sub-items (caddr l)])
		     (if (and current?
			      sub-items
			      (not (null? sub-items)))
			 (vc-append
			  gap-size
			  (sub-items which)
			  rest)
			 rest))))]))))
          (void)))

      (define (comment . s) 
	(make-just-a-comment s))

      ;; ----------------------------------------

      ;; Move separators (that shouldn't be preceded by extra space)
      ;; at the front of a string to the end of the previous item
      (define (shift-no-sep l)
	(let loop ([l 
		    ;; Flatten l, first:
		    (let loop ([l l])
		      (cond
		       [(null? l) null]
		       [(pair? (car l)) (append (loop (car l)) (loop (cdr l)))]
		       [else (cons (car l) (loop (cdr l)))]))]
		   [a null])
	  ;; Combine strings:
	  (cond
	   [(null? l) (reverse a)]
	   [(null? a) (loop (cdr l) (list (car l)))]
	   [(and (string? (car l)) 
		 (regexp-match #rx"^[-',. :;?!)\U201D\U2019]" (car l)))
	    (let ([m (regexp-match #rx"^([^ ]*) (.*)$" (car l))])
	      (if m
                  (if (string? (car a))
		      (loop (cdr l)
			    (list* (caddr m)
				   (string-append (car a) (cadr m))
				   (cdr a)))
		      (loop (cdr l)
			    (list* (caddr m)
				   (hbl-append (car a) (t (cadr m)))
				   (cdr a))))
		  (if (string? (car a))
		      (loop (cdr l)
			    (cons (string-append (car a) (car l))
				  (cdr a)))
		      (loop (cdr l)
			    (cons (hbl-append (car a) (t (car l)))
				  (cdr a))))))]
	   [else (loop (cdr l) (cons (car l) a))])))

      (define current-para-width (make-parameter client-w))

      (define para/kw
        (let ([para (lambda (#:width [width (current-para-width)]
                                     #:align [align 'left]
                                     #:fill? [fill? #t]
                                     #:decode? [decode? #t]
                                     . s)
                      (let ([p (para*/align (case align
                                              [(right) vr-append]
                                              [(center) vc-append]
                                              [else vl-append])
                                            width
                                            (if decode?
                                                (decode s)
                                                s))])
                        (if fill?
                            ((case align
                               [(right) rtl-superimpose]
                               [(center) ctl-superimpose]
                               [else ltl-superimpose])
                             (blank width 0)
                             p)
                            p)))])
          para))

      (define (decode s)
        (let loop ([s s])
          (cond
           [(list? s) 
            (map 
             decode
             ;; Remove "\n", and also cancel extra spaces after "\n":
             (let loop ([s s])
               (cond
                [(null? s) null]
                [(equal? (car s) "\n")
                 (let nloop ([s (cdr s)])
                   (if (and (pair? s)
                            (string? (car s)))
                       (let ([a (regexp-replace #rx"^ +" (car s) "")])
                         (if (string=? a "")
                             (nloop (cdr s))
                             (loop (cons a (cdr s)))))
                       (loop s)))]
                [else (cons (car s) (loop (cdr s)))])))]
           [(not (string? s)) s]
           [(regexp-match-positions #rx"---" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2014"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #rx"--" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2013"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #px"``" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u201C"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #px"''" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u201D"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #rx"'" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2019"
                                (loop (substring s (cdar m)))))]           
           [else s])))

      (define (para*/align v-append w . s)
	(define space (t " "))
	(let loop ([pre #f][s (shift-no-sep s)][rest null])
	  (cond
	   [(null? s)
	    (if (null? rest)
		(or pre (blank))
		(loop pre (car rest) (cdr rest)))]
	   [(list? s) (loop pre (car s) (append (cdr s) rest))]
	   [else
	    (let* ([p (if (string? s) (t s) s)])
	      (cond
	       [(< (+ (if pre (pict-width pre) 0)
		      (if pre (pict-width space) 0)
		      (pict-width p)) 
		   w)
		;; small enough
		(loop (if pre 
			  (hbl-append pre space p) 
			  p)
		      rest null)]
	       [(and (string? s) (regexp-match "(.*) (.*)" s))
		;; can break on string
		=> (lambda (m)
		     (loop pre
			   (cadr m) 
			   (cons
			    (caddr m)
			    rest)))]
	       [(not pre)
		(if (null? rest)
		    p
		    (v-append
		     (current-line-sep)
		     p
		     (loop #f rest null)))]
	       [else
		(v-append
		 (current-line-sep)
		 pre
		 (loop p rest null))]))])))

      (define (para* w . s)
	(para*/align vl-append w s))

      (define (para*/r w . s)
	(para*/align vr-append w s))

      (define (para*/c w . s)
	(para*/align vc-append w s))


      (define (para/align superimpose v-append w . s)
	(superimpose (para*/align v-append w s)
		     (blank w 0)))

      (define (para w . s)
	(para/align lbl-superimpose vl-append w s))

      (define (para/r w . s)
	(para/align rbl-superimpose vr-append w s))

      (define (para/c w . s)
	(para/align cbl-superimpose vc-append w s))


      (define (page-para*/align v-append . s)
	(para*/align v-append (current-para-width) s))

      (define (page-para* . s)
	(page-para*/align vl-append s))

      (define (page-para*/r . s)
	(page-para*/align vr-append s))

      (define (page-para*/c . s)
	(page-para*/align vc-append s))


      (define (page-para/align superimpose v-append . s)
	(para/align superimpose v-append (current-para-width) s))

      (define (page-para . s)
	(page-para/align lbl-superimpose vl-append s))

      (define (page-para/r . s)
	(page-para/align rbl-superimpose vr-append s))

      (define (page-para/c . s)
	(page-para/align cbl-superimpose vc-append s))

      ;; ----------------------------------------

      (define (l-combiner para w l)
	(apply
	 vl-append
	 gap-size
	 (map (lambda (x) (para w x)) l)))

      ;; ----------------------------------------

      (define item/kw
        (let ([item (lambda (#:gap-size [a-gap-size (current-gap-size)]
                                        #:bullet [bullet (if (eq? a-gap-size gap-size)
                                                             bullet
                                                             (scale bullet (/ a-gap-size gap-size)))]
                                        #:width [width (current-para-width)]
                                        #:align [align 'left]
                                        #:fill? [fill? #t]
                                        #:decode? [decode? #t]
                                      . s)
                      (htl-append (/ a-gap-size 2)
                                  bullet 
                                  (para/kw #:width (- width
                                                      (pict-width bullet) 
                                                      (/ a-gap-size 2)) 
                                           #:align align
                                           #:fill? fill?
                                           #:decode? decode?
                                           s)))])
          item))

      (define (item*/bullet bullet w . s)
	(htl-append (/ gap-size 2)
		    bullet 
		    (para* (- w
			      (pict-width bullet) 
			      (/ gap-size 2)) 
			   s)))

      (define (item* w . s)
	(apply item*/bullet bullet w s))

      (define (item w . s)
	(lbl-superimpose (item* w s)
			 (blank w 0)))

      (define (item/bullet b w . s)
	(lbl-superimpose (item*/bullet b w s)
			 (blank w 0)))

      (define (page-item* . s)
	(item* (current-para-width) s))

      (define (page-item . s)
	(item (current-para-width) s))

      (define (page-item*/bullet b . s)
	(item*/bullet b (current-para-width) s))

      (define (page-item/bullet b . s)
	(item/bullet b (current-para-width) s))

      ;; ----------------------------------------

      (define subitem/kw
        (let ([subitem (lambda (#:gap-size [a-gap-size (current-gap-size)]
                                           #:bullet [bullet (if (eq? gap-size a-gap-size)
                                                                o-bullet
                                                                (scale o-bullet (/ a-gap-size gap-size)))]
                                           #:width [width (current-para-width)]
                                           #:align [align 'left]
                                           #:fill? [fill? #t]
                                           #:decode? [decode? #t]
                                           . s)
                         (inset (htl-append (/ a-gap-size 2)
                                            bullet 
                                            (para/kw #:width (- width
                                                                (* 2 a-gap-size)
                                                                (pict-width bullet) 
                                                                (/ a-gap-size 2)) 
                                                     #:align align
                                                     #:fill? fill?
                                                     #:decode? decode?
                                                     s))
                                (* 2 a-gap-size) 0 0 0))])
          subitem))

      (define (subitem* w . s)
	(inset (htl-append (/ gap-size 2)
			   o-bullet 
			   (para* (- w
				     (* 2 gap-size)
				     (pict-width bullet) 
				     (/ gap-size 2)) 
				  s))
	       (* 2 gap-size) 0 0 0))

      (define (subitem w . s)
	(lbl-superimpose (subitem* w s)
			 (blank w 0)))

      (define (page-subitem* . s)
	(subitem* (current-para-width) s))

      (define (page-subitem . s)
	(subitem (current-para-width) s))

      ;; ----------------------------------------

      (define (paras* w . l)
	(l-combiner para* w l))

      (define (paras w . l)
	(l-combiner para w l))

      (define (page-paras* . l)
	(l-combiner (lambda (x y) (page-para* y)) (current-para-width) l))

      (define (page-paras . l)
	(l-combiner (lambda (x y) (page-para y)) (current-para-width) l))

      ;; ----------------------------------------

      (define (itemize w . l)
	(l-combiner item w l))

      (define (itemize* w . l)
	(l-combiner item* w l))

      (define (page-itemize . l)
	(l-combiner (lambda (x y) (page-item y)) (current-para-width) l))

      (define (page-itemize* . l)
	(l-combiner (lambda (x y) (page-item* y)) (current-para-width) l))

      ;; ----------------------------------------

      (define (size-in-pixels p)
	(if (not (and (= use-screen-w screen-w)
		      (= use-screen-h screen-h)))
	    (scale p 
		   (/ screen-w use-screen-w)
		   (/ screen-h use-screen-h))
	    p))

      ;; ----------------------------------------

      (define clickback
	(lambda (pict thunk [show-click? #t])
	  (let ([w (pict-width pict)]
		[h (pict-height pict)])
	    (cons-picture*
	     pict
	     `((place 
		0 0
		,(dc (lambda (dc x y)
		       (let-values ([(sx sy) (send dc get-scale)]
				    [(dx dy) (send dc get-origin)])
			 (viewer:add-click-region!
			  (make-click-region (+ (* x sx) dx)
					     (+ (* y sy) dy)
					     (+ (* (+ x w) sx) dx)
					     (+ (* (+ y h) sy) dy)
					     thunk
					     show-click?))))
		     w h
		     (pict-ascent pict)
		     (pict-descent pict))))))))

      (define interactive
	(lambda (pict window-proc)
	  (let ([w (pict-width pict)]
		[h (pict-height pict)])
	    (cons-picture*
	     pict
	     `((place 
		0 0
		,(dc (lambda (dc x y)
		       (let-values ([(sx sy) (send dc get-scale)]
				    [(dx dy) (send dc get-origin)])
			 (viewer:add-interactive!
			  (make-interact (+ (* x sx) dx)
                                         (+ (* y sy) dy)
                                         (+ (* (+ x w) sx) dx)
                                         (+ (* (+ y h) sy) dy)
                                         window-proc))))
		     w h
		     (pict-ascent pict)
		     (pict-descent pict))))))))

      ;; ----------------------------------------

      (define (add-transition! who trans)
	(let ([slide (viewer:most-recent-talk-slide)])
	  (when slide
	    (set-sliderec-transitions! slide 
				       (append (sliderec-transitions slide)
						(list trans))))))
      
      (define scroll-bm #f)
      (define scroll-dc (make-object bitmap-dc%))

      (define scroll-transition
	(lambda (x y w h dx dy [duration 0.20] [steps 12])
	  (add-transition! 'scroll-transition
			   (lambda (offscreen-dc)
			     (let* ([steps-done 0]
				    [xs (/ use-screen-w screen-w)]
				    [ys (/ use-screen-h screen-h)]
				    [bcw (send (send offscreen-dc get-bitmap) get-width)]
				    [bch (send (send offscreen-dc get-bitmap) get-height)]
				    [mx (- margin (/ (- use-screen-w bcw) 2 xs))]
				    [my (- margin (/ (- use-screen-h bch) 2 ys))]
				    [x-space (ceiling (* xs (/ (abs dx) steps)))]
				    [y-space (ceiling (* ys (/ (abs dy) steps)))]
				    [x-in (if (positive? dx)
					      x-space
					      0)]
				    [y-in (if (positive? dy)
					      y-space
					      0)])
			       (unless (and scroll-bm
					    (>= (send scroll-bm get-width) 
						(+ x-space (* xs w)))
					    (>= (send scroll-bm get-height) 
						(+ y-space (* ys h))))
				 (set! scroll-bm (make-bitmap
						  (inexact->exact (ceiling (+ x-space (* xs w))))
						  (inexact->exact (ceiling (+ y-space (* ys h))))))
				 (if (send scroll-bm ok?)
				     (send scroll-dc set-bitmap scroll-bm)
				     (set! scroll-bm #f)))

			       (when scroll-bm
				 (send scroll-dc clear)
				 (send scroll-dc draw-bitmap-section (send offscreen-dc get-bitmap)
				       x-in y-in
				       (* (+ x mx) xs) (* (+ y my) ys)
				       (* w xs) (* h ys)))
			       
			       (lambda (canvas offscreen-dc)
				 (if (or (not scroll-bm) (= steps-done steps))
				     'done
				     (let*-values ([(cw ch) (send canvas get-client-size)])
				       (let ([xm (- margin (/ (- use-screen-w bcw) 2 xs))]
					     [ym (- margin (/ (- use-screen-h bch) 2 ys))])
					 (set! steps-done (add1 steps-done))
					 (let ([draw
						(lambda (dc xm ym)
						  (send dc draw-bitmap-section
							scroll-bm
							(- (* (+ x xm (* dx (/ steps-done steps))) xs) x-in)
							(- (* (+ y ym (* dy (/ steps-done steps))) ys) y-in)
							0 0 
							(ceiling (* xs (+ w (/ (abs dx) steps))))
							(ceiling (* ys (+ h (/ (abs dy) steps))))))])
					   (draw (send canvas get-dc) xm ym)
					   (draw offscreen-dc mx my)))
				       (/ duration steps)))))))))

      (define pause-transition
	(lambda (time)
	  (add-transition! 'pause-transition
			   (lambda (offscreen-dc)
			     (let ([done? #f])
			       (lambda (canvas offscreen-dc)
				 (if done?
				     'done
				     (begin
				       (set! done? #t)
				       time))))))))))
