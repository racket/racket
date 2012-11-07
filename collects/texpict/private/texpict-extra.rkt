#lang scheme/unit

  (require mzlib/etc
           mzlib/list)

  (require "texpict-sig.rkt"
           "common-sig.rkt")

  (import texpict-common^
          texpict-internal^)
  (export texpict-extra^
          texpict-common-setup^)

      (define using-pict2e-package
	(make-parameter #f
			(lambda (x)
			  (and x #t))))

      (define use-old-connect
	(make-parameter #f
			(lambda (x)
			  (and x #t))))

      (define output-measure-commands
	(make-parameter #t
			(lambda (x)
			  (and x #t))))

      (define draw-bezier-lines
	(make-parameter #f
			(lambda (x)
			  (if (procedure? x)
			      (begin
				(unless (procedure-arity-includes? x 1)
				  (raise-type-error 'draw-bezier-lines
						    "boolean or procedure of one argument"
						    x))
				x)
			      (and x #t)))))

      (define serialize-tex-picts
	(make-parameter #f
			(lambda (x)
			  (and x #t))))

      (define tex-series-prefix
	(make-parameter #f
			(lambda (s)
			  (when s
			    (unless (string? s)
			      (raise-type-error 'tex-series-prefix "string or #f" s)))
			  s)))

      (define current-tex-sizer
	(make-parameter (lambda (t) #f)))

      (define label-sizes null)
      (define (extract-num s) ; strip off trainling `pt'
	(let ([str (symbol->string s)])
	  (inexact->exact
	   (ceiling
	    (string->number (substring str 0 (- (string-length str) 2)))))))

      (define (read-in-sizes file)
	(parameterize ([read-case-sensitive #t])
	  (when (file-exists? file)
	    (set! label-sizes
		  (append (with-input-from-file file
			    (lambda ()
			      (let loop ()
				(let ([e (read)])
				  (if (eof-object? e)
				      null
				      (let ([w (read)]
					    [h (read)]
					    [d (read)])
					(cons (list e
						    (extract-num w) 
						    (extract-num h)
						    (extract-num d))
					      (loop))))))))
			  label-sizes)))))

      ;; Marshall a tex string into a simple symbol
      (define digits (make-vector 64))
      (let loop ([i 0])
	(unless (= i 10)
	  (vector-set! digits i (integer->char (+ i (char->integer #\0))))
	  (loop (add1 i))))
      (let loop ([i 0])
	(unless (= i 26)
	  (vector-set! digits (+ i 10) (integer->char (+ i (char->integer #\a))))
	  (vector-set! digits (+ i 36) (integer->char (+ i (char->integer #\A))))
	  (loop (add1 i))))
      (vector-set! digits 62 #\-)
      (vector-set! digits 63 #\+)
      (define (number->base-64-string prefix n)
	(let loop ([n n][s null])
	  (if (zero? n)
	      (list->string (cons prefix s))
	      (loop (arithmetic-shift n -6)
		    (cons (vector-ref digits (bitwise-and 63 n)) s)))))
      (define serial-number 0)
      (define (serialize s)
	(cond
	 [(serialize-tex-picts)
	  (set! serial-number (add1 serial-number))
	  (format "~a.~a" serial-number s)]
	 [(tex-series-prefix)
	  (format "~a.~a" (tex-series-prefix) s)]
	 [else s]))
      (define (make-label s)
	(string->symbol
	 (serialize
	  (number->base-64-string
	   #\T
	   (let loop ([l (string->list s)][n 0])
	     (if (null? l)
		 n
		 (loop (cdr l) (+ (arithmetic-shift n 7) (char->integer (car l))))))))))

      (define tex
	(case-lambda
	 [(t) (tex t 10 10)]
	 [(t guess-width guess-height)
	  (let* ([label (make-label t)]
		 [info (or (assq label label-sizes)
			   (let ([v ((current-tex-sizer) t)])
			     (and v
				  (cons label v))))]
		 [w (if info (cadr info) guess-width)]
		 [h (if info (caddr info) guess-height)]
		 [d (if info (cadddr info) guess-height)])
	    (make-pict `(picture ,w ,(+ d h)
				 (put 0 ,d
				      ,(if (output-measure-commands)
					   (format "\\mztpMeasure{~a}{~a}"
						   t label)
					   t)))
		       w
		       (+ d h)
		       h d
		       null
		       #f
                       #f))]))

      (define (text-line/phantom text phantom . args)
	(apply tex (format "\\makebox[0pt]{\\vphantom{~a}}~a" phantom text) args))

      (define (text-line text . args)
	(apply text-line/phantom text "Xy" args))

      (define (tex-no-descent . args)
	(clip-descent (apply tex args)))

      (define tex-paragraph
	(case-lambda
	 [(w str) (tex-paragraph w str 'top)]
	 [(w str align)
	  (tex (format "\\parbox[~a]{~apt}{~a}"
		       (case align
			 [(top) 't]
			 [(bottom) 'b]
			 [else (error 'tex-paragraph "bad alignment: ~a" align)])
		       w
		       str))]))

      (define delimit-str
	"\\hbox{$\\~a{\\hbox{$\\left~a\\rule{0pt}{~apt}\\right.$}}$}")

      (define (mk-delimit left? middle? right? delim h)
	(let ([str (format delimit-str
			   (cond
			    [left? "mathopen"]
			    [right? "mathclose"]
			    [middle? "mathrel"])
			   delim
			   h)])
	  (tex str 10 h)))

      (define (left-delimit delim h)
	(mk-delimit #t #f #f delim h))
      (define (middle-delimit delim h)
	(mk-delimit #f #t #f delim h))
      (define (right-delimit delim h)
	(mk-delimit #f #f #t delim h))

      (define (left-brace h)
	(left-delimit "\\{" h))
      (define (right-brace h)
	(right-delimit "\\}" h))

      (define (make-h-brace kind w)
	(tex (format "$\\~a{\\hbox{\\begin{picture}(~a,0)(0,0)\\end{picture}}}$"
		     kind w)))

      (define (top-brace w)
	(make-h-brace "overbrace" w))
      (define (bottom-brace w)
	(make-h-brace "underbrace" w))

      (define (generate-possible-slopes n)
	(let ([positives
	       (sort
		(remove-duplicates
		 (apply append (build-list n
					   (lambda (i)
					     (build-list n
							 (lambda (j) (/ (+ i 1) (+ j 1))))))))
		<=)])
	  (append
	   (reverse (map - positives))
	   (list 0)
	   positives)))

      (define (remove-duplicates lst)
	(cond
	 [(null? lst) null]
	 [else (cons (car lst)
		     (remove-duplicates
		      (filter
		       (lambda (x) (not (equal? x (car lst))))
		       (cdr lst))))]))

      (define possible-arrow-slopes (generate-possible-slopes 4))
      (define possible-line-slopes (generate-possible-slopes 6))

      (define (find-slope/robby dh dv possible-slopes)
	(if (= dh 0)
	    'vertical
	    (let* ([slope (/ dv dh)])
	      (let loop ([best (car possible-slopes)]
			 [rst (cdr possible-slopes)])
		(cond
		 [(null? rst) best]
		 [else
		  (if (<= (abs (- slope (car rst)))
			  (abs (- slope best)))
		      (loop (car rst) (cdr rst))
		      (loop best (cdr rst)))])))))

      (define (find-slope dh dv max-slope-num h-within v-within) ; max-slope-num is 4 or 6
	; Result is (slope new-dh), where slope can be 'vertical, in which case
	;                           new-dh is really dv
	(letrec ([best-of-two
		  (lambda (a b)
		    (let*-values ([(ls lh) (a)]
				  [(rs rh) (b)])
		      (if (and ls (or (not rs) (< (abs (- lh dh)) (abs (- rh dh)))))
			  (values ls lh)
			  (values rs rh))))]
		 [search-h
		  (lambda (dh dv depth direction)
		    (if (zero? depth)
			(values #f #f)
			(if (zero? dh)
			    (values 'vertical dv)
			    (let ([slope (/ dv dh)])
			      (if (and (<= (abs (numerator slope)) max-slope-num)
				       (<= (abs (denominator slope)) max-slope-num))
				  (values slope dh)
				  (search-h (+ dh direction) dv (sub1 depth) direction))))))]
		 [sign (lambda (x) (if (positive? x) 1 -1))]
		 [flip
		  (lambda (s l)
		    (if s
			(cond
			 [(eq? s 'vertical) (values (sign l) 0 (abs l))]
			 [(zero? s) (values 'vertical l)]
			 [else (values (/ 1 s) (round (* s l)))])
			(values #f #f)))]
		 [search-v
		  (lambda (dh dv depth direction)
		    (call-with-values (lambda () (search-h dv dh depth direction))
				      flip))]
		 [change-h
		  (lambda (dh dv h-within)
		    (best-of-two (lambda () (search-h dh dv h-within -1))
				 (lambda () (search-h dh dv h-within 1))))]
		 [change-v
		  (lambda (dh dv v-within)
		    (call-with-values (lambda () (change-h dv dh v-within))
				      flip))])
	  (cond
	   [(zero? v-within) (change-h dh dv h-within)]
	   [(zero? h-within) (change-v dh dv v-within)]
	   [else (let-values ([(s l) (search-h dh dv 1 0)])
		   (if s
		       (values s l)
		       (best-of-two
			(lambda ()
			  (best-of-two (lambda () (find-slope dh (add1 dv) max-slope-num h-within (sub1 v-within)))
				       (lambda () (find-slope dh (sub1 dv) max-slope-num h-within (sub1 v-within)))))
			(lambda ()
			  (best-of-two (lambda () (find-slope (add1 dh) dv max-slope-num (sub1 h-within) v-within))
				       (lambda () (find-slope (sub1 dh) dv max-slope-num (sub1 h-within) v-within)))))))])))
      
      (define (parse-slope sl dh dv)
	(if (eq? sl 'vertical)
	    (if (negative? dv)
		(values 0 -1 (- dv))
		(values 0 1 dv))
	    (let ([d (denominator sl)]
		  [n (numerator sl)])
	      (if (negative? dh)
		  (values (- d) (- n) (abs dh))
		  (values d n dh)))))

      (define connect 
	(case-lambda
	 [(x1 y1 x2 y2) (connect x1 y1 x2 y2 #f)]
	 [(x1 y1 x2 y2 arrow?)
	  (if (not (or (use-old-connect) (draw-bezier-lines)))
	      (~connect 'r +inf.0 x1 y1 x2 y2 arrow?)
	      (if (draw-bezier-lines)
		  (let* ([get-len (lambda () (sqrt (+ (* (- x1 x2) (- x1 x2))
						      (* (- y1 y2)  (- y1 y2)))))]
			 [c (if (procedure? (draw-bezier-lines))
				((draw-bezier-lines) (get-len))
				#f)])
		    `((qbezier ,c ,x1 ,y1 ,(quotient (+ x1 x2) 2) ,(quotient (+ y1 y2) 2) ,x2 ,y2)))
		  (let* ([dh (- x2 x1)]
			 [dv (- y2 y1)]
			 [s
			  (if (using-pict2e-package)
			      (/ dv dh)
			      (find-slope/robby
			       (- x2 x1)
			       (- y2 y1)
			       (if arrow? possible-arrow-slopes possible-line-slopes)))])
		    (let-values ([(lh lv ll) (parse-slope s dh dv)])
		      `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,lh ,lv ,ll)))))))]))

      #|

      ;; old body of connect

      (let loop ([dd (if (draw-bezier-lines) 0 1)])
	(if (> dd (if (draw-bezier-lines) 0 4))
	    ; give up
	    (if (draw-bezier-lines)
		(let* ([get-len (lambda () (sqrt (+ (* (- x1 x2) (- x1 x2))
						    (* (- y1 y2)  (- y1 y2)))))]
		       [c (if (procedure? (draw-bezier-lines))
			      ((draw-bezier-lines) (get-len))
			      #f)])
		  `((qbezier ,c ,x1 ,y1 ,(quotient (+ x1 x2) 2) ,(quotient (+ y1 y2) 2) ,x2 ,y2)))
		(let ([xd (- x2 x1)])
		  `((put ,x1 ,y1 (line ,(if (negative? xd) -1 1) 0 ,(abs xd))))))
	    (let-values ([(s l) (find-slope (- x2 x1) (- y2 y1) 
					    (if (using-pict2e-package)
						+inf.0
						(if arrow? 4 6))
					    dd dd)])
	      (if s
		  (let-values ([(lh lv ll) (parse-slope s l)])
		    `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,lh ,lv ,ll))))
		  (loop (add1 dd))))))

      |#

      (define ~connect 
	(case-lambda
	 [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
	 [(exact close-enough x1 y1 x2 y2 arrow?)
	  (if (= x2 x1)
	      ; "infinite" slope
	      (let ([dy (- y2 y1)])
		`((put ,x1 ,y1 (,(if arrow? 'vector 'line) 0 ,(if (negative? dy) -1 1) ,(abs dy)))))
	      (let ([real-slope (/ (- y2 y1) (- x2 x1))]
		    [split (lambda (xm ym)
			     (append
			      (~connect exact close-enough xm ym x1 y1 #f)
			      (~connect exact close-enough xm ym x2 y2 arrow?)))])
		(if (or (>= real-slope (if arrow? 7/8 11/12))
			(<= real-slope (if arrow? -7/8 -11/12)))
		    ; rounds to "infinite" slope
		    (if (> (abs (- x2 x1)) close-enough)
			(split x1 (truncate (quotient (+ y1 y2) 2)))
			(let ([dy (- y2 y1)])
			  `((put ,x1 ,y1 (,(if arrow? 'vector 'line) 
					  0 
					  ,(if (negative? dy) -1 1) ,(abs dy))))))
		    (let* ([slope (let loop ([slope real-slope][tolerances
								(if arrow?
								    '(1/100 1/12 1/4)
								    '(1/100 1/50 1/25 1/10 1/6))])
				    (if (<= (denominator slope) (if arrow? 4 6))
					slope
					(loop (rationalize real-slope (car tolerances))
					      (cdr tolerances))))]
			   [exact-x? (or (eq? exact 'x) (zero? slope))]
			   [r (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))]
			   [dx (cond
				[exact-x? (- x2 x1)]
				[(eq? exact 'r) (truncate (* r (let ([d (denominator slope)]
								     [n (numerator slope)])
								 (/ d (sqrt (+ (* d d) (* n n)))))))]
				[else (truncate (* (/ slope) (- y2 y1)))])]
			   [dy (truncate (* slope dx))])
		      (if (or (and exact-x?
				   (> (abs (- dy (- y2 y1))) close-enough))
			      (and (not exact-x?) (eq? exact 'y)
				   (> (abs (- dx (- x2 x1))) close-enough))
			      (and (not exact-x?) (eq? exact 'y)
				   (> (abs (- (sqrt (+ (* dx dx) (* dy dy))) r)) close-enough)))
			  (if (or exact-x? (eq? exact 'r))
			      (let ([xm (truncate (quotient (+ x1 x2) 2))]) 
				(split xm (+ y1 (truncate (* slope (- xm x1))))))
			      (let ([ym (truncate (quotient (+ y1 y2) 2))]) 
				(split (+ x1 (truncate (* (/ slope) (- ym y1)))) ym)))
			  (let ([same-sign (lambda (v s)
					     (if (negative? s)
						 (- (abs v))
						 (abs v)))])
			    `((put ,x1 ,y1 (,(if arrow? 'vector 'line) 
					    ,(same-sign (denominator slope) (- x2 x1))
					    ,(same-sign (numerator slope) (- y2 y1))
					    ,(abs dx))))))))))]))

      (define (pict->string s)
	(let output ([s (prepare-for-output s)])
	  (if (string? s)
	      s
	      (let ([tag (car s)])
		(case tag
		  [(local)
		   (format "{~a}\n" (output (cadr s)))]
		  [(begin)
		   (apply string-append (map output (cdr s)))]
		  [(picture)
		   (format "\\begin{picture}(~a,~a)\n~a\\end{picture}\n"
			   (cadr s) (caddr s)
			   (apply string-append (map output (cdddr s))))]
		  [(color)
		   (format "\\special{color push ~a}\n~a\\special{color pop}\n"
			   (cadr s) (output (cddr s)))]
		  [(thickness)
		   (format "\\~a~a" (cadr s) (output (caddr s)))]
		  [(put)
		   (format "\\put(~a,~a){~a}\n" (cadr s) (caddr s) (output (cadddr s)))]
		  [(qbezier)
		   (apply format "\\qbezier~a(~a,~a)(~a,~a)(~a,~a)\n"
			  (if (cadr s)
			      (format "[~a]" (cadr s))
			      "")
			  (cddr s))]
		  [(line vector)
		   (format "\\~a(~a,~a){~a}" tag (cadr s) (caddr s) (cadddr s))]
		  [(circle)
		   (format "\\circle{~a}" (cadr s))]
		  [(circle*)
		   (format "\\circle*{~a}" (cadr s))]
		  [(frame)
		   (format "\\frame{~a}" (output (cadr s)))]
		  [(colorbox)
		   (format "\\colorbox{~a}{~a}" (cadr s) (output (caddr s)))]
		  [(oval)
		   (format "\\oval(~a,~a)~a" (caddr s) (cadddr s) (cadr s))]
		  [(make-box)
		   (format "\\makebox(~a, ~a)[~a]{~a}"
			   (cadr s) (caddr s) (cadddr s) (car (cddddr s)))]
		  [(prog)
		   (error 'pict->string "cannot handle prog pict")]
		  [else (error 'pict->string "bad tag: ~s" tag)])))))

      (define pict->commands pict->command-list)

      (define (convert-pict p v d) d)
