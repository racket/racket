(module check mzscheme
  (require mzlib/class
           (prefix wx: "kernel.rkt")
           "wx.rkt"
           "const.rkt")
  (provide (protect (all-defined)))

  (define (key-code-symbol? x)
    (and (member x '(start cancel clear shift control menu pause
                           capital prior next end home left up right down
                           escape select print execute snapshot insert help 
                           numpad0 numpad1 numpad2 numpad3 numpad4 numpad5
                           numpad6 numpad7 numpad8 numpad9 numpad-enter 
                           multiply add separator subtract decimal divide 
                           f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14
                           f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 
                           numlock scroll wheel-up wheel-down release press))
         #t))
  
  (define (who->name who)
    (cond
     [(symbol? who) who]
     [(eq? (car who) 'method) (string->symbol (format "~a in ~a" (caddr who) (cadr who)))]
     [(eq? (car who) 'iconstructor) (iconstructor-name (cadr who))]
     [else (constructor-name (cadr who))]))

  (define (label-string? s)
    (and (string? s) 
	 (let ([l (string-length s)])
	   (and l
		(<= 0 l 200)))))

  (define (constructor-name who)
    (string->symbol (format "initialization for ~a%" who)))

  (define (iconstructor-name who)
    (string->symbol (format "initialization for a class that implements ~a<%>" who)))

  (define (check-orientation cwho l)
    (check-style cwho '(vertical horizontal) '(vertical-label horizontal-label deleted) l))

  (define (check-container-ready cwho p)
    (when p
      (let ([wx (mred->wx p)])
	(unless wx
	  (raise-mismatch-error (who->name cwho)
				"container is not yet fully initialized: " 
				p)))))

  (define (check-instance who class class-name false-ok? v)
    (unless (or (and false-ok? (not v)) (is-a? v class))
      (raise-type-error (who->name who) (format "~a object~a" class-name (if false-ok? " or #f" "")) v)))

  (define (check-string/false who str)
    (unless (or (not str) (string? str))
      (raise-type-error (who->name who) "string or #f" str)))

  (define (check-path who str)
    (unless (path-string? str)
      (raise-type-error (who->name who) "path or string" str)))

  (define (check-path/false who str)
    (unless (or (not str) (path-string? str))
      (raise-type-error (who->name who) "path, string, or #f" str)))

  (define (check-string who str)
    (unless (string? str)
      (raise-type-error (who->name who) "string" str)))

  (define (check-label-string who str)
    (unless (label-string? str)
      (raise-type-error (who->name who) "string (up to 200 characters)" str)))

  (define (check-label-string/false who str)
    (unless (or (not str) (label-string? str))
      (raise-type-error (who->name who) "string  (up to 200 characters) or #f" str)))

  (define (check-char/false who c)
    (unless (or (not c) (char? c))
      (raise-type-error (who->name who) "character or #f" c)))

  (define (check-callback who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 2))
      (raise-type-error (who->name who) "procedure of arity 2" callback)))

  (define (check-callback1 who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 1))
      (raise-type-error (who->name who) "procedure of arity 1" callback)))

  (define (check-bounded-integer min max false-ok?)
    (lambda (who range)
      (unless (or (and false-ok? (not range))
		  (and (integer? range) (exact? range) (<= min range max)))
	(raise-type-error (who->name who) 
			  (format "exact integer in [~a, ~a]~a"
				  min max
				  (if false-ok? " or #f" ""))
			  range))))

  (define check-range-integer (check-bounded-integer 0 10000 #f))

  (define check-slider-integer (check-bounded-integer -10000 10000 #f))

  (define check-init-pos-integer (check-bounded-integer -10000 10000 #t))

  (define check-margin-integer (check-bounded-integer 0 1000 #f))

  (define check-gauge-integer (check-bounded-integer 1 1000000 #f))

  (define (check-wheel-step cwho wheel-step)
    (when (and wheel-step
	       (not (and (integer? wheel-step)
			 (exact? wheel-step)
			 (<= 1 wheel-step 10000))))
      (raise-type-error (who->name cwho)
			"#f or exact integer in [1,10000]"
			wheel-step)))

  (define (check-fraction who x)
    (unless (and (real? x) (<= 0.0 x 1.0))
      (raise-type-error (who->name who) 
			"real number in [0.0, 1.0]"
			x)))

  (define (-check-non-negative-integer who i false-ok?)
    (when (or i (not false-ok?))
      (unless (and (integer? i) (exact? i) (not (negative? i)))
	(raise-type-error (who->name who) 
			  (if false-ok?
			      "non-negative exact integer or #f" 
			      "non-negative exact integer" )
			  i))))

  (define (check-non-negative-integer who i)
    (-check-non-negative-integer who i #f))

  (define (check-non-negative-integer/false who i)
    (-check-non-negative-integer who i #t))

  (define check-dimension (check-bounded-integer 0 10000 #t))
  (define check-non#f-dimension (check-bounded-integer 0 10000 #f))

  (define (check-label-string-or-bitmap who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%))
      (raise-type-error (who->name who) "string (up to 200 characters) or bitmap% object" label)))

  (define (check-label-string-or-bitmap-or-both who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%)
                (and (list? label) 
                     (= 3 (length label)) 
                     (is-a? (car label) wx:bitmap%)
                     (label-string? (cadr label))
                     (memq (caddr label) '(left right top bottom))))
      (raise-type-error (who->name who) 
                        (string-append
                         "string (up to 200 characters), bitmap% object, or list of bitmap%, "
                         "string, and image-placement symbol ('left, 'right, 'top, or 'bottom)")
                        label)))

  (define (check-label-string-or-bitmap/false who label)
    (unless (or (not label) (label-string? label) (is-a? label wx:bitmap%))
      (raise-type-error (who->name who) "string (up to 200 characters), bitmap% object, or #f" label)))

  (define (check-label-string/bitmap/iconsym who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%)
		(memq label '(app caution stop)))
      (raise-type-error (who->name who) "string (up to 200 characters), bitmap% object, or icon symbol" label)))

  (define (check-font who f)
    (unless (or (eq? f no-val) (f . is-a? . wx:font%))
      (raise-type-error (who->name who) "font% object" f)))

  (define (check-style who reqd other-allowed style)
    (unless (and (list? style) (andmap symbol? style))
      (raise-type-error (who->name who) "list of style symbols" style))
    (when reqd
      (letrec ([or-together (lambda (l)
			      (if (= (length l) 2)
				  (format "~a or ~a" (car l) (cadr l))
				  (let loop ([l l])
				    (if (null? (cdr l))
					(format "or ~a" (car l))
					(format "~a, ~a" (car l) (loop (cdr l)))))))])
	(unless (ormap (lambda (i) (memq i reqd)) style)
	  (raise-type-error (who->name who)
			    (format "style list, missing ~a"
				    (if (= (length reqd) 1)
					(car reqd)
					(string-append
					 "one of "
					 (or-together reqd))))
			    style))))
    (if (and (not reqd) (null? other-allowed))
	(unless (null? style)
	  (raise-type-error (who->name who) "empty style list" style))
	(let* ([l (append (or reqd null) other-allowed)]
	       [bad (ormap (lambda (x) (if (memq x l) #f x)) style)])
	  (when bad
	    (raise-type-error (who->name who) (format "style list, ~e not allowed" bad) style))
	  (let loop ([l style])
	    (unless (null? l)
	      (when (memq (car l) (cdr l))
		(raise-type-error (who->name who) (format "style list, ~e allowed only once" (car l)) style))
	      (loop (cdr l))))))))
