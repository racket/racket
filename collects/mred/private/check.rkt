(module check mzscheme
  (require mzlib/class
           (prefix wx: "kernel.rkt")
           "wx.rkt"
           "const.rkt")
  (provide (protect (all-defined)))

  (define (key-code-symbol? x)
    (and (member x '(start cancel clear shift rshift control rcontrol
                           menu pause capital prior next end home left 
                           up right down escape select print execute
                           snapshot insert help numpad0 numpad1 numpad2
                           numpad3 numpad4 numpad5 numpad6 numpad7
                           numpad8 numpad9 numpad-enter multiply add
                           separator subtract decimal divide f1 f2 f3
                           f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15
                           f16 f17 f18 f19 f20 f21 f22 f23 f24 numlock
                           scroll wheel-up wheel-down wheel-left
                           wheel-right release press))
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
	  (raise-arguments-error (who->name cwho)
                                 "container is not yet fully initialized" 
                                 "container" p)))))

  (define (check-instance who class class-name false-ok? v)
    (unless (or (and false-ok? (not v)) (is-a? v class))
      (raise-argument-error (who->name who) 
                            (let ([c (format "(is-a?/c ~a)" class-name)])
                              (if false-ok?
                                  (format "(or/c ~a #f)" c)
                                  c))
                            v)))

  (define (check-string/false who str)
    (unless (or (not str) (string? str))
      (raise-argument-error (who->name who) "(or/c string? #f)" str)))

  (define (check-path who str)
    (unless (path-string? str)
      (raise-argument-error (who->name who) "path-string?" str)))

  (define (check-path/false who str)
    (unless (or (not str) (path-string? str))
      (raise-argument-error (who->name who) "(or/c path-string? #f)" str)))

  (define (check-string who str)
    (unless (string? str)
      (raise-argument-error (who->name who) "string?" str)))

  (define (check-label-string who str)
    (unless (label-string? str)
      (raise-argument-error (who->name who) "label-string?" str)))

  (define (check-label-string/false who str)
    (unless (or (not str) (label-string? str))
      (raise-argument-error (who->name who) "(or/c label-string? #f)" str)))

  (define (check-char/false who c)
    (unless (or (not c) (char? c))
      (raise-argument-error (who->name who) "(or/c char? #f)" c)))

  (define (check-callback who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 2))
      (raise-argument-error (who->name who) "(procedure-arity-includes/c 2)" callback)))

  (define (check-callback1 who callback)
    (unless (and (procedure? callback)
		 (procedure-arity-includes? callback 1))
      (raise-argument-error (who->name who) "(procedure-arity-includes/c 2)" callback)))

  (define (check-bounded-integer min max false-ok?)
    (lambda (who range)
      (unless (or (and false-ok? (not range))
		  (and (integer? range) (exact? range) (<= min range max)))
	(raise-argument-error (who->name who) 
                              (let ([i (format "(integer-in ~a ~a)" min max)])
                                (if false-ok?
                                    (format "(or/c ~a #f)" i)
                                    i))
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
      (raise-argument-error (who->name cwho)
                            "(or/c #f (integer-in 1 10000))"
                            wheel-step)))

  (define (check-fraction who x)
    (unless (and (real? x) (<= 0.0 x 1.0))
      (raise-argument-error (who->name who) 
                            "(real-in 0.0 1.0)"
                            x)))

  (define (-check-non-negative-integer who i false-ok?)
    (when (or i (not false-ok?))
      (unless (and (integer? i) (exact? i) (not (negative? i)))
	(raise-argument-error (who->name who) 
                              (if false-ok?
                                  "(or/c exact-nonnegative-integer? #f)" 
                                  "exact-nonnegative-integer?")
                              i))))

  (define (check-non-negative-integer who i)
    (-check-non-negative-integer who i #f))

  (define (check-non-negative-integer/false who i)
    (-check-non-negative-integer who i #t))

  (define check-dimension (check-bounded-integer 0 10000 #t))
  (define check-non#f-dimension (check-bounded-integer 0 10000 #f))

  (define (check-label-string-or-bitmap who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%))
      (raise-argument-error (who->name who)  "(or/c label-string? (is-a?/c bitmap%))" label)))

  (define (check-label-string-or-bitmap-or-both who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%)
                (and (list? label) 
                     (= 3 (length label)) 
                     (is-a? (car label) wx:bitmap%)
                     (label-string? (cadr label))
                     (memq (caddr label) '(left right top bottom))))
      (raise-argument-error (who->name who) 
                            (string-append
                             "(or/c label-string?\n"
                             "      (is-a?/c bitmap%)\n"
                             "      (list/c (is-a?/c bitmap%)\n"
                             "              string\n"
                             "              (or/c 'left 'right 'top 'bottom)))")
                            label)))

  (define (check-label-string-or-bitmap/false who label)
    (unless (or (not label) (label-string? label) (is-a? label wx:bitmap%))
      (raise-argument-error (who->name who) "(or/c label-string? (is-a?/c bitmap%) #f)" label)))

  (define (check-label-string/bitmap/iconsym who label)
    (unless (or (label-string? label) (is-a? label wx:bitmap%)
		(memq label '(app caution stop)))
      (raise-argument-error (who->name who) "(or/c label-string? (is-a?/c bitmap%) 'app 'caution 'stop)" label)))

  (define (check-font who f)
    (unless (or (eq? f no-val) (f . is-a? . wx:font%))
      (raise-argument-error (who->name who) "(is-a?/c font%)" f)))

  (define (check-style who reqd other-allowed style)
    (unless (and (list? style) (andmap symbol? style))
      (raise-argument-error (who->name who) "(listof symbol?)" style))
    (when reqd
      (unless (ormap (lambda (i) (memq i reqd)) style)
        (letrec ([or-together (lambda (l)
                                (if (= (length l) 2)
                                    (format "~e or ~e" (car l) (cadr l))
                                    (let loop ([l l])
                                      (if (null? (cdr l))
                                          (format "or ~e" (car l))
                                          (format "~e, ~a" (car l) (loop (cdr l)))))))])
          (raise-arguments-error (who->name who)
                                 (string-append
                                  "missing a required option in given style list\n"
                                  "  must include: " (or-together reqd))
                                 "given" style))))
    (if (and (not reqd) (null? other-allowed))
	(unless (null? style)
	  (raise-arguments-error (who->name who) 
                                 "empty style list required" 
                                 "given" style))
	(let* ([l (append (or reqd null) other-allowed)]
	       [bad (ormap (lambda (x) (if (memq x l) #f x)) style)])
	  (when bad
	    (raise-arguments-error (who->name who) 
                                   "invalid symbol in given style list"
                                   "invalid symbol" bad
                                   "given" style))
	  (let loop ([l style])
	    (unless (null? l)
	      (when (memq (car l) (cdr l))
		(raise-arguments-error (who->name who) 
                                       "duplicate style in given style list"
                                       "duplicate" (car l)
                                       "given" style))
	      (loop (cdr l))))))))
