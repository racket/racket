;;; util.ss -- utility procedures for MysterX

(module util mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "list.ss"))	   

  (provide 
   fold-strings-with-spaces
   map-to-string
   empty-string?
   bool->string
   exact-with-bounds?
   list-pos
   remove-ws
   symbols->string
   hex-digit-string?
   hex-color-string?
   empty-property-error)
  
  (define (fold-strings-with-spaces strs)
    (foldr (lambda (s accum) 
	     (if (string? accum)
		 (string-append s " " accum)
		 s))
	     'dummy
	     strs))

  (define (map-to-string f)
    (lambda (lst)
    (let loop ([lst lst])
      (cond
       [(null? lst) ""]
       [(null? (cdr lst))
	(f (car lst))]
       [else
	(string-append (f (car lst)) 
		       " "
		       (loop (cdr lst)))]))))

  (define empty-string?
    (lambda (s)
      (and (string? s) 
	   (eq? (string-length s) 0))))

  (define (bool->string v)
    (if v
	"true"
	"false"))

  (define (exact-with-bounds? n lo hi)
	(and (number? n)
	     (exact? n)
	     (>= n lo)
	     (<= n hi)))

  (define (list-pos v lst)
    (let loop ([lst lst]
	       [n 0])
      (if (eq? v (car lst))
	  n
	  (loop (cdr lst) (add1 n)))))
   
  (define remove-ws ; remove leading whitespace
    (lambda (cs)
      (cond [(null? cs) '()]
	    [(char-whitespace? (car cs))
	     (remove-ws (cdr cs))]
	    [else cs])))

  (define symbols->string ; '(a b c ...) => "a b c ..."
    (lambda (syms)
      (cond [(null? syms) ""]
	    [(null? (cdr syms))
	     (symbol->string (car syms))]
	    [else
	     (string-append (symbol->string (car syms))
			    " "
			    (symbols->string (cdr syms)))])))

  (define (hex-digit-string? elt)
    (let ([lst (string->list elt)]
	  [hex-digit? 
	   (lambda (c)
	     (or (char-numeric? c)
		 (memq c '(#\a #\b #\c #\d #\e #\f 
			   #\A #\B #\C #\D #\E #\F))))])
      (and (= (length lst) 7)
	   (eq? (car lst) #\#)
	   (andmap hex-digit? (cdr lst)))))

  (define (hex-color-string? s)
    (and (string? s)
	 (hex-digit-string? s)))

  (define empty-property-error
     (lambda (p)
       (error (format "Empty value for property ~a" p)))))




