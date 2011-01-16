; QuickCheck clone

(define-record-type :generator
  (make-generator proc)
  generator?
  ;; int(size) random-generator -> val
  (proc generator-proc))

; for transliteration from Haskell
(define (return val)
  (make-generator
   (lambda (size rgen)
     val)))

(define (>>= m1 k)
  (let ((proc1 (generator-proc m1)))
    (make-generator
     (lambda (size rgen)
       (call-with-values
	   (lambda ()
	     (random-generator-split rgen))
	 (lambda (rgen1 rgen2)
	   (let ((gen (k (proc1 size rgen1))))
	     ((generator-proc gen) size rgen2))))))))

(define (sequence gens)
  (if (null? gens)
      (return '())
      (>>= (car gens)
	   (lambda (val)
	     (>>= (sequence (cdr gens))
		  (lambda (rest)
		    (return (cons val rest))))))))

; for export
(define generator-unit return)
(define generator-bind >>=)
(define generator-sequence sequence)

(define (lift->generator proc . gens)
  (>>= (sequence gens)
       (lambda (vals)
	 (return (apply proc vals)))))

; [lower, upper]
(define (choose-integer lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-integer rgen lower upper))
       (lambda (n _)
	 n)))))

(define (choose-real lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-real rgen lower upper))
       (lambda (n _)
	 n)))))

(define choose-ascii-char
  (lift->generator integer->char (choose-integer 0 127)))

(define choose-ascii-letter
  (lift->generator (lambda (i)
		     (string-ref
		      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" i))
		   (choose-integer 0 51)))

(define choose-printable-ascii-char
  (lift->generator integer->char (choose-integer 32 127)))

(define max-scalar-value #x10FFFF)
(define gap-start #xD800)
(define gap-end #xE000)
(define gap-size (- gap-end gap-start))

(define (choose-char lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-integer rgen (char->integer lower)
			   (min (char->integer upper)
				(- max-scalar-value gap-size))))
       (lambda (n _)
	 (integer->char
	  (if (< n gap-start)
	      n
	      (+ n gap-size))))))))

; int (generator a) -> (generator a)
(define (variant v gen)
  (let ((proc (generator-proc gen)))
    (make-generator
     (lambda (size rgen)
       (let loop ((v (+ 1 v))
		  (rgen rgen))
	 (if (zero? v)
	     (proc size rgen)
	     (call-with-values
		 (lambda ()
		   (random-generator-split rgen))
	       (lambda (rgen1 rgen2)
		 (loop (- v 1) rgen2)))))))))

; int random-gen (generator a) -> a
(define (generate n rgen gen)
  (call-with-values
      (lambda ()
	(random-integer rgen 0 n))
    (lambda (size nrgen)
      ((generator-proc gen) size nrgen))))

; (vals -> (generator b)) -> (generator (vals -> b))
(define (promote proc)
  (make-generator
   (lambda (size rgen)
     (lambda vals
       (let ((g (apply proc vals)))
	 ((generator-proc g) size rgen))))))

; (int -> (generator a)) -> (generator a)
(define (sized proc)
  (make-generator
   (lambda (size rgen)
     (let ((g (proc size)))
       ((generator-proc g) size rgen)))))

; (list a) -> (generator a)
(define (choose-one-of lis)
  (lift->generator (lambda (n)
		     (list-ref lis n))
		   (choose-integer 0 (- (length lis) 1))))

; vector from the paper
; (generator a) int -> (generator (list a))
(define (choose-list el-gen n)
  (let recur ((n n))
    (if (zero? n)
	(return '())
	(>>= el-gen
	     (lambda (val)
	       (>>= (recur (- n 1))
		    (lambda (rest)
		      (return (cons val rest)))))))))

; (generator char) int -> (generator string)
(define (choose-string char-gen n)
  (lift->generator list->string (choose-list char-gen n)))

(define (choose-symbol char-gen n)
  (>>= (choose-string char-gen n)
       (lambda (s)
	 (return (string->symbol s)))))

(define (choose-vector el-gen n)
  (lift->generator list->vector (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(define (choose-mixed gens)
  (>>= (choose-one-of gens)
       force))

; (list (pair int (generator a))) -> (generator a)
(define (choose-with-frequencies lis)
  (>>= (choose-integer 1 (apply + (map car lis)))
       (lambda (n)
	 (pick n lis))))

(define (pick n lis)
  (let ((k (caar lis)))
    (if (<= n k)
	(cdar lis)
	(pick (- n k) lis))))

(define-record-type :arbitrary
  (make-arbitrary generator transformer)
  arbitrary?
  ;; (generator a)
  (generator arbitrary-generator)
  ;; a (generator b) -> (generator b)
  (transformer arbitrary-transformer))

; class Arbitrary a where
;    arbitrary   :: Gen a
;    coarbitrary :: a -> Gen b -> Gen b

(define (coarbitrary arb val gen)
  ((arbitrary-transformer arb) val gen))

(define arbitrary-boolean
  (make-arbitrary (choose-one-of '(#t #f))
		  (lambda (a gen)
		    (variant (if a 0 1) gen))))

(define arbitrary-integer
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-integer (- n) n)))
		  (lambda (n gen)
		    (variant (if (>= n 0)
				 (* 2 n)
				 (+ (* 2 (- n)) 1))
			     gen))))

(define arbitrary-natural
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-integer 0 n)))
		  (lambda (n gen)
		    (variant n gen))))

(define arbitrary-ascii-char
  (make-arbitrary choose-ascii-char
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-ascii-letter
  (make-arbitrary choose-ascii-letter
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-printable-ascii-char
  (make-arbitrary choose-printable-ascii-char
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-char
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-char (integer->char 0)
				  (integer->char n))))
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define (make-rational a b)
  (/ a
     (+ 1 b)))

(define arbitrary-rational
  (make-arbitrary (lift->generator make-rational
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-natural))
		  (lambda (r gen)
		    (coarbitrary arbitrary-integer
				 (numerator r)
				 (coarbitrary arbitrary-integer
					      (denominator r) gen)))))

(define (fraction a b c)
  (+ a
     (exact->inexact (/ b
			(+ (abs c) 1)))))

(define arbitrary-real
  (make-arbitrary (lift->generator fraction
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-integer))
		  (lambda (r gen)
		    (let ((fr (rationalize (inexact->exact r) 1/1000)))
		      (coarbitrary arbitrary-integer
				   (numerator fr)
				   (coarbitrary arbitrary-integer
						(denominator fr) gen))))))


(define (arbitrary-mixed pred+arbitrary-promise-list)
  (make-arbitrary (choose-mixed (map (lambda (p)
				       (delay (arbitrary-generator (force (cdr p)))))
				     pred+arbitrary-promise-list))
		  (lambda (val gen)
		    (let loop ((lis pred+arbitrary-promise-list) (n 0))
		      (cond
		       ((null? lis)
			(assertion-violation 'arbitrary-mixed
					     "value matches none of the predicates"
					     val pred+arbitrary-promise-list))
		       (((caar lis) val)
			(variant n gen))
		       (else
			(loop (cdr lis) (+ 1 n))))))))

(define (arbitrary-one-of eql? . vals)
  (make-arbitrary (choose-one-of vals)
		  (lambda (val gen)
		    (let loop ((lis vals) (n 0))
		      (cond
		       ((null? lis)
			(assertion-violation 'arbitrary-one-of
					     "value is not in the list"
					     val vals))
		       ((eql? (car lis) val)
			(variant n gen))
		       (else
			(loop (cdr lis) (+ 1 n))))))))
		       
(define (arbitrary-pair arbitrary-car arbitrary-cdr)
  (make-arbitrary (lift->generator cons
				   (arbitrary-generator arbitrary-car)
				   (arbitrary-generator arbitrary-cdr))
		  (lambda (p gen)
		    (coarbitrary arbitrary-car
				 (car p)
				 (coarbitrary arbitrary-cdr
					      (cdr p) gen)))))

; a tuple is just a non-uniform list 
(define (arbitrary-tuple . arbitrary-els)
  (make-arbitrary (apply lift->generator
			 list
			 (map arbitrary-generator arbitrary-els))
		  (lambda (lis gen)
		    (let recur ((arbitrary-els arbitrary-els)
				(lis lis))
		      (if (null? arbitrary-els)
			  gen
			  ((arbitrary-transformer (car arbitrary-els))
			   (car lis)
			   (recur (cdr arbitrary-els)
				  (cdr lis))))))))

(define (arbitrary-record construct accessors . arbitrary-els)
  (make-arbitrary (apply lift->generator
			 construct
			 (map arbitrary-generator arbitrary-els))
		  (lambda (rec gen)
		    (let recur ((arbitrary-els arbitrary-els)
				(lis (map (lambda (accessor) (accessor rec)) accessors)))
		      (if (null? arbitrary-els)
			  gen
			  ((arbitrary-transformer (car arbitrary-els))
			   (car lis)
			   (recur (cdr arbitrary-els)
				  (cdr lis))))))))

(define (arbitrary-sequence choose-sequence sequence->list arbitrary-el)
  (make-arbitrary (sized
		   (lambda (n)
		     (>>= (choose-integer 0 n)
			  (lambda (length)
			    (choose-sequence (arbitrary-generator arbitrary-el) length)))))
		  (lambda (seq gen)
		    (let recur ((lis (sequence->list seq)))
		      (if (null? lis)
			  (variant 0 gen)
			  ((arbitrary-transformer arbitrary-el) 
			   (car lis)
			   (variant 1 (recur (cdr lis)))))))))

(define (arbitrary-list arbitrary-el)
  (arbitrary-sequence choose-list values arbitrary-el))

(define (arbitrary-vector arbitrary-el)
  (arbitrary-sequence choose-vector vector->list arbitrary-el))

(define arbitrary-ascii-string
  (arbitrary-sequence choose-string string->list arbitrary-ascii-char))

(define arbitrary-printable-ascii-string
  (arbitrary-sequence choose-string string->list arbitrary-printable-ascii-char))

(define arbitrary-string
  (arbitrary-sequence choose-string string->list arbitrary-char))

(define arbitrary-symbol
  (arbitrary-sequence choose-symbol
		      (lambda (symbol)
			(string->list (symbol->string symbol)))
		      arbitrary-ascii-letter))

(define (arbitrary-procedure arbitrary-result . arbitrary-args)
  (let ((arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)))
    (make-arbitrary (promote
		     (lambda args
		       ((arbitrary-transformer arbitrary-arg-tuple)
			args
			(arbitrary-generator arbitrary-result))))
		    (lambda (proc gen)
		      (>>= (arbitrary-generator arbitrary-arg-tuple)
			   (lambda (args)
			     ((arbitrary-transformer arbitrary-result)
			      (apply proc args)
			      gen)))))))


(define-record-type :property
  (make-property proc arg-names args)
  property?
  (proc property-proc)
  (arg-names property-arg-names)
  ;; (list (union arbitrary generator))
  (args property-args))

(define-syntax property
  (syntax-rules ()
    ((property ((?id ?gen) ...) ?body0 ?body1 ...)
     (make-property (lambda (?id ...)
		      ?body0 ?body1 ...)
		    '(?id ...)
		    (list ?gen ...)))))

(define-record-type :result
  (make-result ok stamp arguments-list)
  check-result?
  ;; () = unknown, #t, #f
  (ok result-ok)
  (stamp result-stamp) 
  ;; (list (list (pair (union #f symbol) value)))
  (arguments-list result-arguments-list))

(define (result-with-ok res ok)
  (make-result ok
	       (result-stamp res)
	       (result-arguments-list res)))

(define (result-add-stamp res stamp)
  (make-result (result-ok res)
	       (cons stamp (result-stamp res))
	       (result-arguments-list res)))

; result (list (pair (union #f symbol) value)) -> result
(define (result-add-arguments res args)
  (make-result (result-ok res)
	       (result-stamp res)
	       (cons args (result-arguments-list res))))

(define nothing
  (make-result '() '() '()))

; A testable value is one of the following:
; - a :property object
; - a boolean
; - a result record
; - a generator of a result record

(define (coerce->result-generator thing)
  (cond
   ((property? thing)
    (for-all/names (property-proc thing)
		   (property-arg-names thing)
		   (property-args thing)))
   ((boolean? thing) (return (result-with-ok nothing thing)))
   ((check-result? thing) (return thing))
   ((generator? thing) thing)
   (else
    (assertion-violation 'coerce->result-generator 
			 "cannot be coerced to a result generator"
			 thing))))

(define (coerce->generator thing)
  (cond
   ((generator? thing) thing)
   ((arbitrary? thing) (arbitrary-generator thing))
   (else
    (assertion-violation 'coerce->generator
			 "cannot be coerced to a generator" thing))))

(define (for-all proc . args)
  (>>= (sequence (map coerce->generator args))
       (lambda (args)
	 (>>= (coerce->result-generator (apply proc args))
	      (lambda (res)
		(return (result-add-arguments res
					      (map (lambda (arg) (cons #f arg)) args))))))))

(define (for-all/names proc arg-names args)
  (>>= (sequence (map coerce->generator args))
       (lambda (args)
	 (>>= (coerce->result-generator (apply proc args))
	      (lambda (res)
		(return (result-add-arguments res (map cons arg-names args))))))))

(define-syntax ==>
  (syntax-rules ()
    ((==> ?bool ?prop)
     (if ?bool
	 ?prop
	 (return nothing)))))

(define (label str testable)
  (>>= (coerce->result-generator testable)
       (lambda (res)
	 (return (result-add-stamp res str)))))

(define-syntax classify
  (syntax-rules ()
    ((classify ?really? ?str ?testable)
     (let ((testable ?testable))
       (if ?really?
	   (label ?str testable)
	   testable)))))

(define-syntax trivial
  (syntax-rules ()
    ((trivial ?really? ?testable)
     (classify ?really? "trivial" ?testable))))

(define (collect lbl testable)
  (label (external-representation lbl) testable))
 
(define (external-representation obj)
  (let ((port (make-string-output-port)))
    (write obj port)
    (string-output-port-output port)))

; Running the whole shebang

(define-record-type :config
  (make-config max-test max-fail size print-every)
  config?
  (max-test config-max-test)
  (max-fail config-max-fail)
  (size config-size)
  (print-every config-print-every))

(define quick
  (make-config 100
	       1000
	       (lambda (n)
		 (+ 3 (quotient n 2)))
	       values))

(define verbose
  (make-config 100
	       1000
	       (lambda (n)
		 (+ 3 (quotient n 2)))
	       (lambda (n args)
		 (display n)
		 (display ":")
		 (newline)
		 (for-each (lambda (arg)
			     (display arg)
			     (newline))
			   args))))

(define (check-results config prop)
  (let ((rgen (make-random-generator 0)))
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(define (check config prop)
  (call-with-values
      (lambda ()
	(check-results config prop))
    report-result))

(define (quickcheck-results prop)
  (check-results quick prop))

(define (quickcheck prop)
  (check quick prop))

; returns three values:
; - ntest
; - stamps
; - #t for success, #f for exhausted, result for failure

(define (tests config gen rgen ntest nfail stamps)
  (let loop ((rgen rgen)
	     (ntest ntest)
	     (nfail nfail)
	     (stamps stamps))
    (cond
     ((= ntest (config-max-test config))
      (values ntest stamps #t))
     ((= ntest (config-max-fail config))
      (values ntest stamps #f))
     (else
      (call-with-values
	  (lambda ()
	    (random-generator-split rgen))
	(lambda (rgen1 rgen2)
	  (let ((result (generate ((config-size config) ntest) rgen2 gen)))
	    ((config-print-every config) ntest (result-arguments-list result))
	    (case (result-ok result)
	      ((()) (loop rgen1 ntest (+ 1 nfail) stamps))
	      ((#t) (loop rgen1 (+ 1 ntest) nfail (cons (result-stamp result) stamps)))
	      ((#f)
	       (values ntest stamps result))))))))))

(define (report-result ntest stamps maybe-result)
  (case maybe-result
    ((#t)
     (done "OK, passed" ntest stamps))
    ((#f)
     (done "Arguments exhausted after" ntest stamps))
    (else
     (display "Falsifiable, after ")
     (display ntest)
     (display " tests:")
     (newline)
     (for-each write-arguments
	       (result-arguments-list maybe-result)))))

; (pair (union #f symbol) value)
(define (write-argument arg)
  (if (car arg)
      (begin
	(display (car arg))
	(display " = "))
      (values))
  (write (cdr arg)))

; (list (pair (union #f symbol) value))
(define (write-arguments args)
  (if (pair? args)
      (begin
	(write-argument (car args))
	(for-each (lambda (arg)
		    (display " ")
		    (write-argument arg))
		  (cdr args))
	(newline))
      (values)))
		   
(define (done mesg ntest stamps)
  (display mesg)
  (display " ")
  (display ntest)
  (display " tests")
  (let* ((sorted (list-sort stamp<? (filter pair? stamps)))
	 (grouped (group-sizes sorted))
	 (sorted (list-sort (lambda (p1 p2)
			      (< (car p1) (car p2)))
			    grouped))
	 (entries (map (lambda (p)
			 (let ((n (car p))
			       (lis (cdr p)))
			 (string-append (number->string (quotient (* 100 n) ntest))
					"% "
					(intersperse ", " lis))))
		       (reverse sorted))))
    (cond
     ((null? entries)
      (display ".")
      (newline))
     ((null? (cdr entries))
      (display " (")
      (display (car entries))
      (display ").")
      (newline))
     (else
      (display ".") (newline)
      (for-each (lambda (entry)
		  (display entry)
		  (display ".")
		  (newline))
		entries)))))

(define (group-sizes lis)
  (if (null? lis)
      '()
      (let loop ((current (car lis))
		 (size 1)
		 (lis (cdr lis))
		 (rev '()))
	(cond
	 ((null? lis)
	  (reverse (cons (cons size current) rev)))
	 ((equal? current (car lis))
	  (loop current (+ 1 size) (cdr lis) rev))
	 (else
	  (loop (car lis) 1 (cdr lis) (cons (cons size current) rev)))))))

(define (stamp<? s1 s2)
  (cond
   ((null? s1)
    (pair? s1))
   ((null? s2)
    #t)
   ((string<? (car s1) (car s2))
    #t)
   ((string=? (car s1) (car s2))
    (stamp<? (cdr s1) (cdr s2)))
   (else #f)))


(define (intersperse del lis)
  (if (null? lis)
      ""
      (string-append (car lis)
		     (let recur ((lis (cdr lis)))
		       (if (null? lis)
			   ""
			   (string-append del
					  (recur (cdr lis))))))))
