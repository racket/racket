(module base-gm mzscheme
  (require (lib "list.ss")
	   (lib  "etc.ss"))
  
  (provide cons-to-end
	   assoc-get
	   debug
	   make-debug
	   to-string
	   member-eq?	   
	   string->char
	   last
	   member-str?
	   quicksort-vector!
	   for
	   for-vector
	   but-last
	   halt
	   prog1
	   struct->list
           for-list

	   make-hash
	   hash-get
	   hash-put!
	   hash-remove!
	   hash-map
	   hash-for-each
	   hash-mem?

	   (all-from (lib "list.ss"))
	   (all-from (lib "etc.ss")))
  
  (define-syntax prog1
    (syntax-rules
	() 
      {(prog1 arg1 args ...)
       (let ((v arg1))
	 args ...
	 v)}))
  
  (define-syntax halt
    (syntax-rules
	()
      [(halt arg ...)
       (begin
	 (debug "There was a problem with " arg ...)
	 (error "Error."))]))  
  
;;;(define (halt . args)
;;;  (apply error
;;;	 (list (string-append
;;;		"Error: "
;;;		(foldl string-append "" (reverse (map to-string args)))))))
  
  (define (but-last ls)
    (cond
     ((empty? ls) (error "incorrect list to butlast"))
     ((empty? (rest ls)) empty)
     (else (cons (first ls) (but-last (rest ls))))))
  
  (define-syntax for
    (syntax-rules
	()
      [(for x start stop body ...)
       (let ((x start))
	 (letrec ((loop
		   (lambda ()
		     (if (> x stop)
			 'done
			 (begin
			   body ...
			   (set! x (+ x 1))
			   (loop))))))
	   (loop)))]))
  
  (define-syntax for-vector
    (syntax-rules
	(with)
      [(for-vector v with x body ...)
       (for x 0 (- (vector-length v) 1)
	    body ...)]))

  (define-syntax for-list
    (syntax-rules
     (with)
     [(for-list ls with x body ...)
      (let ((x 'dummy))
        (letrec ((loop
                  (lambda (param)
                    (if (empty? param)
                        'done
                        (begin
                          (set! x (car param))
                          body ...
                          (loop (rest param)))))))
          (loop ls)))]))

  (define (quicksort-vector! v less-than)
    (let ([count (vector-length v)])
      (let loop ([min 0][max count])
	(if (< min (sub1 max))
	    (let ([pval (vector-ref v min)])
	      (let pivot-loop ([pivot min]
			       [pos (add1 min)])
		(if (< pos max)
		    (let ([cval (vector-ref v pos)])
		      (if (less-than cval pval)
			  (begin
			    (vector-set! v pos (vector-ref v pivot))
			    (vector-set! v pivot cval)
			    (pivot-loop (add1 pivot) (add1 pos)))
			  (pivot-loop pivot (add1 pos))))
		    (if (= min pivot)
			(loop (add1 pivot) max)
			(begin
			  (loop min pivot)
			  (loop pivot max)))))))))
    v)
  
  
  
  
  
  (define (member-str? s ls)
    (cond
     ((empty? ls) false)
     ((string=? s (first ls)) true)
     (else (member-str? s (rest ls)))))
  
  (define (last ls)
    (cond
     ((empty? ls) (error "took a last but it was emptry"))
     ((empty? (rest ls)) (first ls))
     (else (last (rest ls)))))
  
  (define (string->char s)
    (first (string->list s)))
  
  (define (member-eq? x ls)
    (not (empty? (filter (lambda (y) (eq? x y)) ls))))
  
  (define (to-string arg . fns)
    (let loop ((arg arg))
      (cond
       ((not arg) "#f")
       ((void? arg) "#<void>")
       ((eq? arg #t) "#t")
       ((char? arg) (list->string (list arg)))
       ((string? arg) arg)
       ((symbol? arg) (symbol->string arg))
       ((number? arg) (number->string arg))
       ((vector? arg) (loop (vector->list arg)))
       ((empty? arg) "empty")
       ((list? arg) (string-append
		     "("
		     (loop (first arg))
		     (foldr string-append "" 
			    (map (lambda (x)
				   (string-append " "
						  (loop x))) (rest arg)))
		     ")"))
       ((cons? arg) (string-append
		     "("
		     (loop (first arg))
		     " . "
		     (loop (rest arg))
		     ")"))
       
       (true (let loop ((cur fns))
	       (if (empty? cur) (halt "to-string: " arg)
		   (or ((first cur) arg) 
		       (loop (rest cur)))))))))
    

  (define (debug . args)
    (for-each display args)
    (newline))

  (define (make-debug . fns)
    (lambda args (for-each (lambda (x) (display (apply to-string (cons x fns)))
				   (display " ")) args)
	    (newline)))

  (define (assoc-get label ls)
    (cond
     ((empty? ls) (error (string-append "failed to find " (to-string label))))
     ((eq? label (first (first ls)))
      (first ls))
     (else (assoc-get label (rest ls)))))
  
  (define (cons-to-end a ls)
    (cond
     ((empty? ls) (cons a ls))
     (else (cons (first ls)
		 (cons-to-end a (rest ls))))))
  
  (define (struct->list itm)
    (cond [(struct? itm) (map struct->list (vector->list (struct->vector itm)))]
	  [(list? itm)   (map struct->list itm)]
	  [else itm]))

  (define (struct-name s) (vector-ref (struct->vector s) 0))

  
  (define make-hash make-hash-table)
  (define hash-get hash-table-get)
  (define hash-put! hash-table-put!)
  (define hash-remove! hash-table-remove!)
  (define hash-map hash-table-map)
  (define hash-for-each hash-table-for-each)
  (define (hash-mem? hash item) (hash-get hash item (lambda () false)))
)