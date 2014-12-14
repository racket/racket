
(load-relative "loadtest.rktl")

(Section 'readtable)

(require (only-in racket/port 
                  [relocate-input-port relocate-input-port]))
(define (shift-rt-port p deltas)
  (let ([p (relocate-input-port p 
				(add1 (car deltas))
				(cadr deltas)
				(add1 (caddr deltas)))])
    (port-count-lines! p)
    p))
	
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic readtable tests

(arity-test make-readtable 1 -1)
(arity-test readtable? 1 1)
(arity-test readtable-mapping 2 2)

(err/rt-test (make-readtable 5))
(err/rt-test (make-readtable #f 5))
(err/rt-test (make-readtable #f #\a))
(err/rt-test (make-readtable #f #\a 5))
(err/rt-test (make-readtable #f #\a #\b))
(err/rt-test (make-readtable #f #\a 'terkminating-macro))
(err/rt-test (make-readtable #f #\a 'terkminating-macro))

(test #f current-readtable)
(test #t readtable? (make-readtable #f))
(test #t readtable? (make-readtable (make-readtable #f)))

(let ([plain-dollar
       (case-lambda
	[(ch port)
	 (test #t ormap (lambda (x) (char=? ch x)) '(#\$ #\&))
	 `dollar]
	[(ch port src line col pos)
	 (test #t ormap (lambda (x) (char=? ch x)) '(#\$ #\&))
	 `dollar])]
      [plain-percent
       (case-lambda
	[(ch port)
	 (test #\% values ch)
	 `(percent ,(read/recursive port))]
	[(ch port src line col pos)
	 (test #\% values ch)
	 (test 'string values src)
	 `(percent ,(read-syntax/recursive src port))])]
      [hash-dollar
       (case-lambda
	[(ch port)
	 (test #\$ values ch)
	 `(dollar . ,(read/recursive port))]
	[(ch port src line col pos)
	 (test #\$ values ch)
	 `(dollar . ,(read-syntax/recursive src port))])]
      [comment3
       (case-lambda
	[(ch port src line col pos)
	 (test #\_ values ch)
	 (read-char port) (read-char port) (read-char port)
	 (make-special-comment #f)])]
      [comment3.2
       (case-lambda
	[(ch port src line col pos)
	 (test #\? values ch)
	 (read-char port) (read-char port) (read-char port)
	 (make-special-comment #f)])])
  (let ([t (make-readtable #f 
			   #\$ 'terminating-macro plain-dollar
			   #\% 'non-terminating-macro plain-percent
			   #\^ #\| #f
			   #\< #\( #f
			   #\= #\\ #f
			   #\~ #\space #f
			   #\_ 'terminating-macro comment3
			   #\$ 'dispatch-macro  hash-dollar
                           #\? 'dispatch-macro comment3.2)])
    (test-values '(#\a #f #f) (lambda () (readtable-mapping t #\a)))
    (test-values '(#\| #f #f) (lambda () (readtable-mapping t #\^)))
    (test-values '(#\( #f #f) (lambda () (readtable-mapping t #\<)))
    (test-values '(#\\ #f #f) (lambda () (readtable-mapping t #\=)))
    (test-values '(#\space #f #f) (lambda () (readtable-mapping t #\~)))
    (test-values (list 'terminating-macro plain-dollar hash-dollar) (lambda () (readtable-mapping t #\$)))
    (test-values (list 'terminating-macro comment3 #f) (lambda () (readtable-mapping t #\_)))
    (test-values (list 'non-terminating-macro plain-percent #f) (lambda () (readtable-mapping t #\%)))
    (let ([t2 (make-readtable t
			      #\& #\$ t
			      #\a #\a t
			      #\^ #\^ #f)])
      (test-values '(#\a #f #f) (lambda () (readtable-mapping t2 #\a)))
      (test-values '(#\^ #f #f) (lambda () (readtable-mapping t2 #\^)))
      (test-values '(#\space #f #f) (lambda () (readtable-mapping t #\~)))
      (test-values (list 'terminating-macro plain-dollar #f) (lambda () (readtable-mapping t2 #\&)))

      (letrec ([test-read
		(case-lambda 
		 [(s l check-pos? try-syntax?)
		  (define (go read)
		    (let* ([o (open-input-string s)])
		      (port-count-lines! o)
		      (let loop ()
			(let ([v (read o)])
			  (if (eof-object? v)
			      null
			      (cons v (loop)))))))
		  (test l (lambda (a b) (go read)) 'read s)
                  (when try-syntax?
                    (test l (lambda (a b) (map syntax->datum 
                                               (go (lambda (p) (read-syntax 'string p)))))
                          'read-syntax s)
                    (when check-pos?
                      (let ([stx (car (go (lambda (p) (read-syntax 'string (shift-rt-port p (list 1 2 3))))))])
                        (test 2 syntax-line stx)
                        (test 2 syntax-column stx)
                        (test 4 syntax-position stx))))]
		 [(s l) (test-read s l #t #t)]
		 [(s l check-pos?) (test-read s l check-pos? #t)])])
	
	(test-read "a$%_^b" '(a$%_^b))

	(let ([try-table
	       (lambda (t old-caret?)
		 (parameterize ([current-readtable t])
		   (test-read "a$b" '(a dollar b))
		   (when old-caret?
		     (test-read "a&b" '(a dollar b)))
		   (test-read "a #$ b" '(a (dollar . b)))
		   (test-read "(#1=a #$ #1#)" '((a (dollar . a))) #t #f)
		   (test-read "(#1=a #$ (#1#))" '((a (dollar a))) #t #f)
		   (test-read "a%b" '(a%b))
		   (test-read "a % b" '(a (percent b)))
		   (test-read "(#1=a % #1#)" '((a (percent a))) #t #f)
		   (test-read "(#1=a % (#1#))" '((a (percent (a)))) #t #f)
		   (test-read "a _xxx b" '(a b))
		   (test-read "(a _xxx b)" '((a b)))
		   (test-read "(a _xxx . b)" '((a . b)))
		   (test-read "(a #?xxx . b)" '((a . b)))
		   (test-read "(a . _xxx b)" '((a . b)))
		   (test-read "(a . #?xxx b)" '((a . b)))
		   (if old-caret?
		       (test-read "(a ^_xxx^ b)" '((a ^ ^ b)))
		       (test-read "(a ^_xxx^ b)" '((a _xxx b))))
		   (test-read "(a =_xxx b)" '((a _xxx b)))
		   (test-read "<a xxx b)" '((a xxx b)))
		   (test-read "<a~xxx~b)" '((a xxx b)))))])
	  (try-table t #f)
	  (try-table t2 #t))

	(let ([try-as-plain (lambda (ch)
			      (parameterize ([current-readtable (make-readtable #f
										ch #\a #f)])
				(let ([s1 (format "a~ab" ch)]
				      [s2 (format "~aab~a" ch ch)])
				  (test-read s1 (list (string->symbol s1)))
				  (test-read s2 (list (string->symbol s2)) #f)
				  (let ([blank (if (char=? ch #\space)
						   #\newline
						   #\space)])
				    (test-read (format "a~a~a~ab" blank ch blank) 
					       (list 'a (string->symbol (string ch)) 'b))))))])
	  (for-each try-as-plain (string->list "()[]{}|\\ \r\n\t\v',\"#")))

	;; Check /recursive functions with pre char and initial readtable
	(for-each
	 (lambda (base-readtable swap?)
	   (for-each
	    (lambda (read/recursive)
	      (let ([t (make-readtable #f 
				       #\~ 'terminating-macro (lambda (ch port src line col pos)
								(define read/rec
								  (if src
								      (lambda (port char readtable)
									(read-syntax/recursive 
									 src port
									 char readtable))
								      read/recursive))
								(if (eq? (char=? #\! (peek-char port)) (not swap?))
								    (read/rec port #\( base-readtable)
								    (read/rec port #\{ base-readtable))))])
		(parameterize ([current-readtable t])
		  (test-read "~!a (b))" `((!a (b))))
		  (test-read "~?a (b)}" `((?a (b)))))))
	    (list read/recursive (lambda (port char readtable)
				   (read-syntax/recursive 'ok port char readtable)))))
	 (list #f (make-readtable #f
				  #\! 'terminating-macro (lambda (ch port src line col pos) (error 'ack))
				  #\? 'terminating-macro (lambda (ch port src line col pos) (error 'ack))
				  #\( #\{ #f
				  #\{ #\( #f))
	 (list #f #t))
	
	(void)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure we can't expose fixup process for graphs and cycles

(let ([result #f]
      [mk (lambda (v)
	    `(,v ,(vector v) ,(box v) ,(let ([ht (make-hasheq)])
					 (hash-set! ht v v)
					 ht)))])
  (let ([get-zero
	 (case-lambda
	  [(ch port)
	   (let ([v (read/recursive (open-input-string "#0#"))])
	     (set! result (mk v))
	     result)]
	  [(ch port src line col pos)
	   (let ([v (read-syntax/recursive src (open-input-string "#0#"))])
	     (set! result (datum->syntax-object #f (mk v) #f))
	     result)])])
    (let ([t (make-readtable #f 
			     #\$ 'terminating-macro get-zero)])
      (let ([go
	     (lambda (read car cadr caddr cadddr unbox vector-ref hash-map
			   list? vector? box? hash?)
	       (let ([v (parameterize ([current-readtable t])
			  (read (open-input-string "#0=$")))])
		 (test #t list? v)
		 (test #t list? (car v))
		 (test #t vector? (cadr v))
		 (test #t box? (caddr v))
		 (test #t hash? (cadddr v))
		 (test #t eq? v (car v))
		 (test #t eq? v (vector-ref (cadr v) 0))
		 (test #t eq? v (unbox (caddr v)))
		 (test #t pair? (memq v (hash-map (cadddr v) (lambda (k v) k))))
		 (test #t pair? (memq v (hash-map (cadddr v) (lambda (k v) v))))
		 (test #f eq? v result)))])
	(go read car cadr caddr cadddr unbox vector-ref hash-map
	    list? vector? box? hash?)
        #;
	(go (lambda (p) (read-syntax 'string p))
	    (lambda (stx) (car (syntax->list stx))) 
	    (lambda (stx) (cadr (syntax->list stx))) 
	    (lambda (stx) (caddr (syntax->list stx))) 
	    (lambda (stx) (cadddr (syntax->list stx))) 
	    (lambda (stx) (unbox (syntax-e stx)))
	    (lambda (stx p) (vector-ref  (syntax-e stx) p))
	    (lambda (stx f) (hash-map (syntax-e stx) f))
	    (lambda (stx) (and (syntax->list stx) #t))
	    (lambda (stx) (vector? (syntax-e stx)))
	    (lambda (stx) (box? (syntax-e stx)))
	    (lambda (stx) (hash? (syntax-e stx))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that sharing is preserved
	    
(let ([get-graph
       (case-lambda
	[(ch port) (read (open-input-string "#0=(#0#)"))]
	[(ch port src line col pos)
	 (datum->syntax-object #f (read (open-input-string "#1=(#1#)")) #f)])])
  (let ([t (make-readtable #f 
			   #\$ 'terminating-macro get-graph)])
    (let ([go
	   (lambda (read car)
	     (let ([v (parameterize ([current-readtable t])
			(read (open-input-string "$")))])
	       ;; Check that cycle is preserved by unrolling lots
	       (test #f boolean? (car (car (car (car v)))))))])
      (go read car)
      (err/rt-test
       (go (lambda (p) (read-syntax 'string p))
           (lambda (stx) (car (syntax->list stx))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace the symbol reader

(let ([tcs
       ;; As a default reader, makes all symbols three characters long,
       ;;  except that ! is a comment:
       (case-lambda
	[(ch port)
	 (if (char=? ch #\!)
	     (make-special-comment #f)
	     (string->symbol (string ch (read-char port) (read-char port))))]
	[(ch port src line col pos)
	 (if (char=? ch #\!)
	     (make-special-comment #f)
	     (string->symbol (string ch (read-char port) (read-char port))))])])

  (let ([t (make-readtable #f 
			   #f 'non-terminating-macro tcs)])
    (parameterize ([current-readtable t])
      (test 'abc read (open-input-string "abcd"))
      (test 'abc read (open-input-string " abcd"))
      (test 'abc read (open-input-string " !!!abcd"))
      (test '|\u1| read (open-input-string " !!!\\u1bcd")))

    ;; Now change a to return 'a:
    (let ([t2 (make-readtable t
			      #\a 'terminating-macro (lambda (ch port src line col pos)
						       (string->symbol (string ch))))])
      (parameterize ([current-readtable t2])
	(test 'a read (open-input-string "abcd"))
	(test 'bac read (open-input-string "bacd"))
	(test 'a read (open-input-string "!acd")))

      ;; Map z to a, and # to b
      (let ([t3 (make-readtable t2
				#\z #\a t2
				#\# #\b t2)])
	(parameterize ([current-readtable t3])
	  (test 'a read (open-input-string "abcd"))
	  (test 'bac read (open-input-string "bacd"))
	  (test '|#ac| read (open-input-string "#acd"))
	  (test 'z read (open-input-string "z#acd")))))))
	
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map other chars to parens

(let ([try (lambda (open close other-open other-close)
             (let ([readstr (lambda (s)
                              (read (open-input-string s)))])
               (parameterize ([current-readtable
                               (make-readtable #f
                                               #\< open #f
                                               #\> close #f)])
                 (test '((2) 1 3) readstr (format "<1 . ~a2> . 3~a" open close)))
               (parameterize ([current-readtable
                               (make-readtable #f
                                               open #\a #f
                                               close #\a #f
                                               #\< open #f
                                               #\! #\. #f
                                               #\> close #f)])
                 (test '(1 . 2) readstr "<1 . 2>")
                 (test '(1 . 2) readstr "<1 ! 2>")
                 (test (string->symbol (format "~a1" open))
                       readstr (format "~a1 ! 2~a" open close))
                 (test '(2 1 3) readstr "<1 ! 2 ! 3>")
                 (test '((2) 1 3) readstr "<1 ! <2> ! 3>")
                 (test '((2) 1 3) readstr (format "<1 ! ~a2~a ! 3>"
                                                  other-open other-close))
                 (err/rt-test (readstr "#<1 2 3>") exn:fail:read?)
                 (err/rt-test (readstr (format "<1 2 3~a" other-close)) exn:fail:read?)
                 (test '#(1 2 3 3) readstr "#4<1 2 3>"))))])
  (try #\( #\) #\[ #\])
  (try #\[ #\] #\( #\))
  (try #\{ #\} #\[ #\]))
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
