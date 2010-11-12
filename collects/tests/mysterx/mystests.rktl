;;; mystests.ss -- test suite for MysterX

(require mzlib/class)
(require mysterx)

(define errors? #f)

(define wb 
  (instantiate mx-browser% ()
	       (label "MysTest")
	       (width 230)
	       (height 250)))

(define doc (send wb current-document))

(define ctrl (send doc insert-object-from-coclass "TestControl Class" 95 95 'percent))

(define (inv f . args) (apply com-invoke ctrl f args))

(define (test-currency n)
  (= n (com-currency->number (number->com-currency n))))

(define (test-scode n)
  (= n (com-scode->number (number->com-scode n))))

(define (test-date date)
  (equal? date (com-date->date (date->com-date date))))

(for-each
 (lambda (n)
   (unless (test-scode n)
	   (printf "Error in test-scode for value ~a\n" n)
	   (set! errors? #t)))
 '(25 -22 -1 -233344433 177000000 859489222))


(define (test-numprop ndx retval)
	(com-set-property! ctrl (list "Numprop" ndx) 55)
	(unless (= (com-get-property ctrl (list "Numprop" ndx)) retval)
		(printf "Error in setting Numprop")	
		(set! errors? #t)))

(test-numprop 26 42)
(test-numprop 10 99)

(print-struct #t)

(let ([date (seconds->date (current-seconds))])
  (set-date-dst?! date #f)
  (set-date-time-zone-offset! date 0)
  (unless (test-date date)
	  (printf "Error in test-date\n")
	  (set! errors? #t)))

(for-each
 (lambda (n)
   (unless (test-currency n)
	   (printf "Error in test-currency for value ~a\n" n)
	   (set! errors? #t)))
 '(0 1 3.14 25.00 -22.34 11.7832 91000000000 25034343434.9933))



(define com-tests
	`(("AddTest" (39 ,(box 420)) ,(+ 39 420))
	  ("AddTest" (420 ,(box 39)) ,(+ 420 39))
	  ("FloatTest" (4.7 5.2) ,(- 5.2 4.7))
	  ("FloatTest" (88.7 33.2) ,(- 33.2 88.7))
	  ("FloatTest" (-88.7 33.2) ,(- 33.2 -88.7))
	  ("UnsignedTest" (92 97) ,(- 97 92))
	  ("UnsignedTest" (20 33) ,(- 33 20))
	  ("UnsignedTest" (1 12) ,(- 12 1))
	  ("StringTest" ("abc" "def") ,"abcdef")
	  ("StringTest" ("Supercali" "fragilistic") ,"Supercalifragilistic")
          ("ShortTest"  (42 17) ,(* 42 17))
          ("ShortTest"  (77 -22) ,(* 77 -22))))

(for-each 
 (lambda (t)
   (let ([got (apply inv (car t) (cadr t))]
	 [expected (caddr t)])
     (unless (equal? got expected)
	     (set! errors? #t)
	     (printf "Error in com-tests. Expected: ~a\nGot     : ~a\n"
		     expected got))))
 com-tests)

(define caption "SomeCaption")

(com-set-property! ctrl "Caption" caption)

(unless (string=? caption (com-get-property ctrl "Caption"))
	(set! errors? #t))

(if errors?
      (printf "There were errors!\n")
      (printf "No errors in conversions and COM tests\n")) 	

(define (make-mousefun s)
  (let ([t (string-append s ": button = ~a shift = ~a x = ~a y = ~a\n")])
    (lambda (button shift x y) 
      (printf t button shift x y))))

(define (mouse-pair s)
  (list s (make-mousefun s)))


(unless errors?
	(for-each 
	 (lambda (sf) 
	   (com-register-event-handler ctrl (car sf) (cadr sf)))
	 `(("Click"
	    ,(lambda () (printf "Click\n")))
	   ,(mouse-pair "MouseMove")
	   ,(mouse-pair "MouseDown")
	   ,(mouse-pair "MouseUp")))

	(printf "Try clicking and moving the mouse over the object\n")
	(printf "You should see Click, MouseMove, MouseDown, and MouseUp events\n"))
