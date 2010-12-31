#lang mzscheme
(require "test-suite-utils.ss")

(test
 'dragable-min-size1
 (λ (min-w/min-h) (equal? min-w/min-h '(10 20)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f)) 0 #t))
                    list))

(test
 'dragable-min-size2
 (λ (min-w/min-h) (equal? min-w/min-h '(10 20)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f)) 0 #f))
                    list))

(test
 'dragable-min-size3
 (λ (min-w/min-h) (equal? min-w/min-h '(30 60)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 0 #t))
                    list))

(test
 'dragable-min-size4
 (λ (min-w/min-h) (equal? min-w/min-h '(40 40)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 0 #f))
                    list))

(test
 'dragable-min-size5
 (λ (min-w/min-h) (equal? min-w/min-h '(30 65)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 5 #t))
                    list))

(test
 'dragable-min-size6
 (λ (min-w/min-h) (equal? min-w/min-h '(45 40)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 5 #f))
                    list))

(test
 'dragable-place-children1
 (λ (l) (equal? l '(() ())))
 `(call-with-values (λ () (panel:dragable-place-children '() 100 200 '() 0 #t))
                    list))

(test
 'dragable-place-children2
 (λ (l) (equal? l '(((0 0 100 200)) ())))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f)) 100 200 '(1) 0 #t))
                    list))

(test
 'dragable-place-children3
 (λ (l) (equal? l '(((0 0 100 200)) ())))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f)) 100 200 '(1) 0 #f))
                    list))

(test
 'dragable-place-children4
 (λ (l) (equal? l '(((0 0 100 150) (0 150 100 150)) ((150 150)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #t))
                    list))

(test
 'dragable-place-children5
 (λ (l) (equal? l '(((0 0 50 300) (50 0 50 300)) ((50 50)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children5
 (λ (l) (equal? l '(((0 0 100 100) (0 100 100 200)) ((100 100)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/3 2/3) 0 #t))
                    list))

(test
 'dragable-place-children6
 (λ (l) (equal? l '(((0 0 10 300) (10 0 90 300)) ((10 10)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/10 9/10) 0 #f))
                    list))

(test
 'dragable-place-children7
 (λ (l) (equal? l '(((0 0 10 300) (20 0 90 300)) ((10 20)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 110 300 '(1/10 9/10) 10 #f))
                    list))

(test
 'dragable-place-children8
 (λ (l) (equal? l '(((0 0 10 300) (20 0 20 300) (50 0 70 300)) ((10 20) (40 50)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f) (10 10 #t #f)) 120 300 '(1/10 2/10 7/10) 10 #f))
                    list))

(test
 'dragable-place-children9
 (λ (l) (equal? l '(((0 0 30 300) (30 0 70 300)) ((30 30)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (70 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children10
 (λ (l) (equal? l '(((0 0 70 300) (70 0 30 300)) ((70 70)))))
 `(call-with-values (λ () (panel:dragable-place-children '((70 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children11
 (λ (l) (equal? l '(((0 0 70 300) (70 0 10 300) (80 0 20 300)) ((70 70) (80 80)))))
 `(call-with-values (λ () (panel:dragable-place-children '((70 10 #t #f) (10 10 #t #f) (20 10 #t #f)) 100 300 '(1/2 1/4 1/4) 0 #f))
                    list))

(test
 'dragable-place-children12
 (λ (l) (equal? l '(((0 0 242 629) (247 0 243 629)) ((242 247)))))
 `(call-with-values (λ () (panel:dragable-place-children '((30 30 #t #t) (30 30 #t #t)) 490 629 '(1/2 1/2) 5 #f))
                    list))

;(dragable-place-children infos width height percentages gap-width vertical?)

;; with stuff that doesn't fit....

#;
(test 
 'single-panel
 (lambda (x) (eq? x 'passed))
 `(let* ([semaphore (make-semaphore 0)]
	 [semaphore-frame%
	  (class frame% 
	    (define/augment (on-close) (semaphore-post semaphore))
	    (super-new))]
	 [f (make-object semaphore-frame% "Single Panel Test")]
	 [blue-brush (send the-brush-list find-or-create-brush "BLUE" 'solid)]
	 [green-brush (send the-brush-list find-or-create-brush "FOREST GREEN" 'solid)]
	 [black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid)]
	 [grid-canvas%
	  (class canvas% 
	    (init-field lines)
	    (init label)
	    (inherit get-dc get-client-size)
	    (override on-paint)
	    (define (on-paint)
	      (let-values ([(width height) (get-client-size)])
		(let ([dc (get-dc)]
		      [single-width (/ width lines)]
		      [single-height (/ height lines)])
		  (send dc set-pen black-pen)
		  (let loop ([i lines])
		    (cond
		      [(zero? i) (void)]
		      [else
		       (let loop ([j lines])
			 (cond
			   [(zero? j) (void)]
			   [else 
			    (send dc set-brush
				  (if (= 0 (modulo (+ i j) 2))
				      blue-brush green-brush))
			    (send dc draw-rectangle
				  (* single-width (- i 1))
				  (* single-height (- j 1))
				  single-width
				  single-height)
			    (loop (- j 1))]))
		       (loop (- i 1))])))))
	    (super-instantiate ())

	    ;; soon to be obsolete, hopefully.
	    (inherit set-label)
	    (set-label label)

	    (inherit min-width min-height)
	    (min-width 50)
	    (min-height 50))]
	 [border-panel (make-object horizontal-panel% f '(border))]
	 [single-panel (make-object panel:single% border-panel)]
	 [children
	  (list
	   (instantiate grid-canvas% () (lines 3) (parent single-panel) (label "Small") (stretchable-width #f) (stretchable-height #f))
	   (instantiate grid-canvas% () (lines 3) (parent single-panel) (label "Wide") (stretchable-width #f) (stretchable-height #t))
	   (instantiate grid-canvas% () (lines 3) (parent single-panel) (label "Tall") (stretchable-width #t) (stretchable-height #f))
	   (instantiate grid-canvas% () (lines 3) (parent single-panel) (label "Wide and Tall") (stretchable-width #t) (stretchable-height #t)))]
	 [active-child (car children)]
	 [radios (make-object horizontal-panel% f)]
	 [make-radio
	  (lambda (label choices callback)
	    (let* ([panel (make-object vertical-panel% radios '(border))]
		   [message (make-object message% label panel)]
		   [radio (make-object radio-box% #f choices panel (lambda (radio _) (callback radio)))]
		   [button (make-object button%
			     "Cycle" panel
			     (lambda (_1 _2)
			       (let ([before (send radio get-selection)]
				     [tot (send radio get-number)])
				 (let loop ([n tot])
				   (unless (zero? n)
				     (send radio set-selection (- tot n))
				     (callback radio)
				     (sleep/yield 1)
				     (loop (- n 1))))
				 (send radio set-selection before)
				 (callback radio))))])
	      radio))]
	 [radio
	  (make-radio
	   "Active Child"
	   (map (lambda (x) (send x get-label)) children)
	   (lambda (radio)
	     (let loop ([n (length children)]
			[cs children])
	       (cond
		 [(null? cs) (void)]
		 [else (let ([c (car cs)])
			 (if (string=? (send radio get-item-label (send radio get-selection))
				       (send c get-label))
			     (begin (set! active-child c)
				    (send single-panel active-child active-child))
			     (loop (- n 1)
				   (cdr cs))))]))))]
	 [vertical-alignment 'center]
	 [horizontal-alignment 'center]
	 [update-alignment (lambda () 
			     (send single-panel set-alignment horizontal-alignment vertical-alignment))]
	 [horiz
	  (make-radio 
	   "Horizontal Alignment"
	   (list "left" "center" "right")
	   (lambda (radio)
	     (set! horizontal-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
	     (update-alignment)))]
	 [vert
	  (make-radio
	   "Vertical Alignment"
	   (list "top" "center" "bottom")
	   (lambda (radio)
	     (set! vertical-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
	     (update-alignment)))]
	 [buttons (make-object horizontal-panel% f)]
	 [result 'failed]
	 [failed (make-object button% "Failed" buttons (lambda (_1 _2) (semaphore-post semaphore)))]
	 [passed (make-object button% "Passed" buttons (lambda (_1 _2) 
							 (set! result 'passed)
							 (semaphore-post semaphore)))])
    (send border-panel min-width 100)
    (send border-panel min-height 100)
    (send vert set-selection 1)
    (send horiz set-selection 1)
    (send buttons stretchable-height #f)
    (send buttons set-alignment 'right 'center)
    (send radios stretchable-height #f)
    (send f show #t)
    (yield semaphore)
    (send f show #f)
    result))
