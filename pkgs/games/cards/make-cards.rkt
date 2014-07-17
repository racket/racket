(module make-cards racket
  (require racket/class
           (prefix-in mred: racket/gui)
           (prefix-in card-class: "card-class.rkt"))

  (provide back deck-of-cards
           (contract-out
            [make-card (->i ([front-bm (back-bm)
                                       (and/c (is-a?/c mred:bitmap%)
                                              (same-size back-bm))]
                             [back-bm (or/c #f (is-a?/c mred:bitmap%))]
                             [suit-id any/c]
                             [value any/c])
                            [result (is-a?/c card-class:card%)])]))
  
  (define (same-size given-bitmap)
    (cond
      [given-bitmap
       (define w (send given-bitmap get-width))
       (define h (send given-bitmap get-height))
       (define (check bmp)
         (and (= w (send bmp get-width))
              (= h (send bmp get-height))))
       (procedure-rename check
                         (string->symbol (format "~ax~a-bitmap?" w h)))]
      [else any/c]))

  (define (get-bitmap file)
    (mred:read-bitmap file 
                      #:try-@2x? ((mred:get-display-backing-scale) . > . 1)))

  (define (make-dim bm-in)
    (let ([w (send bm-in get-width)]
	  [h (send bm-in get-height)]
          [s (inexact->exact (round (send bm-in get-backing-scale)))])
      (let* ([bm (mred:make-bitmap w h #:backing-scale s)]
	     [mdc (make-object mred:bitmap-dc% bm)])
	(send mdc draw-bitmap bm-in 0 0)
	(let* ([len (* w h 4 s s)]
	       [b (make-bytes len)])
	  (send bm get-argb-pixels 0 0 (* w s) (* h s) b #:unscaled? #t)
	  (let loop ([i 0])
	    (unless (= i len)
              (when (positive? (modulo i 4))
                (bytes-set! b i (quotient (* 3 (bytes-ref b i)) 4)))
	      (loop (add1 i))))
	  (send bm set-argb-pixels 0 0 (* w s) (* h s) b #:unscaled? #t))
	(send mdc set-bitmap #f)
	bm)))

  (define here 
    (let ([cp (collection-path "games" "cards")])
      (lambda (file)
	(build-path cp 
		    (if ((mred:get-display-depth)  . <= . 8)
			"locolor"
			"hicolor")
		    file))))

  (define back (get-bitmap (here "card-back.png")))

  (define dim-back
    (make-dim back))

  (define deck-of-cards
    (let* ([w (send back get-width)]
	   [h (send back get-height)])
      (let sloop ([suit 4])
	(if (zero? suit)
	    null
	    (let vloop ([value 13])
	      (sleep)
	      (if (zero? value)
		  (sloop (sub1 suit))
		  (let ([front (get-bitmap
				(here 
				 (format "card-~a-~a.png"
					 (sub1 value)
					 (sub1 suit))))])
		    (cons (make-object card-class:card%
			    suit
			    value
			    w h
			    front back
			    (lambda () (make-dim front))
			    (lambda () dim-back)
                            (make-hash))
			  (vloop (sub1 value))))))))))
  
  (define (make-card front-bm back-bm suit-id value)
    (let ([w (send back get-width)]
	  [h (send back get-height)])
      (make-object card-class:card%
		   suit-id
		   value
		   w h
		   front-bm (or back-bm back)
		   (lambda () (make-dim front-bm))
		   (lambda ()
		     (if back-bm 
			 (make-dim back)
			 dim-back))
                   (make-hash)))))
