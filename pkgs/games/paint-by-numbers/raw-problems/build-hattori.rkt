#lang racket/gui

;;; these come from:
;;; http://www.ask.ne.jp/~hattori/puzzle/menu.html
;;; We must cite him in the game somewhere...

(define pixel-size 10)

(define (main-n n)
  (let ([grid (calculate-grid (build-path 'up "hattori" (format "~a.gif" n)))])
    (display-grid grid)
    (pretty-print (build-problem n grid))
    (newline)
    (eprintf "\n\n")))

(define (calculate-grid filename)
  (eprintf "reading ~a\n" filename)
  (let* ([bitmap (make-object bitmap% filename)]
	 [_ (unless (send bitmap ok?)
	      (error 'bad-bitmap "name: ~a" filename))]
	 [bitmap-dc (make-object bitmap-dc% bitmap)]
	 [raw-width (send bitmap get-width)]
	 [raw-height (send bitmap get-height)]
	 [image-width raw-width]
	 [image-height (get-puzzle-height bitmap-dc raw-height)]
	 [grid-x-start (get-grid-start bitmap-dc #t)]
	 [grid-y-start (get-grid-start bitmap-dc #f)]
	 [puzzle-width (- image-width grid-x-start)]
	 [puzzle-height (- image-height grid-y-start)]
	 [new-bitmap-width (floor (/ (- puzzle-width 1) pixel-size))]
	 [new-bitmap-height (floor (/ (- puzzle-height 1) pixel-size))])

    (begin
      (eprintf "size of picture: ~a x ~a\n" raw-width raw-height)
      (eprintf "  size of image: ~a x ~a\n" image-width image-height)
      (eprintf "grid-start (~a, ~a)\n" grid-x-start grid-y-start)
      (eprintf "size of puzzle: ~a x ~a\n" puzzle-width puzzle-height))
    (reverse
     (let loop ([j new-bitmap-height])
       (cond
	[(zero? j) null]
	[else
	 (cons
	  (reverse
	   (let loop ([i new-bitmap-width])
	     (cond
	      [(zero? i) null]
	      [else
	       (let ([pixel-value
		      (if (black-pixel?
			   bitmap-dc
			   (floor
			    (+ grid-x-start
			       (* pixel-size (+ i -1 1/2))))
			   (floor
			    (+ grid-y-start
			       (* pixel-size (+ j -1 1/2)))))
			  'x
			  'o)])
		 ;(eprintf "(~a, ~a) is ~a\n" i j pixel-value)
		 (cons pixel-value
		       (loop (- i 1))))])))
	  (loop (- j 1)))])))))

(define (display-grid grid)
  (display "+" (current-error-port))
  (for-each (lambda (x) (display "-" (current-error-port))) (car grid))
  (display "+" (current-error-port))
  (newline (current-error-port))

  (for-each
   (lambda (row)
     (display "|" (current-error-port))
     (for-each
      (lambda (i)
	(if (eq? i 'x)
	    (display "#" (current-error-port))
	    (display " " (current-error-port))))
      row)
     (display "|" (current-error-port))
     (newline (current-error-port)))
   grid)
  (display "+" (current-error-port))
  (for-each (lambda (x) (display "-" (current-error-port))) (car grid))
  (display "+" (current-error-port))
  (newline (current-error-port)))

(define tmp-color (make-object color%))

(define (colored-pixel? value)
  (lambda (bitmap-dc x y)
    (send bitmap-dc get-pixel x y tmp-color)
    (= value
       (send tmp-color red)
       (send tmp-color green)
       (send tmp-color blue))))

(define white-pixel? (colored-pixel? 255))
(define black-pixel? (colored-pixel? 0))

(define (get-puzzle-height bitmap-dc raw-height)
  (let loop ([i raw-height])
    (cond
      [(white-pixel? bitmap-dc 0 (- i 1))
       (loop (- i 1))]
      [else i])))

(define (get-grid-start bitmap-dc x?)
  (let loop ([i 0])
    (cond
      [(if x?
	   (white-pixel? bitmap-dc i 0)
	   (white-pixel? bitmap-dc 0 i))
       (loop (+ i 1))]
      [else i])))


(define (on-off->blocks l)
  (let loop ([l l]
	     [in? #f]
	     [size 0])
    (cond
     [(null? l) (if (= size 0)
		    null
		    (list size))]
     [else
      (let ([on? (eq? (car l) 'x)])
	(cond
	 [(and in? on?) 
	  (loop (cdr l)
		on?
		(+ size 1))]
	 [(and in? (not on?))
	  (cons size
		(loop (cdr l)
		      #f
		      0))]
	 [(and (not in?) on?)
	  (loop (cdr l)
		#t
		1)]
	 [(and (not in?) (not on?))
	  (loop (cdr l)
		#f
		0)]))])))

(define (transpose l) (apply map list l))

(define (build-problem n on-off-lists)
  (list (format "Hattori ~a" n)
	(map on-off->blocks on-off-lists)
	(map on-off->blocks (transpose on-off-lists))
	on-off-lists))

(provide main)
(define (main)
  (call-with-output-file "raw-hattori.rkt"
    (lambda (port)
      (parameterize ([current-output-port port])
        (printf "`(\n")
        (let loop ([n 1])
          (when (<= n 139)
            (main-n n)
            (loop (+ n 1))))
        (printf ")")))
    #:exists 'truncate))
