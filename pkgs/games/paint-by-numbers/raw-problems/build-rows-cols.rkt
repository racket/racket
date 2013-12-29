#lang racket
(require racket/gui
         racket/class)
(define argv (current-command-line-arguments))

(module test racket/base)

(when (equal? (vector) argv)
  (error 'build-rows-cols.rkt
         "expected an image file on the command-line"))

(define image (vector-ref argv 0))
(eprintf "processing ~a\n" image)

(define bitmap (make-object bitmap% image))
(when (send bitmap is-color?)
  (eprintf "expected a monochrome bitmap -- all non-black spaces will be considered white\n"))

(newline (current-error-port))

(define bitmap-dc (make-object bitmap-dc% bitmap))

(define on-off-lists
  (let ([color (make-object color%)])
    (let loop ([i (send bitmap get-height)]
	       [ans null])
      (cond
       [(zero? i) ans]
       [else
	(loop
	 (- i 1)
	 (cons
	  (let loop ([j (send bitmap get-width)]
		     [ans null])
	    (cond
	     [(zero? j) ans]
	     [else
	      (send bitmap-dc get-pixel (- j 1) (- i 1) color)
	      (loop (- j 1)
		    (cons
		     (if (and (= 0 (send color red))
			      (= 0 (send color blue))
			      (= 0 (send color green)))
			 'on
			 'off)
		     ans))]))
	  ans))]))))

(define (on-off->blocks l)
  (let loop ([l l]
	     [in? #f]
	     [size 0])
    (cond
     [(null? l) (if (= size 0)
		    null
		    (list size))]
     [else
      (let ([on? (eq? (car l) 'on)])
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

(for-each
 (lambda (l)
   (for-each
    (lambda (i) (display (if (eq? i 'on) "#" " ") (current-error-port)))
    l)
   (newline (current-error-port)))
 on-off-lists)
(newline (current-error-port))

(define rows (map on-off->blocks on-off-lists))
(define cols (map on-off->blocks (transpose on-off-lists)))

(write (list image rows cols))
(newline)
