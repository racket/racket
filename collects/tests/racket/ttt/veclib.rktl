;; --------------------------------------------------------------------------
;; 2vec-library.ss

; #|
; (unit/sig
;   (make-2vec 2vec-ref 2vec-set! collect)
;   (import plt:userspace^)
; |#
  
  ;; 2 dimensional, square vectors

    (define collect
      (lambda (base combine)
	(define C 
	  (lambda (l)
	    (cond
	     ((null? l) base)
	     (else (combine l (car l) (C (cdr l)))))))
	C))
    
  (define (make-2vec N element)
    (make-vector (* N N) element))
  
  (define (2vec-ref 2vec i j)
    (let ((L (sqrt (vector-length 2vec))))
      (vector-ref 2vec (+ (* i L) j))))
  
  (define (2vec-set! 2vec i j element)
    (let ((L (sqrt (vector-length 2vec))))
      (if (and (< i L) (< j L))
	  (vector-set! 2vec (+ (* i L) j) element)
	  (error '2vec-set! "~s ~s" i j))))
  
  (define (I N)
    (let ((2vec (make-2vec N 0)))
      (let loop ((i 0) (j 0))
	(if (= i N)
	    (void)
	    (begin
	      (2vec-set! 2vec i j 1)
	      (loop (add1 i) (add1 j)))))
      2vec))
  
  (define (P N)
    (let ((2vec (make-2vec N 0)))
      (let loop ((i 0) (j 0))
	(cond
	  [(and (= i N) (= j 0)) (void)]
	  [(< j N) (2vec-set! 2vec i j (list i j)) (loop i (add1 j))]
	  [(< i N) (loop (add1 i) 0)]
	  [else (error 'P "impossible ~s ~s" i j)]))
      2vec))

; #|
;   )
; |#

