(module lists mzscheme
  (define SIZE 10000)

  (define (sequence start stop)
    (if (> start stop)
	'()
	(cons start (sequence (+ start 1) stop))))

  (define (head-to-tail! headlist taillist)
    (when (null? taillist) (begin
			     (set! taillist (list (car headlist)))
			     (set! headlist (cdr headlist))))
    (letrec ((htt-helper (lambda (dest)
			   (when (not (null? headlist))
			     (let ((headlink headlist))
			       (set-cdr! dest headlink)
			       (set! headlist (cdr headlist))
			       (htt-helper headlink))))))
      (htt-helper taillist)
      (values headlist taillist)))

  (define (test-lists)
    (let* ([L1 (sequence 1 SIZE)]
	   [L2 (append L1 '())]
	   [L3 '()])
      (set!-values (L2 L3) (head-to-tail! L2 L3))
      (set!-values (L3 L2) (head-to-tail! (reverse! L3) L2))
      (set! L1 (reverse! L1))
      (cond ((not (= SIZE (car L1))) 0)
	    ((not (equal? L1 L2))	   0)
	    (else           (length L1)))))

  (define (main args)
    (let ((result #f))
      (let loop ((counter (if (= (vector-length args) 0)
			      1
			      (string->number (vector-ref args 0)))))
	(when (> counter 0)
	  (set! result (test-lists))
	  (loop (- counter 1))))
      (printf "~s~n" result)))

  (main (current-command-line-arguments)))