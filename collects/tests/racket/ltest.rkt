(printf "nested loop~n")
(time
 (let loop ([n 10000])
  (unless (zero? n)
      (let loop2 ([m 10])
	(if (zero? m)
	    (loop (sub1 n))
	    (loop2 (sub1 m)))))))

(printf "single loop~n")
(time
 (let loop ([n 100000])
   (unless (zero? n)
	   (loop (sub1 n)))))

(printf "Y loop~n")
(time
 ((lambda (f n) (f f n))
  (lambda (loop n)
    (unless (zero? n)
	    (loop loop (sub1 n))))
  100000))


(printf "let closure recur~n")
(time
 (let ([f (lambda (x) (sub1 x))])
   (let loop ([n 100000])
     (unless (zero? n)
	     (loop (f n))))))

(printf "direct closure recur~n")
(time
 (let loop ([n 100000])
   (unless (zero? n)
	   (loop ((lambda (x) (sub1 x)) n)))))

(printf "direct closure recur if~n")
(time
 (let loop ([n 100000])
   (if (zero? n)
       (void)
       (loop ((lambda (x) (sub1 x)) n)))))

(printf "let closure top-level~n")
(define loop
 (let ([f (lambda (x) (sub1 x))])
   (lambda (n)
     (unless (zero? n)
	     (loop (f n))))))
(time (loop 100000))

(printf "direct closure top-level~n")
(define loop
  (lambda (n)
    (unless (zero? n)
	    (loop ((lambda (x) (sub1 x)) n)))))
(time (loop 100000))


; > (load "ltest.ss")
; cpu time: 1820  real time: 1826
; cpu time: 1420  real time: 1422
; cpu time: 1960  real time: 1957
; cpu time: 2630  real time: 2626
; > (load "ltest.ss")
; cpu time: 1790  real time: 1803
; cpu time: 1430  real time: 1468
; cpu time: 2150  real time: 2159
; cpu time: 2820  real time: 2824

; > (load "ltest.ss")
; nested loop
; cpu time: 1750  real time: 1817
; single loop
; cpu time: 1430  real time: 1425
; Y loop
; cpu time: 1500  real time: 1500
; let closure recur
; cpu time: 1830  real time: 1835
; direct closure recur
; cpu time: 1790  real time: 1791
; direct closure recur if
; cpu time: 1800  real time: 1793
; let closure top-level
; cpu time: 1810  real time: 1804
; direct closure top-level
; cpu time: 1760  real time: 1758
