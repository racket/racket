;;; primes.scm  --  Jens Axel Søgaard

; (primes m)  returns the list of all primes in the range 2 to m.


; This was written to show the use of the priority queues 
; in priority-queue.scm.

(require (planet "priority-queue.scm" ("soegaard" "galore.plt" 2 1)))

;; ALGORITHM

; The algorithm finds all primes between 2 and m.

;; VARIABLE INITIALIZATION
;  L = (list 2)
;  n = 3  
;  PQ empty priority queue with elements of the type (e,d) in NxN sorted after e.

; INVARIANT

; The idea is to let L, n and PQ satisfy these invariants in the main loop.

;  1. n odd
;  2. L = { p | p in [2,n-1] prime }
;  3. eliminated(PQ) = { x in [n,N] | there exists p in [2,n-1] s.t. p|x }
;                    = { x in [n,N] | there exists (e,d) s.t. d|x-e }
;  4. for all (e,d) in PQ we have e>=n

; Therefore n is a prime, if n is not equal to the first component of
; the least element (e,d) in PQ.

; primes : integer -> (list integer)
;  return list of primes in the range 2 to m
(define (primes m)
  (let loop ([n 3]
             [L (list 2)]
             [PQ (empty)])
    (if (> n m)
        L
        (if (or (empty? PQ)
                (< n (first (find-min PQ))))
            ; n is prime 
            (loop (+ n 2) 
                  (cons n L) 
                  (update (insert (list (* 2 n) n) (* 2 n) PQ ) 
                          (+ n 1)))
            ; n is composite 
            (loop (+ n 2)
                  L 
                  (update PQ (+ n 1)))))))

; Update "pushes" the already eliminated elements in order to
; satisfy the fourth invariant (i.e. e>n for all elements (e,d) )
; Note, that it's possible that several elements need updating.

; update : priority-queue integer -> priority-queue
(define (update PQ n)
  (let* ([min (find-min PQ)]
	 [e   (first min)]
	 [d   (second min)])
    (if (<= e n)
        (update (insert (list (+ e d) d) (+ e d)
                        (delete-min PQ))
                n)
        PQ)))


; pi : integer -> integer
;  return the number of primes less or equal to x
(define (pi x)
  (length (primes x)))


; TEST

(for-each (lambda (n)
            (display (format "The number of primes less than ~a is ~a.\n" n (pi n))))
          (list 10 100 1000 10000))
(display (format "The primes less than 1000 is: \n~a\n " (reverse (primes 1000))))
