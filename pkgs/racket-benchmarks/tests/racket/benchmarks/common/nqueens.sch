;;; NQUEENS -- Compute number of solutions to 8-queens problem.
;; 2006/08 -- renamed `try' to `try-it' to avoid Bigloo collision (mflatt)
;; 2010/04 -- got rid of the one-armed id (stamourv)

(define trace? #f)

(define (nqueens n)

  (define (one-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (try-it x y z)
    (if (null? x)
      (if (null? y)
        (begin (if trace? (begin (write z) (newline)) #t) 1)
        0)
      (+ (if (ok? (car x) 1 z)
           (try-it (append (cdr x) y) '() (cons (car x) z))
           0)
         (try-it (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (try-it (one-to n) '() '()))

(let ((input (with-input-from-file "input.txt" read)))
  (time
   (let loop ((n 10000) (v 0))
     (if (zero? n)
         v
         (loop (- n 1) (nqueens (if input 8 0)))))))
