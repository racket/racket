; The recursive-nfa benchmark.  (Figure 45, page 143.)

(define (recursive-nfa input)
  
  (define (state0 input)
    (or (state1 input) (state3 input) #f))
  
  (define (state1 input)
    (and (not (null? input))
         (or (and (char=? (car input) #\a)
                  (state1 (cdr input)))
             (and (char=? (car input) #\c)
                  (state1 input))
             (state2 input))))
  
  (define (state2 input)
    (and (not (null? input))
         (char=? (car input) #\b)
         (not (null? (cdr input)))
         (char=? (cadr input) #\c)
         (not (null? (cddr input)))
         (char=? (caddr input) #\d)
         'state2))
  
  (define (state3 input)
    (and (not (null? input))
         (or (and (char=? (car input) #\a)
                  (state3 (cdr input)))
             (state4 input))))
  
  (define (state4 input)
    (and (not (null? input))
         (char=? (car input) #\b)
         (not (null? (cdr input)))
         (char=? (cadr input) #\c)
         'state4))
  
  (or (state0 (string->list input))
      'fail))

(time (let ((input (string-append (make-string 133 #\a) "bc")))
        (let loop ((n 10000))
          (unless (zero? n)
            (recursive-nfa input)
            (loop (- n 1))))))



