#lang racket

(require racklog
         tests/eli-tester)

;This is a very trivial program.  In Prolog, it would be:
;
;    city(amsterdam).
;    city(brussels).
;    country(holland).
;    country(belgium).

(define %city
  (lambda (x)
    (%or (%= x 'amsterdam)
         (%= x 'brussels))))

(define %country
  (lambda (x)
    (%or (%= x 'holland)
         (%= x 'belgium))))

;For a more Prolog-style syntax, you can rewrite the same thing,
;using the `%rel' macro, as the following:

(define %city*
  (%rel ()
        (('amsterdam))
        (('brussels))))

(define %country*
  (%rel ()
        (('holland))
        (('belgium))))

;Typical easy queries:
(test 
 (%which (x) (%city x)) 
 (%more)  
 (%more) => #f
 (%which (x) (%country x))
 (%more) 
 (%more) => #f
 (%which () (%city 'amsterdam))
 (%more) => #f
 (%which () (%country 'amsterdam)) => #f)
