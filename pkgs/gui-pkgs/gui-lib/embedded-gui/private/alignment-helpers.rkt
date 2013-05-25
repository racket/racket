(module alignment-helpers mzscheme
  
  (require
   mzlib/list
   mzlib/class
   mred
   mzlib/contract
   
   "interface.rkt"
   "snip-lib.rkt")
  
  (provide/contract
   (vacuous-max (() (listof number?) . ->* . (number?)))
   (child-height ((union (is-a?/c alignment<%>) (is-a?/c snip%)) . -> . number?))
   (child-width ((union (is-a?/c alignment<%>) (is-a?/c snip%)) . -> . number?))
   (insert-before (any/c (listof any/c) any/c . -> . (listof any/c)))
   (insert-after (any/c (listof any/c) any/c . -> . (listof any/c))))
  
  (define (vacuous-max . n)
    (if (empty? n)
        0
        (apply max n)))
  
  #|
  (equal? (vacuous-max 5 2) 5)
  (equal? (vacuous-max) 0)
  (equal? (vacuous-max -2 6) 6)
  (equal? (vacuous-max -3 -5) -3)
  (equal? (vacuous-max -2 0) 0)
  |#
  
  (define (child-height item)
    (cond
      [(is-a? item snip%) (snip-min-height item)]
      [(is-a? item alignment<%>) (send item get-min-height)]))
  
  (define (child-width item)
    (cond
      [(is-a? item snip%) (snip-min-width item)]
      [(is-a? item alignment<%>) (send item get-min-width)]))
  
  (define ((insert how) item alist reference)
    (cond
      [(empty? alist) (error 'insert "Could not find item in list")]
      [(cons? alist)
       (if (equal? (first alist) reference)
           (how item (first alist) (rest alist))
           (cons (first alist)
                 ((insert how) item (rest alist) reference)))]))
  
  (define insert-before
    (insert (lambda (item the-first the-rest)
              (cons item
                    (cons the-first the-rest)))))
  
  (define insert-after
    (insert (lambda (item the-first the-rest)
              (cons the-first
                    (cons item the-rest)))))
  #|
  (equal? (insert-before 1 '(3 4 2 5) 2)
          '(3 4 1 2 5))
  (equal? (insert-before 1 '(1) 1)
          '(1 1))
  (equal? (insert-before 0 '(5 -5 6 -5) -5)
          '(5 0 -5 6 -5))
  (equal? (insert-after 1 '(3 4 2 5) 2)
          '(3 4 2 1 5))
  (equal? (insert-after 1 '(1) 1)
          '(1 1))
  (equal? (insert-after 0 '(5 -5 6 -5) -5)
          '(5 -5 0 6 -5))
  |#
  )
