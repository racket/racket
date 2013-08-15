#lang unstable/2d racket
(require unstable/2d/match
         rackunit)

(check-equal?
 #2dmatch
 ╔═══════╦══════╦══════════╗
 ║     1 ║  'x  ║  x       ║
 ║ 2     ║      ║          ║
 ╠═══════╬══════╬══════════╣
 ║   y   ║  #f  ║(list x y)║
 ╚═══════╩══════╩══════════╝
 (list 1 2))

(define (≤ t1 t2)
  #2dmatch
  ╔══════════╦══════╦═══════╦══════════╦═════════════════╗
  ║     t2   ║ 'Int ║ 'Real ║ 'Complex ║   `(-> ,t2d     ║
  ║ t1       ║      ║       ║          ║        ,t2r)    ║
  ╠══════════╬══════╩═══════╩══════════╬═════════════════╣
  ║  'Int    ║                         ║                 ║
  ╠══════════╬══════╗          #t      ║                 ║
  ║ 'Real    ║      ║                  ║       #f        ║
  ╠══════════╣      ╚═══════╗          ║                 ║
  ║'Complex  ║              ║          ║                 ║
  ╠══════════╣              ╚══════════╬═════════════════╣
  ║`(-> ,t1d ║      #f                 ║(and (≤ t2d t1d) ║
  ║     ,t1r)║                         ║     (≤ t1r t2r))║
  ╚══════════╩═════════════════════════╩═════════════════╝)
  
(check-equal? (≤ 'Int 'Int) #t)
(check-equal? (≤ 'Int 'Real) #t)
(check-equal? (≤ 'Real 'Int) #f)
(check-equal? (≤ 'Complex 'Complex) #t)
(check-equal? (≤ 'Complex 'Int) #f)
(check-equal? (≤ '(-> Real Int) '(-> Int Real)) #t)
(check-equal? (≤ '(-> Int Complex) '(-> Int Real)) #f)

(check-equal?
 #2dmatch
 ╔════════╦═══╗
 ║      3 ║ x ║
 ║ 1      ║   ║
 ╠════════╬═══╣
 ║   2    ║   ║
 ╠════════╣ x ║
 ║   1    ║   ║
 ╚════════╩═══╝
 3)

(check-equal?
 #2dmatch
 ╔════════╦═══╗
 ║      3 ║ x ║
 ║ 1      ║   ║
 ╠════════╬═══╣
 ║   z    ║   ║
 ╠════════╣ x ║
 ║   q    ║   ║
 ╚════════╩═══╝
 3)

(check-equal?
 #2dmatch
 ╔════════╦═══════╗
 ║      3 ║   x   ║
 ║ 1      ║       ║
 ╠════════╬═══════╣
 ║   y    ║       ║
 ╠════════╣(+ x y)║
 ║   y    ║       ║
 ╚════════╩═══════╝
 4)
