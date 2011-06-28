#lang racket
(require tests/eli-tester
         "../lib/db.rkt")

(test
 (local [(define db (db-connect "localhost:27017:test" #:init? #t))
         (struct num (n) #:prefab)]
   (test
    (for ([i (in-range 10)])
      (test
       (for ([j (in-range 10)])
         (test
          (db-set! db (num (+ i j)) (number->string i) (number->string j))))
       
       (sort (db-list db (number->string i)) string<=?) =>
       (for/list ([j (in-range 10)]) (number->string j))
       
       (for ([j (in-range 10)])
         (test
          (db-ref db (number->string i) (number->string j)) =>
          (num (+ i j))))
       
       (for ([j (in-range 10)])
         (test
          (db-set! db (num (* i j)) (number->string i) (number->string j))))
       
       (for ([j (in-range 10)])
         (test
          (db-ref db (number->string i) (number->string j)) =>
          (num (* i j))))))
    
    (db-close! db))))
