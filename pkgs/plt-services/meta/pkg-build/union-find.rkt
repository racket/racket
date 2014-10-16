#lang racket/base

(provide union! find! elect!)

(define (find! reps key)
  (define rep-key (hash-ref reps key key))
  (if (equal? rep-key key)
      key
      (let ([rep-key (find! reps rep-key)])
        (hash-set! reps key rep-key)
        rep-key)))

(define (elect! reps key)
  (define rep-key (find! reps key))
  (unless (equal? rep-key key)
    (hash-set! reps rep-key key)
    (hash-set! reps key key)))

(define (union! reps a-key b-key)
  (define rep-a-key (find! reps a-key))
  (define rep-b-key (find! reps b-key))
  (unless (equal? rep-a-key rep-b-key)
    (hash-set! reps rep-b-key rep-a-key))
  rep-a-key)

(module+ test
  (require rackunit)

  (define t1 (make-hash))
  (void
   (union! t1 "1" "2")
   (union! t1 "a" "b")
   (union! t1 "b" "c")
   (union! t1 "d" "e")
   (union! t1 "f" "d")
   (union! t1 "3" "2")
   (union! t1 "g" "d")
   (union! t1 "b" "d"))

  (check-equal? (find! t1 "a") "a")
  (check-equal? (find! t1 "b") "a")
  (check-equal? (find! t1 "b") "a")
  (check-equal? (find! t1 "d") "a")
  (check-equal? (find! t1 "e") "a")
  (check-equal? (find! t1 "f") "a")
  (check-equal? (find! t1 "g") "a")

  (elect! t1 "c")

  (check-equal? (find! t1 "a") "c")
  (check-equal? (find! t1 "b") "c")
  (check-equal? (find! t1 "b") "c")
  (check-equal? (find! t1 "d") "c")
  (check-equal? (find! t1 "e") "c")
  (check-equal? (find! t1 "f") "c")
  (check-equal? (find! t1 "g") "c")

  (check-equal? (find! t1 "1") "3")
  (check-equal? (find! t1 "2") "3")
  (check-equal? (find! t1 "3") "3"))


  
  
