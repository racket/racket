(load-relative "loadtest.rktl")

(require racket/future)

(Section 'future)

(test 2
  (let ([futures (make-hasheq)])
    (for/async ([_ (in-range 2)])
      (hash-set! futures (current-future) #t))
    (hash-count futures)))

(test 2
  (let ([futures (make-hasheq)])
    (for/async ([i (in-range 10)])
      #:break (= i 1)
      (hash-set! futures (current-future) #t))
    (hash-count futures)))

(test (void)
  (for*/async ([i (in-range 2)]
               [j (in-range 2)])
    (list i j)))
