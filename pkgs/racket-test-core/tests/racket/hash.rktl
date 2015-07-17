
(load-relative "loadtest.rktl")

(Section 'hash)

(require racket/hash)

(test #hash([4 . four] [3 . three] [1 . one] [2 . two])
      hash-union #hash([1 . one] [2 . two]) #hash([3 . three] [4 . four]))
(test #hash([four . 4] [three . 3] [one . 1] [two . 2])
      hash-union #hash([one . 1] [two . 1]) #hash([three . 3] [four . 4] [two . 1])
      #:combine +)

(let ()
  (define h (make-hash))
  (hash-union! h #hash([1 . one] [2 . two]))
  (hash-union! h #hash([3 . three] [4 . four]))
  (test #t
        equal?
        (hash-copy
         #hash([1 . one] [2 . two] [3 . three] [4 . four]))
        h))
(let ()
  (define h (make-hash))
  (hash-union! h #hash([one . 1] [two . 1]))
  (err/rt-test (hash-union! h #hash([three . 3] [four . 4] [two . 1])) exn:fail?))
(let ()
  (define h (make-hash))
  (hash-union! h #hash([one . 1] [two . 1]))
  (hash-union! h #hash([three . 3] [four . 4] [two . 1])
               #:combine/key (lambda (k x y) (+ x y)))
  (test #t
        equal?
        (hash-copy
         #hash([one . 1] [two . 2] [three . 3] [four . 4]))
        h))
