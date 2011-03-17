#;
(
#f line #f col #f - make-sequence - in-vector
123)

#lang typed/scheme
#:optimize

(for: ((i : Integer (vector 1 2 3)))
      (display i))
