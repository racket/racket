#;
(
#f line #f col #f - make-sequence - in-vector
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
123)

#lang typed/scheme
#:optimize

(for: ((i : Integer (vector 1 2 3)))
      (display i))
