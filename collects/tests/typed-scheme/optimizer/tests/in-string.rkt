#;
(
#f line #f col #f - make-sequence - in-string
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
#f line #f col #f - op - dead else branch
123)

#lang typed/scheme
#:optimize

(for: ((i : Char "123"))
      (display i))
