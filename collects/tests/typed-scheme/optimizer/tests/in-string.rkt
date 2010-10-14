#;
(
#f line #f col #f - make-sequence - in-string
123)

#lang typed/scheme
#:optimize

(for: ((i : Char "123"))
      (display i))
