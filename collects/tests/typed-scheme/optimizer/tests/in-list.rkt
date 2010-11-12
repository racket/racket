#;
(
#f line #f col #f - make-sequence - in-list
123)

#lang typed/scheme
#:optimize

(for: ((i : Natural '(1 2 3)))
      (display i))
