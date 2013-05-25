#;
(exn-pred exn:fail:syntax? #rx".*could not be converted.*")

#lang racket/load

(require typed/racket)

(cast 2 (All (a) (Ephemeronof a)))
