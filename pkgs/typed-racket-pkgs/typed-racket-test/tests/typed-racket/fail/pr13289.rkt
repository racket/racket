#;
(exn-pred #rx"fail/pr13289.rkt:9:10:.*in: Natural")
#lang typed/racket

;; This test ensures that the error message for misuse of
;; type names has a source location and reports with the correct
;; syntax

(assert 2 Natural)

