#lang setup/infotab

(define name "SrPersist")
;; no .zo compilation necessary, since all the real code is in C++
(define compile-omit-files '("info.ss" "srpersist.ss"))
