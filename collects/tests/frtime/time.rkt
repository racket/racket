#lang frtime
(define x (rec y (0 . until . (add1 (inf-delay y)))))

(==> (filter-e zero? (changes (modulo seconds 10)))
     (lambda (v)
       (printf "~s\n" (value-now x))))
