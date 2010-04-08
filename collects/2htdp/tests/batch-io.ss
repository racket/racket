#lang scheme 

(require schemeunit)
(require 2htdp/batch-io)

(define file "batch-io.txt")

(define test1 #<<eos
test1
eos
)

(define test2-as-list '("test1" "test2"))

(define test2
  (apply string-append 
         (list (first test2-as-list)
               (string #\newline)
               (second test2-as-list))))

(or (write-file file test1)
    (check-true (string=? (read-file file) test1) "read-file 1"))

(or (write-file file test2)
    (check-true (string=? (read-file file) test2) "read-file 2"))

(or (write-file file test1)
    (check-equal? (read-file-as-lines file) (list test1) "read-file-as-lines 1"))

(or (write-file file test2)
    (check-equal? (read-file-as-lines file) test2-as-list "read-file-as-lines 2"))


(check-exn exn:fail:contract? (lambda () (write-file 0 1)))
(check-exn exn:fail:contract? (lambda () (write-file '("test") 1)))
(check-exn exn:fail:contract? (lambda () (write-file "test" '("test"))))

(check-exn exn:fail:contract? (lambda () (read-file 0)))
(check-exn exn:fail:contract? (lambda () (read-file '("test"))))

(check-exn exn:fail:contract? (lambda () (read-file-as-lines 0)))
(check-exn exn:fail:contract? (lambda () (read-file-as-lines '("test"))))

