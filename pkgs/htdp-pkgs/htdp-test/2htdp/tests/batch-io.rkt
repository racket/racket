#lang racket

(require rackunit)
(require 2htdp/batch-io)

;; ---------------------------------------------------------------------------------------------------
;; use simulate file to test the I/O functions 

(check-equal?
 (simulate-file read-file 
                "hello world"
                " good bye"
                "done")
 "hello world\n good bye\ndone")

(check-equal? 
 (simulate-file read-lines
                "hello world"
                " good bye"
                "done")
 '("hello world" " good bye" "done"))

(check-equal? 
 (simulate-file read-words
                "hello world"
                " good bye"
                "done")
 '("hello" "world" "good" "bye" "done"))

(check-equal? 
 (simulate-file read-words/line
                "hello world"
                " good bye"
                "done")
 '(("hello" "world") ("good" "bye") ("done")))

(check-equal? (simulate-file read-file) "")

;; ---------------------------------------------------------------------------------------------------
;; manipulate an actual file on disk 

(let ()
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
  
  (write-file file test1)
  (check-true (string=? (simulate-file read-file test1) test1) "read-file 1")
  
  (write-file file test2)
  (check-true (string=? (simulate-file read-file test2) test2) "read-file 2")
  
  (write-file file test1)
  (check-equal? (read-lines file) (list test1) "as-lines 1")
  
  (write-file file test2)
  (check-equal? (read-lines file) test2-as-list "as-lines 2")
  
  (define as-1strings1 (map string (string->list test1)))
  (write-file file test1)
  (check-equal? (read-1strings file) as-1strings1 "as-1strings 1")
  
  (define as-1strings2 
    (map string
         (apply append 
                (map string->list 
                     (cdr
                      (foldr (lambda (f r) (cons "\n" (cons f r))) '() 
                             test2-as-list))))))
  
  (write-file file test2)
  (check-equal? (read-1strings file) as-1strings2 "as-lines 2")
  
  (define test2-a-as-list '("test1" "" "test2"))
  
  (define test2-a
    (apply string-append 
           (list (first test2-as-list)
                 (string #\newline)
                 (string #\newline)
                 (second test2-as-list))))
  
  (write-file file test2-a)
  (check-equal? (read-lines file) test2-a-as-list "as-lines 2-a")
  (check-equal? (read-words file) '("test1" "test2") "as-words 2-a")
  
  (define test3 #<< eos
 word1, word2 
 word3, word4
 eos
    )
  
  (write-file file test3)
  (check-equal? (read-words file) '("word1," "word2" "word3," "word4")
                "as-words")
  (check-equal? (read-words/line file) '(("word1," "word2") ("word3," "word4"))
                "as-words")
  (check-equal? (read-csv-file file) '(("word1" "word2") ("word3" "word4"))
                "as-cvs 1")
  (check-equal? (read-csv-file/rows file length) '(2 2) 
                "as-csv/rows")
  (delete-file file))

;; ---------------------------------------------------------------------------------------------------
;; check failure more of functions. 

(check-exn exn:fail:contract? (lambda () (simulate-file cons)))
;; (check-exn exn:fail:contract? (lambda () (simulate-file))) ;; <--- figure this out 

(check-exn exn:fail:contract? (lambda () (write-file 0 1)))
(check-exn exn:fail:contract? (lambda () (write-file '("test") 1)))
(check-exn exn:fail:contract? (lambda () (write-file "test" '("test"))))

(check-exn exn:fail:contract? (lambda () (read-file 0)))
(check-exn exn:fail:contract? (lambda () (read-file '("test"))))

(check-exn exn:fail:contract? (lambda () (read-lines 0)))
(check-exn exn:fail:contract? (lambda () (read-lines '("test"))))

(check-exn exn:fail:contract? (lambda () (read-1strings 0)))
(check-exn exn:fail:contract? (lambda () (read-1strings '("test"))))

;; ---------------------------------------------------------------------------------------------------
;; read from stdio

(let ()
  (define *standard-out* "")
  (define-syntax-rule 
    (catch-stdo e)
    (set! *standard-out* (with-output-to-string (lambda () e))))
  (define-syntax-rule 
    (pipe-stdi e)
    (with-input-from-string *standard-out* (lambda () e)))
    

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
  
  (catch-stdo (write-file 'standard-out test1))
  (check-equal? (pipe-stdi (read-lines 'standard-in)) (list test1) "as-lines 1")
  
  (catch-stdo (write-file 'standard-out test2))
  (check-equal? (pipe-stdi (read-lines 'standard-in)) test2-as-list "as-lines 2")
  
  (define as-1strings1 (map string (string->list test1)))
  (catch-stdo (write-file 'standard-out test1))
  (check-equal? (pipe-stdi (read-1strings 'standard-in)) as-1strings1 "as-1strings 1")
  
  (define as-1strings2 
    (map string
         (apply append 
                (map string->list 
                     (cdr
                      (foldr (lambda (f r) (cons "\n" (cons f r))) '() 
                             test2-as-list))))))
  
  (catch-stdo (write-file 'standard-out test2))
  (check-equal? (pipe-stdi (read-1strings 'standard-in)) as-1strings2 "as-lines 2")
  
  (define test2-a-as-list '("test1" "" "test2"))
  
  (define test2-a
    (apply string-append 
           (list (first test2-as-list)
                 (string #\newline)
                 (string #\newline)
                 (second test2-as-list))))
  
  (catch-stdo (write-file 'standard-out test2-a))
  (check-equal? (pipe-stdi (read-lines 'standard-in)) test2-a-as-list "as-lines 2-a")
  (check-equal? (pipe-stdi (read-words 'standard-in)) '("test1" "test2") "as-words 2-a")
  
  (define test3 #<< eos
 word1, word2 
 word3, word4
 eos
    )
  
  (catch-stdo (write-file 'standard-out test3))
  (check-equal? (pipe-stdi (read-words 'standard-in)) '("word1," "word2" "word3," "word4")
                "as-words")
  (check-equal? (pipe-stdi (read-words/line 'standard-in)) '(("word1," "word2") ("word3," "word4"))
                "as-words")
  (check-equal? (pipe-stdi (read-csv-file 'standard-in)) '(("word1" "word2") ("word3" "word4"))
                "as-cvs 1")
  (check-equal? (pipe-stdi (read-csv-file/rows 'standard-in length)) '(2 2) 
                "as-csv/rows"))
