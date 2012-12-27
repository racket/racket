#lang racket

;; test for pr11445

(require 2htdp/batch-io)

(define batch-io3
  #<<eos
2      
4 2    
3 hamburger 3.00
2 cheeseburger 1.50
1 hamburger 1.00
1 fries 0.75
hamburger
fries
2 1    
4 jackets 42.34
2 jackets 21.17
jackets
eos
  )

(define batch-io3-expected
  #<<eos
You sold 4 hamburgers for a total of $4.00.
You sold 1 fries for a total of $0.75.

You sold 6 jackets for a total of $63.51.


eos
  )

(define batch-io3.sexpr
  '((2)
    (4 2)
    (3 "hamburger" 3.00)
    (2 "cheeseburger" 1.50)
    (1 "hamburger" 1.00)
    (1 "fries" 0.75)
    ("hamburger")
    ("fries")
    (2 1)
    (4 "jackets" 42.34)
    (2 "jackets" 21.17)
    ("jackets")))

(with-output-to-file "batch-io3.txt"
  #:exists 'replace
  (lambda ()
    (printf batch-io3)))

(define batch-io3.read 
  (read-words-and-numbers/line "batch-io3.txt"))

(with-handlers ((exn:fail:filesystem? void))
  (delete-file "batch-io3.txt"))

;; ---------------------------------------------------------------------------------------------------
;; time to test results 

(module+ test 
  (require rackunit)
  (check-equal? batch-io3.read batch-io3.sexpr))

(module+ test
  (define (split file n)
    (values (drop file n) (take file n))))

(module+ test
  ;; String -> Void
  ;; read input, process queries, write output 
  (define (solution file-name)
    ;; S-expression [Dataset -> String] -> String
    (define (input->output i)
      (let loop ((ds# (first (first i))) (file (rest i)))
        (cond
          [(zero? ds#) ""]
          [else
           (let*-values ([(file sales-records# queries#) (apply values (rest file) (first file))]
                         [(file sales-records)           (split file sales-records#)]
                         [(file queries)                 (split file queries#)])
             (string-append (process-dataset sales-records queries) "\n" (loop (sub1 ds#) file)))])))
    ;; Dataset -> String 
    ;; process one dataset 
    (define (process-dataset sales-records queries)
      (foldr (λ (query r) (string-append (total-message sales-records (first query)) r)) "" queries))

    ;; [Listof [List Number String Number]] String -> String 
    ;; compute query result and turn into string 
    (define (total-message sales-records query)
      (define good* (filter (λ (sales-record) (string=? (second sales-record) query)) sales-records))
      (define count (apply + (map first good*)))
      (define value (apply + (map third good*)))
      (format "You sold ~a ~a for a total of $~a.\n" 
              count (add-s query (length good*)) (real->decimal-string value 2)))
    ;; String Natural -> String 
    ;; add s to string if plurality calls for it
    (define (add-s str n)
      (if (and (> n 1) (not (string=? (substring str (- (string-length str) 1)) "s")))
          (string-append str "s")
          str))
    ;; -- in -- ;; should be write-file from batch io
    (with-output-to-string (λ () (printf (input->output batch-io3.sexpr)))))
  
  (check-equal? (solution "don't care") batch-io3-expected))