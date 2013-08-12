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
    (values (drop file n) (take file n)))
  
  (require (for-syntax syntax/parse))
  
  ;; (parse-file x:id ([y:id e:expr][(u:id ...) f:expr] ...) body:expr)
  ;; teases apart x (a list of lines) according to the expressions e, f, ... and successively binds 
  ;; the values in the lines to y, u, ... and eventually evaluates the body ... expressions 
  ;; Restriction: to one body expressions so that this form does not inject errors into *SLs 
  (define-syntax (parse-file stx)
    (syntax-parse stx
      [(parse-file file:id ((x:id e:expr) clauses ...) body:expr)
       #'(let-values ([(x file) (split-at file (enni e))])
           (parse-file file (clauses ...) body))]
      [(parse-file file:id (((x:id ...) e:expr) clauses ...) body:expr)
       #'(let-values ([(file x ...) 
                       (let-values ([(bundle file) (split-at file (enni e))])
                         (apply values file (first bundle)))])
           (parse-file file (clauses ...) body))]
      [(parse-file file:id () body) #'body]))
  
  ;; Any -> Exact-nonnegative-integer
  ;; coercion with error message for parse-file 
  (define (enni x)
    (if (exact-nonnegative-integer? x) x (error 'parse-file pem x)))
  
  (define pem "exact-nonnegative-integer expected for relative file position; given: ~e"))

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
           (parse-file file ([(sales-record# queries#) 1]
                             [sales-records sales-record#]
                             [queries queries#])
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
