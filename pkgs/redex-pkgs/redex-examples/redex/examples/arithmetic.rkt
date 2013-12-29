#lang racket
(require redex)

(define-language lang
  (e (binop e e)
     (sqrt e)
     number)
  (binop +
         -
         *
         /)
  
  (e-ctxt (binop e e-ctxt)
          (binop e-ctxt e)
          (sqrt e-ctxt)
          hole)
  (v number))

(define reductions
  (reduction-relation
   lang
   (c--> (+ number_1 number_2)
         ,(+ (term number_1) (term number_2))
         "add")
   (c--> (- number_1 number_2)
         ,(- (term number_1) (term number_2))
         "subtract")
   (c--> (* number_1 number_2)
         ,(* (term number_1) (term number_2))
         "multiply")
   (c--> (/ number_1 number_2)
         ,(/ (term number_1) (term number_2))
         "divide")
   (c--> (sqrt number_1)
         ,(sqrt (term number_1))
         "sqrt")
   with
   [(--> (in-hole e-ctxt_1 a) (in-hole e-ctxt_1 b))
    (c--> a b)]))

(define traces-file
  (make-temporary-file "traces~a.ps"))

(traces/ps reductions (term (- (* (sqrt 36) (/ 1 2)) (+ 1 2)))
           traces-file)

;; Check for the command line flag --no-print
;; If it's set, don't print the temporary file name,
;; This flag is so that DrDr can avoid seeing a change here. 
;; -- samth
(define print-name?
  (let ([print? #t])
    (command-line
     #:once-each
     ["--no-print" "omit printing of file name" (set! print? #f)])
    print?))

(when print-name?
  (printf "Traces are in ~a\n" traces-file))

;; Test mode: no printing:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "--no-print")])
    (dynamic-require (quote-module-path "..") #f)))
