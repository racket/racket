#lang racket/base

(require "test-suite-utils.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;


(define (test-text-balanced? number str start end expected)
  (test
   (string->symbol (format "scheme:text-balanced?-~a" number))
   (lambda (x) 
     (equal? x expected))
   (λ ()
     (queue-sexp-to-mred
      `(let ([t (new scheme:text%)])
         (send t insert ,str)
         (scheme:text-balanced? t ,start ,end))))))

(test-text-balanced? 0 "" 0 #f #f)
(test-text-balanced? 1 "  \n " 0 #f #f)
(test-text-balanced? 2 "foo)" 0 #f #t)
(test-text-balanced? 3 "(foo" 0 #f #f)
(test-text-balanced? 4 "(foo)" 0 #f #t)
(test-text-balanced? 5 "(foo 'bar))" 0 #f #t)
(test-text-balanced? 6 "(foo) bar ([buz])" 0 #f #t)
(test-text-balanced? 7 "(foo]" 0 #f #t)
(test-text-balanced? 8 "{foo} ((bar) [5.9])" 0 #f #t)
(test-text-balanced? 9 "#(1 2 . 3)" 0 #f #t)
