#lang racket/base

(require "test-suite-utils.rkt")

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

(define (test-indentation which before after)
  (test
   (string->symbol (format "scheme:test-indentation-~a" which))
   (λ (x) (equal? x after))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new scheme:text%)]
              [f (new frame% [label ""] [width 600] [height 600])]
              [ec (new editor-canvas% [parent f] [editor t])])
         (send f reflow-container)
         (send t insert ,before)
         (send t tabify-all)
         (send t get-text))))))

(test-indentation 1 "a" "a")
(test-indentation 2 "(a\n b)" "(a\n b)")
(test-indentation 3 "(a\nb)" "(a\n b)")
(test-indentation 3 "(a b\nc)" "(a b\n   c)")
(test-indentation 3 "(a ...\nb)" "(a ...\n b)")
(test-indentation 4 "(lambda (x)\nb)" "(lambda (x)\n  b)")
(test-indentation 5 "(lambdaa (x)\nb)" "(lambdaa (x)\n         b)")
(test-indentation 6
                  "(define x\n  (let/ec return\n    (when 1\n      (when 2\n\t\t      3))\n    2))"
                  "(define x\n  (let/ec return\n    (when 1\n      (when 2\n        3))\n    2))")
