#lang typed/racket

(: pt (Prompt-Tag String (Integer -> Integer)))
(define pt (make-continuation-prompt-tag))

;; Test abort
(call-with-continuation-prompt
 (λ () (string-append "foo" (abort-current-continuation pt 5)))
 pt
 (λ: ([x : Integer]) x))

(: pt2 (Prompt-Tag Integer ((Integer -> Integer) -> Integer)))
(define pt2 (make-continuation-prompt-tag))

;; Test call/comp & abort
(call-with-continuation-prompt
 (λ () (+ 1 (call-with-composable-continuation
             (λ: ([k : (Integer -> Integer)])
               (abort-current-continuation pt2 k))
             pt2)))
 pt2
 (λ: ([f : (Integer -> Integer)]) (f 5)))