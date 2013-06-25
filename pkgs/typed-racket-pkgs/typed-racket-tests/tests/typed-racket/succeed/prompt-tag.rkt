#lang typed/racket

(: pt (Prompt-Tagof String (Integer -> Integer)))
(define pt (make-continuation-prompt-tag))

;; Test abort
(call-with-continuation-prompt
 (λ () (string-append "foo" (abort-current-continuation pt 5)))
 pt
 (λ: ([x : Integer]) x))

(: pt2 (Prompt-Tagof Integer ((Integer -> Integer) -> Integer)))
(define pt2 (make-continuation-prompt-tag))

;; Test call/comp & abort
(call-with-continuation-prompt
 (λ () (+ 1 ((inst call-with-composable-continuation
                   Integer ((Integer -> Integer) -> Integer) Integer)
             (λ: ([k : (Integer -> Integer)])
               (abort-current-continuation pt2 k))
             pt2)))
 pt2
 (λ: ([f : (Integer -> Integer)]) (f 5)))

;; Test the default handler
(: pt3 (Prompt-Tagof Integer ((-> Integer) -> Integer)))
(define pt3 (make-continuation-prompt-tag))

(+ 2
   (call-with-continuation-prompt
    (λ () (+ 1 (abort-current-continuation pt3 (λ () 5))))
    pt3))
