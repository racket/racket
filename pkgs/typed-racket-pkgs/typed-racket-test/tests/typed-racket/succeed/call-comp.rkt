#lang typed/racket

(require racket/control)

(: tag (Prompt-Tagof Integer (Integer -> Integer)))
(define tag (make-continuation-prompt-tag))

(call-with-continuation-prompt
 (λ ()
    (+ 1
       ((inst call-with-composable-continuation Integer (Integer -> Integer)
              Integer)
        (lambda: ([k : (Integer -> Integer)]) (k 1))
        tag)))
 tag
 (λ: ([x : Integer]) (+ 1 x)))

((inst call/ec Integer Integer Integer) (lambda ([f : (Integer Integer -> Nothing)]) (f 0 1)))
(let/cc k : (values String Symbol Boolean)
   (+ 5 (k "result arity doesn't matter" 'hahaha #t)))
