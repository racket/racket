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
   (values
    (+ 5 (k "result arity matters, only first arg can be a different type" 'hahaha #t))
    'must-be-symbol
    #f))

(: r : (All (a b ...) (-> a b ... b (values a b ... b))))
(define (r A . bs) (let/ec break : (values a b ... b) (apply break A bs)))
(r 0 1 2 3 4 5)

