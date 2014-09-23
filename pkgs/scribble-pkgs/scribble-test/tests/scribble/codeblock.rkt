#lang racket
(require (submod scribble/private/manual-code test) tests/eli-tester)

(define (tokens strs)
  (define-values (toks _) (get-tokens strs #f #f))
  (for/list ([tok (rest toks)])
    (match tok
      [(list _ start end 1)
       (list 'function start end 1)]
      [_ tok])))
(define (make-test-result  lst)
  (define-values (res _)
    (for/fold ([result null] [count 12])
              ([p lst])
      (define next (+ count (second p)))
      (define r (if (eq? (first p) 'function) 1 0))
      (values
       (cons (list (first p) count next r) result)
       next)))
  (cons `(function 6 12 1) (reverse res)))

(provide codeblock-tests)
(module+ main (provide codeblock-tests))
(define (codeblock-tests)
  (test
   (tokens (list "#lang racket\n1"))
   => `((function 6 12 1) (white-space 12 13 0) (constant 13 14 0))
   (tokens (list "#lang racket\n" "(+ 1 2)"))
   => (make-test-result
       '((white-space 1)
         (parenthesis 1) (function 1)
         (white-space 1) (constant 1) (white-space 1) (constant 1)
         (parenthesis 1)))
   (tokens (list "#lang racket\n(apply x (list y))"))
   => (make-test-result
       '((white-space 1)
         (parenthesis 1)
         (function 5) (white-space 1);apply
         (function 1) (white-space 1);x
         (parenthesis 1)
         (function 4) (white-space 1) (function 1);list y
         (parenthesis 1)
         (parenthesis 1)))
   ))
