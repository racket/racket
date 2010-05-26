#lang scheme
(require unstable/list)
(require tests/eli-tester)
(test
 (remf positive? '()) => '()
 (remf positive? '(1 -2 3 4 -5)) => '(-2 3 4 -5)
 (remf even? '(1 -2 3 4 -5)) => '(1 3 4 -5)
 (remf (λ (x) #f) '(1 -2 3 4 -5)) => '(1 -2 3 4 -5))

