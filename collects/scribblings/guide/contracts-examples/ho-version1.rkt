#lang racket/load 

(module a racket/base
  (require racket/contract (only-in racket/list first second rest empty?) (prefix-in old: (only-in racket/list argmax)))
  
  (define (argmax f lov)
    (old:argmax f lov))
  
  (provide
   (contract-out
    [argmax (-> (-> any/c real?) (and/c pair? list?) any/c)])))

(module b racket/base
  (require 'a)
  (require racket/contract/private/blame)
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "***0: ~s\n" (argmax car '())))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "***1: ~s\n" (argmax car '())))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "***2: ~s\n" (argmax car '((apples 3)))))
  (printf "3: ~s\n" (argmax car '((3 apples) (3 oranges)))))

(require 'b)
