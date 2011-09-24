#lang racket/load 

(module a racket/base
  (require racket/contract
           (only-in racket/list first second rest empty?) 
           (prefix-in old: (only-in racket/list argmax)))
  
  (define (argmax f lov)
    (define r (old:argmax f lov))
    (if (and (number? r) (= r 1/4)) 1/5 r)) ;; a bug 
  
  (provide
   (contract-out
    [argmax
      (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) () 
	(r (f lov)
           (lambda (r)
             (define f@r (f r))
             (for/and ((v lov)) (>= f@r (f v))))))])))

(module b racket/base
  (require 'a)
  (require racket/contract/private/blame)
  ;; --- copied from version 1 --- 
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 0: ~s\n" (argmax car '())))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 1: ~s\n" (argmax car '())))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 2: ~s\n" (argmax car '((apples 3)))))
  (printf "3: ~s\n" (argmax car '((3 apples) (3 oranges))))
  ;; --- new tests --- 
  (printf "4: ~s\n" (argmax sqrt '(1)))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 4: ~s\n" (argmax sqrt '(1/4))))
  (printf "5: ~s\n" (argmax car '((3 apples) (3 oranges)))))
  

(require 'b)
