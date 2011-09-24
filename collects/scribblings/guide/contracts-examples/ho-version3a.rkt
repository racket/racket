#lang racket/load 

(module a racket/base
  (require racket/contract
           (only-in racket/list first second rest empty?) 
           (prefix-in old: (only-in racket/list argmax)))
  
  (define (argmax f lov)
    (define r (old:argmax f lov))
    (if (and (pair? r) (eq? (cadr r) 'bananas))
        '(3 oranges)
        r)) ;; yet another bug 
  
  (provide
   (contract-out
    [argmax
     (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
          (r (f lov)
             (lambda (r)
               (define f@r (f r))
               (and (is-first-max? r f@r f lov)
                    (dominates-all f@r f lov)))))]))
  
  ;; @code:comment{where}
  
  ;; @code:comment{@#,dominates1}
  (define (dominates-all f@r f lov)
    (for/and ((v lov)) (>= (f v) f@r)))
  
  ;; @code:comment{@#,first?1}
  (define (is-first-max? r f@r f lov)
    (eq? (first (memf (lambda (v) (= (f v) f@r)) lov)) r)))

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
  ;; --- copies from version 2 --- 
  (printf "4: ~s\n" (argmax sqrt '(1)))
  (printf "5: ~s\n" (argmax car '((3 apples) (3 oranges))))
  ;; --- new tests --- 
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "***6: ~s\n" (argmax car '((3 bananas) (3 oranges))))))


(require 'b)
