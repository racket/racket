#lang racket/load 

(module a racket/base
  (require racket/contract (only-in racket/list first second rest empty?) (prefix-in old: (only-in racket/list argmax)))
  
  (define (argmax f lov) 
    (cond 
      [(empty? (rest lov)) (first lov)]
      [else (define r (old:argmax f lov))
            (if (and (pair? r) (eq? (cadr r) 'bananas))
                '(3 oranges) ;; a bug
                r)]))
  
  (provide
   (contract-out
    [argmax
     (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
          (r (f lov)
             (lambda (r)
               (cond
                [(empty? (rest lov)) (eq? (first lov) r)]
                [else
                 (define f@r (f r))
                 (define flov (map f lov))
                 (and (is-first-max? r f@r (map list lov flov))
                      (dominates-all f@r flov))]))))]))
  
  ; f@r is greater or equal to all f@v in flov
  (define (dominates-all f@r flov)
    (for/and ((f@v flov)) (>= f@r f@v)))
  
  ; r is (second x) for the first x in flov+lov s.t. (= (first x) f@r)
  (define (is-first-max? r f@r lov+flov)
    (define fst (first lov+flov))
    (if (= (second fst) f@r)
        (eq? (first fst) r)
        (is-first-max? f@r r (rest lov+flov)))))

(module b racket/base
  (require 'a)
  (require racket/contract/private/blame)
  ;; --- copied from version 1 --- 
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 0: ~s\n" (argmax car '())))
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "*** 1: ~s\n" (argmax car '())))
  ;; changed: 
  ;; the functin violates -> real but the single element isn't checked
  (printf "2: ~s\n" (argmax car '((apples 3))))
  (printf "3: ~s\n" (argmax car '((3 apples) (3 oranges))))
  ;; --- copies from version 2 --- 
  (printf "4: ~s\n" (argmax sqrt '(1)))
  (printf "5: ~s\n" (argmax car '((3 apples) (3 oranges))))
  ;; --- new tests --- 
  (with-handlers ([exn:fail:contract:blame? void])
    (printf "***6: ~s\n" (argmax car '((3 bananas) (3 oranges))))))

(require 'b)
