#lang racket/load

(module argmin racket/base
  (require racket/contract)
  
  ;; mk-min : (number number -> boolean) symbol (X -> real) (listof X) -> X
  (define (mk-min cmp name f xs)
    (let ([init-min-var (f (car xs))])
      (let loop ([min (car xs)]
                 [min-var init-min-var]
                 [xs (cdr xs)])
        (cond
          [(null? xs) min]
          [else (let ([new-min (f (car xs))])
                  (cond
                    [(cmp new-min min-var) (loop (car xs) new-min (cdr xs))]
                    [else (loop min min-var (cdr xs))]))]))))
  
  (define (argmin f xs) (mk-min < 'argmin f xs))
  
  (define (good f xs cmp)
    (lambda (max)
      ;; if (empty? (rest xs)) pick the only element on the list 
      (define f@max (f max))
      ;; strengthening the contract: make sure that the first element is picked
      ;; weakening: just ensure that the condition holds for a random element
      (andmap (lambda (x) (cmp f@max (f x))) xs)))
  
  (provide 
   [rename-out (argmin o:argmin)])
  
  (provide/contract
   [rename argmin a:argmin
           (-> (-> any/c real?) list? any/c)]
   [rename argmin i:argmin
           (->i ([f (-> any/c real?)][xs list?]) ()
                (_result (f xs) (flat-named-contract "minimal element" (good f xs <=))))]
   [rename argmin d:argmin
           (->d ([f (-> any/c real?)][xs list?]) ()
                (_result (flat-named-contract "minimal element" (good f xs <=))))]))

(module argmin-perf racket/base
  
  (require 'argmin)
  
  (define (test2 n argmin)
    (collect-garbage) (collect-garbage)
    (time
     (for ((i (in-range (quotient n 2))))
       (argmin (lambda (x) (+ (* x x) 12)) x)))
    (list n argmin)
    (void))
  
  (define n 1000)
  (define x (build-list n (lambda (i) (random n))))
  
  'original 
  (test2 n o:argmin)
  
  '->-contracted 
  (test2 n a:argmin)
  
  'd-contracted
  (test2 n d:argmin)
  
  'i-contracted
  (test2 n i:argmin))

(require 'argmin-perf)
