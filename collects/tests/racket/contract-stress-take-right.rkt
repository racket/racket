#lang racket/load

(module take-right racket/base
  (require racket/contract)

  (define-syntax-rule 
    (define/lead (name x n) base combine pfx ...)
    ;; MF: lead is (- (depth x) n) items, ergo traversing lead leaves
    ;;     (- (depth x) (- (depth x) n)) = n items to be dealt with 
    ;;     either via a combine ~ cons or an accumulator 
    (define (name x n)
      (let loop ([lead (drop x n)] [x x][pfx '()] ...)
        (if (pair? lead) 
            (combine (car x) (loop (cdr lead) (cdr x) (cons (car x) pfx) ...))
            (base x pfx ...)))))
  
  (define (drop x n)
    (if (zero? n) x (drop (cdr x) (sub1 n))))
  
  (define-syntax-rule (K- a b) b)

  (define/lead (take-right x n)
    values K-) ;; I 8! values as identity
  
  ;; S-expression -> natural-number/c
  (define (depth l)
    (let D ([l l][d 0])
      (if (pair? l) (D (cdr l) (+ d 1)) d)))
  
  ;; S-expression -> natural-number/c
  (define (min-depth x <)
    (lambda (n)
      (let D ([l x][d n])
        (if (= d 0) 
            #t
            (if (pair? l) 
                (D (cdr l) (- d 1))
                #f)))))
  
  (provide 
   [rename-out (take-right o:take-right)])
  
  (provide/contract
   [rename take-right a:take-right (-> any/c natural-number/c any/c)]
   
   [rename take-right d:take-right
     (->d ([x any/c][n (and/c natural-number/c (min-depth x <))]) ()
          (_result (compose (=/c n) depth)))]
   
   [rename take-right i:take-right
     (->i ([x any/c][n (x) (and/c natural-number/c (min-depth x <))]) ()
          (_result (n) (compose (=/c n) depth)))]))

(module take-right-perf racket/base 
  (require 'take-right)

  (define (test n -right)
    (define x (build-list n add1))
    (collect-garbage) (collect-garbage)
    (time
     (for ((i (in-range (quotient n 2))))
       (-right x i)))
    (list n -right)
    (void))
  
  (define n 10000)
  (define x (build-list n (lambda (i) (random n))))
  
  
  'original 
  (test n o:take-right)
  
  '->-contracted 
  (test n a:take-right)
  
  'd-contracted
  (test n d:take-right)
  
  'i-contracted
  (test n i:take-right))

(require 'take-right-perf)

