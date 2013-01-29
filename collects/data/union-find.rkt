#lang racket/base

(provide uf-set? 
         uf-new
         uf-union!
         uf-find
         uf-set-canonical!)

(struct uf-set (x rank) #:mutable
  #:methods gen:custom-write
  [(define write-proc 
     (λ (uf port mode)
       (write-string "#<uf-set: " port)
       (define recur
         (case mode
           [(#t) write]
           [(#f) display]
           [else (λ (p port) (print p port mode))]))
       (recur (uf-find uf) port)
       (write-string ">" port)))])
(define (uf-new x) (uf-set (box x) 0))
(define (uf-union! a b) 
  (define a-rank (uf-set-rank a))
  (define b-rank (uf-set-rank b))
  (cond
    [(< a-rank b-rank)
     (set-uf-set-x! a b)]
    [else
     (set-uf-set-x! b a)
     (when (= a-rank b-rank)
       (set-uf-set-rank! a 1))]))
(define (uf-find a)
  (define bx (uf-get-box a))
  (unbox bx))
(define (uf-set-canonical! a b)
  (set-box! (uf-get-box a) b))
(define (uf-get-box a)
  (let loop ([a a])
    (cond
      [(box? (uf-set-x a))
       (uf-set-x a)]
      [else
       (define fnd (loop (uf-set-x a)))
       (set-uf-set-x! a fnd)
       fnd])))


(module+ test
  (require rackunit
           racket/list)
  
  (check-equal? (uf-find (uf-new 1)) 1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find a))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find b))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find a)
                  (uf-find a))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find b)
                  (uf-find b))
                1)
  (check-equal? (let ([sp (open-output-string)])
                  (display (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: x>")
  (check-equal? (let ([sp (open-output-string)])
                  (write (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: \"x\">")
  (check-equal? (let ([sp (open-output-string)])
                  (print (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: \"x\">")
  (check-equal? (let ([sp (open-output-string)])
                  (define x (vector 1))
                  (define a (uf-new x))
                  (vector-set! x 0 a)
                  (write x sp)
                  (get-output-string sp))
                "#0=#(#<uf-set: #0#>)")
  (check-equal? (let ([sp (open-output-string)])
                  (define a (uf-new #f))
                  (uf-set-canonical! a a)
                  (write a sp)
                  (get-output-string sp))
                "#0=#<uf-set: #0#>")
  
  
  (define (check-ranks uf)
    (let loop ([uf/box uf]
               [rank -inf.0])
      (cond
        [(box? uf/box) (void)]
        [else 
         (unless (<= rank (uf-set-rank uf))
           (error 'check-ranks "failed for ~s" 
                  (let loop ([uf uf])
                    (cond
                      [(box? uf) `(box ,(unbox uf))]
                      [else `(uf-set ,(loop (uf-set-x uf))
                                     ,(uf-set-rank uf))]))))
         (loop (uf-set-x uf/box)
               (uf-set-rank uf/box))])))
  
  (for ([x (in-range 1000)])
    (define num-sets (+ 2 (random 40)))
    (define uf-sets 
      (shuffle
       (for/list ([x (in-range num-sets)])
         (uf-new x))))
    (let loop ([uf-set (car uf-sets)]
               [uf-sets (cdr uf-sets)])
      (when (zero? (random 3))
        (uf-find uf-set))
      (unless (null? uf-sets)
        (uf-union! uf-set (car uf-sets))
        (loop (car uf-sets)
              (cdr uf-sets))))
    (check-true
     (apply = (map uf-find uf-sets)))
    
    (for ([uf (in-list uf-sets)])
      (check-ranks uf))))

