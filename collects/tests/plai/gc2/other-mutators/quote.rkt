#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 28)

1 2
(define x
  (cons 'apple-pie ; 2 + 3
        (cons 'pumpkin-pie ; 2 + 3
              empty))) ; 2
; Need 12 cells
1 2
(define y
  '(apple-pie pumpkin-pie))
; Need 24 cells

(define (equal? l r)
  (cond
    [(and (empty? l) (empty? r))
     #t]
    [(and (cons? l) (cons? r))
     (and (equal? (first l) (first r))
          (equal? (rest l) (rest r)))]
    [(and (symbol? l) (symbol? r))
     (symbol=? l r)]
    [else
     #f]))
; Need 2 more for the proc

; Need 2 more for the ans
(equal? x y)
