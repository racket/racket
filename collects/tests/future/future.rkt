#lang scheme/base

(require scheme/future
         racunit)

#|Need to add expressions which raise exceptions inside a 
future thunk which can be caught at the touch site 
(as opposed to using with-handlers). 

Both future and touch should be called from within a future thunk. 

We should also test deep continuations.

|# 

;; ----------------------------------------

(check-equal? 2 
              (touch (future (λ () 2))))

(let ([f1 (future (λ () (+ 2 2)))] 
      [f2 (future (λ () (+ 5 3)))]) 
  (check-equal? 12 (+ (touch f2) (touch f1))))

(let* ([v 5]
       [f1 (future (λ () 
                     (set! v 10) 
                     v))]) 
  (check-equal? 10 (touch f1)))

(define (build-rand-list lst len) 
  (case len 
    [(0) lst] 
    [else 
     (build-rand-list (cons 
                       (random) 
                       lst) 
                      (- len 1))]))

(define (append-list-of-lists acc lst) 
  (cond 
    [(null? lst) acc] 
    [else 
      (append-list-of-lists 
        (append acc (car lst)) 
        (cdr lst))]))
  

(let* ([nums '()]
       [f1 (future (λ () 
                     (build-rand-list nums 10)))])
  (set! nums (touch f1)) 
  (check-equal? 20 (length (touch (future (λ () (build-rand-list nums 10)))))))

(let* ([f1 (future (λ () 
                     (build-rand-list '() 20)))] 
       [f2 (future (λ () (length (touch f1))))]) 
  (check-equal? 20 (touch f2)))

(check-equal? 50000
              (let ([fts (for/list ([i (in-range 0 10000)]) 
                           (future (λ () (build-rand-list '() 5))))]) 
                (length (append-list-of-lists '() (map touch fts)))))

(check-equal? 31 
              (let* ([f1 (future (λ () (foldl + 0 '(1 2 3 4 5))))] 
                     [f2 (future (λ () (+ (touch 
                                           (future (λ () 
                                                     (+ 6 
                                                        (touch f1))))) 
                                          10)))]) 
                (touch f2)))

(check-equal? 30000
              (let ([fts (for/list ([i (in-range 0 100)]) 
                           (future (λ () 
                                     (build-rand-list '() 300))))]) 
                (collect-garbage) 
                (collect-garbage) 
                (length (append-list-of-lists '() (map touch fts)))))

(define (sum-to acc limit) 
  (case limit 
    [(0) acc] 
    [else 
     (sum-to (+ acc limit) (- limit 1))]))

(check-equal? 
 600030000 
 (let ([f1 (future (λ () (sum-to 0 20000)))] 
       [f2 (future (λ () (sum-to 0 20000)))] 
       [f3 (future (λ () (sum-to 0 20000)))]) 
   (+ (+ (touch f3) (touch f1)) (touch f2))))

(check-equal? 
 #t 
 (let* ( [f1 (future (λ () (build-rand-list '() 10000)))]
         [f2 (future (λ () 
                       (foldl (λ (a b) 
                                (* a b)) 
                              1 
                              (touch f1))))] 
         [f3 (future (λ () (< (touch f2) 1)))]) 
   (touch f3)))

(check-equal?
 '((1) (1))
 (let ([f1 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))]
       [f2 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))])
   (list (continuation-mark-set->list (touch f1) 'x)
         (continuation-mark-set->list (touch f2) 'x))))

(check-equal?
 '((1 0) (1 0))
 (let ([f1 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))]
       [f2 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))])
   (with-continuation-mark
       'x 0
     (list (continuation-mark-set->list (touch f1) 'x)
           (continuation-mark-set->list (touch f2) 'x)))))

(check-equal?
 '((1 0) (1) ())
 (let ([f1 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))]
       [f2 (future (lambda ()
                     (with-continuation-mark
                         'x 1
                       (current-continuation-marks))))])
   (list (continuation-mark-set->list (with-continuation-mark 'x 0
                                        (touch f1))
                                      'x)
         (continuation-mark-set->list (touch f2) 'x)
         (continuation-mark-set->list (current-continuation-marks) 'x))))

