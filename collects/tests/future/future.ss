(load-relative "loadtest.ss")

(Section 'future)
(require scheme/future)

;; ----------------------------------------

(test 2 
      touch 
       (future (λ () 
                 2)))

(let ([f1 (future (λ () (+ 2 2)))] 
            [f2 (future (λ () (+ 5 3)))]) 
        (test 12 + (touch f2) (touch f1)))

(let* ([v 5]
             [f1 (future (λ () 
                           (set! v 10) 
                           v))]) 
        (test 10 touch f1))

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
    [(empty? lst) acc] 
    [else 
      (append-list-of-lists 
        (append acc (first lst)) 
        (rest lst))]))
  
(let* ([nums '()]
       [f1 (future (λ () 
                     (build-rand-list nums 10)))]) 
  (set! nums (touch f1)) 
  (test 20 length (touch (future (λ () 
                           (build-rand-list nums 10))))))

(let* ([f1 (future (λ () 
                     (build-rand-list '() 20)))] 
       [f2 (future (λ () (length (touch f1))))]) 
  (test 20 touch f2))

(test 50000 'test7
      (let ([fts (for/list ([i (in-range 0 10000)]) 
                   (future (λ () (build-rand-list '() 5))))]) 
        (length (append-list-of-lists '() (map touch fts)))))

(test 31 'test8 
      (let* ([f1 (future (λ () (foldl + 0 '(1 2 3 4 5))))] 
             [f2 (future (λ () (+ (touch 
                                   (future (λ () 
                                             (+ 6 
                                                (touch f1))))) 
                                  10)))]) 
        (touch f2)))

(test 30000 'test9
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

(test 600030000 'test10 
 (let ([f1 (future (λ () (sum-to 0 20000)))] 
       [f2 (future (λ () (sum-to 0 20000)))] 
       [f3 (future (λ () (sum-to 0 20000)))]) 
   (+ (+ (touch f3) (touch f1)) (touch f2))))

(test #t 'test11 
  (let* ( [f1 (future (λ () (build-rand-list '() 10000)))]
          [f2 (future (λ () 
                          (foldl (λ (a b) 
                                    (* a b)) 
                                  1 
                                  (touch f1))))] 
          [f3 (future (λ () (< (touch f2) 1)))]) 
    (touch f3)))
                          

(report-errs)













