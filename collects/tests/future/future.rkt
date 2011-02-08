#lang scheme/base

(require scheme/future 
         scheme/list 
         rackunit)

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
   (sleep 0.1)
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

;Tests for current-future
(check-equal? #f (current-future)) 
(check-equal? #t (equal? (current-future) (current-future)))

(let ([f (future (λ () (current-future)))]) 
  (check-equal? #t (equal? f (touch f))))

;Where futures might be touched before ever making it 
;to a worker kernel thread
(let ([f1 (future (λ () (current-future)))]
      [f2 (future (λ () (current-future)))]) 
  (check-equal? #t (equal? f1 (touch f1))) 
  (check-equal? #f (equal? f2 (touch f1)))
  (check-equal? #t (equal? f2 (touch f2)))
  (check-equal? #f (equal? (touch f2) (touch f1)))
  (check-equal? #f (equal? (current-future) (touch f1))) 
  (check-equal? #f (equal? (current-future) (touch f2))))

;Where futures are pretty much guaranteed to be running 
;on a worker thread
(let ([f1 (future (λ () (current-future)))]
      [f2 (future (λ () (current-future)))]) 
  (sleep 0.1)
  (check-equal? #t (equal? f1 (touch f1))) 
  (check-equal? #f (equal? f2 (touch f1)))
  (check-equal? #t (equal? f2 (touch f2)))
  (check-equal? #f (equal? (touch f2) (touch f1)))
  (check-equal? #f (equal? (current-future) (touch f1))) 
  (check-equal? #f (equal? (current-future) (touch f2))))

;Preceding current-future with an obvious blocking call
(let ([f1 (future (λ () (sleep 1) (current-future)))]
      [f2 (future (λ () (sleep 1) (current-future)))]) 
  (check-equal? #t (equal? f1 (touch f1))) 
  (check-equal? #f (equal? f2 (touch f1)))
  (check-equal? #t (equal? f2 (touch f2)))
  (check-equal? #f (equal? (touch f2) (touch f1)))
  (check-equal? #f (equal? (current-future) (touch f1))) 
  (check-equal? #f (equal? (current-future) (touch f2))))            
              
(let* ([fs (build-list 20 (λ (n) (future (λ () (current-future)))))]
       [retvalfs (map touch fs)]) 
  (check-equal? 20 (length (remove-duplicates retvalfs))))

;; Check `current-future' more, specially trying to get
;; the runtime thread to nest `touch'es:
(let loop ([i 20][f (future (lambda () (current-future)))])
  (if (zero? i)
      (check-equal? f (touch f))
      (loop (sub1 i)
            (future (lambda ()
                      (and (eq? (touch f) f)
                           (current-future)))))))

;Future semaphore tests 
(let* ([m1 (make-fsemaphore 1)] 
       [m2 (make-fsemaphore 0)] 
       [x 2] 
       [lst '()] 
       [rack-sema (make-semaphore 1)]
       [f (future (λ () 
                   (fsemaphore? m2)))]) 
  (check-equal? #t (fsemaphore? m1)) 
  (check-equal? #t (fsemaphore? m2)) 
  (check-equal? #f (fsemaphore? x)) 
  (check-equal? #f (fsemaphore? lst)) 
  (check-equal? #f (fsemaphore? rack-sema)) 
  (check-equal? #t (touch f)))

(let ([m (make-fsemaphore 1)]) 
  (fsemaphore-wait m) 
  (check-equal? 0 (fsemaphore-count m)))

(let ([m (make-fsemaphore 0)]) 
  (fsemaphore-post m) 
  (fsemaphore-wait m)
  (check-equal? 0 (fsemaphore-count m)))

(let ([m (make-fsemaphore 37)]) 
  (check-equal? 37 (fsemaphore-count m))) 

(let ([m (make-fsemaphore 37)]) 
  (fsemaphore-wait m) 
  (fsemaphore-wait m)
  (fsemaphore-post m) 
  (fsemaphore-wait m) 
  (check-equal? 35 (fsemaphore-count m)))

(let ([m1 (make-fsemaphore 38)] 
      [m2 (make-fsemaphore 0)]) 
  (check-equal? #t (fsemaphore-try-wait? m1)) 
  (check-equal? #f (fsemaphore-try-wait? m2)))

(let* ([m1 (make-fsemaphore 20)] 
       [m2 (make-fsemaphore 0)] 
       [f1 (future (λ () 
                    (fsemaphore-try-wait? m2)))] 
       [f2 (future (λ () 
                    (fsemaphore-try-wait? m1)))]) 
  (check-equal? #f (touch f1)) 
  (check-equal? #t (touch f2)))

;Test fsemaphore wait on a future thread 
;(here the future thread should be able to capture the cont. locally)
(let* ([m (make-fsemaphore 0)] 
       [f (future (λ () 
                    (let ([l (cons 1 2)]) 
                      (for ([i (in-range 0 10000)]) 
                        (set! l (cons i l))) 
                      (fsemaphore-wait m) 
                      l)))]) 
  (sleep 3) 
  (fsemaphore-post m) 
  (touch f)
  (check-equal? 0 (fsemaphore-count m)))

;The f1 future should never terminate 
(printf "test n-4~n")
(let* ([m (make-fsemaphore 0)]
       [dummy 5]
       [f1 (future (λ () (fsemaphore-wait m) (set! dummy 42)))] 
       [f2 (future (λ () 88))]) 
  (check-equal? 88 (touch f2)) 
  (sleep 3)
  (check-equal? 0 (fsemaphore-count m))
  (check-equal? 5 dummy))

(printf "test n-3~n")
(let* ([m (make-fsemaphore 0)]
       [dummy 5]
       [f1 (future (λ () 
                     (fsemaphore-wait m) 
                     (set! dummy 42) 
                     dummy))]
       [f2 (future (λ () 
                     (fsemaphore-post m) 
                     #t))])
  (sleep 2) 
  (check-equal? 42 (touch f1)) 
  (check-equal? 0 (fsemaphore-count m)))

(printf "test n-2~n")
(let* ([m1 (make-fsemaphore 0)] 
       [m2 (make-fsemaphore 0)] 
       [dummy 8]
       [f1 (future (λ () 
                     (fsemaphore-wait m2)
                     (set! dummy 10)
                     (fsemaphore-post m1) 
                     #t))] 
       [f2 (future (λ () 
                     (fsemaphore-post m2) 
                     (fsemaphore-wait m1) 
                     (set! dummy (add1 dummy)) 
                     dummy))]) 
  (check-equal? 11 (touch f2)))

(printf "test n-1~n")
(let* ([m (make-fsemaphore 0)] 
       [f1 (future (λ () 
                     (sleep 1) 
                     (fsemaphore-wait m) 
                     5))]) 
  (fsemaphore-post m)
  (check-equal? 5 (touch f1)))

;Test fsemaphore ops after blocking runtime call 
;Here one future will invoke fsemaphore-wait within the context 
;of a touch.  Meanwhile, another future is allocating (requiring 
;the help of the runtime thread which is also "blocked" waiting 
;for the semaphore to become ready.
(printf "test n~n")
(let* ([m (make-fsemaphore 0)] 
       [f1 (future (λ () 
                     (sleep 1) ;Currently a blocking RT call 
                     (fsemaphore-wait m)))] 
       [f2 (future (λ () 
                     (let* ([lst '()]
                            [retval (let loop ([index 10000] [l lst]) 
                                      (cond 
                                        [(zero? index) l] 
                                        [else 
                                         (loop (sub1 index) (cons index l))]))]) 
                       (fsemaphore-post m) 
                       (car retval))))])    
  (sleep 3)
  (touch f1)
  (check-equal? 1 (touch f2)))

;; Make sure that `future' doesn't mishandle functions
;; that aren't be JITted:
(check-equal?
 (for/list ([i (in-range 10)]) (void))
 (map
  touch
  (for/list ([i (in-range 10)])
    (if (even? i)
        (future void)
        (future (parameterize ([eval-jit-enabled #f])
                  (eval #'(lambda () (void)))))))))
