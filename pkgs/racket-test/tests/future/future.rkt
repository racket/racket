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

;Tests specific to would-be-future 
(define-struct future-event (future-id process-id what time prim-name target-fid) 
    #:prefab)

(define (get-events-of-type type log) 
    (filter (λ (e) 
              (equal? (future-event-what e) type)) 
            log))

(define (get-blocks log)
    (get-events-of-type 'block log))
  
  (define (get-touch-blocks log) 
    (get-events-of-type 'touch log))
  
  (define (get-blocks-on prim log) 
    (filter (λ (e) 
              (equal? (future-event-prim-name e) 
                      prim)) 
            (get-blocks log)))

(when (futures-enabled?)
  (define recv (make-log-receiver (current-logger) 'debug)) 
  (define RUNTIME-THREAD 0)  
  (define (raw-log-output)
    (let* ([info (sync/timeout 0 recv)]) 
      (if info 
          (let ([v (vector-ref info 2)]) 
            (if (future-event? v) 
                (cons v (raw-log-output)) 
                (raw-log-output))) 
          empty)))
  
  (let ([f (would-be-future (λ () 
                              (printf "hello1") 
                              (printf "hello2") 
                              (printf "hello3")))]) 
    (touch f) 
    (let ([log (raw-log-output)])
      (check-equal? 3 (length (get-blocks log))) 
      (check-equal? 3 (length (get-blocks-on 'printf log))) 
      (check-equal? 0 (length (get-touch-blocks log)))))
  
  (let ([f1 (would-be-future 
             (λ () 
               (printf "hello1") 
               (printf "hello ~a\n" 
                       (touch 
                        (would-be-future (λ () 
                                           (printf "hello2") 
                                           (printf "hello3") 
                                           42))))))]) 
    (touch f1) 
    (let ([log (raw-log-output)])
      (check-equal? 5 (length (get-blocks log))) 
      (check-equal? 1 (length (get-touch-blocks log)))
      (check-equal? 4 (length (get-blocks-on 'printf log))) 
      (check-equal? 1 (length (get-blocks-on 'would-be-future log))))))

;; ----------------------------------------

(define (run-tests func) 
  (check-equal?
   'yes
   (let/ec k
     (call-with-exception-handler
      (lambda (exn)
        (k (continuation-mark-set-first #f 'special)))
      (lambda ()
        (touch
         (func
          (lambda ()
            (with-continuation-mark
                'special 'yes
              (set-box! 1 1)))))))))

  (check-equal?
   'yes
   (let/ec k
     (call-with-exception-handler
      (lambda (exn)
        (k (continuation-mark-set-first #f 'special)))
      (lambda ()
        (touch
         (func
          (lambda ()
            (with-continuation-mark
                'special 'yes
              (vector-ref (chaperone-vector
                           (vector 1)
                           (lambda (vec i val) 2)
                           (lambda (vec i val) val))
                          0)))))))))

;; ----------------------------------------

  (check-equal?
   #f
   (touch
    (func
     (lambda ()
       (continuation-mark-set-first
        #f
        'key)))))

  (check-equal?
   'an-arbitrary-value
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'key 'an-arbitrary-value
         (continuation-mark-set-first
          #f
          'key))))))

  (check-equal?
   'an-arbitrary-value
   (let ([f (func
             (lambda ()
               (continuation-mark-set-first
                #f
                'key)))])
     (with-continuation-mark
         'key 'an-arbitrary-value
       (touch f))))
  
  (check-equal?
   #f
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'key 'an-arbitrary-value
         (continuation-mark-set-first
          #f
          'other-key))))))

  (check-equal?
   'another-value
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'key 'an-arbitrary-value
         (with-continuation-mark
             'other-key 'another-value
           (continuation-mark-set-first
            #f
            'other-key)))))))

  (check-equal?
   'an-arbitrary-value
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'key 'an-arbitrary-value
         (with-continuation-mark
             'other-key 'another-value
           (continuation-mark-set-first
            #f
            'key)))))))

  (check-equal?
   'an-arbitrary-value
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'key 'an-arbitrary-value
         (values
          (with-continuation-mark
              'other-key 'another-value
            (continuation-mark-set-first
             #f
             'key))))))))

  (check-equal?
   1
   (touch
    (func
     (lambda ()
       (let nt-loop ([x 100])
         (if (zero? x)
             (continuation-mark-set-first
              #f
              'key)
             (values
              (with-continuation-mark
                  'key x
                (nt-loop (sub1 x))))))))))
  
  (check-equal?
   77
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'deep-key 77
         (let nt-loop ([x 100])
           (if (zero? x)
               (continuation-mark-set-first
                #f
                'deep-key)
               (values
                (with-continuation-mark
                    'key x
                  (nt-loop (sub1 x)))))))))))
  
  (check-equal?
   77
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'early-key 77
         (let nt-loop ([x 100])
           (if (zero? x)
               (continuation-mark-set-first
                #f
                'early-key)
               (with-continuation-mark
                   x (sqrt x)
                 (nt-loop (sub1 x))))))))))
  
  (check-equal?
   1050
   (touch
    (func
     (lambda ()
       (with-continuation-mark
           'early-key 77
         (let nt-loop ([x 100])
           (if (zero? x)
               (continuation-mark-set-first
                #f
                50)
               (with-continuation-mark
                   x (+ 1000 x)
                 (nt-loop (sub1 x))))))))))

;; ----------------------------------------
  
  (check-equal? 2 
                (touch (func (λ () 2))))
  
  (let ([f1 (func (λ () (+ 2 2)))] 
        [f2 (func (λ () (+ 5 3)))]) 
    (check-equal? 12 (+ (touch f2) (touch f1))))
  
  (let* ([v 5]
         [f1 (func (λ () 
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
         [f1 (func (λ () 
                       (build-rand-list nums 10)))])
    (set! nums (touch f1)) 
    (check-equal? 20 (length (touch (func (λ () (build-rand-list nums 10)))))))
  
  (let* ([f1 (func (λ () 
                       (build-rand-list '() 20)))] 
         [f2 (func (λ () (length (touch f1))))]) 
    (check-equal? 20 (touch f2)))
  
  (check-equal? 50000
                (let ([fts (for/list ([i (in-range 0 10000)]) 
                             (func (λ () (build-rand-list '() 5))))]) 
                  (length (append-list-of-lists '() (map touch fts)))))
  
  (check-equal? 31 
                (let* ([f1 (func (λ () (foldl + 0 '(1 2 3 4 5))))] 
                       [f2 (func (λ () (+ (touch 
                                             (func (λ () 
                                                       (+ 6 
                                                          (touch f1))))) 
                                            10)))]) 
                  (touch f2)))
  
  (check-equal? 30000
                (let ([fts (for/list ([i (in-range 0 100)]) 
                             (func (λ () 
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
   (let ([f1 (func (λ () (sum-to 0 20000)))] 
         [f2 (func (λ () (sum-to 0 20000)))] 
         [f3 (func (λ () (sum-to 0 20000)))]) 
     (+ (+ (touch f3) (touch f1)) (touch f2))))
  
  (check-equal? 
   #t 
   (let* ( [f1 (func (λ () (build-rand-list '() 10000)))]
           [f2 (func (λ () 
                         (foldl (λ (a b) 
                                  (* a b)) 
                                1 
                                (touch f1))))] 
           [f3 (func (λ () (< (touch f2) 1)))]) 
     (touch f3)))
  
  (check-equal?
   '((1) (1))
   (let ([f1 (func (lambda ()
                       (with-continuation-mark
                           'x 1
                         (current-continuation-marks))))]
         [f2 (func (lambda ()
                       (with-continuation-mark
                           'x 1
                         (current-continuation-marks))))])
     (sleep 0.1)
     (list (continuation-mark-set->list (touch f1) 'x)
           (continuation-mark-set->list (touch f2) 'x))))
  
  (check-equal?
   '((1 0) (1 0))
   (let ([f1 (func (lambda ()
                       (with-continuation-mark
                           'x 1
                         (current-continuation-marks))))]
         [f2 (func (lambda ()
                       (with-continuation-mark
                           'x 1
                         (current-continuation-marks))))])
     (with-continuation-mark
         'x 0
       (list (continuation-mark-set->list (touch f1) 'x)
             (continuation-mark-set->list (touch f2) 'x)))))
  
  (check-equal?
   '((1 0) (1) ())
   (let ([f1 (func (lambda ()
                       (with-continuation-mark
                           'x 1
                         (current-continuation-marks))))]
         [f2 (func (lambda ()
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
  
  (let ([f (func (λ () (current-future)))]) 
    (check-equal? #t (equal? f (touch f))))
  
  ;Where futures might be touched before ever making it 
  ;to a worker kernel thread
  (let ([f1 (func (λ () (current-future)))]
        [f2 (func (λ () (current-future)))]) 
    (check-equal? #t (equal? f1 (touch f1))) 
    (check-equal? #f (equal? f2 (touch f1)))
    (check-equal? #t (equal? f2 (touch f2)))
    (check-equal? #f (equal? (touch f2) (touch f1)))
    (check-equal? #f (equal? (current-future) (touch f1))) 
    (check-equal? #f (equal? (current-future) (touch f2))))
  
  ;Where futures are pretty much guaranteed to be running 
  ;on a worker thread
  (let ([f1 (func (λ () (current-future)))]
        [f2 (func (λ () (current-future)))]) 
    (sleep 0.1)
    (check-equal? #t (equal? f1 (touch f1))) 
    (check-equal? #f (equal? f2 (touch f1)))
    (check-equal? #t (equal? f2 (touch f2)))
    (check-equal? #f (equal? (touch f2) (touch f1)))
    (check-equal? #f (equal? (current-future) (touch f1))) 
    (check-equal? #f (equal? (current-future) (touch f2))))
  
  ;Preceding current-future with an obvious blocking call
  (let ([f1 (func (λ () (sleep 1) (current-future)))]
        [f2 (func (λ () (sleep 1) (current-future)))]) 
    (check-equal? #t (equal? f1 (touch f1))) 
    (check-equal? #f (equal? f2 (touch f1)))
    (check-equal? #t (equal? f2 (touch f2)))
    (check-equal? #f (equal? (touch f2) (touch f1)))
    (check-equal? #f (equal? (current-future) (touch f1))) 
    (check-equal? #f (equal? (current-future) (touch f2))))            
  
  (let* ([fs (build-list 20 (λ (n) (func (λ () (current-future)))))]
         [retvalfs (map touch fs)]) 
    (check-equal? 20 (length (remove-duplicates retvalfs))))
  
  ;; Check `current-future' more, specially trying to get
  ;; the runtime thread to nest `touch'es:
  (let loop ([i 20][f (func (lambda () (current-future)))])
    (if (zero? i)
        (check-equal? f (touch f))
        (loop (sub1 i)
              (func (lambda ()
                        (and (eq? (touch f) f)
                             (current-future)))))))
  
  ;Future semaphore tests 
  (let* ([m1 (make-fsemaphore 1)] 
         [m2 (make-fsemaphore 0)] 
         [x 2] 
         [lst '()] 
         [rack-sema (make-semaphore 1)]
         [f (func (λ () 
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
  
  ;Test for errors when passing bad arguments 
  (check-exn exn:fail:contract? (λ () (make-fsemaphore -1)))
  (check-exn exn:fail:contract? (λ () (make-fsemaphore (cons "a" "b"))))
  (check-exn exn:fail:contract? (λ () (fsemaphore-count (cons "foo" "goo"))))
  (check-exn exn:fail:contract? (λ () (fsemaphore-post (cons 1 2))))
  (check-exn exn:fail:contract? (λ () (fsemaphore-wait (cons 1 2)))) 
  (check-exn exn:fail:contract? (λ () (fsemaphore-try-wait? (cons 1 2))))
  
  (check-exn exn:fail:contract? (λ () 
                                  (let ([f (func (λ () 
                                                     (make-fsemaphore (cons "go" 
                                                                            "mavs"))))]) 
                                    (sleep 0.5) 
                                    (touch f))))
  
  (check-exn exn:fail:contract? (λ () 
                                  (let ([f (func (λ () 
                                                     (make-fsemaphore -1)))]) 
                                    (sleep 0.5) 
                                    (touch f))))
  
  (let ([f (func (λ () 
                     (fsemaphore-post 33)))]) 
    (sleep 0.5) 
    (check-exn exn:fail? (λ () (touch f))))
  
  (let ([f (func (λ () 
                     (fsemaphore-count 33)))]) 
    (sleep 0.5) 
    (check-exn exn:fail? (λ () (touch f))))
  
  (let ([f (func (λ () 
                     (fsemaphore-wait 33)))]) 
    (sleep 0.5) 
    (check-exn exn:fail? (λ () (touch f))))
  
  (let ([f (func (λ () 
                     (fsemaphore-try-wait? 33)))]) 
    (sleep 0.5) 
    (check-exn exn:fail? (λ () (touch f))))
  
  ;try-wait
  (let ([m1 (make-fsemaphore 20)] 
        [m2 (make-fsemaphore 0)]) 
    (check-equal? #t (fsemaphore-try-wait? m1)) 
    (check-equal? #f (fsemaphore-try-wait? m2)))
  
  (let* ([m1 (make-fsemaphore 20)] 
         [m2 (make-fsemaphore 0)] 
         [f1 (func (λ () 
                       (fsemaphore-try-wait? m2)))] 
         [f2 (func (λ () 
                       (fsemaphore-try-wait? m1)))]) 
    (sleep 0.5)
    (check-equal? #f (touch f1)) 
    (check-equal? #t (touch f2)))
  
  (let ([m (make-fsemaphore 3)]) 
    (fsemaphore-try-wait? m) 
    (check-equal? 2 (fsemaphore-count m))) 
  
  (let* ([m (make-fsemaphore 0)] 
         [f (func (λ () 
                      (fsemaphore-post m) 
                      42))]) 
    (sleep 0.5) 
    (fsemaphore-try-wait? m) 
    (check-equal? 0 (fsemaphore-count m)))
  
  ;Test fsemaphore wait on a future thread 
  ;(here the future thread should be able to capture the cont. locally)
  (let* ([m (make-fsemaphore 0)] 
         [f (func (λ () 
                      (let ([l (cons 1 2)]) 
                        (for ([i (in-range 0 10000)]) 
                          (set! l (cons i l))) 
                        (fsemaphore-wait m) 
                        l)))]) 
    (sleep 3) 
    (fsemaphore-post m) 
    (touch f)
    (check-equal? 0 (fsemaphore-count m)))
  
  (let* ([m (make-fsemaphore 0)]
         [dummy 5]
         [f1 (func (λ () (fsemaphore-wait m) (set! dummy 42)))] 
         [f2 (func (λ () 88))]) 
    (check-equal? 88 (touch f2)) 
    (sleep 1)
    (check-equal? 0 (fsemaphore-count m))
    (check-equal? 5 dummy)
    (fsemaphore-post m) 
    (touch f1)
    (check-equal? 42 dummy))
  
  (let* ([m (make-fsemaphore 0)]
         [dummy 5]
         [f1 (func (λ () 
                       (fsemaphore-wait m) 
                       (set! dummy 42) 
                       dummy))]
         [f2 (func (λ () 
                       (fsemaphore-post m) 
                       #t))])
    (sleep 1) 
    (check-equal? #t (touch f2))
    (check-equal? 42 (touch f1)) 
    (check-equal? 0 (fsemaphore-count m)))
  
  (let* ([m1 (make-fsemaphore 0)] 
         [m2 (make-fsemaphore 0)] 
         [dummy 8]
         [f1 (func (λ () 
                       (fsemaphore-wait m2)
                       (set! dummy 10)
                       (fsemaphore-post m1) 
                       #t))] 
         [f2 (func (λ () 
                       (fsemaphore-post m2) 
                       (touch f1)
                       (fsemaphore-wait m1) 
                       (set! dummy (add1 dummy)) 
                       dummy))]) 
    (check-equal? 11 (touch f2)))
  
  (let* ([m (make-fsemaphore 0)] 
         [f1 (func (λ () 
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
  (let* ([m (make-fsemaphore 0)] 
         [f1 (func (λ () 
                       (sleep 1) ;Currently a blocking RT call 
                       (fsemaphore-wait m)))] 
         [f2 (func (λ () 
                       (let* ([lst '()]
                              [retval (let loop ([index 100000] [l lst]) 
                                        (cond 
                                          [(zero? index) l] 
                                          [else 
                                           (loop (sub1 index) (cons index l))]))]) 
                         (fsemaphore-post m) 
                         (car retval))))])    
    (sleep 1)
    (thread (lambda () (touch f1)))
    (check-equal? 1 (touch f2)))
  
  (let* ([m (make-fsemaphore 0)] 
         [f1 (func (λ () 
                       (fsemaphore-wait m)
                       42))] 
         [f2 (func (λ () 
                       (fsemaphore-wait m) 
                       99))]) 
    ;sleep to ensure that both futures will queue up waiting for the fsema 
    (sleep 1) 
    (fsemaphore-post m) 
    (fsemaphore-post m)
    (check-equal? 42 (touch f1)) 
    (check-equal? 99 (touch f2)))
  
  (let* ([m (make-fsemaphore 0)]
         [fs (for/list ([i (in-range 0 19)]) 
               (func (λ () 
                         (fsemaphore-wait m) 
                         i)))]) 
    (sleep 1)
    (for ([i (in-range 0 19)]) 
      (fsemaphore-post m)) 
    (check-equal? 171 (foldl (λ (f acc) 
                               (+ (touch f) acc)) 
                             0 
                             fs)))
  
  ;; Make sure that `future' doesn't mishandle functions
  ;; that aren't be JITted:
  (check-equal?
   (for/list ([i (in-range 10)]) (void))
   (map
    touch
    (for/list ([i (in-range 10)])
      (if (even? i)
          (func void)
          (func (parameterize ([eval-jit-enabled #f])
                    (eval #'(lambda () (void)))))))))
  
  ;; A future shouldn't use up a background thread if its
  ;; starting thread's custodian is shut down:
  (let ()
    (define f #f)
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (sync (thread (lambda ()
                      (set! f (func (lambda ()
                                        (let loop () (loop)))))))))
    (sleep 0.1)
    (custodian-shutdown-all c))
  
  ;; If a future is suspended via a custodian, it should still
  ;; work to touch it:
  (let ()
    (define f #f)
    (define s (make-fsemaphore 0))
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (sync (thread (lambda ()
                      (set! f (func (lambda ()
                                        (fsemaphore-wait s)
                                        10)))))))
    (sleep 0.1)
    (custodian-shutdown-all c)
    (fsemaphore-post s)
    (check-equal? 10 (touch f)))
  
  
  ;; Start a future in a custodian-suspended future:
  (let ()
    (define f #f)
    (define s (make-fsemaphore 0))
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (sync (thread (lambda ()
                      (set! f (func (lambda ()
                                        (fsemaphore-wait s)
                                        (func
                                         (lambda ()
                                           11)))))))))
    (sleep 0.1)
    (custodian-shutdown-all c)
    (fsemaphore-post s)
    (check-equal? 11 (touch (touch f))))
  
  ;; Don't get stuck on a bunch of futures that
  ;; have been disabled:
  (let ()
    (define c (make-custodian))
    (define (loop) (loop))
    (parameterize ([current-custodian c])
      (sync (thread (lambda ()
                      (for ([i (in-range 100)])
                        (func loop))))))
    (sleep 0.1)
    (custodian-shutdown-all c)
    (sleep 0.1))
  
  ;; Stress test:
  (for-each
   (lambda (v) (check-equal? 10 (touch (touch v))))
   (for/list ([i (in-range 10000)])
     (func (lambda () (func (lambda () 10))))))
  
  ;; Stress test:
  (check-equal?
   0
   (touch
    (for/fold ([t (func (lambda () 0))]) ([i (in-range 10000)])
      (func (lambda () (touch t))))))  

  ;; box-cas failure tests
  (let ()
    (define (f x) (box-cas! x 1 2))    
    (define (g x y) y)

    (define b (box 1))
    
    (check-equal? (with-handlers ([exn:fail? (lambda _ 'num)]) 
                    (touch (future (lambda () (f 2))))) 
                  'num)
    (check-equal? (with-handlers ([exn:fail? (lambda _ 'list)]) 
                    (touch (future (lambda () (f (list 1))))))
                  'list)
    
    (check-equal? (with-handlers ([exn:fail? (lambda _ 'chap)]) 
                    (touch (future (lambda () (f (chaperone-box b g g))))))
                  'chap)
    
    (check-equal? (with-handlers ([exn:fail? (lambda _ 'imp)]) 
                    (touch (future (lambda () (f (impersonate-box b g g))))))
                  'imp)
    (check-equal? (unbox b) 1))

  (let ()
    (define b (box 0))
    ;; inc and dec, with retry loops
    (define (inc) 
      (let loop ()
	(define cur (unbox b))
	(unless (box-cas! b cur (+ cur 1))
	  (loop))))
    (define (dec) 
      (let loop ()
	(define cur (unbox b))
	(unless (box-cas! b cur (- cur 1))
	  (loop))))
    (define (inc-dec-loop)
      (for ([i (in-range 100000000)])
	   (inc)
	   (dec)))
    (define t1 (func inc-dec-loop))
    (define t2 (func inc-dec-loop))
    (touch t1)
    (touch t2)
    (check-equal? (unbox b) 0))

  (let ()
    (define b1 (box #true))
    (define (neg-bad)
      (let loop ()
        (unless (box-cas! b1 #true #false)
          (unless (box-cas! b1 #false #true)
            (loop)))))
    (define b2 (box #true))
    (define (neg-good)
      (unless (box-cas! b2 #true #false)
        (box-cas! b2 #false #true)))

    (check-equal? (unbox b1) #true)
    (neg-bad)
    (check-equal? (unbox b1) #false)
    (neg-bad)
    (check-equal? (unbox b1) #true)

    (check-equal? (unbox b2) #true)
    (neg-good)
    (check-equal? (unbox b2) #false)
    (neg-good)
    (check-equal? (unbox b2) #true))

  ;; check handling of marks in stack-overflow handling:
  (let ()
    (define (maybe-add1 n)
      (if (number? n)
          (add1 n)
          n))
    (define (loop n)
      (if (zero? n)
          (continuation-mark-set->list (current-continuation-marks) 'x)
          (with-continuation-mark
              'x
              n
            (maybe-add1 (loop (sub1 n))))))
    (define f (func (lambda () (loop 10000))))
    (check-equal? 10000 (length (touch f))))

  ;; check arity error in overflow handling:
  (let ()
    (define (loop n)
      (if (zero? n)
          (values 0 0)
          (add1 (loop (sub1 n)))))
    
    (for ([i (in-range 1340 1320 -1)])
      (define f (func (lambda () (loop i))))
      (sleep 0.1)
      (with-handlers ([exn:fail? (lambda (exn)
                                   (unless (regexp-match #rx"expected number of values not received" (exn-message exn))
                                     (raise exn)))])
        (touch f))))

  ;; check list-refs
  (let ()
    (define l (build-list 100000 add1))
    (check-equal?
     (map touch
          (for/list ([i 10])
            (func (lambda () (list-ref l 50000)))))
     (for/list ([i 10]) 50001)))
  
  ;Basic odd?/even? tests
  (let ([fa (func (λ () 
                      (and (odd? 33) (odd? 103.0))))] 
        [fb (func (λ ()
                      (or (odd? 32) (odd? 102.0))))]
        [fc (func (λ () 
                      (and (even? 32) (even? 32.0))))] 
        [fd (func (λ () 
                      (or (even? 33) (even? 103.0))))]) 
    (sleep 0.2) 
    (check-true (touch fa)) 
    (check-false (touch fb)) 
    (check-true (touch fc)) 
    (check-false (touch fd)))
  
  ;Make sure we don't crash in error cases for odd?/even?
  (let ([fa (func (λ () 
                      (even? 43.33)))]
        [fb (func (λ () 
                      (odd? 7.0+3.2i)))] 
        [fc (func (λ () 
                      (even? -inf.0)))]
        [fd (func (λ () 
                      (odd? +inf.0)))])
    (sleep 0.2) 
    (check-exn exn:fail:contract? (λ () (touch fa)))
    (check-exn exn:fail:contract? (λ () (touch fb)))
    (check-exn exn:fail:contract? (λ () (touch fc)))
    (check-exn exn:fail:contract? (λ () (touch fd))))

  ;; Check rest args
  (let ([g #f])
    (set! g (lambda x x))
    
    (define f (for/list ([i 10])
                (future (lambda () (g 1 2 3)))))
    (check-true (andmap (lambda (v) (equal? '(1 2 3) v))
                        (for/list ([f (in-list f)]) (touch f)))))
    
  )

(run-tests future)
(run-tests would-be-future)

;; ----------------------------------------

(module+ test
  (module config info
    (define random? #t)))
