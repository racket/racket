
(let ([try
       (lambda (thread m-top n-top do-mid-stream do-abort)
         (let ([result #f])
           (thread-wait
            (thread
             (lambda ()
               (set! result
                     (let pre-loop ([m m-top])
                       (if (zero? m)
                           (list
                            (do-mid-stream
                             (lambda () 
                               (call-with-continuation-prompt
                                (lambda ()
                                  (let loop ([n n-top])
                                    (if (zero? n)
                                        (do-abort
                                         (lambda ()
                                           (abort-current-continuation
                                            (default-continuation-prompt-tag)
                                            (lambda () 5000))))
                                        (+ (loop (sub1 n))))))))))
                           (list (car (pre-loop (sub1 m))))))))))
           (test '(5000) values result)))])
  (try thread 5000 10000 (lambda (mid) (mid)) 
       (lambda (abort) (((call/cc
                          (lambda (k) (lambda () k))))
                        (lambda () (lambda (x) 5000))))))

(test-breaks-ok)

;;----------------------------------------
;; Prompt escapes

;; Simple return
(test 10 call-with-continuation-prompt 
      (lambda () 10))
(test-values '(10 11) (lambda ()
                        (call-with-continuation-prompt 
                         (lambda () (values 10 11)))))
(test-values '() (lambda ()
                   (call-with-continuation-prompt 
                    (lambda () (values)))))

;; Aborts
(test 11 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11))
      (default-continuation-prompt-tag)
      values)
(test 11 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  (lambda () 11))))
(test 12 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  12))
      (default-continuation-prompt-tag)
      values)
(test 12 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  (lambda () 12)))
      (default-continuation-prompt-tag))
(test-values '(11 12)
             (lambda ()
               (call-with-continuation-prompt 
                (lambda () (abort-current-continuation
                            (default-continuation-prompt-tag)
                            11
                            12))
                (default-continuation-prompt-tag)
                values)))
(test-values '(11 12)
             (lambda ()
               (call-with-continuation-prompt 
                (lambda () (abort-current-continuation
                            (default-continuation-prompt-tag)
                            (lambda () (values 11
                                               12)))))))
(test 8 call-with-continuation-prompt 
      (lambda () (+ 17
                    (abort-current-continuation
                     (default-continuation-prompt-tag)
                     (lambda () 8)))))
(test 81 call-with-continuation-prompt 
      (lambda () (+ 17
                    (call-with-continuation-prompt 
                     (lambda ()
                       (abort-current-continuation
                        (default-continuation-prompt-tag)
                        (lambda () 81)))
                     (make-continuation-prompt-tag)))))
(let ([p (make-continuation-prompt-tag)])
  (test 810 call-with-continuation-prompt 
        (lambda () (+ 17
                      (call-with-continuation-prompt 
                       (lambda ()
                         (abort-current-continuation
                          p
                          810))
                       (make-continuation-prompt-tag))))
        p
        values))

;; Aborts with handler
(test 110 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11))
      (default-continuation-prompt-tag)
      (lambda (x) (* x 10)))
(test 23
      call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11
                  12))
      (default-continuation-prompt-tag)
      (lambda (x y) (+ x y)))
;; Handler in tail position:
(test '(11 12 17)
      'handler-in-tail-position
      (with-continuation-mark
          'x 16
        (call-with-continuation-prompt 
         (lambda () (abort-current-continuation
                     (default-continuation-prompt-tag)
                     11
                     12))
         (default-continuation-prompt-tag)
         (lambda (x y) 
           (with-continuation-mark
               'x 17
             (list* x y
                    (continuation-mark-set->list 
                     (current-continuation-marks)
                     'x)))))))

(test-breaks-ok)

;; Abort to a prompt in a d-w post that is deeper than a
;;  prompt with the same tag at the continuation-jump site:
(test 0
      values
      (let ([p1 (make-continuation-prompt-tag)]
            [p2 (make-continuation-prompt-tag)])
        (let/cc k
          (call-with-continuation-prompt
           (lambda ()
             (call-with-continuation-prompt
              (lambda ()
                (dynamic-wind
                    void
                    (lambda ()
                      (call-with-continuation-prompt
                       (lambda ()
                         (k 0))
                       p2))
                    (lambda ()
                      (abort-current-continuation p1 (lambda () 0)))))
              p1))
           p2))))

;; ----------------------------------------
;; Continuations

(with-cc-variants
 (test -17
       call-with-continuation-prompt
       (lambda () -17)))

(with-cc-variants
 (test 17
       call-with-continuation-prompt
       (lambda ()
         (let/cc k
           (k 17)))))

(test-breaks-ok)

(with-cc-variants
 (test 29
       'in-other-prompt1
       (let ([retry #f])
         (test 35
               call-with-continuation-prompt
               (lambda ()
                 (+ 18
                    (let/cc k
                      (set! retry k)
                      17))))
         (+ 1 (call-with-continuation-prompt
               (lambda ()
                 (retry 10)))))))

(with-cc-variants
 (test 60
       'in-other-prompt2
       (let ([retry #f])
         (test 35
               call-with-continuation-prompt
               (lambda ()
                 (+ 18
                    (let/cc k
                      (set! retry k)
                      17))))
         (+ 1 (call-with-continuation-prompt
               (lambda ()
                 (+ (call-with-continuation-prompt
                     (lambda ()
                       (retry 12)))
                    (call-with-continuation-prompt
                     (lambda ()
                       (retry 11))))))))))

(with-cc-variants
 (test '(#f #t)
       'in-other-thread1
       (let ([retry #f]
             [result #f]
             [did? #f])
         (call-with-continuation-prompt
          (lambda ()
            (+ 18
               (begin0
                (let/cc k
                  (set! retry k)
                  17)
                (set! did? #t)))))
         (set! did? #f)
         (thread-wait
          (thread (lambda ()
                    (set! result (retry 0)))))
         (list result did?))))

(with-cc-variants
 (test 18
       'in-other-thread2
       (let ([retry #f]
             [result #f])
         (call-with-continuation-prompt
          (lambda ()
            (+ 18
               (let/cc k
                 (set! retry k)
                 17))))
         (thread-wait
          (thread (lambda ()
                    (set! result 
                          (call-with-continuation-prompt
                           (lambda ()
                             (retry 0)))))))
         result)))

(with-cc-variants
 (test 25
       'back-in-original-thread
       (let ([retry #f]
             [result #f])
         (thread-wait
          (thread
           (lambda ()
             (+ 18
                (let/cc k
                  (set! retry k)
                  17)))))
         (call-with-continuation-prompt
          (lambda ()
            (retry 7))))))

(test-breaks-ok)

;; Catch continuation in composed continuation:
(with-cc-variants
 (test 89
       'catch-composed
       (let ([k (call-with-continuation-prompt
                 (lambda ()
                   ((let/cc k (lambda () k)))))])
         (let ([k2 (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           (car (let/cc k2 (list k2)))))))])
           (call-with-continuation-prompt
            (lambda ()
              (k2 '(89))))))))

;; Grab continuation shallow inside meta-prompt with
;;  delimiting prompt deep in a different meta-prompt.
(with-cc-variants
 (let ([k (call-with-continuation-prompt
           (lambda ()
             ((call/cc
               (lambda (k) (lambda () k))))))])
   (test 10 call-with-continuation-prompt
         (lambda ()
           (let loop ([n 300])
             (if (zero? n)
                 (k (lambda ()
                      (let/cc k2 (k2 10))))
                 (cons n (loop (sub1 n)))))))))

;; Grab continuation deep inside meta-prompt with
;;  delimiting prompt shallow in a different meta-prompt.
(with-cc-variants
 (let ([k (call-with-continuation-prompt
           (lambda ()
             (let loop ([n 12])
               (if (zero? n)
                   ((call/cc
                     (lambda (k) (lambda () k))))
                   (cons 1 (loop (sub1 n)))))))])
   (test '(1 1 1 1 1 1 1 1 1 1 1 1 . 10) call-with-continuation-prompt
         (lambda ()
           ((list-tail k 12)
            (lambda ()
              (let/cc k2 (k2 10))))))))

(test-breaks-ok)

;; ----------------------------------------
;; Overlapping continuations

;; Nested
(let ([p1 (make-continuation-prompt-tag)]
      [p2 (make-continuation-prompt-tag)])
  (let ([k1 #f]
        [k2 #f])
    (test '(p1 p2 100)
          call-with-continuation-prompt
          (lambda ()
            (cons 'p1
                  (call-with-continuation-prompt
                   (lambda ()
                     (cons 'p2
                           ((call/cc
                            (lambda (-k1)
                              (set! k1 -k1)
                              (call/cc (lambda (-k2)
                                         (set! k2 -k2)
                                         (lambda () '(100)))
                                       p2))
                            p1))))
                   p2)))
          p1)
    (err/rt-test (k1) exn:fail:contract:continuation?)
    (err/rt-test (k2) exn:fail:contract:continuation?)
    (err/rt-test (call-with-continuation-prompt 
                  (lambda () (k1))
                  p2)
                 exn:fail:contract:continuation?)
    (err/rt-test (call-with-continuation-prompt 
                  (lambda () (k2))
                  p1)
                 exn:fail:contract:continuation?)
    (test '(p1 p2 101) call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () '(101))))
          p1)
    (test '(p2 102) call-with-continuation-prompt
          (lambda ()
            (k2 (lambda () '(102))))
          p2)
    (test '(p1 p2 102-1) call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () (k2 (lambda () '(102-1))))))
          p1)))

;; Use default tag to catch a meta-continuation of p1.
;; Due to different implementations of the default tag,
;; this test is interesting in the main thread and
;; a sub thread:
(let ()
  (define (go)
    (let ([p1 (make-continuation-prompt-tag)])
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  ((call/cc (lambda (k) (lambda () k))
                            p1)))
                p1)])
        (let ([k2 (list
                   (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           (let/cc k k))))
                    p1))])
          (if (procedure? (car k2))
              ((car k2) 10)
              (test '(10) values k2))))))
  (go)
  (let ([finished #f])
    (thread-wait
     (thread (lambda ()
               (go)
               (set! finished 'finished))))
    (test 'finished values finished)))

;; Use default tag to catch a meta-continuation of p1,
;; then catch continuation again (i.e., loop).
(let ([finished #f])
  (define (go)
    (let ([p1 (make-continuation-prompt-tag)]
          [counter 10])
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  ((call/cc (lambda (k) (lambda () k))
                            p1)))
                p1)])
        (let ([k2 (list
                   (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           ((let/cc k (lambda () k))))))
                    p1))])
          (if (procedure? (car k2))
              ((car k2) (lambda () 
                          (if (zero? counter)
                              10
                              (begin
                                (set! counter (sub1 counter))
                                ((let/cc k (lambda () k)))))))
              (test '(10) values k2))
          (set! finished 'finished)))))
  (go)
  (let ([finished #f])
    (thread-wait
     (thread (lambda ()
               (go)
               (set! finished 'finished))))
    (test 'finished values finished)))

;; ----------------------------------------
;; Composable continuations

(err/rt-test (call-with-continuation-barrier
              ;; When the test is not run in a REPL but is run in the
              ;; main thread, then it should fail without the barrier,
              ;; too.  But we don't have enough control over the test
              ;; environment to assume that.
              (lambda ()
                (call-with-composable-continuation
                 (lambda (x) x))))
             exn:fail:contract:continuation?)

(err/rt-test (call-with-composable-continuation
              (lambda (x) x)
              (make-continuation-prompt-tag 'px))
             exn:fail:contract?)

(let ([k (call-with-continuation-prompt
          (lambda ()
            (call-with-composable-continuation
             (lambda (k) k))))])
  (test 12 k 12)
  (test 13 k (k (k (k 13))))
  (test-values '(12 13) (lambda () (k 12 13))))

(let ([k (call-with-continuation-prompt
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k) (lambda () k))))))])
  (test 12 k (lambda () 12))
  (test-values '(12 13) (lambda () (k (lambda () (values 12 13)))))
  ;; Composition shouldn't introduce a prompt:
  (test 10 call-with-continuation-prompt
        (lambda ()
          (let ([k2 (k (lambda ()
                         (let/cc k2 k2)))])
            (if (procedure? k2)
                (k2 10)
                k2))))
  ;; Escape from composed continuation:
  (let ([p (make-continuation-prompt-tag)])
    (test 8 call-with-continuation-prompt 
          (lambda ()
            (+ 99 (k (lambda () (abort-current-continuation p 8)))))
          p
          values))
  (test 8 call-with-continuation-prompt 
        (lambda ()
          (+ 99 (k (lambda () (abort-current-continuation 
                               (default-continuation-prompt-tag)
                               8)))))
        (default-continuation-prompt-tag)
        values))

;; Etc.
(let ([k1 (call-with-continuation-prompt
           (lambda ()
             ((call-with-composable-continuation
               (lambda (k)
                 (lambda () k))))))]
      [k2 (call-with-continuation-prompt
           (lambda ()
             ((call-with-composable-continuation
               (lambda (k)
                 (lambda () k))))))])
  (test 1000
        call-with-continuation-prompt
        (lambda ()
          (k1 (lambda () (k2 (lambda () 1000))))))
  (test -1000 k1 (lambda () (k2 (lambda () -1000))))
  
  (let ([k3 (call-with-continuation-prompt
             (lambda ()
               (k1 (lambda ()
                     ((call-with-composable-continuation
                       (lambda (k)
                         (lambda () k))))))))])
    (test 1001
          call-with-continuation-prompt
          (lambda ()
            (k3 (lambda () 1001))))
    (test -1001 k3 (lambda () -1001))
    (test 1002
          call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () (k3 (lambda () 1002))))))
    (test -1002 k1 (lambda () (k3 (lambda () -1002)))))
  
  (let ([k4 (call-with-continuation-prompt
             (lambda ()
               (k1
                (lambda ()
                  ((call-with-composable-continuation
                    (lambda (k)
                      (lambda () k))))))))])
    (test -1003 k4 (lambda () -1003)))
    
  (let ([k5 (call-with-continuation-prompt
             (lambda ()
               ((k1
                 (lambda ()
                   (call-with-composable-continuation
                    (lambda (k)
                      (lambda () k))))))))])
    (test -1004 k5 (lambda () -1004))
    
    (let ([k6 (call-with-continuation-prompt
               (lambda ()
                 ((k5
                   (lambda ()
                     (call-with-composable-continuation
                      (lambda (k)
                        (lambda () k))))))))])
      (test -1005 k6 (lambda () -1005))))
    
  (let ([k7 (call-with-continuation-prompt
             (lambda ()
               ((k1
                 (lambda ()
                   ((k1
                     (lambda ()
                       (call-with-composable-continuation
                        (lambda (k)
                          (lambda () (lambda () k))))))))))))])
    (test -1006 k7 (lambda () (lambda () -1006)))
    (test '(-1007) call-with-continuation-prompt
          (lambda ()
            (list (k7 (lambda () (lambda () -1007)))))))
  
  )

;; Check that escape drops the meta-continuation:
(test 0
      'esc
      (let ([p1 (make-continuation-prompt-tag)])
        (let/cc esc
          (let ([k
                 (call-with-continuation-prompt
                  (lambda ()
                    ((call-with-composable-continuation
                      (lambda (k)
                        (lambda () k))
                      p1)))
                  p1)])
            (/ (k (lambda () (esc 0))))))))

;; ----------------------------------------
;; Dynamic wind

(test 89
      'dw
      (let ([k (dynamic-wind
                   void
                   (lambda () (let ([k+e (let/cc k (cons k void))])
                                ((cdr k+e) 89)
                                (car k+e)))
                   void)])
        (let/cc esc
          (k (cons void esc)))))

(let ([l null])
  (let ([k2
         (dynamic-wind
             (lambda () (set! l (cons 'pre0 l)))
             (lambda ()
               (let ([k (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre l)))
                               (lambda () (let ([k (let/cc k k)])
                                            k))
                               (lambda () (set! l (cons 'post l))))))])
                 (test '(post pre pre0) values l)
                 ;; Jump from one to the other:
                 (let ([k2 
                        (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre2 l)))
                               (lambda ()
                                 (dynamic-wind
                                     (lambda () (set! l (cons 'pre3 l)))
                                     (lambda ()
                                       (let/cc k2 (k k2)))
                                     (lambda () (set! l (cons 'post3 l)))))
                               (lambda () (set! l (cons 'post2 l))))))])
                   (test '(post pre post2 post3 pre3 pre2 post pre pre0) values l)
                   k2)))
             (lambda () (set! l (cons 'post0 l))))])
    (test '(post0 post pre post2 post3 pre3 pre2 post pre pre0) values l)
    ;; Restore in context with fewer DWs:
    (test 8 call-with-continuation-prompt (lambda () (k2 8)))
    (test '(post2 post3 pre3 pre2 post0 post pre post2 post3 pre3 pre2 post pre pre0) values l)
    ;; Restore in context with more DWs:
    (set! l null)
    (dynamic-wind
        (lambda () (set! l (cons 'pre4 l)))
        (lambda ()
          (dynamic-wind
              (lambda () (set! l (cons 'pre5 l)))
              (lambda ()
                (call-with-continuation-prompt k2))
              (lambda () (set! l (cons 'post5 l)))))
        (lambda () (set! l (cons 'post4 l))))
    (test '(post4 post5 post2 post3 pre3 pre2 pre5 pre4) values l)))

;; Like the meta-continuation test above, but add a dynamic wind
;;  to be restored in the p1 continuation:
(let ([p1 (make-continuation-prompt-tag)]
      [did #f])
  (let ([k (call-with-continuation-prompt
            (lambda ()
              (dynamic-wind
                  (lambda ()
                    (set! did 'in))
                  (lambda ()
                    ((call/cc (lambda (k) (lambda () k))
                              p1)))
                  (lambda ()
                    (set! did 'out))))
            p1)])
    (set! did #f)
    (let ([k2 (list
               (call-with-continuation-prompt
                (lambda ()
                  (k (lambda ()
                       (test 'in values did)
                       ((let/cc k (lambda () k))))))
                p1))])
      (test 'out values did)
      (if (procedure? (car k2))
          ((car k2) (lambda ()
                      (test 'in values did)
                      10))
          (test '(10) values k2)))))

;; Composable continuations
(let ([l null])
  (let ([k2
         (dynamic-wind
             (lambda () (set! l (cons 'pre0 l)))
             (lambda ()
               (let ([k (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre l)))
                               (lambda () 
                                 ((call-with-composable-continuation
                                   (lambda (k) 
                                     (lambda () k)))))
                               (lambda () (set! l (cons 'post l))))))])
                 (test '(post pre pre0) values l)
                 (test 12 k (lambda () 12))
                 (test '(post pre post pre pre0) values l)
                 k))
             (lambda () (set! l (cons 'post0 l))))])
    (test '(post0 post pre post pre pre0) values l)
    (test 73 k2 (lambda () 73))
    (test '(post pre post0 post pre post pre pre0) values l)
    (set! l null)    
    ;; Add d-w inside k2:
    (let ([k3 (call-with-continuation-prompt
               (lambda ()
                 (k2 (lambda ()
                       (dynamic-wind
                           (lambda () (set! l (cons 'pre2 l)))
                           (lambda () 
                             ((call-with-composable-continuation
                               (lambda (k) 
                                 (lambda () k)))))
                           (lambda () (set! l (cons 'post2 l))))))))])
      (test '(post post2 pre2 pre) values l)
      (test 99 k3 (lambda () 99))
      (test '(post post2 pre2 pre post post2 pre2 pre) values l))
    (set! l null)    
    ;; Add d-w outside k2:
    (let ([k4 (call-with-continuation-prompt
               (lambda ()
                 (dynamic-wind
                     (lambda () (set! l (cons 'pre2 l)))
                     (lambda () 
                       (k2 (lambda ()
                             ((call-with-composable-continuation
                               (lambda (k) 
                                 (lambda () k)))))))
                     (lambda () (set! l (cons 'post2 l))))))])
      (test '(post2 post pre pre2) values l)
      (test 99 k4 (lambda () 99))
      (test '(post2 post pre pre2 post2 post pre pre2) values l))))

;; Jump back into post:    
(let ([l null]
      [p1 (make-continuation-prompt-tag)]
      [p2 (make-continuation-prompt-tag)]
      [k2 #f])
  (define (out v) (set! l (cons v l)))
  (call-with-continuation-prompt
   (lambda ()
     (dynamic-wind
         (lambda () (out 'pre))
         (lambda ()
           (call-with-continuation-prompt
            (lambda ()
              (dynamic-wind
                  (lambda () (out 'pre2))
                  (lambda () (void))
                  (lambda ()
                    (call/cc (lambda (k)
                               (set! k2 k))
                             p2)
                    (out 'post2))))
            p2))
         (lambda () (out 'post1))))
   p1)
  (call-with-continuation-prompt
   (lambda ()
     (k2 10))
   p2)
  (test '(post2 post1 post2 pre2 pre) values l))

;; Jump into post, then back out
(let ([l null]
      [p1 (make-continuation-prompt-tag)]
      [p2 (make-continuation-prompt-tag)]
      [k2 #f]
      [count 0])
    (define (out v) (set! l (cons v l)))
    (let/cc esc
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () (out 'pre1))
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (dynamic-wind
                      (lambda () (out 'pre2))
                      (lambda () (void))
                      (lambda ()
                        (call/cc (lambda (k)
                                   (set! k2 k))
                                 p2)
                        (out 'post2)
                        (esc))))
                p2))
             (lambda () (out 'post1))))
       p1))
    (printf "here ~a\n" count)
    (set! count (add1 count))
    (unless (= count 3)
      (call-with-continuation-prompt
       (lambda ()
         (k2 10))
       p2))
    (test '(post2 post2 post1 post2 pre2 pre1) values l))

(printf "into post from escape\n")

;; Jump into post from an escape, rather than 
;;  from a result continuation
(let ([l null]
      [p1 (make-continuation-prompt-tag)]
      [p2 (make-continuation-prompt-tag)]
      [k2 #f]
      [count 0])
    (define (out v) (set! l (cons v l)))
    (let/cc esc
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () (out 'pre1))
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (dynamic-wind
                      (lambda () (out 'pre2))
                      (lambda () (esc))
                      (lambda ()
                        (call/cc (lambda (k)
                                   (set! k2 k))
                                 p2)
                        (out 'post2))))
                p2))
             (lambda () (out 'post1))))
       p1))
    (set! count (add1 count))
    (unless (= count 3)
      (call-with-continuation-prompt
       (lambda ()
         (k2 10))
       p2))
    (test '(post2 post2 post1 post2 pre2 pre1) values l))

;; ----------------------------------------
;; Continuation marks

(let ([go
       (lambda (access-tag catch-tag blocked?)
         (let ([k (call-with-continuation-prompt
                   (lambda ()
                     (with-continuation-mark
                         'x
                         17
                       ((call/cc (lambda (k) (lambda () k))
                                 catch-tag))))
                   catch-tag)])
           (with-continuation-mark
               'x
               18
             (with-continuation-mark
                 'y
                 8
               (begin
                 (printf "here\n")
                 (test 18 continuation-mark-set-first #f 'x #f catch-tag)
                 (test '(18) continuation-mark-set->list (current-continuation-marks catch-tag) 'x catch-tag)
                 (test 17
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set-first #f 'x #f catch-tag))))
                       catch-tag)
                 (test #f
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set-first #f 'y #f catch-tag))))
                       catch-tag)
                 (test (if (eq? catch-tag (default-continuation-prompt-tag)) #f 8)
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set-first #f 'y #f catch-tag))))
                       (default-continuation-prompt-tag))
                 (test (if blocked?
                           '(17)
                           '(17 18))
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set->list (current-continuation-marks access-tag) 
                                                                    'x access-tag))))
                       catch-tag)
                 (test '(17)
                       continuation-mark-set->list (continuation-marks k catch-tag) 'x catch-tag)
                 (test (if blocked?
                           '()
                           '(8))
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set->list (current-continuation-marks access-tag) 
                                                                    'y access-tag))))
                       catch-tag)
                 
                 'done)))))])
  (go (default-continuation-prompt-tag) (default-continuation-prompt-tag) #t)
  (let ([p2 (make-continuation-prompt-tag 'p2)])
    (call-with-continuation-prompt
     (lambda ()
       (go p2 p2 #t)
       (go p2 (default-continuation-prompt-tag) #f)
       (go (default-continuation-prompt-tag) p2 #f))
     p2)))

(define (non-tail v) (values v))

(let ()
  (define (go access-tag blocked?)
    (let ([k (call-with-continuation-prompt
              (lambda ()
                (with-continuation-mark
                    'x
                    71
                  ((call-with-composable-continuation
                    (lambda (k)
                      (lambda () k)))))))])
      (test #f continuation-mark-set-first #f 'x)
      (test 71 k (lambda () (continuation-mark-set-first #f 'x)))
      (test '(71) continuation-mark-set->list (continuation-marks k) 'x)
      (test 71 'wcm (with-continuation-mark
                        'x 81
                      (k (lambda () (continuation-mark-set-first #f 'x)))))
      (test '(71 81) 'wcm (with-continuation-mark
                              'x 81
                            (non-tail
                             (k (lambda ()
                                  (continuation-mark-set->list (current-continuation-marks) 'x))))))
      (test '(71) 'wcm (with-continuation-mark
                           'x 81
                         (k (lambda ()
                              (continuation-mark-set->list (current-continuation-marks) 'x)))))
      (test '(91 71 81) 'wcm (with-continuation-mark
                                 'x 81
                               (non-tail
                                (k (lambda ()
                                     (non-tail
                                      (with-continuation-mark
                                          'x 91
                                        (continuation-mark-set->list (current-continuation-marks) 'x))))))))
      (test '(91 81) 'wcm (with-continuation-mark
                              'x 81
                            (non-tail
                             (k (lambda ()
                                  (with-continuation-mark
                                      'x 91
                                    (continuation-mark-set->list (current-continuation-marks) 'x)))))))
      (test '(91) 'wcm (with-continuation-mark
                           'x 81
                         (k (lambda ()
                              (with-continuation-mark
                                  'x 91
                                (continuation-mark-set->list (current-continuation-marks) 'x))))))
      (let ([k2 (with-continuation-mark
                    'x 101
                  (call-with-continuation-prompt
                   (lambda ()
                     (with-continuation-mark
                         'x 111
                       (non-tail
                        (k (lambda ()
                             ((call-with-composable-continuation
                               (lambda (k2)
                                 (test (if blocked?
                                           '(71 111)
                                           '(71 111 101))
                                       continuation-mark-set->list (current-continuation-marks access-tag) 
                                       'x access-tag)
                                 (lambda () k2)))))))))))])
        (test '(71 111) continuation-mark-set->list (continuation-marks k2) 'x)
        (test '(71 111) k2 (lambda ()
                             (continuation-mark-set->list (current-continuation-marks) 'x)))
        (test 71 k2 (lambda ()
                      (continuation-mark-set-first #f 'x)))
        (test '(71 111 121) 'wcm (with-continuation-mark
                                     'x 121
                                   (non-tail
                                    (k2 (lambda ()
                                          (continuation-mark-set->list (current-continuation-marks) 'x))))))
        )

      (let ([k2 (with-continuation-mark
                    'x 101
                  (call-with-continuation-prompt
                   (lambda ()
                     (with-continuation-mark
                         'x 111
                       (k (lambda ()
                            ((call-with-composable-continuation
                              (lambda (k2)
                                (test (if blocked?
                                          '(71)
                                          '(71 101))
                                      continuation-mark-set->list (current-continuation-marks access-tag)
                                      'x access-tag)
                                (lambda () k2))))))))))])
        (test '(71) continuation-mark-set->list (continuation-marks k2) 'x)
        (test '(71) k2 (lambda ()
                         (continuation-mark-set->list (current-continuation-marks) 'x)))
        (test 71 k2 (lambda ()
                      (continuation-mark-set-first #f 'x)))
        (test '(71 121) 'wcm (with-continuation-mark
                                 'x 121
                               (non-tail
                                (k2 (lambda ()
                                      (continuation-mark-set->list (current-continuation-marks) 'x)))))))))
  (go (default-continuation-prompt-tag) #t)
  (let ([p2 (make-continuation-prompt-tag 'p2)])
    (call-with-continuation-prompt
     (lambda ()
       (go p2 #f))
     p2)))

;; Check interaction of dynamic winds, continuation composition, and continuation marks
(let ([pre-saw-xs null]
      [post-saw-xs null]
      [pre-saw-ys null]
      [post-saw-ys null])
  (let ([k (call-with-continuation-prompt
            (lambda ()
              (with-continuation-mark
                  'x
                  77
                (dynamic-wind
                    (lambda () 
                      (set! pre-saw-xs (continuation-mark-set->list (current-continuation-marks) 'x))
                      (set! pre-saw-ys (continuation-mark-set->list (current-continuation-marks) 'y)))
                    (lambda ()
                      ((call-with-composable-continuation
                        (lambda (k)
                          (lambda () k)))))
                    (lambda () 
                      (set! post-saw-xs (continuation-mark-set->list (current-continuation-marks) 'x))
                      (set! post-saw-ys (continuation-mark-set->list (current-continuation-marks) 'y)))))))])
    (test '(77)  values pre-saw-xs)
    (test '()  values pre-saw-ys)
    (test '(77)  values post-saw-xs)
    (test '()  values post-saw-ys)
    (let ([jump-in
           (lambda (wrap r-val y-val)
             (test r-val 'wcm
                   (wrap
                    (lambda (esc)
                      (with-continuation-mark
                          'y y-val
                        (k (lambda () (esc)))))))
             (test '(77)  values pre-saw-xs)
             (test (list y-val)  values pre-saw-ys)
             (test '(77)  values post-saw-xs)
             (test (list y-val)  values post-saw-ys)
             (let ([k3 (call-with-continuation-prompt
                        (lambda ()
                          ((call-with-composable-continuation
                            (lambda (k)
                              (lambda () k))))))])
               (test r-val 'wcm
                     (wrap
                      (lambda (esc)
                        (k3
                         (lambda ()
                           (with-continuation-mark
                               'y y-val
                             (k (lambda () (k3 (lambda () (esc)))))))))))))])
      (jump-in (lambda (f) (f (lambda () 10))) 10 88)
      (jump-in (lambda (f) (let/cc esc (f (lambda () (esc 20))))) 20 99)
      (printf "here\n")
      (jump-in (lambda (f)
                    (let ([p1 (make-continuation-prompt-tag)])
                      (call-with-continuation-prompt
                       (lambda ()
                         (f (lambda () (abort-current-continuation p1 (lambda () 30)))))
                       p1)))
               30 111)
      (void))))

;; Tail meta-calls should overwrite continuation marks
(let ([k (call-with-continuation-prompt
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k)
                (lambda () k))))))])
  (with-continuation-mark
      'n #f
    (let loop ([n 10])
      (unless (zero? n)
        (with-continuation-mark
            'n n
          (k (lambda ()
               (test (list n) continuation-mark-set->list (current-continuation-marks) 'n)
               (loop (sub1 n)))))))))

;; Tail meta-calls should propagate cont marks
(let ([k (call-with-continuation-prompt
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k)
                (lambda () k))))))])
  (with-continuation-mark
      'n 10
    (let loop ([n 10])
      (test n continuation-mark-set-first #f 'n)
      (test (list n) continuation-mark-set->list (current-continuation-marks) 'n)
      (unless (zero? n)
        (k (lambda ()
             (with-continuation-mark
                 'n (sub1 n)
               (loop (sub1 n)))))))))

;; Captured mark should replace installed mark
(let ([k (call-with-continuation-prompt
          (lambda ()
            (with-continuation-mark
                'n #t
              ((call-with-composable-continuation
                (lambda (k)
                  (lambda () k)))))))])
  (with-continuation-mark
      'n #f
    (let loop ([n 10])
      (unless (zero? n)
        (with-continuation-mark
            'n n
          (k (lambda ()
               (test (list #t) continuation-mark-set->list (current-continuation-marks) 'n)
               (test #t continuation-mark-set-first #f 'n)
               (loop (sub1 n)))))))))

;; ----------------------------------------
;; Olivier Danvy's traversal

;; Shift & reset via composable and abort
(let ()
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      '()
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 (let ([v (cons (car xs)
                                                (call-with-continuation-prompt
                                                 (lambda ()
                                                   (k (cdr xs)))))])
                                   (lambda () v))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(1 2 3 4 5) traverse '(1 2 3 4 5)))

;; Shift & reset using composable and call/cc
(let ()
  (define call-in-application-context
    (call-with-continuation-prompt
     (lambda ()
       ((call-with-current-continuation
         (lambda (k) (lambda () k)))))))
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      '()
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (call-in-application-context
                                 (lambda ()
                                   (cons (car xs)
                                         (call-with-continuation-prompt
                                          (lambda ()
                                            (k (cdr xs))))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(1 2 3 4 5) traverse '(1 2 3 4 5)))

;; control and prompt using composable and abort
(let ()
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      (list-tail '() 0)
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 (lambda ()
                                   (cons (car xs)
                                         (k (cdr xs))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(5 4 3 2 1) traverse '(1 2 3 4 5)))

;; control and prompt using composable and call/cc
(let ()
  (define call-in-application-context
    (call-with-continuation-prompt
     (lambda ()
       ((call-with-current-continuation
         (lambda (k) (lambda () k)))))))
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      (list-tail '() 0)
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (call-in-application-context
                                 (lambda ()
                                   (cons (car xs)
                                         (k (cdr xs))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(5 4 3 2 1) traverse '(1 2 3 4 5)))

;; ----------------------------------------
;; Check unwinding of runstack overflows on prompt escape

(let ([try
       (lambda (thread m-top n-top do-mid-stream do-abort)
         (let ([result #f])
           (thread-wait
            (thread
             (lambda ()
               (set! result
                     (let pre-loop ([m m-top])
                       (if (zero? m)
                           (list
                            (do-mid-stream
                             (lambda () 
                               (call-with-continuation-prompt
                                (lambda ()
                                  (let loop ([n n-top])
                                    (if (zero? n)
                                        (do-abort
                                         (lambda ()
                                           (abort-current-continuation
                                            (default-continuation-prompt-tag)
                                            (lambda () 5000))))
                                        (+ (loop (sub1 n))))))))))
                           (list (car (pre-loop (sub1 m))))))))))
           (test '(5000) values result)))])
  (try thread 5000 10000 (lambda (mid) (mid)) (lambda (abort) (abort)))
  (try thread 5000 10000 (lambda (mid) (mid)) 
       (lambda (abort) ((call-with-continuation-prompt
                         (lambda ()
                           ((call-with-composable-continuation
                             (lambda (k) (lambda () k))))))
                        (lambda () 5000))))
  (try thread 5000 10000 (lambda (mid) (mid)) 
       (lambda (abort) ((call-with-continuation-prompt
                         (lambda ()
                           ((call/cc
                             (lambda (k) (lambda () k))))))
                        (lambda () 5000))))
  (try thread 5000 10000 (lambda (mid) (mid)) 
       (lambda (abort) (((call/cc
                          (lambda (k) (lambda () k))))
                        (lambda () (lambda (x) 5000)))))
  (try thread 5000 10000 
       (lambda (mid) (call-with-continuation-barrier mid))
       (lambda (abort) (((call/cc
                          (lambda (k) (lambda () k))))
                        (lambda () (lambda (x) 5000)))))
  (let ([p (make-continuation-prompt-tag 'p)])
    (try (lambda (f)
           (thread
            (lambda ()
              (call-with-continuation-prompt f p))))
         5000 10000 
         (lambda (mid) (mid))
         (lambda (abort) 
           ((call/cc
             (lambda (k)
               (thread-wait (thread
                             (lambda ()
                               (call-with-continuation-prompt 
                                (lambda ()
                                  (k abort))
                                p))))
               (lambda () (abort-current-continuation p void)))
             p)))))
  )

(test-breaks-ok)

;; ----------------------------------------
;; Some repeats, but ensure a continuation prompt
;;  and check d-w interaction.

(let ([output null])
  (call-with-continuation-prompt
   (lambda () 
     (dynamic-wind
         (lambda () (set! output (cons 'in output)))
         (lambda ()
           (let ([finished #f])
             (define (go)
               (let ([p1 (make-continuation-prompt-tag)]
                     [counter 10])
                 (let ([k (call-with-continuation-prompt
                           (lambda ()
                             ((call/cc (lambda (k) (lambda () k))
                                       p1)))
                           p1)])
                   (let ([k2 (list
                              (call-with-continuation-prompt
                               (lambda ()
                                 (k (lambda ()
                                      ((let/cc k (lambda () k))))))
                               p1))])
                     (current-milliseconds)
                     (if (procedure? (car k2))
                         ((car k2) (lambda () 
                                     (if (zero? counter)
                                         10
                                         (begin
                                           (set! counter (sub1 counter))
                                           ((let/cc k (lambda () k)))))))
                         (values '(10) values k2))
                     (set! finished 'finished)))))
             (go)))
         (lambda () (set! output (cons 'out output)))))
   (default-continuation-prompt-tag)
   void)
  (test '(out in) values output))

(let ([output null])
  (call-with-continuation-prompt
   (lambda () 
     (dynamic-wind
         (lambda () (set! output (cons 'in output)))
         (lambda ()
           (let ([p1 (make-continuation-prompt-tag)])
             (let/cc esc
               (let ([k
                      (call-with-continuation-prompt
                       (lambda ()
                         ((call-with-composable-continuation
                           (lambda (k)
                             (lambda () k))
                           p1)))
                       p1)])
                 (/ (k (lambda () (esc 0))))))))
         (lambda () (set! output (cons 'out output)))))
   (default-continuation-prompt-tag)
   void)
  (test '(out in) values output))

;;----------------------------------------
;; tests invoking delimited captures in dynamic-wind pre- and post-thunks

;; Arrange for a post-thunk to remove a target
;; for an escape:
(err/rt-test
 (let ([p1 (make-continuation-prompt-tag 'p1)]
       [exit-k #f])
   (let ([x (let/ec esc
              (call-with-continuation-prompt
               (lambda ()
                 (dynamic-wind
                     (lambda () (void))
                     (lambda () (esc 'done))
                     (lambda ()
                       ((call/cc
                         (lambda (k) 
                           (set! exit-k k)
                           (lambda () 10))
                         p1))
                       (printf "post\n"))))
               p1))])
     (call-with-continuation-barrier
      (lambda ()
        (call-with-continuation-prompt
         (lambda ()
           (exit-k (lambda () 'hi)))
         p1)))))
 exn:fail:contract:continuation?)

;; Same thing, but escape via prompt:
(err/rt-test
 (let ([p1 (make-continuation-prompt-tag 'p1)]
       [p2 (make-continuation-prompt-tag 'p2)]
       [output null]
       [exit-k #f])
   (let ([x (call-with-continuation-prompt
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (dynamic-wind
                      (lambda () (void))
                      (lambda () (abort-current-continuation p2 1 2 3))
                      (lambda ()
                        ((call/cc
                          (lambda (k) 
                            (set! exit-k k)
                            (lambda () 10))
                          p1))
                        (set! output (cons 'post output)))))
                p1))
             p2
             void)])
     (call-with-continuation-barrier
      (lambda ()
        (call-with-continuation-prompt
         (lambda ()
           (exit-k (lambda () 'hi)))
         p1)))))
 exn:fail:contract?)

;; Arrange for a barrier to interfere with a continuation
;; jump after dynamic-winds are already being processed:
(let ([p1 (make-continuation-prompt-tag 'p1)]
      [output null]
      [exit-k #f])
  (let ([go
         (lambda (launch)
           (let ([k (let/cc esc
                      (call-with-continuation-prompt
                       (lambda ()
                         (dynamic-wind
                             (lambda () (void))
                             (lambda ()
                               (with-handlers ([void (lambda (exn)
                                                       (test #f "should not be used!" #t))])
                                 (launch esc)))
                             (lambda ()
                               ((call/cc
                                 (lambda (k) 
                                   (set! exit-k k)
                                   (lambda () 10))
                                 p1))
                               (set! output (cons 'post output)))))
                       p1))])
             (call-with-continuation-barrier
              (lambda ()
                (call-with-continuation-prompt
                 (lambda ()
                   (exit-k (lambda () 'hi)))
                 p1)))))])
    (err/rt-test
     (go (lambda (esc) (esc 'middle)))
     exn:fail:contract:continuation?)
    (test '(post post) values output)
    (let ([meta (call-with-continuation-prompt
                 (lambda ()
                   ((call-with-composable-continuation
                     (lambda (k) (lambda () k))))))])
      (err/rt-test
       (go (lambda (esc)
             (meta
              (lambda () (esc 'ok)))))
       exn:fail:contract:continuation?))
    (test '(post post post post) values output)))

;; Similar, but more checking of dropped d-ws:
(let ([p1 (make-continuation-prompt-tag 'p1)]
      [output null]
      [exit-k #f]
      [done? #f])
  ;; Capture a continuation w.r.t. the default prompt tag:
  (call/cc
   (lambda (esc)
     (dynamic-wind
         (lambda () (void))
         (lambda () 
           ;; Set a prompt for tag p1:
           (call-with-continuation-prompt
            (lambda ()
              (dynamic-wind
                  (lambda () (void))
                  ;; inside d-w, jump out:
                  (lambda () (esc 'done))
                  (lambda ()
                    ;; As we jump out, capture a continuation 
                    ;; w.r.t. p1:
                    ((call/cc
                      (lambda (k) 
                        (set! exit-k k)
                        (lambda () 10))
                      p1))
                    (set! output (cons 'inner output)))))
            p1))
         (lambda ()
           ;; This post thunk is not in the
           ;;  delimited continuation captured
           ;; via tag p1:
           (set! output (cons 'outer output))))))
  (unless done?
    (set! done? #t)
    ;; Now invoke the delimited continuation, which must
    ;; somehow continue the jump to `esc':
    (call-with-continuation-prompt
     (lambda ()
       (exit-k (lambda () 10)))
     p1))
  (test '(inner outer inner) values output))

;; Again, more checking of output
(let ([p1 (make-continuation-prompt-tag 'p1)]
      [p2 (make-continuation-prompt-tag 'p2)]
      [output null]
      [exit-k #f])
    ;; Set up a prompt tp jump to:
    (call-with-continuation-prompt
     (lambda ()
       (dynamic-wind
           (lambda () (void))
           (lambda () 
             ;; Set a prompt for tag p1:
             (call-with-continuation-prompt
              (lambda ()
                (dynamic-wind
                    (lambda () (void))
                    ;; inside d-w, jump out:
                    (lambda () (abort-current-continuation
                                p2
                                "done"))
                    (lambda ()
                      ;; As we jump out, capture a continuation 
                      ;; w.r.t. p1:
                      ((call/cc
                        (lambda (k) 
                          (set! exit-k k)
                          (lambda () 10))
                        p1))
                      (set! output (cons 'inner output)))))
              p1))
           (lambda ()
             ;; This post thunk is not in the
             ;;  delimited continuation captured
             ;; via tag p1:
             (set! output (cons 'outer output)))))
     p2
     (lambda (v)
       (set! output (cons 'orig output))))
    ;; Now call, redirecting the escape to here:
    (call-with-continuation-prompt
     (lambda ()
       (call-with-continuation-prompt
        (lambda ()
          (exit-k (lambda () 10)))
        p1))
     p2
     (lambda (v)
       (set! output (cons 'new output))))
    (test '(new inner orig outer inner) values output))

;; abort past a tag
(test 10
      values
      (let ([p1 (make-continuation-prompt-tag)]
            [p2 (make-continuation-prompt-tag)])
        (call-with-continuation-prompt
         (lambda ()
           (call/cc
            (lambda (k)
              (call-with-continuation-prompt
               (lambda ()
                 (k 10))
               p2))
            p1))
         p1)))

;; Check that a prompt is not somehow tied to its original
;;  barrier, so that jumps are not allowed when they should
;;  be:
(test 0
      values
      (let ([p1 (make-continuation-prompt-tag 'p1)]
            [p2 (make-continuation-prompt-tag 'p2)])
        (let ([k (call-with-continuation-prompt
                  (lambda ()
                    (call-with-continuation-prompt
                     (lambda ()
                       ((call-with-current-continuation
                         (lambda (k) (lambda () k))
                         p2)))
                     p1))
                  p2)])
          (call-with-continuation-barrier
           (lambda ()
             (call-with-continuation-barrier
              (lambda ()
                (let ([k1
                       (call-with-continuation-prompt
                        (lambda ()
                          (k
                           (lambda ()
                             ;; prompt for p1 has been restored
                             (call/cc (lambda (k1) k1) p1))))
                        p2)])
                  (call-with-continuation-prompt
                   (lambda ()
                     (k1 0))
                   p1)))))))))

(test 12
      values
      (let ([p1 (make-continuation-prompt-tag 'p1)])
        (let ([k (call-with-continuation-barrier
                  (lambda ()
                    (call-with-continuation-prompt
                     (lambda ()
                       ((call-with-current-continuation
                         (lambda (k) (lambda () k))
                         p1)))
                     p1)))])
          (call-with-continuation-barrier
           (lambda ()
             (call-with-continuation-barrier
              (lambda ()
                (call-with-continuation-barrier
                 (lambda ()
                   (call-with-continuation-prompt
                    (lambda ()
                      (let/cc w
                        (call-with-continuation-prompt
                         (lambda ()
                           (k (lambda () (w 12))))
                         p1)))))))))))))

;; Test capturing and invoking a composable continuation in a post thunk
(let ()
  (define call/pt call-with-continuation-prompt)
  (define call/comp-cc call-with-composable-continuation)
  (define (go p0 direct?)
    (define accum null)
    (define (print v) (set! accum (append accum (list v))))
    (define a #f)
    (define do-a? #t)
    (call/pt
     (lambda ()
       (dynamic-wind
           (lambda () (print 1))
           (lambda ()
             (begin
               (dynamic-wind
                   (lambda () (print 2))
                   (lambda () 
                     ((call/cc (lambda (k)
                                 (begin
                                   (set! a k)
                                   (lambda () 12)))
                               p0)))
                   (lambda () (print 3)))
               (dynamic-wind
                   (lambda () (print 4))
                   (lambda ()
                     (if do-a?
                         (begin
                           (set! do-a? #f)
                           (a (lambda () 11)))
                         12))
                   (lambda ()
                     (begin
                       (print 5)
                       (call/comp-cc
                        (lambda (k)
                          (if direct?
                              (k 10)
                              (call/pt
                               (lambda ()
                                 (k 10))
                               p0
                               (lambda (x) x))))
                        p0))))))
           (lambda () (print 6))))
     p0
     (lambda (x) x))
    accum)
  (test '(1 2 3 4 5 1 6 2 3 4 5 1 6 6) go (default-continuation-prompt-tag) #t)
  (test '(1 2 3 4 5 1 6 2 3 4 5 1 6 6) go (make-continuation-prompt-tag) #t)
  (test '(1 2 3 4 5 1 2 3 4 5 1 6 6 2 3 4 5 1 6 6) go (default-continuation-prompt-tag) #f)
  (test '(1 2 3 4 5 1 2 3 4 5 1 6 6 2 3 4 5 1 6 6) go (make-continuation-prompt-tag) #f))

;; ----------------------------------------
;; Run two levels of continuations where an explicit
;; prompt in a capturing thread is represented by an
;; implicit prompt in the calling thread.

(let ()
  (define (go wrap)
    (let ()
      (define (foo thunk)
        (call-with-continuation-prompt
         (lambda ()
           (wrap
            (lambda ()
              (let/cc ret
                (let ([run? #f])
                  (let/cc run
                    (thread (lambda ()
                              (sync (system-idle-evt))
                              (set! run? #t)
                              (run))))
                  (when run? (ret (thunk))))))))))
      (define s (make-semaphore))
      (foo (lambda () (semaphore-post s)))
      (test s sync s)))
  (go (lambda (f) (f)))
  (go (lambda (f) (dynamic-wind void f void))))

;; ----------------------------------------
;; Second continuation spans two meta-continuations,
;;  and cuts the deeper meta-continuation in half:

(test
 '("x1")
 'nested-half
 (let* ([says null]
        [say (lambda (s)
               (set! says (cons s says)))]
        [a (make-continuation-prompt-tag 'a)]
        [b (make-continuation-prompt-tag 'b)])
   (let ([ak
          (with-continuation-mark 'x "x0"
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'y "y0"
                 (let ([bk (call-with-continuation-prompt
                            (lambda ()
                              (let ([f (call-with-composable-continuation
                                        (lambda (k)
                                          (lambda () k))
                                        b)])
                                (say "bcall")
                                (begin0
                                 (f)
                                 (say "breturn"))))
                            b)])
                   (call-with-continuation-prompt
                    (lambda ()
                      ((bk (lambda ()
                             (let ([f (call/cc (lambda (k) (lambda () (lambda () k))) a)])
                               (begin0
                                (f)
                                (say "areturn")))))))
                    b))))
             a))])
     (with-continuation-mark 'x "x1"
       (call-with-continuation-prompt
        (lambda ()
          (ak (lambda () 
                (lambda ()
                  (continuation-mark-set->list (current-continuation-marks) 'x)))))
        a)))))


;; ----------------------------------------
;; Tests related to cotinuations that capture pre-thunk frames

;; Simple case:
(let ([t
       (lambda (wrapper)
         (test
          '(pre1 mid1 post1 pre2 mid1 post1 post2)
          'cc1
          (let ([k #f]
                [recs null])
            (define (queue v) (set! recs (cons v recs)))
            (call-with-continuation-prompt
             (lambda ()
               (dynamic-wind
                   (lambda ()
                     (queue 'pre1) 
                     (call-with-composable-continuation
                      (lambda (k0)
                        (set! k k0))))
                   (lambda () (queue 'mid1))
                   (lambda () (queue 'post1)))))
            (wrapper
             (lambda ()
               (dynamic-wind
                   (lambda () (queue 'pre2))
                   (lambda () (k))
                   (lambda () (queue 'post2)))))
            (reverse recs))))])
  (t (lambda (f) (f)))
  (t call-with-continuation-prompt))

;; Mix in some extra dynamic winds:
(test
 '(pre1 mid1 post1 pre2 mid1 post1 post2 pre2 mid1 post1 post2)
 'cc2
 (let ([k #f]
       [k2 #f]
       [recs null])
   (define (queue v) (set! recs (cons v recs)))
   (call-with-continuation-prompt
    (lambda ()
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda ()
               (queue 'pre1) 
               ((call-with-composable-continuation
                 (lambda (k0)
                   (set! k k0)
                   void))))
             (lambda () (queue 'mid1))
             (lambda () (queue 'post1)))))
      (let/ec esc
        (dynamic-wind
            (lambda () (queue 'pre2))
            (lambda () 
              (k (lambda ()
                   (let/cc k0
                     (set! k2 k0))))
              (esc))
            (lambda () (queue 'post2))))))
   (call-with-continuation-prompt
    (lambda () (k2)))
   (reverse recs)))

;; Even more dynamic-winds:
(test
 '(pre0 pre1 mid1 post1 post0 
        pre1.5 pre2 pre0 mid1 post1 post0 post2 post1.5 
        pre3 pre1.5 pre2 pre0 mid1 post1 post0 post2 post1.5 post3)
 'cc3
 (let ([k #f]
       [k2 #f]
       [recs null])
   (define (queue v) (set! recs (cons v recs)))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda ()
            (queue 'pre0))
          (lambda ()
            (dynamic-wind
                (lambda ()
                  (queue 'pre1) 
                  ((call-with-composable-continuation
                    (lambda (k0)
                      (set! k k0)
                      void))))
                (lambda () (queue 'mid1))
                (lambda () (queue 'post1))))
          (lambda ()
            (queue 'post0)))))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda () (queue 'pre1.5))
          (lambda ()
            (dynamic-wind
                (lambda () (queue 'pre2))
                (lambda () (k (lambda ()
                                (call-with-composable-continuation
                                 (lambda (k0)
                                   (set! k2 k0))))))
                (lambda () (queue 'post2))))
          (lambda () (queue 'post1.5)))))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda () (queue 'pre3))
          (lambda () (k2))
          (lambda () (queue 'post3)))))
   (reverse recs)))

;; Arrange for the captured pre-thunk to trigger extra cloning
;; of dynmaic wind records in continuation application:
(test
 '(pre1 pre2 post2 post1 pre1 pre2 post2 post1 last pre2 post2 post1)
 'cc4
 (let ([k #f]
       [k2 #f]
       [recs null]
       [tag (make-continuation-prompt-tag)])
   (define (queue v) (set! recs (cons v recs)))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
       (lambda ()
         (queue 'pre1) 
         ((call-with-composable-continuation
           (lambda (k0)
             (set! k k0)
             void))))
       (lambda () 
         (dynamic-wind
          (lambda () (queue 'pre2))
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k0)
                (set! k2 k0)
                void))))
          (lambda () (queue 'post2))))
       (lambda () (queue 'post1)))))
   (let ([k3
          (call-with-continuation-prompt
           (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (k2 (lambda ()
                     (call-with-composable-continuation
                      (lambda (k0)
                        (abort-current-continuation tag (lambda () k0)))))))))
           tag)])
     (queue 'last)
     (call-with-continuation-prompt
      (lambda ()
        (k void))
      tag))
   (reverse recs)))

;; ----------------------------------------
;; Try long chain of composable continuations

(let ([long-loop
       (lambda (on-overflow)
         (let ([v (make-vector 6)])
           (vector-set-performance-stats! v)
           (let ([overflows (vector-ref v 5)])
             ;; Although this is a constant-space loop, the implementation
             ;; pushes each captured continuation further and further down
             ;; the C stack. Eventually, the relevant segment wraps around,
             ;; with an overflow. Push a little deeper and then capture
             ;; that.
             (let loop ([n 0][fuel #f])
               (vector-set-performance-stats! v)
               (cond
                [(and (not fuel)
                      ((vector-ref v 5) . > . overflows))
                 (begin
                   (printf "Overflow at ~a\n" n)
                   (loop n 5))]
                [(and fuel (zero? fuel))
                 (on-overflow)]
                [else
                 ((call-with-continuation-prompt
                   (lambda ()
                     ((call-with-composable-continuation
                       (lambda (k)
                         (lambda (n f) k)))
                      (add1 n)
                      (and fuel (sub1 fuel)))))
                  loop)])))))]
      [once-k #f])
  (printf "Breaking long chain...\n")
  (let ([t (thread (lambda () (long-loop void)))])
    (sleep 0.05)
    (break-thread t)
    (sync (system-idle-evt))
    (test #f thread-running? t))
  (printf "Trying long chain...\n")
  (let ([k (long-loop (lambda () 
                        ((let/cc k (lambda () k)))))])
    (when (procedure? k)
      (set! once-k k)
      (k (lambda () 17)))
    (test #t procedure? once-k)
    (test k values 17)
    (err/rt-test (call-with-continuation-barrier
                   (lambda ()
                     (once-k 18)))
                 exn:fail:contract:continuation?))
  (printf "Trying long chain again...\n")
  (let ([k (call-with-continuation-prompt
            (lambda ()
              (long-loop (lambda () 
                           ((call-with-composable-continuation
                             (lambda (k)
                               (lambda () k))))))))])
    (test 18 k (lambda () 18))
    (err/rt-test (k (lambda () (/ 0))) exn:fail:contract:divide-by-zero?)
    (test 45 call-with-continuation-prompt
          (lambda ()
            (k (lambda () (abort-current-continuation
                           (default-continuation-prompt-tag)
                           (lambda () 45))))))))

;; ----------------------------------------
;; Check continuations captured in continuations applied in
;;  a thread:

(test (void)
      'simple-thread-transfer
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  (call/cc values)))])
        (sync (thread (lambda () (k 6))))
        (void)))

(test (void)
      'capture-in-transferred-thread
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  (let/ec esc
                    (call/cc esc)
                    (call/cc values))))])
        (sync (thread (lambda () (k 6))))
        (void)))

(test (void)
      'capture-in-transferred-thread/values
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  (let/ec esc
                    (define-values (a b) (call/cc esc))
                    (+ a b)
                    (call/cc values))))])
        (sync (thread (lambda () (k 6 7))))
        (void)))

(let ()
  (define sema (make-semaphore 1))
  (define l null)
  (define (push v) (semaphore-wait sema) (set! l (cons v l)) (semaphore-post sema))
  (define (count n)
    (let loop ([l l])
      (cond
       [(null? l) 0]
       [(equal? (car l) n) (add1 (loop (cdr l)))]
       [else (loop (cdr l))])))
  (define (f)
    (push 1)
    (call/cc thread)
    (push 2)
    (call/cc thread)
    (push 3))
  
  (call-with-continuation-prompt f)
  (sync (system-idle-evt))
  (test 1 count 1)
  (test 2 count 2)
  (test 4 count 3))

;; ----------------------------------------
;; Test genearted by a random tester that turns out
;;  to check meta-continuation continuation-mark lookup
;;  in a dynamic-wind thunk:

(test 
 'exn
 'random-dc-test
 (with-handlers ([exn:fail? (lambda (exn) 'exn)])
   (let ()
     (define tag
       (let ([tags (make-hash)])
         (λ (v)
            (hash-ref tags v 
                      (λ ()
                         (let ([t (make-continuation-prompt-tag)])
                           (hash-set! tags v t)
                           t))))))

     (define-syntax-rule (% tag-val expr handler)
       (call-with-continuation-prompt 
        (λ () expr)
        (let ([v tag-val])
          (if (let comparable? ([v v])
                (cond [(procedure? v) #f]
                      [(list? v) (andmap comparable? v)]
                      [else #t]))
              (tag v)
              (raise-type-error '% "non-procedure" v)))
        (let ([h handler])
          (λ (x) (h x)))))

     (define (abort tag-val result) 
       (abort-current-continuation (tag tag-val) result))

     (define (call/comp proc tag-val) 
       (call-with-composable-continuation (compose proc force-unary) (tag tag-val)))

     (define (call/cm key val thunk)
       (with-continuation-mark key val (thunk)))

     (define (current-marks key tag-val)
       (continuation-mark-set->list 
        (current-continuation-marks (tag tag-val))
        key))

     (define ((force-unary f) x) (f x))

     (define (_call/cc proc tag-val)
       (call/cc (compose proc force-unary) (tag tag-val)))

     (letrec ((CEJ-comp-cont_13 #f)
              (CEJ-skip-pre?_12 #f)
              (CEJ-allocated?_11 #f)
              (s-comp-cont_9 #f)
              (s-skip-pre?_8 #f)
              (s-allocated?_7 #f)
              (N-comp-cont_4 #f)
              (N-skip-pre?_3 #f)
              (N-allocated?_2 #f)
              (handlers-disabled?_0 #f))
       (%
        #t
        ((begin
           (set! handlers-disabled?_0 #t)
           ((λ (v_1)
               (%
                v_1
                ((λ (t_5)
                    (if N-allocated?_2
                        (begin (if handlers-disabled?_0 #f (set! N-skip-pre?_3 #t)) (N-comp-cont_4 t_5))
                        (%
                         1
                         (dynamic-wind
                             (λ ()
                                (if handlers-disabled?_0
                                    #f
                                    (if N-allocated?_2
                                        (if N-skip-pre?_3
                                            (set! N-skip-pre?_3 #f)
                                            (begin
                                              (set! handlers-disabled?_0 #t)
                                              ((λ (v_6)
                                                  (% v_6 (_call/cc (λ (k) (abort v_6 k)) v_6) (λ (x) (begin (set! handlers-disabled?_0 #f) x))))
                                               print)))
                                        #f)))
                             (λ () ((call/comp (λ (k) (begin (set! N-comp-cont_4 k) (abort 1 k))) 1)))
                             (λ () (if handlers-disabled?_0 (set! N-allocated?_2 #t) (if N-allocated?_2 #f (set! N-allocated?_2 #t)))))
                         (λ (k) (begin (if handlers-disabled?_0 #f (set! N-skip-pre?_3 #t)) (k t_5))))))
                 (λ ()
                    ((λ (t_10)
                        (if s-allocated?_7
                            (begin (if handlers-disabled?_0 #f (set! s-skip-pre?_8 #t)) (s-comp-cont_9 t_10))
                            (%
                             1
                             (dynamic-wind
                                 (λ () (if handlers-disabled?_0 #f (if s-allocated?_7 (if s-skip-pre?_8 (set! s-skip-pre?_8 #f) #f) #f)))
                                 (λ () ((call/comp (λ (k) (begin (set! s-comp-cont_9 k) (abort 1 k))) 1)))
                                 (λ ()
                                    (if handlers-disabled?_0 (set! s-allocated?_7 #t) (if s-allocated?_7 #f (set! s-allocated?_7 #t)))))
                             (λ (k) (begin (if handlers-disabled?_0 #f (set! s-skip-pre?_8 #t)) (k t_10))))))
                     (λ ()
                        ((λ (t_14)
                            (if CEJ-allocated?_11
                                (begin (if handlers-disabled?_0 #f (set! CEJ-skip-pre?_12 #t)) (CEJ-comp-cont_13 t_14))
                                (%
                                 1
                                 (dynamic-wind
                                     (λ ()
                                        (if handlers-disabled?_0
                                            #f
                                            (if CEJ-allocated?_11 (if CEJ-skip-pre?_12 (set! CEJ-skip-pre?_12 #f) first) #f)))
                                     (λ () ((call/comp (λ (k) (begin (set! CEJ-comp-cont_13 k) (abort 1 k))) 1)))
                                     (λ ()
                                        (if handlers-disabled?_0
                                            (set! CEJ-allocated?_11 #t)
                                            (if CEJ-allocated?_11 call/cm (set! CEJ-allocated?_11 #t)))))
                                 (λ (k) (begin (if handlers-disabled?_0 #f (set! CEJ-skip-pre?_12 #t)) (k t_14))))))
                         (λ () (_call/cc (λ (k) (abort v_1 k)) v_1)))))))
                (λ (x) (begin (set! handlers-disabled?_0 #f) x))))
            #t))
         1234)
        (λ (x) x))))))

;; ----------------------------------------
;; Test genearted by a random tester that turns out
;;  to check meta-continuation offsets for dynamic-wind
;;  frames:

(test
 'expected-result
 'nested-meta-continuaion-test
 (let ()
   (define pt1 (make-continuation-prompt-tag))
   (define pt3 (make-continuation-prompt-tag))

   (define call/comp call-with-composable-continuation)
   (define abort abort-current-continuation)
   (define-syntax-rule (% pt body handler)
     (call-with-continuation-prompt
      (lambda () body)
      pt
      handler))

   ;; (lambda (f) (f)) 
   ;; as a composable continuation:
   (define comp-app
     (%
      pt1
      ((call/comp
        (λ (k) (abort pt1 k))
        pt1))
      (lambda (k) k)))

   ;; (lambda (f) (dynamic-wind void f void))
   ;; as a composable continuation:
   (define dw-comp-app
     (%
      pt1
      (dynamic-wind
          void
          (λ ()
             ((call/comp
               (λ (k) (abort pt1 k))
               pt1)))
          void)
      (lambda (k) k)))

   (%
    pt3
    (dw-comp-app
     (λ ()
        (%
         pt1
         (dynamic-wind
             void
             (λ ()
                ((call/comp
                  (λ (k) (abort pt1 k))
                  pt1)))
             void)
         (λ (k) 
            (k ; composable app under two dyn-winds
             ;;  where the outer dw is in a deeper meta-cont
             (λ ()
                (comp-app
                 (λ () ; at this point, both dws are in a meta-cont
                    (make-will-executor)
                    ((%
                      pt3
                      (comp-app 
                       (λ () ;; both dws are in a meta-meta-cont
                          ;;    as the continuation is captured
                          (call/cc (λ (k) k) pt3)))
                      void) ; = id continuation that aborts to pt3;
                     ;; as the continuation is applied, dws are back to
                     ;; being in a mere meta-cont, but the continuation
                     ;; itself will restore a meta-continuation layer
                     'expected-result)))))))))
    (λ (x) x))))

;; ----------------------------------------
;; There's a "is the target prompt still in place?"
;;  check that should not happen when a composable
;;  continuation is applied. (Random testing discovered
;;  an incorrect check.)

(test
 12345
 'no-prompt-check-on-compose
 (let ()
   (define pt1 (make-continuation-prompt-tag))

   (define-syntax-rule (% pt body handler)
     (call-with-continuation-prompt
      (lambda () body)
      pt
      handler))

   ;; (lambda (v) v)
   ;; as a composable continuation:
   (define comp-id
     (%
      pt1
      (call-with-composable-continuation
       (λ (k) (abort-current-continuation pt1 k))
       pt1)
      (lambda (k) k)))

   ((% pt1
       (dynamic-wind
           (λ () (comp-id 2))
           (λ () 
              ;; As we jump back to this continuation,
              ;; it's ok that no `pt1' prompt is
              ;; in place anymore
              (call-with-composable-continuation
               (λ (k) (abort-current-continuation
                       pt1 
                       k))
               pt1))
           (λ () #f))
       (λ (x) x))
    12345)))

(test
 12345
 'no-prompt-post-check-on-compose
 (let ()
   (define pt1 (make-continuation-prompt-tag))
   
   (define-syntax-rule (% pt body handler)
     (call-with-continuation-prompt
      (lambda () body)
      pt
      handler))

   ((λ (y-comp-cont_7)
       ((λ (x-comp-cont_3)
           ((%
             pt1
             (x-comp-cont_3
              (λ ()
                 (y-comp-cont_7
                  (λ () (call-with-composable-continuation
                         (λ (k) (abort-current-continuation pt1 k))
                         pt1)))))
             (λ (x) x))
            12345))
        (%
         pt1
         (dynamic-wind
             (λ () (y-comp-cont_7 (λ () #f)))
             (λ () ((call-with-composable-continuation
                     (λ (k) (abort-current-continuation pt1 k))
                     pt1)))
             (λ () #f))
         (λ (x) x))))
    (%
     pt1
     (dynamic-wind
         (λ () #f)
         (λ () ((call-with-composable-continuation
                 (λ (k) (abort-current-continuation pt1 k)) 
                 pt1)))
         (λ () #f))
     (λ (x) x)))))

