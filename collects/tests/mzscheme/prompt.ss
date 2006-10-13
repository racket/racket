
(load-relative "loadtest.ss")

(Section 'prompt)

;;----------------------------------------

(define (test-breaks-ok)
  (err/rt-test (break-thread (current-thread)) exn:break?))

(test-breaks-ok)

;;----------------------------------------
;; cc variants

(define call/cc-via-composable 
  (case-lambda 
   [(f) (call/cc-via-composable f (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-composable-continuation
     (lambda (k)
       (f (lambda vs
            (abort-current-continuation 
             tag 
             (lambda () 
               (call-with-continuation-prompt
                (lambda ()
                  (apply k vs))
                tag
                (lambda (thunk) (thunk)))))))))]))
                                           
(define call/cc-via-aborted-and-restored-composable 
  (case-lambda 
   [(f) (call/cc-via-composable f (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation
        tag
        (lambda ()
          (k (f (lambda vs
                  (abort-current-continuation 
                   tag 
                   (lambda () 
                     (call-with-continuation-prompt
                      (lambda ()
                        (apply k vs))
                      tag
                      (lambda (thunk) (thunk))))))))))))]))
                                           
(define call-with-continuation-prompt-for-composable
  (case-lambda
   [(f) (call-with-continuation-prompt-for-composable
         f
         (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-continuation-prompt f
                                   tag
                                   (lambda (thunk) (thunk)))]))

(define (thread-for-composable thunk)
  (thread (lambda ()
            (call-with-continuation-prompt-for-composable
             (lambda () (thunk))))))

(define-syntax (with-cc-variants stx)
  (syntax-case stx ()
    [(_ body)
     (with-syntax ([call/cc (datum->syntax-object stx 'call/cc)]
                   [let/cc (datum->syntax-object stx 'let/cc)]
                   [call-with-continuation-prompt
                    (datum->syntax-object stx
                                          'call-with-continuation-prompt)]
                   [thread (datum->syntax-object stx 'thread)])
       #'(begin
           (define (a-test call/cc call-with-continuation-prompt thread)
             (define-syntax let/cc
               (syntax-rules ()
                 [(_ id bdy (... ...)) 
                  (call/cc (lambda (id) bdy (... ...)))]))
             body)
           (a-test call/cc call-with-continuation-prompt thread)
           (a-test call/cc-via-composable
                   call-with-continuation-prompt-for-composable
                   thread-for-composable)
           (a-test call/cc-via-aborted-and-restored-composable
                   call-with-continuation-prompt-for-composable
                   thread-for-composable)))]))

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

(err/rt-test (call-with-composable-continuation
              (lambda (x) x))
             exn:fail:contract:continuation?)

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
    (set! count (add1 count))
    (unless (= count 3)
      (call-with-continuation-prompt
       (lambda ()
         (k2 10))
       p2))
    (test '(post2 post2 post1 post2 pre2 pre1) values l))

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
                 (test 8
                       call-with-continuation-prompt
                       (lambda ()
                         (k (lambda () (continuation-mark-set-first #f 'y #f catch-tag))))
                       catch-tag)
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
    (sleep)
    (test #f thread-running? t))
  (printf "Trying long chain...\n")
  (let ([k (long-loop (lambda () 
                        ((let/cc k (lambda () k)))))])
    (when (procedure? k)
      (set! once-k k)
      (k (lambda () 17)))
    (test #t procedure? once-k)
    (test k values 17)
    (err/rt-test (call-with-continuation-prompt
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

(report-errs)
