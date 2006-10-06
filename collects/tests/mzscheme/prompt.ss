
(load-relative "loadtest.ss")

(Section 'prompt)

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
                  11)))
(test 12 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  12))
      (default-continuation-prompt-tag))
(test-values '(11 12)
             (lambda ()
               (call-with-continuation-prompt 
                (lambda () (abort-current-continuation
                            (default-continuation-prompt-tag)
                            11
                            12)))))
(test 8 call-with-continuation-prompt 
      (lambda () (+ 17
                    (abort-current-continuation
                     (default-continuation-prompt-tag)
                     8))))
(test 81 call-with-continuation-prompt 
      (lambda () (+ 17
                    (call-with-continuation-prompt 
                     (lambda ()
                       (abort-current-continuation
                        (default-continuation-prompt-tag)
                        81))
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
        p))

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

(with-cc-variants
 (test 29
       'in-other-prompt
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
       'in-other-prompt
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
       'in-other-thread
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
       'in-other-thread
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

;; Use default tag to catch a meta-continuation of p1:
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
          (test '(10) values k2)))))

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
          p))
  (test 8 call-with-continuation-prompt 
        (lambda ()
          (+ 99 (k (lambda () (abort-current-continuation 
                               (default-continuation-prompt-tag)
                               8)))))))

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

    
;; ----------------------------------------
;; Continuation marks

(let ([k (call-with-continuation-prompt
          (lambda ()
            (with-continuation-mark
                'x
                17
              ((let/cc k (lambda () k))))))])
  (with-continuation-mark
      'x
      18
    (with-continuation-mark
        'y
        8
      (begin
        (test 18 continuation-mark-set-first #f 'x)
        (test '(18) continuation-mark-set->list (current-continuation-marks) 'x)
        (test 17
              call-with-continuation-prompt
              (lambda ()
                (k (lambda () (continuation-mark-set-first #f 'x)))))
        (test 8
              call-with-continuation-prompt
              (lambda ()
                (k (lambda () (continuation-mark-set-first #f 'y)))))
        (test '(17 18)
              call-with-continuation-prompt
              (lambda ()
                (k (lambda () (continuation-mark-set->list (current-continuation-marks) 'x)))))
        (test '(17)
              continuation-mark-set->list (continuation-marks k) 'x)
        (test '(8)
              call-with-continuation-prompt
              (lambda ()
                (k (lambda () (continuation-mark-set->list (current-continuation-marks) 'y)))))
        
        'done))))

;; ----------------------------------------

(report-errs)
