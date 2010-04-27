
(load-relative "loadtest.rkt")

(Section 'prompt)

;; ----------------------------------------

(define (test-breaks-ok)
  (err/rt-test (break-thread (current-thread)) exn:break?))


(test (void) call-with-continuation-prompt void)
(test (void) call-with-continuation-prompt void (default-continuation-prompt-tag))
(test (void) call-with-continuation-prompt void (default-continuation-prompt-tag) list)
(test '() call-with-continuation-prompt list (default-continuation-prompt-tag) void)
(test '(1) call-with-continuation-prompt list (default-continuation-prompt-tag) void 1)
(test '(1 2) call-with-continuation-prompt list (default-continuation-prompt-tag) void 1 2)
(test '(1 2 3) call-with-continuation-prompt list (default-continuation-prompt-tag) void 1 2 3)
(test '(1 2 3 4 5 6 7 8 9 10) call-with-continuation-prompt list (default-continuation-prompt-tag) void 
      1 2 3 4 5 6 7 8 9 10)

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
     (with-syntax ([call/cc (datum->syntax stx 'call/cc)]
                   [let/cc (datum->syntax stx 'let/cc)]
                   [call-with-continuation-prompt
                    (datum->syntax stx 'call-with-continuation-prompt)]
                   [thread (datum->syntax stx 'thread)])
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

;; ----------------------------------------

(load-relative "prompt-tests.rkt")

;; ----------------------------------------

;; Run the whole thing in a thread with no prompts around evaluation.
;; This tests the special case of the implicit prompt at the start
;; of a thread.
(thread-wait
 (thread
  (lambda ()
    (namespace-set-variable-value! 'running-prompt-tests-in-thread? #t)
    (let ([p (open-input-file (build-path
                               (or (current-load-relative-directory)
                                   (current-directory))
                               "prompt-tests.rkt"))])
      (let loop ()
        (let ([r (read-syntax (object-name p) p)])
          (unless (eof-object? r)
            (eval r)
            (loop))))))))

;; ----------------------------------------

(report-errs)
