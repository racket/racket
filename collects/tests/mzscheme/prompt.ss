
(load-relative "loadtest.ss")

(Section 'prompt)

;; ----------------------------------------

(define (test-breaks-ok)
  (err/rt-test (break-thread (current-thread)) exn:break?))

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

;; ----------------------------------------

(load-relative "prompt-tests.ss")

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
                               "prompt-tests.ss"))])
      (let loop ()
        (let ([r (read-syntax (object-name p) p)])
          (unless (eof-object? r)
            (eval r)
            (loop))))))))

;; ----------------------------------------

(report-errs)
