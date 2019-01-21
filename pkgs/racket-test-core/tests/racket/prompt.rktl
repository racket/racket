
(load-relative "loadtest.rktl")

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

(load-relative "prompt-tests.rktl")

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
                               "prompt-tests.rktl"))])
      (let loop ()
        (let ([r (read-syntax (object-name p) p)])
          (unless (eof-object? r)
            (eval r)
            (loop))))))))

;; ----------------------------------------
;; Check that a constant-space loop doesn't
;; accumulate memory (test by Nicolas Oury)

(when (custodian-memory-accounting-available?) ; as a check for 3m-ness
  (define prompt1 (make-continuation-prompt-tag 'p1))
  (define prompt2 (make-continuation-prompt-tag 'p2))

  (define (capture-and-abort prompt-tag)
    (call-with-composable-continuation
     (lambda (k) (abort-current-continuation prompt-tag k))
     prompt-tag))

  (define (go i)
    (call-with-continuation-prompt
     (lambda ()
       (call-with-continuation-prompt
        (lambda()
          (for ((j i))
            (capture-and-abort prompt1)
            (capture-and-abort prompt2)))
        prompt2))
     prompt1))

  (test (void) go 100000))

;; ----------------------------------------
;; A GC test:

(require racket/control)

(let ()
  (define (loop n)
    (shift s (if (zero? n)
                 (void)
                 (loop (sub1 n)))))
  (define (overflow-prompt-go)
    (reset (loop 50000)))
  (test (void) overflow-prompt-go))

;; ----------------------------------------
;; control proxies

(define imp-tag
  (impersonate-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x) (* x 2))
   (lambda (x) (+ x 1))))

(define imp-tag-2
  (impersonate-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x y) (values (* x 2) (* y 2)))
   (lambda (x y) (values (+ x 1) (+ y 1)))))

(define imp-tag-3
  (impersonate-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x y) (values (* x 2) (* y 2)))
   (lambda (x y) x)))

(define imp-tag-4
  (impersonate-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x y) (values x x x))
   (lambda (x y) (values x y))))

(define cha-tag
  (chaperone-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x) (if (number? x) x (error "fail")))
   (lambda (x) x)))

(define bad-tag
  (chaperone-prompt-tag
   (make-continuation-prompt-tag)
   (lambda (x) 42)
   (lambda (x) x)))

(define (do-test tag . rst)
  (call-with-continuation-prompt
    (lambda ()
      (apply abort-current-continuation
             (cons tag rst)))
    tag
    (lambda x x)))

(define (in-prompt tag k . vs)
  (call-with-continuation-prompt
    (lambda () (apply k vs))
    tag))

;; make sure proxied tags are still tags
(test #t continuation-prompt-tag? imp-tag)

;; make sure proxies do the right thing
(test '(12) do-test imp-tag 5)
(test '(12 14) do-test imp-tag-2 5 6)
(err/rt-test (do-test imp-tag-2 5) exn:fail?)
(err/rt-test (do-test imp-tag-3 10 11) exn:fail?)
(err/rt-test (do-test imp-tag-4 10 11) exn:fail?)
(test '(7) do-test cha-tag 7)
(err/rt-test (do-test cha-tag "bad") exn:fail?)
(err/rt-test (do-test bad-tag 5) exn:fail?)

;; sanity checks
(test 5 in-prompt imp-tag call/cc (lambda (k) (k 5)) imp-tag)
(test 5 in-prompt imp-tag
      call-with-composable-continuation
      (lambda (k) (k 5))
      imp-tag)
(test #t in-prompt imp-tag
      continuation-prompt-available? imp-tag)

(call-with-continuation-prompt
  (lambda ()
    (with-continuation-mark 'mark 'val
      (test
        'val
        (compose (lambda (s) (continuation-mark-set-first s 'mark))
                 current-continuation-marks)
        imp-tag)))
  imp-tag)

(let ()
  (define abort-k
    (call-with-continuation-prompt
     (lambda ()
       (call/cc (lambda (k) k)))))

  (test
   "sx"
   call-with-continuation-prompt
   (lambda ()
     (+ 1 (abort-k "s")))
   (impersonate-prompt-tag (default-continuation-prompt-tag)
                           values
                           values
                           (lambda (s) (string-append s "x"))))

  (test-values
   '("st" "")
   (lambda ()
     (call-with-continuation-prompt
      (lambda ()
        (+ 1 (abort-k "s" "t")))
      (impersonate-prompt-tag (default-continuation-prompt-tag)
                              values
                              values
                              (lambda (s t) (values (string-append s t) ""))))))

  (let ([v (vector 1)])
    (test
     #t
     chaperone-of?
     (call-with-continuation-prompt
      (lambda ()
        (+ 1 (abort-k v)))
      (chaperone-prompt-tag (default-continuation-prompt-tag)
                            values
                            values
                            (lambda (v) (chaperone-vector v 
                                                          (lambda (x i v) v)
                                                          (lambda (x i v) v)))))
     v)))

(let ()
  (define abort-k/y
  (call-with-continuation-prompt
   (lambda ()
     (call/cc (lambda (k) k)
              (impersonate-prompt-tag (default-continuation-prompt-tag)
                                      values
                                      values
                                      values
                                      (lambda (proc)
                                        (lambda (v) (string-append v "y"))))))))

  (test
   "sy"
   call-with-continuation-prompt
   (lambda ()
     (+ 1 (abort-k/y "s")))))

(let ()
  (define really-abort-k
    (call-with-continuation-prompt
     (lambda ()
       (call/cc (lambda (k) (abort-current-continuation
                             (default-continuation-prompt-tag)
                             (lambda () k))))
       (abort-current-continuation
        (default-continuation-prompt-tag)
        (lambda () "a")))))
  
  (test
   "a"
   call-with-continuation-prompt
   (lambda ()
     (+ 1 (really-abort-k "s")))
   (impersonate-prompt-tag (default-continuation-prompt-tag)
                           values
                           values
                           (lambda (s) (string-append s "x")))))

;; ----------------------------------------
;; check that cc proc doesn't break abort proc

(let ()
  (define l null)

  (define cpt
    (chaperone-prompt-tag
     (make-continuation-prompt-tag)
     (λ (x) (set! l (cons "ho" l)) x)
     (λ (x) (set! l (cons "hi" l)) x)
     ;; commented out intentionally, see below
     (λ (x) x)))
  
  (call-with-continuation-prompt
   (λ ()
      (abort-current-continuation cpt 5))
   cpt
   (λ (x) (+ 1 x)))

  (test '("ho" "hi") values l))

;; ----------------------------------------
;; Check that when a continuation includes a continuation
;; application, that a captured requirement to apply a
;; contiuation-result guard (as added by an impersonator) is not
;; influenced by canceling the guard in a different use of the
;; continuation.

(let ()
  (define other-tag
    (make-continuation-prompt-tag))

  (define other-k #f)

  (define wacky-abort-k
    (call-with-continuation-prompt
     (lambda ()
       (define r
         (call/cc (lambda (k) (abort-current-continuation
                               (default-continuation-prompt-tag)
                               (lambda () k)))))
       (when (call/cc (lambda (k)
                        (set! other-k k)
                        #t)
                      other-tag)
         (abort-current-continuation
          (default-continuation-prompt-tag)
          (lambda () "a")))
       r)))

  (test
   "sx"
   call-with-continuation-prompt
   (lambda ()
     (define str
       (call-with-continuation-prompt
        (lambda ()
          (+ 1 (wacky-abort-k "s")))
        (impersonate-prompt-tag (default-continuation-prompt-tag)
                                values
                                values
                                (lambda (s) (string-append s "x")))))
     (if (equal? str "a")
         (other-k #f)
         str))
   other-tag))

;;----------------------------------------
;; check clean-up when aborting to initial prompt:

(let ([v '?])
  (sync
   (thread (lambda ()
             (with-continuation-mark
                 'x 1
               (abort-current-continuation
                (default-continuation-prompt-tag)
                (lambda ()
                  (set! v (continuation-mark-set-first #f 'x))))))))
  (test #f 'marks-reset v))

(let ([v 0])
  (sync
   (thread (lambda ()
             (dynamic-wind
                 void
                 (lambda ()
                   (abort-current-continuation
                    (default-continuation-prompt-tag)
                    (lambda ()
                      (abort-current-continuation
                       (default-continuation-prompt-tag)
                       void))))
                 (lambda () (set! v (add1 v)))))))
  (test 1 values v))

;;----------------------------------------
;; Check continuation sharing

;; This check is useful for the traditional Racket VM, but it isn't as
;; interesting on Chez Scheme --- where the sharing is more obvious in
;; the implementation but not exposed as `eq?` continuations
(when (eq? 'racket (system-type 'vm))
  (define (f x prev)
    (call/cc
     (lambda (k)
       (test (and (even? x)
                  (x . < . 10))
             eq?
             k
             prev)
       (cond
        [(zero? x) 'done]
        [(even? x) (or (f (sub1 x) k) #t)]
        [else (f (sub1 x) k)]))))

  (void (f 10 #f))
  (void
   (let ([v (call-with-composable-continuation
             (lambda (k)
               k))])
     (if (procedure? v)
         (v 'ok)
         (f 10 #f)))))

;;----------------------------------------
;; Check that a continuation doesn't retain the arguments
;; to the call to `call/cc` that created the continuation.

(unless (eq? 'cgc (system-type 'gc))
  (let ([ht (make-weak-hasheq)])
    (define l
      (for/list ([i 100])
        (call/cc (let ([p (lambda (k) (cons i k))])
                   (hash-set! ht p #t)
                   p))))
    (collect-garbage)
    (collect-garbage)
    ;; All of them should have been collected, but
    ;; since GC makes only asymptotic promises,
    ;; let's check that 3/4 were collected:
    (test #t < (hash-count ht) (* 1/4 (length l)))))

;;----------------------------------------
;; Check that it works to apply a continuation that shares with
;; an enclosing continuation, where a runstack overflow happens
;; between the continuations

(let ()
  (define N 100)
  (define N2 10)
  (define M 10)

  (define p (make-continuation-prompt-tag))

  (define (grab n m k-prev q)
    (cond
     [(positive? n)
      (let ([x (grab (sub1 n) m k-prev q)])
        (lambda () x))]
     [(positive? m)
      ((call/cc
        (lambda (k)
          (grab N2 (sub1 m) k q))
        p))]
     [(positive? q)
      (call-with-continuation-prompt
       (lambda ()
         (k-prev
          (lambda ()
            (grab N M void (sub1 q)))))
       p)]
     [else void]))

  (call-with-continuation-prompt
   (lambda ()
     (grab N M void 10))
   p))

;; ----------------------------------------
;; Check that a non-composable-continuation jump isn't
;; fooled by a dynamic-wind frame that is common to the
;; source and destination but not followed by all
;; matching frames.

(let ()
  (define accum null)
  (define (output! v) (set! accum (cons v accum)))
  (define (check-output! expect)
    (let ([got (reverse accum)])
      (set! accum null)
      (displayln got)
      (test expect values got)))

  ;; Make a composable continuation that holds one dynamic-wind frame
  (define (make-composable-dw pre post)
    (call-with-continuation-prompt
     (lambda ()
       (dynamic-wind
        (lambda () (output! pre))
        (lambda () ((call-with-composable-continuation
                (lambda (k) (lambda () k)))))
        (lambda () (output! post))))))

  (define dw1 (make-composable-dw "+1" "-1"))
  (check-output! '("+1" "-1"))

  (define dw2 (make-composable-dw "+2" "-2"))
  (check-output! '("+2" "-2"))

  (define dw3 (make-composable-dw "+3" "-3"))
  (check-output! '("+3" "-3"))

  (define (compose-dw a-dw b-dw)
    (lambda (f)
      (a-dw (lambda () (b-dw f)))))

  ;; compose dw1 and dw2, with "work" in the middle:
  ((compose-dw dw1 dw2) (lambda () (output! "work")))
  (check-output! '("+1" "+2" "work" "-2" "-1"))

  ;; Compose two composable continuations, and capture the composition
  ;; as non-composable:
  (define (make-non-composable dw)
    (call-with-continuation-prompt
     (lambda ()
       (dw
        (lambda ()
          ((call/cc
            (lambda (k) (lambda () k)))))))))

  (define dw2+dw1 (make-non-composable (compose-dw dw2 dw1)))
  (check-output! '("+2" "+1" "-1" "-2"))

  (define dw3+dw1 (make-non-composable (compose-dw dw3 dw1)))
  (check-output! '("+3" "+1" "-1" "-3"))

  (call-with-continuation-prompt
   (lambda ()
     (dw2+dw1 (lambda () (output! "inside")))))
  (check-output! '("+2" "+1" "inside" "-1" "-2"))

  (define dw3+dw2+dw1 (make-non-composable (compose-dw (compose-dw dw3 dw2) dw1)))
  (check-output! '("+3" "+2" "+1" "-1" "-2" "-3"))

  ;; From a dw3+dw2+d1 composition, jump to a dw2+dw1 composition;
  ;; even though the sourec and destination both have dw2 and dw1
  ;; innermost, the full chains are different, so the jump goes
  ;; all the way out of dw3+dw2+dw1 and back into dw2+dw1
  (call-with-continuation-prompt
   (lambda ()
     (dw3+dw2+dw1 (lambda () (dw2+dw1 (lambda () (output! "inside")))))))
  (check-output! '("+3" "+2" "+1" "-1" "-2" "-3" "+2" "+1" "inside" "-1" "-2"))

  ;; From a dw2+d1 composition, jump to a dw3+dw1 composition;
  ;; this one should still jump all the way out and all the way
  ;; back in:
  (call-with-continuation-prompt
   (lambda ()
     (dw2+dw1 (lambda () (dw3+dw1 (lambda () (output! "inside")))))))
  (check-output! '("+2" "+1" "-1" "-2" "+3" "+1" "inside" "-1" "-3")))

;;----------------------------------------

(report-errs)
