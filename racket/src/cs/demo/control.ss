(import (rumble))

(define-syntax check
  (syntax-rules ()
    [(_ a b)
     (begin
       #;(printf "try ~s\n" 'a)
       (let ([v a])
         (unless (equal? v b)
           (error 'check (format "failed ~s => ~s" 'a v)))))]))

(define check-abort-tag (make-continuation-prompt-tag 'check-abort))

(define-syntax check-error
  (syntax-rules ()
    [(_ a s)
     (let ([v (call-with-continuation-prompt
               (lambda ()
                 (with-continuation-mark
                     exception-handler-key
                   (lambda (exn)
                     (if (exn? exn)
                         (abort-current-continuation
                          check-abort-tag
                          (lambda () (exn-message exn)))
                         exn))
                   a))
               check-abort-tag)]
           [es s])
       (unless (and (string? v)
                    (>= (string-length v) (string-length es))
                    (string=? es (substring v 0 (string-length es))))
         (error 'check (format "failed ~s != ~s" v es))))]))

(define tag1 (make-continuation-prompt-tag 'tag1))
(define tag2 (make-continuation-prompt-tag 'tag2))

(check (eq? (make-continuation-prompt-tag)
            (make-continuation-prompt-tag))
       #f)

(check (call-with-continuation-prompt
        (lambda () 10))
       10)

(check (call-with-continuation-prompt
        (lambda () 10)
        tag1)
       10)

(check (let ([saved #f])
         (let ([a (call-with-continuation-prompt
                   (lambda ()
                     (+ 10
                        (call-with-composable-continuation
                         (lambda (k)
                           (set! saved k)
                           12)
                         tag1)))
                   tag1)])
           (list a
                 (|#%app| saved -12))))
       (list 22 -2))

(check (let ([saved #f])
         (let ([a (call-with-continuation-prompt
                   (lambda ()
                     (+ 10
                        (call-with-continuation-prompt
                         (lambda ()
                           (call-with-composable-continuation
                            (lambda (k)
                              (set! saved k)
                              12)
                            tag1))
                         tag2)))
                   tag1)])
           (list a
                 (|#%app| saved -12))))
       (list 22 -2))

;; Shouldn't take long or use much memory:
(check (call-with-continuation-prompt
        (lambda ()
          (let loop ([n 1000000])
            (call-with-composable-continuation
             (lambda (k)
               (if (zero? n)
                   'ok
                   ;; In tail position:
                   (loop (sub1 n))))
             tag1)))
        tag1)
       'ok)

;; Also shouldn't take long or use much memory:
(check (let ([old-k (lambda (p) (p))]
             [n 100000])
         (call-with-continuation-prompt
          (lambda ()
            (let loop ()
              ((call-with-composable-continuation
                (lambda (k)
                  (let ([prev-k old-k])
                    (set! old-k k)
                    (|#%app| prev-k (lambda ()
                                      (call-with-composable-continuation
                                       (lambda (k)
                                         (cond
                                          [(zero? n)
                                           (lambda () 'also-ok)]
                                          [else
                                           (set! n (sub1 n))
                                           loop])))))))
                tag1))))
          tag1))
       'also-ok)

(check (let ([t (make-continuation-prompt-tag)])
         (call-with-continuation-prompt
          (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (abort-current-continuation
                t
                17))
             (make-continuation-prompt-tag)))
          t
          values))
       17)

(check (let ([syms null])
         (let ([v (dynamic-wind
                   (lambda () (set! syms (cons 'in syms)))
                   (lambda () 'ok)
                   (lambda () (set! syms (cons 'out syms))))])
           (cons v syms)))
       '(ok out in))

(check (let ([syms null])
         (let ([v (call-with-current-continuation
                   (lambda (esc)
                     (dynamic-wind
                      (lambda () (set! syms (cons 'in syms)))
                      (lambda () (|#%app| esc 'esc))
                      (lambda () (set! syms (cons 'out syms))))))])
           (cons v syms)))
       '(esc out in))

(check (let ([syms null])
         (let ([v (call-with-current-continuation
                   (lambda (esc)
                     (dynamic-wind
                      (lambda () (set! syms (cons 'in syms)))
                      (lambda ()
                        (call-with-continuation-prompt
                         (lambda ()
                           'in-prompt)))
                      (lambda () (set! syms (cons 'out syms))))))])
           (cons v syms)))
       '(in-prompt out in))

(check (let ([saved #f]
             [syms null])
         (let ([a (call-with-continuation-prompt
                   (lambda ()
                     (dynamic-wind
                      (lambda () (set! syms (cons 'in syms)))
                      (lambda ()
                        (+ 10
                           (call-with-composable-continuation
                            (lambda (k)
                              (set! saved k)
                              12)
                            tag1)))
                      (lambda () (set! syms (cons 'out syms)))))
                   tag1)])
           (let ([b (|#%app| saved -10)])
             (list a
                   b
                   syms))))
       (list 22 0 '(out in out in)))

(check (let ([saved #f])
         (with-continuation-mark
             'x 0
             (let ([a (call-with-continuation-prompt
                       (lambda ()
                         (with-continuation-mark
                             'x 1
                             (begin
                               (call-with-composable-continuation
                                (lambda (k)
                                  (set! saved k)))
                               (continuation-mark-set->list
                                (current-continuation-marks)
                                'x)))))])
               (list a
                     (|#%app| saved #f)))))
       '((1) (1 0)))

(check (call-with-current-continuation
        (lambda (k)
          (|#%app| k 0)))
       0)

(check (call-with-current-continuation
        (lambda (k)
          (call-with-continuation-prompt
           (lambda ()
             (|#%app| k 100))
           tag1)))
       100)

(check (let ([syms null])
         (let ([saved #f])
           (let ([v
                  (call-with-continuation-prompt
                   (lambda ()
                     ;; This metacontinuation frame will be shared between the
                     ;; capture and invocation:
                     (dynamic-wind
                      (lambda () (set! syms (cons 'in0 syms)))
                      (lambda ()
                        (let ([a (call-with-continuation-prompt
                                  (lambda ()
                                    ;; This metacontinuation frame will not
                                    ;; be shared:
                                    (dynamic-wind
                                     (lambda () (set! syms (cons 'in syms)))
                                     (lambda ()
                                       (+ 10
                                          (call-with-current-continuation
                                           (lambda (k)
                                             (set! saved k)
                                             12)
                                           tag1)))
                                     (lambda () (set! syms (cons 'out syms)))))
                                  tag1)])
                          (let ([b (call-with-continuation-prompt
                                    (lambda ()
                                      (|#%app| saved -8))
                                    tag1)])
                            (list a b))))
                      (lambda () (set! syms (cons 'out0 syms))))))])
             (list v syms))))
       (list (list 22 2) '(out0 out in out in in0)))

;; ----------------------------------------
;; Escape continuations

(check (call-with-escape-continuation
        (lambda (k)
          (+ 1 (|#%app| k 'esc))))
       'esc)

(check (let-values ([(k ek)
                     (call-with-continuation-prompt
                      (lambda ()
                        (call-with-escape-continuation
                         (lambda (ek)
                           (let-values ([(k0 ek0)
                                         ((call-with-composable-continuation
                                           (lambda (k)
                                             (lambda () (values k ek)))))])
                             (values k0 (box ek0)))))))])
         (let-values ([(k2 ek2)
                       (|#%app| k (lambda () (|#%app| (unbox ek) 'none 'skip)))])
           ek2))
       'skip)

(check-error (|#%app| (call-with-escape-continuation
                       (lambda (k) k)))
             "continuation application: attempt to jump into an escape continuation")

(check (with-continuation-mark
           'x 1
           (call-with-escape-continuation
            (lambda (k)
              (with-continuation-mark
                  'x 2
                  (continuation-mark-set->list (rumble:continuation-marks k) 'x)))))
       '(1))

(check-error (rumble:continuation-marks (call-with-escape-continuation
                                         (lambda (k) k)))
             "continuation application: escape continuation not in the current continuation")

;; ----------------------------------------
;; Barriers

(check (call-with-continuation-barrier
        (lambda ()
          'ok))
       'ok)

(check-error (call-with-continuation-prompt
              (lambda ()
                (call-with-continuation-barrier
                 (lambda ()
                   (call-with-composable-continuation
                    (lambda (k)
                      k)
                    (make-continuation-prompt-tag))))))
             "call-with-composable-continuation: continuation includes no prompt with the given tag")

(check-error (call-with-continuation-prompt
              (lambda ()
                (call-with-continuation-barrier
                 (lambda ()
                   (call-with-composable-continuation
                    (lambda (k)
                      k))))))
             "call-with-composable-continuation: cannot capture past continuation barrier")

(check-error (let ([k (call-with-continuation-barrier
                       (lambda ()
                         (call-with-current-continuation
                          (lambda (k)
                            k))))])
               (|#%app| k void))
             "continuation application: attempt to cross a continuation barrier")

;; ----------------------------------------
;; Continuation marks

(printf "Constant-time `continuation-mark-set-first` makes these tests fast enough...\n")

;; Caching within a metacontinuation frame
(let ([N 100000])
  (check (let loop ([n N])
           (cond
            [(zero? n)
             (check (length (continuation-mark-set->list
                             (current-continuation-marks)
                             'there))
                    N)
             n]
            [else
             (if (continuation-mark-set-first #f 'not-there #f)
                 'oops
                 (with-continuation-mark
                     'there n
                     (- (loop (sub1 n)) 1)))]))
         (- N)))

;; Caching across metacontinuation frames
(let ([N 10000])
  (check (let loop ([n N])
           (cond
            [(zero? n)
             (check (length (continuation-mark-set->list
                             (current-continuation-marks)
                             'there))
                    N)
             n]
            [else
             (if (continuation-mark-set-first #f 'not-there #f)
                 'oops
                 (call-with-continuation-prompt
                  (lambda ()
                    (with-continuation-mark
                        'there n
                        (- (loop (sub1 n)) 1)))
                  tag1))]))
         (- N)))

(printf "Done.\n")

(check (call-with-immediate-continuation-mark
        'not-there
        (lambda (v) v))
       #f)
(check (call-with-immediate-continuation-mark
        'not-there
        (lambda (v) v)
        'no)
       'no)
(check (with-continuation-mark
           'there 1
           (call-with-immediate-continuation-mark
            'there
            (lambda (v) v)))
       1)
(check (with-continuation-mark
           'there 1
           (list
            (call-with-immediate-continuation-mark
             'there
             (lambda (v) v))))
       '(#f))

(define (non-tail v) (values v))

(check (with-continuation-mark
           'x1 1
           (with-continuation-mark
               'x2 1
               (non-tail
                (with-continuation-mark
                    'x1 2
                    (non-tail
                     (with-continuation-mark
                         'x2 3
                         (values
                          (continuation-mark-set->list*
                           (current-continuation-marks)
                           '(x1 x2)
                           'nope
                           (default-continuation-prompt-tag)))))))))
       '(#(nope 3) #(2 nope) #(1 1)))

;; Make sure caching doesn't ignore the prompt tag
;; for a continuation-mark lookup
(check (with-continuation-mark
           'x 1
           (non-tail
            (with-continuation-mark
                'y 2
                (call-with-continuation-prompt
                 (lambda ()
                   (call-with-continuation-prompt
                    (lambda ()
                      (let ([a (continuation-mark-set-first #f 'x)])
                        (list a
                              (continuation-mark-set-first #f 'x #f tag1))))
                    tag2))
                 tag1))))
       '(1 #f))

;; ----------------------------------------
;; Engines

(define engine-tag (default-continuation-prompt-tag))

(define e (make-engine (lambda () 'done) engine-tag #f #f))
(check (cdr (e 100 void list vector))
       '(done))

(define e-forever (make-engine (lambda () (let loop () (loop))) engine-tag #f #f))
(check (vector? (e-forever 10 void list vector))
       #t)

(define e-10 (make-engine (lambda () 
                            (let loop ([n 10])
                              (cond
                               [(zero? n)
                                (engine-return 1 2 3)
                                (loop 0)]
                               [else
                                (engine-block)
                                (loop (sub1 n))])))
                          engine-tag
                          #f #f))
(check (let ([started 0])
         (let loop ([e e-10] [n 0])
           (e 100
              (lambda () (set! started (add1 started)))
              (lambda (remain a b c) (list a b c n started))
              (lambda (e)
                (loop e (add1 n))))))
       '(1 2 3 10 11))

;; Check that winders are not run on engine swaps:
(let ([pre 0]
      [post 0])
  (let ([e-10/dw (make-engine (lambda ()
                                (let loop ([n 10])
                                  (cond
                                   [(zero? n)
                                    (values 1 2 3 pre post)]
                                   [else
                                    (engine-block)
                                    (dynamic-wind
                                     (lambda () (set! pre (add1 pre)))
                                     (lambda () (loop (sub1 n)))
                                     (lambda () (set! post (add1 post))))])))
                              engine-tag
                              #f #f)])
    (check (let loop ([e e-10/dw] [n 0])
             (e 200
                void
                (lambda (remain a b c pre t-post) (list a b c pre t-post post n))
                (lambda (e)
                  (loop e (add1 n)))))
           '(1 2 3 10 0 10 10))))

;; ----------------------------------------
;; Thread cells (which are really engine cells):

(let ([ut (make-thread-cell 1)]
      [pt (make-thread-cell 100 #t)])
  (define (gen)
    (define u-old (thread-cell-ref ut))
    (define p-old (thread-cell-ref pt))
    (thread-cell-set! ut (add1 u-old))
    (thread-cell-set! pt (add1 p-old))
    (list u-old
          p-old
          (make-engine gen engine-tag #f #f)
          (thread-cell-ref ut)
          (thread-cell-ref pt)))
  (define l1 ((make-engine gen engine-tag #f #f)
              100
              void
              (lambda (remain l) l)
              (lambda (e) (error 'engine "oops"))))
  (define l2 ((list-ref l1 2)
              100
              void
              (lambda (remain l) l)
              (lambda (e) (error 'engine "oops"))))
  (check (list-ref l1 0) 1)
  (check (list-ref l1 1) 100)
  (check (list-ref l1 3) 2)
  (check (list-ref l1 4) 101)
  (check (list-ref l2 0) 1)
  (check (list-ref l2 1) 101)
  (check (list-ref l2 3) 2)
  (check (list-ref l2 4) 102))

;; ----------------------------------------
;; Parameters:

(define my-param (make-parameter 'init))
(check (procedure? my-param) #t)
(let ([e (with-continuation-mark parameterization-key
             (extend-parameterization (continuation-mark-set-first #f parameterization-key) my-param 'set)
           (make-engine (lambda () (|#%app| my-param)) engine-tag #f #f))])
  (check (|#%app| my-param) 'init)
  (check (e 1000 void (lambda (remain v) v) (lambda (e) (error 'engine "oops"))) 'set))

(let ([also-my-param (make-derived-parameter my-param
                                             (lambda (v) (list v))
                                             (lambda (v) (box v)))])
  (check (procedure? also-my-param) #t)
  (check (|#%app| my-param) 'init)
  (with-continuation-mark parameterization-key
      (extend-parameterization (continuation-mark-set-first #f parameterization-key) also-my-param 'set)
    (begin
      (check (|#%app| my-param) '(set))
      (check (|#%app| also-my-param) '#&(set)))))

;; ----------------------------------------
;; Prompt-tag impersonators

(let ([tag1i (impersonate-prompt-tag tag1
                                     ;; handle
                                     (lambda (args) (list 'handle args))
                                     ;; abort:
                                     (lambda (args) (list 'abort args))
                                     ;; cc-guard:
                                     (lambda (result) (list 'cc-guard result))
                                     ;; call-triggered guard impersonator:
                                     (lambda (proc) (lambda (result)
                                                      (list 'cc-use (proc result)))))])
  (check (call-with-continuation-prompt
          (lambda ()
            (abort-current-continuation tag1 'bye))
          tag1
          (lambda (arg)
            (list 'aborted arg)))
         (list 'aborted 'bye))
  (check (call-with-continuation-prompt
          (lambda ()
            (abort-current-continuation tag1 'bye))
          tag1i
          (lambda (arg)
            (list 'aborted arg)))
         (list 'aborted (list 'handle 'bye)))
  (check (call-with-continuation-prompt
          (lambda ()
            (abort-current-continuation tag1i 'bye))
          tag1
          (lambda (arg)
            (list 'aborted arg)))
         (list 'aborted (list 'abort 'bye)))
  (check (call-with-continuation-prompt
          (lambda ()
            (call-with-current-continuation
             (lambda (k)
               (|#%app| k 'jump))
             tag1))
          tag1i
          (lambda (arg) 'oops))
  (list 'cc-guard 'jump))
  (check (call-with-continuation-prompt
          (lambda ()
            (call-with-current-continuation
             (lambda (k)
               (|#%app| k 'jump))
             tag1i))
          tag1
          (lambda (arg) 'oops))
  (list 'cc-use 'jump))
  (check (call-with-continuation-prompt
          (lambda ()
            (call-with-current-continuation
             (lambda (k)
               (|#%app| k 'jump))
             tag1i))
          tag1i
          (lambda (arg) 'oops))
         (list 'cc-use (list 'cc-guard 'jump)))
  (void))

;; ----------------------------------------
;; call-with-system-wind

(define e-sw (make-engine (let ([pre 0]
                                [post 0])
                            (lambda ()
                              (call-with-system-wind
                               (lambda ()
                                 (#%dynamic-wind
                                  (lambda ()
                                    (set! pre (add1 pre)))
                                  (lambda ()
                                    (let loop ([n 1000])
                                      (if (zero? n)
                                          (list pre post)
                                          (loop (sub1 n)))))
                                  (lambda ()
                                    (set! post (add1 post))))))))
                          engine-tag
                          #f #f))

(check (let ([prefixes 0])
         (let loop ([e e-sw] [i 0])
           (e 110
              (lambda () (set! prefixes (add1 prefixes)))
              (lambda (remain v) (list (> i 2)
                                       (= prefixes (add1 i))
                                       (- (car v) i)
                                       (- (cadr v) i)))
              (lambda (e) (loop e (add1 i))))))
       '(#t #t 1 0))

;; ----------------------------------------

(call-with-continuation-prompt
 (lambda ()
   (error 'demo "this is an intended error"))
 tag1)
