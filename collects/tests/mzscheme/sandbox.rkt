
(load-relative "loadtest.rkt")

(Section 'sandbox)

(require scheme/sandbox)

;; test call-in-nested-thread*
(let ()
  (define (kill) (kill-thread (current-thread)))
  (define (shut) (custodian-shutdown-all (current-custodian)))
  (define-syntax-rule (nested body ...)
    (call-in-nested-thread* (lambda () body ...)))
  (define-syntax-rule (nested* body ...)
    (call-in-nested-thread* (lambda () body ...)
                            (lambda () 'kill)
                            (lambda () 'shut)))
  (test 1 values (nested 1))
  ;; propagates parameters
  (let ([p (make-parameter #f)])
    (nested (p 1))
    (test 1 p)
    (with-handlers ([void void]) (nested (p 2) (error "foo") (p 3)))
    (test 2 p))
  ;; propagates kill-thread
  (test (void) thread-wait
        (thread (lambda ()
                  (nested (kill))
                  ;; never reach here
                  (semaphore-wait (make-semaphore 0)))))
  ;; propagates custodian-shutdown-all
  (test (void) values
        (parameterize ([current-custodian (make-custodian)]) (nested (shut))))
  ;; test handlers parameters
  (test 'kill (lambda () (nested* (kill))))
  (test 'shut (lambda () (nested* (shut)))))

(let ([ev void])
  (define (make-evaluator! . args)
    (set! ev (apply make-evaluator args)))
  (define (make-base-evaluator! . args)
    (set! ev (apply make-evaluator 'scheme/base args)))
  (define (make-base-evaluator/reqs! reqs . args)
    (set! ev (apply make-evaluator 'scheme/base #:requires reqs args)))
  (define (make-module-evaluator! . args)
    (set! ev (apply make-module-evaluator args)))
  (define (run thunk)
    (with-handlers ([void (lambda (e) (list 'exn: e))])
      (call-with-values thunk (lambda vs (cons 'vals: vs)))))
  (define (run* thunk)
    (with-handlers ([void (lambda (e) (list 'exn: e))])
      (call-with-values thunk
          (case-lambda [(x) (and x #t)] [vs (cons 'vals: vs)]))))
  (define (e-match? re run thunk)
    (let ([x (run thunk)])
      (if (and (list? x) (= 2 (length x)) (eq? 'exn: (car x)) (exn? (cadr x)))
        (let ([m (exn-message (cadr x))])
          (or (regexp-match? re m) (list 'bad-exception-message: m)))
        x)))
  (define-syntax thunk (syntax-rules () [(thunk b ...) (lambda () b ...)]))
  (define-syntax t
    (syntax-rules (--eval-- --top-- => <= =err> <err=)
      [(t -?-) (void)]
      [(t -?- --eval-- more ...) (t --eval-- more ...)]
      [(t -?- --top--  more ...) (t --top--  more ...)]
      [(t --eval-- E)         (test #t            run* (thunk (ev `E)))]
      [(t --top--  E)         (test #t            run* (thunk E))]
      [(t --eval-- E => R)    (test `(vals: ,R)   run  (thunk (ev `E)))]
      [(t --top--  E => R)    (test `(vals: ,R)   run  (thunk E))]
      [(t --eval-- E =err> R) (test #t e-match? R run  (thunk (ev `E)))]
      [(t --top--  E =err> R) (test #t e-match? R run  (thunk E))]
      [(t -?- E => R more ...)    (begin (t -?- E => R) (t -?- more ...))]
      [(t -?- E =err> R more ...) (begin (t -?- E =err> R) (t -?- more ...))]
      [(t -?- R <= E more ...)    (t -?- E => R more ...)]
      [(t -?- R <err= E more ...) (t E =err> R more ...)]
      ;; last so it doesn't match the above
      [(t -?- E more ...) (begin (t -?- E) (t -?- more ...))]))
  (define (make-prog . lines)
    (apply string-append (map (lambda (l) (string-append l "\n")) lines)))

  (t

   ;; basic stuff, limits
   --top--
   (make-base-evaluator!
    (make-prog "(define x 1)"
               "(define (id x) x)"
               "(define (plus1 x) x)"
               "(define (loop) (loop))"
               "(define (memory x) (make-vector x))"))
   (set-eval-limits ev 0.5 5)
   --eval--
   x => 1
   (id 1) => 1
   (id (plus1 x)) => 1
   (define id2 id)
   (id2 (id x)) => 1
   blah =err> "before its definition"
   ;; using a string for an input
   "1" => 1
   "(+ 1 2) x (define y 9) y (set! y 99) y" => 99
   "bad\"string" =err> "expected a closing"
   "bad(string" =err> "expected a .\\)."
   "bad)string" =err> "unexpected .\\)."
   "(set! y 999) (string" =err> "expected a .\\)."
   y => 99
   "(set! y 999) (if)" =err> "if: bad syntax"
   y => 999
   ;; test limits
   (loop) =err> "out of time"
   --top--
   (when (custodian-memory-accounting-available?)
     (t --eval-- (memory 3000000) =err> "out of memory"))
   ;; test parameter settings (tricky to get this right since
   ;; with-limits runs stuff in a different thread)
   (set-eval-limits ev #f #f)
   --eval--
   (define p (make-parameter 0))
   (p) => 0
   (p 1)
   (p) => 1
   (thread-wait (thread (lambda () (p 100))))
   (p) => 1
   --top--
   (set-eval-limits ev 1 3)
   --eval--
   (p) => 1
   (p 2)
   (p) => 2
   (thread-wait (thread (lambda () (p 100))))
   (p) => 2
   --top--
   (set-eval-limits ev #f #f)
   --eval--
   (p) => 2
   ;; breaking
   --top--
   (thread (lambda () (sleep 1) (break-evaluator ev)))
   --eval--
   (sleep 2) =err> "user break"
   (printf "x = ~s\n" x) => (void)
   ;; termination
   --eval--
   ,eof =err> "terminated .eof.$"
   123  =err> "terminated .eof.$"
   ,eof =err> "terminated .eof.$"

   ;; other termination messages
   --top-- (make-base-evaluator!) (kill-evaluator ev)
   --eval-- 123 =err> "terminated .evaluator-killed.$"

   ;; nested calls are forbidden
   --top-- (make-base-evaluator!)
   --eval-- (,ev 1) =err> "nested evaluator call"

   ;; eval-limits apply to the sandbox creation too
   --top--
   (parameterize ([sandbox-eval-limits '(0.25 5)])
     (make-base-evaluator! '(sleep 2)))
   =err> "out of time"
   (when (custodian-memory-accounting-available?)
     (t --top--
        (parameterize ([sandbox-eval-limits '(2 2)])
          (make-base-evaluator! '(define a (for/list ([i (in-range 10)])
                                             (collect-garbage)
                                             (make-bytes 500000)))))
        =err> "out of memor(?:y)"))

   ;; i/o
   --top--
   (parameterize ([sandbox-input "3\n"]
                  [sandbox-output 'string]
                  [sandbox-error-output current-output-port])
     (make-base-evaluator! '(define x 123)))
   --eval-- (printf "x = ~s\n" x) => (void)
   --top--  (get-output ev) => "x = 123\n"
   --eval-- (printf "x = ~s\n" x) => (void)
   --top--  (get-output ev) => "x = 123\n"
   --eval-- (printf "x*2 = ~s\n" (+ x x)) => (void)
            (printf "x*10 = ~s\n" (* 10 x)) => (void)
   --top--  (get-output ev) => "x*2 = 246\nx*10 = 1230\n"
   --eval-- (printf "x*(read) = ~s\n" (* x (read))) => (void)
   --top--  (get-output ev) => "x*(read) = 369\n"
   --eval-- (begin (printf "a\n") (fprintf (current-error-port) "b\n"))
   --top--  (get-output ev) => "a\nb\n"
            (get-error-output ev) => #f
   --top--
   (parameterize ([sandbox-output 'string] [sandbox-error-output 'string])
     (make-base-evaluator!))
   --eval-- (begin (printf "a\n") (fprintf (current-error-port) "b\n"))
   --top--  (get-output ev) => "a\n"
            (get-error-output ev) => "b\n"
   --top--
   (parameterize ([sandbox-input 'pipe]
                  [sandbox-output 'bytes]
                  [sandbox-error-output current-output-port]
                  [sandbox-eval-limits '(0.25 10)])
     (make-base-evaluator! '(define x 123)))
   --eval--  (begin (printf "x = ~s\n" x)
                    (fprintf (current-error-port) "err\n"))
   --top--   (get-output ev) => #"x = 123\nerr\n"
             (put-input ev "blah\n")
             (put-input ev "blah\n")
   --eval--  (read-line) => "blah"
             (printf "line = ~s\n" (read-line))
   --top--   (get-output ev) => #"line = \"blah\"\n"
   --eval--  (read-line) =err> "out of time"
   --top--   (put-input ev "blah\n")
             (put-input ev eof)
   --eval--  (read-line) => "blah"
             (read-line) => eof
             (read-line) => eof
   ;; test kill-evaluator here
   --top--
   (kill-evaluator ev) => (void)
   --eval--
   x =err> "terminated .evaluator-killed.$"
   y =err> "terminated .evaluator-killed.$"
   ,eof =err> "terminated .evaluator-killed.$"
   --top--
   (let-values ([(i1 o1) (make-pipe)] [(i2 o2) (make-pipe)])
     ;; o1 -> i1 -ev-> o2 -> i2
     (parameterize ([sandbox-input i1] [sandbox-output o2])
       (make-base-evaluator! '(define x 123)))
     (t --eval-- (printf "x = ~s\n" x) => (void)
        --top--  (read-line i2) => "x = 123"
        --eval-- (printf "x = ~s\n" x) => (void)
        --top--  (read-line i2) => "x = 123"
        --eval-- (printf "x*2 = ~s\n" (+ x x)) => (void)
                 (printf "x*10 = ~s\n" (* 10 x)) => (void)
        --top--  (read-line i2) => "x*2 = 246"
                 (read-line i2) => "x*10 = 1230"
                 (fprintf o1 "3\n")
        --eval-- (printf "x*(read) = ~s\n" (* x (read))) => (void)
        --top--  (read-line i2) => "x*(read) = 369"
        ))

   ;; sexprs as a program
   --top--
   (make-base-evaluator! '(define id (lambda (x) x)))
   --eval--
   (id 123) => 123
   --top--
   (make-base-evaluator! '(define id (lambda (x) x)) '(define fooo 999))
   --eval--
   (id fooo) => 999

   ;; test source locations too
   --top--
   (make-base-evaluator! 0 1 2 '(define foo))
   =err> "program:4:0: define"

   ;; empty program for clean repls
   --top--
   (make-evaluator! '(begin))
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (make-base-evaluator!)
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (make-base-evaluator! '(define x (+ 1 2 3)))
   --eval--
   (define x (+ x 10)) =err> "cannot re-define a constant"

   ;; whole program argument
   --top--
   (make-module-evaluator! '(module foo scheme/base (define x 1)))
   --eval--
   x => 1
   --top--
   (make-module-evaluator! '(module foo scheme/base (provide x) (define x 1)))
   --eval--
   x => 1
   (define x 2) =err> "cannot re-define a constant"

   ;; limited FS access, allowed for requires
   --top--
   (let* ([tmp       (make-temporary-file "sandboxtest~a" 'directory)]
          [strpath   (lambda xs (path->string (apply build-path xs)))]
          [racketlib (strpath (collection-path "racket"))]
          [list-lib  (strpath racketlib "list.rkt")]
          [list-zo   (strpath racketlib "compiled" "list_rkt.zo")]
          [test-lib  (strpath tmp "sandbox-test.rkt")]
          [test-zo   (strpath tmp "compiled" "sandbox-test_ss.zo")]
          [test2-lib (strpath tmp "sandbox-test2.rkt")]
          [test2-zo  (strpath tmp "compiled" "sandbox-test2_ss.zo")])
     (t --top--
        (make-base-evaluator!)
        --eval--
        ;; reading from collects is allowed
        (list? (directory-list ,racketlib))
        (file-exists? ,list-lib) => #t
        (input-port? (open-input-file ,list-lib)) => #t
        ;; writing is forbidden
        (open-output-file ,list-lib) =err> "`write' access denied"
        ;; reading from other places is forbidden
        (directory-list ,tmp) =err> "`read' access denied"
        ;; no network too
        (require racket/tcp)
        (tcp-listen 12345) =err> "network access denied"
        --top--
        ;; reading from a specified require is fine
        (with-output-to-file test-lib
          (lambda ()
            (printf "~s\n" '(module sandbox-test racket/base
                              (define x 123) (provide x)))))
        (make-base-evaluator/reqs! `(,test-lib))
        --eval--
        x => 123
        (length (with-input-from-file ,test-lib read)) => 5
        ;; the directory is still not kosher
        (directory-list ,tmp) =err> "`read' access denied"
        --top--
        ;; should work also for module evaluators
        ;; --> NO!  Shouldn't make user code require whatever it wants
        ;; (make-module-evaluator!
        ;;   `(module foo racket/base (require (file ,test-lib))))
        ;; --eval--
        ;; x => 123
        ;; (length (with-input-from-file ,test-lib read)) => 5
        ;; ;; the directory is still not kosher
        ;; (directory-list tmp) =err> "file access denied"
        --top--
        ;; explicitly allow access to tmp, and write access to a single file
        (make-directory (build-path tmp "compiled"))
        (parameterize ([sandbox-path-permissions
                        `((read ,tmp) (write ,test-zo)
                          ,@(sandbox-path-permissions))])
          (make-base-evaluator!))
        --eval--
        (length (with-input-from-file ,test-lib read)) => 5
        (list? (directory-list ,tmp))
        (open-output-file ,(build-path tmp "blah")) =err> "access denied"
        (delete-directory ,(build-path tmp "blah")) =err> "access denied"
        (list? (directory-list ,racketlib))
        ;; we can read/write/delete list-zo, but we can't load bytecode from
        ;; it due to the code inspector
        (copy-file ,list-zo ,test-zo) => (void)
        (copy-file ,test-zo ,list-zo) =err> "access denied"
        (load/use-compiled ,test-lib) => (void)
        (require 'list) =err> "access from an uncertified context"
        (delete-file ,test-zo) => (void)
        (delete-file ,test-lib) =err> "`delete' access denied"
        --top--
        ;; a more explicit test of bytcode loading, allowing rw access to the
        ;; complete tmp directory, but read-bytecode only for test2-lib
        (parameterize ([sandbox-path-permissions
                        `((write ,tmp) (read-bytecode ,test2-lib)
                          ,@(sandbox-path-permissions))])
          (make-base-evaluator!))
        --eval--
        (define (cp from to)
          (when (file-exists? to) (delete-file to))
          (copy-file from to))
        (cp ,list-lib ,test-lib)  (cp ,list-zo ,test-zo)
        (cp ,list-lib ,test2-lib) (cp ,list-zo   ,test2-zo)
        ;; bytecode from test-lib is bad, even when we can read/write to it
        (load/use-compiled ,test-zo)
        (require 'list) =err> "access from an uncertified context"
        ;; bytecode from test2-lib is explicitly allowed
        (load/use-compiled ,test2-lib)
        (require 'list) => (void))
     ((dynamic-require 'racket/file 'delete-directory/files) tmp))

   ;; languages and requires
   --top--
   (make-evaluator! '(special r5rs) "(define x (eq? 'x 'X))")
   --eval--
   x => #t
   --top--
   (make-base-evaluator! "(define l null)")
   --eval--
   (cond [null? l 0]) => 0
   (last-pair l) =err> "reference to an identifier"
   --top--
   (make-evaluator! '(special beginner)
                    (make-prog "(define l null)" "(define x 3.5)"))
   --eval--
   (cond [null? l 0]) =err> "expected an open parenthesis"
   --top--
   (eq? (ev "6") (ev "(sub1 (* 2 3.5))"))
   (eq? (ev "6") (ev "(sub1 (* 2 x))"))
   --top--
   (make-base-evaluator/reqs! '(racket/list))
   --eval--
   (last-pair '(1 2 3)) => '(3)
   (last-pair null) =err> "expected argument of type"

   ;; coverage
   --top--
   (parameterize ([sandbox-coverage-enabled #t])
     (make-base-evaluator!
      (make-prog "(define (foo x) (+ x 1))"
                 "(define (bar x) (+ x 2))"
                 "(equal? (foo 3) 4)")))
   (pair? (get-uncovered-expressions ev))
   (pair? (get-uncovered-expressions ev #t))
   --eval--
   (foo 3) => 4
   (bar 10) => 12
   --top--
   (null? (get-uncovered-expressions ev #f))
   (pair? (get-uncovered-expressions ev)) ; no-tests coverage still the same

   ;; misc parameters
   --top--
   (parameterize ([sandbox-init-hook
                   (let ([old (sandbox-init-hook)])
                     (lambda ()
                       (old)
                       (compile-enforce-module-constants #f)
                       (compile-allow-set!-undefined #t)))])
     (make-base-evaluator! '(define x 123)))
   --eval--
   (set! x 456) ; would be an error without the `enforce' parameter
   x => 456
   (set! y 789) ; would be an error without the `set!' parameter
   y => 789

   ;; test that output is also collected under the limit
   --top--
   (parameterize ([sandbox-output 'bytes]
                  [sandbox-error-output current-output-port]
                  [sandbox-memory-limit 2]
                  [sandbox-eval-limits '(0.25 1)])
     (make-base-evaluator!))
   ;; GCing is needed to allow these to happen (note: the memory limit is very
   ;; tight here, this test usually fails if the sandbox library is not
   ;; compiled)
   (let ([t (lambda ()
              (t --eval-- (display (make-bytes 400000 65)) (collect-garbage)
                 --top--  (bytes-length (get-output ev)) => 400000))])
     ;; can go arbitrarily high here
     (for ([i (in-range 20)]) (t)))

   ;; test that killing the thread, shutting the custodian, or calling `exit'
   ;; works fine first try it without limits (limits imply a nested
   ;; thread/custodian)
   --top--
   (let ()
     (define (3x2-terminations)
       (t --top-- (make-base-evaluator!) --eval--
          (kill-thread (current-thread)) =err> "terminated .thread-killed.$"
          --top-- (make-base-evaluator!) --eval--
          (custodian-shutdown-all (current-custodian))
          =err> "terminated .custodian-shutdown.$"
          --top-- (make-base-evaluator!) --eval--
          (exit) =err> "terminated .exited.$"
          ;; now test that it's fine when called directly
          --top--
          (make-base-evaluator!)
          (call-in-sandbox-context ev
            (lambda () (kill-thread (current-thread))))
          =err> "terminated .thread-killed.$"
          (make-base-evaluator!)
          (call-in-sandbox-context ev
            (lambda () (custodian-shutdown-all (current-custodian))))
          =err> "terminated .custodian-shutdown.$"
          (make-base-evaluator!)
          (call-in-sandbox-context ev exit) =err> "terminated .exited.$"))
     (define (test-terminations)
       ;; try without, then with per-expression limits
       (parameterize ([sandbox-eval-limits #f]) (3x2-terminations))
       (3x2-terminations))
     (test-terminations))

   ;; when an expression is out of memory, the sandbox should stay alive
   --top--
   (when (custodian-memory-accounting-available?)
     (t --top--
        (parameterize ([sandbox-eval-limits '(2 5)]
                       [sandbox-memory-limit 100])
          (make-base-evaluator!))
        --eval--
        (define a '())
        (define b 1)
        (length
         (for/fold ([v null]) ([i (in-range 20)])
           ;; increases size of sandbox: it's reachable from it (outside of
           ;; this evaluation) because `a' is defined there
           (set! a (cons (make-bytes 500000) a))
           (collect-garbage)
           ;; increases size of the current evaluation
           (cons (make-bytes 500000) v)))
        =err> "out of memo(?:ry)"
        b => 1))

   ))

(report-errs)
