
(load-relative "loadtest.ss")

(Section 'sandbox)

(require (lib "sandbox.ss"))

(let ([ev void])
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
   (set! ev (make-evaluator 'mzscheme '()
              (make-prog "(define x 1)"
                         "(define (id x) x)"
                         "(define (plus1 x) x)"
                         "(define (loop) (loop))"
                         "(define (memory x) (make-vector x))")))
   (set-eval-limits ev 1 3)
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
     (t --eval-- (memory 1000000) =err> "out of memory"))
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
   ;; termination
   --eval--
   (printf "x = ~s\n" x) => (void)
   ,eof =err> "terminated"
   x =err> "terminated"
   ,eof =err> "terminated"

   ;; i/o
   --top--
   (set! ev (parameterize ([sandbox-input "3\n"]
                           [sandbox-output 'string]
                           [sandbox-error-output current-output-port])
              (make-evaluator 'mzscheme '() '(define x 123))))
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
   (set! ev (parameterize ([sandbox-output 'string]
                           [sandbox-error-output 'string])
              (make-evaluator 'mzscheme '())))
   --eval-- (begin (printf "a\n") (fprintf (current-error-port) "b\n"))
   --top--  (get-output ev) => "a\n"
            (get-error-output ev) => "b\n"
   --top--
   (set! ev (parameterize ([sandbox-input 'pipe]
                           [sandbox-output 'bytes]
                           [sandbox-error-output current-output-port]
                           [sandbox-eval-limits '(0.25 10)])
              (make-evaluator 'mzscheme '() '(define x 123))))
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
   x =err> "terminated"
   y =err> "terminated"
   ,eof =err> "terminated"
   --top--
   (let-values ([(i1 o1) (make-pipe)] [(i2 o2) (make-pipe)])
     ;; o1 -> i1 -ev-> o2 -> i2
     (set! ev (parameterize ([sandbox-input i1] [sandbox-output o2])
                (make-evaluator 'mzscheme '() '(define x 123))))
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
   (set! ev (make-evaluator 'mzscheme '() '(define id (lambda (x) x))))
   --eval--
   (id 123) => 123
   --top--
   (set! ev (make-evaluator 'mzscheme '() '(define id (lambda (x) x))
                                          '(define fooo 999)))
   --eval--
   (id fooo) => 999

   ;; test source locations too
   --top--
   (make-evaluator 'mzscheme '() 0 1 2 '(define foo))
     =err> "program:4:0: define"

   ;; empty program for clean repls
   --top--
   (set! ev (make-evaluator '(begin) '()))
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (set! ev (make-evaluator 'mzscheme '()))
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (set! ev (make-evaluator 'mzscheme '() '(define x (+ 1 2 3))))
   --eval--
   (define x (+ x 10)) =err> "cannot change identifier"

   ;; whole program argument
   --top--
   (set! ev (make-evaluator '(module foo mzscheme (define x 1))))
   --eval--
   x => 1
   --top--
   (set! ev (make-evaluator '(module foo mzscheme (provide x) (define x 1))))
   --eval--
   x => 1
   (define x 2) =err> "cannot change identifier"

   ;; limited FS access, allowed for requires
   --top--
   (when (directory-exists? "/tmp") ; non-collects place to play with
     (let* ([mzlib    (path->string (collection-path "mzlib"))]
            [list-lib (path->string (build-path mzlib "list.ss"))]
            [test-lib (path->string (path->complete-path ; <- for windows
                                     "/tmp/sandbox-test.ss"))])
       (t --top--
          (set! ev (make-evaluator 'mzscheme '()))
          --eval--
          ;; reading from collects is allowed
          (list (directory-list ,mzlib))
          (file-exists? ,list-lib) => #t
          (input-port? (open-input-file ,list-lib)) => #t
          ;; writing is forbidden
          (open-output-file ,list-lib) =err> "file access denied"
          ;; reading from other places is forbidden
          (directory-list "/tmp") =err> "file access denied"
          ;; no network too
          (tcp-listen 12345) =err> "network access denied"
          --top--
          ;; reading from a specified require is fine
          (with-output-to-file test-lib
            (lambda ()
              (printf "~s\n" '(module sandbox-test mzscheme
                                (define x 123) (provide x))))
            'replace)
          (set! ev (make-evaluator 'mzscheme `(,test-lib)))
          --eval--
          x => 123
          (length (with-input-from-file ,test-lib read)) => 5
          ;; the directory is still not kosher
          (directory-list "/tmp") =err> "file access denied"
          --top--
          ;; should work also for module evaluators
          ;; --> NO!  Shouldn't make user code require whatever it wants
          ;; (set! ev (make-evaluator `(module foo mzscheme
          ;;                             (require (file ,test-lib)))))
          ;; --eval--
          ;; x => 123
          ;; (length (with-input-from-file ,test-lib read)) => 5
          ;; ;; the directory is still not kosher
          ;; (directory-list "/tmp") =err> "file access denied"
          --top--
          ;; explicitly allow access to /tmp
          (set! ev (let ([rx (if (eq? 'windows (system-type))
                               ;; on windows this will have a drive letter
                               #rx#"^[a-zA-Z]:[/\\]tmp(?:[/\\]|$)"
                               #rx#"^/tmp(?:/|$)")])
                     (parameterize ([sandbox-path-permissions
                                     ;; allow all `/tmp' paths for windows
                                     `((read ,rx)
                                       ,@(sandbox-path-permissions))])
                       (make-evaluator 'mzscheme '()))))
          --eval--
          (length (with-input-from-file ,test-lib read)) => 5
          (list? (directory-list "/tmp"))
          (open-output-file "/tmp/blah") =err> "file access denied"
          (delete-directory "/tmp/blah") =err> "file access denied"
          )))

   ;; languages and requires
   --top--
   (set! ev (make-evaluator 'r5rs '() "(define x (eq? 'x 'X))"))
   --eval--
   x => #t
   --top--
   (set! ev (make-evaluator 'mzscheme '() "(define l null)"))
   --eval--
   (cond [null? l 0]) => 0
   (last-pair l) =err> "reference to an identifier"
   --top--
   (set! ev (make-evaluator 'beginner '() (make-prog "(define l null)"
                                                     "(define x 3.5)")))
   --eval--
   (cond [null? l 0]) =err> "expected an open parenthesis"
   --top--
   (eq? (ev "6") (ev "(sub1 (* 2 3.5))"))
   (eq? (ev "6") (ev "(sub1 (* 2 x))"))
   --top--
   (set! ev (make-evaluator 'mzscheme '((lib "list.ss")) '()))
   --eval--
   (last-pair '(1 2 3)) => '(3)
   (last-pair null) =err> "expected argument of type"

   ;; coverage
   --top--
   (set! ev (parameterize ([sandbox-coverage-enabled #t])
              (make-evaluator 'mzscheme '()
                              (make-prog "(define (foo x) (+ x 1))"
                                         "(define (bar x) (+ x 2))"
                                         "(equal? (foo 3) 4)"))))
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
   (set! ev (parameterize ([sandbox-init-hook
                            (let ([old (sandbox-init-hook)])
                              (lambda ()
                                (old)
                                (compile-enforce-module-constants #f)
                                (compile-allow-set!-undefined #t)))])
              (make-evaluator 'mzscheme '() '(define x 123))))
   --eval--
   (set! x 456) ; would be an error without the `enforce' parameter
   x => 456
   (set! y 789) ; would be an error without the `set!' parameter
   y => 789

   ))
