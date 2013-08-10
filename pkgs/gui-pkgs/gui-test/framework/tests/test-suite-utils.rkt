#lang racket/base

(require (only-in mzscheme fluid-let)
         launcher
         racket/system
         racket/tcp
         racket/pretty
         compiler/find-exe
         "debug.rkt")

(provide
 test-name
 failed-tests
 number-of-tests
 
 ;(struct eof-result ())
 eof-result?
 
 load-framework-automatically
 shutdown-listener shutdown-mred mred-running?
 send-sexp-to-mred queue-sexp-to-mred
 test
 wait-for-frame
 
 ;; sexp -> void
 ;; grabs the frontmost window, executes the sexp and waits for a new frontmost window
 wait-for-new-frame
 
 wait-for
 
 reset-section-jump!
 set-section-jump!
 reset-section-name!
 set-section-name!
 set-only-these-tests!
 get-only-these-tests
 debug-printf
 
 exn->str)

(define section-jump void)
(define (set-section-jump! _s) (set! section-jump _s))
(define (reset-section-jump!) (set! section-jump #f))

(define section-name "<<setup>>")
(define (set-section-name! _s) (set! section-name _s))
(define (reset-section-name!) (set! section-name "<<setup>>"))

(define only-these-tests #f)
(define (get-only-these-tests) only-these-tests)
(define (set-only-these-tests! _t) (set! only-these-tests _t))

(define test-name "<<setup>>")
(define failed-tests null)
(define number-of-tests 0)

(define-struct eof-result ())

(define load-framework-automatically? #t)

(define initial-port 6012)
(define port-filename
  (build-path (find-system-path 'temp-dir)
              "framework-tests-receive-sexps-port.rkt"))

(unless (file-exists? port-filename)
  (call-with-output-file port-filename
    (lambda (port) (write initial-port port))))

(define listener
  (let loop ([port (call-with-input-file port-filename read)])
    (let ([l (with-handlers ([exn:fail? (lambda (_) #f)])
               (tcp-listen port))])
      (if l
          (begin (debug-printf mz-tcp "listening to ~a\n" port)
                 (call-with-output-file port-filename
                   (lambda (p) (write port p))
                   #:exists 'truncate)
                 l)
          (begin (debug-printf mz-tcp "  tcp-listen failed for port ~a\n" port)
                 (loop (add1 port)))))))

(define in-port #f)
(define out-port #f)

(define (restart-mred)
  (shutdown-mred)
  (thread
   (lambda ()
     (define racket-bin (find-exe))
     (unless (system*
              racket-bin
              (path->string
               (collection-file-path "framework-test-engine.rkt" "framework" "tests")))
       (eprintf "starting gracket failed; used path ~s\n"
                racket-bin))))
  (debug-printf mz-tcp "accepting listener\n")
  (let-values ([(in out) (tcp-accept listener)])
    (set! in-port in)
    (set! out-port out))
  (when load-framework-automatically?
    (queue-sexp-to-mred
     '(begin (eval '(require framework))
             (eval '(require framework/tests/private/gui))))))

(define load-framework-automatically
  (case-lambda
    [(new-load-framework-automatically?)
     (unless (eq? (not (not new-load-framework-automatically?))
                  load-framework-automatically?)
       (set! load-framework-automatically? (not (not new-load-framework-automatically?)))
       (shutdown-mred))]
    [() load-framework-automatically?]))

(define shutdown-listener
  (lambda ()
    (shutdown-mred)
    (debug-printf mz-tcp "closing listener\n")
    (tcp-close listener)))

(define shutdown-mred
  (lambda ()
    (when (and in-port
               out-port)
      (with-handlers ([exn:fail? (lambda (x) (void))])
        (close-output-port out-port))
      (with-handlers ([exn:fail? (lambda (x) (void))])
        (close-input-port in-port))
      (set! in-port #f)
      (set! in-port #f))))

(define (mred-running?)
  (if (char-ready? in-port)
      (not (eof-object? (peek-char in-port)))
      #t))

(define queue-sexp-to-mred
  (lambda (sexp)
    (send-sexp-to-mred
     `(let ([thunk (lambda () ,sexp)]  ;; low tech hygiene
            [c (make-channel)])
        (queue-callback (lambda () (channel-put c (with-handlers ((exn:fail? (λ (x) (list 'exn x)))) (list 'normal (thunk))))))
        (let ([res (channel-get c)])
          (if (eq? (list-ref res 0) 'normal)
              (list-ref res 1)
              (raise (list-ref res 1))))))))

(define re:tcp-read-error (regexp "tcp-read:"))
(define re:tcp-write-error (regexp "tcp-write:"))
(define (tcp-error? exn)
  (or (regexp-match re:tcp-read-error (exn-message exn))
      (regexp-match re:tcp-write-error (exn-message exn))))

(namespace-require 'racket) ;; in order to make the eval below work right.
(define (send-sexp-to-mred sexp)
  (let/ec k
    (let ([show-text
           (lambda (sexp)
             (debug-when messages
                         (parameterize ([pretty-print-print-line
                                         (let ([prompt "  "]
                                               [old-liner (pretty-print-print-line)])
                                           (lambda (ln port ol cols)
                                             (let ([ov (old-liner ln port ol cols)])
                                               (if ln 
                                                   (begin (display prompt port)
                                                          (+ (string-length prompt) ov))
                                                   ov))))])
                           (pretty-print sexp)
                           (newline))))])
      (unless (and in-port
                   out-port
                   (with-handlers ([tcp-error? (lambda (x) #f)])
                     (or (not (char-ready? in-port))
                         (not (eof-object? (peek-char in-port))))))
        (restart-mred))
      (debug-printf messages "  ~a // ~a: sending to framework side to eval:\n"
                    section-name test-name)
      (show-text sexp)
      (with-handlers ([exn:fail?
                       (lambda (x)
                         (cond
                           ;; this means that gracket was closed
                           ;; so we can restart it and try again.
                           [(tcp-error? x)
                            (restart-mred)
                            (write sexp out-port)
                            (newline out-port)
                            (flush-output out-port)]
                           [else (raise x)]))])
        (write sexp out-port)
        (newline out-port)
        (flush-output out-port))
      (let ([answer
             (with-handlers ([exn:fail?
                              (lambda (x)
                                (if (tcp-error? x);; assume tcp-error means app closed
                                    eof
                                    (list 'cant-read
                                          (string-append
                                           (exn->str x)
                                           "; rest of string: "
                                           (format
                                            "~s"
                                            (apply
                                             string
                                             (let loop ()
                                               (if (char-ready? in-port)
                                                   (let ([char (read-char in-port)])
                                                     (if (eof-object? char)
                                                         null
                                                         (cons char (loop))))
                                                   null))))))))])
               (read in-port))])
        (debug-printf messages "  ~a // ~a: received from gracket:\n" section-name test-name)
        (show-text answer)
        (unless (or (eof-object? answer)
                    (and (list? answer)
                         (= 2 (length answer))
                         (memq (car answer)
                               '(error last-error cant-read normal))))
          (error 'send-sexp-to-mred "unpected result from gracket: ~s\n" answer))
        (if (eof-object? answer)
            (raise (make-eof-result))
            (case (car answer)
              [(error)
               (error 'send-sexp-to-mred "gracket raised \"~a\"" (list-ref answer 1))]
              [(last-error)
               (error 'send-sexp-to-mred "gracket (last time) raised \"~a\"" (list-ref answer 1))]
              [(cant-read) (error 'mred/cant-parse (list-ref answer 1))]
              [(normal) 
               (eval (list-ref answer 1))]))))))

(define test
  (case-lambda
    [(in-test-name passed? sexp/proc) (test in-test-name passed? sexp/proc 'section)]
    [(in-test-name passed? sexp/proc jump)
     (fluid-let ([test-name in-test-name])
       (when (or (not only-these-tests)
                 (memq test-name only-these-tests))
         (let* ([result
                 (with-handlers ([exn:fail?
                                  (lambda (x)
                                    (if (exn? x)
                                        (exn->str x)
                                        x))])
                   (if (procedure? sexp/proc)
                       (sexp/proc)
                       (begin0 (send-sexp-to-mred sexp/proc)
                               (send-sexp-to-mred ''check-for-errors))))]
                [failed (with-handlers ([exn:fail?
                                         (lambda (x)
                                           (string-append
                                            "passed? test raised exn: "
                                            (if (exn? x)
                                                (exn->str x)
                                                (format "~s" x))))])
                          (not (passed? result)))])
           (set! number-of-tests (+ number-of-tests 1))
           (when failed
             (debug-printf schedule "FAILED ~a:\n  ~s\n" test-name result)
             (set! failed-tests (cons (cons section-name test-name) failed-tests))
             (case jump
               [(section) (section-jump)]
               [(continue) (void)]
               [else (jump)])))))]))

(define (exn->str exn)
  (let ([sp (open-output-string)])
    (parameterize ([current-error-port sp])
      ((error-display-handler) (exn-message exn) exn))
    (get-output-string sp)))

(define (wait-for/wrapper wrapper sexp)
  (let ([timeout 10]
        [pause-time 1/2])
    (send-sexp-to-mred
     (wrapper
      `(let ([test (lambda () ,sexp)])
         (let loop ([n ,(/ timeout pause-time)])
           (if (zero? n)
               (error 'wait-for
                      ,(format "after ~a seconds, ~s didn't come true" timeout sexp))
               (unless (test)
                 (sleep ,pause-time)
                 (loop (- n 1))))))))))

(define (wait-for sexp #:queue? [queue? #f]) 
  (wait-for/wrapper
   (lambda (x) x)
   (if queue?
       `(let ([t (λ () ,sexp)]
              [c (make-channel)])
          (queue-callback (λ () (channel-put c (t))))
          (channel-get c))
       sexp)))

(define (wait-for-new-frame sexp)
  (wait-for/wrapper
   (lambda (w)
     `(let ([frame (get-top-level-focus-window)])
        ,sexp
        ,w))
   `(not (eq? frame (get-top-level-focus-window)))))

(define (wait-for-frame name [eventspace #f])
  (let ([exp `(let ([win (get-top-level-focus-window)])
                (and win
                     (string=? (send win get-label) ,name)))])
    (if eventspace
        (wait-for
         `(parameterize ([current-eventspace ,eventspace])
            ,exp))
        (wait-for exp))))
