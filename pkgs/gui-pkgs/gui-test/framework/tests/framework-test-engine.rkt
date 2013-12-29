#lang racket/base
(require mzlib/pconvert
         racket/tcp
         racket/class
         racket/gui/base
         "debug.rkt")

(module test racket/base)

(define errs null)
(define sema (make-semaphore 1))
(define (protect f)
  (semaphore-wait sema)
  (begin0 (f)
          (semaphore-post sema)))

(define (exception->string x)
  (if (exn? x)
      (let ([p (open-output-string)])
        (parameterize ([current-error-port p])
          ((error-display-handler) (exn-message x) x))
        (get-output-string p))
      (format "uncaught exn: ~s" x)))

(namespace-require 'racket/gui)

(eval '(define (queue-callback/res thunk)
         (define c (make-channel))
         (queue-callback (λ () (channel-put c (thunk))))
         (channel-get c)))

(void
 (thread
  (lambda ()
    (with-handlers ([(lambda (x) #t)
                     (lambda (x)
                       (printf "test suite thread died: ~a\n"
                               (if (exn? x)
                                   (exception->string x)
                                   (format "~s" x))))])
      (let ([port (call-with-input-file
                      (build-path (find-system-path 'temp-dir)
                                  "framework-tests-receive-sexps-port.rkt")
                    read)])
        (debug-printf mr-tcp "about to connect to ~a\n" port)
        (let*-values ([(in out) (tcp-connect "127.0.0.1" port)])
          (let loop ()
            (debug-printf mr-tcp "about to read\n")
            (let ([sexp (read in)])
              (if (eof-object? sexp)
                  (begin
                    (debug-printf mr-tcp "got eof\n")
                    (close-input-port in)
                    (close-output-port out)
                    (exit))
                  (begin
                    (debug-printf mr-tcp "got expression to evaluate\n")
                    (write
                     (let ([these-errs (protect (lambda () (begin0 errs (set! errs null))))])
                       (if (null? these-errs)
                           (with-handlers ([(lambda (x) #t)
                                            (lambda (x) (list 'error (exception->string x)))])
                             (list 'normal (print-convert (eval sexp))))
                           (list 'last-error
                                 (apply string-append
                                        (map (lambda (x) (string-append (exception->string x) (string #\newline)))
                                             these-errs)))))
                     out)
                    (newline out)
                    (flush-output out)
                    (loop)))))))))))

(let ([od (event-dispatch-handler)]
      [port (current-output-port)])
  (event-dispatch-handler
   (lambda (evt)
     (parameterize ([uncaught-exception-handler
                     (let ([oe (uncaught-exception-handler)])
                       (lambda (exn)
                         (protect
                          (lambda ()
                            (set! errs (cons exn errs))))
                         (oe exn)))])
       (call-with-exception-handler
        (lambda (exn)
          ((uncaught-exception-handler) exn))
        (lambda ()
          (od evt)))))))

(yield (make-semaphore 0))
