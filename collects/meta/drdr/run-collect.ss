#lang scheme
(require "status.ss"
         "notify.ss"
         "rewriting.ss"
         "cache.ss")

(define current-subprocess-timeout-seconds
  (make-parameter (* 60 10)))

(define (read-until-evt port-evt k)
  (if port-evt
      (handle-evt port-evt
                  (lambda (bs)
                    (if (eof-object? bs)
                        (k)
                        (k bs))))
      never-evt))

(define (run/collect/wait command . args)
  (define start-time 
    (current-inexact-milliseconds))
  
  ; Run the command
  (define command-line
    (list* command args))
  (define-values
    (the-process stdout stdin stderr)
    (apply subprocess
           #f #f #f 
           command 
           args))
  
  (notify! "Running: ~a ~S" command args)  
  
  ; Run it without input
  (close-output-port stdin)
  
  ; Wait for all the output, then the process death or timeout
  (local
    [(define the-alarm
       (alarm-evt (+ (current-inexact-milliseconds)
                     (* 1000 (current-subprocess-timeout-seconds)))))
     (define (slurp-output-evt loop stdout stderr log)
       (choice-evt
        (read-until-evt stdout
                        (case-lambda 
                          [() 
                           (loop #f stderr log)]
                          [(bs)
                           (loop stdout stderr (list* (make-stdout bs) log))]))
        (read-until-evt stderr
                        (case-lambda 
                          [() 
                           (loop stdout #f log)]
                          [(bs)
                           (loop stdout stderr (list* (make-stderr bs) log))]))))
     (define (finish-log stdout stderr log)
       (if (or stdout stderr)
           (sync (slurp-output-evt finish-log stdout stderr log))
           (reverse log)))
     
     (define final-status
       (let loop ([stdout (read-bytes-line-evt stdout)]
                  [stderr (read-bytes-line-evt stderr)]
                  [log empty])
         (sync (handle-evt the-alarm
                           (lambda (_)
                             (define end-time 
                               (current-inexact-milliseconds))
                             (subprocess-kill the-process #t)
                             (make-timeout start-time end-time command-line (finish-log stdout stderr log))))
               (slurp-output-evt loop stdout stderr log)
               (handle-evt the-process
                           (lambda (_)
                             (define end-time 
                               (current-inexact-milliseconds))
                             (make-exit start-time end-time command-line
                                        (finish-log stdout stderr log)
                                        (subprocess-status the-process)))))))]
    (close-input-port stdout)
    (close-input-port stderr)
    
    (notify! "Done: ~a ~S" command args)
    
    final-status))

(define (run/collect/wait/log log-path . rcw-args)
  (define ran? #f)
  (cache/file
   log-path
   (lambda ()
     (set! ran? #t)
     (rewrite-status
      (apply run/collect/wait rcw-args))))
  ran?)

(define (with-running-program command args thunk)
  (define-values
    (the-process stdout stdin stderr)
    (apply subprocess
           (current-error-port) #f
           (current-error-port)
           command 
           args))
  ; Die if this program does
  (define parent
    (current-thread))
  (define waiter
    (thread
     (lambda ()
       (subprocess-wait the-process)
       (printf "Killing parent because wrapper is dead...~n")
       (kill-thread parent))))
  
  ; Run without stdin
  (close-output-port stdin)
  
  (begin0
    ; Run the thunk
    (thunk)
    
    ; Close the output ports
    #;(close-input-port stdout)
    #;(close-input-port stderr)
    
    ; Kill the guard
    (kill-thread waiter)
    
    ; Kill the process
    (subprocess-kill the-process #t)))

(provide
 (all-from-out "status.ss"))
(provide/contract
 [current-subprocess-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [with-running-program (string? (listof string?) (-> any) . -> . any)]
 [run/collect/wait ((string?) () #:rest (listof string?) . ->* . status?)]
 [run/collect/wait/log ((path-string? string?) () #:rest (listof string?) . ->* . boolean?)])