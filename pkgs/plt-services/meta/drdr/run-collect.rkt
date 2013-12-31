#lang racket
(require "status.rkt"
         "notify.rkt"
         "rewriting.rkt"
         "dirstruct.rkt"
         "cache.rkt")

(define (command+args+env->command+args 
         #:env env
         cmd args)
  (values "/usr/bin/env"
          (append (for/list ([(k v) (in-hash env)])
                    (format "~a=~a" k v))
                  (list* cmd
                         args))))

(define (run/collect/wait 
         #:env env
         #:timeout timeout
         command args)
  (define start-time 
    (current-inexact-milliseconds))
  
  ; Run the command
  (define-values (new-command new-args)
    (command+args+env->command+args
     #:env env
     command args))
  (define command-line
    (list* command args))
  (define-values
    (the-process stdout stdin stderr)
    (parameterize ([subprocess-group-enabled #t])
      (apply subprocess
             #f #f #f
             new-command 
             new-args)))
  
  (notify! "Running: ~a ~S" command args)  
  
  ; Run it without input
  (close-output-port stdin)
  
  ; Wait for all the output and the process death or timeout
  (local
    [(define the-alarm
       (alarm-evt (+ start-time (* 1000 timeout))))
     
     (define line-ch (make-channel))
     (define (read-port-t make port)
       (thread
        (λ ()
          (let loop ()
            (define l (read-bytes-line port))
            (if (eof-object? l)
                (channel-put line-ch l)
                (begin (channel-put line-ch (make l))
                       (loop)))))))
     (define stdout-t (read-port-t make-stdout stdout))
     (define stderr-t (read-port-t make-stderr stderr))
     
     (define final-status
       (let loop ([open-ports 2]
                  [end-time #f]
                  [status #f]
                  [log empty])
         (define process-done? (and end-time #t))
         (define output-done? (zero? open-ports))
         (if (and output-done? process-done?)
             (if status
                 (if (= status 2)
                   (make-timeout start-time end-time command-line (reverse log))
                   (make-exit start-time end-time command-line (reverse log) status))
                 (make-timeout start-time end-time command-line (reverse log)))
             (sync (if process-done?
                       never-evt
                       (choice-evt 
                        (handle-evt the-alarm
                                    (λ (_)
                                      (define end-time 
                                        (current-inexact-milliseconds))
                                      (subprocess-kill the-process #f)
                                      ;; Sleep for 10% of the timeout
                                      ;; before sending the death
                                      ;; signal
                                      (sleep (* timeout 0.1))
                                      (subprocess-kill the-process #t)
                                      (loop open-ports end-time status log)))
                        (handle-evt the-process
                                    (λ (_)
                                      (define end-time 
                                        (current-inexact-milliseconds))
                                      (loop open-ports end-time (subprocess-status the-process) log)))))
                   (if output-done?
                       never-evt
                       (handle-evt line-ch
                                   (match-lambda
                                     [(? eof-object?)
                                      (loop (sub1 open-ports) end-time status log)]
                                     [l
                                      (loop open-ports end-time status (list* l log))])))))))]
    
    (close-input-port stdout)
    (close-input-port stderr)
    
    (notify! "Done: ~a ~S" command args)
    
    final-status))

(define-syntax regexp-replace**
  (syntax-rules ()
    [(_ () s) s]
    [(_ ([pat0 subst0]
         [pat subst]
         ...)
        s)
     (regexp-replace* (regexp-quote pat0)
                      (regexp-replace** ([pat subst] ...) s)
                      subst0)]))

(define (run/collect/wait/log log-path command 
                              #:timeout timeout 
                              #:env env
                              args)
  (define ran? #f)
  (cache/file
   log-path
   (lambda ()
     (define rev (number->string (current-rev)))
     (define home (hash-ref env "HOME"))
     (define tmp (hash-ref env "TMPDIR"))
     (define cwd (path->string (current-directory)))
     (define (rewrite s)
       (regexp-replace** ([rev "<current-rev>"]
                          [tmp "<tmp>"]
                          [home "<home>"]
                          [cwd "<cwd>"])
                         s))
     
     (set! ran? #t)
     (rewrite-status
      #:rewrite rewrite
      (run/collect/wait
       #:timeout timeout
       #:env env
       command args))))
  ran?)

(provide/contract
 [command+args+env->command+args 
  (string? (listof string?) #:env (hash/c string? string?) . -> . (values string? (listof string?)))]
 [run/collect/wait
  (string? 
   #:env (hash/c string? string?) 
   #:timeout exact-nonnegative-integer? 
   (listof string?) 
   . -> . status?)]
 [run/collect/wait/log 
  (path-string? string? 
                #:env (hash/c string? string?) 
                #:timeout exact-nonnegative-integer? 
                (listof string?) 
                . -> . boolean?)])
