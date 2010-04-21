#lang scheme
(require "status.ss"
         "notify.ss"
         "rewriting.ss"
         "dirstruct.ss"
         "cache.ss")

(define (command+args+env->command+args 
         #:env env
         cmd args)
  (values "/usr/bin/env"
          (append (for/list ([(k v) (in-hash env)])
                    (format "~a=~a" k v))
                  (list* cmd
                         args))))

(define (read-until-evt port-evt k)
  (if port-evt
      (handle-evt port-evt
                  (lambda (bs)
                    (if (eof-object? bs)
                        (k)
                        (k bs))))
      never-evt))

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
    (apply subprocess
           #f #f #f 
           new-command 
           new-args))
  
  (notify! "Running: ~a ~S" command args)  
  
  ; Run it without input
  (close-output-port stdin)
  
  ; Wait for all the output, then the process death or timeout
  (local
    [(define the-alarm
       (alarm-evt (+ (current-inexact-milliseconds)
                     (* 1000 timeout))))
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

(define-syntax regexp-replace**
  (syntax-rules ()
    [(_ () s) s]
    [(_ ([pat0 subst0]
         [pat subst]
         ...)
        s)
     (regexp-replace* pat0
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
                          [home "<home>"]
                          [tmp "<tmp>"]
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
 [run/collect/wait/log 
  (path-string? string? 
                #:env (hash/c string? string?) 
                #:timeout exact-nonnegative-integer? 
                (listof string?) 
                . -> . boolean?)])