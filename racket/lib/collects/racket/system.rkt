#lang racket/base
(provide process
         process*
         process/ports
         process*/ports
         system
         system*
         system/exit-code
         system*/exit-code

	 string-no-nuls?
	 bytes-no-nuls?)

(require "private/streams.rkt")

;; Helpers: ----------------------------------------

(define (shell-path/args who argstr)
  (case (system-type)
    [(unix macosx) (append '("/bin/sh" "-c") (list argstr))]
    [(windows) (let ([cmd
                      (let ([d (find-system-path 'sys-dir)])
                        (let ([cmd (build-path d "cmd.exe")])
                          (if (file-exists? cmd)
                            cmd
                            (let ([cmd (build-path d "command.com")])
                              (if (file-exists? cmd)
                                cmd
                                ;; One last try: up a dir
                                (build-path d 'up "command.com"))))))])
                 (list cmd
                       'exact
                       (format "~a /c \"~a\"" (path->string cmd) argstr)))]
    [else (raise-mismatch-error
           who
           (format "~a: don't know what shell to use for platform: " who)
           (system-type))]))

(define (check-exe who exe)
  (unless (path-string? exe)
    (raise-argument-error who "path-string?" exe))
  exe)

(define (path-or-ok-string? s)
  ;; use `path-string?' t check for nul characters in a string,
  ;; but allow the empty string (which is not an ok path), too:
  (or (path-string? s) (equal? "" s)))

(define (string-no-nuls? s)
  (and (string? s) (path-or-ok-string? s)))

(define (bytes-no-nuls? s)
  (and (bytes? s) (not (regexp-match? #rx#"\0" s))))

(define (check-args who args)
  (cond
   [(null? args) (void)]
   [(eq? (car args) 'exact)
    (when (null? (cdr args))
      (raise-mismatch-error 
       who
       "expected a single string argument after: "
       (car args))) 
    (unless (and (>= 2 (length args))
                 (string? (cadr args))
                 (path-or-ok-string? (cadr args)))
      (raise-mismatch-error who
                            "expected a single string argument after 'exact, given: "
                            (cadr args)))
    (when (pair? (cddr args))
      (raise-mismatch-error 
       who
       "expected a single string argument after 'exact, given additional argument: "
       (caddr args)))]
   [else
    (for ([s (in-list args)])
      (unless (or (path-or-ok-string? s) (bytes-no-nuls? s))
        (raise-argument-error who "(or/c path-string? bytes-no-nuls?)" s)))])
  args)

(define (check-command who str)
  (unless (or (string-no-nuls? str) (bytes-no-nuls? str))
    (raise-argument-error who "(or/c string-no-nuls? bytes-no-nuls?)" str)))

(define (set-pwd-default?)
  (or (eq? 'unix (system-type))
      (eq? 'macosx (system-type))))

(define (call-with-pwd f)
  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PWD" (path->string (current-directory)))
    (f)))

;; Old-style functions: ----------------------------------------

(define (do-process*/ports who set-pwd? cout cin cerr exe . args)
  (let-values ([(subp out in err) ((if set-pwd? call-with-pwd (lambda (f) (f)))
                                   (lambda ()
                                     (apply subprocess
                                            (if-stream-out who cout)
                                            (if-stream-in who cin)
                                            (if-stream-out who cerr #t)
                                            (check-exe who exe)
                                            (check-args who args))))]
               [(it-ready) (make-semaphore)])
    (let ([so (streamify-out cout out)]
          [si (streamify-in cin in (lambda (ok?)
                                     (if ok?
                                       (semaphore-post it-ready)
                                       (semaphore-wait it-ready))))]
          [se (streamify-out cerr err)]
          [aport (lambda (x) (and (port? x) x))])
      (when (thread? si)
        ;; Wait for process to end, then stop copying input:
        (thread (lambda ()
                  (sync subp si)
                  (semaphore-wait it-ready)
                  (break-thread si))))
      (let ([threads-still-going?
             (lambda ()
               (ormap (lambda (s) (and (thread? s) (thread-running? s)))
                      (list so si se)))])
        (define (control m)
          (case m
            [(status)
             (let ([s (subprocess-status subp)])
               (cond [(or (not (integer? s)) (threads-still-going?))
                      'running]
                     [(zero? s) 'done-ok]
                     [else 'done-error]))]
            [(exit-code)
             (if (threads-still-going?)
               #f
               (let ([s (subprocess-status subp)]) (and (integer? s) s)))]
            [(wait)
             (subprocess-wait subp)
             (let ([twait (lambda (t) (when (thread? t) (thread-wait t)))])
               (twait so)
               (twait si)
               (twait se))]
            [(interrupt) (subprocess-kill subp #f)]
            [(kill) (subprocess-kill subp #t)]
            [else (raise-argument-error
                   'control-process
                   "(or/c 'status 'exit-code 'wait 'interrupt 'kill)" m)]))
        (list (aport so)
              (aport si)
              (subprocess-pid subp)
              (aport se)
              control)))))

(define (process*/ports cout cin cerr exe
                        #:set-pwd? [set-pwd? (set-pwd-default?)]
                        . args)
  (apply do-process*/ports 'process*/ports set-pwd? cout cin cerr exe args))

(define (process/ports out in err str
                       #:set-pwd? [set-pwd? (set-pwd-default?)])
  (apply do-process*/ports 'process/ports set-pwd? out in err (shell-path/args 'process/ports str)))

(define (process* exe
                  #:set-pwd? [set-pwd? (set-pwd-default?)]
                  . args)
  (apply do-process*/ports 'process* set-pwd? #f #f #f exe args))

(define (process str
                 #:set-pwd? [set-pwd? (set-pwd-default?)])
  (check-command 'process str)
  (apply do-process*/ports 'process set-pwd? #f #f #f (shell-path/args 'process str)))

;; Note: these always use current ports
(define (do-system*/exit-code who set-pwd? exe . args)
  (let ([cout (current-output-port)]
        [cin (current-input-port)]
        [cerr (current-error-port)]
        [it-ready (make-semaphore)])
    (let-values ([(subp out in err)
                  ((if set-pwd? call-with-pwd (lambda (f) (f)))
                   (lambda ()
                     (apply subprocess
                            (if-stream-out who cout)
                            (if-stream-in who cin)
                            (if-stream-out who cerr #t)
                            (check-exe who exe)
                            (check-args who args))))])
      (let ([ot (streamify-out cout out)]
            [it (streamify-in cin in (lambda (ok?)
                                       (if ok?
                                         (semaphore-post it-ready)
                                         (semaphore-wait it-ready))))]
            [et (streamify-out cerr err)])
        (subprocess-wait subp)
        (when it
          ;; stop piping output to subprocess
          (semaphore-wait it-ready)
          (break-thread it))
        ;; wait for other pipes to run dry:
        (when (thread? ot) (thread-wait ot))
        (when (thread? et) (thread-wait et))
        (when err (close-input-port err))
        (when out (close-input-port out))
        (when in (close-output-port in)))
      (subprocess-status subp))))

(define (system*/exit-code exe #:set-pwd? [set-pwd? (set-pwd-default?)] . args)
  (apply do-system*/exit-code 'system*/exit-code set-pwd? exe args))

(define (system* exe #:set-pwd? [set-pwd? (set-pwd-default?)] . args)
  (zero? (apply do-system*/exit-code 'system* set-pwd? exe args)))

(define (system str #:set-pwd? [set-pwd? (set-pwd-default?)])
  (check-command 'system str)
  (zero? (apply do-system*/exit-code 'system set-pwd? (shell-path/args 'system str))))

(define (system/exit-code str #:set-pwd? [set-pwd? (set-pwd-default?)])
  (check-command 'system/exit-code str)
  (apply do-system*/exit-code 'system/exit-code set-pwd? (shell-path/args 'system/exit-code str)))
