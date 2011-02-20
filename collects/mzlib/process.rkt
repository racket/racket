#lang racket/base
(provide process
         process*
         process/ports
         process*/ports
         system
         system*
         system/exit-code
         system*/exit-code)

(require mzlib/port)

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

(define (if-stream-out who p [sym-ok? #f])
  (cond [(and sym-ok? (eq? p 'stdout)) p]
        [(or (not p) (and (output-port? p) (file-stream-port? p))) p]
        [(output-port? p) #f]
        [else (raise-type-error who
                                (if sym-ok?
                                    "output port, #f, or 'stdout" 
                                    "output port or #f")
                                p)]))

(define (if-stream-in who p)
  (cond [(or (not p) (and (input-port? p) (file-stream-port? p))) p]
        [(input-port? p) #f]
        [else (raise-type-error who "input port or #f" p)]))

(define (streamify-in cin in ready-for-break)
  (if (and cin (not (file-stream-port? cin)))
    (thread (lambda ()
              (dynamic-wind
                void
                (lambda ()
                  (with-handlers ([exn:break? void])
                    (ready-for-break #t)
                    (copy-port cin in)
                    (ready-for-break #f)))
                (lambda () (close-output-port in)))
              (ready-for-break #t)))
    in))

(define (streamify-out cout out)
  (if (and cout 
           (not (eq? cout 'stdout))
           (not (file-stream-port? cout)))
      (thread (lambda ()
                (dynamic-wind
                    void
                    (lambda () (copy-port out cout))
                    (lambda () (close-input-port out)))))
      out))

(define (check-exe who exe)
  (unless (path-string? exe)
    (raise-type-error who "path or string" exe))
  exe)

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
                 (path-string? (cadr args)))
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
      (unless (or (path-string? s)
                  (and (bytes? s) ((bytes-length s) . > . 0)
                       (not (regexp-match? #rx"\0" s))))
        (raise-type-error who "path, string, or byte string (without NULs)"
                          s)))])
  args)

(define (check-command who str)
  (unless (or (string? str)
              (bytes? str))
    (raise-type-error who "string or byte string" str)))

;; Old-style functions: ----------------------------------------

(define (do-process*/ports who cout cin cerr exe . args)
  (let-values ([(subp out in err) (apply subprocess
                                         (if-stream-out who cout)
                                         (if-stream-in who cin)
                                         (if-stream-out who cerr #t)
                                         (check-exe who exe)
                                         (check-args who args))]
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
            [else (raise-type-error
                   'control-process
                   "'status, 'exit-code, 'wait, 'interrupt, or 'kill" m)]))
        (list (aport so)
              (aport si)
              (subprocess-pid subp)
              (aport se)
              control)))))

(define (process*/ports cout cin cerr exe . args)
  (apply do-process*/ports 'process*/ports cout cin cerr exe args))

(define (process/ports out in err str)
  (apply do-process*/ports 'process/ports out in err (shell-path/args 'process/ports str)))

(define (process* exe . args)
  (apply do-process*/ports 'process* #f #f #f exe args))

(define (process str)
  (check-command 'process str)
  (apply do-process*/ports 'process #f #f #f (shell-path/args 'process str)))

;; Note: these always use current ports
(define (do-system*/exit-code who exe . args)
  (let ([cout (current-output-port)]
        [cin (current-input-port)]
        [cerr (current-error-port)]
        [it-ready (make-semaphore)])
    (let-values ([(subp out in err)
                  (apply subprocess
                         (if-stream-out who cout)
                         (if-stream-in who cin)
                         (if-stream-out who cerr #t)
                         (check-exe who exe)
                         (check-args who args))])
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

(define (system*/exit-code exe . args)
  (apply do-system*/exit-code 'system*/exit-code exe args))

(define (system* exe . args)
  (zero? (apply do-system*/exit-code 'system* exe args)))

(define (system str)
  (check-command 'system str)
  (zero? (apply do-system*/exit-code 'system (shell-path/args 'system str))))

(define (system/exit-code str)
  (check-command 'system/exit-code str)
  (apply do-system*/exit-code 'system/exit-code (shell-path/args 'system/exit-code str)))
