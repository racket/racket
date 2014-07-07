#lang racket/base
(require racket/system
         racket/format
         racket/port
         racket/date)

(provide (struct-out remote)
         ssh
         scp
         remote-user+host
         current-timeout
         current-tunnel-port)

(struct remote (host user dir))

(define current-timeout (make-parameter 600))
(define current-tunnel-port (make-parameter 18333))

(define scp-exe (find-executable-path "scp"))
(define ssh-exe (find-executable-path "ssh"))

(define (remote-user+host remote)
  (if (not (equal? (remote-user remote) ""))
      (~a (remote-user remote) "@" (remote-host remote))
      (remote-host remote)))

(define (system*/show exe . args)
  (displayln (apply ~a #:separator " " 
                    (map (lambda (p) (if (path? p) (path->string p) p)) 
                         (cons exe args))))
  (flush-output)
  (apply system* exe args))

(define (ssh remote
             #:mode [mode 'auto]
             #:failure-dest [failure-dest #f]
             #:success-dest [success-dest #f]
             #:show-time? [show-time? #f]
             . args)
  (define cmd
    (list "/usr/bin/env" (~a "PLTUSERHOME=" (remote-dir remote) "/user")
          "/bin/sh" "-c" (apply ~a args)))

  (define saved (and (or failure-dest success-dest)
                     (open-output-bytes)))
  (define (tee o1 o2)
    (cond
     [(not o1)
      (values o2 void)]
     [else
      (define-values (i o) (make-pipe 4096))
      (values o
              (let ([t (thread (lambda ()
                                 (copy-port i o1 o2)))])
                (lambda ()
                  (close-output-port o)
                  (sync t))))]))
  (define-values (stdout sync-out) (tee saved (current-output-port)))
  (define-values (stderr sync-err) (tee saved (current-error-port)))

  (define timeout? #f)
  (define orig-thread (current-thread))
  (define timeout (current-timeout))
  (define timeout-thread
    (thread (lambda ()
              (sleep timeout)
              (set! timeout? #t)
              (break-thread orig-thread))))

  (define (show-time)
    (when show-time?
      (printf "The time is now ~a\n" 
              (date->string (seconds->date (current-seconds)) #t))))

  (define ok?
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (with-handlers ([exn? (lambda (exn)
                              (cond
                               [timeout?
                                (eprintf "~a\n" (exn-message exn))
                                (eprintf "Timeout after ~a seconds\n" timeout)
                                #f]
                               [else (raise exn)]))])
        (show-time)
        (begin0
         (if (and (equal? (remote-host remote) "localhost")
                  (equal? (remote-user remote) ""))
             (apply system*/show cmd)
             (apply system*/show ssh-exe
                    ;; create tunnel to connect back to server:
                    "-R" (~a (current-tunnel-port)
                             ":localhost:" 
                             (current-tunnel-port))
                    (remote-user+host remote)
                    ;; ssh needs an extra level of quoting
                    ;;  relative to sh:
                    (for/list ([arg (in-list cmd)])
                      (~a "'" 
                          (regexp-replace* #rx"'" arg "'\"'\"'")
                          "'"))))
         (kill-thread timeout-thread)
         (show-time)))))
  (sync-out)
  (sync-err)
  (let ([dest (if ok? success-dest failure-dest)])
    (when dest
      (call-with-output-file*
       dest
       #:exists 'truncate/replace
       (lambda (o) (write-bytes (get-output-bytes saved) o)))))
  (case mode
    [(result) ok?]
    [else
     (unless ok?
       (error "failed"))]))

(define (scp remote src dest #:mode [mode 'auto])
  (unless (system*/show scp-exe src dest)
    (case mode
      [(ignore-failure) (void)]
      [else (error "failed")])))

