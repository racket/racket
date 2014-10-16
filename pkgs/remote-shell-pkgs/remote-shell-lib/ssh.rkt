#lang racket/base
(require racket/system
         racket/format
         racket/port
         racket/date
         racket/contract)

(provide remote?
         (contract-out 
          (rename create-remote remote
                  ((#:host string?)
                   (#:user string?
                           #:env (listof (cons/c string? string?))
                           #:timeout real?
                           #:remote-tunnels (listof (cons/c (integer-in 1 65535)
                                                            (integer-in 1 65535))))
                   . ->* . remote?))
          [ssh ((remote?)
                (#:mode (or/c 'error 'result 'output)
                        #:failure-log (or/c #f path-string?)
                        #:success-log (or/c #f path-string?)
                        #:show-time? any/c)
                #:rest (listof (or/c string? path-string?))
                . ->* . any)]
          [scp ((remote? path-string? path-string?)
                (#:mode (or/c 'error 'result))
                . ->* .
                void?)]
          [make-sure-remote-is-ready ((remote?)
                                      (#:tries exact-nonnegative-integer?)
                                      . ->* .
                                      void?)]
          [at-remote (remote? path-string? . -> . string?)]))

(struct remote (host user timeout remote-tunnels env)
  #:constructor-name make-remote)

(define create-remote
  (let ()
    (define (remote #:host host
                    #:user [user ""]
                    #:timeout [timeout 600]
                    #:remote-tunnels [remote-tunnels null]
                    #:env [env null])
      (make-remote host user timeout remote-tunnels env))
    remote))

(define scp-exe (find-executable-path "scp"))
(define ssh-exe (find-executable-path "ssh"))

(define (remote-user+host remote)
  (if (not (equal? (remote-user remote) ""))
      (~a (remote-user remote) "@" (remote-host remote))
      (remote-host remote)))

(define (at-remote remote path)
  (~a (remote-user+host remote) ":" path))

(define (system*/show exe . args)
  (displayln (apply ~a #:separator " " 
                    (map (lambda (p) (if (path? p) (path->string p) p)) 
                         (cons exe args))))
  (flush-output)
  (apply system* exe args))

(define (ssh remote
             #:mode [mode 'error]
             #:failure-log [failure-dest #f]
             #:success-log [success-dest #f]
             #:show-time? [show-time? #f]
             . args)
  (define cmd
    (append
     (list "/usr/bin/env")
     (for/list ([e (in-list (remote-env remote))])
       (~a (car e) "=" (cdr e)))
     (list
      "/bin/sh" "-c" (apply ~a args))))

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
  (define timeout (remote-timeout remote))
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
                    (append
                     ;; create tunnels to connect back to server:
                     (apply
                      append
                      (for/list ([tunnel (in-list (remote-remote-tunnels remote))])
                        (list "-R" (~a (car tunnel) ":localhost:" (cdr tunnel)))))
                     (list (remote-user+host remote))
                     ;; ssh needs an extra level of quoting
                     ;;  relative to sh:
                     (for/list ([arg (in-list cmd)])
                       (~a "'" 
                           (regexp-replace* #rx"'" arg "'\"'\"'")
                           "'")))))
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
    [(output) (cons ok? (get-output-bytes saved))]
    [else
     (unless ok?
       (error 'ssh "failed"))]))

(define (scp remote src dest #:mode [mode 'error])
  (define ok? (system*/show scp-exe src dest))
  (case mode
    [(result) ok?]
    [else 
     (unless ok?
       (error 'scp "failed"))]))

(define (make-sure-remote-is-ready remote
                                   #:tries [tries 3])
  (let loop ([tries tries])
    (unless (ssh remote
                 "echo hello"
                 #:mode (if (zero? tries) 'error 'result))
      (sleep 1)
      (loop (sub1 tries)))))
