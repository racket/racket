#lang racket/base
(require "../common/check.rkt"
         "../common/bytes-no-nuls.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/thread.rkt"
         "../path/path.rkt"
         "../path/parameter.rkt"
         "../port/output-port.rkt"
         "../port/input-port.rkt"
         "../port/fd-port.rkt"
         "../port/file-stream.rkt"
         "../file/host.rkt"
         "../string/convert.rkt"
         "../locale/string.rkt"
         "../envvar/main.rkt"
         "../sandman/main.rkt")

(provide (rename-out [do-subprocess subprocess])
         subprocess?
         subprocess-wait
         subprocess-status
         subprocess-kill
         subprocess-pid
         current-subprocess-custodian-mode
         subprocess-group-enabled
         shell-execute)

(struct subprocess ([process #:mutable]
                    [cust-ref #:mutable]
                    is-group?)
  #:constructor-name make-subprocess
  #:property
  prop:evt
  (poller (lambda (sp ctx)
            (define v (rktio_poll_process_done rktio (subprocess-process sp)))
            (if (eqv? v 0)
                (begin
                  (sandman-poll-ctx-add-poll-set-adder!
                   ctx
                   (lambda (ps)
                     (rktio_poll_add_process rktio (subprocess-process sp) ps)))
                  (values #f sp))
                (values (list sp) #f)))))

(define do-subprocess
  (let ()
    (define/who (subprocess stdout stdin stderr group/command . command/args)
      (check who
             (lambda (p) (or (not p) (and (output-port? p) (file-stream-port? p))))
             #:contract "(or/c (and/c output-port? file-stream-port?) #f)"
             stdout)
      (check who
             (lambda (p) (or (not p) (and (input-port? p) (file-stream-port? p))))
             #:contract "(or/c (and/c input-port? file-stream-port?) #f)"
             stdin)
      (check who
             (lambda (p) (or (not p) (eq? p 'stdout) (and (output-port? p) (file-stream-port? p))))
             #:contract "(or/c (and/c output-port? file-stream-port?) #f 'stdout)"
             stderr)
      (define-values (group command exact/args)
        (cond
          [(path-string? group/command)
           (values (and (subprocess-group-enabled) 'new) group/command command/args)]
          [(null? command/args)
           (raise-argument-error who "path-string?" command)]
          [(or (not group/command)
               (eq? group/command 'new)
               (subprocess? group/command))
           (unless (pair? command/args)
             (raise-arguments-error who "missing command argument after group argument"))
           (define command (car command/args))
           (check who path-string? command)
           (when (subprocess? group/command)
             (unless (subprocess-is-group? group/command)
               (raise-arguments-error who "subprocess does not represent a new group"
                                      "subprocess" group/command)))
           (values group/command command (cdr command/args))]
          [else
           (raise-argument-error who "(or/c path-string? #f 'new subprocess?)" group/command)]))
      (define-values (exact? args)
        (cond
          [(and (pair? exact/args)
                (eq? 'exact (car exact/args)))
           (values #t (cdr exact/args))]
          [else
           (values #f exact/args)]))
      (for ([arg (in-list args)]
            [i (in-naturals)])
        (check who
               (lambda (p) (or (path? p) (string-no-nuls? p) (bytes-no-nuls? p)))
               #:contract (if (and (not exact?)
                                   (= i 0)
                                   (= (length args) 2))
                              "(or/c path? string-no-nuls? bytes-no-nuls? 'exact)"
                              "(or/c path? string-no-nuls? bytes-no-nuls?)")
               arg))

      (define cust-mode (current-subprocess-custodian-mode))
      (define env-vars (current-environment-variables))

      (let* ([flags (if (eq? stderr 'stdout)
                        RKTIO_PROCESS_STDOUT_AS_STDERR
                        0)]
             [flags (if exact?
                        (bitwise-ior flags RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE)
                        flags)]
             [flags (if (eq? group 'new)
                        (bitwise-ior flags RKTIO_PROCESS_NEW_GROUP)
                        flags)]
             [flags (if (and (eq? cust-mode 'kill)
                             (positive? (bitwise-and (rktio_process_allowed_flags rktio)
                                                     RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION)))
                        (bitwise-ior flags RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION)
                        flags)])
        
        (define command-bstr (->host (->path command) who '(execute)))

        (start-atomic)
        (poll-subprocess-finalizations)
        (check-current-custodian who)
        (define envvars (rktio_empty_envvars rktio))
        (for ([name (in-list (environment-variables-names env-vars))])
          (rktio_envvars_set rktio envvars name (environment-variables-ref env-vars name)))

        (define send-args (rktio_from_bytes_list
                           (cons command-bstr
                                 (for/list ([arg (in-list args)])
                                   (cond
                                     [(string? arg)
                                      (string->bytes/locale arg (char->integer #\?))]
                                     [(path? arg)
                                      (path-bytes arg)]
                                     [else arg])))))

        (define r (rktio_process rktio command-bstr (add1 (length args)) send-args
                                 (and stdout (fd-port-fd stdout))
                                 (and stdin (fd-port-fd stdin))
                                 (and stderr (not (eq? stderr 'stdout)) (fd-port-fd stderr))
                                 (and (subprocess? group) (subprocess-process group))
                                 (->host (current-directory) #f null)
                                 envvars flags))

        (rktio_free_bytes_list send-args (length args))
        (when envvars
          (rktio_envvars_free rktio envvars))

        (when (rktio-error? r)
          (end-atomic)
          (raise-rktio-error who r "process creation failed"))

        (define in (let ([fd (rktio_process_result_stdout_fd r)])
                     (and fd (open-input-fd fd 'subprocess-stdout))))
        (define out (let ([fd (rktio_process_result_stdin_fd r)])
                      (and fd (open-output-fd fd 'subprocess-stdin))))
        (define err (let ([fd (rktio_process_result_stderr_fd r)])
                      (and fd (open-input-fd fd 'subprocess-stderr))))
        (define sp (make-subprocess (rktio_process_result_process r)
                                    #f
                                    (eq? group 'new)))

        (register-subprocess-finalizer sp)
        (when cust-mode
          (let ([close (if (eq? cust-mode 'kill) kill-subprocess interrupt-subprocess)])
            (set-subprocess-cust-ref! sp (unsafe-custodian-register (current-custodian) sp close #t #f))))

        (rktio_free r)

        (end-atomic)
        (values sp in out err)))
    subprocess))

;; ----------------------------------------

(define/who (subprocess-wait sp)
  (check who subprocess? sp)
  (void (sync sp)))

;; ----------------------------------------

(define/who (subprocess-status sp)
  (check who subprocess? sp)
  (start-atomic)
  (define r (rktio_process_status rktio (subprocess-process sp)))
  (cond
    [(rktio-error? r)
     (end-atomic)
     (raise-rktio-error who r "status access failed")]
    [(rktio_status_running r)
     (rktio_free r)
     (end-atomic)
     'running]
    [else
     (define v (rktio_status_result r))
     (rktio_free r)
     (end-atomic)
     v]))
  
(define/who (subprocess-pid sp)
  (check who subprocess? sp)
  (atomically
   (rktio_process_pid rktio (subprocess-process sp))))

;; ----------------------------------------

;; in atomic mode
(define (kill-subprocess sp)
  (define p (subprocess-process sp))
  (when p
    (rktio_process_kill rktio p)))

;; in atomic mode
(define (interrupt-subprocess sp)
  (define p (subprocess-process sp))
  (when p
    (rktio_process_interrupt rktio p)))

(define/who (subprocess-kill sp force?)
  (check who subprocess? sp)
  (atomically (if force?
                  (kill-subprocess sp)
                  (interrupt-subprocess sp))))

;; ----------------------------------------

(define subprocess-will-executor (make-will-executor))

(define (register-subprocess-finalizer sp)
  (will-register subprocess-will-executor
                 sp
                 (lambda (sp)
                   (when (subprocess-process sp)
                     (rktio_process_forget rktio (subprocess-process sp))
                     (set-subprocess-process! sp #f))
                   (when (subprocess-cust-ref sp)
                     (unsafe-custodian-unregister sp (subprocess-cust-ref sp))
                     (set-subprocess-cust-ref! sp #f))
                   #t)))

(define (poll-subprocess-finalizations)
  (when (will-try-execute subprocess-will-executor)
    (poll-subprocess-finalizations)))

;; ----------------------------------------

(define/who current-subprocess-custodian-mode
  (make-parameter #f (lambda (v)
                       (unless (or (not v) (eq? v 'kill) (eq? v 'interrupt))
                         (raise-argument-error who "(or/c #f 'kill 'interrupt)" v))
                       v)))

(define subprocess-group-enabled
  (make-parameter #f (lambda (v) (and v #t))))

;; ----------------------------------------

(define/who (shell-execute verb target parameters dir show-mode)
  (check who string? #:or-false verb)
  (check who string? target)
  (check who string? parameters)
  (check who path-string? dir)
  (define show_mode
    (case show-mode
      [(sw_hide SW_HIDE) RKTIO_SW_HIDE]
      [(sw_maximize SW_MAXIMIZE) RKTIO_SW_MAXIMIZE]
      [(sw_minimize SW_MINIMIZE) RKTIO_SW_MINIMIZE]
      [(sw_restore SW_RESTORE) RKTIO_SW_RESTORE]
      [(sw_show SW_SHOW) RKTIO_SW_SHOW]
      [(sw_showdefault SW_SHOWDEFAULT) RKTIO_SW_SHOWDEFAULT]
      [(sw_showmaximized SW_SHOWMAXIMIZED) RKTIO_SW_SHOWMAXIMIZED]
      [(sw_showminimized SW_SHOWMINIMIZED) RKTIO_SW_SHOWMINIMIZED]
      [(sw_showminnoactive SW_SHOWMINNOACTIVE) RKTIO_SW_SHOWMINNOACTIVE]
      [(sw_showna SW_SHOWNA) RKTIO_SW_SHOWNA]
      [(sw_shownoactivate SW_SHOWNOACTIVATE) RKTIO_SW_SHOWNOACTIVATE]
      [(sw_shownormal SW_SHOWNORMAL) RKTIO_SW_SHOWNORMAL]
      [else (raise-argument-error who "(or/c 'sw_hide ....)" show-mode)]))
  (define r (rktio_shell_execute rktio
                                 (and verb (string->bytes/utf-8 verb))
                                 (string->bytes/utf-8 target)
                                 (string->bytes/utf-8 parameters)
                                 (->host (->path dir) who '(exists))
                                 show_mode))
  (when (rktio-error? r) (raise-rktio-error 'who "failed" r))
  #f)

;; ----------------------------------------

(void
 (set-get-subprocesses-time!
  (lambda ()
    (rktio_get_process_children_milliseconds rktio))))
