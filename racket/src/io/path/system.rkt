#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../security/main.rkt"
         "../file/host.rkt"
         "../envvar/main.rkt"
         "../file/identity.rkt"
         "path.rkt"
         "parameter.rkt"
         "directory-path.rkt")

(provide find-system-path
         set-exec-file!
         set-run-file!
         set-collects-dir!
         set-config-dir!
         set-addon-dir!
         set-host-collects-dir!
         set-host-config-dir!

         init-current-directory!)

(define/who (find-system-path key)
  (define (as-dir p) (path->directory-path p))
  (begin0
    (case key
      [(exec-file) (or exec-file
                       (string->path "/usr/local/bin/racket"))]
      [(run-file) (or run-file
                      (find-system-path 'exec-file))]
      [(config-dir) (as-dir (or config-dir
                                                (string->path "../etc")))]
      [(collects-dir) (as-dir (or collects-dir
                                  (string->path "../collects")))]
      [(host-config-dir) (as-dir (or host-config-dir
                                     config-dir
                                     (string->path "../etc")))]
      [(host-collects-dir) (as-dir (or host-collects-dir
                                       collects-dir
                                       (string->path "../collects")))]
      [(orig-dir) (as-dir orig-dir)]
      [(temp-dir) (as-dir (rktio-system-path who RKTIO_PATH_TEMP_DIR))]
      [(sys-dir) (as-dir (rktio-system-path who RKTIO_PATH_SYS_DIR))]
      [(pref-dir) (as-dir (rktio-system-path who RKTIO_PATH_PREF_DIR))]
      [(pref-file) (rktio-system-path who RKTIO_PATH_PREF_FILE)]
      [(addon-dir) (as-dir (or addon-dir
                               (rktio-system-path who RKTIO_PATH_ADDON_DIR)))]
      [(home-dir) (as-dir (rktio-system-path who RKTIO_PATH_HOME_DIR))]
      [(desk-dir) (as-dir (rktio-system-path who RKTIO_PATH_DESK_DIR))]
      [(doc-dir) (as-dir (rktio-system-path who RKTIO_PATH_DOC_DIR))]
      [(init-dir) (as-dir (rktio-system-path who RKTIO_PATH_INIT_DIR))]
      [(init-file) (rktio-system-path who RKTIO_PATH_INIT_FILE)]
      [else (raise-argument-error who
                                  (string-append
                                   "(or/c 'home-dir 'pref-dir 'pref-file 'temp-dir\n"
                                   "      'init-dir 'init-file 'addon-dir\n"
                                   "      'doc-dir 'desk-dir 'sys-dir 'exec-file 'run-file\n"
                                   "      'collects-dir 'config-dir 'orig-dir\n"
                                   "      'host-collects-dir 'host-config-dir)")
                                  key)])
    (security-guard-check-file who #f '(exists))))
                                 
(define exec-file #f)
(define (set-exec-file! p) (set! exec-file p))

(define run-file #f)
(define (set-run-file! p) (set! run-file p))

(define orig-dir
  (let ()
    (define os-host-dir (rktio_to_bytes (rktio_get_current_directory rktio)))
    (define os-dir (path->directory-path (host-> os-host-dir)))
    (case (system-type 'os)
      [(windows) os-dir]
      [else
       ;; Check `PWD` environment variable, and use it when it
       ;; refers to the same directory as `os-dir`. That's useful
       ;; when `PWD` refers to a link, for example.
       (define pwd (environment-variables-ref (current-environment-variables) #"PWD"))
       (cond
         [(not pwd) os-dir]
         [else
          (define os-dir-id
            (begin
              (start-atomic)
              (path-or-fd-identity 'original-directory #:host-path os-host-dir #:no-error? #t)))
          (define pwd-id
            (begin
              (start-atomic)
              (path-or-fd-identity 'original-directory #:host-path pwd #:no-error? #t)))
          (cond
            [(and os-dir-id (eqv? os-dir-id pwd-id ))
             (path->directory-path (host-> pwd))]
            [else os-dir])])])))

(define collects-dir #f)
(define (set-collects-dir! p) (set! collects-dir p))

(define config-dir #f)
(define (set-config-dir! p) (set! config-dir p))

(define addon-dir #f)
(define (set-addon-dir! p) (set! addon-dir p))

(define host-collects-dir #f)
(define (set-host-collects-dir! p) (set! host-collects-dir p))

(define host-config-dir #f)
(define (set-host-config-dir! p) (set! host-config-dir p))

(define (rktio-system-path who key)
  (start-atomic)
  (define s (rktio_system_path rktio key))
  (cond
    [(rktio-error? s)
     (end-atomic)
     (raise-rktio-error who s "path lookup failed")]
    [else
     (define bstr (rktio_to_bytes s))
     (rktio_free s)
     (end-atomic)
     (path bstr (system-path-convention-type))]))

(define (init-current-directory!)
  (current-directory orig-dir)
  (current-directory-for-user orig-dir))

(void (init-current-directory!))
