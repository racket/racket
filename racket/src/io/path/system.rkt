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
         set-host-addon-dir!

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
      [(host-addon-dir) (if (eq? host-addon-dir 'inherit)
                            (find-system-path 'addon-dir)
                            (or host-addon-dir
                                (rktio-system-path who RKTIO_PATH_ADDON_DIR)))]
      [(orig-dir) (as-dir orig-dir)]
      [(temp-dir) (as-dir (rktio-system-path who RKTIO_PATH_TEMP_DIR))]
      [(sys-dir) (as-dir (rktio-system-path who RKTIO_PATH_SYS_DIR))]
      [(pref-dir) (as-dir (rktio-system-path who RKTIO_PATH_PREF_DIR))]
      [(pref-file) (rktio-system-path who RKTIO_PATH_PREF_FILE)]
      [(addon-dir) (as-dir (or addon-dir
                               (rktio-system-path who RKTIO_PATH_ADDON_DIR)))]
      [(cache-dir) (as-dir (rktio-system-path who RKTIO_PATH_CACHE_DIR))]
      [(home-dir) (as-dir (rktio-system-path who RKTIO_PATH_HOME_DIR))]
      [(desk-dir) (as-dir (rktio-system-path who RKTIO_PATH_DESK_DIR))]
      [(doc-dir) (as-dir (rktio-system-path who RKTIO_PATH_DOC_DIR))]
      [(init-dir) (as-dir (rktio-system-path who RKTIO_PATH_INIT_DIR))]
      [(init-file) (rktio-system-path who RKTIO_PATH_INIT_FILE)]
      [else (raise-argument-error who
                                  (string-append
                                   "(or/c 'home-dir 'pref-dir 'pref-file 'temp-dir\n"
                                   "      'init-dir 'init-file 'addon-dir 'cache-dir\n"
                                   "      'doc-dir 'desk-dir 'sys-dir 'exec-file 'run-file\n"
                                   "      'collects-dir 'config-dir 'orig-dir\n"
                                   "      'host-collects-dir 'host-config-dir 'host-addon-dir)")
                                  key)])
    (security-guard-check-file who #f '(exists))))

(define exec-file #f)
(define (set-exec-file! p) (set! exec-file p))

(define run-file #f)
(define (set-run-file! p) (set! run-file p))

(define orig-dir
  (let ()
    (define os-host-dir (let ([dir (rktio_get_current_directory rktio)])
                          (if (rktio-error? dir)
                              ;; If there's an error getting the current directory,
                              ;; just use a root directory
                              (case (system-path-convention-type)
                                [(unix) #"/"]
                                [(windows) #"C:\\"])
                              (rktio_to_bytes dir))))
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
              (start-rktio)
              (path-or-fd-identity 'original-directory #:host-path os-host-dir #:no-error? #t)))
          (define pwd-id
            (begin
              (start-rktio)
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

(define host-addon-dir 'inherit)
(define (set-host-addon-dir! p) (set! host-addon-dir p))

(define (rktio-system-path who key)
  (start-rktio)
  (define s (rktio_system_path rktio key))
  (cond
    [(rktio-error? s)
     (end-rktio)
     (raise-rktio-error who s "path lookup failed")]
    [else
     (define bstr (rktio_to_bytes s))
     (rktio_free s)
     (end-rktio)
     (path bstr (system-path-convention-type))]))

(define (init-current-directory!)
  (current-directory orig-dir)
  (current-directory-for-user orig-dir))

(void (init-current-directory!))
