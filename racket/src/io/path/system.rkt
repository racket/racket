#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../security/main.rkt"
         "path.rkt")

(provide find-system-path
         set-exec-file!
         set-run-file!
         set-collects-dir!
         set-config-dir!)

(define/who (find-system-path key)
  (begin0
    (case key
      [(exec-file) (or exec-file
                       (string->path "/usr/local/bin/racket"))]
      [(run-file) (or run-file
                      (find-system-path 'exec-file))]
      [(config-dir host-config-dir) (or config-dir
                                        (string->path "../etc"))]
      [(collects-dir host-collects-dir) (or collects-dir
                                            (string->path "../collects"))]
      [(orig-dir) (string->path (|#%app| current-directory))]
      [(temp-dir) (rktio-system-path who RKTIO_PATH_TEMP_DIR)]
      [(sys-dir) (rktio-system-path who RKTIO_PATH_SYS_DIR)]
      [(pref-dir) (rktio-system-path who RKTIO_PATH_PREF_DIR)]
      [(pref-file) (rktio-system-path who RKTIO_PATH_PREF_FILE)]
      [(addon-dir) (rktio-system-path who RKTIO_PATH_ADDON_DIR)]
      [(home-dir) (rktio-system-path who RKTIO_PATH_HOME_DIR)]
      [(desk-dir) (rktio-system-path who RKTIO_PATH_DESK_DIR)]
      [(doc-dir) (rktio-system-path who RKTIO_PATH_DOC_DIR)]
      [(init-dir) (rktio-system-path who RKTIO_PATH_INIT_DIR)]
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

(define collects-dir #f)
(define (set-collects-dir! p) (set! collects-dir p))

(define config-dir #f)
(define (set-config-dir! p) (set! config-dir p))

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
