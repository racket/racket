#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../path/path.rkt"
         "../file/host.rkt"
         "../file/error.rkt"
         "../string/convert.rkt"
         "../locale/string.rkt")

(provide ffi-get-lib
         ffi-get-obj
         current-load-extension)

;; The FFI is mostly implemented in "cs/core/foreign.ss"
;; and `ffi/unsafe`, but rktio provides the implementation
;; of loading and searching shared libraries.

(define (ffi-get-lib who path as-global? fail-as-false? success-k)
  (check who path-string? #:or-false path)
  (check who (procedure-arity-includes/c 1) success-k)
  (define bstr (and path (->host/as-is path #f #f)))
  (start-atomic)
  (define dll (rktio_dll_open rktio bstr as-global?))
  (define err-str (dll-get-error dll))
  (end-atomic)
  (cond
    [(rktio-error? dll)
     (cond
       [fail-as-false? #f]
       [else
        (define msg (string-append "could not load foreign library"
                                   "\n  path: " (if bstr (bytes->string/locale bstr #\?) "[all opened]")))
        (cond
          [err-str
           (raise
            (exn:fail:filesystem
             (string-append (symbol->string who) ": " msg
                            "\n  system error: " (->string err-str))
             (current-continuation-marks)))]
          [else
           (raise-filesystem-error who dll msg)])])]
    [else (success-k dll)]))

(define (ffi-get-obj who dll dll-name name success-k)
  (check who path-string? #:or-false dll-name)
  (check who bytes? name)
  (check who (procedure-arity-includes/c 1) success-k)
  (start-atomic)
  (define obj (rktio_dll_find_object rktio dll name))
  (define err-str (dll-get-error obj))
  (end-atomic)
  (cond
    [(rktio-error? obj)
     (define msg (string-append "could not find export from foreign library"
                                "\n  name: " (bytes->string/utf-8 name #\?)
                                "\n  library: " (if dll-name (bytes->string/locale (path-bytes (->path dll-name)) #\?) "[all opened]")))
     (cond
       [err-str
        (raise
         (exn:fail:filesystem
          (string-append (symbol->string who) ": " msg
                         "\n  system error: " (->string err-str))
          (current-continuation-marks)))]
       [else
        (raise-filesystem-error who dll msg)])]
    [else (success-k obj)]))

;; in atomic mode
(define (dll-get-error v)
  (and (rktio-error? v)
       (let ([p (rktio_dll_get_error rktio)])
         (cond
           [(rktio-error? p)
            (format-rktio-system-error-message v)]
           [else
            (begin0
              (rktio_to_bytes p)
              (rktio_free p))]))))

(define (->string s)
  (if (bytes? s)
      (bytes->string/utf-8 s #\?)
      s))

; ----------------------------------------

(define/who (default-load-extension path sym)
  (check who path-string? path)
  (check who symbol? sym)
  (raise (exn:fail:unsupported
          "default-load-extension: extensions are not supported"
          (current-continuation-marks))))


(define/who current-load-extension
  (make-parameter default-load-extension
                  (lambda (p)
                    (check who (procedure-arity-includes/c 2) p)
                    p)))
