#lang scheme/base

(require scheme/unit "file-sig.rkt")

(provide dynext:file@)

(define-unit dynext:file@ (import) (export dynext:file^)

  (define (append-zo-suffix s)
    (path-add-suffix s #".zo"))

  (define (append-c-suffix s)
    (path-add-suffix s #".c"))

  (define (append-constant-pool-suffix s)
    (path-add-suffix s #".kp"))

  (define (append-object-suffix s)
    (path-add-suffix s (case (system-type)
                         [(unix beos macos macosx) #".o"]
                         [(windows) #".obj"])))

  (define (append-extension-suffix s)
    (path-add-suffix s (system-type 'so-suffix)))

  (define (extract-suffix appender)
    (subbytes (path->bytes (appender (bytes->path #"x"))) 1))

  (define-values (extract-base-filename/ss
                  extract-base-filename/c
                  extract-base-filename/kp
                  extract-base-filename/o
                  extract-base-filename/ext)
    (let ([mk
           (lambda (who pat kind simple)
             (define rx
               (byte-pregexp (bytes-append #"^(.*)\\.(?i:" pat #")$")))
             (define (extract-base-filename s [p #f])
               (unless (path-string? s)
                 (raise-type-error who "path or valid-path string" s))
               (cond [(regexp-match
                       rx (path->bytes (if (path? s) s (string->path s))))
                      => (lambda (m) (bytes->path (cadr m)))]
                     [p (if simple
                          (error p "not a ~a filename (doesn't end with ~a): ~a"
                                 kind simple s)
                          (path-replace-suffix s #""))]
                     [else #f]))
             extract-base-filename)])
      (values
       (mk 'extract-base-filename/ss #"rkt|ss|scm" "Racket" #f)
       (mk 'extract-base-filename/c
           #"c|cc|cxx|cpp|c[+][+]|m" "C" ".c, .cc, .cxx, .cpp, .c++, or .m")
       (mk 'extract-base-filename/kp #"kp" "constant pool" ".kp")
       (mk 'extract-base-filename/o
           (case (system-type)
             [(unix beos macos macosx) #"o"]
             [(windows) #"obj"])
           "compiled object"
           (extract-suffix append-object-suffix))
       (mk 'extract-base-filename/ext
           (regexp-quote (subbytes (system-type 'so-suffix) 1) #f)
           "Racket extension"
           (extract-suffix append-extension-suffix))))))
