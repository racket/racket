#lang racket/base
(require "../common/check.rkt"
         "../security/main.rkt"
         "../file/host.rkt"
         (prefix-in raw: "parameter.rkt")
         (rename-in "complete.rkt"
                    [path->complete-path raw:path->complete-path])
         (only-in '#%kernel
                  ;; get `chaperone-procedure` that doesn't support keyword arguments:
                  chaperone-procedure)
         "path.rkt")

(provide path->complete-path
         current-drive

         current-directory
         current-directory-for-user
         current-load-relative-directory)

(define path->complete-path
  (case-lambda
    [(p)
     ;; Supplying `current-directory` (as opposed to `raw:current-directory`)
     ;; triggers an appropriate security-guard check if needed:
     (raw:path->complete-path p current-directory #:wrt-given? #f)]
    [(p wrt) (raw:path->complete-path p wrt #:wrt-given? #t)]))

(define/who (current-drive)
  (security-guard-check-file who #f '(exists))
  (if (eq? (system-path-convention-type) 'unix)
      (string->path "/")
      (error who "not yet ready")))

;; ----------------------------------------

(define (make-guard-paths who)
  (case-lambda
    [()
     (security-guard-check-file who #f '(exists))
     (values)]
    [(path)
     (when (path-string? path)
       (->host path who '(exists)))
     path]))

(define/who current-directory
  (chaperone-procedure raw:current-directory (make-guard-paths who)))

(define/who current-directory-for-user
  (chaperone-procedure raw:current-directory-for-user (make-guard-paths who)))

(define/who current-load-relative-directory
  (chaperone-procedure raw:current-load-relative-directory (make-guard-paths who)))
