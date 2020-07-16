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
         "path.rkt"
         "relativity.rkt"
         "simplify.rkt"
         "directory-path.rkt"
         (only-in "windows.rkt" split-drive))

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
     (raw:path->complete-path p current-directory-for-path->complete-path #:wrt-given? #f)]
    [(p wrt) (raw:path->complete-path p wrt #:wrt-given? #t)]))

(define/who (current-drive)
  (security-guard-check-file who #f '(exists))
  (if (eq? (system-path-convention-type) 'unix)
      (string->path "/")
      (let ([dir (current-directory)])
        (path (split-drive (path-bytes dir)) 'windows))))

;; ----------------------------------------

(define (make-guard-paths who normalize?)
  (lambda (path)
    (cond
      [(path-string? path)
       (->host path who '(exists))
       (if normalize?
           (path->directory-path (simplify-path path))
           path)]
      [else path])))

(define (make-wrap-paths who)
  (lambda (path)
    (security-guard-check-file who #f '(exists))
    path))

(define/who current-directory
  (make-derived-parameter raw:current-directory
                          (make-guard-paths who #t)
                          (make-wrap-paths who)))

(define current-directory-for-path->complete-path
  (make-derived-parameter raw:current-directory
                          (make-guard-paths 'path->complete-path #f)
                          (make-wrap-paths 'path->complete-path)))

(define/who current-directory-for-user
  (make-derived-parameter raw:current-directory-for-user
                          (make-guard-paths who #t)
                          (make-wrap-paths who)))

(define/who current-load-relative-directory
  (let ([guard (make-guard-paths who #f)])
    (define full-guard
      (case-lambda
        [() (guard)]
        [(v) (when v
               (unless (and (path-string? v)
                            (complete-path? v))
                 (raise-argument-error who "(or/c (and/c path-string? complete-path?) #f)" v)))
             (guard v)]))
    (make-derived-parameter raw:current-load-relative-directory full-guard full-guard)))
