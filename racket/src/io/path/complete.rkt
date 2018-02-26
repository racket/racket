#lang racket/base
(require "../common/internal-error.rkt"
         "path.rkt"
         "check.rkt"
         "check-path.rkt"
         "relativity.rkt"
         "build.rkt"
         "windows.rkt")

(provide path->complete-path)

;; If `wrt-given?` is #f, then `wrt` can be a thunk to get a path,
;; so that any security checks associated with the thunk are delayed
(define/who (path->complete-path p-in wrt #:wrt-given? [wrt-given? #t])
  (check-path-argument who p-in)
  (when wrt-given?
    (check who (lambda (p) (and (or (path-string? p) (path-for-some-system? p))
                                (complete-path? p)))
           #:contract "(and/c (or/c path-string? path-for-some-system?) complete-path?)"
           wrt))
  (unless (eq? (convention-of-path p-in)
               (if (procedure? wrt)
                   (system-path-convention-type)
                   (convention-of-path wrt)))
    (if wrt-given?
        (raise-arguments-error who
                               "convention of first path incompatible with convention of second path"
                               "first path" p-in
                               "second path" wrt)
        (raise-arguments-error who
                               "no second path supplied, and given path is not for the current platform"
                               "given path" p-in)))
  (define p (->path p-in))
  (cond
   [(complete-path? p) p]
   [(relative-path? p)
    (build-path (if (procedure? wrt) (wrt) wrt) p)]
   [else
    ;; non-complete, non-relative path on Windows, so fill in the drive
    (define wrt-path (->path (if (procedure? wrt) (wrt) wrt)))
    (define drive (split-drive (path-bytes wrt-path)))
    (build-path (path drive 'windows) p)]))

(define (convention-of-path p)
  (if (path? p)
      (path-convention p)
      (system-path-convention-type)))
