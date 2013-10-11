#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         file/gzip
         racket/match
         "common.rkt")

(define (make-parent-directory* p)
  (make-directory* (path-only p)))

(define s3-config (build-path (find-system-path 'home-dir) ".s3cfg-plt"))
(define s3-bucket "pkgs.racket-lang.org")

(define static.gz-path (path-add-suffix static-path ".gz"))

(define s3cmd-path (find-executable-path "s3cmd"))

(define (upload-all)
  (define root (simple-form-path static-path))

  (for ([f (in-directory root)]
        #:when (file-exists? f))
    (define f.time (file-or-directory-modify-seconds f))

    (define rf (find-relative-path root f))
    (define gf (build-path static.gz-path rf))
    (make-parent-directory* gf)

    (define g.time (file-or-directory-modify-seconds gf))

    (when (> f.time g.time)
      (printf "gzipping ~a\n" f)
      (gzip f gf)))

  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-M"
           "--acl-public"
           "--add-header" "Content-Encoding:gzip"
           "--delete-removed"
           (format "~a/" static.gz-path)
           (format "s3://~a/" s3-bucket)))

(define (upload-pkgs pkgs)
  ;; FUTURE make this more efficient
  (upload-all))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "s3"
   #:args pkgs
   (cond
     [(empty? pkgs)
      (upload-all)]
     [else
      (upload-pkgs pkgs)])))
