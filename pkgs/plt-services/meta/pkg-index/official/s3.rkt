#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         file/gzip
         racket/match
         "common.rkt")

(define s3-config (build-path (find-system-path 'home-dir) ".s3cfg-plt"))
(define s3-bucket "pkgs.racket-lang.org")

(define s3cmd-path (find-executable-path "s3cmd"))

(define (upload-all)
  (gzip (format "~a/pkgs-all.json" static-path)
        (format "~a/pkgs-all.json.gz" static-path))

  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-m" "application/javascript"
           "--acl-public"
           "--add-header" "Content-Encoding: gzip"
           "--delete-removed"
           (format "~a/pkgs-all.json.gz" static-path)
           (format "s3://~a/pkgs-all.json.gz" s3-bucket))

  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-M"
           "--acl-public"
           "--delete-removed"
           (format "~a/" static-path)
           (format "s3://~a/" s3-bucket))

  

  (void))

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
