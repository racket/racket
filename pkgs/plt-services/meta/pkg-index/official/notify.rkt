#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         racket/match
         "common.rkt")

(define (upload-notice! m)
  (define notice-p (format "~a/notice.json" static-path))
  
  (write-to-file m notice-p #:exists 'replace)
  
  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-M"
           "--acl-public"
           "--delete-removed"
           notice-p
           (format "s3://~a/notice.json" s3-bucket))

  (void))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "notify"
   #:args (message)
   (upload-notice! message)))
