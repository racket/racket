#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         racket/match
         "common.rkt")

(define (upload-notice! m)
  (display-to-file m notice-path #:exists 'replace))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "notify"
   #:args (message)
   (upload-notice! message)))

(define (do-notify! l)
  (upload-notice! (first l)))
(define (notify! m)
  (run! do-notify! (list m)))

(provide do-notify!
         notify!)
