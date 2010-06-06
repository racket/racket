#lang at-exp s-exp "shared.rkt"

(provide download)
(define download
  (page #:link-title "Download" #:window-title "Download Racket"
        #:file "download/"
    "Download page comes here."))

(require "../download/data.rkt")
