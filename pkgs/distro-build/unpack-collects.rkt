#lang racket/base
(require racket/cmdline
         racket/file
         racket/port
         net/url
         file/untgz
         "display-time.rkt")

(module test racket/base)

(define dest-dir "bundle/racket")

(define server
  (command-line
   #:args (server)
   server))

(define zip-content
  (port->bytes
   (get-pure-port
    (combine-url/relative
     (string->url server)
     "collects.tgz"))))

(display-time)

(define collects-dir (build-path dest-dir "collects"))
(when (directory-exists? collects-dir)
  (delete-directory/files collects-dir))

(untgz (open-input-bytes zip-content)
       #:dest dest-dir)
