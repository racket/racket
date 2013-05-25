#lang racket/base
(require racket/cmdline
         racket/file
         racket/port
         net/url
         file/untgz)

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

(define lib-dir (build-path dest-dir "lib"))
(make-directory* lib-dir)

(define collects-dir (build-path lib-dir "collects"))
(when (directory-exists? collects-dir)
  (delete-directory/files collects-dir))

(untgz (open-input-bytes zip-content)
       #:dest lib-dir)
