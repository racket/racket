#lang racket/base
(require net/url
         net/head
         racket/format
         racket/file
         racket/port)

(provide download-installer)

(define (download-installer snapshot-url installer-dir installer-name substatus)
  (define status-file (build-path installer-dir "status.rktd"))
  (define name+etag (and (file-exists? status-file)
                         (call-with-input-file*
                          status-file
                          read)))
  (define installer-url (combine-url/relative (string->url snapshot-url)
                                              (~a "installers/" installer-name)))
  (define etag
    (cond
     [(equal? (url-scheme installer-url) "file")
      #f]
     [else
      (define p (head-impure-port installer-url))
      (define h (purify-port p))
      (close-input-port p)
      (extract-field "ETag" h)]))
  (cond
   [(and (file-exists? (build-path installer-dir installer-name))
         name+etag
         (equal? (car name+etag) installer-name)
         (cadr name+etag)
         (equal? (cadr name+etag) etag))
    (substatus "Using cached installer, Etag ~a\n" etag)]
   [else
    (delete-directory/files installer-dir #:must-exist? #f)
    (make-directory* installer-dir)
    (call/input-url
     installer-url
     get-pure-port
     (lambda (i)
       (call-with-output-file*
        (build-path installer-dir installer-name)
        #:exists 'replace
        (lambda (o)
          (copy-port i o)))))
    (when etag
      (call-with-output-file*
       status-file
       (lambda (o)
         (write (list installer-name etag) o)
         (newline o))))]))
