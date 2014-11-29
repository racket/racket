#lang racket/base
(require racket/list
         net/url
         racket/port)

(define (upload! the-email the-password the-post)
  (define the-url
    (url "https" #f "pkgd.racket-lang.org" #f #t
         (list (path/param "api" empty)
               (path/param "upload" empty))
         empty
         #f))
  (displayln the-email)
  (displayln the-post)
  (define bs
    (call/input-url the-url
                    (λ (url)
                      (post-pure-port the-url
                                      (with-output-to-bytes
                                       (λ ()
                                         (write (list the-email
                                                      (string->bytes/utf-8 the-password)
                                                      the-post))))))
                    port->bytes))
  (define v (port->string (open-input-bytes bs)))
  (displayln v)
  v)

(module+ main
  (require racket/cmdline)
  (command-line #:program "upload"
                #:args (email password)
                (if (upload! email password
                             (read (current-input-port)))
                  (exit 0)
                  (exit 1))))
