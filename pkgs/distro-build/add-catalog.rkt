#lang racket/base
(require racket/cmdline
         pkg
         pkg/lib
         net/url)

(define (add-catalog! url)
  (define s (url->string url))
  (define l (pkg-config-catalogs))
  (unless (member s l)
    (apply pkg-config-command #:set #t
           "catalogs"
           (append l (list s)))))

(command-line
 #:args
 (dir)
 (add-catalog! (path->url (path->complete-path dir))))
