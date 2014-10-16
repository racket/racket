#lang racket/base
(require pkg/lib
         racket/cmdline
         net/url)

(define dest-file #f)

(define catalog
  (command-line
   #:once-each
   [("-o") file "Output file"
    (set! dest-file file)]
   #:args
   (catalog)
   catalog))

(define catalog-url
  (if (regexp-match? #rx"^[a-z]+:" catalog)
      (string->url catalog)
      (path->url (path->complete-path catalog))))

(define details
  (parameterize ([current-pkg-catalogs (list catalog-url)])
    (get-all-pkg-details-from-catalogs)))

(define (write-out o)
  (write details o)
  (newline o))

(if dest-file
    (call-with-output-file* dest-file
                            #:exists 'truncate/replace
                            write-out)
    (write-out (current-output-port)))


(module test racket/base)
