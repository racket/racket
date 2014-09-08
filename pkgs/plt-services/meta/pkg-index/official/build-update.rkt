#lang racket/base
(require racket/list
         racket/match
         racket/file
         racket/port
         net/http-client
         (prefix-in pkg: pkg/lib)
         "common.rkt")

(define SUMMARY-HOST "pkg-build.racket-lang.org")
(define SUMMARY-URL (string-append "/" SUMMARY-NAME))
(define SUMMARY-ETAG-PATH (build-path cache-path (format "~a.etag" SUMMARY-NAME)))

(define (extract-tag hs)
  (or
   (for/or ([h (in-list hs)])
     (match h
       [(regexp
         #rx#"^ETag: (.*?)$"
         (list _ tag-bys))
        tag-bys]
       [_
        #f]))
   #""))

(define (file->bytes* p d)
  (if (file-exists? p)
    (file->bytes p)
    d))

(define (build-update!)
  (define cur-version
    (file->bytes* SUMMARY-ETAG-PATH #""))
  (printf "Current: ~v\n" cur-version)

  (define-values
    (_0 head-headers _1)
    (http-sendrecv
     SUMMARY-HOST SUMMARY-URL
     #:method #"HEAD"))
  (define head-version
    (extract-tag head-headers))
  (printf "Head: ~v\n" head-version)

  (unless (bytes=? cur-version head-version)
    (define-values
      (_2 get-headers get-ip)
      (http-sendrecv
       SUMMARY-HOST SUMMARY-URL
       #:method #"GET"))
    (define get-version
      (extract-tag get-headers))
    (printf "Get: ~v\n" get-version)

    (define new-file
      (make-temporary-file "summary-~a.rktd" #f cache-path))
    (call-with-output-file new-file
      #:exists 'truncate/replace
      (λ (new-op)
        (copy-port get-ip new-op)))

    (with-output-to-file SUMMARY-ETAG-PATH
      #:exists 'truncate/replace
      (λ ()
        (write-bytes get-version)))

    (rename-file-or-directory new-file SUMMARY-PATH #t)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "build-update"
   #:args ()
   (build-update!)))
