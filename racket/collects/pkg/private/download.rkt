#lang racket/base
(require file/cache
         net/url
         racket/match
         racket/port
         racket/format
         racket/file
         file/tar
         file/untgz
         net/git-checkout
         "path.rkt"
         "print.rkt"
         "config.rkt"
         "network.rkt")

(provide download-file!
         download-repo!
         url-path/no-slash
         clean-cache)

(define (url-path/no-slash url)
  (define p (url-path url))
  (define rp (reverse p))
  (reverse
   (match rp
     [(list* (path/param "" _) rest)
      rest]
     [_ rp])))

(define (do-cache-file file url key checksum use-cache? download-printf download!)
  (cond
   [(and use-cache? checksum)
    (cache-file file
                #:exists-ok? #t
                (list key checksum)
                (get-download-cache-dir)
                download!
                #:log-error-string (lambda (s) (log-pkg-error s))
                #:log-debug-string (lambda (s) (log-pkg-debug s))
                #:notify-cache-use (lambda (s)
                                     (when download-printf
                                       (download-printf "Using ~a for ~a\n"
                                                        s
                                                        (url->string url))))
                #:max-cache-files (get-download-cache-max-files)
                #:max-cache-size (get-download-cache-max-bytes))]
   [else (download!)]))

(define (download-file! url file checksum
                        #:download-printf [download-printf #f]
                        #:use-cache? [use-cache? #t]
                        #:fail-okay? [fail-okay? #f])
  (with-handlers ([exn:fail?
                   (λ (x)
                     (unless fail-okay?
                       (raise x)))])
    (make-parent-directory* file)
    (log-pkg-debug "\t\tDownloading ~a to ~a" (url->string url) file)
    (define (download!)
      (when download-printf
        (download-printf "Downloading ~a\n" (url->string url)))
      (call-with-network-retries
       (lambda ()
         (call-with-output-file*
          file
          #:exists 'truncate/replace
          (λ (op)
            (call/input-url+200
             url
             (λ (ip) (copy-port ip op))
             #:auto-retry? #f
             #:who 'download
             #:not-found-handler
             (lambda (reply-s)
               (pkg-error (~a "error downloading package\n"
                              "  URL: ~a\n"
                              "  server response: ~a")
                          (url->string url)
                          (read-line (open-input-string reply-s))))))))))
    (do-cache-file file url (url->string url) checksum 
                   use-cache? download-printf download!)))

(define (clean-cache pkg-url checksum)
  (when pkg-url
    ;; Something failed after download, so remove cached file (if any):
    (with-handlers ([exn:fail? void]) ; any error is logged already
      (cache-remove (list (url->string pkg-url) checksum)
                    (get-download-cache-dir)
                    #:log-error-string (lambda (s) (log-pkg-error s))
                    #:log-debug-string (lambda (s) (log-pkg-debug s))))))


(define (download-repo! url transport host port repo dest-dir checksum
                        #:download-printf [download-printf #f]
                        #:use-cache? [use-cache? #t])
  (log-pkg-debug "\t\tDownloading ~a to ~a" (url->string url) dest-dir)
  (define tmp.tgz
    (make-temporary-file "~a-repo.tgz" #f))
  (define unpacked? #f)
  
  (define (download!)
    (when download-printf
      (download-printf "Downloading repository ~a\n" (url->string url)))
    (call-with-network-retries
     (lambda ()
       (git-checkout host #:port port repo
                     #:dest-dir dest-dir
                     #:ref checksum
                     #:status-printf (lambda (fmt . args)
                                       (define (strip-ending-newline s)
                                         (regexp-replace #rx"\n$" s ""))
                                       (log-pkg-debug (strip-ending-newline (apply format fmt args))))
                     #:transport transport
                     #:strict-links? #t
                     #:depth 1)))
    (set! unpacked? #t)
    ;; package directory as ".tgz" so it can be cached:
    (parameterize ([current-directory dest-dir])
      (apply tar-gzip tmp.tgz
             #:exists-ok? #t
             (directory-list))))
  
  (do-cache-file tmp.tgz url (vector transport host port repo) checksum 
                 use-cache? download-printf download!)

  (unless unpacked?
    (untgz tmp.tgz #:dest dest-dir))
  
  (delete-file tmp.tgz))
