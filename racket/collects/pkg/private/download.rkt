#lang racket/base
(require file/cache
         net/url
         racket/match
         racket/port
         racket/format
         "path.rkt"
         "print.rkt"
         "config.rkt")

(provide call/input-url+200
         download-file!
         url-path/no-slash
         clean-cache)

(define (call/input-url+200 u fun
                            #:headers [headers '()]
                            #:failure [fail-k (lambda (s) #f)])
  #;(printf "\t\tReading ~a\n" (url->string u))
  (define-values (ip hs) (get-pure-port/headers u headers
                                                #:redirections 25
                                                #:status? #t))
  (if (string=? "200" (substring hs 9 12))
      (begin0
       (fun ip)
       (close-input-port ip))
      (fail-k hs)))

(define (url-path/no-slash url)
  (define p (url-path url))
  (define rp (reverse p))
  (reverse
   (match rp
     [(list* (path/param "" _) rest)
      rest]
     [_ rp])))

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
      (call-with-output-file file
        (λ (op)
          (call/input-url+200
           url
           (λ (ip) (copy-port ip op))
           #:failure
           (lambda (reply-s)
             (pkg-error (~a "error downloading package\n"
                            "  URL: ~a\n"
                            "  server response: ~a")
                        (url->string url)
                        (read-line (open-input-string reply-s))))))))
    (cond
     [(and checksum use-cache?)
      (cache-file file
                  (list (url->string url) checksum)
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
     [else (download!)])))

(define (clean-cache pkg-url checksum)
  (when pkg-url
    ;; Something failed after download, so remove cached file (if any):
    (with-handlers ([exn:fail? void]) ; any error is logged already
      (cache-remove (list (url->string pkg-url) checksum)
                    (get-download-cache-dir)
                    #:log-error-string (lambda (s) (log-pkg-error s))
                    #:log-debug-string (lambda (s) (log-pkg-debug s))))))
