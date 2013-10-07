#lang racket/base
(require web-server/http
         racket/file
         racket/match
         net/url
         racket/list
         racket/path
         racket/promise
         meta/pkg-index/basic/main)

(module+ main
  (require "common.rkt")

  (define pkg-list
    (for/list ([f (in-list (directory-list (build-path pkg-depo pkg-depo-dir)))]
               #:when (regexp-match #rx"\\.plt$" (path->string f)))
      (regexp-replace #rx"\\.plt$" (path->string f) "")))

  (define dispatch
    (pkg-index/basic
     (λ () pkg-list)
     (λ (pkg-name)
       (hasheq 'checksum
               (file->string
                (build-path pkg-depo pkg-depo-dir (format "~a.plt.CHECKSUM" pkg-name)))
               'source
               (format "http://planet-compat.racket-lang.org/~a/~a.plt"
                       pkg-depo-dir pkg-name)
               'url
               (let ()
                 (match-define (regexp #rx"^planet-([^-]+)-(.+)[0-9]+$"
                                       (list _ user pkg))
                               pkg-name)
                 (format "http://planet.racket-lang.org/display.ss?package=~a.plt&owner=~a"
                         pkg user))))))

  (define (url->request u)
    (make-request #"GET" (string->url u) empty
                  (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

  (define (cache url file)
    (define p (build-path cache-dir file))
    (make-directory* (path-only p))
    (void
     (with-output-to-file p
       #:exists 'replace
       (λ () ((response-output (dispatch (url->request url))) (current-output-port))))))

  (cache "/" "index.html")
  (cache "/pkgs" "pkgs")
  (cache "/pkgs-all" "pkgs-all")
  (for ([p (in-list pkg-list)])
    (cache (format "/pkg/~a/display" p) (format "pkg/~a/display/index.html" p))
    (cache (format "/pkg/~a" p) (format "pkg/~a/index.html" p))))
