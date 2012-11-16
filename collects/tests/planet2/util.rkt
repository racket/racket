#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         planet2/util
         "shelly.rkt")

(define-runtime-path test-directory ".")

(define (get-info-domain-cache-path)
  (define c (first (current-library-collection-paths)))
  (define p (build-path c "info-domain" "compiled" "cache.rktd"))
  (and (file-exists? p)
       p))

(define (with-fake-root* t)
  (define tmp-dir
    (make-temporary-file ".racket.fake-root~a" 'directory
                         (find-system-path 'home-dir)))
  (make-directory* tmp-dir)
  (define tmp-dir-s
    (path->string tmp-dir))
  (define before 
    (or (getenv "PLTADDONDIR")
        (path->string (find-system-path 'addon-dir))))
  (dynamic-wind
      void
      (λ ()
        (putenv "PLTADDONDIR"
                tmp-dir-s)
        (t))
      (λ ()
        (delete-directory/files tmp-dir)
        (putenv "PLTADDONDIR"
                before))))
(define-syntax-rule (with-fake-root e ...)
  (with-fake-root* (λ ()  e ...)))

(define (with-thread start-thread thunk)
  (define thread-id (thread start-thread))
  (dynamic-wind
      void
      thunk
      (λ () (kill-thread thread-id))))

(require web-server/http
         web-server/servlet-env)
(define (start-file-server)
  (serve/servlet (λ (req) (response/xexpr "None"))
                 #:command-line? #t
                 #:port 9999
                 #:extra-files-paths (list (build-path test-directory "test-pkgs"))))

(require meta/planet2-index/basic/main)
(define *index-ht-1* (make-hash))
(define *index-ht-2* (make-hash))
(define (start-planet2-server index-ht port)
  (serve/servlet (planet2-index/basic
                  (λ ()
                    (hash-keys index-ht))
                  (λ (pkg-name)
                    (define r (hash-ref index-ht pkg-name #f))
                    (printf "[>server ~a] ~a = ~a\n" port pkg-name r)
                    r))
                 #:command-line? #t
                 #:servlet-regexp #rx""
                 #:port port))

(define servers-on? #f)
(define (with-servers* t)
  (cond
    [servers-on?
     (t)]
    [else
     (set! servers-on? #t)
     (with-thread
      (λ () (start-planet2-server *index-ht-1* 9990))
      (λ ()
        (with-thread
         (λ () (start-planet2-server *index-ht-2* 9991))
         (λ ()
           (with-thread (λ () (start-file-server))
                        t)))))]))
(define-syntax-rule (with-servers e ...)
  (with-servers* (λ () e ...)))

(define-syntax (pkg-tests stx)
  (syntax-case stx ()
    [(_ e ...)
     (with-syntax
         ([run-pkg-tests (datum->syntax #f 'run-pkg-tests)])
       (syntax/loc stx
         (begin
           (define (run-pkg-tests)
             (shelly-begin
              e ...))
           (provide run-pkg-tests)
           (module+ main
             (run-pkg-tests* run-pkg-tests)))))]))

(define (run-pkg-tests* t)
  (with-servers
   (with-fake-root
    (parameterize ([current-directory test-directory])
      (t)))))

(define-syntax-rule (shelly-install** message pkg rm-pkg (pre ...) (more ...))
  (with-fake-root
   (shelly-case
    (format "Test installation of ~a" message)
    pre ...
    $ "racket -e '(require planet2-test1)'" =exit> 1
    $ (format "raco pkg install ~a" pkg)
    $ "racket -e '(require planet2-test1)'"
    more ...
    $ (format "raco pkg remove ~a" rm-pkg)
    $ "racket -e '(require planet2-test1)'" =exit> 1)))

(define-syntax-rule (shelly-install* message pkg rm-pkg more ...)
  (shelly-install** message pkg rm-pkg () (more ...)))

(define-syntax-rule (shelly-install message pkg more ...)
  (shelly-install* message pkg "planet2-test1" more ...))

(define (initialize-indexes)
  (hash-set! *index-ht-1* "planet2-test1"
             (hasheq 'checksum
                     (file->string "test-pkgs/planet2-test1.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/planet2-test1.zip"))

  (hash-set! *index-ht-1* "planet2-test2"
             (hasheq 'checksum
                     (file->string "test-pkgs/planet2-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/planet2-test2.zip"))
  (hash-set! *index-ht-2* "planet2-test2-snd"
             (hasheq 'checksum
                     (file->string "test-pkgs/planet2-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/planet2-test2.zip")))

(provide (all-defined-out))
