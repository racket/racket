#lang racket/base
(require rackunit
         racket/system
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         racket/format
         setup/dirs
         pkg/util
         "shelly.rkt")

(define-runtime-path test-directory ".")

(define-syntax-rule (this-test-is-run-by-the-main-test)
  (module test racket/base))

(define (get-info-domain-cache-path)
  (define c (first (current-library-collection-paths)))
  (define p (build-path c "info-domain" "compiled" "cache.rktd"))
  (and (file-exists? p)
       p))

(define (with-fake-installation* t #:default-scope [default-scope "installation"])
  (define tmp-dir
    (make-temporary-file ".racket.fake-installation~a" 'directory
                         (find-system-path 'temp-dir)))
  (make-directory* tmp-dir)
  (dynamic-wind
      void
      (λ ()
         (define ->s path->string)
         (define config
           (hash
            ;; redirect main installation via "share" to
            ;; our temporary directory:
            'share-dir
            (->s (build-path tmp-dir))

            'installation-name
            "test"

            'default-scope
            default-scope

            ;; Find existing links and packages from the
            ;; old configuration:
            'links-search-files 
            (cons #f
                  (map ->s (get-links-search-files)))
            'pkgs-search-dirs
            (cons #f
                  (map ->s (get-pkgs-search-dirs)))))
         (call-with-output-file*
          (build-path tmp-dir "config.rktd")
          (lambda (o)
            (write config o)
            (newline o)))
         (define tmp-dir-s
           (path->string tmp-dir))
         (parameterize ([current-environment-variables
                         (environment-variables-copy
                          (current-environment-variables))])
           (putenv "PLTCONFIGDIR" tmp-dir-s)
	   (putenv "PATH" (~a (find-console-bin-dir)
			      ":"
			      (getenv "PATH")))
           (t)))
      (λ ()
        (delete-directory/files tmp-dir))))
(define-syntax-rule (with-fake-installation e ...)
  (with-fake-installation* (λ ()  e ...)))

(define (with-fake-root* t)
  (define tmp-dir
    (make-temporary-file ".racket.fake-root~a" 'directory
                         (find-system-path 'home-dir)))
  (make-directory* tmp-dir)
  (dynamic-wind
      void
      (λ ()
         (define tmp-dir-s
           (path->string tmp-dir))
         (parameterize ([current-environment-variables
                         (environment-variables-copy
                          (current-environment-variables))])
           (putenv "PLTADDONDIR" tmp-dir-s)
           (t)))
      (λ ()
        (delete-directory/files tmp-dir))))
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

(require "basic-index.rkt")
(define *index-ht-1* (make-hash))
(define *index-ht-2* (make-hash))
(define (start-pkg-server index-ht port)
  (parameterize ([current-error-port (current-output-port)])
    (serve/servlet (pkg-index/basic
                    (λ (pkg-name)
                      (define r (hash-ref index-ht pkg-name #f))
                      (printf "[>server ~a] ~a = ~a\n" port pkg-name r)
                      r)
                    (λ () index-ht))
                   #:command-line? #t
                   #:servlet-regexp #rx""
                   #:port port)))

(define servers-on? #f)
(define (with-servers* t)
  (cond
    [servers-on?
     (t)]
    [else
     (set! servers-on? #t)
     (with-thread
      (λ () (start-pkg-server *index-ht-1* 9990))
      (λ ()
        (with-thread
         (λ () (start-pkg-server *index-ht-2* 9991))
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
  (putenv "PLT_PKG_NOSETUP" "y")
  (with-servers
   (with-fake-installation*
    #:default-scope "user"
    (lambda ()
      (shelly-case "setup info cache" $ "raco setup -nDKxiI --no-foreign-libs")
      (with-fake-root
       (parameterize ([current-directory test-directory])
         (t)))))))

(define-syntax-rule (shelly-install** message pkg rm-pkg (pre ...) (more ...))
  (with-fake-root
   (shelly-case
    (format "Test installation of ~a" message)
    pre ...
    $ "racket -e '(require pkg-test1)'" =exit> 1
    $ (format "raco pkg install ~a" pkg)
    $ "racket -e '(require pkg-test1)'"
    more ...
    $ (format "raco pkg remove ~a" rm-pkg)
    $ "racket -e '(require pkg-test1)'" =exit> 1)))

(define-syntax-rule (shelly-install* message pkg rm-pkg more ...)
  (shelly-install** message pkg rm-pkg () (more ...)))

(define-syntax-rule (shelly-install message pkg more ...)
  (shelly-install* message pkg "pkg-test1" more ...))

(define (initialize-catalogs)
  (hash-set! *index-ht-1* "pkg-test1"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-test1.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-test1.zip"
                     'tags
                     '("first")))

  (hash-set! *index-ht-1* "pkg-test2"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-test2.zip"
                     'dependencies
                     '("pkg-test1")))

  (hash-set! *index-ht-2* "pkg-test2-snd"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-test2.zip"
                     'dependencies
                     '("pkg-test1"))))

(provide (all-defined-out))
