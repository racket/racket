#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/http/response-structs
         web-server/http/request-structs
         net/url
         racket/format
         racket/cmdline)

(define from-dir "built")

(command-line
 #:once-each
 [("--mode") dir "Serve package archives from <dir> subdirectory"
  (set! from-dir dir)]
 #:args ()
 (void))


(define build-dir (path->complete-path "build"))
(define built-dir (build-path build-dir from-dir))
(define native-dir (build-path build-dir "native"))

(define dirs (list built-dir native-dir))

(define (pkg-name->info req name)
  (define (extract-host-header sel)
    (for/or ([h (in-list (request-headers/raw req))])
      (and (equal? (header-field h) #"Host")
           (let ([m (regexp-match #rx#"^(.*):([0-9]+)$"
                                  (header-value h))])
             (and m
                  (sel (list (bytes->string/utf-8 (cadr m))
                             (string->number (bytes->string/utf-8 (caddr m))))))))))
  (for/or ([d (in-list dirs)])
    (define f (build-path d "catalog" "pkg" name))
    (and (file-exists? f)
         (let ([h (call-with-input-file*
                   f
                   read)])
           (define s (hash-ref h 'source))
           (hash-set h
                     'source
                     (url->string
                      (url "http"
                           #f
                           (or (extract-host-header car)
                               (let ([h (request-host-ip req)])
                                 (if (equal? h "::1")
                                     "localhost"
                                     h)))
                           (or (extract-host-header cadr)
                               (request-host-port req))
                           #t
                           (list (path/param (~a name ".zip") null))
                           null
                           #f)))))))

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" null
            (λ (op) (write v op))))

(define (write-info req pkg-name)
  (response/sexpr (pkg-name->info req pkg-name)))

(define-values (dispatch main-url)
  (dispatch-rules
   [("pkg" (string-arg)) write-info]))

(serve/servlet
 dispatch
 #:command-line? #t
 #:listen-ip #f
 #:extra-files-paths
 (cons
  (build-path build-dir "origin")
  (for/list ([d (in-list dirs)])
    (path->complete-path (build-path d "pkgs"))))
 #:servlet-regexp #rx""
 #:port 9440)
