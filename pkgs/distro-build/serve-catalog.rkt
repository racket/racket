#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/http/response-structs
         web-server/http/request-structs
         net/url
         racket/format
         racket/cmdline
         racket/file
         racket/path
         racket/string
         racket/tcp
         racket/port
         racket/system
         (only-in "config.rkt" extract-options)
         "readme.rkt")

(define from-dir "built")

(define-values (config-file config-mode 
                            default-server-hosts default-server-port 
                            during-cmd-line)
  (command-line
   #:once-each
   [("--mode") dir "Serve package archives from <dir> subdirectory"
    (set! from-dir dir)]
   #:args (config-file config-mode server-hosts server-port . during-cmd)
   (values config-file config-mode 
           server-hosts (string->number server-port)
           during-cmd)))

(define server-hosts
  (hash-ref (extract-options config-file config-mode)
            '#:server-hosts
            (string-split default-server-hosts ",")))
(define server-port
  (hash-ref (extract-options config-file config-mode)
            '#:server-port
            default-server-port))

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

(define (record-installer dir filename desc)
  (when desc
    (define table-file (build-path dir "table.rktd"))
    (call-with-file-lock/timeout 
     #:max-delay 2
     table-file
     'exclusive
     (lambda ()
       (define t (hash-set
                  (if (file-exists? table-file)
                      (call-with-input-file* table-file read)
                      (hash))
                  desc
                  filename))
       (call-with-output-file table-file
         #:exists 'truncate/replace
         (lambda (o) 
           (write t o)
           (newline o))))
     void)))

(define (receive-file req filename)
  (unless (relative-path? filename)
    (error "upload path name must be relative"))
  (define dir (build-path build-dir "installers"))
  (make-directory* dir)
  (call-with-output-file (build-path dir filename)
    #:exists 'truncate/replace
    (lambda (o)
      (write-bytes (request-post-data/raw req) o)))
  (define desc
    (for/or ([h (in-list (request-headers/raw req))])
      (and (equal? (header-field h) #"Description")
           (bytes->string/utf-8 (header-value h)))))
  (record-installer dir filename desc)
  (response/sexpr #t))

(define-values (dispatch main-url)
  (dispatch-rules
   [("pkg" (string-arg)) write-info]
   [("upload" (string-arg)) #:method "put" receive-file]))

;; Tunnel extra hosts to first one:
(when (and (pair? server-hosts)
           (pair? (cdr server-hosts)))
  (for ([host (in-list (cdr server-hosts))])
    (thread
     (lambda ()
       (define l (tcp-listen server-port 5 #t host))
       (let loop ()
         (define-values (i o) (tcp-accept l))
         (define-values (i2 o2) (tcp-connect (car server-hosts) server-port))
         (thread (lambda () 
                   (copy-port i o2)
                   (close-input-port i)
                   (close-output-port o2)))
         (thread (lambda () 
                   (copy-port i2 o)
                   (close-input-port i2)
                   (close-output-port o)))
         (loop))))))

(define (go)
  (serve/servlet
   dispatch
   #:command-line? #t
   #:listen-ip (if (null? server-hosts)
                   #f
                   (car server-hosts))
   #:extra-files-paths
   (append
    (list (build-path build-dir "origin"))
    (list readmes-dir)
    (for/list ([d (in-list dirs)])
      (path->complete-path (build-path d "pkgs")))
    ;; for ".git":
    (list (current-directory)))
   #:servlet-regexp #rx""
   #:port server-port))

(define readmes-dir (build-path build-dir "readmes"))
(make-directory* readmes-dir)

(define readme-file (build-path readmes-dir "README.txt"))
(unless (file-exists? readme-file)
  (printf "Generating default README\n")
  (call-with-output-file*
   readme-file
   (lambda (o)
     (display (make-readme (hash)) o))))

(if (null? during-cmd-line)
    ;; Just run server:
    (go)
    ;; Run server in a background thread, finish by 
    ;; running given command:
    (let ([t (thread go)])
      (sync (system-idle-evt)) ; try to wait until server is ready
      (unless (apply system*
                     (let ([exe (car during-cmd-line)])
                       (if (and (relative-path? exe)
                                (not (path-only exe)))
                           (find-executable-path exe)
                           exe))
                     (cdr during-cmd-line))
        (error 'server-catalog
               "command failed: ~s" 
               during-cmd-line))))
