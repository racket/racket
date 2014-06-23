#lang racket/base
(require racket/cmdline
         "installer-sh.rkt"
         "installer-dmg.rkt"
         "installer-pkg.rkt"
         "installer-exe.rkt"
         "installer-tgz.rkt"
         net/url
         racket/file
         racket/path
         racket/port
         "display-time.rkt")

(module test racket/base)

(define release? #f)
(define source? #f)
(define versionless? #f)
(define mac-pkg? #f)
(define upload-to #f)
(define upload-desc "")
(define download-readme #f)

(define-values (short-human-name human-name base-name dir-name dist-suffix sign-identity)
  (command-line
   #:once-each
   [("--release") "Create a release installer"
    (set! release? #t)]
   [("--source") "Create a source installer"
    (set! source? #t)]
   [("--versionless") "Avoid version number in names and paths"
    (set! versionless? #t)]
   [("--mac-pkg") "Create a \".pkg\" installer on Mac OS X"
    (set! mac-pkg? #t)]
   [("--upload") url "Upload installer"
    (unless (string=? url "")
      (set! upload-to url))]
   [("--desc") desc "Description to accompany upload"
    (set! upload-desc desc)]
   [("--readme") readme "URL for README.txt to include"
    (unless (string=? readme "")
      (set! download-readme readme))]
   #:args
   (human-name base-name dir-name dist-suffix sign-identity)
   (values human-name
           (format "~a v~a" human-name (version))
           (if versionless?
               base-name
               (format "~a-~a" base-name (version)))
           (if (or (and release? (not source?))
                   versionless?)
               dir-name
               (format "~a-~a" dir-name (version)))
           (if (string=? dist-suffix "")
               ""
               (string-append "-" dist-suffix))
           sign-identity)))

(display-time)

(define readme
  (and download-readme
       (let ()
         (printf "Downloading ~a\n" download-readme)
         (define i (get-pure-port (string->url download-readme)))
         (begin0
          (port->string i)
          (close-input-port i)))))

(define installer-file
  (if source?
      (installer-tgz base-name dir-name dist-suffix readme)
      (case (system-type)
        [(unix) (installer-sh human-name base-name dir-name release? dist-suffix readme)]
        [(macosx) (if mac-pkg?
                      (installer-pkg (if (or release? versionless?)
                                         short-human-name
                                         human-name)
                                     base-name dist-suffix readme sign-identity)
                      (installer-dmg (if versionless?
                                         short-human-name
                                         human-name)
                                     base-name dist-suffix readme sign-identity))]
        [(windows) (installer-exe short-human-name base-name (or release? versionless?) 
                                  dist-suffix readme)])))

(call-with-output-file*
 (build-path "bundle" "installer.txt")
 #:exists 'truncate/replace
 (lambda (o) 
   (fprintf o "~a\n" installer-file)
   (fprintf o "~a\n" upload-desc)))

(when upload-to
  (printf "Upload ~a to ~a\n" installer-file upload-to)
  (define i
    (put-pure-port
     (string->url (format "~a~a"
                          upload-to
                          (path->string (file-name-from-path installer-file))))
     (file->bytes installer-file)
     (list (string-append "Description: " upload-desc))))
  (unless (equal? (read i) #t)
    (error "file upload failed")))

(display-time)
