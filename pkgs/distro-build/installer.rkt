#lang racket/base
(require racket/cmdline
         "installer-sh.rkt"
         "installer-dmg.rkt"
         "installer-exe.rkt"
         "installer-tgz.rkt"
         net/url
         racket/file
         racket/path
         racket/port
         "display-time.rkt")

(define release? #f)
(define source? #f)
(define upload-to #f)
(define upload-desc "")
(define download-readme #f)

(define-values (short-human-name human-name base-name dir-name dist-suffix)
  (command-line
   #:once-each
   [("--release") "Create a release installer"
    (set! release? #t)]
   [("--source") "Create a source installer"
    (set! source? #t)]
   [("--upload") url "Upload installer"
    (set! upload-to url)]
   [("--desc") desc "Description to accompany upload"
    (set! upload-desc desc)]
   [("--readme") readme "URL for README.txt to include"
    (set! download-readme readme)]
   #:args
   (human-name base-name dir-name dist-suffix)
   (values human-name
           (format "~a v~a" human-name (version))
           (format "~a-~a" base-name (version))
           (if release?
               dir-name
               (format "~a-~a" dir-name (version)))
           (if (string=? dist-suffix "")
               ""
               (string-append "-" dist-suffix)))))

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
        [(macosx) (installer-dmg human-name base-name dist-suffix readme)]
        [(windows) (installer-exe short-human-name base-name release? dist-suffix readme)])))

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
     (string->url (format "~aupload/~a"
                          upload-to
                          (path->string (file-name-from-path installer-file))))
     (file->bytes installer-file)
     (list (string-append "Description: " upload-desc))))
  (unless (equal? (read i) #t)
    (error "file upload failed")))

(display-time)
