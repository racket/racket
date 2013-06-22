#lang racket/base
(require racket/cmdline
         "installer-sh.rkt"
         "installer-dmg.rkt"
         "installer-exe.rkt"
         net/url
         racket/file
         racket/path)

(define release? #f)
(define upload-to #f)

(define-values (short-human-name human-name dir-name)
  (command-line
   #:once-each
   [("--release") "Create a release installer"
    (set! release? #t)]
   [("--upload") url "Upload installer"
    (set! upload-to url)]
   #:args
   (human-name dir-name)
   (values human-name
           (format "~a v~a" human-name (version))
           (if release?
               dir-name
               (format "~a-~a" dir-name (version))))))

(define installer-file
  (case (system-type)
    [(unix) (installer-sh human-name dir-name release?)]
    [(macosx) (installer-dmg human-name dir-name)]
    [(windows) (installer-exe short-human-name dir-name release?)]))

(call-with-output-file*
 (build-path "bundle" "installer.txt")
 #:exists 'truncate/replace
 (lambda (o) (fprintf o "~a\n" installer-file)))

(when upload-to
  (printf "Upload ~a to ~a\n" installer-file upload-to)
  (define i
    (put-pure-port
     (string->url (format "~aupload/~a"
                          upload-to
                          (path->string (file-name-from-path installer-file))))
     (file->bytes installer-file)))
  (unless (equal? (read i) #t)
    (error "file upload failed")))
