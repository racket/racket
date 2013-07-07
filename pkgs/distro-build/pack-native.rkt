#lang racket/base
(require racket/cmdline
         racket/file
         racket/port
         file/zip
         openssl/sha1
         net/url
         pkg/strip)

(command-line
 #:args
 ()
 (void))

(define src-dir (build-path "build" "native-pkgs"))
(define dest-dir (build-path "build" "native" "pkgs"))
(define catalog-dir (build-path "build" "native" "catalog"))
(define local-catalog-dir (build-path "build" "local" "catalog"))

(make-directory* dest-dir)
(make-directory* catalog-dir)
(make-directory* local-catalog-dir)

(define (stream-directory d)
  (define-values (i o) (make-pipe (* 100 4096)))
  (thread (lambda ()
            (for ([f (in-directory d)])
              (cond
               [(directory-exists? f)
                (write (directory-list f) o)]
               [(file-exists? f)
                (call-with-input-file* 
                 f
                 (lambda (i) (copy-port i o)))]))
            (close-output-port o)))
  i)

(for ([pkg-name (in-list (directory-list src-dir))])
  (when (file-exists? (build-path src-dir pkg-name "info.rkt"))
    (define zip-file (path-add-suffix pkg-name #".zip"))
    (define sum-file (path-add-suffix pkg-name #".srcsum"))
    (define pkg-src-dir (build-path src-dir pkg-name))
    (printf "summing ~a\n" pkg-src-dir)
    (define src-sha1 (sha1 (stream-directory pkg-src-dir)))
    (define dest-zip (build-path (path->complete-path dest-dir) zip-file))
    (define dest-sum (build-path (path->complete-path dest-dir) sum-file))
    (unless (and (file-exists? dest-zip)
                 (file-exists? dest-sum)
                 (equal? (list (version) src-sha1)
                         (call-with-input-file* dest-sum read)))
      (printf "packing ~a\n" zip-file)
      (define tmp-dir (make-temporary-file "~a-pkg" 'directory))
      (generate-stripped-directory 'binary pkg-src-dir tmp-dir)
      (parameterize ([current-directory tmp-dir])
        (when (file-exists? dest-zip) (delete-file dest-zip))
        (apply zip dest-zip (directory-list)))
      (delete-directory/files tmp-dir)
      (call-with-output-file*
       dest-sum
       #:exists 'truncate/replace
       (lambda (o)
         (write (list (version) src-sha1) o)
         (newline o))))
    
    (define (write-catalog-entry catalog-dir)
      (define catalog-pkg-dir (build-path catalog-dir "pkg"))
      (make-directory* catalog-pkg-dir)
      (call-with-output-file*
       (build-path catalog-pkg-dir pkg-name)
       #:exists 'truncate
       (lambda (o)
         (write (hash 'source (path->string dest-zip)
                      'checksum (call-with-input-file* dest-zip sha1)
                      'name (path->string pkg-name)
                      'author "plt@racket-lang.org"
                      'description "native library"
                      'tags '()
                      'dependencies '()
                      'modules '())
                o)
         (newline o))))
    (write-catalog-entry catalog-dir)
    (write-catalog-entry local-catalog-dir)))
