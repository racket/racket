#lang racket/base
(require racket/cmdline
         racket/format
         racket/path)

;; Adjust the configuration to consult a catalog that is
;; expected to map some packages to directory links.

;; Used by the top-level Makefile in the main Racket repository.

(define config-dir-path (build-path "racket" "etc"))
(define config-file-path (build-path config-dir-path "config.rktd"))

(define catalog-relative-path (build-path 'up "share" "pkgs-catalog"))
(define catalog-relative-path-str (path->string catalog-relative-path))

(command-line
 #:args
 ()
 (void))

(when (file-exists? config-file-path)
  (call-with-input-file*
   config-file-path
   (lambda (i)
     (define r (read i))
     (define l (hash-ref r 'catalogs #f))
     (unless (and (list? l)
                  ((length l) . >= . 1)
                  (equal? (car l) catalog-relative-path-str))
       (error 'pkgs-catalog
              (~a "config file exists, but does not have a definition of `catalogs' that starts as expected\n"
                  "  config file: ~a\n"
                  "  expected initial element: ~s\n"
                  "  possible solution: delete the config file")
                config-file-path
                catalog-relative-path-str)))))

(unless (file-exists? config-file-path)
  (printf "Writing ~a\n" config-file-path)
  (call-with-output-file*
   config-file-path
   (lambda (o)
     (write (hash 'catalogs
                  (list catalog-relative-path-str #f)
                  'installation-name
                  "development"
                  'default-scope
                  "installation")
            o)
     (newline o))))
