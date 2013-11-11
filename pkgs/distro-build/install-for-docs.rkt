#lang racket/base
(require racket/cmdline
         racket/file
         racket/string
         racket/system
         compiler/find-exe
         (only-in "config.rkt" extract-options)
         "display-time.rkt")

(define-values (dir config-file config-mode default-pkgs catalogs)
  (command-line
   #:args
   (dir config-file config-mode default-pkgs . catalog)
   (values dir config-file config-mode default-pkgs catalog)))

(define config (extract-options config-file config-mode))

(define pkgs
  (or (hash-ref config '#:pkgs #f)
      (string-split default-pkgs)))

(define (build-path/s . a)
  (path->string (path->complete-path (apply build-path dir a))))
(define (build-path/f . a)
  (string-append "file://" 
                 (path->string (path->complete-path (apply build-path a)))))

(define ht
  (hash 'doc-dir (build-path/s "doc")
        'lib-dir (build-path/s "lib")
        'share-dir (build-path/s "share")
        'dll-dir (build-path/s "lib")
        'links-file (build-path/s "share" "links.rktd")
        'pkgs-dir (build-path/s "share" "pkgs")
        'bin-dir (build-path/s "bin")
        'include-dir (build-path/s "include")
        'catalogs (map build-path/f catalogs)))

(make-directory* (build-path dir "etc"))

(call-with-output-file*
 (build-path dir "etc" "config.rktd")
 #:exists 'truncate/replace
 (lambda (o)
   (write ht o)
   (newline o)))

(display-time)
(printf "Running `raco pkg install' for packages:\n")
(for ([pkg (in-list pkgs)])
  (printf "  ~a\n" pkg))
(unless (apply system* (find-exe) 
               "-G" "build/docs/etc" "-l-" 
               "raco" "pkg" "install"
               "--pkgs"
               "-i" "--deps" "search-auto"
               pkgs)
  (error "install failed"))

(when (hash-ref config '#:pdf-doc? #f)
  (display-time)
  (printf "Running `raco setup' PDF documentation:\n")
  (unless (system* (find-exe) 
                   "-G" "build/docs/etc" "-l-" 
                   "raco" "setup" "--doc-pdf" "build/pdf-doc")
    (error "PDF failed")))
  
(display-time)
