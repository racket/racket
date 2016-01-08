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

(define-values (default-src-catalog src-catalog)
  (command-line
   #:args
   (default-src-catalog src-catalog)
   (values default-src-catalog src-catalog)))

(define src-catalog-is-default?
  (equal? src-catalog default-src-catalog))

(when (file-exists? config-file-path)
  (call-with-input-file*
   config-file-path
   (lambda (i)
     (define r (read i))
     (define l (hash-ref r 'catalogs #f))
     (define starts-as-expected?
       (and (list? l)
            ((length l) . >= . 1)
            (equal? (car l) catalog-relative-path-str)))
     (define has-src-catalog?
       (or (and src-catalog-is-default?
                (member #f l))
           (member src-catalog l)))
     (unless (and starts-as-expected?
                  has-src-catalog?)
       (error 'pkgs-catalog
              (~a "config file exists, but with a mismatched `catalogs';\n"
                  " the existing configuration does not ~a\n"
                  "  config file: ~a\n"
                  "  expected ~acatalog: ~s\n"
                  "  possible solution: delete the config file")
              (if (not starts-as-expected?)
                  "start as expected"
                  "include the specified catalog")
              config-file-path
              (if (not starts-as-expected?)
                  "initial "
                  "")
              (if (not starts-as-expected?)
                  catalog-relative-path-str
                  src-catalog))))))

(unless (file-exists? config-file-path)
  (printf "Writing ~a\n" config-file-path)
  (call-with-output-file*
   config-file-path
   (lambda (o)
     (write (hash 'catalogs
                  (cons catalog-relative-path-str
                        (append
                         (if src-catalog-is-default?
                             '()
                             (list src-catalog))
                         (list #f)))
                  'installation-name
                  "development"
                  'default-scope
                  "installation")
            o)
     (newline o))))
