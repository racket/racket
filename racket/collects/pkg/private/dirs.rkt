#lang racket/base
(require setup/dirs
         racket/file
         racket/path
         "../path.rkt"
         "params.rkt")

;; Finding configurable files and directories

(provide (all-defined-out))

(define (pkg-dir config?)
  (define scope (current-pkg-scope))
  (if (and config?
           (eq? scope 'installation))
      (find-config-dir)
      (get-pkgs-dir scope (current-pkg-scope-version))))

(define (pkg-config-file)
  (build-path (pkg-dir #t) "config.rktd"))

(define (pkg-db-file)
  (build-path (pkg-dir #f) "pkgs.rktd"))

(define (pkg-installed-dir)
  (pkg-dir #f))

(define (pkg-lock-file)
  (make-lock-file-name (pkg-db-file)))

(define (get-all-pkg-scopes)
  (append (let ([main (find-pkgs-dir)])
            (reverse
             (for/list ([d (get-pkgs-search-dirs)])
               (if (equal? d main)
                   'installation
                   (simple-form-path d)))))
          '(user)))
