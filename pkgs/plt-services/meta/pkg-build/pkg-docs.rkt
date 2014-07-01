#lang racket/base
(require racket/cmdline
         setup/getinfo
         setup/dirs
         pkg/path)

;; This module is copied to the virtual machine to extract
;; a package -> documentation mapping.

(define all-pkgs? #f)

(define want-pkgs
  (command-line
   #:once-each
   [("--all") "All packages"
    (set! all-pkgs? #t)]
   #:args
   want-pkg
   want-pkg))

(define dirs (find-relevant-directories '(scribblings)))
(define cache (make-hash))

(define ht
  (for/fold ([ht (hash)]) ([dir (in-list dirs)])
    (define pkg (path->pkg dir #:cache cache))
    (cond
     [(or all-pkgs?
          (member pkg want-pkgs))
      (define i (get-info/full dir))
      (define scribblings (if i (i 'scribblings (lambda () null)) null))
      (for/fold ([ht ht]) ([scribbling (in-list scribblings)])
        (cond
         [(and (list? scribbling)
               (<= 1 (length scribbling) 6)
               (path-string? (car scribbling))
               (or (< (length scribbling) 4)
                   (string? (list-ref scribbling 3))))
          (define path (path->complete-path (car scribbling) dir))
          (define name
            (cond
             [(>= (length scribbling) 4)
              (list-ref scribbling 3)]
             [else
              (define-values (base name dir?) (split-path path))
              (path->string (path-replace-suffix name #""))]))
          (hash-update ht pkg (lambda (l) (cons name l)) null)]
         [else ht]))]
     [else ht])))

(write ht) (newline)
