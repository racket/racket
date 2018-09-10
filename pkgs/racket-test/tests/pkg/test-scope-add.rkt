#lang racket/base
(require racket/cmdline
         setup/dirs)

;; This module is meant to be run via "tests-scope.rkt". It adds the
;; given paths to the package-search list, and it adds "links.rktd" in
;; those paths to the links-search list.

(command-line
 #:args
 path
 (unless (null? path)
   (let ([paths path]
         [file (build-path (find-config-dir) "config.rktd")])
     (define ht (call-with-input-file* file read))
     (define new-ht
       (hash-set (hash-set ht
                           'pkgs-search-dirs
                           (append paths (hash-ref ht 'pkgs-search-dirs '(#f))))
                 'links-search-files
                 (append (for/list ([path (in-list paths)])
                           (path->string (build-path path "links.rktd")))
                         (hash-ref ht 'links-search-files '(#f)))))
     (call-with-output-file*
      file
      #:exists 'truncate/replace
      (lambda (o) (write new-ht o))))))
