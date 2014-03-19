#lang racket

(require racket/cmdline
         "apply-diffs.rkt")

(define (make-diffs directory)
  (cond 
    [directory
     (define files
       (for/list ([f (in-directory directory)]) f))
     (match-define (list base stem)
       (get-base-stem files))
     (define mutant-matches
       (filter-map (λ (f) (regexp-match #rx"^.*-([0-9]+)\\.rkt$" (path->string f))) files))
     (define diffs
       (for/list ([mm (in-list mutant-matches)])
         (match-define (list name num) mm)
         (list num
               (with-output-to-string
                (λ () (system* (find-executable-path "diff") base name))))))
     (for ([nd (in-list diffs)])
       (match-define (list nm diff) nd)
       (define fname (build-path directory (string-append nm ".diff")))
       (call-with-output-file (path->string fname)
         (λ (out)
           (display diff out))
         #:exists 'replace))]
    [else
     (map make-diffs directories)]))

(module+ main
  (command-line
   #:args ([dir #f])
   (begin
     (when dir
       (unless (member dir directories)
         (raise-user-error 'make-diffs.rkt 
                           "expected one of the following directories: ~s" 
                           directories)))
     (void (make-diffs dir)))))
