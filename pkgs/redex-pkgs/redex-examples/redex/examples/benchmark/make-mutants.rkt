#lang racket

(require racket/cmdline
         racket/runtime-path)

(provide directories
         get-base-stem)

(define-runtime-path stlc "stlc")
(define-runtime-path stlc-sub "stlc-sub")

(define-runtime-path poly-stlc "poly-stlc")
(define-runtime-path rbtrees "rbtrees")
(define-runtime-path delim-cont "delim-cont")
(define-runtime-path list-machine "list-machine")

(define directories (list stlc
                          stlc-sub
                          poly-stlc
                          rbtrees
                          delim-cont
                          list-machine))

(define (make-mutants directory)
  (cond 
    [directory
     (define files
       (for/list ([f (in-directory directory)]) f))
     (match-define (list base stem)
       (get-base-stem files)) 
     (define diffs
       (filter (λ (f) (regexp-match #rx"^.*\\.diff$" (path->string f))) files))
     (for ([f (in-list diffs)])
       (define num (second (regexp-match #rx"^.*/([0-9]+)\\.diff$" f)))
       (define name (string-append stem "-" num ".rkt"))
       (copy-file base name #t)
       (system* (find-executable-path "patch") name (path->string f)))]
    [else
     (map make-mutants directories)]))

(define (get-base-stem files)
  (car (filter-map (λ (f) (regexp-match #rx"^(.*)-base\\.rkt$" (path->string f))) files)))

(module+ main
  (command-line
   #:args ([dir #f])
   (void (make-mutants dir))))
