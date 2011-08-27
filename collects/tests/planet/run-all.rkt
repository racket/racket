#lang racket

#|

Run all of the PLaneT tests sequentially since they 
manipulate the planet state in ways that doesn't work
in parallel

|#

(require racket/runtime-path)
(define-runtime-path me "run-all.rkt")

(define ran-tests '())

(dynamic-wind
 void
 (λ ()
   (for ([test (in-list (sort (directory-list (path-only me))
                              string<=?
                              #:key (λ (x) (format "~s" x))))])
     (when (file-exists? (build-path (path-only me) test))
       (when (regexp-match #rx"rkt$" (path->string test))
         (unless (equal? (file-name-from-path me)
                         test)
           (flush-output)
           (printf "============================================================\nrunning ~a\n"
                   test)
           (dynamic-require (build-path (path-only me) test) #f)
           (printf "finished ~a\n\n"
                   test)
           (set! ran-tests (cons test ran-tests)))))))
 (λ ()
   (printf "\nran: ~a\n" ran-tests)))

