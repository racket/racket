#lang racket/base
(require racket/unit
         racket/async-channel
         drracket/private/module-browser
         drracket/private/standalone-module-browser
         rackunit)

(define (fetch-files stx/fn)
  (define progress-channel (make-async-channel))
  (define connection-channel (make-async-channel))
  (define-values/invoke-unit process-program-unit 
    (import process-program-import^)
    (export process-program-export^))
  
  (thread
   (λ () 
     (add-connections stx/fn)
     (async-channel-put connection-channel #f)))
  
  (let loop ()
    (define next (sync connection-channel))
    (cond
      [next (cons next (loop))]
      [else '()])))


(define r/b
  (fetch-files 
   (collection-file-path "module-browser-test1.rkt" "tests" "drracket")))
(check-not-false (member (collection-file-path "base.rkt" "racket")
                         (map car r/b)))
(check-false (member (collection-file-path "list.rkt" "racket")
                     (map car r/b)))

(define r/b+submod
  (fetch-files
   (collection-file-path "module-browser-test2.rkt" "tests" "drracket")))
(check-not-false (member (collection-file-path "base.rkt" "racket")
                         (map car r/b+submod)))
(check-not-false (member (collection-file-path "list.rkt" "racket")
                         (map car r/b+submod)))
