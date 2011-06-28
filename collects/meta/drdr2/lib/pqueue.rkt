#lang racket/base
(require mzlib/os
         racket/contract
         racket/file
         racket/match)

(struct pqueue (dir))

(define (pqueue-tmp dir) (build-path dir "tmp"))
(define (pqueue-dest dir) (build-path dir "queue"))

(define pqueue-init!
  (match-lambda
    [(pqueue dir)
     (make-directory* (pqueue-tmp dir))
     (make-directory* (pqueue-dest dir))]))

(define (pqueue-enqueue! pq v)
  (match-define (pqueue dir) pq)
  (define uniq
    (format "~a.~a"
            (current-inexact-milliseconds)
            (getpid)))
  (define tmp (build-path (pqueue-tmp dir) uniq))
  (define dest (build-path (pqueue-dest dir) uniq))
  
  (with-output-to-file tmp
    (λ () (write v)))
  
  (rename-file-or-directory tmp dest))

(define current-pqueue-wait-seconds (make-parameter 10))

(define (pqueue-dequeue! pq)
  (match-define (pqueue dir) pq)
  (match (directory-list (pqueue-dest dir))
    [(list-rest choice _)
     (define dest 
       (build-path (pqueue-dest dir) choice))
     (define tmp
       (build-path (pqueue-tmp dir) choice))
     
     (define succeeded?
       (with-handlers ([exn? (λ (x) #f)])
         (rename-file-or-directory dest tmp)
         #t))
     (if (not succeeded?)
         (pqueue-dequeue! pq)
         (dynamic-wind 
          void
          (λ ()
            (with-input-from-file tmp read))
          (λ ()
            (delete-file tmp))))]
    [_
     (sleep (current-pqueue-wait-seconds))
     (pqueue-dequeue! pq)]))

(provide/contract
 [current-pqueue-wait-seconds (parameter/c exact-nonnegative-integer?)]
 [struct pqueue ([dir path-string?])]
 [pqueue-init! (pqueue? . -> . void)]
 [pqueue-enqueue! (pqueue? any/c . -> . void)]
 [pqueue-dequeue! (pqueue? . -> . any/c)])
