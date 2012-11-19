#lang racket/base
(require racket/file
         "untar.rkt"
         "gunzip.rkt"
         racket/contract/base)

(provide
 (contract-out
  [untgz (->* ((or/c path-string? input-port?))
              (#:dest 
               (or/c #f path-string?)
               #:strip-count exact-nonnegative-integer?
               #:filter (path? (or/c path? #f)
                               symbol? exact-integer? (or/c path? #f)
                               exact-nonnegative-integer? exact-nonnegative-integer?
                               . -> . any/c))
              void?)]))

(define (untgz in 
               #:dest [dest #f]
               #:strip-count [strip-count 0]
               #:filter [filter void])
  ((if (input-port? in)
       (lambda (in f) (f in))
       call-with-input-file*)
   in
   (lambda (in)
     (define in2
       (cond
        [(and (= (peek-byte in 0) #o037) 
              (= (peek-byte in 1) #o213))
         (define-values (in2 out) (make-pipe 4096))
         (thread
          (lambda ()
            (dynamic-wind
                (lambda () (void))
                (lambda () (gunzip-through-ports in out))
                (lambda () (close-output-port out)))))
         in2]
        [else in]))
     (untar in2 #:dest dest #:strip-count strip-count #:filter filter))))

      
       
  
