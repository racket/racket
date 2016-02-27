#lang racket/base
(require racket/file
         "untar.rkt"
         "gunzip.rkt"
         racket/contract/base
         racket/port)

(provide
 (contract-out
  [untgz (->* ((or/c path-string? input-port?))
              (#:dest 
               (or/c #f path-string?)
               #:strip-count exact-nonnegative-integer?
               #:permissive? any/c
               #:filter (path? (or/c path? #f)
                               symbol? exact-integer? (or/c path? #f)
                               exact-nonnegative-integer? exact-nonnegative-integer?
                               . -> . any/c))
              void?)]))

(define (untgz in 
               #:dest [dest #f]
               #:strip-count [strip-count 0]
               #:permissive? [permissive? #f]
               #:filter [filter void])
  ((if (input-port? in)
       (lambda (in f) (f in))
       call-with-input-file*)
   in
   (lambda (in)
     (define-values (in2 wait)
       (cond
        [(and (= (peek-byte in 0) #o037) 
              (= (peek-byte in 1) #o213))
         (define-values (in2 out) (make-pipe 4096))
         (define t
           (thread
            (lambda ()
              (dynamic-wind
                  (lambda () (void))
                  (lambda () (gunzip-through-ports in out))
                  (lambda () (close-output-port out))))))
         (values in2 (lambda ()
                       ;; drain any remaining bytes:
                       (copy-port in2 (open-output-nowhere))
                       (thread-wait t)))]
        [else (values in void)]))
     (begin0
      (untar in2 #:dest dest #:strip-count strip-count #:permissive? permissive? #:filter filter)
      (wait)))))

      
       
  
