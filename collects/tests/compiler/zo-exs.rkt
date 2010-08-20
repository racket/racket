#lang racket
(require compiler/zo-parse
         compiler/zo-marshal
         tests/eli-tester)

(define (read-compiled-bytes bs)
  (parameterize ([read-accept-compiled #t])
    (read (open-input-bytes bs))))

(define (roundtrip ct)
  (define bs (zo-marshal ct))
  (with-output-to-file "test_rkt.zo" (λ () (write-bytes bs)) #:exists 'replace)
  (test #:failure-prefix (format "~S" ct)
        (test bs
              (zo-parse (open-input-bytes bs)) => ct
              (read-compiled-bytes bs))))

(define mpi (module-path-index-join #f #f))

(test
 #;(roundtrip 
  (compilation-top 0 
                   (prefix 0 empty empty)
                   (current-directory)))
 
 #;(roundtrip 
  (compilation-top 0 
                   (prefix 0 empty empty)
                   (list (current-directory))))
 
 (roundtrip
  (compilation-top                                            
      0                                                          
      (prefix 0 empty empty)
      (cons #hasheq()
            #hasheq())))
 
 #;(local [(define (hash-test make-hash-placeholder)
           (roundtrip 
            (compilation-top 0 
                             (prefix 0 empty empty)
                             (local [(define ht-ph (make-placeholder #f))
                                     (define ht (make-hash-placeholder (list (cons 'g ht-ph))))]
                               (placeholder-set! ht-ph ht)
                               (make-reader-graph ht)))))]
   (hash-test make-hash-placeholder)
   (hash-test make-hasheq-placeholder)
   (hash-test make-hasheqv-placeholder)))


