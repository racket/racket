#lang racket/base
(require racket/contract)

(define-struct cache-table (hash semaphore) #:mutable)

(define (new-cache-table)
  ; Only eq? tables are not locked
  (make-cache-table (make-hasheq)
                    (make-semaphore 1)))

(define (cache-table-clear! ct)
  (call-with-semaphore
   (cache-table-semaphore ct)
   (lambda ()
     (set-cache-table-hash! ct (make-hasheq)))))

(define (cache-table-lookup! ct entry-id entry-thunk)
  (define ht (cache-table-hash ct))
  (define sema (cache-table-semaphore ct))
  ; Fast lookup
  (hash-ref
   ht entry-id
   (lambda ()
     ; Now lock for relookup and computation
     (call-with-semaphore
      sema
      (lambda ()
        (hash-ref! ht entry-id entry-thunk))))))

(provide/contract
 [rename new-cache-table make-cache-table
         (-> cache-table?)]
 [cache-table-lookup! (cache-table? symbol? (-> any/c) . -> . any/c)]
 [cache-table-clear! (cache-table? . -> . void?)]
 [cache-table? (any/c . -> . boolean?)])
