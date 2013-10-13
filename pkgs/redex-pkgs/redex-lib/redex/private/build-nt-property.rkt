#lang racket/base

(require racket/contract
         "lang-struct.rkt")

(provide
 (contract-out
  [build-nt-property (-> (listof nt?)
                         (-> any/c (hash/c symbol? any/c) any/c)
                         any/c
                         (-> any/c any/c any/c)
                         (hash/c symbol? any/c))]
  [build-nt-property/name (-> (listof nt?)
                              (-> any/c symbol? (hash/c symbol? any/c) any/c)
                              any/c
                              (-> any/c any/c any/c)
                              (hash/c symbol? any/c))]))

;; build-nt-property : lang 
;;                     (pattern hash[symbol -o> ans] -> ans)
;;                     init-ans
;;                     (ans ans -> ans) 
;;                  -> hash[nt -o> ans]
;; builds a property table using a fixed point computation,
;; using base-answer and lub as the lattice
(define (build-nt-property lang test-rhs base-answer lub)
  (build-nt-property/name lang
                          (λ (pat n ht)
                             (test-rhs pat ht))
                          base-answer
                          lub))

;; build-nt-property : lang 
;;                     (pattern symbol hash[symbol -o> ans] -> ans)
;;                     init-ans
;;                     (ans ans -> ans) 
;;                  -> hash[nt -o> ans]
(define (build-nt-property/name lang test-rhs/name base-answer lub)
  (define ht (make-hash))
  (for ([nt (in-list lang)])
    (hash-set! ht (nt-name nt) base-answer))
  (let loop ()
    (define something-changed? #f)
    (for ([nt (in-list lang)])
      (define next-val
        (for/fold ([acc base-answer])
                  ([rhs (in-list (nt-rhs nt))])
          (lub acc (test-rhs/name (rhs-pattern rhs) (nt-name nt) ht))))
      (unless (equal? next-val (hash-ref ht (nt-name nt)))
        (hash-set! ht (nt-name nt) next-val)
        (set! something-changed? #t)))
    (when something-changed? (loop)))
  ht)
