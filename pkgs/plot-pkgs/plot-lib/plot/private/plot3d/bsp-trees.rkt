#lang typed/racket/base

(require racket/match
         racket/list
         racket/flonum
         "bsp.rkt")

(provide (all-defined-out))

(: all-bsp-trees (HashTable Symbol (HashTable Integer BSP-Tree)))
(define all-bsp-trees (make-weak-hasheq))

(: build-bsp-trees (-> (HashTable Integer (Listof BSP-Shape)) Symbol))
(define (build-bsp-trees structural-shapes)
  (define key (gensym))
  (define bsp-trees
    (for/hasheq : (HashTable Integer BSP-Tree) ([(layer ss)  (in-hash structural-shapes)])
      (values layer (build-bsp-tree ss))))
  (hash-set! all-bsp-trees key bsp-trees)
  key)

(: walk-bsp-trees (-> Symbol FlVector (HashTable Integer (Listof BSP-Shape))
                      (HashTable Integer (Listof BSP-Shape))))
(define (walk-bsp-trees key view-dir detail-shapes)
  (define bsp-trees (hash-ref all-bsp-trees key
                              (λ () ((inst make-immutable-hasheq Integer BSP-Tree)))))
  (define vx (flvector-ref view-dir 0))
  (define vy (flvector-ref view-dir 1))
  (define vz (flvector-ref view-dir 2))
  
  (define layers (sort (append (hash-keys bsp-trees) (hash-keys detail-shapes)) >))
  (for/hasheq : (HashTable Integer (Listof BSP-Shape)) ([layer  (in-list layers)])
    (define bsp (hash-ref bsp-trees layer (λ () (bsp-leaf empty))))
    (define ss (hash-ref detail-shapes layer (λ () empty)))
    
    (: in-order-ss (Listof BSP-Shape))
    (define in-order-ss
      (let loop ([bsp  (bsp-tree-insert bsp ss)])
        (match bsp
          [(bsp-leaf ss)  ss]
          [(bsp-node plane neg pos)
           (define a (flvector-ref plane 0))
           (define b (flvector-ref plane 1))
           (define c (flvector-ref plane 2))
           (define cos-angle (+ (* a vx) (* b vy) (* c vz)))
           (if (cos-angle . > . -1e-16)
               (append (loop neg) (loop pos))
               (append (loop pos) (loop neg)))])))
    
    (values layer in-order-ss)))
