#lang racket/base
(provide top-sort)
;; top-sort : (listof α) (α -> (listof α)) -> (listof α) or #f
;; returns #f if there is a cycle in the graph
;; (α needs hashing)
(define (top-sort elements neighbors fail)
  (define parents (make-hash))
  (define children (make-hash))
  (define ids (make-hash))

  (for ([element (in-list elements)]
        [n (in-naturals)])
    (hash-set! ids element n))
  
  (define (add-link table from to)
    (hash-set! (hash-ref table from) to #t))
  
  ;; initialize the tables telling me about parents and children
  (for ([ele (in-list elements)])
    (hash-set! parents ele (make-hash))
    (hash-set! children ele (make-hash)))
  
  (for ([parent (in-list elements)])
    (for ([child (in-list (neighbors parent))])
      (add-link parents parent child)
      (add-link children child parent)))

  ;; contains elements that have no (unscheduled) dependencies
  (define pending (make-hash))
  (for ([(k v) (in-hash parents)])
    (when (zero? (hash-count v))
      (hash-set! pending k #t)))
  
  (define sorted
    (let loop ()
      (cond
        [(zero? (hash-count pending))
         '()]
        [else
         (define best #f)
         (for ([(ele _) (in-hash pending)])
           (cond
             [best 
              (when (< (hash-ref ids ele) (hash-ref ids best))
                (set! best ele))]
             [else
              (set! best ele)]))
         (hash-remove! pending best)
         (for ([(child _) (in-hash (hash-ref children best))])
           (define childs-parents (hash-ref parents child))
           (hash-remove! childs-parents best)
           (when (zero? (hash-count childs-parents))
             (hash-set! pending child #t)))
         (cons best (loop))])))
  
  (cond
    [(= (length sorted) (length elements)) sorted]
    [else (fail (remove* sorted elements))]))

