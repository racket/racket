(module dijkstra-solver mzscheme
  (require (lib "heap.ss" "frtime")
           (lib "list.ss")
           "graph.ss")

  (provide (all-defined))
  
  (define (make-node label x y weight) (vector label x y weight))
  (define (node-label n) (vector-ref n 0))
  (define (node-x n) (vector-ref n 1))
  (define (node-y n) (vector-ref n 2))
  (define (node-weight n) (vector-ref n 3))
  (define (set-node-weight! n v) (vector-set! n 3 v))
  
  (define (node< a b) (< (node-weight a) (node-weight b)))
  (define (sqr x) (* x x))
  (define (distance-to a b)
    (sqrt (+ (sqr (- (node-x a) (node-x b)))
             (sqr (- (node-y a) (node-y b))))))
  
  (define (hash-table-pairs hash)
    (hash-table-map hash (lambda (key val) (list key val))))
  
  (define (relax backtrace heap origin dest)
    (let ([candidate-weight
           (+ (node-weight origin)
              (distance-to origin dest))])
      (when (candidate-weight . < . (node-weight dest))
        (set-node-weight! dest candidate-weight)
        ;;(heap-resort heap dest)
        (hash-table-put! backtrace dest origin))))
  
  (define (solve graph nodes source)
    (let ([backtrace (make-hash-table)]
          [heap (make-heap node< eq?)])
      (set-node-weight! source 0)
      (for-each (lambda (node) (heap-insert heap node))
                nodes)
      
      (let loop ()
        (unless (heap-empty? heap)
          (let* ([node (heap-pop heap)]
                 [successors (graph-succs graph node)])
            (for-each 
             (lambda (succ) (relax backtrace heap node succ))
             successors))
          (loop)))
      
      (hash-table-pairs backtrace))))
