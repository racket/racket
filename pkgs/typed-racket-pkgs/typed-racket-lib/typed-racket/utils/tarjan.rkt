#lang racket/base

;; This module provides an implementation of Tarjan's algorithm that
;; will find the strongly connected components in a graph.

(require data/queue
         racket/dict)

(provide tarjan
         make-vertex
         (struct-out vertex))

;; The graph for the algorithm is defined by a dictionary mapping
;; keys to vertices (see the contract on `tarjan` below). The vertices
;; encode edges by holding the keys to adjacent vertices.

;; A (Vertex K V) is a
;;   (vertex V Boolean (Option Integer) (Option Integer) (Listof K))
;;
;; interp. a vertex in a graph, we only use this for Tarjan's algorithm
;;   data     - data in the vertex, might be the key
;;   stack?   - whether this vertex is on the stack (for speed)
;;   index    - index tracked in Tarjan's algorithm
;;   lowlink  - see index
;;   adjacent - list of adjacent vertices by key
(struct vertex (data [stack? #:mutable] [index #:mutable]
                     [lowlink #:mutable] adjacent)
        #:transparent)

;; make-vertex : V (Listof K) -> (Vertex K V)
;; A more convenient constructor for vertices
(define (make-vertex key adjacent)
  (vertex key #f #f #f adjacent))

;; tarjan : (Dict K (Vertex K V)) -> (Listof (Listof (Vertex K V)))
;; Implements Tarjan's algorithm. See Wikipedia
;; http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
;;
;; Note that because this accumulates the result with `cons`, the components
;; are actually output in topological order rather than *reverse* topological order.
;; If you want reverse topological order, reverse the result.
(define (tarjan vertices)
  (define (strongly-connected vtx)
    (set-vertex-index! vtx index)
    (set-vertex-lowlink! vtx index)
    (set! index (add1 index))
    (enqueue-front! stack vtx)
    (set-vertex-stack?! vtx #t)
    (for ([successor-key (in-list (vertex-adjacent vtx))])
      (define successor (dict-ref vertices successor-key))
      (cond [(not (vertex-index successor))
             (strongly-connected successor)
             (set-vertex-lowlink! vtx
                                  (min (vertex-lowlink vtx)
                                       (vertex-lowlink successor)))]
            [(vertex-stack? successor)
             (set-vertex-lowlink! vtx
                                  (min (vertex-lowlink vtx)
                                       (vertex-index successor)))]))
    ;; sets a result component if this was a root vertex
    (when (= (vertex-lowlink vtx) (vertex-index vtx))
      (define new-scc
        (for/list ([elem (in-queue stack)]
                   #:final (equal? vtx elem))
          (dequeue! stack)
          (set-vertex-stack?! elem #f)
          elem))
      (set! sccs (cons new-scc sccs))))

  ;; the body
  (define index 0)
  (define stack (make-queue))
  (define sccs '())
  (for ([(key vtx) (in-dict vertices)]
        #:unless (vertex-index vtx))
    (strongly-connected vtx))
  sccs)
