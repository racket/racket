#lang racket/base

;; This module provides helper functions for type aliases

(require "../utils/utils.rkt"
         (utils tc-utils)
	 data/queue
         racket/dict
         racket/match
         syntax/id-table)

(provide find-strongly-connected-type-aliases)

(module+ test (require rackunit))

;; A Vertex is a
;;   (vertex Identifier Boolean Option<Integer> Option<Integer> Listof<Id>)
;;
;; interp. a vertex in a graph, we only use this for Tarjan's algorithm
(struct vertex (id stack? index lowlink adjacent)
        #:mutable #:transparent)

;; Dict<Id, (List Type Listof<Id>)> -> Listof<Listof<Id>>
;; Find strongly connected type aliases in order to
;; find mutually recursive aliases
;;
;; Returns the components in topologically sorted order
(define (find-strongly-connected-type-aliases dep-map)
  (define vertex-map
    (make-free-id-table
     (for/hash ([(id adjacent) (in-dict dep-map)])
       (values id (vertex id #f #f #f adjacent)))))
  ;; Implements Tarjan's algorithm. See Wikipedia
  ;; http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
  (define (tarjan vertices)
    (define (strongly-connected vtx)
      (set-vertex-index! vtx index)
      (set-vertex-lowlink! vtx index)
      (set! index (add1 index))
      (enqueue-front! stack vtx)
      (set-vertex-stack?! vtx #t)
      (for ([successor-id (vertex-adjacent vtx)])
        (define successor (dict-ref vertices successor-id))
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
          (for/list ([elem stack]
                     #:final (equal? vtx elem))
            (dequeue! stack)
            (set-vertex-stack?! vtx #f)
            (vertex-id elem)))
        (set! sccs (cons new-scc sccs))))

    ;; the body
    (define index 0)
    (define stack (make-queue))
    (define sccs '())
    (for ([(id vtx) (in-dict vertices)]
          #:unless (vertex-index vtx))
      (strongly-connected vtx))
    sccs)
  (tarjan vertex-map))

(module+ test
  ;; two aliases in their own components
  (define example-1
    (list (cons #'x (list #'x))
          (cons #'y (list #'y))))
  ;; all one component
  (define example-2
    (list (cons #'x (list #'x #'y))
          (cons #'y (list #'x))))
  ;; two components, one with two nodes
  (define example-3
    (list (cons #'x (list #'y))
          (cons #'y (list #'x))
          (cons #'z (list))))
  ;; one with cycles, two that form a line
  (define example-4
    (list (cons #'x (list #'y))
          (cons #'y (list #'x))
          (cons #'a (list #'b))
          (cons #'b (list))))
  ;; two large cycles
  (define example-5
    (list (cons #'x (list #'y #'z))
          (cons #'y (list #'x))
          (cons #'z (list #'x #'y))
          (cons #'a (list #'b))
          (cons #'b (list #'c))
          (cons #'c (list #'a))))
  ;; check topological order
  (define example-6
    (list (cons #'a (list #'b))
          (cons #'d (list))
          (cons #'c (list #'d #'e))
          (cons #'b (list #'c))
          (cons #'e (list #'f))
          (cons #'f (list))))

  (define (equal?/id x y)
    (if (and (identifier? x) (identifier? y))
        (free-identifier=? x y)
        (equal?/recur x y equal?/id)))

  (define-binary-check (check-equal?/id equal?/id actual expected))

  (check-equal?/id (find-strongly-connected-type-aliases example-1)
                   (list (list #'x) (list #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-2)
                   (list (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-3)
                   (list (list #'z) (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-4)
                   (list (list #'a) (list #'b) (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-5)
                   (list (list #'b #'a #'c) (list #'z #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-6)
                   (list (list #'a) (list #'b) (list #'c)
                         (list #'e) (list #'f) (list #'d))))

