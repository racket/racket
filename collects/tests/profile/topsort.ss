#lang scheme/base

(require tests/eli-tester profile/structs profile/utils
         scheme/list scheme/match)

(define (connect! from to)
  (define edge (make-edge 0 from 0 to 0))
  (set-node-callers! to   (cons edge (node-callers to  )))
  (set-node-callees! from (cons edge (node-callees from))))

(define (sort-graph . edges)
  (define names (remove-duplicates (remq* '(->) (append* edges))))
  (define nodes (map (lambda (sym) (make-node sym #f '() 0 0 '() '())) names))
  (define ->node (make-immutable-hasheq (map cons names nodes)))
  (for ([edges edges])
    (let loop ([xs edges])
      (match xs
        [(list from '-> to '-> _ ...)
         (connect! (hash-ref ->node from) (hash-ref ->node to))
         (loop (cddr xs))]
        [(list from '-> to _ ...)
         (connect! (hash-ref ->node from) (hash-ref ->node to))
         (loop (cdddr xs))]
        ['() (void)])))
  (map (lambda (nodes) (map node-id nodes)) (topological-sort (car nodes))))

(define (same-levels graph levels)
  (define sorted (sort-graph graph))
  (define (set=? l1 l2) (null? (append (remq* l1 l2) (remq* l2 l1))))
  (andmap set=? sorted levels))

(provide topological-sort-tests)
(define (topological-sort-tests)
  (test

    (same-levels '(* -> A -> B)
                 '((A) (B)))

    (same-levels '(* -> A -> B -> *
                   * -> B -> A -> *)
                 '((A B)))

    (same-levels '(* -> A -> B -> C
                   * -> C)
                 '((A) (B) (C)))

    (same-levels '(* -> A
                   * -> B
                   B -> B)
                 '((A B)))

    (same-levels '(* -> A
                   * -> B -> C
                   * -> C -> B)
                 '((A B C)))

    ))
