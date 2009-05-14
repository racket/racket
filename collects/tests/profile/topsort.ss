#lang scheme/base

(require tests/eli-tester profile/structs profile/utils)

(define (connect! from to)
  (define e (make-edge 0 from 0 to 0))
  (set-node-callers! to   (cons e (node-callers to  )))
  (set-node-callees! from (cons e (node-callees from))))

(define-syntax with-graph
  (syntax-rules (->)
    [(_ [] <from> -> <to> -> more ...)
     (begin (connect! <from> <to>) (with-graph [] <to> -> more ...))]
    [(_ [] <from> -> <to> more ...)
     (begin (connect! <from> <to>) (with-graph [] more ...))]
    [(_ [] more ...) (begin more ...)]
    [(_ [<id> ...] more ...)
     (let ([<id> (make-node '<id> #f '() 0 0 '() '())] ...)
       (with-graph [] more ...))]))

(provide topological-sort-tests)
(define (topological-sort-tests)
  (test

    do (with-graph [A B C]
         A -> B -> C
         (test (topological-sort A values) => (list A B C)))

    do (with-graph [A B C]
         ;; check that a cycle doesn't lead to dropping nodes
         A -> B -> C -> A
         A -> C -> B -> A
         (null? (remq* (topological-sort A values) (list A B C))))

    do (with-graph [A B C D]
         A -> B -> C -> D
         A -> D
         (test (topological-sort A values) => (list A B C D)))

    do (with-graph [A B C]
         A -> B
         A -> C
         C -> C
         (test (memq C (topological-sort A))))

    do (with-graph [A B C D]
         A -> B
         A -> C -> D
         A -> D -> C
         (test (memq C (topological-sort A))))

    ))
