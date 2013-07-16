#lang racket/base

(require tests/eli-tester profile/structs profile/utils
         racket/list racket/match)

(define arrow-sym->times
  ;; arrows with caller/callee times
  ;; `->' defaults to `1->1', and `N->' defaults to `N->N'
  ;; note: A 1->2 B means that the caller time is 2 and the callee time is one,
  ;; since the time are wrt the other node.
  (let ([t (make-hasheq)])
    (lambda (arr)
      (hash-ref! t arr
        (lambda ()
          (define m
            (regexp-match #rx"^(?:([0-9]+)|)->(?:([0-9]+)|)$"
                          (symbol->string arr)))
          (and m
               (list (string->number (or (caddr m) (cadr m) "1"))
                     (string->number (or (cadr m) "1")))))))))

(define (connect! from from-time to to-time)
  (if (memq from (map edge-caller (node-callers to)))
    (error (format "bad graph spec in tests, ~s->~s already connected" from to))
    (let ([edge (make-edge 0 from from-time to to-time)])
      (set-node-callers! to   (cons edge (node-callers to  )))
      (set-node-callees! from (cons edge (node-callees from))))))

(define-match-expander arrow
  (syntax-rules () [(_ ler lee) (app arrow-sym->times (list ler lee))]))

(define (set=? l1 l2) (null? (append (remq* l1 l2) (remq* l2 l1))))

(define (sort-graph . edges)
  (define names
    (remove-duplicates (filter (lambda (s) (not (arrow-sym->times s)))
                               (append* edges))))
  (define nodes (map (lambda (sym) (make-node sym #f '() 0 0 '() '())) names))
  (define ->node (make-immutable-hasheq (map cons names nodes)))
  (for ([edges edges])
    (let loop ([xs edges])
      (match xs
        [(list from (arrow ftime ttime) to (arrow _ _) _ ...)
         (connect! (hash-ref ->node from) ftime (hash-ref ->node to) ttime)
         (loop (cddr xs))]
        [(list from (arrow ftime ttime) to _ ...)
         (connect! (hash-ref ->node from) ftime (hash-ref ->node to) ttime)
         (loop (cdddr xs))]
        ['() (void)])))
  (let ([sorted (topological-sort (car nodes))])
    (unless (set=? nodes (cons (car nodes) (append* sorted)))
      (error 'sort-graph
             "not all nodes appear in sorted output; expected ~e, got ~e"
             nodes (cons (car nodes) (append* sorted))))
    (map (lambda (nodes) (map node-id nodes)) sorted)))

;; `expected' is the desired result, modulo ordering inside the levels
;; can have more than one if there are several valid outputs
(define (same-levels graph expected . more-valid)
  (define sorted (sort-graph graph))
  (or (ormap (lambda (expected)
               (and (= (length sorted) (length expected))
                    (andmap set=? sorted expected)))
             (cons expected more-valid))
      (error (format "bad result, got ~s" sorted))))

;; to see a result: (sort-graph '(* -> A)) (exit)

(provide topological-sort-tests)
(module+ main (topological-sort-tests))
(define (topological-sort-tests)
  (test

   ;; sanity check: works with an empty stack
   ;; actually, this version does get stuck in a loop -- which shouldn't happen
   ;; in reality since there are no *->* edges
   ;; (same-levels '(* -> *)
   ;;              '())
   ;; instead, just have a single * node:
   (topological-sort (make-node '* #f '() 0 0 '() '()))
   => '()

   (same-levels '(* -> A)
                '((A)))

   (same-levels '(* -> A -> B -> *)
                '((A) (B)))

   (same-levels '(* -> A -> *
                  * -> B -> *
                  A -> A)
                '((A B)))

   ;; note that ((B) (A)) would also be consistent, but the code organizes for
   ;; this result
   (same-levels '(* -> A -> B -> A)
                '((A) (B)))

   ;; this is an example using the actual times for the A->B->A case that is
   ;; tested in main.rkt
   (same-levels '(* 2->1 A 1->2 B 2->1 A 1->2 *)
                '((A) (B)))


   (same-levels '(* -> A 2-> B -> A)
                '((A) (B)))

   (same-levels '(* -> A 2-> B 3-> A)
                '((B) (A)))

   (same-levels '(* -> A -> B -> C
                  * -> C)
                '((A) (B) (C)))

   (same-levels '(* -> A -> B -> C -> * ; check with *s too
                  * -> C)
                '((A) (B) (C)))

   (same-levels '(* -> X -> A -> B -> C
                       X -> C )
                '((X) (A) (B) (C)))

   (same-levels '(* -> A  -> B -> C -> *
                  * -> C 3-> B -> *)
                '((A C) (B)))

   (same-levels '(* -> A -> B -> A
                  * -> C)
                '((A C) (B)))

   (same-levels '(* -> A -> B -> *
                  * -> B -> A -> *)
                '((A) (B))
                '((B) (A)))

   (same-levels '(* -> A
                  * -> B
                  B -> B)
                '((A B)))

   (same-levels '(* -> A
                  * -> B -> C
                  * -> C -> B)
                '((A B) (C))
                '((A C) (B)))

   ;; Note that X could be pushed to the third level, making it closer to D,
   ;; but this is not done since for a printout it makes more sense to have all
   ;; the sources at the top.  (It might make sense to do this compacting in
   ;; the gui, but even there it might be more convenient to see all roots at
   ;; the top.)
   (same-levels '(* -> A -> B -> C -> D -> *
                  * ->           X -> D)
                '((X A) (B) (C) (D)))

   (same-levels '(* -> A1 -> A2 -> A3 -> A4 -> A5
                  * -> B1 -> B2 -> B3
                  * -> C1
                  * -> D1 -> D2 -> D3 -> D4)
                '((A1 B1 C1 D1) (A2 B2 D2) (A3 B3 D3) (A4 D4) (A5)))

    ))
