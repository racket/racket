(require "dijkstra-solver.ss"
         (lib "match.ss"))

(define-mztake-process p
                       ("dijkstra.ss")
                       ("heap.ss" [inserts 49 6 bind 'item]
                                  [removes 67 10 bind 'result]))

(define (not-in-order e)
  (filter-e
   (match-lambda
     [('reset _) false]
     [(_ 'reset) false]
     [(previous current) (> previous current)]
     [else false])
   (history-e 2 e)))


(define inserts-and-removes-e (merge-e (removes . ==> . node-weight)
                                       (inserts . -=> . 'reset)))
(define violations (not-in-order inserts-and-removes-e))


(printf-b "all inserts and removes: ~a" (history-b inserts-and-removes-e))
(printf-b "all violations: ~a" (history-b violations))
(printf-b "latest-violation: ~a" (hold violations))


(define ((insert-in-model item) model) (cons item model))
(define ((remove-from-model item) model) (filter (lambda (i) (eq? i item)) model))

(define inserters (accum-e (inserts . ==> . insert-in-model) empty))
(define removers  (accum-e (removes . ==> . remove-from-model) empty))

(define model (merge-e inserters removers))

(printf-b "model: ~a" model)

(start/resume p)
