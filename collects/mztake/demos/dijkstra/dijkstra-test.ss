
(require "dijkstra-solver.ss"
         (lib "match.ss"))

(mztake-process p 
                ("dijkstra.ss")
                ("heap.ss"
                   [inserts 49 6 bind 'item]
                   [removes 67 10 bind 'result]))

(define (not-in-order e)
  (filter-e
   (match-lambda
     [('reset _) false]
     [(_ 'reset) false]
     [(previous current) (> previous current)])
   (history-e 2 e)))

(history-e 5 (history-e 2 (merge-e (removes . ==> . node-weight)
                                    (inserts . -=> . 'reset))))

(define violations
  (not-in-order (merge-e (removes . ==> . node-weight)
                         (inserts . -=> . 'reset))))

(define latest-violation (hold violations))

(define ((insert-in-model item) model) (cons item model))
(define ((remove-from-model item) model) (filter (lambda (i) (eq? i item)) model))

(define inserters (inserts . ==> . insert-in-model))
(define removers (removes . ==> . remove-from-model))

(define model (accum-b (merge-e inserters removers) empty))

(printf-b "latest-violation: ~a" latest-violation)
(printf-b "model: ~a" model)

(start/resume p)
  