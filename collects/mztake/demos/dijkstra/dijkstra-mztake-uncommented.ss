(require (lib "mztake.ss" "mztake")
	 "dijkstra-solver.ss"
         (lib "match.ss"))

(define/bind (loc "heap.ss" 49 6) item)
(define/bind (loc "heap.ss" 67 10) result)

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


#| Implementation of the local model follows... |#
(define ((insert-in-model item) model)
  (printf "~nInserting ~a into model containing:~n~a~n" item (value-now model))
  (cons item model))

(define ((remove-from-model item) model)
  (printf "~nRemoving ~a from model containing:~n~a~n" item (value-now model))
  (filter (lambda (i) (not (equal? i item))) model))

(define inserters (accum-b (inserts . ==> . insert-in-model) empty))
(define removers  (accum-b (removes . ==> . remove-from-model) inserters))

(set-running-e! (violations . -=> . false))