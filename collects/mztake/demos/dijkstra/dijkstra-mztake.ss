#| This script tests a priority queue (heap) that is correctly implemented, but incorrectly used.
   It is not commented because it uses a some advanced FrTime concepts that can easily be looked
   up in the help desk, and both the description and motivation of the example can be found in
   "A Dataflow Language for Scriptable Debugging" (Marceau, Cooper, Krishnamurthi, Reiss),
   available at:

        http://www.cs.brown.edu/~sk/Publications/Papers/Published/mckr-dataflow-lang-script-debug/

   This script uses the concept of maintaining a local model of the heap being debugged, as a simple,
   and very slow, list. The difference is that a fancy heap used can be naively implemented as a list,
   simply removing only the smallest element each time. Models are external to your program, you don't
   have to add any test code to your program to use them. By adding and removing the items to our local
   "model" (the values come from the heap code we used),  we can compare the results and assert whether
   it is working correctly or not. Our model shows the values we should be getting from the program,
   but clearly are not.

   To provide some context for this demo, and what debugging problem MzTake helps us explore, I offer
   the following, out of context, taken directly from the paper:
      We find that the queue's elements are not in sorted order while those in the model
      are. More revealingly, the queue's elements are not the same as those in the model.
      A little further study shows that the bug is in our usage of the priority queue:
      we have failed to account for the fact that the assignment to dest.weight
      in relax (figure 1) updates the weights of nodes already in the queue. Because
      the queue is not sensitive to these updates, what it returns is no longer the
      smallest element in the queue.
   
      On further reading, we trace the error to a subtle detail in the description of
      Dijkstra's algorithm in Cormen, et al.'s book [9, page 530]. The book permits
      the use of a binary heap (which is how we implemented the priority queue) for
      sparse graphs, but subsequently amends the pseudocode to say that the assignment
      to dest.weight must explicitly invoke a key-decrement operation. Our error,
      therefore, was not in the implementation of the heap, but in using the (faster)
      binary heap implementation without satisfying its (stronger) contract. |#

(require (lib "mztake.ss" "mztake")
         (lib "useful-code.ss" "mztake")
	 "dijkstra-solver.ss"
         (lib "match.ss"))

(define inserts (trace (loc '(lib "heap.ss" "frtime") '(let* ((sorter _) _) _))
                       item))
(define removes (trace (loc/r '(dv:ref (t-data _) _))))

#| The following code
merely observes the insertions and removals
   from the heap. We notice whether any of the removals are out
   of order based on the last item removed, as long as there are
   no insertions between the two events. We can keep track of the
   last 2 using history-e. |#

(define (not-in-order e)
  (filter-e
   (match-lambda
     [('reset _) false]
     [(_ 'reset) false]
     [(previous current) (> previous current)]
     [else false])
   (history-e e 2)))


(define inserts-and-removes-e (merge-e (removes . ==> . node-weight)
                                       (inserts . -=> . 'reset)))
(define violations (not-in-order inserts-and-removes-e))

(printf-b "all inserts and removes: ~a" (history-b inserts-and-removes-e))
(printf-b "all violations: ~a" (history-b violations))
(printf-b "latest-violation: ~a" (hold violations))
#| This output indicates that the queue has yielded nodes whose weights are out of order.
   This confirms our suspicion that the problem somehow involves the priority queue. |#

#| Implementation of the local model follows... |#
(define ((insert-in-model item) model)
  (printf "~nInserting ~a into model containing:~n~a~n" item (value-now model))
  (cons item model))

(define ((remove-from-model item) model)
  (printf "~nRemoving ~a from model containing:~n~a~n" item (value-now model))
  (filter (lambda (i) (not (equal? i item))) model))

(define inserters (accum-b (inserts . ==> . insert-in-model) empty))
(define removers  (accum-b (removes . ==> . remove-from-model) inserters))

(set-main! "dijkstra.ss")

(set-running-e! (violations . -=> . false))