(require (as-is mzscheme load)
         (as-is "test-harness.ss" test))
(load "../demos/dijkstra/dijkstra-mztake.ss") 
(map-e (lambda (e)
         (unless e
           (test (dv:vector-length (t-data heap)) 5)))
       (debug-process-running-e (current-process)))
