(module dijkstra mzscheme
  (require "dijkstra-solver.ss"
           "graph.ss"
           (lib "list.ss"))
  (print-struct #t)
  (define g (make-graph 'directed))
  (define (m-node label x y) (make-node label x y +inf.0))
  (define nodes
    (list
     (m-node 'J 200 100)
     (m-node 's 100 125)
     (m-node '1 150 100)
     (m-node '2 150 150)
     (m-node '4 250 100)
     (m-node '5 300 100)
     (m-node '6 300 150)))
  (for-each (lambda (n) (graph-node-add! g n)) nodes)
  (define (n-ref label) 
    (first (filter (lambda (n) (eq? label (node-label n))) nodes)))
  
  (define edges
    (list  (list (n-ref 's) (n-ref '1))
           (list (n-ref 's) (n-ref '2))
           (list (n-ref '1) (n-ref 'J))
           (list (n-ref '4) (n-ref '5))
           (list (n-ref 'J) (n-ref '4))
           (list (n-ref 'J) (n-ref '6))))
  (for-each (lambda (e) (graph-edge-add! g (first e) (second e)))
            edges)
  
  (printf "~n~n---output from dijkstra.ss:~n~a~n---~n"
          (solve g (reverse nodes) (n-ref 's))))