;; -*- compile-command: "mzscheme -M errortrace -u graph.ss" -*-
(module graph mzscheme
  (require (lib "base-gm.ss" "frtime")
           (lib "etc.ss")
           (lib "list.ss"))
  
  (provide make-graph
           ;; --- Constructors :
           graph?
           graph-directed?
           graph-make-similar
           graph-copy
           graph-add-all!
           ;; --- Functions on nodes:
           graph-nodes
           graph-nodes-size
           graph-make-node!
           graph-node-add!
           graph-node-mem?
           graph-node-set!
           graph-node-remove!
           graph-node-collapse!
           graph-node-has-label?
           graph-node-label
           graph-for-each-node
           graph-fold-nodes
           ;; --- Functions on neighbors:
           graph-succs
           graph-preds
           graph-adjs
           graph-for-each-adjs
           ;; --- Functions on edges:
           graph-edges
           graph-edges-size
           graph-edge-add!
           graph-edge-mem?
           graph-edge-set!
           graph-edge-remove!
           graph-edge-has-label?
           graph-edge-label
           graph-for-each-edge
           graph-fold-edges
           ;; --- Simple graph algorithms:
           graph-dfs-from-node
           graph-dfs-all
           graph-components
           graph-strongly-connected-components
           graph-topological-sort
           ;; --- Debugging:
           graph-to-list
           graph-to-string
           graph-test
           )
  
  (define-struct t (flags nNodes nEdges nodes successors predessessors))
  
  ;; Flags can be: 'equal 'directed 'unique-node 'unique-edge 'nodes-must-exists 'safe
  ;; 'safe is a short for '(unique-node unique-edge nodes-must-exists)
  (define (make-graph . flags)
    (let ((flag-hash (make-hash)))
      (set! flags (expands-safe-flag flags))
      (for-each (lambda (flag) (hash-put! flag-hash flag true)) flags)
      (if (member 'equal flags)
          (make-t flag-hash 0 0 (make-hash 'equal) (make-hash 'equal) (make-hash 'equal))
          (make-t flag-hash 0 0 (make-hash) (make-hash) (make-hash)))))
  
  (define (graph? graph) (t? graph))
  
  (define no-value (box empty))
  
  ;; Makes a hash with the same 'equal as the graph
  (define (graph-make-hash graph)
    (if (graph-has-flag? graph 'equal) 
        (make-hash 'equal)
        (make-hash)))
  
  
  (define (expands-safe-flag flags)
    (let loop ((cur flags) (acc empty))
      (cond [(empty? cur) acc]
            [(eq? (first cur) 'safe) (loop (rest cur) (append '(unique-node unique-edge nodes-must-exists) flags))]
            [true (loop (rest cur) (cons (first cur) acc))])))
  
  ;; Make a graph with mostly the same flags as another graph
  (define (graph-make-similar graph plus-flags minus-flags)
    (set! plus-flags (expands-safe-flag plus-flags))
    (set! minus-flags (expands-safe-flag minus-flags))
    (apply make-graph 
           (append plus-flags
                   (filter (lambda (i) (not (member i minus-flags)))
                           (hash-keys (t-flags graph))))))
  
  (define (graph-copy graph)
    (let* ((rtn-nodes (graph-make-hash graph))
           (rtn-successors (graph-make-hash graph))
           (rtn-predessessors (graph-make-hash graph))
           (rtn (make-t (t-flags graph) (t-nNodes graph) (t-nEdges graph) rtn-nodes rtn-successors rtn-predessessors)))
      
      (hash-add-all! rtn-nodes (t-nodes graph))
      (hash-add-all! rtn-successors (t-successors graph))
      (hash-add-all! rtn-predessessors (t-predessessors graph))
      rtn))
  
  (define (graph-add-all! dest-graph src-graph)
    (graph-for-each-node
     src-graph
     (lambda (node)
       (if (graph-node-has-label? src-graph node)
           (graph-node-add! dest-graph node (graph-node-label src-graph node))
           (graph-node-add! dest-graph node))))
    (graph-for-each-edge
     src-graph
     (lambda (from to) 
       (if (graph-edge-has-label? src-graph from to)
           (graph-edge-add! dest-graph from to (graph-edge-label src-graph from to))
           (graph-edge-add! dest-graph from to)))))
  
  (define (graph-has-flag? graph flag)
    (hash-mem? (t-flags graph) flag))
  
  (define (graph-directed? graph)
    (hash-mem? (t-flags graph) 'directed))
  
  ;;; =====================================================================
  ;;; Nodes
  
  (define (graph-nodes graph) (hash-keys (t-nodes graph)))
  
  (define (graph-nodes-size graph) (t-nNodes graph))
  
  (define graph-make-node! 
    (case-lambda
      [(graph) (graph-make-node! graph no-value)]
      [(graph val)
       (let ((sym (string->symbol (string-append "node" (number->string (t-nNodes graph))))))
         (graph-node-add! graph sym val)
         sym)]))
  
  ;; Add a node to the graph. If the node already exists, 
  ;; sets its label, unless the graph has the 'unique-node property,
  ;; in which case this will assert.
  (define graph-node-add! 
    (case-lambda
      [(graph node) (graph-node-add! graph node no-value)]
      [(graph node val)
       (if (hash-mem? (t-nodes graph) node)
           (assert (not (graph-has-flag? graph 'unique-node)))
           (begin 
             (set-t-nNodes! graph (+ 1 (t-nNodes graph)))
             (hash-put! (t-successors graph) node (graph-make-hash graph))
             (if (graph-directed? graph)
                 (hash-put! (t-predessessors graph) node (graph-make-hash graph)))))
       (hash-put! (t-nodes graph) node val)]))
  
  (define (graph-node-mem? graph node)
    (hash-mem? (t-nodes graph) node))
  
  (define (graph-node-set! graph node val)
    (assert (hash-mem? (t-nodes graph) node))
    (hash-put! (t-nodes graph) node val))
  
  (define (graph-node-remove! graph node)
    (assert (graph-node-mem? graph node))
    (for-each (lambda (i) (graph-edge-remove! graph node i))
              (hash-keys (hash-get (t-successors graph) node)))
    
    (if (graph-directed? graph)
        (for-each (lambda (i) (graph-edge-remove! graph i node))
                  (hash-keys (hash-get (t-predessessors graph) node))))
    
    (hash-remove! (t-nodes graph) node)
    (hash-remove! (t-successors graph) node)
    (if (graph-directed? graph)
        (hash-remove! (t-predessessors graph) node))
    (set-t-nNodes! graph (- (t-nNodes graph) 1)))
  
  (define graph-node-collapse!
    (case-lambda
      [(graph node with-self-loop) (graph-node-collapse! graph node with-self-loop (lambda (pred-label succ-label) no-value))]
      [(graph node with-self-loop label-fn)
       (let ((is-directed (graph-directed? graph)))
         (for-each
          
          (lambda (pred)
            (for-each
             (lambda (succ)
               (unless (or (and (not is-directed) (eq? pred succ))
                           (graph-edge-mem? graph pred succ))
                 (let* ((label-pred (hash-get (hash-get (t-successors graph) pred) node))
                        (label-succ (hash-get (hash-get (t-successors graph) node) succ))
                        (new-label (label-fn (if (eq? label-pred no-value) false label-pred)
                                             (if (eq? label-succ no-value) false label-succ))))
                   (when (or with-self-loop (not (eq? pred succ)))
                     (hash-put! (hash-get (t-successors graph) pred) succ new-label)
                     (if is-directed
                         (hash-put! (hash-get (t-predessessors graph) succ) pred new-label)
                         (hash-put! (hash-get (t-successors graph) succ) pred new-label))))))
             (hash-keys (hash-get (t-successors graph) node))))
          
          (hash-keys (hash-get
                      (if is-directed (t-predessessors graph) (t-successors graph))
                      node))))
       (graph-node-remove! graph node)]))
  
  (define (graph-node-has-label? graph node)
    (not (eq? (hash-get (t-nodes graph) node) no-value)))
  
  (define (graph-node-label graph node)
    (let ((r (hash-get (t-nodes graph) node)))
      (if (eq? r no-value) (error "graph-node-label: no value for node" node)
          r)))
  
  (define (graph-succs graph node)
    (assert (graph-directed? graph))
    (hash-keys (hash-get (t-successors graph) node)))
  
  (define (graph-preds graph node)
    (assert (graph-directed? graph))
    (hash-keys (hash-get (t-predessessors graph) node)))
  
  (define (graph-adjs graph node)
    (if (graph-directed? graph) 
        (append (hash-keys (hash-get (t-successors graph) node))
                (hash-keys (hash-get (t-predessessors graph) node)))
        (hash-keys (hash-get (t-successors graph) node))))
  
  (define (graph-for-each-adjs graph node fn)
    (for-each (hash-keys (hash-get (t-successors graph) node))
              (lambda (succ) (fn node succ)))
    (when (graph-directed? graph)
      (for-each (hash-keys (hash-get (t-predessessors graph) node))
                (lambda (pred) (fn pred node)))))
  
  (define (graph-for-each-node graph fn)
    (for-each fn (hash-keys (t-nodes graph))))
  
  (define (graph-fold-nodes graph init fn)
    (let ((acc init))
      (graph-for-each-node
       graph 
       (lambda (node) (set! acc (fn node acc))))
      acc))
  
  ;;; =====================================================================
  ;;; Edges
  
  (define (graph-edges graph)
    (let ((rtn empty))
      (graph-for-each-edge graph (lambda (from to) (set! rtn (cons from to))))
      rtn))
  
  (define (graph-edges-size graph) (t-nEdges graph))
  
  ;; Add an edge to the graph. If the edge already exists, 
  ;; sets its label, unless the graph has the 'unique-edge property,
  ;; in which case this will assert.
  (define graph-edge-add! 
    (case-lambda 
      [(graph from to) (graph-edge-add! graph from to no-value)]
      [(graph from to val)
       
       (if (graph-edge-mem? graph from to)
           (assert (not (graph-has-flag? graph 'unique-edge)))
           (set-t-nEdges! graph (+ (t-nEdges graph) 1)))
       
       (if (graph-has-flag? graph 'nodes-must-exists)
           (assert (and (graph-node-mem? graph from) (graph-node-mem? graph to)))
           (begin (if (not (graph-node-mem? graph from)) (graph-node-add! graph from))
                  (if (not (graph-node-mem? graph to)) (graph-node-add! graph to))))
       
       (hash-put! (hash-get (t-successors graph) from) to val)
       
       (if (graph-directed? graph)
           (hash-put! (hash-get (t-predessessors graph) to) from val)
           (hash-put! (hash-get (t-successors graph) to) from val))]))
  
  (define (graph-edge-mem? graph from to)
    (if (graph-has-flag? graph 'nodes-must-exists) 
        (assert (and (graph-node-mem? graph from)
                     (graph-node-mem? graph to))))
    
    (and (hash-mem? (t-successors graph) from)
         (hash-mem? (hash-get (t-successors graph) from) to)))
  
  (define (graph-edge-set! graph from to val)
    (assert (graph-edge-mem? graph from to))
    (hash-put! (hash-get (t-successors graph) from) to val)
    
    (if (graph-directed? graph)
        (hash-put! (hash-get (t-predessessors graph) to) from val)
        (hash-put! (hash-get (t-successors graph) to) from val)))
  
  (define (graph-edge-remove! graph from to)
    (assert (graph-edge-mem? graph from to))
    (hash-remove! (hash-get (t-successors graph) from) to)
    
    (if (graph-directed? graph)
        (hash-remove! (hash-get (t-predessessors graph) to) from)
        (hash-remove! (hash-get (t-successors graph) to) from)))
  
  (define (graph-edge-has-label? graph from to)
    (not (eq? (hash-get (hash-get (t-successors graph) from) to) no-value)))
  
  (define (graph-edge-label graph from to)
    (let ((r (hash-get (hash-get (t-successors graph) from) to)))
      (if (eq? r no-value) (error "graph-edge-label: no value for edge" (cons from to)))
      r))
  
  (define (graph-for-each-edge graph fn)
    (graph-for-each-node 
     graph
     (lambda (from) 
       (for-each (lambda (to) (fn from to))
                 (hash-keys (hash-get (t-successors graph) from))))))
  
  (define (graph-fold-edges graph init fn)
    (let ((acc init))
      (graph-for-each-edge 
       graph 
       (lambda (from to) (set! acc (fn from to acc))))
      acc))
  
  ;;; =====================================================================
  ;;; Algos
  
  (define (graph-dfs-from-node-with-log graph node dealt-with pre-fn post-fn backward)
    (assert (or (not backward) (graph-directed? graph)))
    (if (not (hash-mem? dealt-with node)) 
        (begin (hash-put! dealt-with node true)
               (pre-fn node)
               (for-each (lambda (n) (graph-dfs-from-node-with-log graph n dealt-with pre-fn post-fn backward))
                         (if backward
                             (hash-keys (hash-get (t-predessessors graph) node))
                             (hash-keys (hash-get (t-successors graph) node))))
               (post-fn node))))
  
  
  (define graph-dfs-from-node
    (case-lambda 
      [(graph node pre-fn) (graph-dfs-from-node graph node pre-fn (lambda (i) i))]
      [(graph node pre-fn post-fn)
       (graph-dfs-from-node-with-log graph node (graph-make-hash graph) pre-fn post-fn false)]))
  
  (define graph-dfs-all
    (case-lambda 
      [(graph pre-fn) (graph-dfs-all graph pre-fn (lambda (i) i))]
      [(graph pre-fn post-fn)
       (let ((dealt-with (graph-make-hash graph)))
         (graph-for-each-node graph (lambda (n) (if (not (hash-mem? dealt-with n)) 
                                                    (graph-dfs-from-node-with-log graph n dealt-with pre-fn post-fn false)))))]))
  
  
  (define (graph-components graph)
    (let ((dealt-with (graph-make-hash graph)))
      (graph-fold-nodes
       graph
       empty
       (lambda (node acc)
         (if (hash-mem? dealt-with node) acc
             (let ((cur-component 
                    (let loop ((cur node) (acc empty))
                      (if (hash-mem? dealt-with cur) acc
                          (begin (hash-put! dealt-with cur true)
                                 (foldl (lambda (adj acc) (loop adj acc)) (cons cur acc) 
                                        (graph-adjs graph cur)))))))
               (cons cur-component acc)))))))
  
  (define (graph-strongly-connected-components graph)
    (assert (graph-directed? graph))
    (let ((finish-times empty)
          (dealt-with (graph-make-hash graph)))
      
      (graph-for-each-node 
       graph 
       (lambda (n) (graph-dfs-from-node-with-log 
                    graph n dealt-with 
                    (lambda (i) i)
                    (lambda (i) (set! finish-times (cons i finish-times)))
                    false)))
      
      (set! dealt-with (graph-make-hash graph))
      
      (let ((component-graph (graph-make-similar graph empty '(safe equal)))
            (node2supernode (make-hash)))
        
        (for-each
         (lambda (n) 
           (if (not (hash-mem? dealt-with n))
               (let ((super-node (graph-make-node! component-graph empty)))
                 (graph-dfs-from-node-with-log 
                  graph n dealt-with
                  (lambda (i)
                    (graph-node-set! component-graph super-node (cons i (graph-node-label component-graph super-node)))
                    (hash-put! node2supernode i super-node))
                  (lambda (i) i)
                  true))))
         finish-times)
        (graph-for-each-edge graph
                             (lambda (from to)
                               (graph-edge-add! component-graph 
                                                (hash-get node2supernode from)
                                                (hash-get node2supernode to))))
        (cons component-graph node2supernode))))
  
  (define (graph-topological-sort graph)
    (assert (graph-directed? graph))
    (let ((rtn empty))
      (graph-dfs-all graph (lambda (i) i) (lambda (node) (set! rtn (cons node rtn))))
      rtn))
  
  
  ;;; =====================================================================
  ;;; Utils
  
  (define graph-to-list
    (case-lambda 
      [(graph) (graph-to-list graph false)]
      [(graph with-labels)
       (hash-map (t-nodes graph)
                 (lambda (node node-val)
                   (let ((node-rep (if (and with-labels (graph-node-has-label? graph node))
                                       (cons node (graph-node-label graph node))
                                       node)))
                     (cons node-rep 
                           (hash-fold (hash-get (t-successors graph) node) empty
                                      (lambda (succ edge-val acc)
                                        (if (and with-labels (graph-edge-has-label? graph node succ))
                                            (cons (cons succ (graph-edge-label graph node succ)) acc)
                                            (cons succ acc))))))))]))
  
  (define (graph-to-string-prv graph with-labels to-string)
    (let ([the-to-string (or to-string
                             (lambda (item) (format "~a" item)))])
      (string-append (if (graph-directed? graph) "[di-graph: " "[undirected-graph:")
                     (the-to-string (map (lambda (n)
                                           (cons (first n) (cons '--> (rest n))))
                                         (graph-to-list graph with-labels)))
                     "]")))
  
  (define (graph-to-string graph . to-string)
    (graph-to-string-prv graph false (if (empty? to-string) false (first to-string))))
  
  (define (graph-to-string-with-labels graph . to-string)
    (graph-to-string-prv graph true (if (empty? to-string) true (first to-string))))
  
  
  ;;; =====================================================================
  ;;; Tests
  
  (define (graph-test)
    (define graph (make-graph 'safe 'directed))
    
    (graph-node-add! graph 'a)
    (graph-node-add! graph 'b 2)
    (graph-node-add! graph 'c 3)
    (graph-node-add! graph 'd)
    
    (graph-edge-add! graph 'a 'c)
    (graph-edge-add! graph 'a 'd "asd")
    (graph-edge-add! graph 'b 'c "dfg")
    (graph-edge-add! graph 'b 'd)
    (graph-edge-add! graph 'd 'a)
    
    (display (graph-node-mem? graph 'a))
    (display (graph-edge-mem? graph 'a 'c))
    (newline)
    (display (graph-node-mem? graph 'v))
    (display (graph-edge-mem? graph 'c 'a))
    (display (graph-edge-mem? graph 'a 'b))
    (newline)
    
    (print-each (graph-to-list graph true))
    (graph-for-each-edge graph (lambda (a b) (print-each "A " a b)))
    
    (graph-dfs-from-node graph 'a (lambda (i) (display i)))
    (newline)
    (graph-dfs-from-node graph 'b (lambda (i) (display i)))
    (newline)
    (graph-dfs-from-node graph 'c (lambda (i) (display i)))
    (newline)
    (graph-dfs-from-node graph 'd (lambda (i) (display i)))
    (newline)
    
    (let ((star (make-graph 'directed)))
      (graph-edge-add! star 1 'x)
      (graph-edge-add! star 'x 1)
      (graph-edge-add! star 2 'x)
      (graph-edge-add! star 'x 3)
      (graph-edge-add! star 'x 4)
      (graph-edge-add! star 'x 5)
      (graph-node-collapse! star 'x false)
      (print-each "collapsed:" (graph-to-list star)))
    
    (let ((strong-graph (make-graph 'directed)))
      
      (graph-edge-add! strong-graph 'e 'a)
      (graph-edge-add! strong-graph 'a 'b)
      (graph-edge-add! strong-graph 'b 'e)
      (graph-edge-add! strong-graph 'e 'f)
      (graph-edge-add! strong-graph 'b 'f)
      (graph-edge-add! strong-graph 'b 'c)
      (graph-edge-add! strong-graph 'f 'g)
      (graph-edge-add! strong-graph 'g 'f)
      (graph-edge-add! strong-graph 'c 'g)
      (graph-edge-add! strong-graph 'c 'd)
      (graph-edge-add! strong-graph 'd 'c)
      (graph-edge-add! strong-graph 'g 'h)
      (graph-edge-add! strong-graph 'd 'h)
      (graph-edge-add! strong-graph 'h 'h)
      
      (graph-edge-add! strong-graph 'xa 'xb)
      (graph-edge-add! strong-graph 'xb 'xc)
      (graph-edge-add! strong-graph 'xc 'xa)
      
      (print-each "strong-graph" strong-graph)
      (print-each "component" (graph-components strong-graph))
      (let ((components (graph-strongly-connected-components strong-graph)))
        (print-each "strong-components" components)
        (print-each "toposort" (graph-topological-sort (first components)))))
    
    (let ((u-graph (make-graph)))
      (graph-edge-add! u-graph 'a 'b)
      (graph-edge-add! u-graph 'b 'c)
      (graph-edge-add! u-graph 'c 'd)
      (graph-edge-add! u-graph 'd 'a)
      (graph-edge-add! u-graph 'd 'e)
      (graph-edge-add! u-graph 'e 'c)
      
      (graph-edge-add! u-graph 'xa 'xb)
      (graph-edge-add! u-graph 'xa 'xc)
      (graph-edge-add! u-graph 'xb 'xd)
      (newline)
      (print-each "u-graph" u-graph)
      (graph-edge-remove! u-graph 'b 'a)
      (graph-node-remove! u-graph 'd)
      (print-each "u-graph" u-graph)
      (print-each "component" (graph-components u-graph)))
    
    )
  ;(graph-test)
  )

