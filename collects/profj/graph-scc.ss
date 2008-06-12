(module graph-scc scheme/base
  
  (provide get-scc)
  
  (define (get-scc nodes get-successors for-each-node)
    (letrec ([in-component (make-hasheq)]
             [dpth-nums (make-hasheq)]
             [root-of-node (make-hasheq)]
             [counter 0]
             [stack null]
             [sccs null]
             
             ;node -> boolean
             [visited? 
              (lambda (node)
                (symbol? (hash-ref in-component node #f)))]
             ;node -> boolean
             [in-component?
              (lambda (node)
                (eq? 'true (hash-ref in-component node #f)))]
             
             ;node node -> node
             [min-root 
              (lambda (old-min new-node)
                #;(printf "~a <= ~a, ~a" 
                          (hash-ref dpth-nums old-min)
                          (hash-ref dpth-nums 
                                    (hash-ref root-of-node new-node)
                                    (lambda () (add1 counter))) counter)
                (if (<= (hash-ref dpth-nums old-min)
                        (hash-ref dpth-nums 
                                  (hash-ref root-of-node new-node)
                                  (add1 counter)))
                    old-min
                    new-node))]
             ;node -> void
             [assign-depth-num 
              (lambda (node)
                (unless (hash-ref dpth-nums node #f)
                  (hash-set! dpth-nums node counter)
                  (set! counter (add1 counter))))]
             
             [push! (lambda (v) (set! stack (cons v stack)))]
             [pop! (lambda () (begin0 (car stack) (set! stack (cdr stack))))]
             
             ;visit: node -> void
             [visit 
              (lambda (node)
                #;(printf "visit of ~a~n" (def-name node))
                (let ([root-v node])
                  (hash-set! root-of-node node root-v)
                  (hash-set! in-component node 'false)
                  (assign-depth-num node)
                  (push! node)
                  (for-each-node
                   (lambda (successor)
                     (unless (visited? successor) (visit successor))
                     #;(printf "finished visiting successor ~a on visit of ~a~n" 
                             (def-name successor) (def-name node))
                     (unless (in-component? successor)
                       #;(printf "old-root-v ~a~n"  (def-name root-v))
                       (set! root-v (min-root root-v successor))
                       #;(printf "new-root-v ~a~n" (def-name root-v))))
                   (get-successors node))
                  #;(printf "root-v ~a for visit of ~a~n" (def-name root-v)
                          (def-name node))
                  (hash-set! root-of-node node root-v)
                  (when (eq? root-v node)
                    (let loop ([w (pop!)] [scc null])
                      #;(printf "~a ~a ~n" w scc)
                      (hash-set! in-component w 'true)
                      (if (eq? w node)
                          (set! sccs (cons (cons w scc) sccs))
                          (loop (pop!) (cons w scc)))))))])
      
      (for-each-node (lambda (node)
                       (set! counter 0)
                       (set! dpth-nums (make-hasheq))
                       (unless (visited? node) (visit node)))
                     nodes)
      
      #;(printf "sccs: ~a~n" (map (lambda (scc)
                                  (map def-name scc)) sccs))
      sccs))
  
  #;(define-struct node (name points-to)(make-inspector))
  #;(define node-list 
      (list (make-node 'a '(b e))
            (make-node 'b '(c))
            (make-node 'c '(a d))
            (make-node 'd '(b))
            (make-node 'e '(f))
            (make-node 'f '(e))))
  #;(define (get-successors node)
      (map (lambda (n)
             (let loop ([nl node-list])
               (if (eq? n (node-name (car nl)))
                   (car nl)
                   (loop (cdr nl)))))
           (node-points-to node)))
  
  )
