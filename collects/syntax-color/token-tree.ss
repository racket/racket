(module token-tree mzscheme
  (require mzlib/class)
  
  (provide token-tree% insert-first! insert-last! insert-last-spec!
           node? node-token-length node-token-data node-left-subtree-length node-left node-right)
  
  ;; A tree is 
  ;;  - #f
  ;;  - (make-node NAT 'a NAT tree tree)
  (define-struct node (token-length token-data left-subtree-length left right))
  
  
  ;; ----- The algorithmic implementation of the splay tree for a buffer ------
  
  ;; search-max: tree * tree list -> tree
  (define (search-max node path)
    (cond
      ((not node)
       (end-search path))
      (else
       (search-max (node-right node) (cons node path)))))
  
  ;; search-min: tree * tree list -> tree
  (define (search-min node path)
    (cond
      ((not node)
       (end-search path))
      (else
       (search-min (node-left node) (cons node path)))))
  
  ;; internal-search: tree * NAT * NAT * tree list -> tree
  ;; key-position is the position in the buffer we are looking for
  ;; offset is the offset for the whole subtree of node in the buffer.
  ;; path is the path back to the root
  (define (internal-search node key-position offset path)
    (cond
      ((not node)
       (end-search path))
      (else
       (let* ((node-start (+ offset (node-left-subtree-length node)))
              (node-end (+ node-start (node-token-length node))))
         (cond
           ((< key-position node-start)
            (internal-search (node-left node) key-position offset (cons node path)))
           ((>= key-position node-end)
            (internal-search (node-right node) key-position node-end (cons node path)))
           (else
            (bottom-up-splay node path)))))))
  
  ;; end-search: tree list -> tree
  (define (end-search path)
    (cond
      ((null? path) #f)
      (else (bottom-up-splay (car path) (cdr path)))))
  
  ;; update-subtree-length-left-rotate: tree * tree -> 
  (define (update-subtree-length-left-rotate self parent)
    (set-node-left-subtree-length! parent 
                                   (- (node-left-subtree-length parent)
                                      (node-left-subtree-length self)
                                      (node-token-length self))))
  
  ;; update-subtree-length-right-rotate: tree * tree ->
  (define (update-subtree-length-right-rotate self parent)
    (set-node-left-subtree-length! self
                                   (+ (node-left-subtree-length parent)
                                      (node-left-subtree-length self)
                                      (node-token-length parent))))
  
  ;; bottom-up-splay: tree * tree list -> tree
  (define (bottom-up-splay self path)
    (cond
      ((null? path) self)               ;; node is root already
      ((null? (cdr path))               ;; node's parent is root
       (let ((parent (car path)))
         (cond
           ((eq? self (node-left parent))
            (set-node-left! parent (node-right self))
            (set-node-right! self parent)
            (update-subtree-length-left-rotate self parent))
           (else
            (set-node-right! parent (node-left self))
            (set-node-left! self parent)
            (update-subtree-length-right-rotate self parent))))
       self)
      (else
       (let ((grand (cadr path))
             (parent (car path)))
         (cond
           ((eq? self (node-left parent))
            (cond
              ((eq? parent (node-left grand))
               (set-node-left! grand (node-right parent))
               (set-node-right! parent grand)
               (set-node-left! parent (node-right self))
               (set-node-right! self parent)
               (update-subtree-length-left-rotate parent grand)
               (update-subtree-length-left-rotate self parent))
              (else
               (set-node-right! grand (node-left self))
               (set-node-left! self grand)
               (set-node-left! parent (node-right self))
               (set-node-right! self parent)
               (update-subtree-length-left-rotate self parent)
               (update-subtree-length-right-rotate self grand))))
           (else
            (cond
              ((eq? parent (node-right grand))
               (set-node-right! grand (node-left parent))
               (set-node-left! parent grand)
               (set-node-right! parent (node-left self))
               (set-node-left! self parent)
               (update-subtree-length-right-rotate parent grand)
               (update-subtree-length-right-rotate self parent))
              (else
               (set-node-left! grand (node-right self))
               (set-node-right! self grand)
               (set-node-right! parent (node-left self))
               (set-node-left! self parent)
               (set-node-left-subtree-length! grand
                                              (- (node-left-subtree-length grand)
                                                 (node-left-subtree-length parent)
                                                 (node-token-length parent)
                                                 (node-left-subtree-length self)
                                                 (node-token-length self)))
               (update-subtree-length-right-rotate self parent)))))
         (if (not (null? (cddr path)))
             (if (eq? grand (node-left (caddr path)))
                 (set-node-left! (caddr path) self)
                 (set-node-right! (caddr path) self)))
         (bottom-up-splay self (cddr path))))))

  (define (size node acc)
    (cond
      ((not node) acc)
      (else
       (let ((left-size (size (node-left node) acc)))
         (size (node-right node) (add1 left-size))))))
      
  (define (max-depth node)
    (cond
      ((not node) 0)
      (else
       (add1 (max (max-depth (node-left node))
                  (max-depth (node-right node)))))))

  (define (do-to-list node)
    (cond
      (node
       (append (do-to-list (node-left node))
               (list (vector (node-token-length node) (node-left-subtree-length node) (node-token-data node)))
               (do-to-list (node-right node))))
      (else null)))
  

  (define (do-splay-tree-for-each f node offset)
    (when node
      (do-splay-tree-for-each f (node-left node) offset)
      (let ((node-start (+ offset (node-left-subtree-length node))))
        (f node-start (node-token-length node) (node-token-data node))
        (do-splay-tree-for-each f (node-right node) (+ node-start (node-token-length node))))))
  
  ;; --------------------- The interface to the splay tree --------------------
  
  (define-local-member-name set-root)
  (define token-tree%
    (class object%
      
      (init (length #f)
            (data #f))

      ;; root: tree
      (define root
        (if length
            (make-node length data 0 #f #f)
            #f))
      
      ;; reset-tree ->
      (define/public (reset-tree)
        (set! root #f))
  
      ;; get-root: -> tree
      (define/public (get-root)
        root)

      ;; set-root: tree ->
      ;; set-root is not accessible outside of this module
      (define/public (set-root t)
        (set! root t))
      
      ;; is-empty?: ->
      (define/public (is-empty?)
        (if root #f #t))
      
      (define/public (get-root-length)
        (if root
            (node-token-length root)
            0))
      
      (define/public (get-root-data)
        (if root
            (node-token-data root)
            #f))
      
      (define/public (get-root-start-position)
        (if root
            (node-left-subtree-length root)
            0))
      
      (define/public (get-root-end-position)
        (if root
            (+ (node-left-subtree-length root)
               (node-token-length root))
            0))
      
      (define/public (add-to-root-length inc)
        (unless root
          (error 'add-to-root-length "Attempted to increase root length of empty token-tree%"))
        (set-node-token-length! root (+ (node-token-length root) inc)))
      
      ;; search!: NAT ->
      ;; Moves the node at key-position to the root
      (define/public (search! key-position)
        (when root
          (set! root (internal-search root key-position 0 null))))
      
      ;; search-max!: ->
      ;; moves the maximum node to the root
      (define/public (search-max!)
        (when root
          (set! root (search-max root null))))
      
      ;; search-min!: ->
      ;; moves the minimum node to the root
      (define/public (search-min!)
        (when root
          (set! root (search-min root null))))
      
      ;; remove-root!: ->
      ;; Removes the root node
      (define/public (remove-root!)
        (when root
          (let ((new-node (search-max (node-left root) null)))
            (cond
              (new-node
               (set-node-right! new-node (node-right root))
               (set! root new-node))
              (else
               (set! root (node-right root)))))))

      
      ;; split: -> NAT * NAT * token-tree% * token-tree%
      ;; splits the tree into 2 trees, setting root to #f
      ;; Returns the start and end position of the token at pos
      ;; Returns all tokens before pos and after pos, not
      ;; including any tokens adjacent to pos.  Thus is s
      ;; pos is on a token boundary, 2 tokens will be dropped.
      ;; In this case, the start will be for the first dropped
      ;; token and the stop will be for the second.
      (define/public (split/data pos)
        (search! pos)
        (let ((t1 (new token-tree%))
              (t2 (new token-tree%)))
          (cond
            (root
             (let ((second-start (get-root-start-position))
                   (second-stop (get-root-end-position))
                   (data (node-token-data root)))
               (send t1 set-root (node-left root))
               (send t2 set-root (node-right root))
               (set! root #f)
               (cond
                 ((= pos second-start)
                  (send t1 search-max!)
                  (let ((first-start (send t1 get-root-start-position)))
                    (send t1 remove-root!)
                    (values first-start second-stop t1 t2 data)))
                 (else
                  (values second-start second-stop t1 t2 data)))))
            (else (values 0 0 t1 t2 #f)))))

      (define/public (split pos)
        (let-values ([(orig-token-start orig-token-end valid-tree invalid-tree orig-data)
                      (split/data pos)])
          (values orig-token-start orig-token-end valid-tree invalid-tree)))

;;      (define/public (split)
;;        (let ((t1 (new token-tree%))
;;              (t2 (new token-tree%)))
;;          (cond
;;            (root
;;             (send t1 set-root (node-left root))
;;             (send t2 set-root (node-right root))
;;             (begin0
;;               (values (node-left-subtree-length root)
;;                       (+ (node-left-subtree-length root) (node-token-length root))
;;                       t1
;;                       t2)
;;               (set! root #f)))
;;            (else (values 0 0 t1 t2)))))
      
      ;; split-after: -> token-tree% * token-tree%
      ;; splits the tree into 2 trees, setting root to #f
      ;; returns a tree including root and its left subtree
      ;; then root's right subtree
      (define/public (split-after)
        (let ((t1 (new token-tree%))
              (t2 (new token-tree%)))
          (when root
            (send t1 set-root root)
            (send t2 set-root (node-right root))
            (set-node-right! root #f)
            (set! root #f))
          (values t1 t2)))
           
      ;; split-before: -> token-tree% * token-tree%
      ;; splits the tree into 2 trees, setting root to #f
      ;; returns root's left subtree and a tree including root
      ;; and its right subtree
      (define/public (split-before)
        (let ((t1 (new token-tree%))
              (t2 (new token-tree%)))
          (when root
            (send t1 set-root (node-left root))
            (send t2 set-root root)
            (set-node-left! root #f)
            (set-node-left-subtree-length! root 0)
            (set! root #f))
          (values t1 t2)))
      
      (define/public (to-list)
        (do-to-list root))
      
      (define/public (for-each f)
        (do-splay-tree-for-each f root 0))
      
      (super-instantiate ())))

  ;; insert-first!: token-tree% * token-tree% ->
  ;; insert tree2 into tree1 as the first thing.
  ;; Set tree2's root to #f
  (define (insert-first! tree1 tree2)
    (send tree2 search-max!)
    (let ((node1 (send tree1 get-root))
          (node2 (send tree2 get-root)))
      (when node2
        (set-node-right! node2 node1)
        (send tree1 set-root node2)
        (send tree2 reset-tree))))
  
  ;; insert-last!: token-tree% * token-tree%  ->
  ;; insert tree2 into tree1 as the last thing.
  ;; Set tree2's root to #f
  (define (insert-last! tree1 tree2)
    (send tree1 search-max!)
    (let ((node1 (send tree1 get-root))
          (node2 (send tree2 get-root)))
      (cond
        (node1
         (set-node-right! node1 node2))
        (else
         (send tree1 set-root node2)))
      (send tree2 reset-tree)))

  ;; insert-last-spec!: token-tree% NAT 'a ->
  ;; Same as (insert-last! tree1 (new token-tree% (length len) (data type)))
  ;; This optimization is important for the colorer.
  (define (insert-last-spec! tree1 len type)
    (send tree1 search-max!)
    (let ((node1 (send tree1 get-root))
          (node2 (make-node len type 0 #f #f)))
      (cond
        (node1
         (set-node-right! node1 node2))
        (else
         (send tree1 set-root node2)))))
    
  )
