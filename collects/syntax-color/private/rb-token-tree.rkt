#lang racket/base

;; rbtree implementation of the token-tree% interface.
;;
;; We have to adapt a few things:
;;
;;     * rb-trees don't move around their root on search, so we need
;;       to keep a separate "focus".
;;
;;     * We use rb:nil, but the original client uses #f to indicate
;;     empty trees.

;; For speed, we use the uncontracted forms in red-black.rkt.
(require (prefix-in rb: (submod "red-black.rkt" uncontracted))
         racket/class)


(provide token-tree% 
         insert-first! 
         insert-last!
         insert-last-spec!
         insert-first-spec!
         node? node-token-length node-token-data 
         node-left-subtree-length node-left node-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-local-member-name
  get-rb
  set-rb!
  set-focus!)


(define token-tree%
  (class object%
    (init (length #f) (data #f))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state initialization
    (define rb (rb:new-tree))  ;; rb is an instance of rb:tree.
    (define focus rb:nil)      ;; focus is an instance of rb:node.
    (define focus-pos -1)       ;; optimization: the position of the focus.
    (when length
      (rb:insert-last/data! rb data length)
      (set-focus! (rb:tree-root rb) 0))
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;; rb->token-tree: rb:tree -> token-tree%
    ;; Wraps a red-black tree into a token tree.
    (define (rb->token-tree an-rb)
      (define t (new token-tree%))
      (send t set-rb! an-rb)
      (send t set-focus!
            (rb:tree-first an-rb) 
            (if (rb:nil-node? (rb:tree-root an-rb)) -1 0))
      t)


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; private methods:
    (define/public (get-rb) 
      rb)

    (define/public (set-rb! new-rb) 
      (set! rb new-rb))

    (define/public (set-focus! new-focus new-pos) 
      (set! focus new-focus)
      (set! focus-pos new-pos))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; public methods:

    ;; reset-tree: -> void
    ;; Empty the contents of the tree.
    (define/public (reset-tree)
      (rb:reset! rb)
      (set-focus! rb:nil -1))

    (define/public (get-root)
      (nil->false focus))
    
    (define/public (is-empty?)
      (rb:nil-node? focus))

    (define/public (get-root-length)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         (rb:node-self-width focus)]))

    (define/public (get-root-data)
      (cond
        [(rb:nil-node? focus)
         #f]
        [else
         (rb:node-data focus)]))

    (define/public (get-root-start-position)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         focus-pos]))

    (define/public (get-root-end-position)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         (+ focus-pos (rb:node-self-width focus))]))
 
    (define/public (add-to-root-length inc)
      (unless (rb:nil-node? focus)
        (rb:update-node-self-width! focus (+ (rb:node-self-width focus) inc))))

    (define/public (search! key-position)
      ;; TODO: add unit test that makes sure search works.  If there is no
      ;; token, the original just jumps to the closest node.
      (unless (rb:nil-node? focus)
        (cond
         [(<= key-position 0)
          (set-focus! (rb:tree-first rb)
                      (first-pos rb))]
         [(>= key-position (rb:node-subtree-width (rb:tree-root rb)))
          (set-focus! (rb:tree-last rb)
                      (last-pos rb))]
         [else
          (cond
           ;; optimization: are we already where we're searching?
           [(= focus-pos key-position)
            (void)]
           ;; optimization: are we searching for the immediate successor?
           [(= key-position (+ focus-pos (rb:node-self-width focus)))
            (define succ (rb:successor focus))
            (cond [(rb:nil-node? succ)
                   (void)]
                  [else
                   (set-focus! succ key-position)])]
           [else
            (define-values (found-node residue) (rb:search/residual rb key-position))
            (set-focus! found-node (- key-position residue))])])))
    

    ;; last-pos: rb:tree -> natural
    ;; Returns the starting position of the last element in rb.
    (define (last-pos rb)
      (cond
       [(rb:nil-node? (rb:tree-root rb))
        -1]
       [else
        (define pos (- (rb:node-subtree-width (rb:tree-root rb))
                       (rb:node-self-width (rb:tree-last rb))))
        pos]))

    (define (first-pos rb)
      (cond
       [(rb:nil-node? (rb:tree-root rb))
        -1]
       [else
        0]))

         
    (define/public (search-max!)
      (unless (rb:nil-node? focus)
        (set-focus! (rb:tree-last rb) (last-pos rb))))
    
    (define/public (search-min!)
      (unless (rb:nil-node? focus)
        (set-focus! (rb:tree-first rb) 0)))

    (define/public (remove-root!)
      (unless (rb:nil-node? focus)
        (define node-to-delete focus)
        (define pred (rb:predecessor focus))
        (cond [(rb:nil-node? pred)
               (define succ (rb:successor focus))
               (set-focus! succ (if (rb:nil-node? succ) -1 0))]
              [else
               (set-focus! pred (- focus-pos (rb:node-self-width pred)))])
        (rb:delete! rb node-to-delete)))
      


    ;; split/data: natural -> (values natural natural token-tree% token-tree% boolean)
    ;; Splits the tree into 2 trees, invalidating our own to nil.
    ;;
    ;; The first two returned values represent the start and end
    ;; position of the token(s) at pos.  The next two values represent
    ;; the tokens before pos and after pos, not including any tokens
    ;; adjacent to pos.
    ;;
    ;; Thus if pos is on a token boundary, 2 tokens will be dropped.
    ;;
    ;; In this case, the start will be for the first dropped
    ;; token and the stop will be for the second.
    ;;
    ;; The last value is the data at the searched position.
    ;;
    ;; The two tree's foci will be at the edges adjacent to where the split occurred.
    (define/public (split/data pos)
      (cond
        [(rb:nil-node? focus)
         (values 0 0 (new token-tree%) (new token-tree%) #f)]
        [else

         ;; We have a few cases to check for:
         ;; Is the pivot on the edge boundary of the first or last tokens?
         ;; Is the pivot on the boundary between two tokens?
         (cond

          ;; Case 1.
          ;; At the start-edge of the first token?
          [(<= pos 0)
           ;; If so, just delete the first token.
           (define first-token (rb:tree-first rb))
           (rb:delete! rb first-token)
           (define right-tree (rb->token-tree rb))
           (send right-tree set-focus! 
                 (rb:tree-first rb)
                 (first-pos rb))
           (set-focus! rb:nil -1)
           (values 0 
                   (rb:node-self-width first-token)
                   (new token-tree%)
                   right-tree 
                   (rb:node-data first-token))]

          ;; Case 2.
          ;; At the end-edge of the last token?
          [(>= pos (rb:node-subtree-width (rb:tree-root rb)))
           (define total-width (rb:node-subtree-width (rb:tree-root rb)))
           (define last-token (rb:tree-last rb))

           (rb:delete! rb last-token)
           (define left-tree (rb->token-tree rb))
           (send left-tree set-focus! (rb:tree-last rb) (last-pos rb))
           (set-focus! rb:nil -1)
           (values (- total-width (rb:node-self-width last-token))
                   total-width
                   left-tree 
                   (new token-tree%) 
                   (rb:node-data last-token))]

          [else
           ;; Otherwise, pos is somewhere inside the range, and we're
           ;; guaranteed to find the pivot somewhere.
           (search! pos)
           (cond
            ;; If the residue after searching is zero, then we're right
            ;; on the boundary between two tokens, and must delete both.
            [(= focus-pos pos)
             (define pivot-node focus)
             (define-values (left right) (rb:split! rb pivot-node))

             ;; We know the left is non-empty, since otherwise we would
             ;; have hit case 1.
             (define left-last (rb:tree-last left))
             (rb:delete! left left-last)
             (set-focus! rb:nil -1)
             (define-values (left-tree right-tree)
               (values (rb->token-tree left)
                       (rb->token-tree right)))
             (send left-tree set-focus! (rb:tree-last left) (last-pos left))
             (send right-tree set-focus! (rb:tree-first right) (first-pos right))
             (values (- pos (rb:node-self-width left-last))
                     (+ pos (rb:node-self-width pivot-node))
                     left-tree
                     right-tree
                     (rb:node-data pivot-node))]

            [else
             ;; Otherwise, the position is inside just one token.
             (define pivot-node focus)
             (define start-pos focus-pos)
             (define end-pos (+ start-pos (rb:node-self-width pivot-node)))
             (define-values (left right) (rb:split! rb pivot-node))
             (set-focus! rb:nil -1)
             (define-values (left-tree right-tree)
               (values (rb->token-tree left)
                       (rb->token-tree right)))
             (send left-tree set-focus! (rb:tree-last left) (last-pos left))
             (send right-tree set-focus! (rb:tree-first right) (first-pos right))
             (values start-pos end-pos 
                     left-tree
                     right-tree
                     (rb:node-data pivot-node))])])]))
    


    (define/public (split pos)
      (define-values (start-pos end-pos left-tree right-tree data)
        (split/data pos))
      (values start-pos end-pos left-tree right-tree))



    ;; split-after: -> token-tree% * token-tree%
    ;; splits the tree into 2 trees, setting root to #f
    ;; returns a tree including the focus and its predecessors
    ;; then the focus's successors
    ;;
    ;; The left tree's focus is defined to be at its last,
    ;; and the right tree's focus is defined to be at its first.
    ;;
    ;; FIXME: add test case checking semantics of focus after a split-after.
    (define/public (split-after)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-last! left focus)
         (set-focus! rb:nil -1)
         (define-values (left-tree right-tree)
           (values (rb->token-tree left) (rb->token-tree right)))
         (send right-tree set-focus! (rb:tree-first right) (first-pos right))
         (send left-tree set-focus! (rb:tree-last left) (last-pos left))
         (values left-tree right-tree)]))
        

    ;; split-before: -> token-tree% * token-tree%
    ;; splits the tree into 2 trees, setting root to #f
    ;; returns the focus's predecessors, and then a tree including the focus
    ;; and its successors.
    ;;
    ;; The left tree's focus is defined to be at its last,
    ;; and the right tree's focus is defined to be at its first.
    ;;
    ;; FIXME: add test case checking semantics of focus after a split-before.
    (define/public (split-before)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-first! right focus)
         (set-focus! rb:nil -1)
         (define-values (left-tree right-tree)
           (values (rb->token-tree left) (rb->token-tree right)))
         (send left-tree set-focus! (rb:tree-last left) (last-pos left))
         (send right-tree set-focus! (rb:tree-first right) (first-pos right))
         (values left-tree right-tree)]))


    (define/public (to-list)
      (cond
        [(rb:nil-node? focus) '()]
        [else
         (reverse 
          (rb:tree-fold-inorder rb
                                (lambda (n acc)
                                  (cons (vector (rb:node-self-width n)
                                                (node-left-subtree-length n)
                                                (rb:node-data n))
                                        acc))
                                '()))]))

    (define/public (for-each f)
      (cond
        [(rb:nil-node? focus)
         (void)]
        [else
         (rb:tree-fold-inorder rb
                               (lambda (n acc)
                                 (f acc 
                                    (rb:node-self-width n)
                                    (rb:node-data n))
                                 (+ acc (rb:node-self-width n)))
                               0)]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; insert-first!: token-tree% * token-tree% -> void
;; insert tree2 into tree1 as the first thing.
;;
;; Effects:
;;
;; 1.  tree1 will contain the contents of tree2 + tree1
;; 2.  tree2 will be reset to the empty tree.
;;
;; I'm not exactly sure if the behavior of where the tree is focused
;; is something defined.
(define (insert-first! tree1 tree2)
  (define-values (rb1 rb2)
    (values (send tree1 get-rb) (send tree2 get-rb)))
  (define rb-joined (rb:join! rb2 rb1))
  (send tree1 set-rb! rb-joined)
  (send tree1 set-focus!
        (rb:tree-root rb-joined) 
        (node-left-subtree-length (rb:tree-root rb-joined)))
  (send tree2 reset-tree))


;; insert-last!: token-tree% * token-tree%  -> void
;; insert tree2 into tree1 as the last thing.
;;
;; Effects:
;;
;; 1.  tree1 will contain the contents of tree1 + tree2
;; 2.  tree2 will be reset to the empty tree.
;;
;; I'm not exactly sure if the behavior of where the tree is focused
;; is something defined.
(define (insert-last! tree1 tree2)
  (define-values (rb1 rb2)
    (values (send tree1 get-rb) (send tree2 get-rb)))
  (define rb-joined (rb:join! rb1 rb2))
  (send tree1 set-rb! rb-joined)
  (send tree1 set-focus! 
        (rb:tree-root rb-joined) 
        (node-left-subtree-length (rb:tree-root rb-joined)))
  (send tree2 reset-tree))



;; insert-last-spec!: tree natural any -> void
;; Inserts content at the end of the tree.
;;
;; I'm not exactly sure if the behavior of where the tree is focused
;; is something defined.
(define (insert-last-spec! tree length data)
  ;; TODO: add unit test that makes sure insert-last-spec! works.  It's missing
  ;; from the test suite.
  (define the-rb (send tree get-rb))
  (rb:insert-last/data! the-rb data length)
  (send tree set-focus!
        (rb:tree-root the-rb) 
        (node-left-subtree-length (rb:tree-root the-rb))))


;; insert-first-spec!: tree natural any -> void
;; Inserts content at the beginning of the tree.
(define (insert-first-spec! tree length data)
  ;; TODO: add unit test that makes sure insert-last-spec! works.  It's missing
  ;; from the test suite.
  (define the-rb (send tree get-rb))
  (rb:insert-first/data! the-rb data length)
  (send tree set-focus! 
        (rb:tree-root the-rb)
        (node-left-subtree-length (rb:tree-root the-rb))))



(define node? 
  (procedure-rename rb:node? 'node?))
(define node-token-data
  (procedure-rename rb:node-data 'node-token-data))
(define node-token-length
  (procedure-rename rb:node-self-width 'node-token-length))
(define (node-left-subtree-length n)
  (rb:node-subtree-width (rb:node-left n)))

(define (node-left n)
  (cond [(eq? n #f) 
         #f]
        [else
         (nil->false (rb:node-left n))]))

(define (node-right n)
  (cond [(eq? n #f) 
         #f]
        [else
         (nil->false (rb:node-right n))]))

(define-syntax-rule (nil->false n)
  (if (eq? n rb:nil)
      #f
      n))
