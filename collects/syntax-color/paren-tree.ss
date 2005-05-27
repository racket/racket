(module paren-tree mzscheme
  (require (lib "class.ss")
           "token-tree.ss")
  
  (provide paren-tree%)
  
  (define paren-tree%
    (class object%

      ;; matches: (listof (list/p symbol symbol))
      ;; Symbols for the open-close pairs
      (init matches)

      (define open-matches-table (make-hash-table))
      (for-each (lambda (x)
                  (hash-table-put! open-matches-table (car x) (cadr x)))
                matches)
      
      (define close-matches-table (make-hash-table))
      (for-each (lambda (x)
                  (hash-table-put! close-matches-table (cadr x) (car x)))
                matches)
      
      (define/private (is-open? x)
        (hash-table-get open-matches-table x (lambda () #f)))
      
      (define/private (is-close? x)
        (hash-table-get close-matches-table x (lambda () #f)))
      
      (define/private (matches? open close)
        (equal? (hash-table-get open-matches-table open (lambda () #f))
                close))

      (define tree (new token-tree%))
      (define invalid-tree (new token-tree%))
      
      (define/private (split tree pos)
        (send tree search! pos)
        (let ((token-start (send tree get-root-start-position)))
          (cond
            ((send tree is-empty?)
             (values (new token-tree%) (new token-tree%)))
            ((= pos token-start)
             (send tree split-before))
            (else
             (let-values (((first next) (send tree split-after)))
               (let ((first-end (send first get-root-end-position)))
                 (send first add-to-root-length (- pos first-end))
                 (insert-first! next (new token-tree%
                                          (length (- first-end pos))
                                          (data (cons #f 0))))
                 (values first next)))))))
      
      ;; split-tree: natural-number -> 
      ;; Everything at and after pos is marked as invalid.
      ;; pos must not be a position inside of a token.
      (define/public (split-tree pos)
        (let-values (((l r) (split tree pos)))
          (set! tree l)
          (set! invalid-tree r)))
      
      ;; merget-tree: natural-number ->
      ;; Makes the num-to-keep last positions that have been marked
      ;; invalid valid again.
      (define/public (merge-tree num-to-keep)
        (send invalid-tree search-max!)
        (let*-values (((bad good) (split invalid-tree (- (send invalid-tree get-root-end-position)
                                                         num-to-keep)))
                      ((data) (send good get-root-data)))
          (when (and data
                     (not (or (is-open? (car data))
                              (is-close? (car data)))))
            (add-token #f (send good get-root-length))
            (send good remove-root!))
          (insert-last! tree good)))
      
      
      ;; add-token: symbol * natural-number ->
      ;; Adds the token to the end of the valid part of the tree.
      (define/public (add-token type length)
        (cond
          ((or (send tree is-empty?) (is-open? type) (is-close? type))
           ; Big performance increase using the -spec version.
           ;(insert-last! tree (new token-tree% (length length) (data (cons type length))))
           (insert-last-spec! tree length (cons type length)))
          (else
           (send tree search-max!)
           (send tree add-to-root-length length))))

      ;; truncate: natural-number ->
      ;; removes the tokens after pos
      (define/public (truncate pos)
        (let-values (((l r) (split tree pos)))
          (set! tree l)))
        
      ;; match-forward: natural-number? -> (union #f natural-number)^3
      ;; The first return is the starting position of the open-paren
      ;; The second return is the position of the closing paren.
      ;; If the third return is #f, then the first two returns
      ;; represent a real match.
      ;; If the third return is a number, it is the maximum position
      ;; in the tree that was searched.
      ;; If it indicates an error, the first two results give the
      ;; starting and stoping positions for error highlighting.
      ;; If all three return #f, then there was no tree to search, or 
      ;; the position did not immediately preceed an open.
      (define/public (match-forward pos)
        (send tree search! pos)
        (cond
          ((and (not (send tree is-empty?))
                (is-open? (car (send tree get-root-data)))
                (= (send tree get-root-start-position) pos))
           (let ((end
                  (let/ec ret
                    (do-match-forward (node-right (send tree get-root))
                                      (send tree get-root-end-position)
                                      (list (car (send tree get-root-data)))
                                      ret)
                    #f)))
             (cond
               (end
                (values pos end #f))
               (else
                (send tree search-max!)
                (let ((end (send tree get-root-end-position)))
                  (send tree search! pos)
                  (values pos (+ pos (cdr (send tree get-root-data))) end))))))
          (else
           (values #f #f #f))))
      
      ;; match-backward: natural-number? -> (union #f natural-number)^3
      ;; The first return is the starting position of the open-paren
      ;; The second return is the position of the closing paren.
      ;; If the third return is #f, then the first two returns
      ;; represent a real match, otherwise it represents an error
      ;; If it indicates an error, the first two results give the
      ;; starting and stoping positions for error highlighting.
      ;; If all three return #f, then there was no tree to search, or 
      ;; the position did not immediately follow a close.
      (define/public (match-backward pos)
        (send tree search! (if (> pos 0) (sub1 pos) pos))
        (cond
          ((and (not (send tree is-empty?))
                (is-close? (car (send tree get-root-data)))
                (= (+ (cdr (send tree get-root-data))
                      (send tree get-root-start-position))
                   pos))
           (let ((end
                  (let/ec ret
                    (do-match-backward (node-left (send tree get-root))
                                       0
                                       (list (car (send tree get-root-data)))
                                       ret)
                    #f)))
             (cond
               (end
                (values end pos #f))
               (else
                (send tree search! pos)
                (values (- pos (cdr (send tree get-root-data))) pos #t)))))
          (else
           (values #f #f #f))))
      
      ;; is-open-pos?: natural-number -> (union #f symbol)
      ;; if the position starts an open, return the corresponding close,
      ;; otherwise return #f
      (define/public (is-open-pos? pos)
        (send tree search! pos)
        (let ((d (send tree get-root-data)))
          (and (= (send tree get-root-start-position) pos)
               d
               (is-open? (car d)))))

      ;; is-close-pos?: natural-number -> (union #f symbol)
      ;; if the position starts an close, return the corresponding open,
      ;; otherwise return #f
      (define/public (is-close-pos? pos)
        (send tree search! pos)
        (let ((d (send tree get-root-data)))
          (and (= (send tree get-root-start-position) pos)
               d
               (is-close? (car d)))))
      
      (define/private (do-match-forward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (car (node-token-data node)))
                  (left-stack (do-match-forward (node-left node) top-offset stack escape))
                  (new-stack
                   (cond
                     ((is-open? type) (cons type left-stack))
                     ((and (is-close? type) (matches? (car left-stack) type))
                      (cdr left-stack))
                     ((is-close? type) (escape #f))
                     (else left-stack)))
                  (start (+ top-offset (node-left-subtree-length node))))
             (cond
               ((null? new-stack)
                (let ((loc (+ start (cdr (node-token-data node)))))
                  (escape loc)))
               (else
                (do-match-forward (node-right node) (+ start (node-token-length node)) new-stack escape)))))))
      
      (define/private (do-match-backward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (car (node-token-data node)))
                  (right-stack (do-match-backward (node-right node)
                                                  (+ top-offset (node-left-subtree-length node)
                                                     (node-token-length node))
                                                  stack escape))
                  (new-stack
                   (cond
                     ((is-close? type) (cons type right-stack))
                     ((and (is-open? type) (matches? type (car right-stack)))
                      (cdr right-stack))
                     ((is-open? type) (escape #f))
                     (else right-stack))))
             (cond
               ((null? new-stack)
                (escape (+ top-offset (node-left-subtree-length node))))
               (else
                (do-match-backward (node-left node) top-offset new-stack escape)))))))
      
      (define/public (test)
        (let ((v null)
              (i null))
          (send tree for-each (lambda (a b c)
                                (set! v (cons (list a b c) v))))
          (send invalid-tree for-each (lambda (a b c)
                                        (set! i (cons (list a b c) i))))
          (list (reverse v) (reverse i))))
        
      (super-instantiate ())
      )))
