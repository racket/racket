(module paren-tree mzscheme
  (require mzlib/class
           mzlib/list
           "token-tree.rkt")
  
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
      
      (define back-cache (make-hash-table))
      (define (reset-cache) (set! back-cache (make-hash-table)))
      
      (define/private (is-open? x)
        (hash-table-get open-matches-table x #f))
      
      (define/private (is-close? x)
        (hash-table-get close-matches-table x #f))
      
      (define/private (matches? open close)
        (equal? (hash-table-get open-matches-table open #f)
                close))

      ;; The tree and invalid-tree splay trees map ranges of text to paren
      ;; records whose type field is a symbol that indicates which type of
      ;; (opening or closing) parenthesis begins the range being mapped.
      ;; The length field indicates how many characters the actual parenthesis
      ;; is.  In the special case that there is a region that is not preceded
      ;; with a parenthesis (that is, the region before the first parenthesis in
      ;; a buffer), the type will be #f, and the length will be 0.
      
      (define-struct paren (type length))
      (define tree (new token-tree%))
      (define invalid-tree (new token-tree%))
      
      (define common-parens
        (list (make-paren '|(| 1)
              (make-paren '|)| 1)
              (make-paren '|]| 1)
              (make-paren '|[| 1)
              (make-paren '|}| 1)
              (make-paren '|{| 1)))
      (define false-zero-paren (make-paren #f 0))
      
      (define (build-paren type len)
        (cond
          [(eq? len 1)
           (or (ormap (λ (cp) (and (equal? (paren-type cp) type)
                                   cp))
                      common-parens)
               (make-paren type len))]
          [(and (eq? length 0) (eq? type #f))
           false-zero-paren]
          [else
           (make-paren type len)]))
        
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
                                          (data (build-paren #f 0))))
                 (values first next)))))))
      
      ;; split-tree: natural-number -> void
      ;; Everything at and after pos is marked as invalid.
      ;; pos must not be a position inside of a token.
      (define/public (split-tree pos)
        (reset-cache)
        (let-values (((l r) (split tree pos)))
          (set! tree l)
          (set! invalid-tree r)))
      
      ;; merge-tree: natural-number -> void
      ;; Makes the num-to-keep last positions that have been marked
      ;; invalid valid again.
      (define/public (merge-tree num-to-keep)
        (reset-cache)
        (send invalid-tree search-max!)
        (let*-values (((bad good) (split invalid-tree (- (send invalid-tree get-root-end-position)
                                                         num-to-keep)))
                      ((data) (send good get-root-data)))
          (when (and data
                     (not (or (is-open? (paren-type data))
                              (is-close? (paren-type data)))))
            (add-token #f (send good get-root-length))
            (send good remove-root!))
          (insert-last! tree good)))
      
      
      ;; add-token: (union #f symbol) * natural-number ->
      ;; Adds the token to the end of the valid part of the tree.
      ;; If type is #f, then this is not a parenthesis token.  If it is a symbol, then
      ;; it should be in one of the pairs in the matches field.
      (define/public (add-token type length)
        (reset-cache)
        (cond
          ((or (send tree is-empty?) (is-open? type) (is-close? type))
           ; Big performance increase using the -spec version.
           ;(insert-last! tree (new token-tree% (length length) (data (cons type length))))
           (insert-last-spec! tree length 
                              (build-paren type (if type length 0))))
          (else
           (send tree search-max!)
           (send tree add-to-root-length length))))

      ;; truncate: natural-number ->
      ;; removes the tokens after pos
      (define/public (truncate pos)
        (reset-cache)
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
      ;; the position did not immediately precede an open.
      (define/public (match-forward pos)
        (send tree search! pos)
        (cond
          ((and (not (send tree is-empty?))
                (is-open? (paren-type (send tree get-root-data)))
                (= (send tree get-root-start-position) pos))
           (let ((end
                  (let/ec ret
                    (do-match-forward (node-right (send tree get-root))
                                      (send tree get-root-end-position)
                                      (list (paren-type (send tree get-root-data)))
                                      ret)
                    #f)))
             (cond
               (end
                (values pos end #f))
               (else
                (send tree search-max!)
                (let ((end (send tree get-root-end-position)))
                  (send tree search! pos)
                  (values pos (+ pos (paren-length (send tree get-root-data))) end))))))
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
        (define (not-found)
          (send tree search! pos)
          (values (- pos (paren-length (send tree get-root-data))) pos #t))
        (define already (hash-table-get back-cache pos 'todo))
        (cond
          [(not (eq? 'todo already)) (values already pos #f)]
          [else
           (send tree search! (max 0 (sub1 pos)))
           (let ([type (send tree get-root-data)])
             (cond
               [(and (not (send tree is-empty?))
                     (is-close? (paren-type type))
                     (= (+ (paren-length (send tree get-root-data))
                           (send tree get-root-start-position))
                        pos))
                (let loop ()
                  (let ([p (send tree get-root-start-position)])
                    (cond
                      [(= 0 p) (not-found)]
                      [else
                       (send tree search! (sub1 p))
                       (let ([prev-type (paren-type (send tree get-root-data))]
                             [prev-start-pos (send tree get-root-start-position)])
                         (cond
                           [(and (is-open? prev-type) (matches? prev-type (paren-type type)))
                            (hash-table-put! back-cache pos prev-start-pos)
                            (values prev-start-pos pos #f)]
                           [(is-close? prev-type)
                            (let-values ([(new-start new-end new-err)
                                          (match-backward (+ prev-start-pos 
                                                             (paren-length (send tree get-root-data))))])
                              (cond
                                [new-err
                                 (not-found)]
                                [(and (not new-start) (not new-end) (not new-err))
                                 (error 'colorer-internal)]
                                [else
                                 (send tree search! new-start)
                                 (loop)]))]
                           [(is-open? prev-type)
                            (not-found)]
                           [else 
                            (loop)]))])))]
               [else
                (values #f #f #f)]))]))

      #;(define/public (match-backward pos)
        (send tree search! (if (> pos 0) (sub1 pos) pos))
        (cond
          ((and (not (send tree is-empty?))
                (is-close? (paren-type (send tree get-root-data)))
                (= (+ (paren-length (send tree get-root-data))
                      (send tree get-root-start-position))
                   pos))
           (let ((end
                  (let/ec ret
                    (do-match-backward (node-left (send tree get-root))
                                       0
                                       (list (paren-type (send tree get-root-data)))
                                       ret)
                    #f)))
             (cond
               (end
                (values end pos #f))
               (else
                (send tree search! pos)
                (values (- pos (paren-length (send tree get-root-data))) pos #t)))))
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
               (is-open? (paren-type d)))))

      ;; is-close-pos?: natural-number -> (union #f symbol)
      ;; if the position starts an close, return the corresponding open,
      ;; otherwise return #f
      (define/public (is-close-pos? pos)
        (send tree search! pos)
        (let ((d (send tree get-root-data)))
          (and (= (send tree get-root-start-position) pos)
               d
               (is-close? (paren-type d)))))
      
      (define/private (do-match-forward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (paren-type (node-token-data node)))
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
                (let ((loc (+ start (paren-length (node-token-data node)))))
                  (escape loc)))
               (else
                (do-match-forward (node-right node) (+ start (node-token-length node)) new-stack escape)))))))
      
      
      (define/private (do-match-backward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (paren-type (node-token-data node)))
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
                                (set! v (cons (list a b (cons (paren-type c) (paren-length c))) v))))
          (send invalid-tree for-each (lambda (a b c)
                                        (set! i (cons (list a b (cons (paren-type c) (paren-length c))) i))))
          (list (reverse v) (reverse i))))
        
      (super-instantiate ())
      )))
