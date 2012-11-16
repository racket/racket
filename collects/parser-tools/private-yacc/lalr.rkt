(module lalr mzscheme

  ;; Compute LALR lookaheads from DeRemer and Pennello 1982

  (require "lr0.rkt"
           "grammar.rkt"
           mzlib/list
           mzlib/class)

  (provide compute-LA)
  
  ;; compute-DR: LR0-automaton * grammar -> (trans-key -> term set)
  ;; computes for each state, non-term transition pair, the terminals
  ;; which can transition out of the resulting state
  ;; output term set is represented in bit-vector form
  (define (compute-DR a g)
    (lambda (tk)
      (let ((r (send a run-automaton (trans-key-st tk) (trans-key-gs tk))))
        (term-list->bit-vector
         (filter
          (lambda (term)
            (send a run-automaton r term))
          (send g get-terms))))))
  
  ;; compute-reads: 
  ;;   LR0-automaton * grammar -> (trans-key -> trans-key list)
  (define (compute-reads a g)
    (let ((nullable-non-terms 
           (filter (lambda (nt) (send g nullable-non-term? nt))
                   (send g get-non-terms))))
      (lambda (tk)
        (let ((r (send a run-automaton (trans-key-st tk) (trans-key-gs tk))))
          (map (lambda (x) (make-trans-key r x))
               (filter (lambda (non-term) (send a run-automaton r non-term))
                       nullable-non-terms))))))
  
  ;; compute-read: LR0-automaton * grammar -> (trans-key -> term set)
  ;; output term set is represented in bit-vector form
  (define (compute-read a g)
    (let* ((dr (compute-DR a g))
           (reads (compute-reads a g)))
      (digraph-tk->terml (send a get-mapped-non-term-keys)
                         reads
                         dr
                         (send a get-num-states))))
  ;; returns the list of all k such that state k transitions to state start on the
  ;; transitions in rhs (in order)
  (define (run-lr0-backward a rhs dot-pos start num-states)
    (let loop ((states (list start))
               (i (sub1 dot-pos)))
      (cond
        ((< i 0) states)
        (else (loop (send a run-automaton-back states (vector-ref rhs i))
                    (sub1 i))))))

  ;; prod->items-for-include: grammar * prod * non-term -> lr0-item list
  ;; returns the list of all (B -> beta . nt gamma) such that prod = (B -> beta nt gamma)
  ;; and gamma =>* epsilon
  (define (prod->items-for-include g prod nt)
    (let* ((rhs (prod-rhs prod))
           (rhs-l (vector-length rhs)))
      (append (if (and (> rhs-l 0) (eq? nt (vector-ref rhs (sub1 rhs-l))))
                  (list (make-item prod (sub1 rhs-l)))
                  null)
              (let loop ((i (sub1 rhs-l)))
                (cond
                  ((and (> i 0) 
                        (non-term? (vector-ref rhs i)) 
                        (send g nullable-non-term? (vector-ref rhs i)))
                   (if (eq? nt (vector-ref rhs (sub1 i)))
                       (cons (make-item prod (sub1 i))
                             (loop (sub1 i)))
                       (loop (sub1 i))))
                  (else null))))))

  ;; prod-list->items-for-include: grammar * prod list * non-term -> lr0-item list
  ;; return the list of all (B -> beta . nt gamma) such that  (B -> beta nt gamma) in prod-list
  ;; and gamma =>* epsilon
  (define (prod-list->items-for-include g prod-list nt)
    (apply append (map (lambda (prod) (prod->items-for-include g prod nt)) prod-list)))

  ;; comput-includes: lr0-automaton * grammar -> (trans-key -> trans-key list)
  (define (compute-includes a g)
    (let ((num-states (send a get-num-states))
          (items-for-input-nt (make-vector (send g get-num-non-terms) null)))
      (for-each
       (lambda (input-nt)
         (vector-set! items-for-input-nt (non-term-index input-nt)
                      (prod-list->items-for-include g (send g get-prods) input-nt)))
       (send g get-non-terms))
      (lambda (tk)
        (let* ((goal-state (trans-key-st tk))
               (non-term (trans-key-gs tk))
               (items (vector-ref items-for-input-nt (non-term-index non-term))))
          (trans-key-list-remove-dups
           (apply append
                  (map (lambda (item)
                         (let* ((prod (item-prod item))
                                (rhs (prod-rhs prod))
                                (lhs (prod-lhs prod)))
                           (map (lambda (state)
                                  (make-trans-key state lhs))
                                (run-lr0-backward a 
                                                  rhs
                                                  (item-dot-pos item)
                                                  goal-state 
                                                  num-states))))
                       items)))))))
  
  ;; compute-lookback: lr0-automaton * grammar -> (kernel * proc -> trans-key list)
  (define (compute-lookback a g)
    (let ((num-states (send a get-num-states)))
      (lambda (state prod)
        (map (lambda (k) (make-trans-key k (prod-lhs prod)))
             (run-lr0-backward a (prod-rhs prod) (vector-length (prod-rhs prod)) state num-states)))))
  
  ;; compute-follow:  LR0-automaton * grammar -> (trans-key -> term set)
  ;; output term set is represented in bit-vector form
  (define (compute-follow a g includes)
    (let ((read (compute-read a g)))
      (digraph-tk->terml (send a get-mapped-non-term-keys)
                         includes
                         read
                         (send a get-num-states))))
    
  ;; compute-LA: LR0-automaton * grammar -> kernel * prod -> term set
  ;; output term set is represented in bit-vector form
  (define (compute-LA a g)
    (let* ((includes (compute-includes a g))
           (lookback (compute-lookback a g))
           (follow  (compute-follow a g includes)))
      (lambda (k p)
        (let* ((l (lookback k p))
               (f (map follow l)))
          (apply bitwise-ior (cons 0 f))))))

  (define (print-DR dr a g)
    (print-input-st-sym dr "DR" a g print-output-terms))
  (define (print-Read Read a g)
    (print-input-st-sym Read "Read" a g print-output-terms))
  (define (print-includes i a g)
    (print-input-st-sym i "includes" a g print-output-st-nt))
  (define (print-lookback l a g)
    (print-input-st-prod l "lookback" a g print-output-st-nt))
  (define (print-follow f a g)
    (print-input-st-sym f "follow" a g print-output-terms))
  (define (print-LA l a g)
    (print-input-st-prod l "LA" a g print-output-terms))

  (define (print-input-st-sym f name a g print-output)
    (printf "~a:\n" name)
    (send a for-each-state
     (lambda (state)
       (for-each
        (lambda (non-term)
          (let ((res (f (make-trans-key state non-term))))
            (if (not (null? res))
                (printf "~a(~a, ~a) = ~a\n"
                        name
                        state
                        (gram-sym-symbol non-term)
                        (print-output res)))))
        (send g get-non-terms))))
    (newline))

  (define (print-input-st-prod f name a g print-output)
    (printf "~a:\n" name)
    (send a for-each-state
     (lambda (state)
       (for-each
        (lambda (non-term)
          (for-each
           (lambda (prod)
             (let ((res (f state prod)))
               (if (not (null? res))
                   (printf "~a(~a, ~a) = ~a\n"
                           name
                           (kernel-index state)
                           (prod-index prod)
                           (print-output res)))))
           (send g get-prods-for-non-term non-term)))
        (send g get-non-terms)))))
  
  (define (print-output-terms r)
    (map 
     (lambda (p)
       (gram-sym-symbol p))
     r))
  
  (define (print-output-st-nt r)
    (map
     (lambda (p)
       (list
        (kernel-index (trans-key-st p))
        (gram-sym-symbol (trans-key-gs p))))
     r))

  ;; init-tk-map : int -> (vectorof hashtable?)
  (define (init-tk-map n)
    (let ((v (make-vector n #f)))
      (let loop ((i (sub1 (vector-length v))))
        (when (>= i 0)
          (vector-set! v i (make-hash-table))
          (loop (sub1 i))))
      v))
  
  ;; lookup-tk-map : (vectorof (symbol? int hashtable)) -> trans-key? -> int
  (define (lookup-tk-map map)
    (lambda (tk)
      (let ((st (trans-key-st tk))
            (gs (trans-key-gs tk)))
        (hash-table-get (vector-ref map (kernel-index st))
                        (gram-sym-symbol gs)
                        (lambda () 0)))))

  ;; add-tk-map : (vectorof (symbol? int hashtable)) -> trans-key int -> 
  (define (add-tk-map map)
    (lambda (tk v)
      (let ((st (trans-key-st tk))
            (gs (trans-key-gs tk)))
        (hash-table-put! (vector-ref map (kernel-index st))
                         (gram-sym-symbol gs)
                         v))))
             
  ;; digraph-tk->terml: 
  ;;   (trans-key list) * (trans-key -> trans-key list) * (trans-key -> term list) * int * int * int
  ;;     -> (trans-key -> term list)
  ;; DeRemer and Pennello 1982
  ;; Computes (f x) = (f- x) union Union{(f y) | y in (edges x)}
  ;; A specialization of digraph in the file graph.rkt
  (define (digraph-tk->terml nodes edges f- num-states)
    (letrec [
             ;; Will map elements of trans-key to term sets represented as bit vectors
             (results (init-tk-map num-states))
             
             ;; Maps elements of trans-keys to integers.
             (N (init-tk-map num-states))
             
             (get-N (lookup-tk-map N))
             (set-N (add-tk-map N))
             (get-f (lookup-tk-map results))
             (set-f (add-tk-map results))
             
             (stack null)
             (push (lambda (x)
                     (set! stack (cons x stack))))
             (pop (lambda () 
                    (begin0 
                     (car stack)
                     (set! stack (cdr stack)))))
             (depth (lambda () (length stack)))

             ;; traverse: 'a -> 
             (traverse
              (lambda (x)
                (push x)
                (let ((d (depth)))
                  (set-N x d)
                  (set-f x (f- x))
                  (for-each (lambda (y)
                              (when (= 0 (get-N y))
                                (traverse y))
                              (set-f x (bitwise-ior (get-f x) (get-f y)))
                              (set-N x (min (get-N x) (get-N y))))
                            (edges x))
                  (when (= d (get-N x))
                    (let loop ((p (pop)))
                      (set-N p +inf.0)
                      (set-f p (get-f x))
                      (unless (equal? x p)
                        (loop (pop))))))))]
      (for-each (lambda (x)
                  (when (= 0 (get-N x))
                    (traverse x)))
                nodes)
      get-f))
)
