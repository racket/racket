(module lr0 mzscheme

  ;; Handle the LR0 automaton
  
  (require "grammar.rkt"
           "graph.rkt"
           mzlib/list
           mzlib/class)
  
  (provide build-lr0-automaton lr0%
           (struct trans-key (st gs)) trans-key-list-remove-dups
           kernel-items kernel-index)

  ;; kernel = (make-kernel (LR1-item list) index)
  ;;   the list must be kept sorted according to item<? so that equal? can
  ;;   be used to compare kernels
  ;;   Each kernel is assigned a unique index, 0 <= index < number of states
  ;; trans-key = (make-trans-key kernel gram-sym)
  (define-struct kernel (items index) (make-inspector))
  (define-struct trans-key (st gs) (make-inspector))

  (define (trans-key<? a b)
    (let ((kia (kernel-index (trans-key-st a)))
          (kib (kernel-index (trans-key-st b))))
      (or (< kia kib)
          (and (= kia kib)
               (< (non-term-index (trans-key-gs a))
                  (non-term-index (trans-key-gs b)))))))
  
  (define (trans-key-list-remove-dups tkl)
    (let loop ((sorted (sort tkl trans-key<?)))
      (cond
        ((null? sorted) null)
        ((null? (cdr sorted)) sorted)
        (else
         (if (and (= (non-term-index (trans-key-gs (car sorted)))
                     (non-term-index (trans-key-gs (cadr sorted))))
                  (= (kernel-index (trans-key-st (car sorted)))
                     (kernel-index (trans-key-st (cadr sorted)))))
             (loop (cdr sorted))
             (cons (car sorted) (loop (cdr sorted))))))))


  ;; build-transition-table : int (listof (cons/c trans-key X) ->
  ;;                          (vectorof (symbol X hashtable))
  (define (build-transition-table num-states assoc)
    (let ((transitions (make-vector num-states #f)))
      (let loop ((i (sub1 (vector-length transitions))))
        (when (>= i 0)
          (vector-set! transitions i (make-hash-table))
          (loop (sub1 i))))
      (for-each
       (lambda (trans-key/kernel)
         (let ((tk (car trans-key/kernel)))
           (hash-table-put! (vector-ref transitions (kernel-index (trans-key-st tk)))
                            (gram-sym-symbol (trans-key-gs tk))
                            (cdr trans-key/kernel))))
       assoc)
      transitions))
  
  ;; reverse-assoc : (listof (cons/c trans-key? kernel?)) ->
  ;;                 (listof (cons/c trans-key? (listof kernel?)))
  (define (reverse-assoc assoc)
    (let ((reverse-hash (make-hash-table 'equal))
          (hash-table-add!
           (lambda (ht k v)
             (hash-table-put! ht k (cons v (hash-table-get ht k (lambda () null)))))))
      (for-each
       (lambda (trans-key/kernel)
         (let ((tk (car trans-key/kernel)))
           (hash-table-add! reverse-hash 
                            (make-trans-key (cdr trans-key/kernel)
                                            (trans-key-gs tk))
                            (trans-key-st tk))))
       assoc)
      (hash-table-map reverse-hash cons)))


  ;; kernel-list-remove-duplicates
  ;; LR0-automaton = object of class lr0%
  (define lr0%
    (class object%
      (super-instantiate ())
      ;; term-assoc : (listof (cons/c trans-key? kernel?))
      ;; non-term-assoc : (listof (cons/c trans-key? kernel?))
      ;; states : (vectorof kernel?)
      ;; epsilons : ???
      (init-field term-assoc non-term-assoc states epsilons)
            
      (define transitions (build-transition-table (vector-length states)
                                                  (append term-assoc non-term-assoc)))
      
      (define reverse-term-assoc (reverse-assoc term-assoc))
      (define reverse-non-term-assoc (reverse-assoc non-term-assoc))
      (define reverse-transitions
        (build-transition-table (vector-length states)
                                (append reverse-term-assoc reverse-non-term-assoc)))
      
      (define mapped-non-terms (map car non-term-assoc))
      
      (define/public (get-mapped-non-term-keys)
        mapped-non-terms)

      (define/public (get-num-states)
        (vector-length states))
      
      (define/public (get-epsilon-trans)
        epsilons)

      (define/public (get-transitions)
        (append term-assoc non-term-assoc))
      
      ;; for-each-state : (state ->) ->
      ;; Iteration over the states in an automaton
      (define/public (for-each-state f)
        (let ((num-states (vector-length states)))
          (let loop ((i 0))
            (if (< i num-states)
                (begin
                  (f (vector-ref states i))
                  (loop (add1 i)))))))
      
      ;; run-automaton: kernel? gram-sym? -> (union kernel #f)
      ;; returns the state reached from state k on input s, or #f when k
      ;; has no transition on s
      (define/public (run-automaton k s)
        (hash-table-get (vector-ref transitions (kernel-index k))
                        (gram-sym-symbol s)
                        (lambda () #f)))

      ;; run-automaton-back : (listof kernel?) gram-sym? -> (listof kernel)
      ;; returns the list of states that can reach k by transitioning on s.
      (define/public (run-automaton-back k s)
        (apply append
               (map 
                (lambda (k)
                  (hash-table-get (vector-ref reverse-transitions (kernel-index k))
                                  (gram-sym-symbol s)
                                  (lambda () null)))
                k)))))

  (define (union comp<?)
    (letrec ((union
              (lambda (l1 l2)
                (cond
                 ((null? l1) l2)
                 ((null? l2) l1)
                 (else (let ((c1 (car l1))
                             (c2 (car l2)))
                         (cond
                          ((comp<? c1 c2)
                           (cons c1 (union (cdr l1) l2)))
                          ((comp<? c2 c1)
                           (cons c2 (union l1 (cdr l2))))
                          (else (union (cdr l1) l2)))))))))
      union))
  

  ;; The kernels in the automaton are represented cannonically.
  ;; That is (equal? a b) <=> (eq? a b)
  (define (kernel->string k)
    (apply string-append 
           `("{" ,@(map (lambda (i) (string-append (item->string i) ", ")) 
                        (kernel-items k)) 
             "}")))

  ;; build-LR0-automaton: grammar -> LR0-automaton
  ;; Constructs the kernels of the sets of LR(0) items of g
  (define (build-lr0-automaton grammar)
;    (printf "LR(0) automaton:\n")
    (letrec (
             (epsilons (make-hash-table 'equal))
             (grammar-symbols (append (send grammar get-non-terms)
                                      (send grammar get-terms)))
             ;; first-non-term: non-term -> non-term list
             ;; given a non-terminal symbol C, return those non-terminal 
             ;; symbols A s.t. C -> An for some string of terminals and
             ;; non-terminals n where -> means a rightmost derivation in many 
             ;; steps.  Assumes that each non-term can be reduced to a string 
             ;; of terms.
             (first-non-term 
              (digraph (send grammar get-non-terms)
                       (lambda (nt)
                         (filter non-term?
                                 (map (lambda (prod)
                                        (sym-at-dot (make-item prod 0)))
                                      (send grammar get-prods-for-non-term nt))))
                       (lambda (nt) (list nt))
                       (union non-term<?)
                       (lambda () null)))
             
             ;; closure: LR1-item list -> LR1-item list
             ;; Creates a set of items containing i s.t. if A -> n.Xm is in it,
             ;; X -> .o is in it too.
             (LR0-closure
              (lambda (i)
                (cond
                 ((null? i) null)
                 (else
                  (let ((next-gsym (sym-at-dot (car i))))
                    (cond
                     ((non-term? next-gsym)
                      (cons (car i)
                            (append 
                             (apply append
                                    (map (lambda (non-term) 
                                           (map (lambda (x) 
                                                  (make-item x 0))
                                                (send grammar 
                                                      get-prods-for-non-term
                                                      non-term)))
                                         (first-non-term next-gsym)))
                             (LR0-closure (cdr i)))))
                     (else
                      (cons (car i) (LR0-closure (cdr i))))))))))


             ;; maps trans-keys to kernels
             (automaton-term null)
             (automaton-non-term null)
             
             ;; keeps the kernels we have seen, so we can have a unique
             ;; list for each kernel
             (kernels (make-hash-table 'equal))

             (counter 0)
             
             ;; goto: LR1-item list -> LR1-item list list
             ;; creates new kernels by moving the dot in each item in the
             ;; LR0-closure of kernel to the right, and grouping them by 
             ;; the term/non-term moved over.  Returns the kernels not
             ;; yet seen, and places the trans-keys into automaton
             (goto
              (lambda (kernel)
                (let (
                      ;; maps a gram-syms to a list of items
                      (table (make-hash-table))

                      ;; add-item!: 
                      ;;   (symbol (listof item) hashtable) item? ->
                      ;; adds i into the table grouped with the grammar
                      ;; symbol following its dot
                      (add-item!
                       (lambda (table i)
                         (let ((gs (sym-at-dot i)))
                           (cond
                            (gs
                             (let ((already 
                                    (hash-table-get table
                                                    (gram-sym-symbol gs)
                                                    (lambda () null))))
                               (unless (member i already)
                                 (hash-table-put! table 
                                                  (gram-sym-symbol gs)
                                                  (cons i already)))))
                            ((= 0 (vector-length (prod-rhs (item-prod i))))
                             (let ((current (hash-table-get epsilons
                                                            kernel
                                                            (lambda () null))))
                               (hash-table-put! epsilons
                                                kernel
                                                (cons i current)))))))))
                  
                  ;; Group the items of the LR0 closure of the kernel
                  ;; by the character after the dot
                  (for-each (lambda (item)
                              (add-item! table item))
                            (LR0-closure (kernel-items kernel)))
                  
                  ;; each group is a new kernel, with the dot advanced.
                  ;; sorts the items in a kernel so kernels can be compared
                  ;; with equal? for using the table kernels to make sure
                  ;; only one representitive of each kernel is created
                  (filter 
                   (lambda (x) x)
                   (map
                    (lambda (i)
                      (let* ((gs (car i))
                             (items (cadr i))
                             (new #f)
                             (new-kernel (sort
                                          (filter (lambda (x) x)
                                                  (map move-dot-right items))
                                          item<?))
                             (unique-kernel (hash-table-get
                                             kernels
                                             new-kernel
                                             (lambda () 
                                               (let ((k (make-kernel
                                                         new-kernel
                                                         counter)))
                                                 (set! new #t)
                                                 (set! counter (add1 counter))
                                                 (hash-table-put! kernels
                                                                  new-kernel
                                                                  k)
                                                 k)))))
                        (cond
                          ((term? gs)
                           (set! automaton-term (cons (cons (make-trans-key kernel gs)
                                                            unique-kernel)
                                                      automaton-term)))
                          (else
                           (set! automaton-non-term (cons (cons (make-trans-key kernel gs)
                                                                unique-kernel)
                                                          automaton-non-term))))
                        #;(printf "~a -> ~a on ~a\n" 
                                  (kernel->string kernel)
                                  (kernel->string unique-kernel)
                                  (gram-sym-symbol gs))
                        (if new
                            unique-kernel
                            #f)))
                    (let loop ((gsyms grammar-symbols))
                      (cond
                        ((null? gsyms) null)
                        (else
                         (let ((items (hash-table-get table
                                                      (gram-sym-symbol (car gsyms))
                                                      (lambda () null))))
                           (cond
                             ((null? items) (loop (cdr gsyms)))
                             (else
                              (cons (list (car gsyms) items)
                                    (loop (cdr gsyms))))))))))))))

             (starts 
              (map (lambda (init-prod) (list (make-item init-prod 0)))
                   (send grammar get-init-prods)))
             (startk
              (map (lambda (start)
                     (let ((k (make-kernel start counter)))
                       (hash-table-put! kernels start k)
                       (set! counter (add1 counter))
                       k))
                   starts))
             (new-kernels (make-queue)))

      (let loop ((old-kernels startk)
                 (seen-kernels null))
        (cond
         ((and (empty-queue? new-kernels) (null? old-kernels))
          (make-object lr0% 
                       automaton-term
                       automaton-non-term
                       (list->vector (reverse seen-kernels))
                       epsilons))
         ((null? old-kernels)
          (loop (deq! new-kernels) seen-kernels))
         (else 
          (enq! new-kernels (goto (car old-kernels)))
          (loop (cdr old-kernels) (cons (car old-kernels) seen-kernels)))))))

  (define-struct q (f l) (make-inspector))
  (define (empty-queue? q)
    (null? (q-f q)))
  (define (make-queue)
    (make-q null null))
  (define (enq! q i)
    (if (empty-queue? q)
        (let ((i (mcons i null)))
          (set-q-l! q i)
          (set-q-f! q i))
        (begin
          (set-mcdr! (q-l q) (mcons i null))
          (set-q-l! q (mcdr (q-l q))))))
  (define (deq! q)
    (begin0
     (mcar (q-f q))
     (set-q-f! q (mcdr (q-f q)))))

)
