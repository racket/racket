; Modified 2 March 1997 by Will Clinger to add graphs-benchmark
; and to expand the four macros below.
; Modified 11 June 1997 by Will Clinger to eliminate assertions
; and to replace a use of "recur" with a named let.
; Modified 4 May 2010 by Vincent St-Amour to get rid of one-armed ifs
; Modified 10 May 2010 by Vincent St-Amour to convert to Typed Scheme
;
; Performance note: (graphs-benchmark 7) allocates
;   34509143 pairs
;     389625 vectors with 2551590 elements
;   56653504 closures (not counting top level and known procedures)

; End of new code.

;;; ==== std.rkt ====

; (define-syntax assert
;     (syntax-rules ()
; 	((assert test info-rest ...)
; 	    #f)))
; 
; (define-syntax deny
;     (syntax-rules ()
; 	((deny test info-rest ...)
; 	    #f)))
; 
; (define-syntax when
;     (syntax-rules ()
; 	((when test e-first e-rest ...)
; 	    (if test
; 		(begin e-first
; 		    e-rest ...)))))
; 
; (define-syntax unless
;     (syntax-rules ()
; 	((unless test e-first e-rest ...)
; 	    (if (not test)
; 		(begin e-first
; 		    e-rest ...)))))

;;; ==== util.rkt ====


; Fold over list elements, associating to the left.
(: fold (All (X Y) ((Listof X) (X Y -> Y) Y -> Y)))
(define fold
    (lambda (lst folder state)
	'(assert (list? lst)
	    lst)
	'(assert (procedure? folder)
	    folder)
	(do ((lst lst
		    (cdr lst))
		(state state
		    (folder (car lst)
			state)))
	    ((null? lst)
		state))))

; Given the size of a vector and a procedure which
; sends indices to desired vector elements, create
; and return the vector.
(: proc->vector (All (X) (Integer (Integer -> X) -> (Vectorof X))))
(define proc->vector
  (lambda (size f)
    '(assert (and (integer? size)
                 (exact? size)
                 (>= size 0))
      size)
    '(assert (procedure? f)
      f)
    (if (zero? size)
        (vector)
        (let ((x (make-vector size (f 0))))
          (let loop ((i 1))
            (if (< i size) (begin               ; [wdc - was when]
                             (vector-set! x i (f i))
                             (loop (+ i 1)))
                #t))
          x))))

(: vector-fold (All (X Y) ((Vectorof X) (X Y -> Y) Y -> Y)))
(define vector-fold
    (lambda (vec folder state)
	'(assert (vector? vec)
	    vec)
	'(assert (procedure? folder)
	    folder)
	(let ((len
		    (vector-length vec)))
	    (do ((i 0
			(+ i 1))
		    (state state
			(folder (vector-ref vec i)
			    state)))
		((= i len)
		    state)))))

(: vec-map (All (X Y) ((Vectorof X) (X -> Y) -> (Vectorof Y))))
(define vec-map
    (lambda (vec proc)
	(proc->vector (vector-length vec)
	    (lambda: ((i : Integer))
		(proc (vector-ref vec i))))))

; Given limit, return the list 0, 1, ..., limit-1.
(: giota (Integer -> (Listof Integer)))
(define giota
    (lambda (limit)
	'(assert (and (integer? limit)
		(exact? limit)
		(>= limit 0))
	    limit)
	(let: _-*- : (Listof Integer)
              ((limit : Integer
                      limit)
               (res : (Listof Integer)
                    '()))
	    (if (zero? limit)
		res
		(let ((limit
			    (- limit 1)))
		    (_-*- limit
			(cons limit res)))))))

; Fold over the integers [0, limit).
(: gnatural-fold (All (X) (Integer (Integer X -> X) X -> X)))
(define gnatural-fold
    (lambda (limit folder state)
	'(assert (and (integer? limit)
		(exact? limit)
		(>= limit 0))
	    limit)
	'(assert (procedure? folder)
	    folder)
	(do ((i 0
		    (+ i 1))
		(state state
		    (folder i state)))
	    ((= i limit)
		state))))

; Iterate over the integers [0, limit).
(: gnatural-for-each (Integer (Integer -> Any) -> Void))
(define gnatural-for-each
    (lambda (limit proc!)
	'(assert (and (integer? limit)
		(exact? limit)
		(>= limit 0))
	    limit)
	'(assert (procedure? proc!)
	    proc!)
	(do: : Void
             ((i : Integer 0
                 (+ i 1)))
             ((= i limit))
	    (proc! i))))

(: natural-for-all? (Integer (Integer -> Boolean) -> Boolean))
(define natural-for-all?
    (lambda (limit ok?)
	'(assert (and (integer? limit)
		(exact? limit)
		(>= limit 0))
	    limit)
	'(assert (procedure? ok?)
	    ok?)
	(let _-*-
	    ((i 0))
	    (or (= i limit)
		(and (ok? i)
		    (_-*- (+ i 1)))))))

(: natural-there-exists? (Integer (Integer -> Boolean) -> Boolean))
(define natural-there-exists?
    (lambda (limit ok?)
	'(assert (and (integer? limit)
		(exact? limit)
		(>= limit 0))
	    limit)
	'(assert (procedure? ok?)
	    ok?)
	(let _-*-
	    ((i 0))
	    (and (not (= i limit))
		(or (ok? i)
		    (_-*- (+ i 1)))))))

(: there-exists? (All (X) ((Listof X) (X -> Boolean) -> Boolean)))
(define there-exists?
    (lambda (lst ok?)
	'(assert (list? lst)
	    lst)
	'(assert (procedure? ok?)
	    ok?)
	(let _-*-
	    ((lst lst))
	    (and (not (null? lst))
		(or (ok? (car lst))
		    (_-*- (cdr lst)))))))


;;; ==== ptfold.rkt ====


; Fold over the tree of permutations of a universe.
; Each branch (from the root) is a permutation of universe.
; Each node at depth d corresponds to all permutations which pick the
; elements spelled out on the branch from the root to that node as
; the first d elements.
; Their are two components to the state:
;	The b-state is only a function of the branch from the root.
;	The t-state is a function of all nodes seen so far.
; At each node, b-folder is called via
;	(b-folder elem b-state t-state deeper accross)
; where elem is the next element of the universe picked.
; If b-folder can determine the result of the total tree fold at this stage,
; it should simply return the result.
; If b-folder can determine the result of folding over the sub-tree
; rooted at the resulting node, it should call accross via
;	(accross new-t-state)
; where new-t-state is that result.
; Otherwise, b-folder should call deeper via
;	(deeper new-b-state new-t-state)
; where new-b-state is the b-state for the new node and new-t-state is
; the new folded t-state.
; At the leaves of the tree, t-folder is called via
;	(t-folder b-state t-state accross)
; If t-folder can determine the result of the total tree fold at this stage,
; it should simply return that result.
; If not, it should call accross via
;	(accross new-t-state)
; Note, fold-over-perm-tree always calls b-folder in depth-first order.
; I.e., when b-folder is called at depth d, the branch leading to that
; node is the most recent calls to b-folder at all the depths less than d.
; This is a gross efficiency hack so that b-folder can use mutation to
; keep the current branch.
(: fold-over-perm-tree (All (Elem BState TState)
                            ((Listof Elem)
                             (Elem BState TState
                                   (BState TState -> TState)
                                   (TState -> TState)
                                   -> TState)
                             BState
                             (BState TState (TState -> TState) -> TState)
                             TState
                             -> TState)))
(define fold-over-perm-tree
    (lambda (universe b-folder b-state t-folder t-state)
	'(assert (list? universe)
	    universe)
	'(assert (procedure? b-folder)
	    b-folder)
	'(assert (procedure? t-folder)
	    t-folder)
	(let: _-*- : TState
	    ((universe : (Listof Elem)
                       universe)
		(b-state : BState
                         b-state)
		(t-state : TState
                         t-state)
		(accross : (TState -> TState)
                         (lambda (final-t-state)
                           final-t-state)))
	    (if (null? universe)
		(t-folder b-state t-state accross)
		(let: _-**- : TState
                      ((in : (Listof Elem)
                        universe)
                       (out : (Listof Elem)
			    '())
			(t-state : TState
                                 t-state))
		    (let*: ((first : Elem
                                   (car in))
			    (rest : (Listof Elem)
                                  (cdr in))
			    (accross : (TState -> TState)
                                     (if (null? rest)
                                         accross
                                         (lambda: ((new-t-state : TState))
                                           (_-**- rest
                                                  (cons first out)
                                                  new-t-state)))))
			(b-folder first
			    b-state
			    t-state
			    (lambda: ((new-b-state : BState)
                                      (new-t-state : TState))
				(_-*- (fold out
                                            (ann cons
                                                 (Elem (Listof Elem)
                                                       -> (Listof Elem)))
                                            rest)
                                      new-b-state
                                      new-t-state
                                      accross))
			    accross)))))))


;;; ==== minimal.rkt ====

(define-type Graph (Vectorof (Vectorof Boolean)))

; A directed graph is stored as a connection matrix (vector-of-vectors)
; where the first index is the `from' vertex and the second is the `to'
; vertex.  Each entry is a bool indicating if the edge exists.
; The diagonal of the matrix is never examined.
; Make-minimal? returns a procedure which tests if a labelling
; of the vertices is such that the matrix is minimal.
; If it is, then the procedure returns the result of folding over
; the elements of the automoriphism group.  If not, it returns #f.
; The folding is done by calling folder via
;	(folder perm state accross)
; If the folder wants to continue, it should call accross via
;	(accross new-state)
; If it just wants the entire minimal? procedure to return something,
; it should return that.
; The ordering used is lexicographic (with #t > #f) and entries
; are examined in the following order:
;	1->0, 0->1
;
;	2->0, 0->2
;	2->1, 1->2
;
;	3->0, 0->3
;	3->1, 1->3
;	3->2, 2->3
;	...
(: make-minimal? (All (State)
                      (Integer ->
                               (Integer
                                Graph
                                ((Vectorof Integer)
                                 Boolean
                                 (Boolean -> Boolean)
                                 -> Boolean)
                                Boolean
                                -> Boolean))))
(define make-minimal?
    (lambda (max-size)
	'(assert (and (integer? max-size)
		(exact? max-size)
		(>= max-size 0))
	    max-size)
	(let: ((iotas : (Vectorof (Listof Integer))
                      (proc->vector (+ max-size 1)
                                    giota))
               (perm : (Vectorof Integer)
                     (make-vector max-size 0)))
	    (lambda (size graph folder state)
		'(assert (and (integer? size)
			(exact? size)
			(<= 0 size max-size))
		    size
		    max-size)
		'(assert (vector? graph)
		    graph)
		'(assert (procedure? folder)
		    folder)
		(fold-over-perm-tree
                 (vector-ref iotas size)
                 (lambda: ((perm-x : Integer)
                           (x : Integer)
                           (state : Boolean)
                           (deeper : (Integer Boolean
                                              -> Boolean))
                           (accross : (Boolean
                                       -> Boolean)))
                          (case (cmp-next-vertex graph perm x perm-x)
			    ((less)
                             #f)
			    ((equal)
                             (vector-set! perm x perm-x)
                             (deeper (+ x 1)
                                     state))
			    ((more)
                             (accross state))
                            (else
                             (error "can't happen"))))
                 0
                 (lambda: ((leaf-depth : Integer)
                           (state : Boolean)
                           (accross : (Boolean -> Boolean)))
                   '(assert (eqv? leaf-depth size)
			    leaf-depth
			    size)
                   (folder perm state accross))
                 state)))))

; Given a graph, a partial permutation vector, the next input and the next
; output, return 'less, 'equal or 'more depending on the lexicographic
; comparison between the permuted and un-permuted graph.
(: cmp-next-vertex (Graph (Vectorof Integer) Integer Integer
                          -> (U 'less 'equal 'more)))
(define cmp-next-vertex
    (lambda (graph perm x perm-x)
	(let ((from-x
		    (vector-ref graph x))
		(from-perm-x
		    (vector-ref graph perm-x)))
	    (let _-*-
		((y
			0))
		(if (= x y)
		    'equal
		    (let ((x->y?
				(vector-ref from-x y))
			    (perm-y
				(vector-ref perm y)))
			(cond ((eq? x->y?
				    (vector-ref from-perm-x perm-y))
				(let ((y->x?
					    (vector-ref (vector-ref graph y)
						x)))
				    (cond ((eq? y->x?
						(vector-ref (vector-ref graph perm-y)
						    perm-x))
					    (_-*- (+ y 1)))
					(y->x?
					    'less)
					(else
					    'more))))
			    (x->y?
				'less)
			    (else
				'more))))))))


;;; ==== rdg.rkt ====

(define-type RDG (Vectorof (Listof Integer)))

; Fold over rooted directed graphs with bounded out-degree.
; Size is the number of vertices (including the root).  Max-out is the
; maximum out-degree for any vertex.  Folder is called via
;	(folder edges state)
; where edges is a list of length size.  The ith element of the list is
; a list of the vertices j for which there is an edge from i to j.
; The last vertex is the root.
(: fold-over-rdg (All (State) (Exact-Positive-Integer
                               Integer
                               (RDG State -> State)
                               State
                               -> State)))
(define fold-over-rdg
    (lambda (size max-out folder state)
	'(assert (and (exact? size)
		(integer? size)
		(> size 0))
	    size)
	'(assert (and (exact? max-out)
		(integer? max-out)
		(>= max-out 0))
	    max-out)
	'(assert (procedure? folder)
	    folder)
	(let*: ((root : Integer
		    (- size 1))
		(edge? : Graph
		    (proc->vector size
			(lambda: ((from : Integer))
			    (ann (make-vector size #f)
                                 (Vectorof Boolean)))))
		(edges : RDG
		    (make-vector size '()))
		(out-degrees : (Vectorof Integer)
		    (make-vector size 0))
		(minimal-folder : (Integer
                                   Graph
                                   ((Vectorof Integer)
                                    Boolean
                                    (Boolean -> Boolean)
                                    -> Boolean)
                                   Boolean
                                   -> Boolean)
                    ;; make-minimal?'s type says it can return #f, but it won't
		    (or (make-minimal? root)
                        (error "can't happen")))
		(non-root-minimal? : (Integer -> Boolean)
		    (let ((cont
				(lambda: ((perm : (Vectorof Integer))
                                          (state : Boolean)
                                          (accross : (Boolean -> Boolean)))
				    '(assert (eq? state #t)
					state)
				    (accross #t))))
			(lambda: ((size : Integer))
			    (minimal-folder size
                                            edge?
                                            cont
                                            #t))))
		(root-minimal? : ( -> Boolean)
		    (let ((cont
				(lambda: ((perm : (Vectorof Integer))
                                          (state : Boolean)
                                          (accross : (Boolean -> Boolean)))
				    '(assert (eq? state #t)
					state)
				    (case (cmp-next-vertex edge? perm root root)
					((less)
					    #f)
					((equal more)
					    (accross #t))
					(else
                                         (error "can't happen"))))))
			(lambda ()
			    (minimal-folder root
                                            edge?
                                            cont
                                            #t)))))
	    (let: _-*- : State
		((vertex : Integer
                         0)
		    (state : State
                           state))
		(cond ((not (non-root-minimal? vertex))
			state)
		    ((= vertex root)
			'(assert
			    (begin
				(gnatural-for-each root
				    (lambda (v)
					'(assert (= (vector-ref out-degrees v)
						(length (vector-ref edges v)))
					    v
					    (vector-ref out-degrees v)
					    (vector-ref edges v))))
				#t))
			(let ((reach?
				    (make-reach? root edges))
				(from-root
				    (vector-ref edge? root)))
			    (let: _-*- : State
				((v : Integer
					0)
                                 (outs : Integer
                                       0)
                                 (efr : (Listof Integer)
                                      '())
                                 (efrr : (Listof (Vectorof Boolean))
                                       '())
                                 (state : State
                                        state))
				(cond ((not (or (= v root)
						(= outs max-out)))
					(vector-set! from-root v #t)
					(let ((state
						    (_-*- (+ v 1)
							(+ outs 1)
							(cons v efr)
							(cons (vector-ref reach? v)
							    efrr)
							state)))
					    (vector-set! from-root v #f)
					    (_-*- (+ v 1)
						outs
						efr
						efrr
						state)))
				    ((and (natural-for-all? root
						(lambda (v)
						    (there-exists? efrr
							(lambda: ((r : (Vectorof Boolean)))
							    (vector-ref r v)))))
					    (root-minimal?))
					(vector-set! edges root efr)
					(folder
					    (proc->vector size
						(lambda: ((i : Integer))
						    (vector-ref edges i)))
					    state))
				    (else
					state)))))
		    (else
			(let ((from-vertex
				    (vector-ref edge? vertex)))
			    (let _-**-
				((sv
					0)
				    (outs
					0)
				    (state
					state))
				(if (= sv vertex)
				    (begin
					(vector-set! out-degrees vertex outs)
					(_-*- (+ vertex 1)
					    state))
				    (let* ((state
						; no sv->vertex, no vertex->sv
						(_-**- (+ sv 1)
						    outs
						    state))
					    (from-sv
						(vector-ref edge? sv))
					    (sv-out
						(vector-ref out-degrees sv))
					    (state
						(if (= sv-out max-out)
						    state
						    (begin
							(vector-set! edges
							    sv
							    (cons vertex
								(vector-ref edges sv)))
							(vector-set! from-sv vertex #t)
							(vector-set! out-degrees sv (+ sv-out 1))
							(let* ((state
								    ; sv->vertex, no vertex->sv
								    (_-**- (+ sv 1)
									outs
									state))
								(state
								    (if (= outs max-out)
									state
									(begin
									    (vector-set! from-vertex sv #t)
									    (vector-set! edges
										vertex
										(cons sv
										    (vector-ref edges vertex)))
									    (let ((state
											; sv->vertex, vertex->sv
											(_-**- (+ sv 1)
											    (+ outs 1)
											    state)))
										(vector-set! edges
										    vertex
										    (cdr (vector-ref edges vertex)))
										(vector-set! from-vertex sv #f)
										state)))))
							    (vector-set! out-degrees sv sv-out)
							    (vector-set! from-sv vertex #f)
							    (vector-set! edges
								sv
								(cdr (vector-ref edges sv)))
							    state)))))
					(if (= outs max-out)
					    state
					    (begin
						(vector-set! edges
						    vertex
						    (cons sv
							(vector-ref edges vertex)))
						(vector-set! from-vertex sv #t)
						(let ((state
							    ; no sv->vertex, vertex->sv
							    (_-**- (+ sv 1)
								(+ outs 1)
								state)))
						    (vector-set! from-vertex sv #f)
						    (vector-set! edges
							vertex
							(cdr (vector-ref edges vertex)))
						    state)))))))))))))

; Given a vector which maps vertex to out-going-edge list,
; return a vector  which gives reachability.
(: make-reach? (Integer RDG -> Graph))
(define make-reach?
    (lambda (size vertex->out)
	(let ((res
		    (proc->vector size
			(lambda: ((v : Integer))
			    (let: ((from-v : (Vectorof Boolean)
                                           (make-vector size #f)))
				(vector-set! from-v v #t)
				(for-each
				    (lambda: ((x : Integer))
					(vector-set! from-v x #t))
				    (vector-ref vertex->out v))
				from-v)))))
	    (gnatural-for-each size
		(lambda: ((m : Integer))
		    (let ((from-m
				(vector-ref res m)))
			(gnatural-for-each size
			    (lambda: ((f : Integer))
				(let ((from-f
					    (vector-ref res f)))
				    (if (vector-ref from-f m); [wdc - was when]
                                       (begin
					(gnatural-for-each size
					    (lambda: ((t : Integer))
						(if (vector-ref from-m t)
                                                   (begin ; [wdc - was when]
						    (vector-set! from-f t #t))
                                                   #t))))
                                       #t)))))))
	    res)))


;;; ==== test input ====

; Produces all directed graphs with N vertices, distinguished root,
; and out-degree bounded by 2, upto isomorphism (there are 44).

;(define go
;  (let ((N 7))
;    (fold-over-rdg N
;      2 
;      cons
;      '())))

(let ((input (with-input-from-file "input.txt" read)))
  (time
   (let: loop : (Listof RDG)
         ((n : Integer 45) (v : (Listof RDG) '()))
     (if (zero? n)
         v
         (loop (- n 1)
               (fold-over-rdg (if input 6 1)
                              2 
                              (ann cons (RDG (Listof RDG) -> (Listof RDG)))
                              (ann '() (Listof RDG))))))))
