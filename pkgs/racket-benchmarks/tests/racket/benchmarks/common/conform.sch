;
; conform.scm   [portable/R^399RS version]
; By Jim Miller [mods by oz]
;               [call to run-benchmark added by wdc 14 Feb 1997]

; (declare (usual-integrations))

;; SORT

(define (vector-copy v)
  (let* ((length (vector-length v))
	 (result (make-vector length)))
    (let loop ((n 0))
      (vector-set! result n (vector-ref v n))
      (if (= n length)
	  v
	  (loop (+ n 1))))))

(define (sort obj pred)
  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
	(split l '() '())
	l))

  (define (split l one two)
    (if (pair? l)
	(split (cdr l) two (cons (car l) one))
	(merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
	  ((pred (car two) (car one))
	   (cons (car two)
		 (merge (cdr two) one)))
	  (else
	   (cons (car one)
		 (merge (cdr one) two)))))

  (cond ((or (pair? obj) (null? obj))
	 (loop obj))
	((vector? obj)
	 (sort! (vector-copy obj) pred))
	(else
	 (error "sort: argument should be a list or vector" obj))))

;; This merge sort is stable for partial orders (for predicates like
;; <=, rather than like <).

(define (sort! v pred)
  (define (sort-internal! vec temp low high)
    (if (< low high)
	(let* ((middle (quotient (+ low high) 2))
	       (next (+ middle 1)))
	  (sort-internal! temp vec low middle)
	  (sort-internal! temp vec next high)
	  (let loop ((p low) (p1 low) (p2 next))
	    (if (not (> p high))
		(cond ((> p1 middle)
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (+ p 1) p1 (+ p2 1)))
		      ((or (> p2 high)
			   (pred (vector-ref temp p1)
				 (vector-ref temp p2)))
		       (vector-set! vec p (vector-ref temp p1))
		       (loop (+ p 1) (+ p1 1) p2))
		      (else
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (+ p 1) p1 (+ p2 1)))))))))

  (if (not (vector? v))
      (error "sort!: argument not a vector" v))

  (sort-internal! v
		  (vector-copy v)
		  0
		  (- (vector-length v) 1))
  v)

;; SET OPERATIONS
; (representation as lists with distinct elements)

(define (adjoin element set)
  (if (memq element set) set (cons element set)))

(define (eliminate element set)
  (cond ((null? set) set)
	((eq? element (car set)) (cdr set))
	(else (cons (car set) (eliminate element (cdr set))))))

(define (intersect list1 list2)
  (let loop ((l list1))
    (cond ((null? l) '())
	  ((memq (car l) list2) (cons (car l) (loop (cdr l))))
	  (else (loop (cdr l))))))

(define (union list1 list2)
  (if (null? list1)
      list2
      (union (cdr list1)
	     (adjoin (car list1) list2))))

;; GRAPH NODES

; (define-structure
;   (internal-node
;    (print-procedure (unparser/standard-method
; 		     'graph-node
; 		     (lambda (state node)
; 		       (unparse-object state (internal-node-name node))))))
;   name
;   (green-edges '())
;   (red-edges '())
;   blue-edges)

; Above is MIT version; below is portable

(define make-internal-node vector)
(define (internal-node-name node) (vector-ref node 0))
(define (internal-node-green-edges node) (vector-ref node 1))
(define (internal-node-red-edges node) (vector-ref node 2))
(define (internal-node-blue-edges node) (vector-ref node 3))
(define (set-internal-node-name! node name) (vector-set! node 0 name))
(define (set-internal-node-green-edges! node edges) (vector-set! node 1 edges))
(define (set-internal-node-red-edges! node edges) (vector-set! node 2 edges))
(define (set-internal-node-blue-edges! node edges) (vector-set! node 3 edges))

; End of portability stuff

(define (make-node name . blue-edges)	; User's constructor
  (let ((name (if (symbol? name) (symbol->string name) name))
	(blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (car blue-edges))))
    (make-internal-node name '() '() blue-edges)))

(define (copy-node node)
  (make-internal-node (name node) '() '() (blue-edges node)))

; Selectors

(define name internal-node-name)
(define (make-edge-getter selector)
  (lambda (node)
    (if (or (none-node? node) (any-node? node))
	(error "Can't get edges from the ANY or NONE nodes")
	(selector node))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))

; Mutators

(define (make-edge-setter mutator!)
  (lambda (node value)
    (cond ((any-node? node) (error "Can't set edges from the ANY node"))
	  ((none-node? node) 'OK)
	  (else (mutator! node value)))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))

;; BLUE EDGES

; (define-structure
;   (blue-edge
;    (print-procedure
;     (unparser/standard-method
;      'blue-edge
;      (lambda (state edge)
;        (unparse-object state (blue-edge-operation edge))))))
;   operation arg-node res-node)

; Above is MIT version; below is portable

(define make-blue-edge vector)
(define (blue-edge-operation edge) (vector-ref edge 0))
(define (blue-edge-arg-node edge) (vector-ref edge 1))
(define (blue-edge-res-node edge) (vector-ref edge 2))
(define (set-blue-edge-operation! edge value) (vector-set! edge 0 value))
(define (set-blue-edge-arg-node! edge value) (vector-set! edge 1 value))
(define (set-blue-edge-res-node! edge value) (vector-set! edge 2 value))

; End of portability stuff

; Selectors
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)

; Mutators
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)

; Higher level operations on blue edges

(define (lookup-op op node)
  (let loop ((edges (blue-edges node)))
    (cond ((null? edges) '())
	  ((eq? op (operation (car edges))) (car edges))
	  (else (loop (cdr edges))))))

(define (has-op? op node)
  (not (null? (lookup-op op node))))

; Add a (new) blue edge to a node

; (define (adjoin-blue-edge! blue-edge node)
;   (let ((current-one (lookup-op (operation blue-edge) node)))
;     (cond ((null? current-one)
; 	       (set-blue-edges! node
; 		 (cons blue-edge (blue-edges node))))
; 	  ((and (eq? (arg-node current-one) (arg-node blue-edge))
; 		(eq? (res-node current-one) (res-node blue-edge)))
; 	   'OK)
; 	   (else (error "Two non-equivalent blue edges for op"
; 			blue-edge node)))))

;; GRAPHS

; (define-structure
;   (internal-graph
;    (print-procedure
;     (unparser/standard-method 'graph
;      (lambda (state edge)
;        (unparse-object state (map name (internal-graph-nodes edge)))))))
;   nodes already-met already-joined)

; Above is MIT version; below is portable

(define make-internal-graph vector)
(define (internal-graph-nodes graph) (vector-ref graph 0))
(define (internal-graph-already-met graph) (vector-ref graph 1))
(define (internal-graph-already-joined graph) (vector-ref graph 2))
(define (set-internal-graph-nodes! graph nodes) (vector-set! graph 0 nodes))

; End of portability stuff

; Constructor

(define (make-graph . nodes)
  (make-internal-graph nodes (make-empty-table) (make-empty-table)))

; Selectors

(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)

; Higher level functions on graphs

(define (add-graph-nodes! graph nodes)
  (set-internal-graph-nodes! graph (cons nodes (graph-nodes graph))))

(define (copy-graph g)
  (define (copy-list l) (vector->list (list->vector l)))
  (make-internal-graph
   (copy-list (graph-nodes g))
   (already-met g)
   (already-joined g)))

(define (clean-graph g)
  (define (clean-node node)
    (if (not (or (any-node? node) (none-node? node)))
	(begin
	  (set-green-edges! node '())
	  (set-red-edges! node '()))))
  (for-each clean-node (graph-nodes g))
  g)

(define (canonicalize-graph graph classes)
  (define (fix node)
    (define (fix-set object selector mutator)
      (mutator object 
	       (map (lambda (node)
		      (find-canonical-representative node classes))
		    (selector object))))
    (if (not (or (none-node? node) (any-node? node)))
	(begin
	  (fix-set node green-edges set-green-edges!)
	  (fix-set node red-edges set-red-edges!)
	  (for-each 
	   (lambda (blue-edge)
	     (set-arg-node! blue-edge
			    (find-canonical-representative (arg-node blue-edge) classes))
	     (set-res-node! blue-edge
			    (find-canonical-representative (res-node blue-edge) classes)))
	   (blue-edges node))))
    node)
  (define (fix-table table)
    (define (canonical? node) (eq? node (find-canonical-representative node classes)))
    (define (filter-and-fix predicate-fn update-fn list)
      (let loop ((list list))
	(cond ((null? list) '())
	      ((predicate-fn (car list))
	       (cons (update-fn (car list)) (loop (cdr list))))
	      (else (loop (cdr list))))))
    (define (fix-line line)
      (filter-and-fix
       (lambda (entry) (canonical? (car entry)))
       (lambda (entry) (cons (car entry)
			     (find-canonical-representative (cdr entry) classes)))
       line))
    (if (null? table)
	'()
	(cons (car table)
	      (filter-and-fix
	       (lambda (entry) (canonical? (car entry)))
	       (lambda (entry) (cons (car entry) (fix-line (cdr entry))))
	       (cdr table)))))
  (make-internal-graph
   (map (lambda (class) (fix (car class))) classes)
   (fix-table (already-met graph))
   (fix-table (already-joined graph))))

;; USEFUL NODES

(define none-node (make-node 'none #t))
(define (none-node? node) (eq? node none-node))

(define any-node (make-node 'any '()))
(define (any-node? node) (eq? node any-node))

;; COLORED EDGE TESTS


(define (green-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
	((none-node? from-node) #t)
	((memq to-node (green-edges from-node)) #t)
	(else #f)))

(define (red-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
	((none-node? from-node) #t)
	((memq to-node (red-edges from-node)) #t)
	(else #f)))

;; SIGNATURE

; Return signature (i.e. <arg, res>) given an operation and a node

(define sig
  (let ((none-comma-any (cons none-node any-node)))
    (lambda (op node)			; Returns (arg, res)
      (let ((the-edge (lookup-op op node)))
	(if (not (null? the-edge))
	    (cons (arg-node the-edge) (res-node the-edge))
	    none-comma-any)))))

; Selectors from signature

(define (arg pair) (car pair))
(define (res pair) (cdr pair))

;; CONFORMITY

(define (conforms? t1 t2)
  (define nodes-with-red-edges-out '())
  (define (add-red-edge! from-node to-node)
    (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
    (set! nodes-with-red-edges-out
	  (adjoin from-node nodes-with-red-edges-out)))
  (define (greenify-red-edges! from-node)
    (set-green-edges! from-node
		      (append (red-edges from-node) (green-edges from-node)))
    (set-red-edges! from-node '()))
  (define (delete-red-edges! from-node)
    (set-red-edges! from-node '()))
  (define (does-conform t1 t2)
    (cond ((or (none-node? t1) (any-node? t2)) #t)
	  ((or (any-node? t1) (none-node? t2)) #f)
	  ((green-edge? t1 t2) #t)
	  ((red-edge? t1 t2) #t)
	  (else
	   (add-red-edge! t1 t2)
	   (let loop ((blues (blue-edges t2)))
	     (if (null? blues)
		 #t
		 (let* ((current-edge (car blues))
			(phi (operation current-edge)))
		   (and (has-op? phi t1)
			(does-conform
			 (res (sig phi t1))
			 (res (sig phi t2)))
			(does-conform
			 (arg (sig phi t2))
			 (arg (sig phi t1)))
			(loop (cdr blues)))))))))
  (let ((result (does-conform t1 t2)))
    (for-each (if result greenify-red-edges! delete-red-edges!)
	      nodes-with-red-edges-out)
    result))

(define (equivalent? a b)
  (and (conforms? a b) (conforms? b a)))

;; EQUIVALENCE CLASSIFICATION
; Given a list of nodes, return a list of equivalence classes

(define (classify nodes)
  (let node-loop ((classes '())
		  (nodes nodes))
    (if (null? nodes)
	(map (lambda (class)
	       (sort class
		     (lambda (node1 node2)
		       (< (string-length (name node1))
			  (string-length (name node2))))))
	     classes)
	(let ((this-node (car nodes)))
	  (define (add-node classes)
	    (cond ((null? classes) (list (list this-node)))
		  ((equivalent? this-node (caar classes))
		   (cons (cons this-node (car classes))
			 (cdr classes)))
		  (else (cons (car classes)
			      (add-node (cdr classes))))))
	  (node-loop (add-node classes)
		     (cdr nodes))))))

; Given a node N and a classified set of nodes,
; find the canonical member corresponding to N

(define (find-canonical-representative element classification)
  (let loop ((classes classification))
    (cond ((null? classes) (error "Can't classify" element)) 
	  ((memq element (car classes)) (car (car classes)))
	  (else (loop (cdr classes))))))

; Reduce a graph by taking only one member of each equivalence 
; class and canonicalizing all outbound pointers

(define (reduce graph)
  (let ((classes (classify (graph-nodes graph))))
    (canonicalize-graph graph classes)))

;; TWO DIMENSIONAL TABLES

(define (make-empty-table) (list 'TABLE))

(define (lookup table x y)
  (let ((one (assq x (cdr table))))
    (if one
	(let ((two (assq y (cdr one))))
	  (if two (cdr two) #f))
	#f)))

(define (insert! table x y value)
  (define (make-singleton-table x y)
    (list (cons x y)))
  (let ((one (assq x (cdr table))))
    (if one
	(set-cdr! one (cons (cons y value) (cdr one)))
	(set-cdr! table (cons (cons x (make-singleton-table y value))
			      (cdr table))))))

;; MEET/JOIN 
; These update the graph when computing the node for node1*node2

(define (blue-edge-operate arg-fn res-fn graph op sig1 sig2)
  (make-blue-edge op
		  (arg-fn graph (arg sig1) (arg sig2))
		  (res-fn graph (res sig1) (res sig2))))

(define (meet graph node1 node2)
  (cond ((eq? node1 node2) node1)
	((or (any-node? node1) (any-node? node2)) any-node) ; canonicalize
	((none-node? node1) node2)
	((none-node? node2) node1)
	((lookup (already-met graph) node1 node2)) ; return it if found
	((conforms? node1 node2) node2)
	((conforms? node2 node1) node1)
	(else
	 (let ((result
		(make-node (string-append "(" (name node1) " ^ " (name node2) ")"))))
	   (add-graph-nodes! graph result)
	   (insert! (already-met graph) node1 node2 result)
	   (set-blue-edges! result
	     (map
	      (lambda (op)
		(blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
	      (intersect (map operation (blue-edges node1))
			 (map operation (blue-edges node2)))))
	   result))))

(define (join graph node1 node2)
  (cond ((eq? node1 node2) node1)
	((any-node? node1) node2)
	((any-node? node2) node1)
	((or (none-node? node1) (none-node? node2)) none-node) ; canonicalize
	((lookup (already-joined graph) node1 node2)) ; return it if found
	((conforms? node1 node2) node1)
	((conforms? node2 node1) node2)
	(else
	 (let ((result
		(make-node (string-append "(" (name node1) " v " (name node2) ")"))))
	   (add-graph-nodes! graph result)
	   (insert! (already-joined graph) node1 node2 result)
	   (set-blue-edges! result
             (map
	      (lambda (op)
		(blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
	      (union (map operation (blue-edges node1))
		     (map operation (blue-edges node2)))))
	   result))))

;; MAKE A LATTICE FROM A GRAPH

(define (make-lattice g print?)
  (define (step g)
    (let* ((copy (copy-graph g))
	   (nodes (graph-nodes copy)))
      (for-each (lambda (first)
		  (for-each (lambda (second)
			      (meet copy first second)
			      (join copy first second))
			    nodes))
		nodes)
      copy))
  (define (loop g count)
    (if print? (display count))
    (let ((lattice (step g)))
      (if print? (begin (display " -> ")
			(display (length (graph-nodes lattice)))))
      (let* ((new-g (reduce lattice))
	     (new-count (length (graph-nodes new-g))))
	(if (= new-count count)
	    (begin
	      (if print? (newline))
	      new-g)
	    (begin
	      (if print? (begin (display " -> ")
				(display new-count) (newline)))
	      (loop new-g new-count))))))
  (let ((graph
	 (apply make-graph
		(adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
    (loop graph (length (graph-nodes graph)))))

;; DEBUG and TEST

(define a '())
(define b '())
(define c '())
(define d '())

(define (reset)
  (set! a (make-node 'a))
  (set! b (make-node 'b))
  (set-blue-edges! a (list (make-blue-edge 'phi any-node b)))
  (set-blue-edges! b (list (make-blue-edge 'phi any-node a)
			   (make-blue-edge 'theta any-node b)))
  (set! c (make-node "c"))
  (set! d (make-node "d"))
  (set-blue-edges! c (list (make-blue-edge 'theta any-node b)))
  (set-blue-edges! d (list (make-blue-edge 'phi any-node c)
			   (make-blue-edge 'theta any-node d)))
  '(made a b c d))

(define (test)
  (reset)
  (map name
       (graph-nodes
	(make-lattice (make-graph a b c d any-node none-node) #t))))
					   ;;; note printflag #t
;(define (time-test)
;  (let ((t (runtime)))
;    (let ((ans (test)))
;      (cons ans (- (runtime) t)))))

;
; run and make sure result is correct
;
(define (go)
  (reset)
  (let ((result '("(((b v d) ^ a) v c)"
		  "(c ^ d)"
		  "(b v (a ^ d))"
		  "((a v d) ^ b)"
		  "(b v d)"
		  "(b ^ (a v c))"
		  "(a v (c ^ d))"
		  "((b v d) ^ a)"
		  "(c v (a v d))"
		  "(a v c)"
		  "(d v (b ^ (a v c)))"
		  "(d ^ (a v c))"
		  "((a ^ d) v c)"
		  "((a ^ b) v d)"
		  "(((a v d) ^ b) v (a ^ d))"
		  "(b ^ d)"
		  "(b v (a v d))"
		  "(a ^ c)"
		  "(b ^ (c v d))"
		  "(a ^ b)"
		  "(a v b)"
		  "((a ^ d) ^ b)"
		  "(a ^ d)"
		  "(a v d)"
		  "d"
		  "(c v d)"
		  "a"
		  "b"
		  "c"
		  "any"
		  "none")))

    (if (equal? (test) result)
	(display " ok.")
	(display " um."))
    (newline)))

;[mods made by wdc]
;(go)
;(exit)

(time (let loop ((n 100))
        (if (zero? n)
            'done
            (begin
              (go)
              (loop (- n 1))))))



