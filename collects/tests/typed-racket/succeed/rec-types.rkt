#lang typed-scheme

(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(define-type-alias comparator (top top -> number))


(define-typed-struct (a) heap              ([compare : comparator]))
(define-typed-struct (a) (heap-empty heap) ())
(define-typed-struct (a) (heap-node heap)
  ([rank : number] [elm : a] [left : (Un (heap-node a) (heap-empty a))] [right : (Un (heap-node a) (heap-empty a))]))

(define-type-alias (Heap a) (Un (heap-empty a) (heap-node a)))


(pdefine: (b) (heap-size [h : (Heap b)]) : number
	  (cond [(heap-empty? h) 0]
		[(heap-node? h)
		 (+ 1 (+ (heap-size (heap-node-left h))
			 (heap-size (heap-node-right h))))]
		;; FIXME - shouldn't need else clause
		[else (error "Never happens!")]))


(define-typed-struct npheap              ([compare : comparator]))
(define-typed-struct (npheap-empty npheap) ())
(define-typed-struct (npheap-node npheap)
  ([rank : number] [elm : symbol] [left : (Un npheap-node npheap-empty)] [right : (Un npheap-node npheap-empty)]))

(define-type-alias npHeap (Un npheap-empty npheap-node))


(define: (npheap-size [h : npHeap]) : number
  (cond [(npheap-empty? h) 0]
	[else
	 (+ 1 (+ (npheap-size (npheap-node-left h))
		 (npheap-size (npheap-node-right h))))]))
