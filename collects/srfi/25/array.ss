;; SRFI-25: Array implementation
;;; 1997 - 2001 Jussi Piitulainen

;;; --- Intro ---

;;; This interface to arrays is based on Alan Bawden's array.scm of
;;; 1993 (earlier version in the Internet Repository and another
;;; version in SLIB). This is a complete rewrite, to be consistent
;;; with the rest of Scheme and to make arrays independent of lists.

;;; Some modifications are due to discussion in srfi-25 mailing list.

;;; (array? obj)
;;; (make-array shape [obj])             changed arguments
;;; (shape bound ...)                    new
;;; (array shape obj ...)                new
;;; (array-rank array)                   changed name back
;;; (array-start array dimension)        new
;;; (array-end array dimension)          new
;;; (array-ref array k ...)
;;; (array-ref array index)              new variant
;;; (array-set! array k ... obj)         changed argument order
;;; (array-set! array index obj)         new variant
;;; (share-array array shape proc)       changed arguments

;;; All other variables in this file have names in "array:".

;;; Should there be a way to make arrays with initial values mapped
;;; from indices? Sure. The current "initial object" is lame.
;;;
;;; Removed (array-shape array) from here. There is a new version
;;; in arlib though.

;;; --- Representation type dependencies ---

;;; The mapping from array indices to the index to the underlying vector
;;; is whatever array:optimize returns. The file "opt" provides three
;;; representations:
;;; 
;;; mbda) mapping is a procedure that allows an optional argument
;;; tter) mapping is two procedures that takes exactly the indices
;;; ctor) mapping is a vector of a constant term and coefficients
;;;
;;; Choose one in "opt" to make the optimizer. Then choose the matching
;;; implementation of array-ref and array-set!.
;;;
;;; These should be made macros to inline them. Or have a good compiler
;;; and plant the package as a module.

;;; 1. Pick an optimizer.
;;; 2. Pick matching index representation.
;;; 3. Pick a record implementation; as-procedure is generic; syntax inlines.
;;; 3. This file is otherwise portable.

;;; --- Portable R5RS (R4RS and multiple values) ---


(module array mzscheme
  (require (lib "9.ss" "srfi");; record-types
	   )
  (provide array? make-array shape array array-rank
	   array-start array-end array-ref array-set! share-array
	   ;; FIXME: should we export all these?
	   ;; Array library: high level implementation of useful procedures
	   array-shape array-length array-size array-equal?
	   shape-for-each array-for-each-index
	   tabulate-array array-retabulate! array-map array-map!
	   array->vector share-array/prefix share-array/origin
	   array-append transpose share-nths)
  ;; As defined in as-srfi-9-record.scm in the reference implementation:
  (define-record-type
    array:srfi-9-record-type-descriptor
    (array:make vec ind shp)
    array:array?
    (vec array:vector)
    (ind array:index)
    (shp array:shape))

  ;; Using SRFI-25 ctor based index representation
  (require (lib "include.ss"))
  (include "ix-ctor.scm")
  (include "op-ctor.scm")

;;; --- Portable R5RS (R4RS and multiple values) ---

;;; (array? obj)
;;; returns #t if `obj' is an array and #t or #f otherwise.

  (define (array? obj)
    (array:array? obj))

;;; (make-array shape)
;;; (make-array shape obj)
;;; makes array of `shape' with each cell containing `obj' initially.

  (define (make-array shape . rest)
    (or (array:good-shape? shape)
	(error "make-array: shape is not a shape"))
    (apply array:make-array shape rest))

  (define (array:make-array shape . rest)
    (let ((size (array:size shape)))
      (array:make
       (if (pair? rest)
	   (apply (lambda (o) (make-vector size o)) rest)
	   (make-vector size))
       (if (= size 0)
	   (array:optimize-empty
	    (vector-ref (array:shape shape) 1))
	   (array:optimize
	    (array:make-index shape)
	    (vector-ref (array:shape shape) 1)))
       (array:shape->vector shape))))

;;; (shape bound ...)
;;; makes a shape. Bounds must be an even number of exact, pairwise
;;; non-decreasing integers. Note that any such array can be a shape.

  (define (shape . bounds)
    (let ((v (list->vector bounds)))
      (or (even? (vector-length v))
	  (error (string-append "shape: uneven number of bounds: "
				(array:list->string bounds))))
      (let ((shp (array:make
		  v
		  (if (pair? bounds)
		      (array:shape-index)
		      (array:empty-shape-index))
		  (vector 0 (quotient (vector-length v) 2)
			  0 2))))
	(or (array:good-shape? shp)
	    (error (string-append "shape: bounds are not pairwise "
				  "non-decreasing exact integers: "
				  (array:list->string bounds))))
	shp)))

;;; (array shape obj ...)
;;; is analogous to `vector'.

  (define (array shape . elts)
    (or (array:good-shape? shape)
	(error (string-append "array: shape " (array:thing->string shape)
			      " is not a shape")))
    (let ((size (array:size shape)))
      (let ((vector (list->vector elts)))
	(or (= (vector-length vector) size)
	    (error (string-append "array: an array of shape "
				  (array:shape-vector->string
				   (array:vector shape))
				  " has "
				  (number->string size)
				  " elements but got "
				  (number->string (vector-length vector))
				  " values: "
				  (array:list->string elts))))
	(array:make
	 vector
	 (if (= size 0)
	     (array:optimize-empty
	      (vector-ref (array:shape shape) 1))
	     (array:optimize
	      (array:make-index shape)
	      (vector-ref (array:shape shape) 1)))
	 (array:shape->vector shape)))))

;;; (array-rank array)
;;; returns the number of dimensions of `array'.

  (define (array-rank array)
    (quotient (vector-length (array:shape array)) 2))

;;; (array-start array k)
;;; returns the lower bound index of array along dimension k. This is
;;; the least valid index along that dimension if the dimension is not
;;; empty.

  (define (array-start array d)
    (vector-ref (array:shape array) (+ d d)))

;;; (array-end array k)
;;; returns the upper bound index of array along dimension k. This is
;;; not a valid index. If the dimension is empty, this is the same as
;;; the lower bound along it.

  (define (array-end array d)
    (vector-ref (array:shape array) (+ d d 1)))

;;; (share-array array shape proc)
;;; makes an array that shares elements of `array' at shape `shape'.
;;; The arguments to `proc' are indices of the result.  The values of
;;; `proc' are indices of `array'.

;;; Todo: in the error message, should recognise the mapping and show it.

  (define (share-array array subshape f)
    (or (array:good-shape? subshape)
	(error (string-append "share-array: shape "
			      (array:thing->string subshape)
			      " is not a shape")))
    (let ((subsize (array:size subshape)))
      (or (array:good-share? subshape subsize f (array:shape array))
	  (error (string-append "share-array: subshape "
				(array:shape-vector->string
				 (array:vector subshape))
				" does not map into supershape "
				(array:shape-vector->string
				 (array:shape array))
				" under mapping "
				(array:map->string
				 f
				 (vector-ref (array:shape subshape) 1)))))    
      (let ((g (array:index array)))
	(array:make
	 (array:vector array)
	 (if (= subsize 0)
	     (array:optimize-empty
	      (vector-ref (array:shape subshape) 1))
	     (array:optimize
	      (lambda ks
		(call-with-values
		 (lambda () (apply f ks))
		 (lambda ks (array:vector-index g ks))))
	      (vector-ref (array:shape subshape) 1)))
	 (array:shape->vector subshape)))))

;;; --- Hrmph ---

;;; (array:share/index! ...)
;;; reuses a user supplied index object when recognising the
;;; mapping. The mind balks at the very nasty side effect that
;;; exposes the implementation. So this is not in the spec.
;;; But letting index objects in at all creates a pressure
;;; to go the whole hog. Arf.

;;; Use array:optimize-empty for an empty array to get a
;;; clearly invalid vector index.

;;; Surely it's perverse to use an actor for index here? But
;;; the possibility is provided for completeness.

  (define (array:share/index! array subshape proc index)
    (array:make
     (array:vector array)
     (if (= (array:size subshape) 0)
	 (array:optimize-empty
	  (quotient (vector-length (array:shape array)) 2))
	 ((if (vector? index)
	      array:optimize/vector
	      array:optimize/actor)
	  (lambda (subindex)
	    (let ((superindex (proc subindex)))
	      (if (vector? superindex)
		  (array:index/vector
		   (quotient (vector-length (array:shape array)) 2)
		   (array:index array)
		   superindex)
		  (array:index/array
		   (quotient (vector-length (array:shape array)) 2)
		   (array:index array)
		   (array:vector superindex)
		   (array:index superindex)))))
	  index))
     (array:shape->vector subshape)))

  (define (array:optimize/vector f v)
    (let ((r (vector-length v)))
      (do ((k 0 (+ k 1)))
	  ((= k r))
	(vector-set! v k 0))
      (let ((n0 (f v))
	    (cs (make-vector (+ r 1)))
	    (apply (array:applier-to-vector (+ r 1))))
	(vector-set! cs 0 n0)
	(let wok ((k 0))
	  (if (< k r)
	      (let ((k1 (+ k 1)))
		(vector-set! v k 1)
		(let ((nk (- (f v) n0)))
		  (vector-set! v k 0)
		  (vector-set! cs k1 nk)
		  (wok k1)))))
	(apply (array:maker r) cs))))

  (define (array:optimize/actor f a)
    (let ((r (array-end a 0))
	  (v (array:vector a))
	  (i (array:index a)))
      (do ((k 0 (+ k 1)))
	  ((= k r))
	(vector-set! v (array:actor-index i k) 0))
      (let ((n0 (f a))
	    (cs (make-vector (+ r 1)))
	    (apply (array:applier-to-vector (+ r 1))))
	(vector-set! cs 0 n0)
	(let wok ((k 0))
	  (if (< k r)
	      (let ((k1 (+ k 1))
		    (t (array:actor-index i k)))
		(vector-set! v t 1)
		(let ((nk (- (f a) n0)))
		  (vector-set! v t 0)
		  (vector-set! cs k1 nk)
		  (wok k1)))))
	(apply (array:maker r) cs))))

;;; --- Internals ---

  (define (array:shape->vector shape)
    (let ((idx (array:index shape))
	  (shv (array:vector shape))
	  (rnk (vector-ref (array:shape shape) 1)))
      (let ((vec (make-vector (* rnk 2))))
	(do ((k 0 (+ k 1)))
	    ((= k rnk)
	     vec)
	  (vector-set! vec (+ k k)
		       (vector-ref shv (array:shape-vector-index idx k 0)))
	  (vector-set! vec (+ k k 1)
		       (vector-ref shv (array:shape-vector-index idx k 1)))))))

;;; (array:size shape)
;;; returns the number of elements in arrays of shape `shape'.

  (define (array:size shape)
    (let ((idx (array:index shape))
	  (shv (array:vector shape))
	  (rnk (vector-ref (array:shape shape) 1)))
      (do   ((k 0 (+ k 1))
	     (s 1 (* s
		     (- (vector-ref shv (array:shape-vector-index idx k 1))
			(vector-ref shv (array:shape-vector-index idx k 0))))))
	  ((= k rnk) s))))

;;; (array:make-index shape)
;;; returns an index function for arrays of shape `shape'. This is a
;;; runtime composition of several variable arity procedures, to be
;;; passed to array:optimize for recognition as an affine function of
;;; as many variables as there are dimensions in arrays of this shape.

  (define (array:make-index shape)
    (let ((idx (array:index shape))
	  (shv (array:vector shape))
	  (rnk (vector-ref (array:shape shape) 1)))
      (do ((f (lambda () 0)
	      (lambda (k . ks)
		(+ (* s (- k (vector-ref
			      shv
			      (array:shape-vector-index idx (- j 1) 0))))
		   (apply f ks))))
	   (s 1 (* s (- (vector-ref
			 shv
			 (array:shape-vector-index idx (- j 1) 1))
			(vector-ref
			 shv
			 (array:shape-vector-index idx (- j 1) 0)))))
	   (j rnk (- j 1)))
	  ((= j 0)
	   f))))


;;; --- Error checking ---

;;; (array:good-shape? shape)
;;; returns true if `shape' is an array of the right shape and its
;;; elements are exact integers that pairwise bound intervals `[lo..hi)´.

  (define (array:good-shape? shape)
    (and (array:array? shape)
	 (let ((u (array:shape shape))
	       (v (array:vector shape))
	       (x (array:index shape)))
	   (and (= (vector-length u) 4)
		(= (vector-ref u 0) 0)
		(= (vector-ref u 2) 0)
		(= (vector-ref u 3) 2))
	   (let ((p (vector-ref u 1)))
	     (do ((k 0 (+ k 1))
		  (true #t (let ((lo (vector-ref
				      v
				      (array:shape-vector-index x k 0)))
				 (hi (vector-ref
				      v
				      (array:shape-vector-index x k 1))))
			     (and true
				  (integer? lo)
				  (exact? lo)
				  (integer? hi)
				  (exact? hi)
				  (<= lo hi)))))
		 ((= k p) true))))))

;;; (array:good-share? subv subsize mapping superv)
;;; returns true if the extreme indices in the subshape vector map
;;; into the bounds in the supershape vector.

;;; If some interval in `subv' is empty, then `subv' is empty and its
;;; image under `f' is empty and it is trivially alright.  One must
;;; not call `f', though.

  (define (array:good-share? subshape subsize f super)
    (or (zero? subsize)
	(letrec
	    ((sub (array:vector subshape))
	     (dex (array:index subshape))
	     (ck (lambda (k ks)
		   (if (zero? k)
		       (call-with-values
			(lambda () (apply f ks))
			(lambda qs (array:good-indices? qs super)))
		       (and (ck (- k 1)
				(cons (vector-ref
				       sub
				       (array:shape-vector-index
					dex
					(- k 1)
					0))
				      ks))
			    (ck (- k 1)
				(cons (- (vector-ref
					  sub
					  (array:shape-vector-index
					   dex
					   (- k 1)
					   1))
					 1)
				      ks)))))))
	  (let ((rnk (vector-ref (array:shape subshape) 1)))
	    (or (array:unchecked-share-depth? rnk)
		(ck rnk '()))))))

;;; Check good-share on 10 dimensions at most. The trouble is,
;;; the cost of this check is exponential in the number of dimensions.

  (define (array:unchecked-share-depth? rank)
    (if (> rank 10)
	(begin
	  (display `(warning: unchecked depth in share:
			      ,rank subdimensions))
	  (newline)
	  #t)
	#f))

;;; (array:check-indices caller indices shape-vector)
;;; (array:check-indices.o caller indices shape-vector)
;;; (array:check-index-vector caller index-vector shape-vector)
;;; return if the index is in bounds, else signal error.
;;;
;;; Shape-vector is the internal representation, with
;;; b and e for dimension k at 2k and 2k + 1.

  (define (array:check-indices who ks shv)
    (or (array:good-indices? ks shv)
	(error (array:not-in who ks shv))))

  (define (array:check-indices.o who ks shv)
    (or (array:good-indices.o? ks shv)
	(error (array:not-in who (reverse (cdr (reverse ks))) shv))))

  (define (array:check-index-vector who ks shv)
    (or (array:good-index-vector? ks shv)
	(error (array:not-in who (vector->list ks) shv))))

  (define (array:check-index-actor who ks shv)
    (let ((shape (array:shape ks)))
      (or (and (= (vector-length shape) 2)
	       (= (vector-ref shape 0) 0))
	  (error "not an actor"))
      (or (array:good-index-actor?
	   (vector-ref shape 1)
	   (array:vector ks)
	   (array:index ks)
	   shv)
	  (array:not-in who (do ((k (vector-ref shape 1) (- k 1))
				 (m '() (cons (vector-ref
					       (array:vector ks)
					       (array:actor-index
						(array:index ks)
						(- k 1)))
					      m)))
				((= k 0) m))
			shv))))

  (define (array:good-indices? ks shv)
    (let ((d2 (vector-length shv)))
      (do ((kp ks (if (pair? kp)
                      (cdr kp)))
           (k 0 (+ k 2))
           (true #t (and true (pair? kp)
                         (array:good-index? (car kp) shv k))))
	  ((= k d2)
	   (and true (null? kp))))))

  (define (array:good-indices.o? ks.o shv)
    (let ((d2 (vector-length shv)))
      (do   ((kp ks.o (if (pair? kp)
			  (cdr kp)))
	     (k 0 (+ k 2))
	     (true #t (and true (pair? kp)
			   (array:good-index? (car kp) shv k))))
	  ((= k d2)
	   (and true (pair? kp) (null? (cdr kp)))))))

  (define (array:good-index-vector? ks shv)
    (let ((r2 (vector-length shv)))
      (and (= (* 2 (vector-length ks)) r2)
	   (do ((j 0 (+ j 1))
		(k 0 (+ k 2))
		(true #t (and true
			      (array:good-index? (vector-ref ks j) shv k))))
	       ((= k r2) true)))))

  (define (array:good-index-actor? r v i shv)
    (and (= (* 2 r) (vector-length shv))
	 (do ((j 0 (+ j 1))
	      (k 0 (+ k 2))
	      (true #t (and true
			    (array:good-index? (vector-ref
						v
						(array:actor-index i j))
					       shv
					       k))))
	     ((= j r) true))))

;;; (array:good-index? index shape-vector 2d)
;;; returns true if index is within bounds for dimension 2d/2.

  (define (array:good-index? w shv k)
    (and (integer? w)
	 (exact? w)
	 (<= (vector-ref shv k) w)
	 (< w (vector-ref shv (+ k 1)))))

  (define (array:not-in who ks shv)
    (let ((index (array:list->string ks))
	  (bounds (array:shape-vector->string shv)))
      (error (string-append who
			    ": index " index
			    " not in bounds " bounds))))

  (define (array:list->string ks)
    (do ((index "" (string-append index (array:thing->string (car ks)) " "))
	 (ks ks (cdr ks)))
	((null? ks) index)))

  (define (array:shape-vector->string shv)
    (do ((bounds "" (string-append bounds
				   "["
				   (number->string (vector-ref shv t))
				   ".."
				   (number->string (vector-ref shv (+ t 1)))
				   ")"
				   " "))
	 (t 0 (+ t 2)))
	((= t (vector-length shv)) bounds)))

  (define (array:thing->string thing)
    (cond
     ((number? thing) (number->string thing))
     ((symbol? thing) (string-append "#<symbol>" (symbol->string thing)))
     ((char? thing) "#<char>")
     ((string? thing) "#<string>")
     ((list? thing) (string-append "#" (number->string (length thing))
				   "<list>"))
     
     ((pair? thing) "#<pair>")
     ((array? thing) "#<array>")
     ((vector? thing) (string-append "#" (number->string
					  (vector-length thing))
				     "<vector>"))
     ((procedure? thing) "#<procedure>")
     (else
      (case thing
	((()) "()")
	((#t) "#t")
	((#f) "#f")
	(else
	 "#<whatsit>")))))

;;; And to grok an affine map, vector->vector type. Column k of arr
;;; will contain coefficients n0 ... nm of 1 k1 ... km for kth value.
;;; 
;;; These are for the error message when share fails.

  (define (array:index-ref ind k)
    (if (vector? ind)
	(vector-ref ind k)
	(vector-ref
	 (array:vector ind)
	 (array:actor-index (array:index ind) k))))

  (define (array:index-set! ind k o)
    (if (vector? ind)
	(vector-set! ind k o)
	(vector-set!
	 (array:vector ind)
	 (array:actor-index (array:index ind) k)
	 o)))

  (define (array:index-length ind)
    (if (vector? ind)
	(vector-length ind)
	(vector-ref (array:shape ind) 1)))

  (define (array:map->string proc r)
    (let* ((m (array:grok/arguments proc r))
	   (s (vector-ref (array:shape m) 3)))
      (do ((i "" (string-append i c "k" (number->string k)))
	   (c "" ", ")
	   (k 1 (+ k 1)))
	  ((< r k)
	   (do ((o "" (string-append o c (array:map-column->string m r k)))
		(c "" ", ")
		(k 0 (+ k 1)))
	       ((= k s)
		(string-append i " => " o)))))))

  (define (array:map-column->string m r k)
    (let ((v (array:vector m))
	  (i (array:index m)))
      (let ((n0 (vector-ref v (array:vector-index i (list 0 k)))))
	(let wok ((j 1)
		  (e (if (= n0 0) "" (number->string n0))))
	  (if (<= j r)
	      (let ((nj (vector-ref v (array:vector-index i (list j k)))))
		(if (= nj 0)
		    (wok (+ j 1) e)
		    (let* ((nj (if (= nj 1) ""
				   (if (= nj -1) "-"
				       (string-append (number->string nj)
						      " "))))
			   (njkj (string-append nj "k" (number->string j))))
		      (if (string=? e "")
			  (wok (+ j 1) njkj)
			  (wok (+ j 1) (string-append e " + " njkj))))))
	      (if (string=? e "") "0" e))))))

  (define (array:grok/arguments proc r)
    (array:grok/index!
     (lambda (vec)
       (call-with-values
	(lambda ()
	  (array:apply-to-vector r proc vec))
	vector))
     (make-vector r)))

  (define (array:grok/index! proc in)
    (let ((m (array:index-length in)))
      (do ((k 0 (+ k 1)))
	  ((= k m))
	(array:index-set! in k 0))
      (let* ((n0 (proc in))
	     (n (array:index-length n0)))
	(let ((arr (make-array (shape 0 (+ m 1) 0 n))))	; (*)
	  (do ((k 0 (+ k 1)))
	      ((= k n))
	    (array-set! arr 0 k (array:index-ref n0 k))) ; (**)
	  (do ((j 0 (+ j 1)))
	      ((= j m))
	    (array:index-set! in j 1)
	    (let ((nj (proc in)))
	      (array:index-set! in j 0)
	      (do ((k 0 (+ k 1)))
		  ((= k n))
		(array-set! arr (+ j 1) k (- (array:index-ref nj k) ; (**)
					     (array:index-ref n0 k))))))
	  arr))))
  ;; (*)  Should not use `make-array' and `shape' here
  ;; (**) Should not use `array-set!' here
  ;; Should use something internal to the library instead: either lower
  ;; level code (preferable but complex) or alternative names to these same.

  ;; The array library:
  (include "arlib.scm")
  
  )  

  