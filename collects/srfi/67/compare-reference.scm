; Copyright (c) 2005 Sebastian Egner and Jens Axel S{\o}gaard.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ``Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
; 
; Compare procedures SRFI (reference implementation)
; Sebastian.Egner@philips.com, Jensaxel@soegaard.net
; history of this file:
;   SE, 14-Oct-2004: first version
;   SE, 18-Oct-2004: 1st redesign: axioms for 'compare function'
;   SE, 29-Oct-2004: 2nd redesign: higher order reverse/map/refine/unite
;   SE,  2-Nov-2004: 3rd redesign: macros cond/refine-compare replace h.o.f's
;   SE, 10-Nov-2004: (im,re) replaced by (re,im) in complex-compare
;   SE, 11-Nov-2004: case-compare by case (not by cond); select-compare added
;   SE, 12-Jan-2005: pair-compare-cdr
;   SE, 15-Feb-2005: stricter typing for compare-<type>; pairwise-not=?
;   SE, 16-Feb-2005: case-compare -> if-compare -> if3; <? </<? chain<? etc.
;   JS, 24-Feb-2005: selection-compare added
;   SE, 25-Feb-2005: selection-compare -> kth-largest modified; if<? etc.
;   JS, 28-Feb-2005: kth-largest modified - is "stable" now
;   SE, 28-Feb-2005: simplified pairwise-not=?/kth-largest; min/max debugged
;   SE, 07-Apr-2005: compare-based type checks made explicit
;   SE, 18-Apr-2005: added (rel? compare) and eq?-test
;   SE, 16-May-2005: naming convention changed; compare-by< etc. optional x y

; =============================================================================

; Reference Implementation
; ========================
;
; in R5RS (including hygienic macros)
;  + SRFI-16 (case-lambda) 
;  + SRFI-23 (error) 
;  + SRFI-27 (random-integer)

; Implementation remarks:
;   * In general, the emphasis of this implementation is on correctness
;     and portability, not on efficiency.
;   * Variable arity procedures are expressed in terms of case-lambda
;     in the hope that this will produce efficient code for the case
;     where the arity is statically known at the call site.
;   * In procedures that are required to type-check their arguments,
;     we use (compare x x) for executing extra checks. This relies on
;     the assumption that eq? is used to catch this case quickly.
;   * Care has been taken to reference comparison procedures of R5RS
;     only at the time the operations here are being defined. This
;     makes it possible to redefine these operations, if need be.
;   * For the sake of efficiency, some inlining has been done by hand.
;     This is mainly expressed by macros producing defines.
;   * Identifiers of the form compare:<something> are private.
;
; Hints for low-level implementation:
;   * The basis of this SRFI are the atomic compare procedures, 
;     i.e. boolean-compare, char-compare, etc. and the conditionals
;     if3, if=?, if<? etc., and default-compare. These should make
;     optimal use of the available type information.
;   * For the sake of speed, the reference implementation does not
;     use a LET to save the comparison value c for the ERROR call.
;     This can be fixed in a low-level implementation at no cost.
;   * Type-checks based on (compare x x) are made explicit by the
;     expression (compare:check result compare x ...).
;   * Eq? should  can used to speed up built-in compare procedures,
;     but it can only be used after type-checking at least one of
;     the arguments.

(define (compare:checked result compare . args)
  (for-each (lambda (x) (compare x x)) args)
  result)


; 3-sided conditional

(define-syntax if3
  (syntax-rules ()
    ((if3 c less equal greater)
     (case c
       ((-1) less)
       (( 0) equal)
       (( 1) greater)
       (else (error "comparison value not in {-1,0,1}"))))))


; 2-sided conditionals for comparisons

(define-syntax compare:if-rel?
  (syntax-rules ()
    ((compare:if-rel? c-cases a-cases c consequence)
     (compare:if-rel? c-cases a-cases c consequence (if #f #f)))
    ((compare:if-rel? c-cases a-cases c consequence alternate)
     (case c
       (c-cases consequence)
       (a-cases alternate)
       (else    (error "comparison value not in {-1,0,1}"))))))

(define-syntax if=?
  (syntax-rules ()
    ((if=? arg ...)
     (compare:if-rel? (0) (-1 1) arg ...))))

(define-syntax if<?
  (syntax-rules ()
    ((if<? arg ...)
     (compare:if-rel? (-1) (0 1) arg ...))))

(define-syntax if>?
  (syntax-rules ()
    ((if>? arg ...)
     (compare:if-rel? (1) (-1 0) arg ...))))

(define-syntax if<=?
  (syntax-rules ()
    ((if<=? arg ...)
     (compare:if-rel? (-1 0) (1) arg ...))))

(define-syntax if>=?
  (syntax-rules ()
    ((if>=? arg ...)
     (compare:if-rel? (0 1) (-1) arg ...))))

(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? arg ...)
     (compare:if-rel? (-1 1) (0) arg ...))))


; predicates from compare procedures

(define-syntax compare:define-rel?
  (syntax-rules ()
    ((compare:define-rel? rel? if-rel?)
     (define rel?
       (case-lambda
	(()        (lambda (x y) (if-rel? (default-compare x y) #t #f)))
	((compare) (lambda (x y) (if-rel? (compare         x y) #t #f)))
	((x y)                   (if-rel? (default-compare x y) #t #f))
	((compare x y)
	 (if (procedure? compare)
	     (if-rel? (compare x y) #t #f)
	     (error "not a procedure (Did you mean rel/rel??): " compare))))))))

(compare:define-rel? =?    if=?)
(compare:define-rel? <?    if<?)
(compare:define-rel? >?    if>?)
(compare:define-rel? <=?   if<=?)
(compare:define-rel? >=?   if>=?)
(compare:define-rel? not=? if-not=?)


; chains of length 3

(define-syntax compare:define-rel1/rel2?
  (syntax-rules ()
    ((compare:define-rel1/rel2? rel1/rel2? if-rel1? if-rel2?)
     (define rel1/rel2?
       (case-lambda
	(()
	 (lambda (x y z)
	   (if-rel1? (default-compare x y)
		     (if-rel2? (default-compare y z) #t #f)
		     (compare:checked #f default-compare z))))
	((compare)
	 (lambda (x y z)
	   (if-rel1? (compare x y)
		(if-rel2? (compare y z) #t #f)
		(compare:checked #f compare z))))
	((x y z)
	 (if-rel1? (default-compare x y)
	       (if-rel2? (default-compare y z) #t #f)
	       (compare:checked #f default-compare z)))
	((compare x y z)
	 (if-rel1? (compare x y)
	       (if-rel2? (compare y z) #t #f)
	       (compare:checked #f compare z))))))))

(compare:define-rel1/rel2? </<?   if<?  if<?)
(compare:define-rel1/rel2? </<=?  if<?  if<=?)
(compare:define-rel1/rel2? <=/<?  if<=? if<?)
(compare:define-rel1/rel2? <=/<=? if<=? if<=?)
(compare:define-rel1/rel2? >/>?   if>?  if>?)
(compare:define-rel1/rel2? >/>=?  if>?  if>=?)
(compare:define-rel1/rel2? >=/>?  if>=? if>?)
(compare:define-rel1/rel2? >=/>=? if>=? if>=?)


; chains of arbitrary length

(define-syntax compare:define-chain-rel?
  (syntax-rules ()
    ((compare:define-chain-rel? chain-rel? if-rel?)
     (define chain-rel?
       (case-lambda
	((compare)
	 #t)
	((compare x1)
	 (compare:checked #t compare x1))
	((compare x1 x2)
	 (if-rel? (compare x1 x2) #t #f))
	((compare x1 x2 x3)
	 (if-rel? (compare x1 x2)
		  (if-rel? (compare x2 x3) #t #f)
		  (compare:checked #f compare x3)))
	((compare x1 x2 . x3+)
	 (if-rel? (compare x1 x2)
		  (let chain? ((head x2) (tail x3+))
		    (if (null? tail)
			#t
			(if-rel? (compare head (car tail))
				 (chain? (car tail) (cdr tail))
				 (apply compare:checked #f 
					compare (cdr tail)))))
		  (apply compare:checked #f compare x3+))))))))

(compare:define-chain-rel? chain=?  if=?)
(compare:define-chain-rel? chain<?  if<?)
(compare:define-chain-rel? chain>?  if>?)
(compare:define-chain-rel? chain<=? if<=?)
(compare:define-chain-rel? chain>=? if>=?)


; pairwise inequality

(define pairwise-not=?
  (let ((= =) (<= <=))
    (case-lambda
      ((compare)
       #t)
      ((compare x1)
       (compare:checked #t compare x1))
      ((compare x1 x2)
       (if-not=? (compare x1 x2) #t #f))
      ((compare x1 x2 x3)
       (if-not=? (compare x1 x2)
                 (if-not=? (compare x2 x3)
                           (if-not=? (compare x1 x3) #t #f)
                           #f)
		 (compare:checked #f compare x3)))
      ((compare . x1+)
       (let unequal? ((x x1+) (n (length x1+)) (unchecked? #t))
         (if (< n 2)
	     (if (and unchecked? (= n 1))
		 (compare:checked #t compare (car x))
		 #t)
             (let* ((i-pivot (random-integer n))
                    (x-pivot (list-ref x i-pivot)))
               (let split ((i 0) (x x) (x< '()) (x> '()))
                 (if (null? x)
                     (and (unequal? x< (length x<) #f)
                          (unequal? x> (length x>) #f))
                     (if (= i i-pivot)
                         (split (+ i 1) (cdr x) x< x>)
                         (if3 (compare (car x) x-pivot)
                              (split (+ i 1) (cdr x) (cons (car x) x<) x>)
			      (if unchecked?
				  (apply compare:checked #f compare (cdr x))
				  #f)
                              (split (+ i 1) (cdr x) x< (cons (car x) x>)))))))))))))


; min/max

(define min-compare
  (case-lambda
    ((compare x1)
     (compare:checked x1 compare x1))
    ((compare x1 x2)
     (if<=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3) x1 x3)
            (if<=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3)
                   (if<=? (compare x1 x4) x1 x4)
                   (if<=? (compare x3 x4) x3 x4))
            (if<=? (compare x2 x3)
                   (if<=? (compare x2 x4) x2 x4)
                   (if<=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let min ((xmin (if<=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmin
           (min (if<=? (compare xmin (car xs)) xmin (car xs))
                (cdr xs)))))))

(define max-compare
  (case-lambda
    ((compare x1)
     (compare:checked x1 compare x1))
    ((compare x1 x2)
     (if>=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3) x1 x3)
            (if>=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3)
                   (if>=? (compare x1 x4) x1 x4)
                   (if>=? (compare x3 x4) x3 x4))
            (if>=? (compare x2 x3)
                   (if>=? (compare x2 x4) x2 x4)
                   (if>=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let max ((xmax (if>=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmax
           (max (if>=? (compare xmax (car xs)) xmax (car xs))
                (cdr xs)))))))


; kth-largest

(define kth-largest
  (let ((= =) (< <))
    (case-lambda
      ((compare k x0)
       (case (modulo k 1)
         ((0)  (compare:checked x0 compare x0))
         (else (error "bad index" k))))
      ((compare k x0 x1)
       (case (modulo k 2)
         ((0) (if<=? (compare x0 x1) x0 x1))
         ((1) (if<=? (compare x0 x1) x1 x0))
         (else (error "bad index" k))))
      ((compare k x0 x1 x2)
       (case (modulo k 3)
         ((0) (if<=? (compare x0 x1)
                     (if<=? (compare x0 x2) x0 x2)
                     (if<=? (compare x1 x2) x1 x2)))
         ((1) (if3 (compare x0 x1)
                   (if<=? (compare x1 x2)
                          x1
                          (if<=? (compare x0 x2) x2 x0))
                   (if<=? (compare x0 x2) x1 x0)
                   (if<=? (compare x0 x2)
                          x0
                          (if<=? (compare x1 x2) x2 x1))))
         ((2) (if<=? (compare x0 x1)
                     (if<=? (compare x1 x2) x2 x1)
                     (if<=? (compare x0 x2) x2 x0)))
         (else (error "bad index" k))))
      ((compare k x0 . x1+) ; |x1+| >= 1
       (if (not (and (integer? k) (exact? k)))
           (error "bad index" k))
       (let ((n (+ 1 (length x1+))))
         (let kth ((k   (modulo k n))
                   (n   n)  ; = |x|
                   (rev #t) ; are x<, x=, x> reversed?
                   (x   (cons x0 x1+)))
           (let ((pivot (list-ref x (random-integer n))))
             (let split ((x x) (x< '()) (n< 0) (x= '()) (n= 0) (x> '()) (n> 0))
               (if (null? x)
                   (cond
                     ((< k n<)
                      (kth k n< (not rev) x<))
                     ((< k (+ n< n=))
                      (if rev
                          (list-ref x= (- (- n= 1) (- k n<)))
                          (list-ref x= (- k n<))))
                     (else
                      (kth (- k (+ n< n=)) n> (not rev) x>)))
                   (if3 (compare (car x) pivot)
                        (split (cdr x) (cons (car x) x<) (+ n< 1) x= n= x> n>)
                        (split (cdr x) x< n< (cons (car x) x=) (+ n= 1) x> n>)
                        (split (cdr x) x< n< x= n= (cons (car x) x>) (+ n> 1))))))))))))


; compare functions from predicates

(define compare-by<
  (case-lambda
   ((lt)     (lambda (x y) (if (lt x y) -1 (if (lt y x)  1 0))))
   ((lt x y)               (if (lt x y) -1 (if (lt y x)  1 0)))))

(define compare-by>
  (case-lambda
   ((gt)     (lambda (x y) (if (gt x y) 1 (if (gt y x)  -1 0))))
   ((gt x y)               (if (gt x y) 1 (if (gt y x)  -1 0)))))

(define compare-by<=
  (case-lambda
   ((le)     (lambda (x y) (if (le x y) (if (le y x) 0 -1) 1)))
   ((le x y)               (if (le x y) (if (le y x) 0 -1) 1))))

(define compare-by>=
  (case-lambda
   ((ge)     (lambda (x y) (if (ge x y) (if (ge y x) 0 1) -1)))
   ((ge x y)               (if (ge x y) (if (ge y x) 0 1) -1))))

(define compare-by=/<
  (case-lambda
   ((eq lt)     (lambda (x y) (if (eq x y) 0 (if (lt x y) -1 1))))
   ((eq lt x y)               (if (eq x y) 0 (if (lt x y) -1 1)))))

(define compare-by=/>
  (case-lambda
   ((eq gt)     (lambda (x y) (if (eq x y) 0 (if (gt x y) 1 -1))))
   ((eq gt x y)               (if (eq x y) 0 (if (gt x y) 1 -1)))))

; refine and extend construction

(define-syntax refine-compare
  (syntax-rules ()
    ((refine-compare)
     0)
    ((refine-compare c1)
     c1)
    ((refine-compare c1 c2 cs ...)
     (if3 c1 -1 (refine-compare c2 cs ...) 1))))

(define-syntax select-compare
  (syntax-rules (else)
    ((select-compare x y clause ...)
     (let ((x-val x) (y-val y))
       (select-compare (x-val y-val clause ...))))
    ; used internally: (select-compare (x y clause ...))
    ((select-compare (x y))
     0)
    ((select-compare (x y (else c ...)))
     (refine-compare c ...))
    ((select-compare (x y (t? c ...) clause ...))
     (let ((t?-val t?))
       (let ((tx (t?-val x)) (ty (t?-val y)))
         (if tx
             (if ty (refine-compare c ...) -1)
             (if ty 1 (select-compare (x y clause ...)))))))))

(define-syntax cond-compare
  (syntax-rules (else)
    ((cond-compare)
     0)
    ((cond-compare (else cs ...))
     (refine-compare cs ...))
    ((cond-compare ((tx ty) cs ...) clause ...)
     (let ((tx-val tx) (ty-val ty))
       (if tx-val
           (if ty-val (refine-compare cs ...) -1)
           (if ty-val 1 (cond-compare clause ...)))))))


; R5RS atomic types

(define-syntax compare:type-check
  (syntax-rules ()
    ((compare:type-check type? type-name x)
     (if (not (type? x))
         (error (string-append "not " type-name ":") x)))
    ((compare:type-check type? type-name x y)
     (begin (compare:type-check type? type-name x)
            (compare:type-check type? type-name y)))))

(define-syntax compare:define-by=/<
  (syntax-rules ()
    ((compare:define-by=/< compare = < type? type-name)
     (define compare
       (let ((= =) (< <))
	 (lambda (x y)
	   (if (type? x)
	       (if (eq? x y)
		   0
		   (if (type? y)
		       (if (= x y) 0 (if (< x y) -1 1))
		       (error (string-append "not " type-name ":") y)))
	       (error (string-append "not " type-name ":") x))))))))

(define (boolean-compare x y)
  (compare:type-check boolean? "boolean" x y)
  (if x (if y 0 1) (if y -1 0)))

(compare:define-by=/< char-compare char=? char<? char? "char")

(compare:define-by=/< char-compare-ci char-ci=? char-ci<? char? "char")

(compare:define-by=/< string-compare string=? string<? string? "string")

(compare:define-by=/< string-compare-ci string-ci=? string-ci<? string? "string")

(define (symbol-compare x y)
  (compare:type-check symbol? "symbol" x y)
  (string-compare (symbol->string x) (symbol->string y)))

(compare:define-by=/< integer-compare = < integer? "integer")

(compare:define-by=/< rational-compare = < rational? "rational")

(compare:define-by=/< real-compare = < real? "real")

(define (complex-compare x y)
  (compare:type-check complex? "complex" x y)
  (if (and (real? x) (real? y))
      (real-compare x y)
      (refine-compare (real-compare (real-part x) (real-part y))
                      (real-compare (imag-part x) (imag-part y)))))

(define (number-compare x y)
  (compare:type-check number? "number" x y)
  (complex-compare x y))


; R5RS compound data structures: dotted pair, list, vector

(define (pair-compare-car compare)
  (lambda (x y)
    (compare (car x) (car y))))

(define (pair-compare-cdr compare)
  (lambda (x y)
    (compare (cdr x) (cdr y))))

(define pair-compare
  (case-lambda
    
    ; dotted pair
    ((pair-compare-car pair-compare-cdr x y)
     (refine-compare (pair-compare-car (car x) (car y))
                     (pair-compare-cdr (cdr x) (cdr y))))
    
    ; possibly improper lists
    ((compare x y)
     (cond-compare 
      (((null? x) (null? y)) 0)
      (((pair? x) (pair? y)) (compare              (car x) (car y))
                             (pair-compare compare (cdr x) (cdr y)))
      (else                  (compare x y))))
    
    ; for convenience
    ((x y)
     (pair-compare default-compare x y))))

(define list-compare
  (case-lambda
    ((compare x y empty? head tail)
     (cond-compare
      (((empty? x) (empty? y)) 0)
      (else (compare              (head x) (head y))
            (list-compare compare (tail x) (tail y) empty? head tail))))
    
    ; for convenience
    ((        x y empty? head tail)
     (list-compare default-compare x y empty? head tail))
    ((compare x y              )
     (list-compare compare         x y null? car   cdr))
    ((        x y              )
     (list-compare default-compare x y null? car   cdr))))

(define list-compare-as-vector
  (case-lambda
    ((compare x y empty? head tail)
     (refine-compare
      (let compare-length ((x x) (y y))
        (cond-compare
         (((empty? x) (empty? y)) 0)
         (else (compare-length (tail x) (tail y)))))
      (list-compare compare x y empty? head tail)))
    
    ; for convenience
    ((        x y empty? head tail)
     (list-compare-as-vector default-compare x y empty? head tail))
    ((compare x y              )
     (list-compare-as-vector compare         x y null?  car  cdr))
    ((        x y              )
     (list-compare-as-vector default-compare x y null?  car  cdr))))

(define vector-compare
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((n (size x)) (m (size y)))
         (refine-compare 
          (integer-compare n m)
          (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
            (if (= i n)
                0
                (refine-compare (compare (ref x i) (ref y i))
                                (compare-rest (+ i 1))))))))
      
      ; for convenience
      ((        x y size ref)
       (vector-compare default-compare x y size          ref))
      ((compare x y           )
       (vector-compare compare         x y vector-length vector-ref))
      ((        x y           )
       (vector-compare default-compare x y vector-length vector-ref)))))

(define vector-compare-as-list
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((nx (size x)) (ny (size y)))
         (let ((n (min nx ny)))
           (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
             (if (= i n)
                 (integer-compare nx ny)
                 (refine-compare (compare (ref x i) (ref y i))
                                 (compare-rest (+ i 1))))))))
      
      ; for convenience
      ((        x y size ref)
       (vector-compare-as-list default-compare x y size          ref))
      ((compare x y           )
       (vector-compare-as-list compare         x y vector-length vector-ref))
      ((        x y           )
       (vector-compare-as-list default-compare x y vector-length vector-ref)))))


; default compare

(define (default-compare x y)
  (select-compare 
   x y
   (null?    0)
   (pair?    (default-compare (car x) (car y))
	     (default-compare (cdr x) (cdr y)))
   (boolean? (boolean-compare x y))
   (char?    (char-compare    x y))
   (string?  (string-compare  x y))
   (symbol?  (symbol-compare  x y))
   (number?  (number-compare  x y))
   (vector?  (vector-compare default-compare x y))
   (else (error "unrecognized type in default-compare" x y))))

; Note that we pass default-compare to compare-{pair,vector} explictly.
; This makes sure recursion proceeds with this default-compare, which 
; need not be the one in the lexical scope of compare-{pair,vector}.


; debug compare

(define (debug-compare c)
  
  (define (checked-value c x y)
    (let ((c-xy (c x y)))
      (if (or (eqv? c-xy -1) (eqv? c-xy 0) (eqv? c-xy 1))
          c-xy
          (error "compare value not in {-1,0,1}" c-xy (list c x y)))))
  
  (define (random-boolean)
    (zero? (random-integer 2)))
  
  (define q ; (u v w) such that u <= v, v <= w, and not u <= w
    '#(
       ;x < y   x = y   x > y   [x < z]
       0       0       0    ; y < z
               0    (z y x) (z y x) ; y = z
               0    (z y x) (z y x) ; y > z
               
               ;x < y   x = y   x > y   [x = z]
               (y z x) (z x y)    0    ; y < z
               (y z x)    0    (x z y) ; y = z
               0    (y x z) (x z y) ; y > z
               
               ;x < y   x = y   x > y   [x > z]
               (x y z) (x y z)    0    ; y < z
               (x y z) (x y z)    0    ; y = z
               0       0       0    ; y > z
               ))
  
  (let ((z? #f) (z #f)) ; stored element from previous call
    (lambda (x y)
      (let ((c-xx (checked-value c x x))
	    (c-yy (checked-value c y y))
	    (c-xy (checked-value c x y))
	    (c-yx (checked-value c y x)))
	(if (not (zero? c-xx))
	    (error "compare error: not reflexive" c x))
	(if (not (zero? c-yy))
	    (error "compare error: not reflexive" c y))
	(if (not (zero? (+ c-xy c-yx)))
	    (error "compare error: not anti-symmetric" c x y))
	(if z?
	    (let ((c-xz (checked-value c x z))
		  (c-zx (checked-value c z x))
		  (c-yz (checked-value c y z))
		  (c-zy (checked-value c z y)))
	      (if (not (zero? (+ c-xz c-zx)))
		  (error "compare error: not anti-symmetric" c x z))
	      (if (not (zero? (+ c-yz c-zy)))
		  (error "compare error: not anti-symmetric" c y z))
	      (let ((ijk (vector-ref q (+ c-xy (* 3 c-yz) (* 9 c-xz) 13))))
		(if (list? ijk)
		    (apply error
			   "compare error: not transitive"
			   c 
			   (map (lambda (i) (case i ((x) x) ((y) y) ((z) z)))
				ijk)))))
	    (set! z? #t))
	(set! z (if (random-boolean) x y)) ; randomized testing
	c-xy))))
