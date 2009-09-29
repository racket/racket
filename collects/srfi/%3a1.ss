#!r6rs
;;; SRFI-1 list-processing library 			-*- Scheme -*-
;;; Reference implementation
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

;;; This is a library of list- and pair-processing functions. I wrote it after
;;; carefully considering the functions provided by the libraries found in
;;; R4RS/R5RS Scheme, MIT Scheme, Gambit, RScheme, MzScheme, slib, Common
;;; Lisp, Bigloo, guile, T, APL and the SML standard basis. It is a pretty
;;; rich toolkit, providing a superset of the functionality found in any of
;;; the various Schemes I considered.

;;; This implementation is intended as a portable reference implementation
;;; for SRFI-1. See the porting notes below for more information.

; Ikarus porting begun by Abdulaziz Ghuloum, 
; and continued by Derick Eddington.

(library (srfi :1 lists)
  (export
    ;;; Exported:
    xcons #;tree-copy make-list list-tabulate list-copy 
    proper-list? circular-list? dotted-list? not-pair? null-list? list=
    circular-list length+
    iota
    first second third fourth fifth sixth seventh eighth ninth tenth
    car+cdr
    take       drop       
    take-right drop-right 
    take!      drop-right!
    split-at   split-at!
    last last-pair
    zip unzip1 unzip2 unzip3 unzip4 unzip5
    count
    append! append-reverse append-reverse! concatenate concatenate! 
    unfold       fold       pair-fold       reduce
    unfold-right            pair-fold-right reduce-right
    append-map append-map! map! pair-for-each filter-map map-in-order
    filter! partition! remove! 
    find-tail any every list-index
    take-while drop-while take-while!
    span break span! break!
    delete delete!
    alist-cons alist-copy
    delete-duplicates delete-duplicates!
    alist-delete alist-delete!
    reverse! 
    lset<= lset= lset-adjoin  
    lset-union  lset-intersection  lset-difference  lset-xor  
    lset-diff+intersection
    lset-union! lset-intersection! lset-difference! lset-xor!
    lset-diff+intersection!
    ;; re-exported:
    append assq assv caaaar caaadr caaar caadar caaddr
    caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
    car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar
    cddadr cddar cdddar cddddr cdddr cddr cdr cons cons*
    length list list-ref memq memv null? pair?
    reverse set-car! set-cdr!
    ;; different than R6RS:
    assoc filter find fold-right for-each map member partition remove)
  (import 
    (except (rnrs)
            assoc error filter find fold-right
            for-each map member partition remove)
    (rnrs mutable-pairs))

;;; 
;;; In principle, the following R4RS list- and pair-processing procedures
;;; are also part of this package's exports, although they are not defined
;;; in this file:
;;;   Primitives: cons pair? null? car cdr set-car! set-cdr!
;;;   Non-primitives: list length append reverse cadr ... cddddr list-ref
;;;                   memq memv assq assv
;;;   (The non-primitives are defined in this file, but commented out.)
;;;
;;; These R4RS procedures have extended definitions in SRFI-1 and are defined
;;; in this file:
;;;   map for-each member assoc
;;;
;;; The remaining two R4RS list-processing procedures are not included: 
;;;   list-tail (use drop)
;;;   list? (use proper-list?)


;;; A note on recursion and iteration/reversal:
;;; Many iterative list-processing algorithms naturally compute the elements
;;; of the answer list in the wrong order (left-to-right or head-to-tail) from
;;; the order needed to cons them into the proper answer (right-to-left, or
;;; tail-then-head). One style or idiom of programming these algorithms, then,
;;; loops, consing up the elements in reverse order, then destructively 
;;; reverses the list at the end of the loop. I do not do this. The natural
;;; and efficient way to code these algorithms is recursively. This trades off
;;; intermediate temporary list structure for intermediate temporary stack
;;; structure. In a stack-based system, this improves cache locality and
;;; lightens the load on the GC system. Don't stand on your head to iterate!
;;; Recurse, where natural. Multiple-value returns make this even more
;;; convenient, when the recursion/iteration has multiple state values.

;;; Porting:
;;; This is carefully tuned code; do not modify casually.
;;;   - It is careful to share storage when possible;
;;;   - Side-effecting code tries not to perform redundant writes.
;;; 
;;; That said, a port of this library to a specific Scheme system might wish
;;; to tune this code to exploit particulars of the implementation. 
;;; The single most important compiler-specific optimisation you could make
;;; to this library would be to add rewrite rules or transforms to:
;;; - transform applications of n-ary procedures (e.g. LIST=, CONS*, APPEND,
;;;   LSET-UNION) into multiple applications of a primitive two-argument 
;;;   variant.
;;; - transform applications of the mapping functions (MAP, FOR-EACH, FOLD, 
;;;   ANY, EVERY) into open-coded loops. The killer here is that these 
;;;   functions are n-ary. Handling the general case is quite inefficient,
;;;   requiring many intermediate data structures to be allocated and
;;;   discarded.
;;; - transform applications of procedures that take optional arguments
;;;   into calls to variants that do not take optional arguments. This
;;;   eliminates unnecessary consing and parsing of the rest parameter.
;;;
;;; These transforms would provide BIG speedups. In particular, the n-ary
;;; mapping functions are particularly slow and cons-intensive, and are good
;;; candidates for tuning. I have coded fast paths for the single-list cases,
;;; but what you really want to do is exploit the fact that the compiler
;;; usually knows how many arguments are being passed to a particular
;;; application of these functions -- they are usually explicitly called, not
;;; passed around as higher-order values. If you can arrange to have your
;;; compiler produce custom code or custom linkages based on the number of
;;; arguments in the call, you can speed these functions up a *lot*. But this
;;; kind of compiler technology no longer exists in the Scheme world as far as
;;; I can see.
;;;
;;; Note that this code is, of course, dependent upon standard bindings for
;;; the R5RS procedures -- i.e., it assumes that the variable CAR is bound
;;; to the procedure that takes the car of a list. If your Scheme 
;;; implementation allows user code to alter the bindings of these procedures
;;; in a manner that would be visible to these definitions, then there might
;;; be trouble. You could consider horrible kludgery along the lines of
;;;    (define fact 
;;;      (let ((= =) (- -) (* *))
;;;        (letrec ((real-fact (lambda (n) 
;;;                              (if (= n 0) 1 (* n (real-fact (- n 1)))))))
;;;          real-fact)))
;;; Or you could consider shifting to a reasonable Scheme system that, say,
;;; has a module system protecting code from this kind of lossage.
;;;
;;; This code does a fair amount of run-time argument checking. If your
;;; Scheme system has a sophisticated compiler that can eliminate redundant
;;; error checks, this is no problem. However, if not, these checks incur
;;; some performance overhead -- and, in a safe Scheme implementation, they
;;; are in some sense redundant: if we don't check to see that the PROC 
;;; parameter is a procedure, we'll find out anyway three lines later when
;;; we try to call the value. It's pretty easy to rip all this argument 
;;; checking code out if it's inappropriate for your implementation -- just
;;; nuke every call to CHECK-ARG.
;;;
;;; On the other hand, if you *do* have a sophisticated compiler that will
;;; actually perform soft-typing and eliminate redundant checks (Rice's systems
;;; being the only possible candidate of which I'm aware), leaving these checks 
;;; in can *help*, since their presence can be elided in redundant cases,
;;; and in cases where they are needed, performing the checks early, at
;;; procedure entry, can "lift" a check out of a loop. 
;;;
;;; Finally, I have only checked the properties that can portably be checked
;;; with R5RS Scheme -- and this is not complete. You may wish to alter
;;; the CHECK-ARG parameter checks to perform extra, implementation-specific
;;; checks, such as procedure arity for higher-order values.
;;;
;;; The code has only these non-R4RS dependencies:
;;;   A few calls to an ERROR procedure;
;;;   Uses of the R5RS multiple-value procedure VALUES and the m-v binding
;;;     RECEIVE macro (which isn't R5RS, but is a trivial macro).
;;;   Many calls to a parameter-checking procedure check-arg:
;;;    (define (check-arg pred val caller)
;;;      (let lp ((val val))
;;;        (if (pred val) val (lp (error "Bad argument" val pred caller)))))
  (define-syntax check-arg
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred val caller)
         (and (identifier? #'val) (identifier? #'caller))
         #'(unless (pred val)
             (assertion-violation 'caller "check-arg failed" val))])))

;;;   A few uses of the LET-OPTIONAL and :OPTIONAL macros for parsing
;;;     optional arguments.

;;;
;;; Most of these procedures use the NULL-LIST? test to trigger the
;;; base case in the inner loop or recursion. The NULL-LIST? function
;;; is defined to be a careful one -- it raises an error if passed a
;;; non-nil, non-pair value. The spec allows an implementation to use
;;; a less-careful implementation that simply defines NULL-LIST? to
;;; be NOT-PAIR?. This would speed up the inner loops of these procedures
;;; at the expense of having them silently accept dotted lists.

;;; A note on dotted lists:
;;; I, personally, take the view that the only consistent view of lists
;;; in Scheme is the view that *everything* is a list -- values such as
;;; 3 or "foo" or 'bar are simply empty dotted lists. This is due to the
;;; fact that Scheme actually has no true list type. It has a pair type,
;;; and there is an *interpretation* of the trees built using this type
;;; as lists.
;;;
;;; I lobbied to have these list-processing procedures hew to this
;;; view, and accept any value as a list argument. I was overwhelmingly
;;; overruled during the SRFI discussion phase. So I am inserting this
;;; text in the reference lib and the SRFI spec as a sort of "minority
;;; opinion" dissent.
;;;
;;; Many of the procedures in this library can be trivially redefined
;;; to handle dotted lists, just by changing the NULL-LIST? base-case
;;; check to NOT-PAIR?, meaning that any non-pair value is taken to be
;;; an empty list. For most of these procedures, that's all that is
;;; required.
;;;
;;; However, we have to do a little more work for some procedures that
;;; *produce* lists from other lists.  Were we to extend these procedures to
;;; accept dotted lists, we would have to define how they terminate the lists
;;; produced as results when passed a dotted list. I designed a coherent set
;;; of termination rules for these cases; this was posted to the SRFI-1
;;; discussion list. I additionally wrote an earlier version of this library
;;; that implemented that spec. It has been discarded during later phases of
;;; the definition and implementation of this library.
;;;
;;; The argument *against* defining these procedures to work on dotted
;;; lists is that dotted lists are the rare, odd case, and that by 
;;; arranging for the procedures to handle them, we lose error checking
;;; in the cases where a dotted list is passed by accident -- e.g., when
;;; the programmer swaps a two arguments to a list-processing function,
;;; one being a scalar and one being a list. For example,
;;;     (member '(1 3 5 7 9) 7)
;;; This would quietly return #f if we extended MEMBER to accept dotted
;;; lists.
;;;
;;; The SRFI discussion record contains more discussion on this topic.

(define (error . args)
  (if (and (<= 2 (length args)) (symbol? (car args)) (string? (cadr args)))
    (apply assertion-violation args)
    (apply assertion-violation "(library (srfi :1 lists))"
           "misuse of error procedure" args)))
  
;;; Constructors
;;;;;;;;;;;;;;;;

;;; Occasionally useful as a value to be passed to a fold or other
;;; higher-order procedure.
(define (xcons d a) (cons a d))

;;;; Recursively copy every cons.
;(define (tree-copy x)
;  (let recur ((x x))
;    (if (not (pair? x)) x
;	(cons (recur (car x)) (recur (cdr x))))))

;;; Make a list of length LEN.

(define (make-list len . maybe-elt)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		   ((null? (cdr maybe-elt)) (car maybe-elt))
		   (else (error 'make-list "Too many arguments"
				(cons len maybe-elt))))))
    (do ((i len (- i 1))
	 (ans '() (cons elt ans)))
	((<= i 0) ans))))


;(define (list . ans) ans)	; R4RS


;;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.

(define (list-tabulate len proc)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len list-tabulate)
  (check-arg procedure? proc list-tabulate)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

;;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
;;; (cons* a1) = a1	(cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
;;;
;;; (cons first (unfold not-pair? car cdr rest values))

;(define (cons* first . rest)
;  (let recur ((x first) (rest rest))
;    (if (pair? rest)
;       (cons x (recur (car rest) (cdr rest)))
;       x)))

;;; (unfold not-pair? car cdr lis values)

(define (list-copy lis)				
  (let recur ((lis lis))			
    (if (pair? lis)				
	(cons (car lis) (recur (cdr lis)))	
	lis)))					

;;; IOTA count [start step]	(start start+step ... start+(count-1)*step)

;;;(define (iota count . maybe-start+step)
;;;  (check-arg integer? count iota)
;;;  (if (< count 0) (error "Negative step count" iota count))
;;;  (let-optionals maybe-start+step ((start 0) (step 1))
;;;    (check-arg number? start iota)
;;;    (check-arg number? step iota)
;;;    (let ((last-val (+ start (* (- count 1) step))))
;;;      (do ((count count (- count 1))
;;;	   (val last-val (- val step))
;;;	   (ans '() (cons val ans)))
;;;	  ((<= count 0)  ans)))))
	  
;;; using case-lambda instead of let-optional
(define iota
  (case-lambda
    [(count) (iota count 0 1)]
    [(count start) (iota count start 1)]
    [(count start step)
     (check-arg integer? count iota)
     (if (< count 0) (error 'iota "Negative step count" count))
     (check-arg number? start iota)
     (check-arg number? step iota)
     (let ((last-val (+ start (* (- count 1) step))))
       (do ((count count (- count 1))
         (val last-val (- val step))
         (ans '() (cons val ans)))
        ((<= count 0)  ans)))]))
	

;;; I thought these were lovely, but the public at large did not share my
;;; enthusiasm...
;;; :IOTA to		(0 ... to-1)
;;; :IOTA from to	(from ... to-1)
;;; :IOTA from to step  (from from+step ...)

;;; IOTA: to		(1 ... to)
;;; IOTA: from to	(from+1 ... to)
;;; IOTA: from to step	(from+step from+2step ...)

;(define (%parse-iota-args arg1 rest-args proc)
;  (let ((check (lambda (n) (check-arg integer? n proc))))
;    (check arg1)
;    (if (pair? rest-args)
;	(let ((arg2 (check (car rest-args)))
;	      (rest (cdr rest-args)))
;	  (if (pair? rest)
;	      (let ((arg3 (check (car rest)))
;		    (rest (cdr rest)))
;		(if (pair? rest) (error "Too many parameters" proc arg1 rest-args)
;		    (values arg1 arg2 arg3)))
;	      (values arg1 arg2 1)))
;	(values 0 arg1 1))))
;
;(define (iota: arg1 . rest-args)
;  (receive (from to step) (%parse-iota-args arg1 rest-args iota:)
;    (let* ((numsteps (floor (/ (- to from) step)))
;	   (last-val (+ from (* step numsteps))))
;      (if (< numsteps 0) (error "Negative step count" iota: from to step))
;      (do ((steps-left numsteps (- steps-left 1))
;	   (val last-val (- val step))
;	   (ans '() (cons val ans)))
;	  ((<= steps-left 0) ans)))))
;
;
;(define (:iota arg1 . rest-args)
;  (receive (from to step) (%parse-iota-args arg1 rest-args :iota)
;    (let* ((numsteps (ceiling (/ (- to from) step)))
;	   (last-val (+ from (* step (- numsteps 1)))))
;      (if (< numsteps 0) (error "Negative step count" :iota from to step))
;      (do ((steps-left numsteps (- steps-left 1))
;	   (val last-val (- val step))
;	   (ans '() (cons val ans)))
;	  ((<= steps-left 0) ans)))))



(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))

;;; <proper-list> ::= ()			; Empty proper list
;;;		  |   (cons <x> <proper-list>)	; Proper-list pair
;;; Note that this definition rules out circular lists -- and this
;;; function is required to detect this case and return false.

(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))


;;; A dotted list is a finite list (possibly of length 0) terminated
;;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;;; is a dotted list of length 0.
;;;
;;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
;;;               |   (cons <x> <dotted-list>)	; Proper-list pair

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

(define (not-pair? x) (not (pair? x)))	; Inline me.

;;; This is a legal definition which is fast and sloppy:
;;;     (define null-list? not-pair?)
;;; but we'll provide a more careful one:
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error 'null-list? "argument out of domain" l))))
           

(define (list= elt= . lists)
  (or (null? lists) ; special case
      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b-orig (car others))
		  (others      (cdr others)))
	      (if (eq? list-a list-b-orig)	; EQ? => LIST=
		  (lp1 list-b-orig others)
		  (let lp2 ((list-a list-a) (list-b list-b-orig))
		    (if (null-list? list-a)
			(and (null-list? list-b)
			     (lp1 list-b-orig others))
			(and (not (null-list? list-b))
			     (elt= (car list-a) (car list-b))
			     (lp2 (cdr list-a) (cdr list-b)))))))))))
			


;;; R4RS, so commented out.
;(define (length x)			; LENGTH may diverge or
;  (let lp ((x x) (len 0))		; raise an error if X is
;    (if (pair? x)			; a circular list. This version
;        (lp (cdr x) (+ len 1))		; diverges.
;        len)))

(define (length+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))

(define (zip list1 . more-lists) (apply map list list1 more-lists))


;;; Selectors
;;;;;;;;;;;;;

;;; R4RS non-primitives:
;(define (caar   x) (car (car x)))
;(define (cadr   x) (car (cdr x)))
;(define (cdar   x) (cdr (car x)))
;(define (cddr   x) (cdr (cdr x)))
;
;(define (caaar  x) (caar (car x)))
;(define (caadr  x) (caar (cdr x)))
;(define (cadar  x) (cadr (car x)))
;(define (caddr  x) (cadr (cdr x)))
;(define (cdaar  x) (cdar (car x)))
;(define (cdadr  x) (cdar (cdr x)))
;(define (cddar  x) (cddr (car x)))
;(define (cdddr  x) (cddr (cdr x)))
;
;(define (caaaar x) (caaar (car x)))
;(define (caaadr x) (caaar (cdr x)))
;(define (caadar x) (caadr (car x)))
;(define (caaddr x) (caadr (cdr x)))
;(define (cadaar x) (cadar (car x)))
;(define (cadadr x) (cadar (cdr x)))
;(define (caddar x) (caddr (car x)))
;(define (cadddr x) (caddr (cdr x)))
;(define (cdaaar x) (cdaar (car x)))
;(define (cdaadr x) (cdaar (cdr x)))
;(define (cdadar x) (cdadr (car x)))
;(define (cdaddr x) (cdadr (cdr x)))
;(define (cddaar x) (cddar (car x)))
;(define (cddadr x) (cddar (cdr x)))
;(define (cdddar x) (cdddr (car x)))
;(define (cddddr x) (cdddr (cdr x)))


(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

;;; take & drop

(define (take lis k)
  (check-arg integer? k take)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (check-arg integer? k drop)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (take! lis k)
  (check-arg integer? k take!)
  (if (zero? k) '()
      (begin (set-cdr! (drop lis (- k 1)) '())
	     lis)))

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list, 
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.

(define (take-right lis k)
  (check-arg integer? k take-right)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (check-arg integer? k drop-right)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().
(define (drop-right! lis k)
  (check-arg integer? k drop-right!)
  (let ((lead (drop lis k)))
    (if (pair? lead)

	(let lp ((lag lis)  (lead (cdr lead)))	; Standard case
	  (if (pair? lead)
	      (lp (cdr lag) (cdr lead))
	      (begin (set-cdr! lag '())
		     lis)))

	'())))	; Special case dropping everything -- no cons to side-effect.

;(define (list-ref lis i) (car (drop lis i)))	; R4RS

;;; These use the APL convention, whereby negative indices mean 
;;; "from the right." I liked them, but they didn't win over the
;;; SRFI reviewers.
;;; K >= 0: Take and drop  K elts from the front of the list.
;;; K <= 0: Take and drop -K elts from the end   of the list.

;(define (take lis k)
;  (check-arg integer? k take)
;  (if (negative? k)
;      (list-tail lis (+ k (length lis)))
;      (let recur ((lis lis) (k k))
;	(if (zero? k) '()
;	    (cons (car lis)
;		  (recur (cdr lis) (- k 1)))))))
;
;(define (drop lis k)
;  (check-arg integer? k drop)
;  (if (negative? k)
;      (let recur ((lis lis) (nelts (+ k (length lis))))
;	(if (zero? nelts) '()
;	    (cons (car lis)
;		  (recur (cdr lis) (- nelts 1)))))
;      (list-tail lis k)))
;
;
;(define (take! lis k)
;  (check-arg integer? k take!)
;  (cond ((zero? k) '())
;	((positive? k)
;	 (set-cdr! (list-tail lis (- k 1)) '())
;	 lis)
;	(else (list-tail lis (+ k (length lis))))))
;
;(define (drop! lis k)
;  (check-arg integer? k drop!)
;  (if (negative? k)
;      (let ((nelts (+ k (length lis))))
;	(if (zero? nelts) '()
;	    (begin (set-cdr! (list-tail lis (- nelts 1)) '())
;		   lis)))
;      (list-tail lis k)))

(define-syntax receive
  (syntax-rules ()
    [(_ (id* ...) expr body body* ...)
     (let-values ([(id* ...) expr]) body body* ...)]))


(define (split-at x k)
  (check-arg integer? k split-at)
  (let recur ((lis x) (k k))
    (if (zero? k) (values '() lis)
	(receive (prefix suffix) (recur (cdr lis) (- k 1))
	  (values (cons (car lis) prefix) suffix)))))

(define (split-at! x k)
  (check-arg integer? k split-at!)
  (if (zero? k) (values '() x)
      (let* ((prev (drop x (- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev '())
	(values x suffix))))


(define (last lis) (car (last-pair lis)))

(define (last-pair lis)
  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))


;;; Unzippers -- 1 through 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unzip1 lis) (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis)	; Use NOT-PAIR? to handle
	(let ((elt (car lis)))			; dotted lists.
	  (receive (a b) (recur (cdr lis))
	    (values (cons (car  elt) a)
		    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c) (recur (cdr lis))
	    (values (cons (car   elt) a)
		    (cons (cadr  elt) b)
		    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d) (recur (cdr lis))
	    (values (cons (car    elt) a)
		    (cons (cadr   elt) b)
		    (cons (caddr  elt) c)
		    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d e) (recur (cdr lis))
	    (values (cons (car     elt) a)
		    (cons (cadr    elt) b)
		    (cons (caddr   elt) c)
		    (cons (cadddr  elt) d)
		    (cons (car (cddddr  elt)) e)))))))


;;; append! append-reverse append-reverse! concatenate concatenate!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
	(let ((first (car lists))
	      (rest (cdr lists)))
	  (if (not (pair? first)) (lp rest first)

	      ;; Now, do the splicing.
	      (let lp2 ((tail-cons (last-pair first))
			(rest rest))
		(if (pair? rest)
		    (let ((next (car rest))
			  (rest (cdr rest)))
		      (set-cdr! tail-cons next)
		      (lp2 (if (pair? next) (last-pair next) tail-cons)
			   rest))
		    first)))))))

;;; APPEND is R4RS.
;(define (append . lists)
;  (if (pair? lists)
;      (let recur ((list1 (car lists)) (lists (cdr lists)))
;        (if (pair? lists)
;            (let ((tail (recur (car lists) (cdr lists))))
;              (fold-right cons tail list1)) ; Append LIST1 & TAIL.
;            list1))
;      '()))

;(define (append-reverse rev-head tail) (fold cons tail rev-head))

;(define (append-reverse! rev-head tail)
;  (pair-fold (lambda (pair tail) (set-cdr! pair tail) pair)
;             tail
;             rev-head))

;;; Hand-inline the FOLD and PAIR-FOLD ops for speed.

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(let ((next-rev (cdr rev-head)))
	  (set-cdr! rev-head tail)
	  (lp next-rev rev-head)))))


(define (concatenate  lists) (reduce-right append  '() lists))
(define (concatenate! lists) (reduce-right append! '() lists))

;;; Fold/map internal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These little internal utilities are used by the general
;;; fold & mapper funs for the n-ary cases . It'd be nice if they got inlined.
;;; One the other hand, the n-ary cases are painfully inefficient as it is.
;;; An aggressive implementation should simply re-write these functions 
;;; for raw efficiency; I have written them for as much clarity, portability,
;;; and simplicity as can be achieved.
;;;
;;; I use the dreaded call/cc to do local aborts. A good compiler could
;;; handle this with extreme efficiency. An implementation that provides
;;; a one-shot, non-persistent continuation grabber could help the compiler
;;; out by using that in place of the call/cc's in these routines.
;;;
;;; These functions have funky definitions that are precisely tuned to
;;; the needs of the fold/map procs -- for example, to minimize the number
;;; of times the argument lists need to be examined.

;;; Return (map cdr lists). 
;;; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
	(if (pair? lists)
	    (let ((lis (car lists)))
	      (if (null-list? lis) (abort '())
		  (cons (cdr lis) (recur (cdr lists)))))
	    '())))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].

(define (%cars+cdrs lists)
  (let f ([ls lists] [a* '()] [d* '()])
    (cond
      [(pair? ls) 
       (let ([a (car ls)])
         (if (pair? a) 
             (f (cdr ls) (cons (car a) a*) (cons (cdr a) d*))
             (values '() '())))]
      [else (values (reverse a*) (reverse d*))])))

;  (call-with-current-continuation
;    (lambda (abort)
;      (let recur ((lists lists))
;        (if (pair? lists)
;	    (receive (list other-lists) (car+cdr lists)
;	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
;		  (receive (a d) (car+cdr list)
;		    (receive (cars cdrs) (recur other-lists)
;		      (values (cons a cars) (cons d cdrs))))))
;	    (values '() '()))))))

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list. What a hack.
(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values (list cars-final) '()))))))

;;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (if (pair? lists)
	(receive (list other-lists) (car+cdr lists)
	  (receive (a d) (car+cdr list)
	    (receive (cars cdrs) (recur other-lists)
	      (values (cons a cars) (cons d cdrs)))))
	(values '() '()))))


;;; count
;;;;;;;;;
(define (count pred list1 . lists)
  (check-arg procedure? pred count)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((list1 list1) (lists lists) (i 0))
	(if (null-list? list1) i
	    (receive (as ds) (%cars+cdrs lists)
	      (if (null? as) i
		  (lp (cdr list1) ds
		      (if (apply pred (car list1) as) (+ i 1) i))))))

      ;; Fast path
      (let lp ((lis list1) (i 0))
	(if (null-list? lis) i
	    (lp (cdr lis) (if (pred (car lis)) (+ i 1) i))))))


;;; fold/unfold
;;;;;;;;;;;;;;;

(define unfold-right
  (case-lambda
    [(p f g seed)
     (unfold-right p f g seed '())]
    [(p f g seed tail)
     (check-arg procedure? p unfold-right)
     (check-arg procedure? f unfold-right)
     (check-arg procedure? g unfold-right)
     (let lp ((seed seed) (ans tail))
       (if (p seed) ans
         (lp (g seed)
             (cons (f seed) ans))))]))


(define (unfold p f g seed . maybe-tail-gen)
  (check-arg procedure? p unfold)
  (check-arg procedure? f unfold)
  (check-arg procedure? g unfold)
  (if (pair? maybe-tail-gen) ;;; so much for :optional (aghuloum)

      (let ((tail-gen (car maybe-tail-gen)))
	(if (pair? (cdr maybe-tail-gen))
	    (apply error 'unfold "Too many arguments" p f g seed maybe-tail-gen)

	    (let recur ((seed seed))
	      (if (p seed) (tail-gen seed)
		  (cons (f seed) (recur (g seed)))))))

      (let recur ((seed seed))
	(if (p seed) '()
	    (cons (f seed) (recur (g seed)))))))
      

(define (fold kons knil lis1 . lists)
  (check-arg procedure? kons fold)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
	  (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))
	    
      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-list? lis) ans
	    (lp (cdr lis) (kons (car lis) ans))))))


(define (fold-right kons knil lis1 . lists)
  (check-arg procedure? kons fold-right)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis))))))))


(define (pair-fold-right f zero lis1 . lists)
  (check-arg procedure? f pair-fold-right)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) zero
	      (apply f (append! lists (list (recur cdrs)))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) zero (f lis (recur (cdr lis)))))))

(define (pair-fold f zero lis1 . lists)
  (check-arg procedure? f pair-fold)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))	; N-ary case
	(let ((tails (%cdrs lists)))
	  (if (null? tails) ans
	      (lp tails (apply f (append! lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
	(if (null-list? lis) ans
	    (let ((tail (cdr lis)))		; Grab the cdr now,
	      (lp tail (f lis ans)))))))	; in case F SET-CDR!s LIS.
      

;;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;;; These cannot meaningfully be n-ary.

(define (reduce f ridentity lis)
  (check-arg procedure? f reduce)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (check-arg procedure? f reduce-right)
  (if (null-list? lis) ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
	(if (pair? lis)
	    (f head (recur (car lis) (cdr lis)))
	    head))))



;;; Mappers: append-map append-map! pair-for-each map! filter-map map-in-order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-map f lis1 . lists)
  (check-arg procedure? f append-map)
  (really-append-map append  f lis1 lists))
(define (append-map! f lis1 . lists) 
  (check-arg procedure? f append-map!)
  (really-append-map append! f lis1 lists))

(define (really-append-map appender f lis1 lists)
  (if (pair? lists)
      (receive (cars cdrs) (%cars+cdrs (cons lis1 lists))
	(if (null? cars) '()
	    (let recur ((cars cars) (cdrs cdrs))
	      (let ((vals (apply f cars)))
		(receive (cars2 cdrs2) (%cars+cdrs cdrs)
		  (if (null? cars2) vals
		      (appender vals (recur cars2 cdrs2))))))))

      ;; Fast path
      (if (null-list? lis1) '()
	  (let recur ((elt (car lis1)) (rest (cdr lis1)))
	    (let ((vals (f elt)))
	      (if (null-list? rest) vals
		  (appender vals (recur (car rest) (cdr rest)))))))))


(define (pair-for-each proc lis1 . lists)
  (check-arg procedure? proc pair-for-each)
  (if (pair? lists)

      (let lp ((lists (cons lis1 lists)))
	(let ((tails (%cdrs lists)))
	  (if (pair? tails)
	      (begin (apply proc lists)
		     (lp tails)))))

      ;; Fast path.
      (let lp ((lis lis1))
	(if (not (null-list? lis))
	    (let ((tail (cdr lis)))	; Grab the cdr now,
	      (proc lis)		; in case PROC SET-CDR!s LIS.
	      (lp tail))))))

;;; We stop when LIS1 runs out, not when any list runs out.
(define (map! f lis1 . lists)
  (check-arg procedure? f map!)
  (if (pair? lists)
      (let lp ((lis1 lis1) (lists lists))
	(if (not (null-list? lis1))
	    (receive (heads tails) (%cars+cdrs/no-test lists)
	      (set-car! lis1 (apply f (car lis1) heads))
	      (lp (cdr lis1) tails))))

      ;; Fast path.
      (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
  lis1)


;;; Map F across L, and save up all the non-false results.
(define (filter-map f lis1 . lists)
  (check-arg procedure? f filter-map)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
		    (else (recur cdrs))) ; Tail call in this arm.
	      '())))
	    
      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	    (let ((tail (recur (cdr lis))))
	      (cond ((f (car lis)) => (lambda (x) (cons x tail)))
		    (else tail)))))))


;;; Map F across lists, guaranteeing to go left-to-right.
;;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;;; in which case this procedure may simply be defined as a synonym for MAP.

(define (map-in-order f lis1 . lists)
  (check-arg procedure? f map-in-order)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (let ((x (apply f cars)))		; Do head first,
		(cons x (recur cdrs)))		; then tail.
	      '())))
	    
      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	    (let ((tail (cdr lis))
		  (x (f (car lis))))		; Do head first,
	      (cons x (recur tail)))))))	; then tail.


;;; We extend MAP to handle arguments of unequal length.
(define map map-in-order)	

;;; Contributed by Michael Sperber since it was missing from the
;;; reference implementation.
(define (for-each f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
		 (if (pair? cars)
		     (begin
		       (apply f cars)	; Do head first,
		       (recur cdrs)))))	; then tail.
    
      ;; Fast path.
      (let recur ((lis lis1))
	(if (not (null-list? lis))
	    (begin
	      (f (car lis))		; Do head first,
	      (recur (cdr lis)))))))	; then tail.

;;; filter, remove, partition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
;;; disorder the elements of their argument.

;; This FILTER shares the longest tail of L that has no deleted elements.
;; If Scheme had multi-continuation calls, they could be made more efficient.

(define (filter pred lis)			; Sleazing with EQ? makes this
  (check-arg procedure? pred filter)		; one faster.
  (let recur ((lis lis))		
    (if (null-list? lis) lis			; Use NOT-PAIR? to handle dotted lists.
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.


;;; Another version that shares longest tail.
;(define (filter pred lis)
;  (receive (ans no-del?)
;      ;; (recur l) returns L with (pred x) values filtered.
;      ;; It also returns a flag NO-DEL? if the returned value
;      ;; is EQ? to L, i.e. if it didn't have to delete anything.
;      (let recur ((l l))
;	(if (null-list? l) (values l #t)
;	    (let ((x  (car l))
;		  (tl (cdr l)))
;	      (if (pred x)
;		  (receive (ans no-del?) (recur tl)
;		    (if no-del?
;			(values l #t)
;			(values (cons x ans) #f)))
;		  (receive (ans no-del?) (recur tl) ; Delete X.
;		    (values ans #f))))))
;    ans))



;(define (filter! pred lis)			; Things are much simpler
;  (let recur ((lis lis))			; if you are willing to
;    (if (pair? lis)				; push N stack frames & do N
;        (cond ((pred (car lis))		; SET-CDR! writes, where N is
;               (set-cdr! lis (recur (cdr lis))); the length of the answer.
;               lis)				
;              (else (recur (cdr lis))))
;        lis)))


;;; This implementation of FILTER!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are 
;;;   usually expensive on modern machines, and can be extremely expensive on 
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the 
;;; minimal number of SET-CDR!s to splice the tail of one run of ins to the 
;;; beginning of the next.

(define (filter! pred lis)
  (check-arg procedure? pred filter!)
  (let lp ((ans lis))
    (cond ((null-list? ans)       ans)			; Scan looking for
	  ((not (pred (car ans))) (lp (cdr ans)))	; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (if (pair? lis)
					(if (pred (car lis))
					    (scan-in lis (cdr lis))
					    (scan-out prev (cdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (pair? lis)
					   (if (pred (car lis))
					       (begin (set-cdr! prev lis)
						      (scan-in lis (cdr lis)))
					       (lp (cdr lis)))
					   (set-cdr! prev lis))))))
		  (scan-in ans (cdr ans))
		  ans)))))



;;; Answers share common tail with LIS where possible; 
;;; the technique is slightly subtle.

(define (partition pred lis)
  (check-arg procedure? pred partition)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis)	; Use NOT-PAIR? to handle dotted lists.
	(let ((elt (car lis))
	      (tail (cdr lis)))
	  (receive (in out) (recur tail)
	    (if (pred elt)
		(values (if (pair? out) (cons elt in) lis) out)
		(values in (if (pair? in) (cons elt out) lis))))))))



;(define (partition! pred lis)			; Things are much simpler
;  (let recur ((lis lis))			; if you are willing to
;    (if (null-list? lis) (values lis lis)	; push N stack frames & do N
;        (let ((elt (car lis)))			; SET-CDR! writes, where N is
;          (receive (in out) (recur (cdr lis))	; the length of LIS.
;            (cond ((pred elt)
;                   (set-cdr! lis in)
;                   (values lis out))
;                  (else (set-cdr! lis out)
;                        (values in lis))))))))


;;; This implementation of PARTITION!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on 
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice these runs together into the result 
;;; lists.

(define (partition! pred lis)
  (check-arg procedure? pred partition!)
  (if (null-list? lis) (values lis lis)

      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
			  (let lp ((in-prev in-prev) (lis lis))
			    (if (pair? lis)
				(if (pred (car lis))
				    (lp lis (cdr lis))
				    (begin (set-cdr! out-prev lis)
					   (scan-out in-prev lis (cdr lis))))
				(set-cdr! out-prev lis))))) ; Done.

	       (scan-out (lambda (in-prev out-prev lis)
			   (let lp ((out-prev out-prev) (lis lis))
			     (if (pair? lis)
				 (if (pred (car lis))
				     (begin (set-cdr! in-prev lis)
					    (scan-in lis out-prev (cdr lis)))
				     (lp lis (cdr lis)))
				 (set-cdr! in-prev lis)))))) ; Done.

	;; Crank up the scan&splice loops.
	(if (pred (car lis))
	    ;; LIS begins in-list. Search for out-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values lis l))
		    ((pred (car l)) (lp l (cdr l)))
		    (else (scan-out prev-l l (cdr l))
			  (values lis l))))	; Done.

	    ;; LIS begins out-list. Search for in-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values l lis))
		    ((pred (car l))
		     (scan-in l prev-l (cdr l))
		     (values l lis))		; Done.
		    (else (lp l (cdr l)))))))))


;;; Inline us, please.
(define (remove  pred l) (filter  (lambda (x) (not (pred x))) l))
(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))



;;; Here's the taxonomy for the DELETE/ASSOC/MEMBER functions.
;;; (I don't actually think these are the world's most important
;;; functions -- the procedural FILTER/REMOVE/FIND/FIND-TAIL variants
;;; are far more general.)
;;;
;;; Function			Action
;;; ---------------------------------------------------------------------------
;;; remove pred lis		Delete by general predicate
;;; delete x lis [=]		Delete by element comparison
;;;					     
;;; find pred lis		Search by general predicate
;;; find-tail pred lis		Search by general predicate
;;; member x lis [=]		Search by element comparison
;;;
;;; assoc key lis [=]		Search alist by key comparison
;;; alist-delete key alist [=]	Alist-delete by key comparison

(define delete 
  (case-lambda
    [(x lis)
     (delete x lis equal?)]
    [(x lis =) 
     (filter (lambda (y) (not (= x y))) lis)]))

(define delete! 
  (case-lambda
    [(x lis)
     (delete! x lis equal?)]
    [(x lis =)
     (filter! (lambda (y) (not (= x y))) lis)]))

;;; Extended from R4RS to take an optional comparison argument.
(define member  
  (case-lambda
    [(x lis)
     (member x lis equal?)]
    [(x lis =)
     (find-tail (lambda (y) (= x y)) lis)]))

;;; R4RS, hence we don't bother to define.
;;; The MEMBER and then FIND-TAIL call should definitely
;;; be inlined for MEMQ & MEMV.
;(define (memq    x lis) (member x lis eq?))
;(define (memv    x lis) (member x lis eqv?))


;;; right-duplicate deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete-duplicates delete-duplicates!
;;;
;;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;;; in long lists, sort the list to bring duplicates together, then use a 
;;; linear-time algorithm to kill the dups. Or use an algorithm based on
;;; element-marking. The former gives you O(n lg n), the latter is linear.

(define delete-duplicates 
  (case-lambda
    [(lis)
     (delete-duplicates lis equal?)]
    [(lis elt=)
     (check-arg procedure? elt= delete-duplicates)
     (let recur ((lis lis))
       (if (null-list? lis) lis
         (let* ((x (car lis))
                (tail (cdr lis))
                (new-tail (recur (delete x tail elt=))))
           (if (eq? tail new-tail) lis (cons x new-tail)))))]))

(define delete-duplicates! 
  (case-lambda
    [(lis)
     (delete-duplicates! lis equal?)]
    [(lis elt=)
     (check-arg procedure? elt= delete-duplicates!)
     (let recur ((lis lis))
       (if (null-list? lis) lis
           (let* ((x (car lis))
                  (tail (cdr lis))
                  (new-tail (recur (delete! x tail elt=))))
             (when (not (eq? tail new-tail))
               (set-cdr! lis new-tail))
             lis)))]))


;;; alist stuff
;;;;;;;;;;;;;;;

;;; Extended from R4RS to take an optional comparison argument.
(define assoc 
  (case-lambda
    [(x lis)
     (assoc x lis equal?)]
    [(x lis =)
     (find (lambda (entry) (= x (car entry))) lis)]))

(define (alist-cons key datum alist) (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (elt) (cons (car elt) (cdr elt)))
       alist))

(define alist-delete 
  (case-lambda
    [(key alist)
     (alist-delete key alist equal?)]
    [(key alist =)
     (filter (lambda (elt) (not (= key (car elt)))) alist)]))

(define alist-delete! 
  (case-lambda
    [(key alist)
     (alist-delete! key alist equal?)]
    [(key alist =)
     (filter! (lambda (elt) (not (= key (car elt)))) alist)]))


;;; find find-tail take-while drop-while span break any every list-index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

(define (find-tail pred list)
  (check-arg procedure? pred find-tail)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list)) list
	     (lp (cdr list))))))

(define (take-while pred lis)
  (check-arg procedure? pred take-while)
  (let recur ((lis lis))
    (if (null-list? lis) '()
	(let ((x (car lis)))
	  (if (pred x)
	      (cons x (recur (cdr lis)))
	      '())))))

(define (drop-while pred lis)
  (check-arg procedure? pred drop-while)
  (let lp ((lis lis))
    (if (null-list? lis) '()
	(if (pred (car lis))
	    (lp (cdr lis))
	    lis))))

(define (take-while! pred lis)
  (check-arg procedure? pred take-while!)
  (if (or (null-list? lis) (not (pred (car lis)))) '()
      (begin (let lp ((prev lis) (rest (cdr lis)))
	       (if (pair? rest)
		   (let ((x (car rest)))
		     (if (pred x) (lp rest (cdr rest))
			 (set-cdr! prev '())))))
	     lis)))

(define (span pred lis)
  (check-arg procedure? pred span)
  (let recur ((lis lis))
    (if (null-list? lis) (values '() '())
	(let ((x (car lis)))
	  (if (pred x)
	      (receive (prefix suffix) (recur (cdr lis))
		(values (cons x prefix) suffix))
	      (values '() lis))))))

(define (span! pred lis)
  (check-arg procedure? pred span!)
  (if (or (null-list? lis) (not (pred (car lis)))) (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
		      (if (null-list? rest) rest
			  (let ((x (car rest)))
			    (if (pred x) (lp rest (cdr rest))
				(begin (set-cdr! prev '())
				       rest)))))))
	(values lis suffix))))
  

(define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))
(define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))

(define (any pred lis1 . lists)
  (check-arg procedure? pred any)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(and (pair? heads)
	     (let lp ((heads heads) (tails tails))
	       (receive (next-heads next-tails) (%cars+cdrs tails)
		 (if (pair? next-heads)
		     (or (apply pred heads) (lp next-heads next-tails))
		     (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (and (not (null-list? lis1))
	   (let lp ((head (car lis1)) (tail (cdr lis1)))
	     (if (null-list? tail)
		 (pred head)		; Last PRED app is tail call.
		 (or (pred head) (lp (car tail) (cdr tail))))))))


;(define (every pred list)              ; Simple definition.
;  (let lp ((list list))                ; Doesn't return the last PRED value.
;    (or (not (pair? list))
;        (and (pred (car list))
;             (lp (cdr list))))))

(define every
  (case-lambda
    [(p ls) 
     (or (null-list? ls)
         (let f ([p p] [a (car ls)] [d (cdr ls)])
           (cond
             [(pair? d) 
              (and (p a) (f p (car d) (cdr d)))]
             [else (p a)])))]
    [(p ls1 ls2)
     (cond
       [(and (pair? ls1) (pair? ls2)) 
        (let f ([p p] [a1 (car ls1)] [d1 (cdr ls1)] [a2 (car ls2)] [d2 (cdr ls2)])
          (cond
            [(and (pair? d1) (pair? d2)) 
             (and (p a1 a2) (f p (car d1) (cdr d1) (car d2) (cdr d2)))]
            [else (p a1 a2)]))]
       [else #t])]
    [(pred lis1 . lists)
     (receive (heads tails) (%cars+cdrs (cons lis1 lists))
       (or (not (pair? heads))
           (let lp ((heads heads) (tails tails))
             (receive (next-heads next-tails) (%cars+cdrs tails)
       	(if (pair? next-heads)
       	    (and (apply pred heads) (lp next-heads next-tails))
       	    (apply pred heads))))))]))

      ;; Fast path
;;;       (or (null-list? lis1)
;;; 	  (let lp ((head (car lis1))  (tail (cdr lis1)))
;;; 	    (if (null-list? tail)
;;; 		(pred head)	; Last PRED app is tail call.
;;; 		(and (pred head) (lp (car tail) (cdr tail))))))))

(define (list-index pred lis1 . lists)
  (check-arg procedure? pred list-index)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((lists (cons lis1 lists)) (n 0))
	(receive (heads tails) (%cars+cdrs lists)
	  (and (pair? heads)
	       (if (apply pred heads) n
		   (lp tails (+ n 1))))))

      ;; Fast path
      (let lp ((lis lis1) (n 0))
	(and (not (null-list? lis))
	     (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

;;; Reverse
;;;;;;;;;;;

;R4RS, so not defined here.
;(define (reverse lis) (fold cons '() lis))
				      
;(define (reverse! lis)
;  (pair-fold (lambda (pair tail) (set-cdr! pair tail) pair) '() lis))

(define (reverse! lis)
  (let lp ((lis lis) (ans '()))
    (if (null-list? lis) ans
        (let ((tail (cdr lis)))
          (set-cdr! lis ans)
          (lp tail lis)))))

;;; Lists-as-sets
;;;;;;;;;;;;;;;;;

;;; This is carefully tuned code; do not modify casually.
;;; - It is careful to share storage when possible;
;;; - Side-effecting code tries not to perform redundant writes.
;;; - It tries to avoid linear-time scans in special cases where constant-time
;;;   computations can be performed.
;;; - It relies on similar properties from the other list-lib procs it calls.
;;;   For example, it uses the fact that the implementations of MEMBER and
;;;   FILTER in this source code share longest common tails between args
;;;   and results to get structure sharing in the lset procedures.

(define (%lset2<= = lis1 lis2) (every (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (check-arg procedure? = lset<=)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)	; Fast path
		       (%lset2<= = s1 s2)) ; Real test
		   (lp s2 rest)))))))

(define (lset= = . lists)
  (check-arg procedure? = lset=)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2)	; Fast path
		       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
		   (lp s2 rest)))))))


(define (lset-adjoin = lis . elts)
  (check-arg procedure? = lset-adjoin)
  (fold (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
	lis elts))


(define (lset-union = . lists)
  (check-arg procedure? = lset-union)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
					       ans
					       (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (lset-union! = . lists)
  (check-arg procedure? = lset-union!)
  (reduce (lambda (lis ans)		; Splice new elts of LIS onto the front of ANS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (pair-fold (lambda (pair ans)
				(let ((elt (car pair)))
				  (if (any (lambda (x) (= x elt)) ans)
				      ans
				      (begin (set-cdr! pair ans) pair))))
			      ans lis))))
	  '() lists))


(define (lset-intersection = lis1 . lists)
  (check-arg procedure? = lset-intersection)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member x lis =)) lists))
			lis1)))))

(define (lset-intersection! = lis1 . lists)
  (check-arg procedure? = lset-intersection!)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (member x lis =)) lists))
			 lis1)))))


(define (lset-difference = lis1 . lists)
  (check-arg procedure? = lset-difference)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member x lis =)))
				 lists))
			lis1)))))

(define (lset-difference! = lis1 . lists)
  (check-arg procedure? = lset-difference!)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (not (member x lis =)))
				  lists))
			 lis1)))))


(define (lset-xor = . lists)
  (check-arg procedure? = lset-xor)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ans)
				  (if (member xb a-int-b =) ans (cons xb ans)))
				a-b
				b)))))
	  '() lists))


(define (lset-xor! = . lists)
  (check-arg procedure? = lset-xor!)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
	      (cond ((null? a-b)     (lset-difference! = b a))
		    ((null? a-int-b) (append! b a))
		    (else (pair-fold (lambda (b-pair ans)
				       (if (member (car b-pair) a-int-b =) ans
					   (begin (set-cdr! b-pair ans) b-pair)))
				     a-b
				     b)))))
	  '() lists))


(define (lset-diff+intersection = lis1 . lists)
  (check-arg procedure? = lset-diff+intersection)
  (cond ((every null-list? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition (lambda (elt)
			   (not (any (lambda (lis) (member elt lis =))
				     lists)))
			 lis1))))

(define (lset-diff+intersection! = lis1 . lists)
  (check-arg procedure? = lset-diff+intersection!)
  (cond ((every null-list? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition! (lambda (elt)
			    (not (any (lambda (lis) (member elt lis =))
				      lists)))
			  lis1))))
;;; end of library
) 
