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

;;; Exported:
;;; xcons tree-copy make-list list-tabulate cons* list-copy 
;;; proper-list? circular-list? dotted-list? not-pair? null-list? list=
;;; circular-list length+
;;; iota
;;; first second third fourth fifth sixth seventh eighth ninth tenth
;;; car+cdr
;;; take       drop       
;;; take-right drop-right 
;;; take!      drop-right!
;;; split-at   split-at!
;;; last last-pair
;;; zip unzip1 unzip2 unzip3 unzip4 unzip5
;;; count
;;; append! append-reverse append-reverse! concatenate concatenate! 
;;; unfold       fold       pair-fold       reduce
;;; unfold-right fold-right pair-fold-right reduce-right
;;; append-map append-map! map! pair-for-each filter-map map-in-order
;;; filter  partition  remove
;;; filter! partition! remove! 
;;; find find-tail any every list-index
;;; take-while drop-while take-while!
;;; span break span! break!
;;; delete delete!
;;; alist-cons alist-copy
;;; delete-duplicates delete-duplicates!
;;; alist-delete alist-delete!
;;; reverse! 
;;; lset<= lset= lset-adjoin  
;;; lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
;;; lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!
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

;; JBC, 2003-10-20: some of the names provided by list.ss are prefixed
;; with an s: to avoid colliding with mzscheme.  The wrapper 1.ss 
;; changes their names back to the non-prefixed form. 

(module list mzscheme

  (require (lib "optional.ss" "srfi"))

  (require "cons.ss"
	   "selector.ss"
	   "predicate.ss"
	   (all-except "misc.ss" append! reverse!)
	   (rename "misc.ss" s:append! append!)
	   (rename "misc.ss" s:reverse! reverse!)
	   (all-except "fold.ss" map for-each)
	   (rename "fold.ss" s:map map)
	   (rename "fold.ss" s:for-each for-each)
	   (all-except "search.ss" member)
	   (rename "search.ss" s:member member)
	   "filter.ss"
	   "delete.ss"
	   (all-except "alist.ss" assoc)
	   (rename "alist.ss" s:assoc assoc)
	   "lset.ss")


  (provide 
   (all-from "cons.ss")
   (all-from "selector.ss")
   (all-from "predicate.ss")
   (all-from "misc.ss")
   (all-from "fold.ss")
   (all-from "search.ss")
   (all-from "filter.ss")
   (all-from "delete.ss")
   (all-from "alist.ss")
   (all-from "lset.ss"))



;;end of the unit
)