; Module header is generated automatically
#cs(module guides mzscheme
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))

;; $Id: guides.scm,v 2.4 2003/12/08 02:07:23 kl Exp kl $
;; DataGuide is a "structural summary" for semistructured data and may be 
;; considered as analog of traditional database schema in context of 
;; semistructured data management.

;==============================================================================
; Auxilliary

(define dgs:version 
  (string-append " $Revision: 2.4 $" nl " $Date: 2003/12/08 02:07:23 $"))

;------------------------------------------------------------------------------
; Customized versions of SRFI-1 functions

; right fold 
(define (dgs:fold op init seq)
  (if (null? seq)
    init
    (op (car seq)
	(dgs:fold op init (cdr seq)))))

; find from SRFI-1 optimized for speed
(define (dgs:find pred seq)
  (cond
    ((let lp ((seq seq))
    (and (pair? seq)
	 (if (pred (car seq)) 
	   seq
	     (lp (cdr seq)))))
     => car)
    (else #f)))

;------------------------------------------------------------------------------
; DataGuide management

; Add location path <lp> in <tree>
; If location path is already present - then it is skipped.
(define (add-lp tree lp)
    (cond
      ; lp is empty : return the tree as it is
      ((null? lp) tree)
      ; lp is present in the tree: traverse the tree recursively
      ((dgs:find (lambda(x)
	       (and (pair? x)
		    (eq? (car lp) (car x)))) 
	     tree) 
       => (lambda(x)
	    (add-lp x (cdr lp))
	    tree))
      ; lp is absent in the tree: add it and return modified tree
      (else
	(if (null? tree)
	  ; if the tree is empty
	  (set! tree (list lp))
         (set-cdr! tree (cons lp (cdr tree))))
	 tree
	  ))  
  )


;==============================================================================
; DataGuides for SXML tree (DOM-style)
; This functions build DataGuides for semistructured data represented
; data represented as SXML tree.

; Flat DataGuide for given node or nodeset <obj>
; Flat DataGuide is a list of all the unique location paths found in the 
; source data. It contains no location paths which are absent in the source
; data.
; Location paths with trailing @ (which are LPs of attributes-lists) are
; excluded.
(define (sxml-guide-flat obj . ignore)
  (define (helper lp)
  (lambda (x y)
    (if (and (pair? x)
	   (not (memq (car x) 
                      (if (null? ignore) 
			'(*PI* *COMMENT* *NAMESPACES* *ENTITY*)
		      (car ignore))))) 
      (let ((this-lp (cons (car x) lp)))
 ;      (cerr nl y)
	(dgs:fold 
	  (helper this-lp)
		 (if (or (eq? '@ (car x)) ; excludes @-ended location paths
			 (member this-lp y))
		   y
		   (cons this-lp y))
		 (cdr x)))
      y)))
  (map reverse
       (dgs:fold (helper '()) '() obj)
       ))

; Strong DataGuide for given node or nodeset <obj>
; Strong DataGuide is a tree which contains one instance of every location 
; path found in the source data, and which contains no location paths which 
; are absent in the source data
(define (sxml-guide obj . ignore)
  (define (helper lp)
  (lambda (x y)
    (if (and (pair? x)
	   (not (memq (car x) 
                      (if (null? ignore) 
			'(*PI* *COMMENT* *NAMESPACES* *ENTITY*)
		      (car ignore))))) 
      (let ((this-lp (cons (car x) lp)))
	(dgs:fold (helper this-lp)
		 (add-lp y (reverse this-lp))
		 (cdr x)))
      y)))
  (dgs:fold (helper '()) '() obj)
  )

;==============================================================================
; DataGuides (SSAX-style)
; This functions build DataGuides for while parsing XML data.

; Flat data guide
; The seed is pair whose car is current location path (reversed) and 
; whose cdr is DataGuide accumulated.
(define (xml-guide-flat xml-port)
 (cdr 
  (map reverse
  ((ssax:make-parser
	   NEW-LEVEL-SEED 
	   (lambda (elem-gi attributes namespaces
			      expected-content seed)
	     (cons (cons elem-gi (car seed)) ; Add element name to current LP 
		   (cdr seed)))
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let ((attr-lps (map
				 (lambda(attr)
				   `(,(car attr) @ ,@(car seed)))
				 attributes))) 
	       (cons (car parent-seed)    
	             ; Add LP to DataGuide, if unique 	
		     (if
		       (member (car seed) (cdr seed))
		     ; If elements LP is already in DG - then add its attributes
		     ; which are not in DG already
		       (append
			 (filter 
			   (lambda(lp) (not (member lp (cdr seed))))
				 attr-lps)
			 (cdr seed))
	             ; If elements LP is unique - then add to DataGuide
		     ;  it and all its attributes
		     (append attr-lps (cons (car seed) (cdr seed)))
		     ))))
	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       seed))

	    xml-port 
	    (cons '() '()) ; Initial seed
	    ))))



(provide (all-defined)))
