; Module header is generated automatically
#cs(module sxpath-ext mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")

;; W3C compliant extensions to SXPathlib
; $Id: sxpath-ext.scm,v 1.911 2002/12/06 22:10:53 kl Exp kl $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; SXML counterparts to W3C XPath Core Functions Library

; The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
; Converts a given object to a string
; NOTE:
;  1. When converting a nodeset - a document order is not preserved
;  2. number->string function returns the result in a form which is slightly
; different from XPath Rec. specification
(define (sxml:string object)
  (cond
    ((string? object) object)
    ((nodeset? object) (if (null? object)
			 ""
			 (sxml:string-value (car object))))
    ((number? object)
     (if (and (rational? object) (not (integer? object)))  ; like 1/2
         (number->string (exact->inexact object))
         (number->string object)))
    ((boolean? object) (if object "true" "false"))
    (else "")))  ; Unknown type -> empty string. 
                 ; Option: write its value to string port?

; The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
; Converts its argument to a boolean
(define (sxml:boolean object)
  (cond
    ((boolean? object) object)
    ((number? object) (not (= object 0)))
    ((string? object) (> (string-length object) 0))
    ((nodeset? object) (not (null? object)))
    (else #f)))  ; Not specified in XPath Rec.

; The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
; Converts its argument to a number
; NOTE: 
;  1. The argument is not optional (yet?)
;  2. string->number conversion is not IEEE 754 round-to-nearest
;  3. NaN is represented as 0
(define (sxml:number obj)
  (cond
    ((number? obj) obj)
    ((string? obj)
     (let ((nmb (call-with-input-string obj read)))
       (if (number? nmb)
	 nmb
	 0))) ; NaN
    ((boolean? obj) (if obj 1 0))
    ((nodeset? obj) (sxml:number (sxml:string obj)))
    (else 0))) ; unknown datatype

; Returns a string value for a given node in accordance to
; XPath Rec. 5.1 - 5.7 
(define (sxml:string-value node)
  (cond
    ((not (pair? node))  ; a text node or data node
     (sxml:string node))
    ((null? (cdr node))
     "")
    (else
     (apply string-append  ; a list of arguments is always non-null
            (map
             (lambda (node)
               (if (sxml:node? node)  ; not annot-attr node or aux list node
                   (sxml:string-value node) ""))
             (cdr node))))))

; Select SXML element by its unique IDs
; XPath Rec. 4.1
;  object - a nodeset or a datatype which can be converted to a string by means
; of a 'string' function
;  id-index = ( (id-value . element) (id-value . element) ... ) 
; This index is used for selection of an element by its unique ID. 
; The result is a nodeset
(define (sxml:id id-index)
  (lambda(object)
    (if (nodeset? object)
      (let loop ((str-lst (map sxml:string-value object))
		 (res '()))
	(if (null? str-lst)
	  (reverse res)
	  (let ((node (sxml:lookup (car str-lst) id-index)))
	    (if (not node)  ; no such element
	      (loop (cdr str-lst) res)
	      (loop (cdr str-lst) (cons node res))))))
      (let rpt ((lst (string->list (sxml:string object)))
		(tmp '())
		(res '()))
	(cond
	  ((null? lst)
	   (if (null? tmp) 
	     (reverse res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (reverse res)
		 (reverse (cons node res))))))
	  ((member (car lst) '(#\space #\return #\newline #\tab))
	   (if (null? tmp)
	     (rpt (cdr lst) tmp res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (rpt (cdr lst) '() res)
		 (rpt (cdr lst) '() (cons node res))))))
	  (else (rpt (cdr lst) (cons (car lst) tmp) res)))))))
                       
             
;=========================================================================
; Comparators for XPath objects 

; Implements XPath equality comparison in a straightforward nested loop manner
(define (sxml:nested-loop-join string-set1 string-set2 string-op)
  (let first ((str-set1 string-set1)
              (str-set2 string-set2))
    (cond
      ((null? str-set1) #f)
      ((let second ((elem (car str-set1))
                    (set2 str-set2))
         (cond
           ((null? set2) #f)
           ((string-op elem (car set2)) #t)
           (else (second elem (cdr set2))))) #t)
      (else
       (first (cdr str-set1) str-set2)))))

;-------------------------------------------------
; Merge-sort for speeding up equality comparison of two nodesets

; Similar to R5RS 'list-tail' but returns the new list consisting of the first
; 'k' members of 'lst'
(define (sxml:list-head lst k)
  (if (or (null? lst) (zero? k))
      '()
      (cons (car lst) (sxml:list-head (cdr lst) (- k 1)))))

; Implements merge-sort of the given lst
; Returns the sorted list, the smallest member first
; less-than?-pred ::= (lambda (obj1 obj2) ...)
; less-than?-pred returns #t if obj1<obj2 with respect to the given ordering
(define (sxml:merge-sort less-than?-pred lst)
  (letrec
      ((merge-sorted-lists
        ; Merges 2 sorted lists into one sorted list
        (lambda (lst1 lst2)
          (cond
            ((null? lst1) lst2)
            ((null? lst2) lst1)
            ; both lists are non-null here
            ((less-than?-pred (car lst1) (car lst2))
             (cons (car lst1)
                   (merge-sorted-lists (cdr lst1) lst2)))
            (else
             (cons (car lst2)
                   (merge-sorted-lists lst1 (cdr lst2))))))))
    (if
     (or (null? lst) (null? (cdr lst)))  ; already sorted
     lst
     (let ((middle (inexact->exact (round (/ (length lst) 2)))))
       (merge-sorted-lists
        (sxml:merge-sort less-than?-pred (sxml:list-head lst middle))
        (sxml:merge-sort less-than?-pred (list-tail lst middle)))))))

; Implementation of XPath equality comparison for 2 string-sets with
; merge-sort join algorithm
(define (sxml:merge-sort-join string-set1 string-set2 string-op)
  (let loop ((str-set1 (sxml:merge-sort string<? string-set1))
             (str-set2 (sxml:merge-sort string<? string-set2)))
    (cond
      ((or (null? str-set1) (null? str-set2))
       #f)
      ((string-op (car str-set1) (car str-set2))
       ; comparison condition fulfilled for a pair of nodes
       #t)
      ((string<? (car str-set1) (car str-set2))
       ; we can remove (car str-set1) from our further consideration
       (loop (cdr str-set1) str-set2))
      (else  ; vice versa
       (loop str-set1 (cdr str-set2))))))

;-------------------------------------------------
; Radix-sort join for equality comparison of 2 nodesets
; The running time of the algorithm is proportional to the nodeset size and
; to node string-value length
; 
; Since each nodeset contains O(n) nodes and string-value for each node is
; O(n) in length, radix-sort join algorithm evaluates in O(n^2) time. By
; comparison, nested loop join requires O(n^3) time, merge-sort join
; implemented above requires O(n^2*log(n)).
;
; On the other hand, radix-sort join is time-ineffective for relatively small
; nodesets being joined. For small nodesets, the above implemented sort-merge
; join runs more effectively. Radix-sort join is promising for large nodesets.

; Represents a list of chars as a branch in the string-tree
; The list of chars must be non-empty
(define (sxml:charlst->branch lst)
  (if (null? (cdr lst))  ; this is the last character in the lst
      `(,(car lst) #t)
      `(,(car lst) #f ,(sxml:charlst->branch (cdr lst)))))

; Converts a string to a string-tree
(define (sxml:string->tree str)
  (let ((lst (string->list str)))
    (if (null? lst)   ; an empty string is given
        '(*top* #t)
        `(*top* #f ,(sxml:charlst->branch lst)))))

; Adds a new string to string-tree
; In a special case, tree257 may be #f. The function than creates a new tree,
; which contains just the representation for str
(define (sxml:add-string-to-tree str tree)
  (letrec
      ((add-lst-to-tree   ; adds the list of chars to tree
        (lambda (lst tree)
          (if
           (null? lst)  ; the lst is over
           (if
            (cadr tree)  ; whether it is already in the tree
            tree
            (cons (car tree)
                  (cons #t (cddr tree))))
           (let ((curr-char (car lst)))
             (let iter-alist ((alist (cddr tree))
                              (res (list (cadr tree) (car tree))))
               (cond
                 ((null? alist)  ; branch not in a tree
                  (reverse
                   (cons
                    (sxml:charlst->branch lst)
                    res)))
                 ((char=? (caar alist) curr-char)  ; entry found
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons
                     (add-lst-to-tree (cdr lst) (car alist))
                     res))
                   (append
                    (reverse
                     (cons
                      (add-lst-to-tree (cdr lst) (car alist))
                      res))
                    (cdr alist))))
                 ((char>? (caar alist) curr-char)
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons (car alist)
                          (cons (sxml:charlst->branch lst) res)))
                   (append
                    (reverse
                     (cons
                      (sxml:charlst->branch lst)
                      res))
                    alist)))
                 (else
                  (iter-alist (cdr alist)
                              (cons (car alist) res))))))))))
    (add-lst-to-tree (string->list str) tree)))

; Whether a given string is presented in the string-tree
(define (sxml:string-in-tree? str tree)  
  (let loop ((lst (string->list str))
             (tree tree))
    (cond
      ((null? lst)  ; the string is over
       (cadr tree))
      ((assv (car lst) (cddr tree))             
       => (lambda (new-tree)
            (loop (cdr lst) new-tree)))
      (else #f))))
        
; XPath equality comparison for 2 string-sets
;  bool-op - comparison function for 2 boolean values
(define (sxml:radix-sort-join string-set1 string-set2 bool-op)
  (if
   (null? string-set1)  ; always #f
   #f
   (let ((tree
          (let iter-1 ((set1 (cdr string-set1))
                       (tree (sxml:string->tree (car string-set1))))
            (if (null? set1)
                tree
                (iter-1 (cdr set1)
                        (sxml:add-string-to-tree (car set1) tree))))))
     (let iter-2 ((set2 string-set2))
       (cond
         ((null? set2)  ; equality not found
          #f)
         ((bool-op (sxml:string-in-tree? (car set2) tree) #t)
          #t)
         (else
          (iter-2 (cdr set2))))))))

;-------------------------------------------------
; Equality comparison

; A helper for XPath equality operations: = , !=
;  'bool-op', 'number-op' and 'string-op' are comparison operations for 
; a pair of booleans,  numbers and strings respectively
(define (sxml:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (cond
      ((and (not (nodeset? obj1)) (not (nodeset? obj2)))  
       ; neither object is a nodeset
       (cond
         ((boolean? obj1) (bool-op obj1 (sxml:boolean obj2)))
         ((boolean? obj2) (bool-op (sxml:boolean obj1) obj2))
         ((number? obj1) (number-op obj1 (sxml:number obj2)))
         ((number? obj2) (number-op (sxml:number obj1) obj2))
         (else  ; both objects are strings
          (string-op obj1 obj2))))
      ((and (nodeset? obj1) (nodeset? obj2))  ; both objects are nodesets
       (let ((lng1 (length obj1))
             (lng2 (length obj2)))
         (cond
           ((and (< lng1 100000) (< lng2 100000))
            ((if  ; either nodeset is a short one              
              (or (<= lng1 2) (<= lng2 2))
              sxml:nested-loop-join
              sxml:merge-sort-join)
             (map sxml:string-value obj1)
             (map sxml:string-value obj2)
             string-op))
           ((< lng1 lng2)            
            (sxml:radix-sort-join (map sxml:string-value obj1)
                                  (map sxml:string-value obj2)
                                  bool-op))
           (else  ; lng2 < lng1
            (sxml:radix-sort-join (map sxml:string-value obj2)
                                  (map sxml:string-value obj1)
                                  bool-op)))))
      (else  ; one of the objects is a nodeset, another is not
       (call-with-values
        (lambda ()  ; Equality operations are commutative
          (if (nodeset? obj1) (values obj1 obj2) (values obj2 obj1)))
        (lambda (nset elem)
          (cond
            ((boolean? elem) (bool-op elem (sxml:boolean nset)))
            ((number? elem)
             (let loop ((nset 
                         (map
                          (lambda (node) (sxml:number (sxml:string-value node)))
                          nset)))
               (cond
                 ((null? nset) #f)
                 ((number-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            ((string? elem)
             (let loop ((nset (map sxml:string-value nset)))
               (cond
                 ((null? nset) #f)
                 ((string-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            (else  ; unknown datatype
             (cerr "Unknown datatype: " elem nl)
             #f))))))))

(define sxml:equal? (sxml:equality-cmp eq? = string=?))

(define sxml:not-equal?
  (sxml:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2)))))

;-------------------------------------------------
; Relational comparison

; Relational operation ( < , > , <= , >= ) for two XPath objects
;  op is comparison procedure: < , > , <= or >=
(define (sxml:relational-cmp op)
  (lambda (obj1 obj2)
    (cond
      ((not (or (nodeset? obj1) (nodeset? obj2)))  ; neither obj is a nodeset
       (op (sxml:number obj1) (sxml:number obj2)))
      ((boolean? obj1)  ; 'obj1' is a boolean, 'obj2' is a nodeset
       (op (sxml:number obj1) (sxml:number (sxml:boolean obj2))))
      ((boolean? obj2)  ; 'obj1' is a nodeset, 'obj2' is a boolean
       (op (sxml:number (sxml:boolean obj1)) (sxml:number obj2)))
      ((or (null? obj1) (null? obj2)) ; one of the objects is an empty nodeset
       #f)
      (else  ; at least one object is a nodeset
       (op
        (cond
          ((nodeset? obj1)  ; 'obj1' is a (non-empty) nodeset
           (let ((nset1 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj1)))
             (let first ((num1 (car nset1))
                         (nset1 (cdr nset1)))
               (cond
                 ((null? nset1) num1)
                 ((op num1 (car nset1)) (first num1 (cdr nset1)))
                 (else (first (car nset1) (cdr nset1)))))))
          ((string? obj1) (sxml:number obj1))
          (else  ; 'obj1' is a number
           obj1))
        (cond
          ((nodeset? obj2)  ; 'obj2' is a (non-empty) nodeset
           (let ((nset2 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj2)))
             (let second ((num2 (car nset2))
                          (nset2 (cdr nset2)))
               (cond
                 ((null? nset2) num2)
                 ((op num2 (car nset2)) (second (car nset2) (cdr nset2)))
                 (else (second num2 (cdr nset2)))))))
          ((string? obj2) (sxml:number obj2))
          (else  ; 'obj2' is a number
           obj2)))))))
           

;=========================================================================
; XPath axes
; An order in resulting nodeset is preserved

; Ancestor axis
(define (sxml:ancestor test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    '()
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) (cdr path))
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))

; Ancestor-or-self axis
(define (sxml:ancestor-or-self test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor-or-self test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    ((sxml:filter test-pred?) (list node))
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) path)
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))
                                                                      
; Descendant axis
; It's similar to original 'node-closure' a resulting nodeset is 
; in depth-first order rather than breadth-first
; Fix: din't descend in non-element nodes!
(define (sxml:descendant test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant test-pred?) node)
      (let rpt ((res '())
		(more ((sxml:child sxml:node?) node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       (cdr more))))))))

; Descendant-or-self axis
(define (sxml:descendant-or-self test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant-or-self test-pred?) node)
      (let rpt ((res '())
		(more (list node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       ; sxml:node?
		       (cdr more))))))))

; Following axis
(define (sxml:following test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list root-node)
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((eq? (caar seq) node)
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append 
			res
			((sxml:descendant-or-self test-pred?) (car seq)))))))
	    ((and (sxml:element? (caar seq))
		  (memq node (sxml:attr-list (caar seq))))
	     (let rpt ((sq (cdr (apply append seq)))
		       (res ((sxml:descendant test-pred?) (caar seq))))
	       (if (null? sq)
		 res
		 (rpt (cdr sq)
		      (append res
			      ((sxml:descendant-or-self test-pred?) (car sq)))))))
	    (else
	      (loop (cons 
		      ((sxml:child sxml:node?) (caar seq))
		      (cons (cdar seq) (cdr seq)))))))))))

; Following-sibling axis
(define (sxml:following-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map (sxml:child sxml:node?)
			      (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

; Namespace axis
(define (sxml:namespace test-pred?)
  (lambda (node)   ; node or nodeset
    ((sxml:filter test-pred?) 
     (sxml:ns-list node))))

; Preceding axis
(define (sxml:preceding test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:preceding test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list (reverse root-node))
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((or (eq? (caar seq) node)
		 (not (null? ((sxml:attribute 
				(lambda (n)
				  (eq? n node))) 
			      (caar seq)))))
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append res
			      (reverse ((sxml:descendant-or-self test-pred?) 
					(car seq))))))))
	    (else (loop (cons (reverse ((sxml:child sxml:node?) (caar seq)))
			      (cons (cdar seq) (cdr seq)))))))))))

; Preceding-sibling axis
(define (sxml:preceding-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if(nodeset? node)
	(map-union ((sxml:preceding-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map
			   (lambda (n)
			     (reverse ((sxml:child sxml:node?) n)))
			   (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

(provide (all-defined)))
