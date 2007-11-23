; Module header is generated automatically
#cs(module sxpathlib mzscheme
(require (rename (lib "pretty.ss") pp pretty-print))
(require "common.ss")
(require "myenv.ss")
(require (lib "string.ss" "srfi/13"))
(require "util.ss")

;;			XML processing in Scheme
;		     SXPath -- SXML Query Language
;
; $Id: sxpathlib.scm,v 3.918 2004/02/05 22:52:33 kl Exp kl $
;
; This code is in Public Domain
; It's based on SXPath by Oleg Kiselyov, and multiple improvements 
; implemented by Dmitry Lizorkin.
;
; The list of differences from original SXPath.scm my be found in changelog.txt
; 
;  Kirill Lisovsky    lisovsky@acm.org
;
;                                 *  *  *
;
; SXPath is a query language for SXML, an instance of XML Information
; set (Infoset) in the form of s-expressions. See SSAX.scm for the
; definition of SXML and more details. SXPath is also a translation into
; Scheme of an XML Path Language, XPath:
;	http://www.w3.org/TR/xpath
; XPath and SXPath describe means of selecting a set of Infoset's items
; or their properties.
;
; To facilitate queries, XPath maps the XML Infoset into an explicit
; tree, and introduces important notions of a location path and a
; current, context node. A location path denotes a selection of a set of
; nodes relative to a context node. Any XPath tree has a distinguished,
; root node -- which serves as the context node for absolute location
; paths. Location path is recursively defined as a location step joined
; with a location path. A location step is a simple query of the
; database relative to a context node. A step may include expressions
; that further filter the selected set. Each node in the resulting set
; is used as a context node for the adjoining location path. The result
; of the step is a union of the sets returned by the latter location
; paths.
;
; The SXML representation of the XML Infoset (see SSAX.scm) is rather
; suitable for querying as it is. Bowing to the XPath specification,
; we will refer to SXML information items as 'Nodes':
; 	<Node> ::= <Element> | <attributes-coll> | <attrib>
; 		   | "text string" | <PI>
; This production can also be described as
;	<Node> ::= (name . <Nodelist>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodelist> ::= (<Node> ...)
; Nodelists, and Nodes other than text strings are both lists. A
; <Nodelist> however is either an empty list, or a list whose head is not
; a symbol.  A symbol at the head of a node is either an XML name (in
; which case it's a tag of an XML element), or an administrative name
; such as '@'.  This uniform list representation makes processing rather
; simple and elegant, while avoiding confusion. The multi-branch tree
; structure formed by the mutually-recursive datatypes <Node> and
; <Nodelist> lends itself well to processing by functional languages.
;
; A location path is in fact a composite query over an XPath tree or
; its branch. A singe step is a combination of a projection, selection
; or a transitive closure. Multiple steps are combined via join and
; union operations. This insight allows us to _elegantly_ implement
; XPath as a sequence of projection and filtering primitives --
; converters -- joined by _combinators_. Each converter takes a node
; and returns a nodelist which is the result of the corresponding query
; relative to that node. A converter can also be called on a set of
; nodes. In that case it returns a union of the corresponding queries over
; each node in the set. The union is easily implemented as a list
; append operation as all nodes in a SXML tree are considered
; distinct, by XPath conventions. We also preserve the order of the
; members in the union. Query combinators are high-order functions:
; they take converter(s) (which is a Node|Nodelist -> Nodelist function)
; and compose or otherwise combine them. We will be concerned with
; only relative location paths [XPath]: an absolute location path is a
; relative path applied to the root node.
;
; Similarly to XPath, SXPath defines full and abbreviated notations
; for location paths. In both cases, the abbreviated notation can be
; mechanically expanded into the full form by simple rewriting
; rules. In case of SXPath the corresponding rules are given as
; comments to a sxpath function, below. The regression test suite at
; the end of this file shows a representative sample of SXPaths in
; both notations, juxtaposed with the corresponding XPath
; expressions. Most of the samples are borrowed literally from the
; XPath specification, while the others are adjusted for our running
; example, tree1.
;


;=============================================================================
; Basic converters and applicators
; A converter is a function
;	type Converter = Node|Nodelist -> Nodelist
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodelist, yields a non-empty
; nodelist, the converter-predicate is deemed satisfied. Throughout
; this file a nil nodelist is equivalent to #f in denoting a failure.

; Returns #t if given object is a nodelist
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

; If x is a nodelist - returns it as is, otherwise wrap it in a list.
(define (as-nodeset x)
  (if (nodeset? x) x (list x)))

;-----------------------------------------------------------------------------
; Node test
; The following functions implement 'Node test's as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.

; Predicate which returns #t if <obj> is SXML element, otherwise returns #f. 
(define (sxml:element? obj)	
   (and (pair? obj)
	(symbol? (car obj))
	(not (memq (car obj) 
			   ; '(@ @@ *PI* *COMMENT* *ENTITY* *NAMESPACES*)
			   ; the line above is a workaround for old SXML
	'(@ @@ *PI* *COMMENT* *ENTITY*)))))

; The function ntype-names?? takes a list of acceptable node names as a
; criterion and returns a function, which, when applied to a node, 
; will return #t if the node name is present in criterion list and #f
; othervise.
;	ntype-names?? :: ListOfNames -> Node -> Boolean
(define (ntype-names?? crit)
  (lambda(node)
    (and (pair? node)
	 (memq (car node) crit))))

; The function ntype?? takes a type criterion and returns
; a function, which, when applied to a node, will tell if the node satisfies
; the test.
;	ntype?? :: Crit -> Node -> Boolean
;
; The criterion 'crit' is 
;  one of the following symbols:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-list>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*data*		- tests if the Node is a data node 
;                         (text, number, boolean, etc., but not pair)
;	*PI*		- tests if the Node is a PI node
;	*COMMENT*	- tests if the Node is a COMMENT node
;	*ENTITY*        - tests if the Node is a ENTITY node
;	*any*		- #t for any type of Node
(define (ntype?? crit)
  (case crit
    ((*) sxml:element?)
    ((*any*) (lambda (node) #t))
    ((*text*) (lambda (node) (string? node)))
    ((*data*) (lambda (node) (not (pair? node))))
    (else (lambda (node) (and (pair? node) (eq? crit (car node)))))
    ))

; This function takes a namespace-id, and returns a predicate
; Node -> Boolean, which is #t for nodes with this very namespace-id.
; ns-id is a string
; (ntype-namespace-id?? #f) will be #t for nodes with non-qualified names.
(define (ntype-namespace-id?? ns-id)
  (lambda (node)
    (and (pair? node)
	 (not (memq (car node) 
			 '(@ @@ *PI* *COMMENT* *ENTITY*)))
	 (let ((nm (symbol->string (car node))))
	   (cond 
	     ((string-rindex nm #\:)	   
	      => (lambda (pos) 
	      (and 
		(= pos (string-length ns-id))
		(string-prefix? ns-id nm))))
	     (else (not ns-id)))))))
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

; This function takes a predicate and returns it complemented 
; That is if the given predicate yelds #f or '() the complemented one  
; yields the given node (#t) and vice versa.
(define (sxml:complement pred)
  (lambda(node)
    (case (pred node)
      ((#f '()) node)
      (else #f))))

; Curried equivalence converter-predicates
(define (node-eq? other)
  (lambda (node)
    (eq? other node)))

(define (node-equal? other)
  (lambda (node)
    (equal? other node)))

; node-pos:: N -> Nodelist -> Nodelist, or
; node-pos:: N -> Converter
; Select the N'th element of a Nodelist and return as a singular Nodelist;
; Return an empty nodelist if the Nth element does not exist.
; ((node-pos 1) Nodelist) selects the node at the head of the Nodelist,
; if exists; ((node-pos 2) Nodelist) selects the Node after that, if
; exists.
; N can also be a negative number: in that case the node is picked from
; the tail of the list.
; ((node-pos -1) Nodelist) selects the last node of a non-empty nodelist;
; ((node-pos -2) Nodelist) selects the last but one node, if exists.
(define (node-pos n)
  (lambda (nodelist)
    (cond
     ((not (nodeset? nodelist)) '())
     ((null? nodelist) nodelist)
     ((eqv? n 1) (list (car nodelist)))
     ((negative? n) ((node-pos (+ n 1 (length nodelist))) nodelist))
     (else
      (assert (positive? n))
      ((node-pos (-- n)) (cdr nodelist))))))

; filter:: Converter -> Converter
; A filter applicator, which introduces a filtering context. The argument
; converter is considered a predicate, with either #f or nil result meaning
; failure.
(define (sxml:filter pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)) 
	       (res '()))
      (if (null? lst)
	  (reverse res)
	  (let ((pred-result (pred? (car lst))))
	    (loop (cdr lst)
		  (if (and pred-result (not (null? pred-result)))
		      (cons (car lst) res)
		      res)))))))

; take-until:: Converter -> Converter, or
; take-until:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have been processed
; till that moment (that is, which fail the predicate).
; take-until is a variation of the filter above: take-until passes
; elements of an ordered input set till (but not including) the first
; element that satisfies the predicate.
; The nodelist returned by ((take-until (not pred)) nset) is a subset -- 
; to be more precise, a prefix -- of the nodelist returned by
; ((filter pred) nset)
(define (take-until pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		'()
		(cons (car lst) (loop (cdr lst)))))
	  ))))

; take-after:: Converter -> Converter, or
; take-after:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have not been processed:
; that is, return the elements of the input nodelist that follow the first
; element that satisfied the predicate.
; take-after along with take-until partition an input nodelist into three
; parts: the first element that satisfies a predicate, all preceding
; elements and all following elements.
(define (take-after pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		(cdr lst)
		(loop (cdr lst))))
	  ))))

; Apply proc to each element of lst and return the list of results.
; if proc returns a nodelist, splice it into the result
;
; From another point of view, map-union is a function Converter->Converter,
; which places an argument-converter in a joining context.
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; node-reverse :: Converter, or
; node-reverse:: Node|Nodelist -> Nodelist
; Reverses the order of nodes in the nodelist
; This basic converter is needed to implement a reverse document order
; (see the XPath Recommendation).
(define node-reverse 
  (lambda (node-or-nodelist)
    (if (not (nodeset? node-or-nodelist)) (list node-or-nodelist)
	(reverse node-or-nodelist))))

; node-trace:: String -> Converter
; (node-trace title) is an identity converter. In addition it prints out
; a node or nodelist it is applied to, prefixed with the 'title'.
; This converter is very useful for debugging.
(define (node-trace title)
  (lambda (node-or-nodelist)
    (cout nl "-->" title " :")
    (pp node-or-nodelist)
    node-or-nodelist))


;------------------------------------------------------------------------------
; Converter combinators
;
; Combinators are higher-order functions that transmogrify a converter
; or glue a sequence of converters into a single, non-trivial
; converter. The goal is to arrive at converters that correspond to
; XPath location paths.
;
; From a different point of view, a combinator is a fixed, named
; _pattern_ of applying converters. Given below is a complete set of
; such patterns that together implement XPath location path
; specification. As it turns out, all these combinators can be built
; from a small number of basic blocks: regular functional composition,
; map-union and filter applicators, and the nodelist union.



; select-kids:: Pred -> Node -> Nodelist
; Given a Node, return an (ordered) subset its children that satisfy
; the Pred (a converter, actually)
; select-kids:: Pred -> Nodelist -> Nodelist
; The same as above, but select among children of all the nodes in
; the Nodelist
;
; More succinctly, the signature of this function is
; select-kids:: Converter -> Converter
(define (select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond 
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      ((sxml:filter test-pred?) (cdr node)))	; it's a single node
     (else (map-union (select-kids test-pred?) node)))))


; node-self:: Pred -> Node -> Nodelist, or
; node-self:: Converter -> Converter
; Similar to select-kids but apply to the Node itself rather
; than to its children. The resulting Nodelist will contain either one
; component, or will be empty (if the Node failed the Pred).
(define node-self sxml:filter)


; node-join:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-join:: [Converter] -> Converter
; join the sequence of location steps or paths as described
; in the title comments above.
(define (node-join . selectors)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (selectors selectors))
      (if (null? selectors) nodelist
	  (loop 
	   (if (nodeset? nodelist)
	       (map-union (car selectors) nodelist)
	       ((car selectors) nodelist))
	   (cdr selectors))))))


; node-reduce:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-reduce:: [Converter] -> Converter
; A regular functional composition of converters.
; From a different point of view,
;    ((apply node-reduce converters) nodelist)
; is equivalent to
;    (foldl apply nodelist converters)
; i.e., folding, or reducing, a list of converters with the nodelist
; as a seed.
(define (node-reduce . converters)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (converters converters))
      (if (null? converters) nodelist
	  (loop ((car converters) nodelist) (cdr converters))))))


; node-or:: [Converter] -> Converter
; This combinator applies all converters to a given node and
; produces the union of their results.
; This combinator corresponds to a union, '|' operation for XPath
; location paths.
(define (node-or . converters)
  (lambda (node-or-nodelist)
    (let loop ((result '()) (converters converters))
      (if (null? converters) result
	  (loop (append result (or ((car converters) node-or-nodelist) '()))
		(cdr converters))))))


; node-closure:: Converter -> Converter
; Select all _descendants_ of a node that satisfy a converter-predicate.
; This combinator is similar to select-kids but applies to
; grand... children as well.
; This combinator implements the "descendant::" XPath axis
; Conceptually, this combinator can be expressed as
; (define (node-closure f)
;      (node-or
;        (select-kids f)
;	 (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (ntype?? '*)) will return an empty nodelist. At
; this point further iterations will no longer affect the result and
; can be stopped.
(define (node-closure test-pred?)	    
(let ((kid-selector (select-kids test-pred?)))
  (lambda (node) ; Nodelist or node
    (let loop ((parent node) (result '()))
      (if (null? parent) result
	(loop (sxml:child-elements parent)
	  (append result
	    (kid-selector parent)))
	)))))

;=============================================================================
; Unified with sxpath-ext and sxml-tools

; According to XPath specification 2.3, this test is true for any
; XPath node.
; For SXML auxiliary lists and lists of attributes has to be excluded.
(define (sxml:node? node)
  (not (and 
	 (pair? node)
	 (memq (car node) '(@ @@)))))

; Returns the list of attributes for a given SXML node
; Empty list is returned if the given node os not an element,
; or if it has no list of attributes
(define (sxml:attr-list obj)
  (if (and  (sxml:element? obj) 
	    (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cdadr obj)
	 '()))

; Attribute axis
(define (sxml:attribute test-pred?)
  (let ((fltr (sxml:filter test-pred?)))
    (lambda (node)
      (fltr
	(apply append
	       (map
             sxml:attr-list
		(as-nodeset node)))))))

; Child axis
;  This function is similar to 'select-kids', but it returns an empty
;  child-list for PI, Comment and Entity nodes
(define (sxml:child test-pred?)
  (lambda (node)		; node or node-set
    (cond 
      ((null? node) node)
      ((not (pair? node)) '())   ; No children
      ((memq (car node) '(*PI* *COMMENT* *ENTITY*))   ; PI, Comment or Entity
       '())   ; No children
      ((symbol? (car node))    ; it's a single node       
       ((sxml:filter test-pred?) (cdr node)))
      (else (map-union (sxml:child test-pred?) node)))))

; Parent axis
; Given a predicate, it returns a function 
;  RootNode -> Converter
; which which yields a 
;  node -> parent 
; converter then applied to a rootnode.
; Thus, such a converter may be constructed using
;  ((sxml:parent test-pred) rootnode)
; and returns a parent of a node it is applied to.
; If applied to a nodelist, it returns the 
; list of parents of nodes in the nodelist. The rootnode does not have
; to be the root node of the whole SXML tree -- it may be a root node
; of a branch of interest.
; The parent:: axis can be used with any SXML node.
(define (sxml:parent test-pred?)
  (lambda (root-node)   ; node or nodelist
    (lambda (node)   ; node or nodelist
      (if (nodeset? node)
	(map-union ((sxml:parent test-pred?) root-node) node)
	(let rpt ((pairs
		    (apply append
		     (map 
			      (lambda (root-n)
				(map
				  (lambda (arg) (cons arg root-n))
				  (append 
				    (sxml:attr-list root-n)
				    (sxml:child-nodes root-n))))
                              (as-nodeset root-node)))
		     ))
	  (if (null? pairs)
	    '()
	    (let ((pair (car pairs)))
	      (if (eq? (car pair) node)
		((sxml:filter test-pred?) (list (cdr pair)))
		(rpt (append
			(map
			  (lambda (arg) (cons arg (car pair)))
			  (append 
			    (sxml:attr-list (car pair))
			    (sxml:child-nodes (car pair))))
			(cdr pairs)
			))))))))))


;=============================================================================
; Popular short cuts 

; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodelist, it returns the list
; of parents of nodes in the nodelist.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
;
; Please note: this function is provided for backward compatibility 
; with SXPath/SXPathlib ver. 3.5.x.x and earlier.
; Now it's a particular case of 'sxml:parent' application: 
(define node-parent (sxml:parent (ntype?? '*any*)))

(define sxml:child-nodes (sxml:child sxml:node?))

(define sxml:child-elements (select-kids sxml:element?))


(provide (all-defined)))
