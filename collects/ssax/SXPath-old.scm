;			XML processing in Scheme
;		     SXPath -- SXML Query Language
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
;	<Node> ::= (name . <Nodeset>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodeset> ::= (<Node> ...)
; Nodesets, and Nodes other than text strings are both lists. A
; <Nodeset> however is either an empty list, or a list whose head is not
; a symbol.  A symbol at the head of a node is either an XML name (in
; which case it's a tag of an XML element), or an administrative name
; such as '@'.  This uniform list representation makes processing rather
; simple and elegant, while avoiding confusion. The multi-branch tree
; structure formed by the mutually-recursive datatypes <Node> and
; <Nodeset> lends itself well to processing by functional languages.
;
; A location path is in fact a composite query over an XPath tree or
; its branch. A singe step is a combination of a projection, selection
; or a transitive closure. Multiple steps are combined via join and
; union operations. This insight allows us to _elegantly_ implement
; XPath as a sequence of projection and filtering primitives --
; converters -- joined by _combinators_. Each converter takes a node
; and returns a nodeset which is the result of the corresponding query
; relative to that node. A converter can also be called on a set of
; nodes. In that case it returns a union of the corresponding queries over
; each node in the set. The union is easily implemented as a list
; append operation as all nodes in a SXML tree are considered
; distinct, by XPath conventions. We also preserve the order of the
; members in the union. Query combinators are high-order functions:
; they take converter(s) (which is a Node|Nodeset -> Nodeset function)
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
; To do:
; Rename filter to node-filter or ns-filter
; Use ;=== for chapters, ;--- for sections, and ;^^^ for end sections
;
; $Id: SXPath-old.scm,v 1.2 2004/11/09 14:11:40 sperber Exp $


	; See http://pobox.com/~oleg/ftp/Scheme/myenv.scm
	; See http://pobox.com/~oleg/ftp/Scheme/myenv-scm.scm
	; See http://pobox.com/~oleg/ftp/Scheme/myenv-bigloo.scm
;(module SXPath
;  (include "myenv-bigloo.scm"))		; For use with Bigloo 2.2b
;(load "myenv-scm.scm")		; For use with SCM v5d2
;(include "myenv.scm")		; For use with Gambit-C 3.0



(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

;-------------------------
; Basic converters and applicators
; A converter is a function
;	type Converter = Node|Nodeset -> Nodeset
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodeset, yields a non-empty
; nodeset, the converter-predicate is deemed satisfied. Throughout
; this file a nil nodeset is equivalent to #f in denoting a failure.

; The following function implements a 'Node test' as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.
;
; The function node-typeof? takes a type criterion and returns a function,
; which, when applied to a node, will tell if the node satisfies
; the test.
;	node-typeof? :: Crit -> Node -> Boolean
;
; The criterion 'crit' is a symbol, one of the following:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-coll>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*PI*		- tests if the Node is a PI node
;	*any*		- #t for any type of Node

(define (node-typeof? crit)
  (lambda (node)
    (case crit
      ((*) (and (pair? node) (not (memq (car node) '(@ *PI*)))))
      ((*any*) #t)
      ((*text*) (string? node))
      (else
       (and (pair? node) (eq? crit (car node))))
)))


; Curried equivalence converter-predicates
(define (node-eq? other)
  (lambda (node)
    (eq? other node)))

(define (node-equal? other)
  (lambda (node)
    (equal? other node)))

; node-pos:: N -> Nodeset -> Nodeset, or
; node-pos:: N -> Converter
; Select the N'th element of a Nodeset and return as a singular Nodeset;
; Return an empty nodeset if the Nth element does not exist.
; ((node-pos 1) Nodeset) selects the node at the head of the Nodeset,
; if exists; ((node-pos 2) Nodeset) selects the Node after that, if
; exists.
; N can also be a negative number: in that case the node is picked from
; the tail of the list.
; ((node-pos -1) Nodeset) selects the last node of a non-empty nodeset;
; ((node-pos -2) Nodeset) selects the last but one node, if exists.

(define (node-pos n)
  (lambda (nodeset)
    (cond
     ((not (nodeset? nodeset)) '())
     ((null? nodeset) nodeset)
     ((eqv? n 1) (list (car nodeset)))
     ((negative? n) ((node-pos (+ n 1 (length nodeset))) nodeset))
     (else
      (assert (positive? n))
      ((node-pos (dec n)) (cdr nodeset))))))

; filter:: Converter -> Converter
; A filter applicator, which introduces a filtering context. The argument
; converter is considered a predicate, with either #f or nil result meaning
; failure.
(define (filter pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))) (res '()))
      (if (null? lst)
	  (reverse res)
	  (let ((pred-result (pred? (car lst))))
	    (loop (cdr lst)
		  (if (and pred-result (not (null? pred-result)))
		      (cons (car lst) res)
		      res)))))))

; take-until:: Converter -> Converter, or
; take-until:: Pred -> Node|Nodeset -> Nodeset
; Given a converter-predicate and a nodeset, apply the predicate to
; each element of the nodeset, until the predicate yields anything but #f or
; nil. Return the elements of the input nodeset that have been processed
; till that moment (that is, which fail the predicate).
; take-until is a variation of the filter above: take-until passes
; elements of an ordered input set till (but not including) the first
; element that satisfies the predicate.
; The nodeset returned by ((take-until (not pred)) nset) is a subset -- 
; to be more precise, a prefix -- of the nodeset returned by
; ((filter pred) nset)

(define (take-until pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		'()
		(cons (car lst) (loop (cdr lst)))))
	  ))))


; take-after:: Converter -> Converter, or
; take-after:: Pred -> Node|Nodeset -> Nodeset
; Given a converter-predicate and a nodeset, apply the predicate to
; each element of the nodeset, until the predicate yields anything but #f or
; nil. Return the elements of the input nodeset that have not been processed:
; that is, return the elements of the input nodeset that follow the first
; element that satisfied the predicate.
; take-after along with take-until partition an input nodeset into three
; parts: the first element that satisfies a predicate, all preceding
; elements and all following elements.

(define (take-after pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		(cdr lst)
		(loop (cdr lst))))
	  ))))

; Apply proc to each element of lst and return the list of results.
; if proc returns a nodeset, splice it into the result
;
; From another point of view, map-union is a function Converter->Converter,
; which places an argument-converter in a joining context.

(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; node-reverse :: Converter, or
; node-reverse:: Node|Nodeset -> Nodeset
; Reverses the order of nodes in the nodeset
; This basic converter is needed to implement a reverse document order
; (see the XPath Recommendation).
(define node-reverse 
  (lambda (node-or-nodeset)
    (if (not (nodeset? node-or-nodeset)) (list node-or-nodeset)
	(reverse node-or-nodeset))))

; node-trace:: String -> Converter
; (node-trace title) is an identity converter. In addition it prints out
; a node or nodeset it is applied to, prefixed with the 'title'.
; This converter is very useful for debugging.

(define (node-trace title)
  (lambda (node-or-nodeset)
    (cout nl "-->")
    (display title)
    (display " :")
    (pretty-print node-or-nodeset)
    node-or-nodeset))


;-------------------------
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
; map-union and filter applicators, and the nodeset union.



; select-kids:: Pred -> Node -> Nodeset
; Given a Node, return an (ordered) subset its children that satisfy
; the Pred (a converter, actually)
; select-kids:: Pred -> Nodeset -> Nodeset
; The same as above, but select among children of all the nodes in
; the Nodeset
;
; More succinctly, the signature of this function is
; select-kids:: Converter -> Converter

(define (select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond 
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      ((filter test-pred?) (cdr node)))	; it's a single node
     (else (map-union (select-kids test-pred?) node)))))


; node-self:: Pred -> Node -> Nodeset, or
; node-self:: Converter -> Converter
; Similar to select-kids but apply to the Node itself rather
; than to its children. The resulting Nodeset will contain either one
; component, or will be empty (if the Node failed the Pred).
(define node-self filter)


; node-join:: [LocPath] -> Node|Nodeset -> Nodeset, or
; node-join:: [Converter] -> Converter
; join the sequence of location steps or paths as described
; in the title comments above.
(define (node-join . selectors)
  (lambda (nodeset)		; Nodeset or node
    (let loop ((nodeset nodeset) (selectors selectors))
      (if (null? selectors) nodeset
	  (loop 
	   (if (nodeset? nodeset)
	       (map-union (car selectors) nodeset)
	       ((car selectors) nodeset))
	   (cdr selectors))))))


; node-reduce:: [LocPath] -> Node|Nodeset -> Nodeset, or
; node-reduce:: [Converter] -> Converter
; A regular functional composition of converters.
; From a different point of view,
;    ((apply node-reduce converters) nodeset)
; is equivalent to
;    (foldl apply nodeset converters)
; i.e., folding, or reducing, a list of converters with the nodeset
; as a seed.
(define (node-reduce . converters)
  (lambda (nodeset)		; Nodeset or node
    (let loop ((nodeset nodeset) (converters converters))
      (if (null? converters) nodeset
	  (loop ((car converters) nodeset) (cdr converters))))))


; node-or:: [Converter] -> Converter
; This combinator applies all converters to a given node and
; produces the union of their results.
; This combinator corresponds to a union, '|' operation for XPath
; location paths.
; (define (node-or . converters)
;   (lambda (node-or-nodeset)
;     (if (null? converters) node-or-nodeset
; 	(append 
; 	 ((car converters) node-or-nodeset)
; 	 ((apply node-or (cdr converters)) node-or-nodeset)))))
; More optimal implementation follows
(define (node-or . converters)
  (lambda (node-or-nodeset)
    (let loop ((result '()) (converters converters))
      (if (null? converters) result
	  (loop (append result (or ((car converters) node-or-nodeset) '()))
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
;	 (node-reduce (select-kids (node-typeof? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (node-typeof? '*)) will return an empty nodeset. At
; this point further iterations will no longer affect the result and
; can be stopped.

(define (node-closure test-pred?)	    
  (lambda (node)		; Nodeset or node
    (let loop ((parent node) (result '()))
      (if (null? parent) result
	  (loop ((select-kids (node-typeof? '*)) parent)
		(append result
			((select-kids test-pred?) parent)))
	  ))))

; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodeset, it returns the list
; of parents of nodes in the nodeset. The rootnode does not have
; to be the root node of the whole SXML tree -- it may be a root node
; of a branch of interest.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
; Note that the parent:: axis can be used with an attribute node as well!

(define (node-parent rootnode)
  (lambda (node)		; Nodeset or node
    (if (nodeset? node) (map-union (node-parent rootnode) node)
	(let ((pred
	       (node-or
		(node-reduce
		 (node-self (node-typeof? '*))
		 (select-kids (node-eq? node)))
		(node-join
		 (select-kids (node-typeof? '@))
		 (select-kids (node-eq? node))))))
	  ((node-or
	    (node-self pred)
	    (node-closure pred))
	   rootnode)))))

;-------------------------
; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))
; (sxpath1 '//) -> (node-or 
;		     (node-self (node-typeof? '*any*))
;		      (node-closure (node-typeof? '*any*)))
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 ?symbol)     -> (select-kids (node-typeof? ?symbol)
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-filter) -> (filter (sxpath path-filter))

(define (sxpath path)
  (lambda (nodeset)
    (let loop ((nodeset nodeset) (path path))
    (cond
     ((null? path) nodeset)
     ((nodeset? nodeset)
      (map-union (sxpath path) nodeset))
     ((procedure? (car path))
      (loop ((car path) nodeset) (cdr path)))
     ((eq? '// (car path))
      (loop
       ((if (nodeset? nodeset) append cons) nodeset
	((node-closure (node-typeof? '*any*)) nodeset))
       (cdr path)))
     ((symbol? (car path))
      (loop ((select-kids (node-typeof? (car path))) nodeset)
	    (cdr path)))
     ((and (pair? (car path)) (eq? 'equal? (caar path)))
      (loop ((select-kids (apply node-equal? (cdar path))) nodeset)
	    (cdr path)))
     ((and (pair? (car path)) (eq? 'eq? (caar path)))
      (loop ((select-kids (apply node-eq? (cdar path))) nodeset)
	    (cdr path)))
     ((pair? (car path))
      (let reducer ((nodeset 
		     (if (symbol? (caar path))
			 ((select-kids (node-typeof? (caar path))) nodeset)
			 (loop nodeset (caar path))))
		    (reducing-path (cdar path)))
	(cond
	 ((null? reducing-path) (loop nodeset (cdr path)))
	 ((number? (car reducing-path))
	  (reducer ((node-pos (car reducing-path)) nodeset)
		   (cdr reducing-path)))
	 (else
	  (reducer ((filter (sxpath (car reducing-path))) nodeset)
		   (cdr reducing-path))))))
     (else
      (error "Invalid path step: " (car path)))
))))

;------------------------------------------------------------------------
; Sample XPath/SXPath expressions: regression test suite for the
; implementation above.

; A running example

(define tree1 
  '(html
    (head (title "Slides"))
    (body
     (p (@ (align "center"))
	(table (@ (style "font-size: x-large"))
	       (tr
		(td (@ (align "right")) "Talks ")
		(td (@ (align "center")) " = ")
		(td " slides + transition"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " data + control"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary")))
     )))


; Example from a posting "Re: DrScheme and XML", 
; Shriram Krishnamurthi, comp.lang.scheme, Nov. 26. 1999.
; http://www.deja.com/getdoc.xp?AN=553507805
(define tree3
  '(poem (@ (title "The Lovesong of J. Alfred Prufrock")
	    (poet "T. S. Eliot"))
	 (stanza
	  (line "Let us go then, you and I,")
	  (line "When the evening is spread out against the sky")
	  (line "Like a patient etherized upon a table:"))
	 (stanza
	  (line "In the room the women come and go")
	  (line "Talking of Michaelangelo."))))

; Validation Test harness

(define-syntax run-test
 (syntax-rules (define)
   ((run-test "scan-exp" (define vars body))
    (define vars (run-test "scan-exp" body)))
   ((run-test "scan-exp" ?body)
    (letrec-syntax
      ((scan-exp			; (scan-exp body k)
	 (syntax-rules (quote quasiquote !)
	   ((scan-exp '() (k-head ! . args))
	     (k-head '() . args))
	   ((scan-exp (quote (hd . tl)) k)
	     (scan-lit-lst (hd . tl) (do-wrap ! quasiquote k)))
	   ((scan-exp (quasiquote (hd . tl)) k)
	     (scan-lit-lst (hd . tl) (do-wrap ! quasiquote k)))
	   ((scan-exp (quote x) (k-head ! . args))
	     (k-head 
	       (if (string? (quote x)) (string->symbol (quote x)) (quote x))
	       . args))
	   ((scan-exp (hd . tl) k)
	     (scan-exp hd (do-tl ! scan-exp tl k)))
	   ((scan-exp x (k-head ! . args))
	     (k-head x . args))))
	(do-tl
	  (syntax-rules (!)
	    ((do-tl processed-hd fn () (k-head ! . args))
	      (k-head (processed-hd) . args))
	    ((do-tl processed-hd fn old-tl k)
	      (fn old-tl (do-cons ! processed-hd k)))))
	(do-cons
	  (syntax-rules (!)
	    ((do-cons processed-tl processed-hd (k-head ! . args))
	      (k-head (processed-hd . processed-tl) . args))))
	(do-wrap
	  (syntax-rules (!)
	    ((do-wrap val fn (k-head ! . args))
	      (k-head (fn val) . args))))
	(do-finish
	  (syntax-rules ()
	    ((do-finish new-body) new-body)))

	(scan-lit-lst			; scan literal list
	  (syntax-rules (quote unquote unquote-splicing !)
	   ((scan-lit-lst '() (k-head ! . args))
	     (k-head '() . args))
	   ((scan-lit-lst (quote (hd . tl)) k)
	     (do-tl quote scan-lit-lst ((hd . tl)) k))
	   ((scan-lit-lst (unquote x) k)
	     (scan-exp x (do-wrap ! unquote k)))
	   ((scan-lit-lst (unquote-splicing x) k)
	     (scan-exp x (do-wrap ! unquote-splicing k)))
	   ((scan-lit-lst (quote x) (k-head ! . args))
	     (k-head 
	       ,(if (string? (quote x)) (string->symbol (quote x)) (quote x))
	       . args))
	    ((scan-lit-lst (hd . tl) k)
	      (scan-lit-lst hd (do-tl ! scan-lit-lst tl k)))
	    ((scan-lit-lst x (k-head ! . args))
	      (k-head x . args))))
	)
      (scan-exp ?body (do-finish !))))
  ((run-test body ...)
   (begin
     (run-test "scan-exp" body) ...))
))

; Overwrite the above macro to switch the tests off
; (define-macro (run-test selector node expected-result) #f)

; Location path, full form: child::para 
; Location path, abbreviated form: para
; selects the para element children of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para (@) "second par"))
       )
      (expected '((para (@) "para") (para (@) "second par")))
      )
  (run-test (select-kids (node-typeof? 'para)) tree expected)
  (run-test (sxpath '(para)) tree expected)
)

; Location path, full form: child::* 
; Location path, abbreviated form: *
; selects all element children of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '((para (@) "para") (br (@)) (para "second par")))
      )
  (run-test (select-kids (node-typeof? '*)) tree expected)
  (run-test (sxpath '(*)) tree expected)
)



; Location path, full form: child::text() 
; Location path, abbreviated form: text()
; selects all text node children of the context node
(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '("cdata"))
      )
  (run-test (select-kids (node-typeof? '*text*)) tree expected)
  (run-test (sxpath '(*text*)) tree expected)
)


; Location path, full form: child::node() 
; Location path, abbreviated form: node()
; selects all the children of the context node, whatever their node type
(let* ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected (cdr tree))
      )
  (run-test (select-kids (node-typeof? '*any*)) tree expected)
  (run-test (sxpath '(*any*)) tree expected)
)

; Location path, full form: child::*/child::para 
; Location path, abbreviated form: */para
; selects all para grandchildren of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para "third para")))
       )
      (expected
       '((para "third para")))
      )
  (run-test
   (node-join (select-kids (node-typeof? '*))
	      (select-kids (node-typeof? 'para)))
   tree expected)
  (run-test (sxpath '(* para)) tree expected)
)


; Location path, full form: attribute::name 
; Location path, abbreviated form: @name
; selects the 'name' attribute of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para (@) "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem")))
      )
  (run-test
   (node-join (select-kids (node-typeof? '@))
	      (select-kids (node-typeof? 'name)))
   tree expected)
  (run-test (sxpath '(@ name)) tree expected)
)

; Location path, full form:  attribute::* 
; Location path, abbreviated form: @*
; selects all the attributes of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem") (id "idz")))
      )
  (run-test
   (node-join (select-kids (node-typeof? '@))
	      (select-kids (node-typeof? '*)))
   tree expected)
  (run-test (sxpath '(@ *)) tree expected)
)


; Location path, full form: descendant::para 
; Location path, abbreviated form: .//para
; selects the para element descendants of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para") (para "second par") (para (@) "third para")))
      )
  (run-test
   (node-closure (node-typeof? 'para))
   tree expected)
  (run-test (sxpath '(// para)) tree expected)
)

; Location path, full form: self::para 
; Location path, abbreviated form: _none_
; selects the context node if it is a para element; otherwise selects nothing

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (run-test (node-self (node-typeof? 'para)) tree '())
  (run-test (node-self (node-typeof? 'elem)) tree (list tree))
)

; Location path, full form: descendant-or-self::node()
; Location path, abbreviated form: //
; selects the context node, all the children (including attribute nodes)
; of the context node, and all the children of all the (element)
; descendants of the context node.
; This is _almost_ a powerset of the context node.
(let* ((tree
       '(para (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       (cons tree
	(append (cdr tree)
       '((@) "para" (@) "second par"
	 (@ (name "aa")) (para (@) "third para")
	 (@) "third para"))))
      )
  (run-test
   (node-or
    (node-self (node-typeof? '*any*))
    (node-closure (node-typeof? '*any*)))
   tree expected)
  (run-test (sxpath '(//)) tree expected)
)

; Location path, full form: ancestor::div 
; Location path, abbreviated form: _none_
; selects all div ancestors of the context node
; This Location expression is equivalent to the following:
;	/descendant-or-self::div[descendant::node() = curr_node]
; This shows that the ancestor:: axis is actually redundant. Still,
; it can be emulated as the following SXPath expression demonstrates.

; The insight behind "ancestor::div" -- selecting all "div" ancestors
; of the current node -- is
;  S[ancestor::div] context_node =
;    { y | y=subnode*(root), context_node=subnode(subnode*(y)),
;          isElement(y), name(y) = "div" }
; We observe that
;    { y | y=subnode*(root), pred(y) }
; can be expressed in SXPath as 
;    ((node-or (node-self pred) (node-closure pred)) root-node)
; The composite predicate 'isElement(y) & name(y) = "div"' corresponds to 
; (node-self (node-typeof? 'div)) in SXPath. Finally, filter
; context_node=subnode(subnode*(y)) is tantamount to
; (node-closure (node-eq? context-node)), whereas node-reduce denotes the
; the composition of converters-predicates in the filtering context.

(let*
    ((root
	 '(div (@ (name "elem") (id "idz")) 
		(para (@) "para") (br (@)) "cdata" (para (@) "second par")
		(div (@ (name "aa")) (para (@) "third para"))))
     (context-node	; /descendant::any()[child::text() == "third para"]
      (car
       ((node-closure 
	 (select-kids
	  (node-equal? "third para")))
       root)))
    (pred
     (node-reduce (node-self (node-typeof? 'div))
		  (node-closure (node-eq? context-node))
		  ))
     )
  (run-test
   (node-or
     (node-self pred)
     (node-closure pred))
   root 
   (cons root
	 '((div (@ (name "aa")) (para (@) "third para")))))
)



; Location path, full form: child::div/descendant::para 
; Location path, abbreviated form: div//para
; selects the para element descendants of the div element
; children of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")
	     (div (para "fourth para"))))
       )
      (expected
       '((para (@) "third para") (para "fourth para")))
      )
  (run-test
   (node-join 
    (select-kids (node-typeof? 'div))
    (node-closure (node-typeof? 'para)))
   tree expected)
  (run-test (sxpath '(div // para)) tree expected)
)


; Location path, full form: /descendant::olist/child::item 
; Location path, abbreviated form: //olist/item
; selects all the item elements that have an olist parent (which is not root)
; and that are in the same document as the context node
; See the following test.

; Location path, full form: /descendant::td/attribute::align 
; Location path, abbreviated form: //td/@align
; Selects 'align' attributes of all 'td' elements in tree1
(let ((tree tree1)
      (expected
       '((align "right") (align "center") (align "center") (align "center"))
      ))
  (run-test
   (node-join 
    (node-closure (node-typeof? 'td))
    (select-kids (node-typeof? '@))
    (select-kids (node-typeof? 'align)))
   tree expected)
  (run-test (sxpath '(// td @ align)) tree expected)
)


; Location path, full form: /descendant::td[attribute::align] 
; Location path, abbreviated form: //td[@align]
; Selects all td elements that have an attribute 'align' in tree1
(let ((tree tree1)
      (expected
       '((td (@ (align "right")) "Talks ") (td (@ (align "center")) " = ")
	 (td (@ (align "center")) " = ") (td (@ (align "center")) " = "))
       ))
  (run-test
   (node-reduce 
    (node-closure (node-typeof? 'td))
    (filter
     (node-join
      (select-kids (node-typeof? '@))
      (select-kids (node-typeof? 'align)))))
   tree expected)
  (run-test (sxpath `(// td ,(node-self (sxpath '(@ align)))))  tree expected)
  (run-test (sxpath '(// (td (@ align)))) tree expected)
  (run-test (sxpath '(// ((td) (@ align)))) tree expected)
  ; note! (sxpath ...) is a converter. Therefore, it can be used
  ; as any other converter, for example, in the full-form SXPath.
  ; Thus we can mix the full and abbreviated form SXPath's freely.
  (run-test
   (node-reduce 
    (node-closure (node-typeof? 'td))
    (filter
     (sxpath '(@ align))))
   tree expected)
)


; Location path, full form: /descendant::td[attribute::align = "right"] 
; Location path, abbreviated form: //td[@align = "right"]
; Selects all td elements that have an attribute align = "right" in tree1
(let ((tree tree1)
      (expected
       '((td (@ (align "right")) "Talks "))
       ))
  (run-test
   (node-reduce 
    (node-closure (node-typeof? 'td))
    (filter
     (node-join
      (select-kids (node-typeof? '@))
      (select-kids (node-equal? '(align "right"))))))
   tree expected)
  (run-test (sxpath '(// (td (@ (equal? (align "right")))))) tree expected)
)

; Location path, full form: child::para[position()=1] 
; Location path, abbreviated form: para[1]
; selects the first para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para"))
      ))
  (run-test
   (node-reduce
    (select-kids (node-typeof? 'para))
    (node-pos 1))
   tree expected)
  (run-test (sxpath '((para 1))) tree expected)
)

; Location path, full form: child::para[position()=last()] 
; Location path, abbreviated form: para[last()]
; selects the last para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para "second par"))
      ))
  (run-test
   (node-reduce
    (select-kids (node-typeof? 'para))
    (node-pos -1))
   tree expected)
  (run-test (sxpath '((para -1))) tree expected)
)

; Illustrating the following Note of Sec 2.5 of XPath:
; "NOTE: The location path //para[1] does not mean the same as the
; location path /descendant::para[1]. The latter selects the first
; descendant para element; the former selects all descendant para
; elements that are the first para children of their parents."

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (run-test
   (node-reduce	; /descendant::para[1] in SXPath
    (node-closure (node-typeof? 'para))
    (node-pos 1))
   tree '((para (@) "para")))
  (run-test (sxpath '(// (para 1))) tree
	    '((para (@) "para") (para (@) "third para")))
)

; Location path, full form: parent::node()
; Location path, abbreviated form: ..
; selects the parent of the context node. The context node may be
; an attribute node!
; For the last test:
; Location path, full form: parent::*/attribute::name
; Location path, abbreviated form: ../@name
; Selects the name attribute of the parent of the context node

(let* ((tree
	'(elem (@ (name "elem") (id "idz")) 
	       (para (@) "para") (br (@)) "cdata" (para "second par")
	       (div (@ (name "aa")) (para (@) "third para")))
	)
       (para1		; the first para node
	(car ((sxpath '(para)) tree)))
       (para3		; the third para node
	(car ((sxpath '(div para)) tree)))
       (div		; div node
	(car ((sxpath '(// div)) tree)))
       )
  (run-test
   (node-parent tree)
   para1 (list tree))
  (run-test
   (node-parent tree)
   para3 (list div))
  (run-test		; checking the parent of an attribute node
   (node-parent tree)
   ((sxpath '(@ name)) div) (list div))
  (run-test
   (node-join
    (node-parent tree)
    (select-kids (node-typeof? '@))
    (select-kids (node-typeof? 'name)))
   para3 '((name "aa")))
  (run-test
   (sxpath `(,(node-parent tree) @ name))
   para3 '((name "aa")))
)

; Location path, full form: following-sibling::chapter[position()=1]
; Location path, abbreviated form: none
; selects the next chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-after node_eq(self::*,cnode)] 
;		[position()=1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((sxpath '(// (chapter (@ (equal? (id "two")))))) tree)))
       (expected
       '((chapter (@ (id "three")) "Chap 3 text")))
      )
  (run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (node-typeof? 'chapter)))
    (take-after (node-eq? a-node))
    (node-pos 1)
    )
   a-node expected)
)

; preceding-sibling::chapter[position()=1]
; selects the previous chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-until node_eq(self::*,cnode)] 
;		[position()=-1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((sxpath '(// (chapter (@ (equal? (id "three")))))) tree)))
       (expected
       '((chapter (@ (id "two")) "Chap 2 text")))
      )
  (run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (node-typeof? 'chapter)))
    (take-until (node-eq? a-node))
    (node-pos -1)
    )
   a-node expected)
)


; /descendant::figure[position()=42]
; selects the forty-second figure element in the document
; See the next example, which is more general.

; Location path, full form:
;    child::table/child::tr[position()=2]/child::td[position()=3] 
; Location path, abbreviated form: table/tr[2]/td[3]
; selects the third td of the second tr of the table
(let ((tree ((node-closure (node-typeof? 'p)) tree1))
      (expected
       '((td " data + control"))
       ))
  (run-test
   (node-join
    (select-kids (node-typeof? 'table))
    (node-reduce (select-kids (node-typeof? 'tr))
		 (node-pos 2))
    (node-reduce (select-kids (node-typeof? 'td))
		 (node-pos 3)))
   tree expected)
  (run-test (sxpath '(table (tr 2) (td 3))) tree expected)
)


; Location path, full form:
;		child::para[attribute::type='warning'][position()=5] 
; Location path, abbreviated form: para[@type='warning'][5]
; selects the fifth para child of the context node that has a type
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 6"))
      ))
  (run-test
   (node-reduce
    (select-kids (node-typeof? 'para))
    (filter
     (node-join
      (select-kids (node-typeof? '@))
      (select-kids (node-equal? '(type "warning")))))
    (node-pos 5))
   tree expected)
  (run-test (sxpath '( (((para (@ (equal? (type "warning"))))) 5 )  ))
	    tree expected)
  (run-test (sxpath '( (para (@ (equal? (type "warning"))) 5 )  ))
	    tree expected)
)


; Location path, full form:
;		child::para[position()=5][attribute::type='warning'] 
; Location path, abbreviated form: para[5][@type='warning']
; selects the fifth para child of the context node if that child has a 'type'
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 5"))
      ))
  (run-test
   (node-reduce
    (select-kids (node-typeof? 'para))
    (node-pos 5)
    (filter
     (node-join
      (select-kids (node-typeof? '@))
      (select-kids (node-equal? '(type "warning"))))))
   tree expected)
  (run-test (sxpath '( (( (para 5))  (@ (equal? (type "warning"))))))
	    tree expected)
  (run-test (sxpath '( (para 5 (@ (equal? (type "warning")))) ))
	    tree expected)
)

; Location path, full form:
;		child::*[self::chapter or self::appendix]
; Location path, semi-abbreviated form: *[self::chapter or self::appendix]
; selects the chapter and appendix children of the context node
(let ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
      (expected
       '((chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (appendix (@ (id "A")) "App A text"))
      ))
  (run-test
   (node-join
    (select-kids (node-typeof? '*))
    (filter
     (node-or
      (node-self (node-typeof? 'chapter))
      (node-self (node-typeof? 'appendix)))))
   tree expected)
  (run-test (sxpath `(* ,(node-or (node-self (node-typeof? 'chapter))
				  (node-self (node-typeof? 'appendix)))))
	    tree expected)
)


; Location path, full form: child::chapter[child::title='Introduction'] 
; Location path, abbreviated form: chapter[title = 'Introduction']
; selects the chapter children of the context node that have one or more
; title children with string-value equal to Introduction
; See a similar example: //td[@align = "right"] above.

; Location path, full form: child::chapter[child::title] 
; Location path, abbreviated form: chapter[title]
; selects the chapter children of the context node that have one or
; more title children
; See a similar example //td[@align] above.

(cerr nl "Example with tree3: extracting the first lines of every stanza" nl)
(let ((tree tree3)
      (expected
       '("Let us go then, you and I," "In the room the women come and go")
      ))
  (run-test
   (node-join
    (node-closure (node-typeof? 'stanza))
    (node-reduce 
     (select-kids (node-typeof? 'line)) (node-pos 1))
    (select-kids (node-typeof? '*text*)))
   tree expected)
  (run-test (sxpath '(// stanza (line 1) *text*)) tree expected)
)

