; Module header is generated automatically
#cs(module sxml-tools mzscheme
(require (lib "defmacro.ss"))
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))
(require "sxpathlib.ss")

;;                            S X M L   T o o l s               
; $Revision: 3.14 $ from $Date: 2003/12/23 05:39:31 $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to lisovsky@acm.org
;           Kirill Lisovsky
;
;   SXML normal form used for normalization-dependent functions:
; If attr-list is present it's always the second in SXML element.
; If aux-list is present - then list of attributes is always
; included, and aux-list is always the third.
;   Minimized form is just the same, but all the empty aux-lists are 
; absent, and empty attr-lists are present only in elements with aux-lists
; present. 

;==============================================================================
; Auxiliary functions.

; unlike filter-map from SRFI-1 this function uses separate predicate 
; and mapping functions. 
; Applies proc to  all the elements of source list that satisfy the predicate 
; and return the list of the results.
(define (filter-and-map pred proc lis)			
  (let rpt ((l lis))		
    (if (null? l)
      '()
      (if (pred (car l))
	(cons (proc (car l)) (rpt (cdr l)))
	(rpt (cdr l))))))
      
; Applies pred to every member of lst and yields #t if all the results
; are #t
(define (check-list pred lst) 
  (cond
    ((null? lst) #t)
    ((pred (car lst))
     (check-list pred (cdr lst)))
    (else #f)))

; Returns attr-list node for a given obj 
;   or #f if it is absent
(define (sxml:attr-list-node obj)
  (if (and (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cadr obj)
	 #f))

; Returns attr-list wrapped in list
;   or '((@)) if it is absent and aux-list is present 
;   or '() if both lists are absent
(define (sxml:attr-as-list obj)
  (cond
    ((sxml:attr-list-node obj)
     => list)
    ((sxml:aux-list-node obj)
     '((@)))
    (else '())))


; Returns aux-list node for a given obj 
;   or #f if it is absent
(define (sxml:aux-list-node obj)
  (if
    (or (null? (cdr obj))
	(null? (cddr obj))
	(not (pair? (caddr obj)))
	(not (eq? (caaddr obj) '@@)))
    #f
    (caddr obj)))

; Returns aux-list wrapped in list 
;   or '() if it is absent
(define (sxml:aux-as-list obj)
  (cond 
    ((sxml:aux-list-node obj)
     => list)
    (else '())))

; optimized (string-rindex name #\:)
; returns position of a separator between namespace-id and LocalName
(define-macro (sxml:find-name-separator len)
  `(let rpt ((pos (-- ,len))) 
     (cond
       ((negative? pos) #f) 	
       ((char=? #\: (string-ref name pos)) pos)
       (else (rpt (-- pos))))))
  

; sxml error message
(define (sxml:error . messages)
  (cerr nl "SXML ERROR: ")
  (apply cerr messages)
  (cerr nl)
  (exit -1))

;==============================================================================
; Predicates

; Predicate which returns #t if given element <obj> is empty. 
; Empty element has no nested elements, text nodes, PIs, Comments or entities
; but it may contain attributes or namespace-id.
; It is a SXML counterpart of XML empty-element.
(define (sxml:empty-element? obj)
  (not 
    ((select-first-kid 
     (lambda(x)
       (or ((ntype-names?? '(*PI* *COMMENT* *ENTITY*)) x)
           ((ntype?? '*) x)
	   (string? x)))) obj)))

; Returns #t if the given <obj> is shallow-normalized SXML element.
; The element itself has to be normalised but its nested elements are not tested.
(define (sxml:shallow-normalized? obj)
  (or 
    (null? (cdr obj))
    (and (or 
	   (and 
	     (pair? (cadr obj)) 
	     (eq? (caadr obj) '@))
	   (not ((select-first-kid (ntype-names?? '(@ @@))) obj)))
	 (or (null? (cddr obj))
	     (and (pair? (caddr obj)) 
		  (eq? (caaddr obj) '@@))
	     (not ((select-first-kid (ntype?? '@@)) obj))))))

; Returns #t if the given <obj> is normalized SXML element.
;  The element itself and all its nested elements have to be normalised.
(define (sxml:normalized? obj)
    (and
      (sxml:shallow-normalized? obj)
    (check-list
      (lambda(x)
	(if
	   (sxml:element? x)
	   (sxml:normalized? x)
	   #t))
       (sxml:content obj))
    ))

; Returns #t if the given <obj> is shallow-minimized SXML element.
; The element itself has to be minimised but its nested elements are not tested.
(define (sxml:shallow-minimized? obj)
  (and
    (sxml:shallow-normalized? obj)
    (not (and (sxml:aux-list-node obj) 
	      (null? (sxml:aux-list obj))))
    (not (and (sxml:attr-list-node obj)
	      (null? (sxml:attr-list obj))
	      (not (sxml:aux-list-node obj))))))

; Returns #t if the given <obj> is minimized SXML element.
;  The element itself and all its nested elements have to be minimised.
(define (sxml:minimized? obj)
    (and
      (sxml:shallow-minimized? obj)
    (check-list
      (lambda(x)
	(if
	   (sxml:element? x)
	   (sxml:minimized? x)
	   #t))
       (sxml:content obj))
    ))

;==============================================================================
; Accessors  

; Returns a name of a given SXML node
; It is introduced for the sake of encapsulation.
(define sxml:name car)

; A version of sxml:name, which returns #f if the given <obj> is 
; not a SXML element.
; Otherwise returns its name.
(define (sxml:element-name obj)
  (and ((ntype?? '*) obj) 
       (car obj)))

; Safe version of sxml:name, which returns #f if the given <obj> is 
; not a SXML node.
; Otherwise returns its name.
(define (sxml:node-name obj)
  (and (pair? obj) 
       (symbol? (car obj))
    (car obj)))

; Returns Local Part of Qualified Name (Namespaces in XML production [6])
; for given obj, which is ":"-separated suffix of its Qualified Name
; If a name of a node given is NCName (Namespaces in XML production [4]), then 
; it is returned as is.
; Please note that while SXML name is a symbol this function returns a string.
(define (sxml:ncname obj)
  (let* ((name (symbol->string (car obj)))
	 (len (string-length name)))
    (cond
      ((sxml:find-name-separator len)
       => (lambda (pos) 
	    (substring name (+ pos 1) len)))
      (else name))))

; Returns namespace-id part of given name, or #f if it's LocalName
(define (sxml:name->ns-id sxml-name)
  (let* ((name (symbol->string sxml-name)))
    (cond
      ((sxml:find-name-separator (string-length name))
       => (lambda (pos) 
	    (substring name  0 pos)))
      (else #f))))
    

; Returns the content of given SXML element or nodeset (just text and element
; nodes) representing it as a list of strings and nested elements in document 
; order.  This list is empty if <obj> is empty element or empty list.
(define (sxml:content obj)
  (((if (nodeset? obj) 
      sxml:filter
      select-kids) 
    (lambda(x)
      (or
	(string? x)   ;  ((ntype?? '*text*) x)
       ((ntype?? '*) x)))) 
   obj))

; Returns a string which combines all the character data 
; from text node childrens of the given SXML element
; or "" if there is no text node children
(define (sxml:text obj)
  (let ((tnodes
	 ((select-kids
	   string?) 
	   obj)))
    (cond 
      ((null? tnodes) "")
      ((null? (cdr tnodes))
       (car tnodes))
      (else (apply string-append tnodes)))))

;------------------------------------------------------------------------------
; Normalization-dependent accessors
;
;
; "Universal" accessors are less effective but may be used for non-normalized SXML
; Safe accessors are named with suffix '-u'
;
; "Fast" accessors are optimized for normalized SXML data.
; They are not applicable to arbitrary non-normalized SXML data
; Their names has no specific suffixes

; Returns all the content of normalized SXML element except attr-list and
; aux-list.
; Thus it includes PI, COMMENT and  ENTITY nodes as well as TEXT and ELEMENT nodes
; returned by sxml:content.
; Returns  a list of nodes in document order or empty list if <obj> is empty 
; element or empty list.
; This function is faster than sxml:content
(define (sxml:content-raw obj)
  ((if (and (not (null? (cdr obj))) 
	    (pair? (cadr obj)) (eq? (caadr obj) '@))
     (if (and (not (null? (cddr obj))) 
	      (pair? (caddr obj)) (eq? (caaddr obj) '@@))
       cdddr
       cddr)
     cdr) obj))


; Returns the list of attributes for given element or nodeset.
; Analog of ((sxpath '(@ *)) obj)
; Empty list is returned if there is no list of attributes.
(define (sxml:attr-list-u obj)
  (cond (((select-first-kid (ntype?? '@)) obj)
	 => cdr)
	(else '())))

; Returns the list of auxiliary nodes for given element or nodeset.
; Analog of ((sxpath '(@@ *)) obj)
; Empty list is returned if a list of auxiliary nodes is absent.
(define (sxml:aux-list obj)
  (if
    (or (null? (cdr obj))
	(null? (cddr obj))
	(not (pair? (caddr obj)))
	(not (eq? (caaddr obj) '@@)))
    '()
    (cdaddr obj)))  

; Returns the list of auxiliary nodes for given element or nodeset.
; Analog of ((sxpath '(@@ *)) obj)
; Empty list is returned if a list of auxiliary nodes is absent.
(define (sxml:aux-list-u obj)
  (cond (((select-first-kid (ntype?? '@@)) obj)
	 => cdr)
	(else '())))

; Return the first aux-node with <aux-name> given in SXML element <obj> 
; or #f is such a node is absent.
; NOTE: it returns just the FIRST node found even if multiple nodes are
; present, so it's mostly intended for nodes with unique names 
(define (sxml:aux-node obj aux-name)
  (cond 
    ((assq aux-name (sxml:aux-list obj)))
    (else #f))) 

; Return a list of aux-node with <aux-name> given in SXML element <obj> 
; or '() if such a node is absent.
(define (sxml:aux-nodes obj aux-name)
  (filter 
    (lambda(x) (eq? aux-name (car x)))
    (sxml:aux-list obj)))

; Accessor for an attribute <attr-name> of given SXML element <obj> which 
; It returns: 
;    the value of the attribute if the attribute is present
;    #f if there is no such an attribute in the given element
(define (sxml:attr obj attr-name)
  (cond 
    ((assq attr-name (sxml:attr-list obj))
     => cadr)
    (else #f)))

; Extracts a value of attribute with given name from attr-list
(define (sxml:attr-from-list attr-list name)
	    (cond 
	      ((assq name attr-list) 
	       => cadr)
	      (else #f)))

; Accessor for a numerical attribute <attr-name> of given SXML element <obj> 
; which It returns: 
;    a value of the attribute as the attribute as a number if the attribute 
;    is present and its value may be converted to number using string->number
;    #f if there is no such an attribute in the given element or
;    its value can't be converted to a number
(define (sxml:num-attr obj attr-name)
  (cond 
    ((assq attr-name (sxml:attr-list obj))
     => (lambda(x) (string->number (cadr x))))
    (else #f)))

; Accessor for an attribute <attr-name> of given SXML element <obj> which 
; may also be an attributes-list or nodeset (usually content of SXML element)
;
; It returns: 
;    the value of the attribute if the attribute is present
;    #f if there is no such an attribute in the given element
(define (sxml:attr-u obj attr-name)
  (cond 
    ((assq attr-name
	   ; the list of attributes is computed below
	   (cond
	     ((and (not (null? (cdr obj))) 
		   (pair? (cadr obj))
		   (eq? '@ (caadr obj)))
	      (cdadr obj))   ; fast track for normalized elements 
	     ((eq? '@ (car obj))
	      (cdr obj))     ; if applied to attr-list
	     (else (sxml:attr-list-u obj))))
     => cadr)
    (else #f)))

; Returns the list of namespaces for given element.
; Analog of ((sxpath '(@@ *NAMESPACES* *)) obj)
; Empty list is returned if there is no list of namespaces.
(define (sxml:ns-list obj)
  (cond ((assv '*NAMESPACES* (sxml:aux-list obj))
	 => cdr)
	(else '())))

; Returns the list of namespace-assoc's for given namespace-id in 
; SXML element <obj>.
; Analog of ((sxpath '(@@ *NAMESPACES* namespace-id)) obj)
; Empty list is returned if there is no namespace-assoc with namespace-id
; given.
(define (sxml:ns-id->nodes obj namespace-id)
  (filter 
    (lambda(x)
      (eq? (car x) namespace-id))
    (sxml:ns-list obj)))

; It returns: 
;    A  URI's for namespace-id given 
;    #f if there is no namespace-assoc with namespace-id given
(define (sxml:ns-id->uri obj namespace-id)
  (cond 
    ((assq namespace-id (sxml:ns-list obj))
     => cadr)
    (else #f)))

; Returns a list of namespace-assocs nodes for NS URI given 
(define (sxml:ns-uri->nodes obj URI)
  (filter
    (lambda (ns-assoc) 
      (string=? (cadr ns-assoc) URI))
    (sxml:ns-list obj)))

; Returns a namespace-id for NS URI given 
(define (sxml:ns-uri->id obj URI)
  (let rpt ((ns-assocs (sxml:ns-list obj)))
  (cond
      ((null? ns-assocs) #f)
      ((string=? (cadar ns-assocs) URI)
       (caar ns-assocs))
      (else (rpt (cdr ns-assocs)))
    )))

; Returns namespace-id for given namespace-assoc list
(define sxml:ns-id car)

; Returns URI for given namespace-assoc list
(define sxml:ns-uri cadr)

; It returns namespace prefix for given namespace-assoc list
;  Original (as in XML document) prefix for namespace-id given 
; has to be strored as the third element in namespace-assoc list 
; if it is different from namespace-id.
;    If original prefix is omitted in namespace-assoc then
;      namespace-id is used instead
(define (sxml:ns-prefix ns-assoc)
      (if (> (length ns-assoc) 2)
	(caddr ns-assoc)
	(car ns-assoc))) 

;==============================================================================
; Data modification functions
; Constructors and mutators for normalized SXML data
; 
; This functions are optimized for normalized SXML data.
; They are not applicable to arbitrary non-normalized SXML data
; 
; Most of the functions are provided in two variants: 
; 1. side-effect intended functions for linear update of given elements.
;   Their names are ended with exclamation mark.
;   An example: 
;      sxml:change-content! 
; 2. pure functions without side-effects which return modified elements.
;   An example: 
;      sxml:change-content
 
; Change the content of given SXML element to <new-content>
; If <new-content> is an empty list then the <obj> is transformed 
; The resulting SXML element is normalized
; Former name sxml:content!
#;(define (sxml:change-content! obj new-content)
  (set-cdr! obj 
	    `(
              ,@(sxml:attr-as-list obj)
              ,@(sxml:aux-as-list obj)
	      ,@new-content)))

; Change the content of given SXML element to <new-content>
; If <new-content> is an empty list then the <obj> is transformed 
; to an empty element
; The resulting SXML element is normalized
(define (sxml:change-content obj new-content)
  `(,(sxml:name obj) 
              ,@(sxml:attr-as-list obj)
              ,@(sxml:aux-as-list obj)
	,@new-content))

; The resulting SXML element is normalized, if <new-attrlist> is empty,
; the cadr of <obj> is (@)
(define (sxml:change-attrlist obj new-attrlist)
  `(,(sxml:name obj) 
     ,@(cond 
	 (new-attrlist
	  `((@ ,@new-attrlist)))
	 ((sxml:aux-list-node obj)
	   '((@)))
	 (else `()))
     ,@(sxml:aux-as-list obj)
     ,@(sxml:content obj)))

; The resulting SXML element is normalized, if <new-attrlist> is empty,
; the cadr of <obj> is (@)
; Former name sxml:attrlist!
#;(define (sxml:change-attrlist! obj new-attrlist)
  (set-cdr! obj 
	`(
	  ,@(cond 
	      (new-attrlist
		`((@ ,@new-attrlist)))
	      ((sxml:aux-list-node obj)
	       '((@)))
	      (else `()))
	  ,@(sxml:aux-as-list obj)
	  ,@(sxml:content obj))))
      
; Change a name of SXML element destructively
; Former name was 'sxml:name!'
#;(define (sxml:change-name! obj new-name)
  (set-car! obj new-name))

; Returns SXML element with its name changed 
(define (sxml:change-name obj new-name)
  (cons new-name (cdr obj)))

; Returns SXML element <obj> with attribute <attr> added or #f
; if the attribute with given name already exists, 
; <attr> is (<attr-name> <attr-value>)
; Pure functional counterpart to sxml:add-attr!
(define (sxml:add-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (if (assq (car attr) attr-list) 
      #f
      `(,(sxml:name obj)
	(@ ,@(cons attr attr-list))
	,@(sxml:aux-as-list obj)
	,@(sxml:content obj)))))

; Add an attribute <attr> for an element <obj>
; Returns #f if the attribute with given name already exists. 
; The resulting SXML node is normalized.
; Linear update counterpart to sxml:add-attr
#;(define (sxml:add-attr! obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (if (assq (car attr) attr-list) 
      #f
      (begin
      (set-cdr! obj 
	`(
	(@ ,@(cons attr attr-list))
	,@(sxml:aux-as-list obj)
	,@(sxml:content obj)))
      obj))))


; Returns SXML element <obj> with changed value of attribute <attr> or #f
; if where is no attribute with given name. 
; <attr> is (<attr-name> <attr-value>)
(define (sxml:change-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (if (null? attr-list)
      #f
      (cond 
	((assv (car attr) attr-list) 
	 => (lambda (y)
	      `(,(sxml:name obj)
		 (@ ,@(map
			(lambda(at)
			  (if
			    (eq? at y)
			    attr
			    at))
			attr-list))
		 ,@(sxml:aux-as-list obj)
		 ,@(sxml:content obj)
		 )))
	(else #f)))))
    
; Change value of the attribute for element <obj> 
; <attr> is (<attr-name> <attr-value>)
; Returns #f if where is no such attribute
#;(define (sxml:change-attr! obj attr)
  (let ((x (sxml:attr-list obj)))
     (if (null? x)
       #f
       (cond 
	 ((assv (car attr) x) => (lambda (y)
				  (set-cdr! y (cdr attr)) obj))
	 (else #f)))))

; Set attribute <attr> of element <obj> 
; If there is no such attribute the new one is added
(define (sxml:set-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (cond 
      ((assv (car attr) attr-list) 
       => (lambda (y)
	    `(,(sxml:name obj)
	       (@ ,@(map
		      (lambda(at)
			(if
			  (eq? at y)
			  attr
			  at))
		      attr-list))
	       ,@(sxml:aux-as-list obj)
	       ,@(sxml:content obj)
	       )))
      (else 
	`(,(sxml:name obj)
	   (@ ,@(cons attr attr-list)) 
	   ,@(sxml:aux-as-list obj)
	   ,@(sxml:content obj))))
    ))

; Set attribute <attr> of element <obj> 
; If there is no such attribute the new one is added
#;(define (sxml:set-attr! obj attr)
  (let ((attr-list (sxml:attr-list obj)))
     (cond 
       ((assv (car attr) attr-list) 
	=> (lambda (x) (set-cdr! x (cdr attr))))
       (else (set-cdr! obj
		       `((@ ,@(cons attr attr-list)) 
			 ,@(sxml:aux-as-list obj)
			 ,@(sxml:content obj))))
   )))

; Returns SXML element <obj> with an auxiliary node <aux-node> added 
(define (sxml:add-aux obj aux-node)
      `(,(sxml:name obj)
	(@ ,@(sxml:attr-list obj))
	(@@ ,@(cons aux-node (sxml:aux-list obj)))
	,@(sxml:content obj)))

; Add an auxiliary node <aux-node> for an element <obj>
#;(define (sxml:add-aux! obj aux-node)
      (set-cdr! obj 
	`(
	(@ ,@(sxml:attr-list obj))
	(@@ ,@(cons aux-node (sxml:aux-list obj)))
	,@(sxml:content obj)))
      obj)

; Eliminates empty lists of attributes and aux-lists for given SXML element 
; <obj> and its descendants ("minimize" it)
; Returns: minimized and normalized SXML element
#;(define (sxml:squeeze! obj)
   (set-cdr! obj 
  `(,@(cond 
	((sxml:attr-list-node obj)
	 => (lambda (atl) 
	      (if (and (null? (cdr atl)) 
		       (null? (sxml:aux-list obj)))
		 '()
	         (list atl))))	
	(else '()))
    ,@(cond ((sxml:aux-list-node obj)
	     => (lambda (axl) 
	      (if (null? (cdr axl))
		'()
	         (list axl))))
	(else '()))
    ,@(map
	(lambda(x)
	  (cond 
	    (((ntype?? '*) x)
	     (sxml:squeeze! x)
	     x)
	    (else x)))
       (sxml:content obj))
    ))
   )

	     
; Eliminates empty lists of attributes and aux-lists for given SXML element 
; <obj> and its descendants ("minimize" it)
; Returns: minimized and normalized SXML element
(define (sxml:squeeze obj)
  `(,(sxml:name obj)
   ,@(cond 
	((sxml:attr-list-node obj)
	 => (lambda (atl) 
	      (if (and (null? (cdr atl)) 
		       (null? (sxml:aux-list obj)))
		 '()
	         (list atl))))	
	(else '()))
    ,@(cond ((sxml:aux-list-node obj)
	     => (lambda (axl) 
	      (if (null? (cdr axl))
		'()
	         (list axl))))
	(else '()))
    ,@(map
	(lambda(x)
	  (cond 
	    (((ntype?? '*) x)
	     (sxml:squeeze x))
	    (else x)))
       (sxml:content obj))))

; Eliminates empty lists of attributes and ALL aux-lists for given SXML element 
; <obj> and its descendants
; Returns: minimized and normalized SXML element
(define (sxml:clean obj)
  `(,(sxml:name obj)
   ,@(cond 
	((sxml:attr-list-node obj)
	 => (lambda (atl) 
	      (if (null? (cdr atl)) 
		 '()
	         (list atl))))	
	(else '()))
    ,@(map
	(lambda(x)
	  (cond 
	    (((ntype?? '*) x)
	     (sxml:clean x))
	    (else x)))
       (sxml:content obj))))
;==============================================================================
; SXPath-related 

;------------------------------------------------------------------------------
; Extensions

; select-first-kid:: Pred -> Node -> Node
; Given a Node, return its first child that satisfy
; the test-pred?
; Returns #f if there is no such a child
; select-first-kid:: Pred -> Nodeset -> Node
; The same as above, but select among children of all the nodes in
; the Nodeset
(define (select-first-kid test-pred?)
 (lambda(obj)
  (let rpt ((lst (if (symbol? (car obj)) 
		  (cdr obj)
		  obj)))
    (cond 
      ((null? lst) #f)
      ((and (pair? (car lst))
	    (test-pred? (car lst)))
	(car lst))
      (else (rpt (cdr lst)))) 
    )))

;------------------------------------------------------------------------------
; Fast node-parent 

; Returns a function of one argument - SXML element - which returns its parent
; node using *PARENT* pointer in aux-list
; '*TOP-PTR* may be used as a pointer to root node
; It return an empty list when applyed to root node
(define (sxml:node-parent rootnode)
  (lambda(obj)
  (cond 
    ((sxml:aux-node obj '*PARENT*)
     => (lambda(x)
 	  (if 
 	    (eq? '*TOP-PTR* (cadr x))
	  rootnode
	  ((cadr x)))))
    ((and (pair? obj)
          (eq? (car obj) '*TOP* ))
     '())           
     (else (sxml:error nl "PARENT pointer is absent in: " obj nl)
	   ))))

(define (sxml:add-parents obj . top-ptr)
  (let rpt 
    ((elt obj)
     (p '*TOP*)
     (at-aux (if (eq? (sxml:name obj) '*TOP*)
		(list (cons '@@ (sxml:aux-list-u obj)))
		(list
		  (cons '@ (sxml:attr-list obj))
		  (cons '@@ (cons `(*PARENT* ,(lambda() (car top-ptr))) 
						 (sxml:aux-list obj))))))
     ) ; *TOP* is a parent for top-level element
    (let* ((h (list (sxml:name elt)))
	   (b  (append 
		 at-aux
		 (map
		     (lambda(x)
		       (cond 
			 (((ntype?? '*) x)
			  (rpt x h
			       (list
				 (cons '@ (sxml:attr-list x))
				 (cons '@@ (cons `(*PARENT* ,(lambda() h)) 
						 (sxml:aux-list x))))
			       ))
			 (else x)))
		     (sxml:content elt)))))
      (cons (car h) b))))

; Lookup an element using its ID 
(define (sxml:lookup id index)
    (cond
      ((assoc id index) 
       => cdr)
      (else #f)))

;==============================================================================
; Markup generation

;------------------------------------------------------------------------------
; XML

; Creates the XML markup for attributes.
(define (sxml:attr->xml attr)
   (list " " (sxml:ncname attr)
	 "='" (cadr attr) "'"))

; Return a string or a list of strings where all the occurences of 
; characters < > & " ' in a given string are replaced by corresponding 
; character entity references. See also:  sxml:string->html
(define sxml:string->xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") 
		    (#\" . "&quot;") (#\' . "&apos;"))))

; A version of dispatch-node specialized and optimized for SXML->XML
; transformation.
(define (sxml:sxml->xml tree)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (sxml:sxml->xml a-tree)) 
	  tree))
    ((pair? tree)
     (let* ((name (sxml:name tree))   ; NS (URI-prefixed) not supported
	    (nm (symbol->string name))
	    (content (sxml:content-raw tree)))
	 `("<" ,nm ,@(map sxml:attr->xml (sxml:attr-list tree))
	   ,@(if (null? content) '("/>")
	       `(">" ,@(sxml:sxml->xml content) "</" ,nm ">")))))
    ((string? tree) (sxml:string->xml tree)) ; *text*
    (else (sxml:error "sxml->html - unexpected type of node: " tree))))


;------------------------------------------------------------------------------
; HTML

; Creates the HTML markup for attributes.
(define (sxml:attr->html attr)
	 (if (equal? "" (cadr attr))
             (list " " (sxml:ncname attr))
             (list " " (sxml:ncname attr) "='" (cadr attr) "'")))



; Given a string, check to make sure it does not contain characters
; < > & " that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.
; Borrowed from Oleg Kiselyov's XML-to-HTML.scm (where its name is
; string->goodHTML)
(define sxml:string->html
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


; This predicate yields #t for "unterminated" HTML 4.0 tags
(define (sxml:non-terminated-html-tag? tag) 
  (memq tag 
     '(area base basefont br col frame hr img input isindex link meta param)))


; A version of dispatch-node specialized and optimized for SXML->HTML
; transformation.
(define (sxml:sxml->html tree)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (sxml:sxml->html a-tree)) 
	  tree))
    ((pair? tree)
     (let* ((name (sxml:name tree))
	    (nm (symbol->string name))
	    (content (sxml:content-raw tree)))
	 `("<" ,nm ,@(map sxml:attr->html (sxml:attr-list tree))
	   ,@(if (null? content)
	       (if (sxml:non-terminated-html-tag? name) '(">") '("/>"))
	       `(">" ,@(sxml:sxml->html content) "</" ,nm ">")))))
    ((string? tree) (sxml:string->html tree)) ; *text*
    (else (sxml:error "sxml->html - unexpected type of node: " tree))))


(provide (all-defined)))
