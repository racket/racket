; Module header is generated automatically
#cs(module xpath-context_xlink mzscheme
(require (lib "string.ss" "srfi/13"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")
(require "sxpath-ext.ss")
(require "xpath-parser.ss")
(require "txpath.ss")
(require "xpath-ast.ss")
(require (lib "htmlprag.ss" "web-server/tests/tmp/htmlprag"))
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))

;; Context-based XPath implementation
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; <nodeset> ::= ( <nodeset-member>* )
; <nodeset-member> ::= <node> | <context>
; <context> ::= ( *CONTEXT*  <node>  <ancestor>* )
; <node> - an SXML node (a context node)
; <ancestor>* - context node's parent, grandparent, grandgrandparent etc.
;
; A CONTEXT doesn't contain more ANCESTORs than actually required for
; evaluating the location path. This is achieved by means of an "intellectual"
; parsing of the location path. The number of ANCESTORs stored in the CONTEXT
; can differ for different path steps.

;=========================================================================
; Basic operations over context

; A fast however unsafe predicate
; Assumes that the 'node' provided is a pair
(define (sxml:context-u? node)
  (eq? (car node) '*CONTEXT*))

; Safer predicate
(define (sxml:context? node)
  (and (pair? node) (eq? (car node) '*CONTEXT*)))

;-------------------------------------------------
; Accessors

; Fast however unsafe accessors
; Assume that the argument is the proper context
(define sxml:context->node-u cadr)
(define sxml:context->ancestors-u cddr)
(define sxml:context->content-u cdr)

; Safe accessors
; Can be applied to both a context and an ordinary node
(define (sxml:context->node context)
  (if (sxml:context? context) (cadr context) context))

(define (sxml:context->ancestors context)
  (if (sxml:context? context) (cddr context) '()))

(define (sxml:context->content context)
  (if (sxml:context? context) (cdr context) (list context)))

; Given a context-set, converts it to a nodeset
(define (draft:contextset->nodeset obj)
  (if (nodeset? obj)
      (map sxml:context->node obj)
      obj))

;-------------------------------------------------
; Mutators

; Constructor
(define (draft:make-context node ancestors)
  (cons '*CONTEXT* (cons node ancestors)))

; A smarter constructor
; Makes context only when required, with the 'num-anc' required
(define (draft:smart-make-context node ancestors num-anc)
  (if
   (or (and num-anc (zero? num-anc))
       (null? ancestors))
   node  ; no need for context construction
   (cons '*CONTEXT*
         (cons node
               (draft:list-head ancestors num-anc)))))

; Provided a 'nodeset' of sibling nodes, wraps each into context
; If 'ancestors' is empty, keeps 'nodeset' unchanged
(define (draft:siblings->context-set nodeset ancestors)
  (if (null? ancestors)
      nodeset
      (map
       (lambda (node) (draft:make-context node ancestors))
       nodeset)))

;-------------------------------------------------
; Operations on num-ancestors
; Complexity results from #f as a value for num-ancestors (which means that the
; number of ancestors is infinite)

(define (draft:na+ na1 na2)
  (if
   (or (not na1) (not na2)) ; either argument is infinite
   #f
   (+ na1 na2)))

(define (draft:na-minus na value)
  (if (not na) na (- na value)))

; Minus, with the result that is always non-negative
(define (draft:na-minus-nneg na value)
  (cond
    ((not na) na)
    ((< (- na value) 0) 0)
    (else (- na value))))

(define (draft:na-max . na-lst)
  (cond
    ((null? na-lst) 0)
    ((member #f na-lst) #f)
    (else (apply max na-lst))))

(define (draft:na-min . na-lst)
  (if
   (null? na-lst) 0
   (let ((num-lst (filter (lambda (x) x) na-lst)))
     (if (null? num-lst) #f  ; all na-lst consists of #f
         (apply min num-lst)))))

(define (draft:na> na1 na2)
  (cond
   ((not na2) ; second argument in infinite
    #f)
   ((not na1) ; first argument is infinite
    #t)
   (else  ; niether argument is infinite
    (> na1 na2))))

(define (draft:na>= na1 na2)
  (cond
   ((not na2) ; second argument in infinite
    (not na1))  
   ((not na1) ; first argument is infinite
    #t)
   (else  ; niether argument is infinite
    (>= na1 na2))))


;=========================================================================
; Misc helpers

; Similar to R5RS 'list-tail' but returns the new list consisting of the first
; 'k' members of 'lst'
; If k>(length lst) or k=#f, lst is returned
; NOTE1: k=#f is used in this implementation to represent positive infinity
; NOTE2: Unless k=#f, the result is always a newly allocated list. This is the
;  main methodological difference between this function and R5RS 'list-tail'
(define (draft:list-head lst k)
  (letrec
      ((list-head
        (lambda (lst k)
          (if (or (null? lst) (zero? k))
              '()
              (cons (car lst) (list-head (cdr lst) (- k 1)))))))
    (if k
        (list-head lst k)
        lst)))

; Returns the last member of the list
; It is an error for the list to be empty
(define (draft:list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (draft:list-last (cdr lst))))

; Constructs the (listof value), whose length is num
(define (draft:make-list value num)
  (if (= num 0)
      '()
      (cons value (draft:make-list value (- num 1)))))

; Similar to txp:signal-semantic-error, but returns #f
(define (draft:signal-semantic-error . text)
  (apply txp:signal-semantic-error text)
  #f)

; The top of the SXML document?
(define (draft:top? node)
  (and (pair? node) (eq? (car node) '*TOP*)))

; Removes eq duplicates from the nodeset
(define (draft:remove-eq-duplicates nodeset)
  (cond
    ((null? nodeset) nodeset)
    ((memq (car nodeset) (cdr nodeset))
     (draft:remove-eq-duplicates (cdr nodeset)))
    (else
     (cons (car nodeset) (draft:remove-eq-duplicates (cdr nodeset))))))

; Reaches the root of the root of the contextset
; Result: nodeset
(define (draft:reach-root contextset)
  (let ((nodeset (map
                  (lambda (node)
                    (if
                     (sxml:context? node)
                     (draft:list-last (sxml:context->ancestors-u node))
                     node))
                  contextset)))
    (if (or (null? nodeset) (null? (car nodeset)))  ; (length nodeset)<=1
        nodeset
        (draft:remove-eq-duplicates nodeset))))

; Recovers context for each node of the nodeset
; Context recovery is performed in its usual technique: searching from the
; root of the document. As a result, this function can be fairly slow.
; In this implementation, it is sometimes called after an XPath 'id' function,
; for location paths like  "id(name)/.."
; By nature of 'id-index', context is lost when we access elements by their
; ID. It may be a good idea to rework the structure of 'id-index' to make it
; more suitable for purposes of this context-based XPath implementation.
; A good news is that only a few elements are usually selected by XPath 'id'
; function, thus the overhead of searching from the root node might be
; acceptable in this case. 
(define (draft:recover-contextset nodeset root-node num-anc)
  (map
   (lambda (node)
     (draft:smart-make-context
      node
      (((sxml:ancestor (lambda (x) #t)) root-node) node)
      num-anc))
   nodeset))

;-------------------------------------------------
; For sxpath: handling a procedure as a location step

; Makes a context-set from a nodeset supplied, with the num-anc required
;  ancestors-set ::= (listof ancestors)
;  ancestors ::= (listof node)
; Members of the nodeset are known to be descendants-or-selves of
; (map car ancestors-set)
(define (draft:find-proper-context nodeset ancestors-set num-anc)
  (map
   (lambda (node)
     (if
      (sxml:context? node)  ; already a context
      node  ; nothing to be done  
      (let loop ((this-level ancestors-set)
                 (next-level '()))
        (if
         (null? this-level)  ; this level fully analyzed
         (if (null? next-level)  ; failed to find
             node
             (loop next-level '()))
         (let ((ancestors (car this-level)))
           (if
            (eq? node (car ancestors))  ; proper ancestors found
            (draft:make-context
             node
             (draft:list-head (cdr ancestors) num-anc))
            (loop (cdr this-level)
                  (append
                   (map
                    (lambda (n) (cons n ancestors))
                    ((sxml:child sxml:node?) (car ancestors)))
                   (map
                    (lambda (n) (cons n ancestors))
                    ((sxml:attribute (lambda (x) #t)) (car ancestors)))
                   next-level))))))))
   nodeset))


;=========================================================================
; XPath axes
; Implementation is based on the concept of context
; Compared to "general" SXPath, a new optional argument was added:
;  NUM-ANCESTORS - number of node's ancestors that will be required later in
;  the location path. For example, NUM-ANCESTORS=1 means that the node's parent
;  only must be remembered in the CONTEXT, grandparents will not be required

; Ancestor axis
(define (draft:ancestor test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((ancs-to-view (sxml:context->ancestors-u node))
                        (res '()))
               (cond
                 ((null? ancs-to-view)  ; processed everyone
                  (reverse res)  ; reverse document order required
                  )
                 ((test-pred? (car ancs-to-view))  ; can add it to result
                  (loop
                   (cdr ancs-to-view)
                   (cons
                    (draft:smart-make-context
                     (car ancs-to-view) (cdr ancs-to-view) num-anc)
                    res)))
                 (else  ; current node doesn't satisfy the predicate
                  (loop (cdr ancs-to-view) res))))
             '()  ; no ancestors
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Ancestor-or-self axis
(define (draft:ancestor-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((sxml:context? node)
               (let loop ((ancs-to-view (sxml:context->content-u node))
                          (res '()))
                 (cond
                   ((null? ancs-to-view)  ; processed everyone
                    (reverse res)  ; reverse document order required
                    )
                   ((test-pred? (car ancs-to-view))  ; can add it to result
                    (loop
                     (cdr ancs-to-view)
                     (cons
                      (draft:smart-make-context
                       (car ancs-to-view) (cdr ancs-to-view) num-anc)
                      res)))
                   (else  ; current node doesn't satisfy the predicate
                    (loop (cdr ancs-to-view) res)))))
              ; ordinary SXML node
              ((test-pred? node)  ; satisfies the predicate
               (list node))
              (else
               '())))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Attribute axis
; Borrows much from draft:child
(define (draft:attribute test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no attributes
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((sxml:filter test-pred?)
                 (sxml:attr-list (sxml:context->node-u node)))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              (else  ; an ordinary node, and is a pair
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (sxml:attr-list node))
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))  

; Child axis
(define (draft:child test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no children
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((select-kids test-pred?) (sxml:context->node-u node))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              ; an ordinary node, and is a pair
              ((memq (car node) '(*PI* *COMMENT* *ENTITY*))
               '())
              (else
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (cdr node))  ; like in 'select-kids'
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Descendant axis
(define (draft:descendant test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (draft:child sxml:node? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset                        
            (let rpt ((res '())
                      (more (child node)))
              (if (null? more)
                  (reverse res)
                  (rpt
                   (if (test-pred? (sxml:context->node (car more)))
                       (cons (car more) res)
                       res)
                   (append (child (car more)) (cdr more))))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))
                        
; Descendant-or-self axis
(define (draft:descendant-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (draft:child sxml:node? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset                        
            (let rpt ((res '())
                      (more (list node)))
              (if (null? more)
                  (reverse res)
                  (rpt
                   (if (test-pred? (sxml:context->node (car more)))
                       (cons (car more) res)
                       res)
                   (append (child (car more)) (cdr more))))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Following axis
(define (draft:following test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((curr-node (sxml:context->node-u node))
                        (ancs-to-view (sxml:context->ancestors-u node))
                        (res '()))
               (if
                (null? ancs-to-view)  ; processed everyone                 
                res
                (loop
                 (car ancs-to-view)
                 (cdr ancs-to-view)
                 (append
                  res
                  (descend
                   (draft:siblings->context-set
                    (cond
                       ((memq curr-node
                        (cdr  ; parent is an element => cdr gives its children
                         (car ancs-to-view)))
                        => cdr)
                       (else  ; curr-node is an attribute node
                        ((select-kids sxml:node?) (car ancs-to-view))))
                     (draft:list-head ancs-to-view num-anc)))))))
             '()  ; no following members
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Following-sibling axis
(define (draft:following-sibling test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)                  
                  (not (null? (sxml:context->ancestors-u node))))
             (cond
               ((memq (sxml:context->node-u node)
                      (cdr  ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u node))))
                => (lambda (foll-siblings)
                     (draft:siblings->context-set
                      ((sxml:filter test-pred?) (cdr foll-siblings))
                      (draft:list-head
                       (sxml:context->ancestors-u node) num-anc))))
               (else  ; no following siblings
                '()))
             '()  ; no parent => no siblings
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Namespace axis
; Borrows much from draft:child
(define (draft:namespace test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no namespaces
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((sxml:filter test-pred?)
                 (sxml:ns-list (sxml:context->node-u node)))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              (else  ; an ordinary node, and is a pair
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (sxml:ns-list node))
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Parent axis
(define (draft:parent test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)
                  (not (null? (sxml:context->ancestors-u node)))
                  (test-pred? (car (sxml:context->ancestors-u node))))
             (draft:smart-make-context
              (car (sxml:context->ancestors-u node))
              (cdr (sxml:context->ancestors-u node))
              num-anc)
             '()  ; no parent
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Preceding axis
(define (draft:preceding test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((curr-node (sxml:context->node-u node))
                        (ancs-to-view (sxml:context->ancestors-u node))                        
                        (to-descend '()))
               (cond
                 ((null? ancs-to-view)  ; processed everyone
                  (map-union
                   (lambda (node) (reverse (descend node)))
                   to-descend))
                 ((memq curr-node
                        (reverse
                         ((select-kids sxml:node?)
                          (car ancs-to-view))))
                  => (lambda (prec-siblings)
                       (loop
                        (car ancs-to-view)
                        (cdr ancs-to-view)
                        (append
                         to-descend
                         (draft:siblings->context-set
                           (cdr prec-siblings)
                           (draft:list-head ancs-to-view num-anc))))))
                 (else  ; no preceding siblings
                  (loop (car ancs-to-view)
                        (cdr ancs-to-view)
                        to-descend))))
             '()  ; no preceding members
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Preceding-sibling axis
(define (draft:preceding-sibling test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)                  
                  (not (null? (sxml:context->ancestors-u node))))
             (cond
               ((memq (sxml:context->node-u node)
                      (reverse
                       (cdr  ; parent is an element => cdr gives its children
                        (car (sxml:context->ancestors-u node)))))
                => (lambda (prec-siblings)
                     (draft:siblings->context-set
                      ((sxml:filter test-pred?) (cdr prec-siblings))
                      (draft:list-head
                       (sxml:context->ancestors-u node) num-anc))))
               (else  ; no preceding siblings
                '()))
             '()  ; no parent => no siblings
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Self axis
; num-ancestors is not used here
(define (draft:self test-pred? . num-ancestors)
  (sxml:filter
   (lambda (node) (test-pred? (sxml:context->node node)))))


;==========================================================================
; XPath Core Function Library

;-------------------------------------------------
; 4.1 Node Set Functions

; last()
(define (draft:core-last num-anc)
  (lambda (nodeset position+size var-binding)
    (cdr position+size)))
  
; position()
(define (draft:core-position num-anc)
  (lambda (nodeset position+size var-binding)
    (car position+size)))

; count(node-set)
(define (draft:core-count num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
        ((nodeset? res) (length res))
        (else
         (sxml:xpointer-runtime-error
          "count() function - an argument is not a nodeset")
         0)))))

; id(object)
(define (draft:core-id num-anc arg-func) 
  (lambda (nodeset position+size var-binding)    
    (let* ((root-node (draft:reach-root nodeset))
           (id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)  ; no id-index
       '()  ; ID function returns an empty nodeset
       (let ((res ((sxml:id (cdar id-nset))  ; implemented in "sxpath-ext.scm"
                   (draft:contextset->nodeset
                    (arg-func nodeset position+size var-binding)))))
         (if (and num-anc (zero? num-anc))  ; no ancestors required
             res
             (draft:recover-contextset res root-node num-anc)))))))

; local-name(node-set?)
(define (draft:core-local-name num-anc . arg-func)  ; optional argument 
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (let ((name (symbol->string (caar nodeset))))
               (cond
                 ((string-rindex name #\:)
                  => (lambda (pos)
                       (substring name (+ pos 1) (string-length name))))
                 (else  ; a NCName
                  name)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")              
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring
                          name (+ pos 1) (string-length name))))
                   (else  ; a NCName
                    name))))))))))

; namespace-uri(node-set?)
(define (draft:core-namespace-uri num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (let ((name (symbol->string (caar nodeset))))
               (cond
                 ((string-rindex name #\:)
                  => (lambda (pos)
                       (substring name 0 pos)))
                 (else  ; a NCName
                  "")))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))           
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring name 0 pos)))
                   (else ""))))))))))

; name(node-set?)
(define (draft:core-name num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (symbol->string (caar nodeset))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))        
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (symbol->string (caar obj)))))))))


;-------------------------------------------------
; 4.2 String Functions

; string(object?)
(define (draft:core-string num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (sxml:string
         (draft:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (sxml:string
           (draft:contextset->nodeset
            (func nodeset position+size var-binding)))))))

; concat(string, string, string*)
(define (draft:core-concat num-anc . arg-func-lst)
  (lambda (nodeset position+size var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (sxml:string
         (draft:contextset->nodeset
          (f nodeset position+size var-binding))))
      arg-func-lst))))

; starts-with(string, string)
(define (draft:core-starts-with num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      (string-prefix? str2 str1))))

; contains(string, string)
(define (draft:core-contains num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      (if (substring? str2 str1) #t #f)  ; must return a boolean
      )))
  
; substring-before(string, string)
(define (draft:core-substring-before num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (substring? str2 str1)))
      (if (not pos)  ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

; substring-after(string, string)
(define (draft:core-substring-after num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (substring? str2 str1)))
      (if
       (not pos)  ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

; substring(string, number, number?)
(define (draft:core-substring num-anc arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)  ; no third argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((str (sxml:string
                    (draft:contextset->nodeset
                     (arg-func1 nodeset position+size var-binding))))
              (num1 (sxml:number
                     (draft:contextset->nodeset
                      (arg-func2 nodeset position+size var-binding)))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset position+size var-binding)
          (let ((str (sxml:string
                      (draft:contextset->nodeset
                       (arg-func1 nodeset position+size var-binding))))
                (num1 (sxml:number
                       (draft:contextset->nodeset
                        (arg-func2 nodeset position+size var-binding))))
                (num2 (sxml:number
                       (draft:contextset->nodeset
                        (arg-func3 nodeset position+size var-binding)))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

; string-length(string?)
(define (draft:core-string-length num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (string-length
         (sxml:string (draft:contextset->nodeset nodeset))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (string-length
           (sxml:string
            (draft:contextset->nodeset
             (func nodeset position+size var-binding))))))))

; normalize-space(string?)
(define (draft:core-normalize-space num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let rpt ((src (string-split
                        (sxml:string (draft:contextset->nodeset nodeset))
                        sxml:whitespace))
                  (res '()))
          (cond
            ((null? src)
             (apply string-append (reverse res)))
            ((= (string-length (car src)) 0)  ; empty string
             (rpt (cdr src) res))
            ((null? res)
             (rpt (cdr src) (cons (car src) res)))
            (else
             (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((src (string-split
                          (sxml:string
                           (draft:contextset->nodeset
                            (func nodeset position+size var-binding)))
                          sxml:whitespace))
                    (res '()))
            (cond
              ((null? src)
               (apply string-append (reverse res)))
              ((= (string-length (car src)) 0)  ; empty string
               (rpt (cdr src) res))
              ((null? res)
               (rpt (cdr src) (cons (car src) res)))
              (else
               (rpt (cdr src) (cons (car src) (cons " " res))))))))))

; translate(string, string, string)
(define (draft:core-translate num-anc arg-func1 arg-func2 arg-func3)
  (lambda (nodeset position+size var-binding)    
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding))))
          (str3 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func3 nodeset position+size var-binding)))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                 ((null? lst2) (reverse alist))
                 ((null? lst3)
                  (append
                   (reverse alist)
                   (map
                    (lambda (ch) (cons ch #f))
                    lst2)))
                 (else
                  (while
                   (cdr lst2)
                   (cdr lst3)
                   (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
            ((null? lst1) (list->string (reverse res)))
            ((assoc (car lst1) alist)
             => (lambda (pair)
                  (if (cdr pair)
                      (rpt (cdr lst1) (cons (cdr pair) res))
                      (rpt (cdr lst1) res))))
            (else
             (rpt (cdr lst1) (cons (car lst1) res)))))))))
  

;-------------------------------------------------
; 4.3 Boolean Functions

; boolean(object)
(define (draft:core-boolean num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (sxml:boolean
     (arg-func nodeset position+size var-binding))))

; not(boolean)
(define (draft:core-not num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (not (sxml:boolean 
          (arg-func nodeset position+size var-binding)))))

; true()
(define (draft:core-true num-anc)
  (lambda (nodeset position+size var-binding) #t))

; false()
(define (draft:core-false num-anc)
  (lambda (nodeset position+size var-binding) #f))

; lang(string)
(define (draft:core-lang num-anc arg-func)
  (lambda (nodeset position+size var-binding)    
    (let ((arg (sxml:string
                (draft:contextset->nodeset
                 (arg-func nodeset position+size var-binding))))
          (lng
           ((draft:child (ntype?? '*text*))
            ((draft:attribute (ntype?? 'xml:lang))
             ((draft:ancestor-or-self (lambda (x) #t))
              (car nodeset)  ; context-node = (car nodeset)
              )))))
      (and (not (null? lng))
           (or (string-ci=? arg (car lng))
               (string-prefix-ci? (string-append arg "-") (car lng)))))))       
  

;-------------------------------------------------
; 4.4 Number Functions

; number(object?)
(define (draft:core-number num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (sxml:number (draft:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (sxml:number
           (draft:contextset->nodeset
            (func nodeset position+size var-binding)))))))

; sum(node-set)
(define (draft:core-sum num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
        ((nodeset? res)
         (apply +
                (map
                 (lambda (node)
                   (sxml:number
                    (sxml:string-value (sxml:context->node node))))
                 res)))
        (else
         (sxml:xpointer-runtime-error
          "SUM function - an argument is not a nodeset")
         0)))))

; floor(number)
(define (draft:core-floor num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (floor (sxml:number
             (draft:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))

; ceiling(number)
(define (draft:core-ceiling num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (ceiling (sxml:number
               (draft:contextset->nodeset
                (arg-func nodeset position+size var-binding)))))))

; round(number)
(define (draft:core-round num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (round (sxml:number
             (draft:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))


;=========================================================================
; XPath AST processing
; AST is considered to be properly formed
       
; {5} <AxisSpecifier> ::= (axis-specifier  <AxisName> )
; {6} <AxisName> ::= (ancestor)
;                    | (ancestor-or-self)
;                    | (attribute)
;                    | (child)
;                    | (descendant)
;                    | (descendant-or-self)
;                    | (following)
;                    | (following-sibling)
;                    | (namespace)
;                    | (parent)
;                    | (preceding)
;                    | (preceding-sibling)
;                    | (self)
;                    | (arc)  ; the following 3 are added by SXLink
;                    | (traverse)
;                    | (traverse-arc)
; Returns:  (list lambda num-ancestors pass-vars?)
;  pass-vars? - a boolean: whether var-bindings must be passed to the axis
(define (draft:ast-axis-specifier op num-anc)
  (if
   (not (eq? (car op) 'axis-specifier))
   (draft:signal-semantic-error "not an AxisSpecifier - " op)
   (case (caadr op)  ; AxisName
     ((ancestor)
      (list draft:ancestor #f #f))
     ((ancestor-or-self)
      (list draft:ancestor-or-self #f #f))
     ((attribute)
      (list draft:attribute (draft:na-minus-nneg num-anc 1) #f))
     ((child)
      (list draft:child (draft:na-minus-nneg num-anc 1) #f))
     ((descendant)
      (list draft:descendant (draft:na-minus-nneg num-anc 1) #f))
     ((descendant-or-self)
      (list draft:descendant-or-self num-anc #f))
     ((following)
      (list draft:following #f #f))
     ((following-sibling)
      (list draft:following-sibling (draft:na-max num-anc 1) #f))
     ((namespace)
      (list draft:namespace (draft:na-minus-nneg num-anc 1) #f))
     ((parent)
      (list draft:parent (draft:na+ num-anc 1) #f))
     ((preceding)
      (list draft:preceding #f #f))
     ((preceding-sibling)
      (list draft:preceding-sibling (draft:na-max num-anc 1) #f))
     ((self)
      (list draft:self num-anc #f))
     ((arc)
      (list xlink:axis-arc #f #f))
     ((traverse)
      (list xlink:axis-traverse #f #t))
     ((traverse-arc)
      (list xlink:axis-traverse-arc #f #t))
     (else
      (draft:signal-semantic-error "unknown AxisName - " op)))))


; {7} <NodeTest> ::= (node-test (*))
;                    | (node-test (namespace-uri  <String> ))
;                    | (node-test (namespace-uri  <String> )?
;                                 (local-name  <String> ))
;                    | (node-test (comment))
;                    | (node-test (text))
;                    | (node-test (pi <String>? ))
;                    | (node-test (point))
;                    | (node-test (range))
; + added by sxpath native syntax:
;                    | (node-test (equal?  <SXML-node> ))
;                    | (node-test (eq?  <SXML-node> ))
;                    | (node-test (names  <String>+ ))
;                    | (node-test (not-names  <String>+ ))s
(define (draft:ast-node-test op)
  (if
   (not (eq? (car op) 'node-test))
   (draft:signal-semantic-error "not an NodeTest - " op)
   (case (caadr op)  ; NodeTest name
     ((*)
      (ntype?? '*))
     ((namespace-uri)
      (cond
        ((= (length op) 2)  ; NodeTest in the form of prefix:*
         (ntype-namespace-id?? (cadadr op)))
        ((eq? (caaddr op) 'local-name)
         (ntype?? (string->symbol
                   (string-append (cadadr op) ":" (cadr (caddr op))))))
        (else
         (draft:signal-semantic-error "improper QName NodeTest - " op))))
     ((local-name)      
      (ntype?? (string->symbol (cadadr op))))
     ((comment)
      (ntype?? '*COMMENT*))
     ((text)
      (ntype?? '*text*))
     ((pi)
      (if (= (length (cadr op)) 2)  ; PI target supplied
          (let ((target (string->symbol (cadadr op))))
            (lambda (node)
              (and (pair? node)
                   (eq? (car node) '*PI*)
                   (equal? (cadr node) target))))
          (lambda (node)
            (and (pair? node) (eq? (car node) '*PI*)))))
     ((node) sxml:node?)
     ((point)
      (draft:signal-semantic-error
       "point() NodeTest is not supported by this implementation"))
     ((range)
      (draft:signal-semantic-error
       "range() NodeTest is not supported by this implementation"))
     ((equal?)
      (node-equal? (cadadr op)))
     ((eq?)
      (node-eq? (cadadr op)))
     ((names)
      (ntype-names?? (cdadr op)))
     ((not-names)
      (sxml:complement (ntype-names?? (cdadr op))))
     (else
      (draft:signal-semantic-error "unknown NodeTest - " op)))))

;-------------------------------------------------
; In this section, each function accepts 2 arguments
;  op - S-expression which represents the operation
;  num-anc - how many ancestors are required in the context after that
;            operation
; and returns either #f, which signals of a semantic error, or
;  (cons (lambda (nodeset position+size var-binding) ...)
;        num-anc-it-requires)
;  position+size - the same to what was called 'context' in TXPath-1  
  
; {1} <LocationPath> ::= <RelativeLocationPath>
;                        | <AbsoluteLocationPath>
(define (draft:ast-location-path op num-anc)
  (case (car op)
    ((absolute-location-path)
     (draft:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (draft:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

; {2} <AbsoluteLocationPath> ::= (absolute-location-path  <Step>* )
(define (draft:ast-absolute-location-path op num-anc)
  (cond
    ((not (eq? (car op) 'absolute-location-path))
     (draft:signal-semantic-error "not an AbsoluteLocationPath - " op))
    ((null? (cdr op))  ; no Steps
     (cons
      (lambda (nodeset position+size var-binding)
        (draft:reach-root nodeset))
      #f))
    (else
     (and-let*
      ((steps-res (draft:ast-step-list (cdr op) num-anc)))
      (cons
       (if
        (null? (cdar steps-res))  ; only a single step
        (let ((step-impl (caar steps-res)))
          (lambda (nodeset position+size var-binding)
            (step-impl
             (draft:reach-root nodeset) position+size var-binding)))
        (let ((converters (car steps-res)))
          (lambda (nodeset position+size var-binding)
            (let rpt ((nset (draft:reach-root nodeset))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs)))))))
       #f)))))

; {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (draft:ast-relative-location-path op num-anc)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res (draft:ast-step-list (cdr op) num-anc)))
    (cons
     (if
      (null? (cdar steps-res))  ; only a single step
      (caar steps-res)
      (let ((converters (car steps-res)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((nset nodeset)
                    (fs converters))
            (if (null? fs)
                nset
                (rpt ((car fs) nset position+size var-binding)
                     (cdr fs)))))))
     (cdr steps-res)))))

; {4} <Step> ::= (step  <AxisSpecifier> <NodeTest> <Predicate>* )
;                | (range-to  (expr <Expr>)  <Predicate>* )
(define (draft:ast-step op num-anc)
  (cond
    ((eq? (car op) 'range-to)
     (draft:signal-semantic-error "range-to function not implemented"))
    ((eq? (car op) 'filter-expr)  ; can be produced by sxpath
     (draft:ast-filter-expr op num-anc))
    ((eq? (car op) 'lambda-step)  ; created by sxpath
     (cons
      (let ((proc (cadr op)))
        (if
         (and num-anc (zero? num-anc))  ; no ancestors required
         (lambda (node position+size var-binding)
           (proc (draft:contextset->nodeset (as-nodeset node))
                 var-binding))
         (lambda (node position+size var-binding)
           (draft:find-proper-context
            (proc (draft:contextset->nodeset (as-nodeset node))
                  var-binding)
            (append
             (map sxml:context->content (as-nodeset node))
             (apply append   ; nodes that can be obtained through var values
                    (map
                     (lambda (pair)
                       (if (nodeset? (cdr pair))
                           (map sxml:context->content (cdr pair))
                           '()))
                     var-binding)))
            num-anc))))
      num-anc))
    ((eq? (car op) 'step)
     (if
      (null? (cdddr op))  ; no Predicates
      (and-let*
       ((axis-lst (draft:ast-axis-specifier (cadr op) num-anc))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst) ntest num-anc)))
         (cons
          (if (caddr axis-lst)  ; var-binding is to be passed
              (lambda (nodeset position+size var-binding)
                (axis nodeset var-binding))
              (lambda (nodeset position+size var-binding)
                (axis nodeset)))
          (cadr axis-lst))))
      (and-let*
       ((preds-res (draft:ast-predicate-list (cdddr op) 0))
        (axis-lst (draft:ast-axis-specifier
                   (cadr op) (draft:na-max num-anc (cdr preds-res))))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst)
                    ntest (draft:na-max num-anc (cdr preds-res))))
             (pred-impl-lst (car preds-res)))
         (cons
          (if
           (caddr axis-lst)  ; variables are to be passed to the axis
           (lambda (nodeset position+size var-binding)
             (map-union
              (lambda (node)
                (let loop ((nset (axis node var-binding))
                           (preds pred-impl-lst))
                  (if
                   (null? preds)
                   nset
                   (loop ((car preds) nset position+size var-binding)
                         (cdr preds)))))
              nodeset))
           (lambda (nodeset position+size var-binding)
             (map-union
              (lambda (node)
                (let loop ((nset (axis node))
                           (preds pred-impl-lst))
                  (if
                   (null? preds)
                   nset
                   (loop ((car preds) nset position+size var-binding)
                         (cdr preds)))))
              nodeset)))
          (cadr axis-lst))))))
    (else
     (draft:signal-semantic-error "not a Step - " op))))

; {4a} ( <Step>+ )
; Returns (cons (listof step-impl) num-anc) or #f
(define (draft:ast-step-list step-lst num-anc)
  (let loop ((steps-to-view (reverse step-lst))
             (res-lst '())
             (num-anc num-anc))
    (if
     (null? steps-to-view)  ; everyone processed
     (cons res-lst num-anc)
     (and-let*
      ((step-res (draft:ast-step (car steps-to-view) num-anc)))
      (loop
       (cdr steps-to-view)
       (cons (car step-res) res-lst)
       (cdr step-res))))))

; {8} <Predicate> ::= (predicate  <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (draft:ast-predicate op num-anc)
  (if
   (not (eq? (car op) 'predicate))
   (draft:signal-semantic-error "not an Predicate - " op)
   (and-let*
    ((expr-res (draft:ast-expr (cadr op) 0)))
    (let ((pred (car expr-res)))
      (cons
       (lambda (nodeset position+size var-binding)         
         (if
          (null? nodeset)  ; already empty
          nodeset  ; nothing to filter
          (let ((size (length nodeset)))  ; context size              
            (let loop ((nset nodeset)
                       (res '())
                       (pos 1))
              (if
               (null? nset)
               (reverse res)
               (let ((value (pred (list (car nset))
                                  (cons pos size)
                                  var-binding)))
                 (loop (cdr nset)
                       (if (if (number? value)
                               (= value pos)
                               (sxml:boolean value))
                           (cons (car nset) res)
                           res)
                       (+ pos 1))))))))
       (cdr expr-res))))))

; {8a} ( <Predicate>+ )
; Returns (cons (listof pred-impl) num-anc) or #f
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (draft:ast-predicate-list op-lst num-anc)  
  (let ((pred-res-lst
         (map
          (lambda (op) (draft:ast-predicate op 0))
          op-lst)))
    (if
     (member #f pred-res-lst)  ; error detected
     #f
     (cons
      (map car pred-res-lst)
      (apply draft:na-max (map cdr pred-res-lst))))))

; {9} <Expr> ::= <OrExpr>
;                | <AndExpr>
;                | <EqualityExpr>
;                | <RelationalExpr>
;                | <AdditiveExpr>
;                | <MultiplicativeExpr>
;                | <UnionExpr>
;                | <PathExpr>
;                | <FilterExpr>
;                | <VariableReference>
;                | <Literal>
;                | <Number>
;                | <FunctionCall>
;                | <LocationPath>
(define (draft:ast-expr op num-anc)
  (case (car op)
    ((or)
     (draft:ast-or-expr op num-anc))
    ((and)
     (draft:ast-and-expr op num-anc))
    ((= !=)
     (draft:ast-equality-expr op num-anc))
    ((< > <= >=)
     (draft:ast-relational-expr op num-anc))
    ((+ -)
     (draft:ast-additive-expr op num-anc))
    ((* div mod)
     (draft:ast-multiplicative-expr op num-anc))
    ((union-expr)
     (draft:ast-union-expr op num-anc))
    ((path-expr)
     (draft:ast-path-expr op num-anc))
    ((filter-expr)
     (draft:ast-filter-expr op num-anc))
    ((variable-reference)
     (draft:ast-variable-reference op num-anc))
    ((literal)
     (draft:ast-literal op num-anc))
    ((number)
     (draft:ast-number op num-anc))
    ((function-call)
     (draft:ast-function-call op num-anc))
    ((absolute-location-path)
     (draft:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (draft:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "unknown Expr - " op))))

; {10} <OrExpr> ::= (or <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for OrExpr
(define (draft:ast-or-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #f)
            ((sxml:boolean ((car fs) nodeset position+size var-binding)) #t)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))
     
; {11} <AndExpr> ::= (and <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for AndExpr
(define (draft:ast-and-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #t)
            ((not
              (sxml:boolean ((car fs) nodeset position+size var-binding)))
             #f)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {12} <EqualityExpr> ::= (=  <Expr> <Expr> )
;                         | (!=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for EqualityExpr
(define (draft:ast-equality-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((cmp-op (cadr (assq (car op) `((= ,sxml:equal?)
                                        (!= ,sxml:not-equal?)))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {13} <RelationalExpr> ::= (<  <Expr> <Expr> )
;                           | (>  <Expr> <Expr> )
;                           | (<=  <Expr> <Expr> )
;                           | (>=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for RelationalExpr
(define (draft:ast-relational-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((cmp-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((< ,<) (> ,>) (<= ,<=) (>= ,>=))))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {14} <AdditiveExpr> ::= (+  <Expr> <Expr> )
;                         | (-  <Expr> <Expr>? )
; NOTE: num-anc is dummy here, since it is always 0 for AdditiveExpr
(define (draft:ast-additive-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((add-op (cadr (assq (car op) `((+ ,+) (- ,-)))))
           (expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (apply
         add-op
         (map
          (lambda (expr)
            (sxml:number
             (draft:contextset->nodeset
              (expr nodeset position+size var-binding))))
          expr-impls)))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {15} <MultiplicativeExpr> ::= (*  <Expr> <Expr> )
;                               | (div  <Expr> <Expr> )
;                               | (mod  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for MultiplicativeExpr
(define (draft:ast-multiplicative-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((mul-op
          (cadr (assq (car op) `((* ,*) (div ,/) (mod ,remainder)))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (mul-op
         (sxml:number
          (draft:contextset->nodeset
           (left nodeset position+size var-binding)))
         (sxml:number
          (draft:contextset->nodeset
           (right nodeset position+size var-binding)))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {16} <UnionExpr> ::= (union-expr  <Expr> <Expr>+ )
(define (draft:ast-union-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((res '())
                  (fs expr-impls))
          (if
           (null? fs)
           res
           (let ((nset ((car fs) nodeset position+size var-binding)))
             (rpt
              (append 
               res
               (cond
                 ((not (nodeset? nset))
                  (sxml:xpointer-runtime-error 
                   "expected - nodeset instead of " nset)
                  '())
                 (else nset)))
              (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {17} <PathExpr> ::= (path-expr  <FilterExpr> <Step>+ )
(define (draft:ast-path-expr op num-anc)
  (and-let*
    ((steps-res (draft:ast-step-list (cddr op) num-anc))
     (filter-lst (draft:ast-filter-expr (cadr op) (cdr steps-res))))
    (let ((init-impl (car filter-lst))
          (converters (car steps-res)))
      (cons       
        (lambda (nodeset position+size var-binding)
          (let ((nset
                 (init-impl nodeset position+size var-binding)))
            (let rpt ((nset 
                       (cond
                         ((nodeset? nset) nset)
                         (else
                          (sxml:xpointer-runtime-error 
                           "expected - nodeset instead of " nset)
                          '())))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs))))))
        (cdr filter-lst)))))

; {18} <FilterExpr> ::= (filter-expr (primary-expr  <Expr> )
;                                    <Predicate>* )
(define (draft:ast-filter-expr op num-anc)
  (cond
    ((not (eq? (car op) 'filter-expr))
     (draft:signal-semantic-error "not an FilterExpr - " op))
    ((not (eq? (caadr op) 'primary-expr))
     (draft:signal-semantic-error "not an PrimaryExpr - " (cadr op)))
    ((null? (cddr op))  ; no Predicates
     (draft:ast-expr (cadadr op) num-anc))
    (else
     (and-let*
       ((preds-res (draft:ast-predicate-list (cddr op) 0))
        (expr-lst (draft:ast-expr
                   (cadadr op) (draft:na-max num-anc (cdr preds-res)))))
       (let ((expr-impl (car expr-lst))
             (pred-impl-lst (car preds-res)))
         (cons
          (lambda (nodeset position+size var-binding)
            (let ((prim-res (expr-impl nodeset position+size var-binding)))              
              (let loop ((nset (cond
                                 ((nodeset? prim-res) prim-res)
                                 (else 
                                  (sxml:xpointer-runtime-error 
                                   "expected - nodeset instead of " prim-res)
                                  '())))
                         (preds pred-impl-lst))
                (if
                 (null? preds)
                 nset
                 (loop ((car preds) nset position+size var-binding)
                       (cdr preds))))))
            (cdr expr-lst)))))))     

; {19} <VariableReference> ::= (variable-reference  <String> )
(define (draft:ast-variable-reference op num-anc)
  (let ((name (string->symbol (cadr op))))
    (cons
     (lambda (nodeset position+size var-binding)
       (cond
         ((assoc name var-binding)
          => cdr)
         (else
          (sxml:xpointer-runtime-error "unbound variable - " name)
          '())))
     0)))

; {20} <Literal> ::= (literal  <String> )
(define (draft:ast-literal op num-anc)
  (let ((literal (cadr op)))
    (cons
     (lambda (nodeset position+size var-binding) literal)
     0)))
     
; {21} <Number> :: (number  <Number> )
(define (draft:ast-number op num-anc)
  (let ((number (cadr op)))
    (cons
     (lambda (nodeset position+size var-binding) number)
     0)))

; {22} <FunctionCall> ::= (function-call (function-name  <String> )
;                                        (argument  <Expr> )* )
(define (draft:ast-function-call op num-anc)
  (let ((core-alist
         ; (list fun-name min-num-args max-num-args na4res impl)
         `((last 0 0 0 ,draft:core-last)
           (position 0 0 0 ,draft:core-position)
           (count 1 1 0 ,draft:core-count)
           (id 1 1 #f ,draft:core-id)
           (local-name 0 1 0 ,draft:core-local-name)
           (namespace-uri 0 1 0 ,draft:core-namespace-uri)
           (name 0 1 0 ,draft:core-name)
           (string 0 1 0 ,draft:core-string)
           (concat 2 -1 0 ,draft:core-concat)
           (starts-with 2 2 0 ,draft:core-starts-with)
           (contains 2 2 0 ,draft:core-contains)
           (substring-before 2 2 0 ,draft:core-substring-before)
           (substring-after 2 2 0 ,draft:core-substring-after)
           (substring 2 3 0 ,draft:core-substring)
           (string-length 0 1 0 ,draft:core-string-length)
           (normalize-space 0 1 0 ,draft:core-normalize-space)
           (translate 3 3 0 ,draft:core-translate)
           (boolean 1 1 0 ,draft:core-boolean)
           (not 1 1 0 ,draft:core-not)
           (true 0 0 0 ,draft:core-true)
           (false 0 0 0 ,draft:core-false)
           (lang 1 1 #f ,draft:core-lang)
           (number 0 1 0 ,draft:core-number)
           (sum 1 1 0 ,draft:core-sum)
           (floor 1 1 0 ,draft:core-floor)
           (ceiling 1 1 0 ,draft:core-ceiling)
           (round 1 1 0 ,draft:core-round))))
    (cond
      ((not (eq? (caadr op) 'function-name))
       (draft:signal-semantic-error "not an FunctionName - " (cadr op)))
      ((assq (string->symbol (cadadr op)) core-alist)       
       => (lambda (description)  ; Core function found
            (cond
              ((< (length (cddr op)) (cadr description))
               (draft:signal-semantic-error
                "too few arguments for the Core Function call - "
                (cadadr op)))
              ((and (>= (caddr description) 0)
                    (> (length (cddr op)) (caddr description)))
               (draft:signal-semantic-error
                "too many arguments for the Core Function call - "
                (cadadr op)))
              (else  ; correct number of arguments
               (and-let*
                ((args-impl (draft:ast-function-arguments (cddr op))))
                (cons
                 ; Producing a function implementation
                 (apply (list-ref description 4) num-anc args-impl)
                 (list-ref description 3)))))))
           (else  ; function definition not found
            (draft:signal-semantic-error
             "function call to an unknown function - " (cadadr op))))))

; {22a} ( (argument  <Expr> )* )
; na-lst - number of ancestors required for each of the arguments
; Returns: (listof expr-impl) or #f
(define (draft:ast-function-arguments op-lst)
  (let ((arg-res-lst
         (map
          (lambda (op)
            (if
             (not (eq? (car op) 'argument))
             (draft:signal-semantic-error "not an Argument - " op)
             (draft:ast-expr (cadr op) 0)))
          op-lst)))
    (if
     (member #f arg-res-lst)  ; semantic error detected
     #f
     (map car arg-res-lst))))


;-------------------------------------------------
; Section dedicated to XPointer AST

; {25} <XPointer> ::= <ChildSeq>
;                     | <FullXPtr>
;                     | <Expr>
(define (draft:ast-xpointer op num-anc)    
  (case (car op)
    ((child-seq)
     (draft:ast-child-seq op num-anc))
    ((full-xptr)
     (draft:ast-full-xptr op num-anc))
    (else
     (draft:ast-expr op num-anc))))

; {26} <ChildSeq> ::= (child-seq (name  <String> ))
;                     | (child-seq (name  <String> )?
;                                  (number  <Number> )+ )
(define (draft:ast-child-seq op num-anc)
  (if
   (eq? (caadr op) 'name)
   (and-let*
    ((numbers-res (draft:ast-number-list (cddr op) num-anc)))
    (let ((id-value (cadadr op))
          (converters (car numbers-res))
          (num-ancestors (cdr numbers-res)))
      (cons
       (lambda (nodeset position+size var-binding)
         (let* ((root-node (draft:reach-root nodeset))
                (id-nset ((sxml:child (ntype?? 'id-index))
                          ((sxml:child (ntype?? '@@)) root-node))))
           (if
            (null? id-nset)  ; no id-index
            '()
            (let ((nd (sxml:lookup id-value (cdar id-nset))))
              (if (not nd)
                  '()
                  (let rpt ((nset 
                             (if (and num-ancestors (zero? num-ancestors))
                                 (list nd)
                                 (draft:recover-contextset
                                  (list nd) root-node num-ancestors)))
                            (fs converters))
                    (if (null? fs)
                        nset
                        (rpt ((car fs) nset) (cdr fs)))))))))
       #f)))
   (and-let*
    ((numbers-res (draft:ast-number-list (cdr op) num-anc)))
    (let ((converters (car numbers-res)))
      (cons
       (lambda (nodeset position+size var-binding)
         (let ((child-seq-impl
                (lambda (node)
                  (let rpt ((nset nodeset) (fs converters))
                    (if (null? fs)
                        nset
                        (rpt ((car fs) nset) (cdr fs)))))))
           (if (nodeset? nodeset)
               (map-union child-seq-impl nodeset)
               (child-seq-impl nodeset))))
       (cdr numbers-res))))))
    
; {26a} ( (number  <Number> )+ )
; Returns (cons (listof sxpath-converter) num-anc) or #f
(define (draft:ast-number-list number-lst num-anc)
  (let loop ((to-view (reverse number-lst))
             (res-lst '())
             (num-anc num-anc))
    (cond
      ((null? to-view)  ; everyone processed
       (cons res-lst num-anc))
      ((not (eq? (caar to-view) 'number))
       (draft:signal-semantic-error "not an Number - " (car to-view)))
      (else       
       (loop
        (cdr to-view)        
        (cons (draft:child (ntype?? '*) num-anc)
              (cons (node-pos (cadar to-view))
                    res-lst))
        (draft:na-minus-nneg num-anc 1))))))

; {27} <FullXPtr> ::= (full-xptr  <Expr> <Expr>+ )
(define (draft:ast-full-xptr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (if (null? fs)
              '()
              (let ((nset ((car fs) nodeset position+size var-binding)))
                (if (null? nset)
                    (rpt (cdr fs))
                    nset)))))        
      (apply draft:na-max (map cdr expr-res-lst)))))))


;=========================================================================
; Highest level API functions
; xpath-string - an XPath location path (a string)
; ns+na - can contain 'ns-binding' and/or 'num-ancestors' and/or none of them
; ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding ::= (listof (prefix . uri))
;  prefix - a symbol
;  uri - a string
; num-ancestors - number of ancestors required for resulting nodeset. Can
;  generally be omitted and is than defaulted to 0, which denotes a _usual_
;  nodeset. If a negative number, this signals that all ancestors should be
;  remembered in the context
;
; Returns: (lambda (nodeset position+size var-binding) ...)
; position+size - the same to what was called 'context' in TXPath-1
; var-binding - XPath variable bindings (an optional argument)
;  var-binding = (listof (var-name . value))
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
;  string, nodeset. NOTE: a node must be represented as a singleton nodeset

; Given a list of arguments, returns
;  (values ns-binding num-anc)
(define (draft:arglist->ns+na arglst)
  (let loop ((arglst arglst)
             (ns-binding '())
             (num-anc 0))
    (cond
      ((null? arglst) (values ns-binding num-anc))
      ((pair? (car arglst))
       (loop (cdr arglst) (car arglst) num-anc))
      ((number? (car arglst))
       (loop (cdr arglst) ns-binding
             (if (negative? (car arglst)) #f (car arglst))))
      (else
       (loop (cdr arglst) ns-binding num-anc)))))
  
; Helper for constructing several highest-level API functions
(define (draft:api-helper grammar-parser ast-parser)
  (lambda (xpath-string . ns+na)
    (call-with-values
     (lambda () (draft:arglist->ns+na ns+na))
     (lambda (ns-binding num-anc)
       (and-let*
        ((ast (grammar-parser xpath-string ns-binding))
         (impl-lst (ast-parser ast num-anc)))
        (let ((res (car impl-lst)))
          (lambda (node . var-binding)
            ((if (and num-anc (zero? num-anc))
                 draft:contextset->nodeset
                 (lambda (x) x))             
             (res (as-nodeset node) (cons 1 1)
                  ;(xlink:add-docs-to-vars
                  ; node
                  (if (null? var-binding)
                      var-binding (car var-binding))
                  ; )
                  )))))))))

(define draft:xpath (draft:api-helper txp:xpath->ast draft:ast-location-path))
(define draft:xpointer (draft:api-helper txp:xpointer->ast draft:ast-xpointer))
(define draft:xpath-expr (draft:api-helper txp:expr->ast draft:ast-expr))
(define draft:sxpath (draft:api-helper txp:sxpath->ast draft:ast-expr))

; Aliases
(define txpath-with-context draft:xpath)
(define txpath/c draft:xpath)
(define sxpath-with-context draft:sxpath)
(define sxpath/c draft:sxpath)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Automatically united by Module Manager
; Source filename: ../Ssax-sxml/sxml-tools/xpath-context.scm

;; XLink implementation and the API for XLink processing in Scheme
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; doc ::= '(*TOP*
;           (@@
;            (sxlink
;             (declared-here  <sxlink-arc>* )
;             (embedded)?
;             (outgoing
;              (node  <sxlink-arc>+ )*
;             )
;            )
;            ...   ; more aux list members
;           )
;          ...)

;==========================================================================
; XLink-related node tests
; They test whether an SXML node has a definite XLink type
; ATTENTION:
;  1. A non-prefixed XLink namespace uri is used for these node tests. If
; a prefix is used, these functions behave incorrectly.
;  2. These predicates should be used carefully - element's XLink-related
; meaning depends not only on its xlink:type attribute, but also on its
; position among other XLink element. For example, an element with an
; xlink:type="arc" attribute is not an arc element if it has anything other
; then an extended-link element as a parent

; Helper for predicates
;  type - a string, is supposed to have one of the following values:
; "extended", "simple", "locator", "resource", "arc", "title".
; A lambda is returned. When applied to an SXML node, it determines
; whether the node's xlink:type attribute has a 'type' value.
(define (xlink:ntype?? type)
  (lambda (node)
    (let ((attval
           ((select-kids (ntype?? '*text*))
            ((select-kids (ntype?? 'http://www.w3.org/1999/xlink:type))
             ((select-kids (ntype?? '@)) node)))))
      (if (null? attval)  ; there is no xlink:type attribute
          #f
          (string=? (car attval) type)))))

; Node tests for different XLink elements
(define xlink:elem-extended? (xlink:ntype?? "extended"))
(define xlink:elem-simple? (xlink:ntype?? "simple"))
(define xlink:elem-locator? (xlink:ntype?? "locator"))
(define xlink:elem-resource? (xlink:ntype?? "resource"))
(define xlink:elem-arc? (xlink:ntype?? "arc"))
(define xlink:elem-title? (xlink:ntype?? "title"))


;==========================================================================
; Utility functions over document auxiliary information

;-------------------------------------------------
; Document's URI
; The following functions moved to "xlink-parser.scm"
;  xlink:get-uri
;  xlink:set-uri-for-sxlink-arcs

; Sets the URI for the SXML document
(define (xlink:set-uri uri doc)
  (let ((aux-nset ((select-kids (ntype?? '@@)) doc)))
    (if
     (or (null? aux-nset)  ; no aux node at all yet
         ; no sxlink/declared-here subnode
         (null? ((select-kids (ntype?? 'declared-here))
                 ((select-kids (ntype?? 'sxlink)) (car aux-nset)))))
     (xlink:replace-branch  ; inserts the @@/uri node in the document
      doc '(@@ uri) (list uri))
     (xlink:replace-branch
      doc
      '(@@)
      (cdr
       ((xlink:branch-helper  ; inserts URI to sxlink-arcs
         (lambda (declared-here-node dummy)
           (cons
            (car declared-here-node)
            (xlink:set-uri-for-sxlink-arcs
             uri (cdr declared-here-node)))))
        (xlink:replace-branch  ; inserts (modified) URI
         (car aux-nset) '(uri) (list uri))
        '(sxlink declared-here)
        '()  ; dummy
        ))))))

;-------------------------------------------------
; Id-index of the document

; Returns the id-index of the SXML document
; #f is returned is there is no "@@/id-index" subtree in the document
(define (xlink:id-index doc)
  (let ((nodeset ((select-kids (ntype?? 'id-index))
                  ((select-kids (ntype?? '@@)) doc))))
    (if (null? nodeset)  ; there is no "@@/id-index" subtree
        #f
        (cdar nodeset))))

;-------------------------------------------------
; SXLink members of the auxiliary list

; Returns (listof sxlink-arc) located in "@@/sxlink/declared-here"
; These are SXLink arcs that are declared in this document
(define (xlink:arcs-declared-here doc)
  ((select-kids (ntype?? '*any*))
   ((select-kids (ntype?? 'declared-here))
    ((select-kids (ntype?? 'sxlink))
     ((select-kids (ntype?? '@@)) doc)))))

; Whether outgoing SXLink arcs are embedded into the document.
; This is denoted by the presense of "@@/sxlink/embedded" empty element.
(define (xlink:arcs-embedded? doc)
  (not (null? ((select-kids (ntype?? 'embedded))
               ((select-kids (ntype?? 'sxlink))
                ((select-kids (ntype?? '@@)) doc))))))

; Returns the content of "@@/sxlink/outgoing"
; The result is the associative list between nodes of the document and
; SXLink arcs that start from the corresponding node
(define (xlink:arcs-outgoing doc)
  ((select-kids (ntype?? '*any*))
   ((select-kids (ntype?? 'outgoing))
    ((select-kids (ntype?? 'sxlink))
     ((select-kids (ntype?? '@@)) doc)))))


;==========================================================================
; Get the document by its URI

; Handler for error messages
(define (xlink:api-error . text)
  (cerr "XLink API error: ")
  (apply cerr text)
  (cerr nl))

; Id+XLink parser parameterized
(define xlink:parser (ssax:multi-parser 'id 'xlink))

; Returns the SXML representation for the resource specified by REQ-URI.
; Resource types supported: XML and HTML. XML is parsed into SXML with SSAX,
; HTML is parsed with HTML Prag.
; Additionally, linking information is parsed. For XML, linking information is
; assumed to be specified with XLink. For HTML, <a> elements are treated as
; simple links.
; In case of an error (resource doesn't exist or its type is unsupported), an
; error is signalled with 'xlink:api-error' and #f is returned.
(define (xlink:get-document-by-uri req-uri)
  (case (ar:resource-type req-uri)
    ((#f)  ; resource doesn't exist
     (xlink:api-error "resource doesn't exist: " req-uri)
     #f)
    ((xml plain unknown)
     (let* ((port (open-input-resource req-uri))
            (doc (xlink:parser port)))
       (close-input-port port)
       (xlink:set-uri req-uri doc)))
    ((html)
     (let* ((port (open-input-resource req-uri))
            (doc (html->sxml port)))
       (close-input-port port)
       (SHTML->SHTML+xlink
        (xlink:set-uri req-uri doc))))    
    (else  ; unknown resource type
     (xlink:api-error "resource type not supported: " req-uri)
     #f)))


;==========================================================================
; Loading multiple documents by their URIs

;-------------------------------------------------
; Helper accessors to SXLink arcs

; Returns URIs of resources that participate in SXLink arcs
;  sxlink-arcs ::= (listof sxlink-arc)
; Result: (listof string)
; The result may contain duplicates
(define (xlink:arcs-uris sxlink-arcs)
  ((select-kids (ntype?? '*text*))
   ((select-kids (ntype?? 'uri))
    ((select-kids (ntype-names?? '(from to))) sxlink-arcs))))

; Returns URIs of all linkbases encountered among SXLink arcs
; Result: (listof string)
; The result may contain duplicates
(define (xlink:arcs-linkbase-uris sxlink-arcs)
  ((select-kids (ntype?? '*text*))
   ((select-kids (ntype?? 'uri))
    ((select-kids (ntype?? 'to))
     (filter (ntype?? 'linkbase) sxlink-arcs)))))

;-------------------------------------------------
; Working on the set of SXML documents
;  doc-set ::= (listof document)

; Returns the list of URIs of the documents in the doc-set
(define (xlink:uris doc-set)
  (filter
   (lambda (x) x)
   (map xlink:get-uri doc-set)))

; Removes equal duplicates from the list
(define (xlink:remove-equal-duplicates lst)
  (cond
    ((null? lst) lst)
    ((member (car lst) (cdr lst))
     (xlink:remove-equal-duplicates (cdr lst)))
    (else
     (cons (car lst) (xlink:remove-equal-duplicates (cdr lst))))))

; procedure xlink:find-doc :: URI-STRING (listof SXML-TREE) -> SXML-TREE
;
; Finding a document in 'doc-set' by its 'uri-string'.
; If there is no such document, #f is returned.
;  doc-set ::= (listof SXML-TREE)
(define (xlink:find-doc uri-string doc-set)
  (let loop ((doc-set doc-set))
    (cond
      ((null? doc-set) #f)
      ((equal? (xlink:get-uri (car doc-set)) uri-string)
       (car doc-set))
      (else (loop (cdr doc-set))))))

;-------------------------------------------------
; Extending the set of documents with additional documents being referred to

; Returns a list of URIs which are refered by XLink markup
; Result:  (listof string)
; The list may contain duplicates.
(define (xlink:referenced-uris doc-set)
  (apply append
         (map
          (lambda (doc)
            (xlink:arcs-uris (xlink:arcs-declared-here doc)))
          doc-set)))

; Returns a list of linkbase URIs which are refered by XLink markup
; Result:  (listof string)
; The list may contain duplicates.
(define (xlink:referenced-linkbase-uris doc-set)
  (apply append
         (map
          (lambda (doc)
            (xlink:arcs-linkbase-uris (xlink:arcs-declared-here doc)))
          doc-set)))

; A helped low-level function for extending the doc-set with more documents
; Is parameterized with
;  referenced-uris ::= (lambda (doc-set) ...)
; that would return URIs refered by XLink markup in the doc-set
; When parameterized, returns
;  (lambda (doc-set . max-steps) ...)
;  max-steps - maximal number of recursive steps
;  The lambda returns the expanded doc-set
(define (xlink:add-documents-helper referenced-uris)
  (lambda (doc-set . max-steps)
    (let ((max-steps (if (null? max-steps) -1 (car max-steps))))
      (let loop ((doc-set doc-set)
                 (loaded-uris (xlink:uris doc-set))
                 (to-load (referenced-uris doc-set))
                 (step 0))
        (if
         (or (null? to-load) (= step max-steps))
         doc-set
         (let rpt ((loaded-uris loaded-uris)
                   (to-load to-load)
                   (added-docs '()))
           (cond
             ((null? to-load)
              (loop (append added-docs doc-set)
                    loaded-uris
                    (referenced-uris added-docs)
                    (+ step 1)))
             ((member (car to-load) loaded-uris)
              (rpt loaded-uris
                   (cdr to-load)
                   added-docs))
             (else   ; we load the linkbase
              (let ((doc (xlink:get-document-by-uri (car to-load))))
                (rpt (cons (car to-load) loaded-uris)
                     (cdr to-load)
                     (if doc (cons doc added-docs) added-docs)))))))))))

; Two most common parameterized functions. The first one recursively loads
; linkbases. The second one recursively loads all refered documents
(define xlink:add-linkbases-recursively
  (xlink:add-documents-helper xlink:referenced-linkbase-uris))
(define xlink:add-documents-recursively
  (xlink:add-documents-helper xlink:referenced-uris))

;-------------------------------------------------
; Higher-level functions

; Parameterized with options, returns
;  (lambda (uri . uris) ...)
; which is the lambda for getting documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
; number of recursive steps defined by the <number> supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
; of recursive steps defined by the <number> supplied
(define (xlink:get-documents-with-params . options)
  (let ((get-initial-docs  ; Returns documents by their URIs
         (lambda (uris)
           (filter  ; keeps only correctly loaded documents
            (lambda (x) x)
            (map xlink:get-document-by-uri
                 (xlink:remove-equal-duplicates uris)))))
        (linkbases-pairs
         (filter
          (lambda (option) (and (pair? option) (eq? (car option) 'linkbases)))
          options))
        (docs-pairs
         (filter
          (lambda (option) (and (pair? option) (eq? (car option) 'docs)))
          options)))
    (let ((linkbases? (or (memq 'linkbases options)
                          (not (null? linkbases-pairs))))
          (max-steps-linkbases (if (null? linkbases-pairs)
                                   -1
                                   (cadar linkbases-pairs)))
          (documents? (or (memq 'docs options)
                          (not (null? docs-pairs))))
          (max-steps-documents (if (null? docs-pairs)
                                   -1
                                   (cadar docs-pairs))))
      (cond
        ((and linkbases? documents?)
         (lambda (uri . uris)
           (xlink:add-linkbases-recursively
            (xlink:add-documents-recursively
             (get-initial-docs (cons uri uris))
             max-steps-documents)
            max-steps-linkbases)))
        (linkbases?
         (lambda (uri . uris)
           (xlink:add-linkbases-recursively            
            (get-initial-docs (cons uri uris))            
            max-steps-linkbases)))
        (documents?
         (lambda (uri . uris)           
           (xlink:add-documents-recursively
            (get-initial-docs (cons uri uris))
            max-steps-documents)))
        (else  ; nothing extra to be loaded
         (lambda (uri . uris) (get-initial-docs (cons uri uris))))))))

; The most common parameterized case.
; Loads documents and all linkbases
(define xlink:get-documents+linkbases
  (xlink:get-documents-with-params 'linkbases))


;==========================================================================
; Working on the set of linked documents
;  linked-docs ::= (listof document)

;  alist ::= (listof
;             (cons key (listof item)))
; For equal keys in the alist, the function unites the corresponding key values
; Returns the new alist
(define (xlink:unite-duplicate-keys-in-alist alist)
  (let loop ((src alist)
             (res '()))
    (if
     (null? src)
     res
     (let ((curr-key (caar src)))
       (let rpt ((scan (cdr src))
                 (content (cdar src))
                 (other '()))
         (cond
           ((null? scan)
            (loop other
                  (cons (cons curr-key content)
                        res)))
           ((equal? (caar scan) curr-key)
            (rpt (cdr scan)
                 (append content (cdar scan))
                 other))
           (else  ; a different key
            (rpt (cdr scan) content
                 (cons (car scan) other)))))))))
                         
; Documents exchange their SXLink arcs, such as each arc is moved to the
; "@@/sxlink/outgoing" branch of the document where the arc's starting
; resource is
; Additional SXLink arcs may be specified in the optional argument.
(define (xlink:docs-exchange-arcs doc-set . sxlink-arcs)
  (let ((doc-set-uris (xlink:uris doc-set))
        (sxlink-arcs (if (null? sxlink-arcs) '() (car sxlink-arcs))))
    ; outgoing-alist ::= (listof
    ;                     (cons uri
    ;                           (listof (cons node (listof sxlink-arc)))))
    ; declared-here-alist ::= (listof
    ;                           (cons uri (listof sxlink-arc)))
    (let loop ((outgoing-alist (map
                                (lambda (doc)
                                  (cons
                                   (xlink:get-uri doc)
                                   (xlink:arcs-outgoing doc)))
                                doc-set))
               (declared-here-alist (map list doc-set-uris))
               (arcs-to-scan
                (append sxlink-arcs
                        (apply append
                               (map xlink:arcs-declared-here doc-set)))))
      (if
       (null? arcs-to-scan)  ; all arcs processed
       (let ((outgoing-alist
              (xlink:unite-duplicate-keys-in-alist outgoing-alist))
             (declared-here-alist
              (xlink:unite-duplicate-keys-in-alist declared-here-alist)))
         (map
          (lambda (doc)
            (let ((uri (xlink:get-uri doc)))
              (xlink:replace-branch               
                doc
                '(@@ sxlink)
                `((declared-here
                   ,@(cdr (assoc uri declared-here-alist)))
                  ,@(if (xlink:arcs-embedded? doc) '((embedded)) '())
                  (outgoing
                   ,@(xlink:unite-duplicate-keys-in-alist
                      (cdr (assoc uri outgoing-alist))))))))
            doc-set))
       (let* ((curr-arc (car arcs-to-scan))
              (uri-from (car  ; URI must be presented
                         ((select-kids (ntype?? '*text*))
                          ((select-kids (ntype?? 'uri))
                           ((select-kids (ntype?? 'from))
                            curr-arc)))))
              (uri-decl (car  ; URI must be presented
                         ((select-kids (ntype?? '*text*))
                          ((select-kids (ntype?? 'uri))
                           ((select-kids (ntype?? 'declaration))
                            curr-arc))))))
         (if
          (not (member uri-from doc-set-uris))
          ; This arc starts from none of the documents from doc-set
          (loop outgoing-alist
                (cons (list uri-decl curr-arc) declared-here-alist)
                (cdr arcs-to-scan))
          (let ((nodes  ; nodes that are the starting resource
                 (let ((nodes-nset
                        ((select-kids (ntype?? 'nodes))
                         ((select-kids (ntype?? 'from))
                          curr-arc))))
                   (if
                    (not (null? nodes-nset))
                    (cdar nodes-nset)
                    (let ((xpointer-nset
                           ((select-kids (ntype?? 'xpointer))
                            ((select-kids (ntype?? 'from)) curr-arc)))
                          (starting-doc (xlink:find-doc uri-from doc-set)))
                      (if
                       (null? xpointer-nset)  ; no XPointer
                       ((select-kids (ntype?? '*))  ; document element
                        starting-doc)
                       (let ((func (sxml:xpointer (cadar xpointer-nset))))
                         (if
                          (not func)  ; parser error
                          #f
                          (let ((starting-nset (func starting-doc)))
                            (if
                             (nodeset? starting-nset)
                             starting-nset
                             #f))))))))))
            (if
              nodes   ; starting resource selects some nodes
              (loop               
               (cons (cons uri-from
                           (map
                            (lambda (node) (list node curr-arc))
                            nodes))
                     outgoing-alist)
               declared-here-alist
               (cdr arcs-to-scan))
              (loop outgoing-alist
                    (cons (list uri-decl curr-arc) declared-here-alist)
                    (cdr arcs-to-scan))))))))))

;-------------------------------------------------
; Embedding XLink arcs into the document
; The element node with embedded XLink arcs looks as follows
;  element-node ::= (name
;                    (@ ...)
;                    (@@
;                     (sxlink  <sxlink-arc>+ )
;                     ...)  ; other members of the aux list
;                    ...)
;  attribute-node ::= (name "value"
;                           (@@
;                            (sxlink  <sxlink-arc>+ )
;                            ...)  ; other members of the aux list
;                          )

; Takes SXLink arcs outgoing from the document and embeds these arcs into
; element and attribute nodes of the document.
; The modified document is returned
; The function doesn't make a copy of nodes that remain unchanged
(define (xlink:embed-arcs-into-document document)
  (letrec
      (; These helper functions return
       ; (values node outgoing-alist changed?)
       ;  node - the (modified) node
       ;  outgoing-alist ::= (listof (cons node (listof sxlink-arc)))
       ;  changed? - whether the node was changed      
       (process-element-node
        (lambda (node outgoing-alist)
          (cond
            ((or (not (pair? node))
                 (eq? (car node) '@@))
             ; Text node or aux node
             (values node outgoing-alist #f))
            ((eq? (car node) '@)
             (call-with-values
              (lambda ()
                ((process-nodeset process-attribute-node)
                 (cdr node) outgoing-alist))
              (lambda (content new-out-alist changed?)
                (if changed?
                    (values (cons '@ content)
                            new-out-alist
                            changed?)
                    (values node outgoing-alist changed?)))))
            (else  ; this is the element node
             (call-with-values
              (lambda ()
                (cond
                  ((assq node outgoing-alist)
                   => (lambda (alist-member)
                        (values
                         (cdr alist-member)
                         (filter
                          (lambda (memb) (not (eq? memb alist-member)))
                          outgoing-alist))))
                  (else  ; the node is not the starting resource
                   (values #f outgoing-alist))))
              (lambda (outgoing-arcs new-out-alist)
                (call-with-values
                 (lambda () ((process-nodeset process-element-node)
                             (cdr node) new-out-alist))
                 (lambda (content new-out-alist changed?)
                   (cond
                     ((not (or outgoing-arcs changed?))
                      ; node remains unchanged                    
                      (values node outgoing-alist changed?))
                     ((not outgoing-arcs)  ; no arcs from that node
                      (values (cons (car node) content)
                              new-out-alist
                              changed?))
                     (else  ; the node is the starting resource
                      (let ((new-content
                             (if changed? content (cdr node))))
                        (values
                         (cond
                           ((not (null?  ; aux list presented
                                  ((select-kids (ntype?? '@@)) new-content)))
                            (xlink:append-branch
                             (cons (car node) new-content)
                             '(@@ sxlink) outgoing-arcs))
                           (((ntype?? '@)  ; attribute node presented                         
                             (car new-content))
                            `(,(car node)
                              ,(car content)  ; attribute node
                              (@@ (sxlink ,@outgoing-arcs))
                              ,@(cdr content)))
                           (else  ; no attribute node
                            `(,(car node)
                              (@)
                              (@@ (sxlink ,@outgoing-arcs))
                              ,@content)))
                         new-out-alist
                         #t))))))))))))
       (process-attribute-node
        (lambda (node outgoing-alist)
          (cond
            ((assq node outgoing-alist)
             => (lambda (alist-member)
                  (values
                   (if
                    (null?  ; no aux node in the attribute
                     ((select-kids (ntype?? '@@)) node))
                    (append node
                            `((@@
                               (sxlink ,@(cdr alist-member)))))
                    (xlink:append-branch
                     node '(@@ sxlink) (cdr alist-member)))
                   (filter
                    (lambda (memb) (not (eq? memb alist-member)))
                    outgoing-alist)
                   #t)))
            (else   ; the attribute node is not a starting resource
             (values node outgoing-alist #f)))))
       ; Is parameterized with one of the previous functions and
       ; processes the nodeset
       (process-nodeset
        (lambda (processing-func)
          (lambda (nodeset outgoing-alist)
            (let loop ((nset nodeset)
                       (out-alist outgoing-alist)
                       (changed? #f)
                       (res '()))
              (if
               (null? nset)  ; nodeset processed
               (values (reverse res)
                       out-alist
                       changed?)
               (call-with-values
                (lambda () (processing-func (car nset) out-alist))
                (lambda (new-node new-out-alist ch?)
                  (loop (cdr nset)
                        new-out-alist
                        (or changed? ch?)
                        (cons new-node res))))))))))
    (call-with-values
     (lambda () ((process-nodeset process-element-node)
                 (cdr document)
                 (xlink:arcs-outgoing document)))
     (lambda (content new-out-alist changed?)
       (if (not changed?)  ; the document remains unchanged
           (xlink:replace-branch
            document '(@@ sxlink embedded) '())
           (xlink:replace-branch
            (cons '*TOP* content)
            '(@@ sxlink)
            `((declared-here ,@(xlink:arcs-declared-here document))
              (embedded)
              (outgoing ,@new-out-alist))))))))

; Returns all embedded SXLink arcs in the document
; Result: (listof sxlink-arc)
(define (xlink:arcs-embedded doc)
  (let ((get-kids
         (select-kids
          (lambda (node) (and (pair? node) (not (eq? '@@ (car node))))))))
    (let loop ((nodes-to-scan (get-kids doc))
               (res '()))
      (if
       (null? nodes-to-scan)  ; everyone processed
       (draft:remove-eq-duplicates res)
       (loop
        (append (get-kids (car nodes-to-scan)) (cdr nodes-to-scan))
        (append
         ((select-kids (ntype?? '*any*))
          ((select-kids (ntype?? 'sxlink))
           ((select-kids (ntype?? '@@)) (car nodes-to-scan))))
         res))))))


;==========================================================================
; Load documents with respect to the other documents

; Parameterized with options, returns
;  (lambda (linked-docs uri . uris) ...)
; which is the lambda for getting more documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
;                            number of recursive steps defined by the <number>
;                            supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
;                       of recursive steps defined by the <number> supplied
;  'embed - embed SXLink arcs into nodes that are starting resources for that
;           arcs
;  'no-embed - don't embed SXLink arcs into documents loaded
(define (xlink:parameterized-load-with-respect-documents . options)
  (let ((doc-getter (apply xlink:get-documents-with-params options))
        (embed? (memq 'embed options))
        (no-embed? (memq 'no-embed options)))
    (lambda (linked-docs . uris)
      (let* ((loaded-uris (xlink:uris linked-docs))
             (req-docs
              (xlink:docs-exchange-arcs
               (filter
                (lambda (x) x)
                (map
                 (lambda (uri)
                   (if
                    (member uri loaded-uris)  ; document already loaded
                    (xlink:find-doc uri linked-docs)
                    (xlink:get-document-by-uri uri)))
                 (xlink:remove-equal-duplicates uris)))
               (apply append (map xlink:arcs-declared-here linked-docs)))))
        (cond
          (no-embed? req-docs)
          ((or embed?  ; embed arcs
               (member #t (map xlink:arcs-embedded? linked-docs)))
           (map xlink:embed-arcs-into-document req-docs))
          (else req-docs))))))

; The most common case of parametrization
(define xlink:get-docs-with-respect-to-loaded
  (xlink:parameterized-load-with-respect-documents 'linkbase))


;==========================================================================
; Excluding documents from linked-docs
; TODO: to be implemented later

; Returns all SXLink arcs encountered in the document. This envolves:
;  a) declared here arcs,
;  b) outgoing arcs, and
;  c) embedded arcs
; Returns (listof sxlink-arcs)
;(define (xlink:arcs-all doc)

; Returns linked-docs 
;(define (xlink:exclude-documents linked-docs uri . uris)
  

;==========================================================================
; High-level API functions

; Parameterized with options, returns
;  (lambda (uri . uris) ...)
; which is the lambda for getting documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
;                            number of recursive steps defined by the <number>
;                            supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
;                       of recursive steps defined by the <number> supplied
;  'embed - embed SXLink arcs into nodes that are starting resources for that
;           arcs
(define (xlink:load-linked-docs-with-params . options)
  (let ((doc-getter (apply xlink:get-documents-with-params options)))
    (if
     (memq 'embed options)  ; embed
     (lambda (uri . uris)
       (map
        xlink:embed-arcs-into-document
        (xlink:docs-exchange-arcs (apply doc-getter (cons uri uris)))))
     (lambda (uri . uris)
       (xlink:docs-exchange-arcs (apply doc-getter (cons uri uris)))))))

; procedure xlink:documents :: {REQ-URI}+  -> (listof SXML-TREE)
; procedure xlink:documents-embed :: {REQ-URI}+  -> (listof SXML-TREE)
;
; Both `xlink:documents' and `xlink:documents-embed' accept one or more
; strings as their arguments. Each string supplied denotes the URI of the
; requested document to be loaded. The requested document(s) are loaded
; and are represented in SXML. All XLink links declared in these document(s)
; are represented as a set of SXLink arcs. If any XLink links refer to XLink
; linkbases [<a href="http://www.w3.org/TR/xlink/#xlg">XLink</a>],
; these linkbases are additionally loaded, for additional SXLink arcs
; declared there.
;
; The starting resource for each SXLink arc is determined:
; 1. For each SXML document loaded, the function `xlink:document' adds all
;    SXLink arcs whose starting resource is located within this document, to
;    the auxiliary list of its document node (*TOP*).
; 2. The function 'xlink:documents-embed' embeds each SXLink arc into its
;    starting resource-node, via auxiliary list of that node. For text nodes
;    serving for starting resources, their SXLink arcs are stored in the
;    auxiliary list of the document node (*TOP*), since SXML text nodes do
;    not support their own auxiliary lists.
;
; Supported URI formats:
;  + local file
;  + http:// schema
;
; Supported document formats: XML and HTML. In the case of HTML,
; <A> hyperlinks are considered as XLink simple links.
;
; Result: (listof SXML-TREE)
; A particular SXML document can be located in this list using the
; function `xlink:find-doc'.
(define xlink:documents
  (xlink:load-linked-docs-with-params 'linkbases))
(define xlink:documents-embed
  (xlink:load-linked-docs-with-params 'linkbases 'embed))

;-------------------------------------------------
; Convenient function for getting a document by its URI

; procedure sxml:document :: REQ-URI [NAMESPACE-PREFIX-ASSIG] ->
;                             -> SXML-TREE
;
; Obtain a [possibly, remote] document by its URI
; Supported URI formats:  local file and HTTP schema
; Supported document formats:  XML and HTML
;
; REQ-URI - a string that contains the URI of the requested document
; NAMESPACE-PREFIX-ASSIG - is passed as-is to the SSAX parser: there it is
;  used for assigning certain user prefixes to certain namespaces.
;  NAMESPACE-PREFIX-ASSIG is an optional argument and has an effect for an
;  XML resource only. For an HTML resource requested, NAMESPACE-PREFIX-ASSIG
;  is silently ignored.
;
; Result: the SXML representation for the requested document
(define (sxml:document req-uri . namespace-prefix-assig)
  (if
   (string? req-uri)
   (case (ar:resource-type req-uri)
     ((#f)  ; resource doesn't exist
      (xlink:api-error "resource doesn't exist: " req-uri)
      #f)
     ((xml plain unknown)
      (let* ((port (open-input-resource req-uri))
             (doc (ssax:xml->sxml
                   port
                   (if (null? namespace-prefix-assig)
                       namespace-prefix-assig
                       (car namespace-prefix-assig)))))
        (close-input-port port)
        doc   ; DL: can also add URI: (xlink:set-uri req-uri doc)
        ))
     ((html)
      (let* ((port (open-input-resource req-uri))
             (doc (html->sxml port)))
        (close-input-port port)
        doc   ; DL: can also add URI: (xlink:set-uri req-uri doc)
        ))
     (else  ; unknown resource type
      (xlink:api-error "resource type not supported: " req-uri)
      #f))
   ; Otherwise: REQ-URI is not a string - producing an exception
   (exc:signal  ; relies on SRFI-12
    (make-property-condition
     'exn
     'message
     "sxml:document: expects type <string> as 1st argument"))))


;==========================================================================
; SXPath-related stuff

; Whether an SXLink arc
(define xlink:arc?
  (ntype-names??
   '(linkbase simple outbound inbound third-party local-to-local)))

;-------------------------------------------------
; Working with the administrative variable '*docs*

; Returns the value of the administrative SXPath variable '*docs*
; This variable stores linked-docs
(define (xlink:docs-variable var-binding)
  (cond
    ((assq '*docs* var-binding)
     => cdr)
    (else '())))

; Extends var-bindings with administative information about linked-docs
;  node - a single node or a nodeset
(define (xlink:add-docs-to-vars node var-binding)
  (if (assq '*docs* var-binding)  ; variable already exists
      var-binding
      (cons
       (cons '*docs*
             (filter
              (lambda (doc)
                (and (draft:top? doc) (xlink:get-uri doc)))
              (draft:reach-root (as-nodeset node))))
       var-binding)))  
  
;-------------------------------------------------
; Accessors to SXLink arcs that start from the given SXML node

; Returns SXLink arcs that are embedded into the node as aux list members
; Result: (listof sxlink-arc)
(define (xlink:node-embedded-arcs node)
  (if (draft:top? node)  ; the root node
      '()  ; no embedded arcs
      ((select-kids (ntype?? '*any*))
       ((select-kids (ntype?? 'sxlink))
        ((select-kids (ntype?? '@@)) node)))))

; Returns SXLink arcs that are specified at the top-level of the document and
; start from node
(define (xlink:node-arcs-on-top node document)
  (cond
    ((assq node (xlink:arcs-outgoing document))
     => cdr)
    (else '())))

; Returns all SXLink arcs (both embedded and specified at the top-level) that
; start from ther node
; The union of the two previous functions
(define (xlink:node-arcs node document)
  (append (xlink:node-embedded-arcs node)
          (xlink:node-arcs-on-top node document)))

;-------------------------------------------------
; Traversing SXLink arcs

; Traverse all SXLink arcs to their ending resources
;  sxlink-arcs ::= (listof sxlink-arc)
;  linked-docs ::= (listof document)
;  num-ancestors - number of ancestors required for ending resources
(define (xlink:traverse-arcs sxlink-arcs linked-docs num-ancestors)
  (let* ((arcs-to
          ((select-kids (ntype?? 'to)) sxlink-arcs))
         (req-docs
          (apply
           xlink:get-docs-with-respect-to-loaded
           (cons
            linked-docs
            (if
             (and num-ancestors (zero? num-ancestors))
             ((select-kids (ntype?? '*text*))
              ((select-kids (ntype?? 'uri))
               (filter  ; elements that have a <nodes> subelement
                (lambda (arc-to)
                  (null? ((select-kids (ntype?? 'nodes)) arc-to)))
                arcs-to)))
            ((select-kids (ntype?? '*text*))
             ((select-kids (ntype?? 'uri)) arcs-to)))))))
    ;(pp req-docs)
    (map-union
     (lambda (arc-to)
       (let ((nodes-nset
              ((select-kids (ntype?? 'nodes)) arc-to)))
         (if
          (and num-ancestors (zero? num-ancestors)
               (not (null? nodes-nset)))
          (cadar nodes-nset)
          ; otherwise we need the document and the XPointer node
          (let ((doc (xlink:find-doc
                      (car ((select-kids (ntype?? '*text*))
                            ((select-kids (ntype?? 'uri)) arc-to)))
                      req-docs))
                (xpointer-nset
                 ((select-kids (ntype?? '*text*))
                  ((select-kids (ntype?? 'xpointer)) arc-to))))
            ;(pp doc)            
            ;(display xpointer-nset)
            ;(newline)
            (cond
              ((not doc)  ; document couldn't be loaded
               '())
              ((null? xpointer-nset)
               ; no XPointer part => addresses the document element
               ((draft:child (ntype?? '*) num-ancestors)
                doc))
              (else
               (let ((impl
                      (draft:xpointer (car xpointer-nset)
                                      (if num-ancestors num-ancestors -1))))
                 (if
                  (not impl)  ; parser error
                  '()
                  (let ((res (impl doc)))
                    (if
                     (nodeset? res)
                     res
                     (begin
                       (xlink:api-error
                        "XPointer fragment identifier doesn't "
                        "select any nodeset: " (car xpointer-nset))
                       '())))))))))))
     arcs-to)))

;-------------------------------------------------
; Additional XPath axes

; XPath+XLink arc axis
; This axis returns all SXLink arcs that start from the context node
;  num-ancestors is dummy here, since SXLink arcs don't have ancestors
(define (xlink:axis-arc test-pred? . num-ancestors)
  (let ((this-axis
         (lambda (node)  ; not a nodeset
           (let ((root-node
                  (if (sxml:context? node)
                      (draft:list-last (sxml:context->ancestors-u node))
                      node)))
             (if (draft:top? root-node)
                 (xlink:node-arcs (sxml:context->node node) root-node)
                 (xlink:node-embedded-arcs (sxml:context->node node)))))))
    (lambda (node)   ; node or nodeset
      (filter test-pred?
              (if (nodeset? node)
                  (map-union this-axis node)
                  (this-axis node))))))

; XPath+XLink traverse axis
; This axis traverses from the context node
; The lambda produced additionally takes the var-binding. In var-binding, the
; linked-docs can be stored in the administrative variable '*docs*
(define (xlink:axis-traverse test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (get-arcs  ; returns SXLink arcs that start from a given node
          (lambda (node)  ; not a nodeset
            (let ((root-node
                   (if (sxml:context? node)
                       (draft:list-last (sxml:context->ancestors-u node))
                       node)))
             (if (draft:top? root-node)
                 (xlink:node-arcs (sxml:context->node node) root-node)
                 (xlink:node-embedded-arcs (sxml:context->node node)))))))
    ; node can be both a single node and a nodeset here
    (lambda (node var-binding)
      (filter
       (lambda (node)
         (test-pred? (sxml:context->node node)))       
       (xlink:traverse-arcs
        (if (nodeset? node)
            (map-union get-arcs node)
            (get-arcs node))
        (xlink:docs-variable var-binding)
        num-anc)))))

; XPath+XLink traverse-arc axis
; The axis traverses from the context node that is an SXLink arc
; The lambda produced additionally takes the var-binding. In var-binding, the
; linked-docs can be stored in the administrative variable '*docs*
(define (xlink:axis-traverse-arc test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node var-binding)
      (filter
       (lambda (node)
         (test-pred? (sxml:context->node node)))       
       (xlink:traverse-arcs
        (filter xlink:arc?
                (draft:reach-root (as-nodeset node)))
        (xlink:docs-variable var-binding)
        num-anc)))))

(provide (all-defined)))
