; Module header is generated automatically
#cs(module ddo-axes mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tmp/ssax"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")
(require "xpath-context_xlink.ss")

;; The implementation of SXPath axes with support for distinct document order
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; The implementation of axes is based on the idea of context

;=========================================================================
; Miscellaneous helpers

; Returns the last member of the lst
; lst is expected to be non-empty
(define (ddo:list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (ddo:list-last (cdr lst))))

; Selects all attribute and child nodes of a given 'node'
; Node is not supposed to be a context
(define (ddo:attr-child node)
  (cond
    ((or (not (pair? node))  ; Leaf node
         (null? (cdr node))  ; empty element
         (memq (car node) '(*PI* *COMMENT* *ENTITY*))   ; PI, Comment or Entity
         )  ; => no children
     '())
    ((and (pair? (cadr node))
          (eq? '@ (caadr node)))
     ; attribute node presented
     (append (cdadr node)  ; attributes
             (filter sxml:node? (cddr node))))
    (else  ; no attributes
     (filter sxml:node? (cdr node)))))

; For a given node, returns its attribute nodes and attribute value nodes in
; document order
; Node is not supposed to be a context
(define (ddo:attrs-and-values node)
  (apply append
         (map  ; attribute and its content
          (lambda (a) (cons a (cdr a)))
          (sxml:attr-list node))))

; Removes those members of the input 'nodeset' that are attributes or
; attribute values of a given 'node'. Nodeset is supposed to be in distinct
; document order. The order of attribute nodes in the 'nodeset' is supposed
; to be the same as in the original SXML document
; Works for ordinary nodes are well as for contexts
(define (ddo:discard-attributes node nodeset)
  (let loop ((attrs (ddo:attrs-and-values (sxml:context->node node)))
             (nset nodeset))
    (if (or (null? attrs) (null? nset))
        nset
        (loop (cdr attrs)
              (if (eq? (car attrs) (sxml:context->node (car nset)))
                  (cdr nset) nset)))))
  

;=========================================================================
; XPath axes for location steps not involving position-based predicates
; In this section, all axes expect the argument node-set in distinct document
; order, and return the result in distinct document order

; Ancestor axis
; In general, every two nodes have have some common ancestors (at least the
; root of the document). When we obtain ancestors of the context node, only
; those of them must be added to the result which are different from the
; ancestors of the previous node in the input node-set
(define (ddo:ancestor test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-ancestors '())
                 (res '()))
        (if
         (null? src)  ; everyone processed
         (reverse res)
         (let ((curr (car src)))
           (if
            (sxml:context? curr)
            (let rpt ((curr-ancs (reverse
                                  (sxml:context->ancestors-u curr)))
                      (dupl '()))
              (cond
                ((null? curr-ancs)  ; no new ancestors
                 (loop (cdr src) prev-ancestors res))
                ((memq (car curr-ancs) prev-ancestors)
                 ; member of the ancestorial chain
                 (rpt (cdr curr-ancs)
                      (cons (car curr-ancs) dupl)))
                (else  ; the first different ancestor in a chain found
                 (let creat ((new-ancestors dupl)
                             (curr-ancs curr-ancs)
                             (res res))
                   (cond
                     ((null? curr-ancs)  ; everyone processed
                      (loop (cdr src)
                            new-ancestors
                            res))
                     ((test-pred? (car curr-ancs))
                      ; add to the result
                      (creat (cons (car curr-ancs) new-ancestors)
                             (cdr curr-ancs)
                             (cons
                              (draft:smart-make-context
                               (car curr-ancs)
                               new-ancestors
                               num-anc)
                              res)))
                     (else  ; this node doesn't satisfy the node test
                      (creat (cons (car curr-ancs) new-ancestors)
                             (cdr curr-ancs)
                             res)))))))
            ; no ancestors for this node
            (loop (cdr src) prev-ancestors res))))))))
            
; Ancestor-or-self axis
; See the comment for ddo:ancestor
(define (ddo:ancestor-or-self test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-ancestors '())
                 (res '()))
        (if
         (null? src)  ; everyone processed
         (reverse res)
         (let rpt ((curr-ancs (reverse
                               (sxml:context->content (car src))))
                   (dupl '()))
           (cond
             ((null? curr-ancs)  ; no new ancestors
              (loop (cdr src) prev-ancestors res))
             ((memq (car curr-ancs) prev-ancestors)
              ; member of the ancestorial chain
              (rpt (cdr curr-ancs)
                   (cons (car curr-ancs) dupl)))
             (else  ; the first different ancestor in a chain found
              (let creat ((new-ancestors dupl)
                          (curr-ancs curr-ancs)
                          (res res))
                (cond
                  ((null? curr-ancs)  ; everyone processed
                   (loop (cdr src)
                         new-ancestors
                         res))
                  ((test-pred? (car curr-ancs))
                   ; add to the result
                   (creat (cons (car curr-ancs) new-ancestors)
                          (cdr curr-ancs)
                          (cons
                           (draft:smart-make-context
                            (car curr-ancs)
                            new-ancestors
                            num-anc)
                           res)))
                  (else  ; this node doesn't satisfy the node test
                   (creat (cons (car curr-ancs) new-ancestors)
                          (cdr curr-ancs)
                          res))))))))))))

; Attribute axis
; The alias for drart:attribute, since no reordering or duplicate elimination
; is required
(define ddo:attribute draft:attribute)

; Child axis
; If the input node is such that node of the nodes is the descendant of
; another, draft:child will produce the result in distinct document
; order
; In the general case, the implementation for child axis is more
; complicated, since it must provide the correct ordering
(define (ddo:child test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        ((child4this
          ; Arguments
          ;  curr-node - current node (or context) to process
          ;  src - the remaining nodes of the input node-set
          ; Returns: (values res src)
          (lambda (curr-node src)
            (let iter-attrs ((src src)
                             (res '()))
              (cond
                ((null? src)  ; the zoo is completely over
                 (values
                  (append res
                          ((draft:child test-pred? num-anc) curr-node))
                  src  ; null
                  ))
                ((memq (sxml:context->node (car src))
                       (sxml:attr-list (sxml:context->node curr-node)))
                 ; next in src is the attribute of the curr-node
                 (iter-attrs
                  (cdr src)
                  (append res ((draft:child test-pred? num-anc) (car src)))))
                (else   ; normal behaviour
                 (let ((res-ancestors
                        (sxml:context->content curr-node)))
                   (let iter-cands ((res-candidates
                                     (child (sxml:context->node curr-node)))
                                    (src src)
                                    (res res))
                     (cond
                       ((null? src)  ; this zoo is over
                        (values
                         (append
                          res
                          (draft:siblings->context-set
                           ((sxml:filter test-pred?) res-candidates)
                           (draft:list-head res-ancestors num-anc)))
                         src  ; always null
                         ))
                       ((null? res-candidates)
                        (values res src))
                       (else  ; processing the current res-candidate
                        (let rpt ((more (list (car res-candidates)))
                                  (next (sxml:context->node (car src)))
                                  (src src)
                                  (res
                                   (if
                                    (test-pred? (car res-candidates))
                                    (append
                                     res
                                     (list
                                      (draft:smart-make-context
                                       (car res-candidates)
                                       res-ancestors num-anc)))
                                    res)))
                          (cond
                            ((null? more)
                             ; no more src members
                             (iter-cands (cdr res-candidates) src res))
                            ((eq? (car more) next)
                             ; next node is a descendant-or-self candidate
                             ; or the attribute of its descendants
                             (call-with-values
                              (lambda () (child4this (car src) (cdr src)))
                              (lambda (add-res new-src)
                                (if
                                 (null? new-src)
                                 (iter-cands   ; will exit there
                                  (cdr res-candidates)
                                  new-src
                                  (append res add-res))
                                 (rpt
                                  (cdr more)  ; kids processed by recursive
                                  (sxml:context->node (car new-src))
                                  new-src
                                  (append res add-res))))))
                            (else
                             (rpt
                              (append (ddo:attr-child (car more))
                                      (cdr more))
                              next src res))))))))))))))
      (lambda (node)   ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((nset node)
                    (res '()))
           (if
            (null? nset)
            res
            (call-with-values
             (lambda () (child4this (car nset) (cdr nset)))
             (lambda (add-res new-nset)
               (iter new-nset (append res add-res))))))
         ((draft:child test-pred? num-anc) node))))))
        
; Descendant axis
; We should take into account that one node in the input node set may be the
; descendant of the other node in the input node-set. Evaluation of descendant
; axis should not take the former node into account then, since its descendants
; won't add any new nodes to the result with respects to descendants of the
; latter node.
; Note that if the input node is such that node of the nodes is the descendant
; of another, draft:descendant will produce the result in distinct document
; order
(define (ddo:descendant test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc (draft:descendant test-pred? num-anc)))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (next-node #f)
                 (content-to-scan '())
                 (res '()))
        (if
         (null? content-to-scan)
         (cond
           ((null? src)  ; everyone processed
            (reverse res))
           ((null? (cdr src))  ; of length 1 => never produces duplicates
            (append (reverse res)
                    (desc (car src))))
           (else
            (loop (cdr src)
                  (sxml:context->node (cadr src))
                  (let ((cntnt (sxml:context->content (car src))))
                    (map
                     (lambda (c) (cons c cntnt))
                     (child (sxml:context->node (car src)))))
                  res)))
         (let ((curr-cntnt (car content-to-scan)))
           (call-with-values
            (lambda ()
              (if
               ; next input node should be removed from consideration
               (eq? (car curr-cntnt) next-node)
               (values
                (cdr src)
                (if (null? (cdr src))  ; no next node
                    #f 
                    (sxml:context->node (cadr src))))
               (values src next-node)))
            (lambda (new-src new-next)
              (loop new-src
                    new-next
                    (append
                     (map
                      (lambda (c) (cons c curr-cntnt))
                      (child (car curr-cntnt)))
                     (cdr content-to-scan))
                    (if
                     (test-pred? (car curr-cntnt))  ; satisfies the node test
                     (cons
                      (draft:smart-make-context
                       (car curr-cntnt) (cdr curr-cntnt) num-anc)
                      res)
                     res))))))))))

; Descendant-or-self axis
; See the comment for ddo:descendant
; Note that if the input node is such that node of the nodes is the descendant
; of another, draft:descendant-or-self will produce the result in distinct
; document order
(define (ddo:descendant-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc-os (draft:descendant-or-self test-pred? num-anc)))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (next-node #f)
                 (content-to-scan '())
                 (res '()))
        (if
         (null? content-to-scan)
         (cond
           ((null? src)  ; everyone processed
            (reverse res))
           ((null? (cdr src))  ; of length 1 => never produces duplicates
            (append (reverse res)
                    (desc-os (car src))))
           (else
            (loop (cdr src)
                  (sxml:context->node (cadr src))
                  (list (sxml:context->content (car src)))
                  res)))
         (let ((curr-cntnt (car content-to-scan)))
           (call-with-values
            (lambda ()
              (if
               ; next input node should be removed from consideration
               (eq? (car curr-cntnt) next-node)
               (values
                (cdr src)
                (if (null? (cdr src))  ; no next node
                    #f 
                    (sxml:context->node (cadr src))))
               (values src next-node)))
            (lambda (new-src new-next)
              (loop new-src
                    new-next
                    (append
                     (map
                      (lambda (c) (cons c curr-cntnt))
                      (child (car curr-cntnt)))
                     (cdr content-to-scan))
                    (if
                     (test-pred? (car curr-cntnt))  ; satisfies the node test
                     (cons
                      (draft:smart-make-context
                       (car curr-cntnt) (cdr curr-cntnt) num-anc)
                      res)
                     res))))))))))

; Following axis
; The implementation exploits the idea expressed in
; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
; that is, it is sufficient to calculate following for the first_dmax
; member of the input nodeset
(define (ddo:following test-pred? . num-ancestors)
  (let ((child (sxml:child sxml:node?))
        (foll (apply draft:following (cons test-pred? num-ancestors))))
    (lambda (node)   ; node or nodeset
      (cond
        ((null? node)  ; empty nodeset - nothing to do
         '())
        ((and (pair? node) (not (symbol? (car node))))  ; non-empty nodeset
         (if
          (null? (cdr node))  ; a singleton nodeset
          (foll (car node))
          (let loop ((candidate (car node))
                     (next (sxml:context->node (cadr node)))
                     (more (cdr node))
                     (descendants (list (sxml:context->node (car node)))))
            (cond
              ((null? descendants)
               ; none of the node-set members are descendants of the candidate
               ; => apply following axis
               (foll candidate))
              ((eq? (car descendants) next)
               ; the next node is the new candidate
               (if (null? (cdr more))  ; the next node is the final candidate
                   (foll (car more))
                   (loop (car more)
                         (sxml:context->node (cadr more))
                         (cdr more)
                         (list next))))
              ((memq next (ddo:attrs-and-values (car descendants)))
               ; next node in src is an attribute node or attribute value node
               (foll (car more)))
              (else  ; proceed deeper in a tree
               (loop candidate next more
                     (append (child (car descendants)) (cdr descendants))))))))
        (else  ; a single node
         (foll node))))))

; Following-sibling axis
(define (ddo:following-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (all-following-siblings
         (lambda (node)  ; not a nodeset
           (if
            (and (sxml:context? node)                  
                 (not (null? (sxml:context->ancestors-u node))))
            (cond
              ((memq (sxml:context->node-u node)
                     (cdr  ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u node))))
               => (lambda (x) x))
              (else  ; no following siblings
               '()))
            '()  ; no parent => no siblings
            ))))
    (letrec
        ((reordering
          ; Arguments
          ;  res-candidates = (listof node) - candidates for the result, not
          ; yet filtered with a node test
          ;  res-ancestors - ancestors of res-candidates
          ;  src - the remaining nodes of the input node-set
          ; Returns: (values res src)
          (lambda (res-candidates res-ancestors src)
            (let loop ((res-candidates res-candidates)
                       (src src)
                       (res '())
                       (nonself? #f))
              (cond
                ((null? res-candidates)
                 (values res src))
                ((null? src)  ; this zoo is over
                 (values
                  (append
                   res
                   (draft:siblings->context-set
                    ((sxml:filter test-pred?)
                     (if nonself?
                         res-candidates
                         (cdr res-candidates)))
                    (draft:list-head res-ancestors num-anc)))
                  src  ; always null
                  ))
                ((eq? (car res-candidates) (sxml:context->node (car src)))
                 (loop res-candidates (cdr src) res nonself?))
                (else  ; processing the current res-candidate
                 (let ((res-candidate (car res-candidates)))
                   (let rpt ((more (list res-candidate))
                             (next (sxml:context->node (car src)))
                             (src src)
                             (res (if
                                   (and nonself? (test-pred? res-candidate))
                                   (append
                                    res
                                    (list
                                      (draft:smart-make-context
                                       res-candidate res-ancestors num-anc)))
                                   res)))
                     (cond
                       ((null? more)
                        ; no more src members among res-candidate descendants
                        (loop (cdr res-candidates) src res #t))
                       ((eq? (car more) next)
                        ; next node is a descendant-or-self of res-candidate
                        (call-with-values
                         (lambda ()
                           (reordering
                            (all-following-siblings (car src))
                            (sxml:context->ancestors (car src))
                            (cdr src)))
                         (lambda (add-res new-src)
                           (if
                            (null? new-src)
                            (loop (cdr res-candidates)
                                  new-src
                                  (append res add-res)
                                  #t)
                            (rpt (cdr more)  ; kids processed by recursive
                                 (sxml:context->node (car new-src))
                                 new-src
                                 (append res add-res))))))
                       ((memq next (ddo:attrs-and-values (car more)))
                        ; the next node is the attribute node or
                        ; attribute value node => it has no siblings
                        (if
                          (null? (cdr src))
                          (loop (cdr res-candidates)
                                (cdr src)  ; null
                                res
                                #t)
                          (rpt more  ; check for the other attributes
                               (sxml:context->node (car src))
                               (cdr src)
                               res)))
                       (else
                        (rpt (append (child (car more)) (cdr more))
                             next src res)))))))))))
      (lambda (node)   ; node or nodeset
        (if
         (nodeset? node)        
         (let iter ((nset node)
                    (res '()))
           (if
            (null? nset)
            res
            (call-with-values
             (lambda ()
               (reordering (all-following-siblings (car nset))
                           (sxml:context->ancestors (car nset))
                           (cdr nset)))
             (lambda (add-res new-nset)
               (iter new-nset (append res add-res))))))
         ((draft:following-sibling test-pred? num-anc) node))))))
    
; Namespace axis
; The alias for drart:namespace, since no reordering or duplicate elimination
; is required
(define ddo:namespace draft:namespace)

; Parent axis
; When locating a parent, the thing we should care about is that several nodes
; in the input node-set may have the same parent node
(define (ddo:parent test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-parents '())
                 (res '()))
        (if
         (null? src)
         (reverse res)
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (let ((curr-parent (car (sxml:context->ancestors-u curr))))
              (if
               (memq curr-parent prev-parents)  ; a duplicate node               
               ; this node is already in the result
               (loop (cdr src) prev-parents res)
               (loop (cdr src)
                     (cons curr-parent prev-parents)
                     (if
                      (test-pred? curr-parent)
                      (cons
                        (draft:smart-make-context
                         curr-parent
                         (cdr (sxml:context->ancestors-u curr))
                         num-anc)
                        res)
                      res))))
            ; no parent
            (loop (cdr src) prev-parents res))))))))

; Preceding axis
; The implementation exploits the idea expressed in
; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
; that is, it is sufficient to calculate preceding for the last_dmin member
; of the input nodeset
(define (ddo:preceding test-pred? . num-ancestors)
  (let ((prec (apply draft:preceding (cons test-pred? num-ancestors))))
    (lambda (node)   ; node or nodeset
      (cond
        ((null? node)  ; empty nodeset - nothing to do
         '())        
        ((and (pair? node) (not (symbol? (car node))))  ; non-empty nodeset
         (if
          (null? (cdr node))  ; a singleton nodeset
          (prec (car node))
          (let ((node (reverse node)))
            (let loop ((candidate (car node))
                       (next (sxml:context->node (cadr node)))
                       (more (cdr node))
                       (descendants
                        (reverse
                         (ddo:attr-child (sxml:context->node (car node))))))
              (cond
                ((null? descendants)
                 ; none of the node-set members are descendants of the candidate
                 ; => apply following axis
                 (reverse (prec candidate)))
                ((eq? (car descendants) next)
                 ; the next node is the new candidate
                 (if (null? (cdr more))  ; the next node is the final candidate
                     (reverse (prec (car more)))
                     (loop (car more)
                           (sxml:context->node (cadr more))
                           (cdr more)
                           (reverse (ddo:attr-child next)))))
                (else  ; proceed deeper in a tree
                 (loop candidate next more
                       (append (reverse (ddo:attr-child (car descendants)))
                               (cdr descendants)))))))))
         (else  ; a single node
          (reverse (prec node)))))))

; Preceding-sibling axis
(define (ddo:preceding-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (all-preceding-siblings
         ; Selects preceding siblings of the node (should be context)
         (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)                  
                  (not (null? (sxml:context->ancestors-u node))))
             (cond
               ((memq (sxml:context->node-u node)
                      (reverse
                       (cdr  ; parent is an element => cdr gives its children
                        (car (sxml:context->ancestors-u node)))))
                => cdr)
               (else  ; no preceding siblings
                '()))
             '()  ; no parent => no siblings
             ))))
    (letrec
        ((reordering
          ; Arguments
          ;  res-candidates = (listof node) - candidates for the result, not
          ; yet filtered with a node test
          ;  res-ancestors - ancestors of res-candidates
          ;  src - the remaining nodes of the input node-set
          ; Returns: (values res src)
          (lambda (res-candidates res-ancestors src)
            (let loop ((res-candidates res-candidates)
                       (src src)
                       (res '()))              
              (cond
                ((null? res-candidates)
                 (values res src))
                ((null? src)  ; this zoo is over
                 (values
                  (append
                   res
                   (draft:siblings->context-set
                    ((sxml:filter test-pred?) res-candidates)
                    (draft:list-head res-ancestors num-anc)))
                  src  ; always null
                  ))
                ((eq? (car res-candidates) (sxml:context->node (car src)))
                 (loop res-candidates (cdr src) res))
                (else  ; processing the current res-candidate
                 (let ((res-candidate (car res-candidates)))
                   (let rpt ((more (reverse (child res-candidate)))
                             (next (sxml:context->node (car src)))
                             (src src)
                             (res res))
                     (cond
                       ((null? more)
                        ; no more src members among res-candidate descendants
                        (loop
                         (cdr res-candidates)
                         src
                         (if (test-pred? res-candidate)
                             (append res
                                     (list
                                      (draft:smart-make-context
                                       res-candidate res-ancestors num-anc)))
                             res)))
                       ((eq? (car more) next)
                        ; next node is a descendant-or-self of res-candidate
                        (call-with-values
                         (lambda ()
                           (reordering
                            (all-preceding-siblings (car src))
                            (sxml:context->ancestors (car src))
                            (cdr src)))
                         (lambda (add-res new-src)
                           (let ((new-src
                                  (cond
                                    ((null? new-src) new-src)
                                    ((eq? res-candidate
                                          (sxml:context->node (car new-src)))
                                     (cdr new-src))
                                    (else new-src))))
                             (if
                              (null? new-src)
                              (loop (cdr res-candidates)
                                    new-src
                                    (if
                                     (test-pred? res-candidate)
                                     (append
                                      res
                                      add-res                                 
                                      (list
                                       (draft:smart-make-context
                                        res-candidate res-ancestors num-anc)))
                                     (append res add-res)))
                              (rpt (cdr more)  ; kids processed by recursive
                                   (sxml:context->node (car new-src))
                                   new-src
                                   (append res add-res)))))))
                       (else
                        (rpt (append (reverse (child (car more))) (cdr more))
                             next src res)))))))))))
      (lambda (node)   ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((nset (reverse node))
                    (res '()))
           (if
            (null? nset)
            (reverse res)
            (call-with-values
             (lambda ()
               (reordering (all-preceding-siblings (car nset))
                           (sxml:context->ancestors (car nset))
                           (cdr nset)))
             (lambda (add-res new-nset)
               (iter new-nset (append res add-res))))))
         ((draft:following-sibling test-pred? num-anc) node))))))
  
; Self axis
; The alias for drart:self, since no reordering or duplicate elimination
; is required
; num-ancestors is not used here
(define ddo:self draft:self)

;-------------------------------------------------
; Particular case: all nodes in the input node-set are on the same level of
; hierarchy within a document
; In this case, some axes can be implemented more effectively

; Following axis for special case
; According to
; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
; it is sufficient to calculate following for the first member of the input
; nodeset
(define (ddo:following-single-level test-pred? . num-ancestors)
  (let ((foll (apply draft:following
                     (cons test-pred? num-ancestors))))
    (lambda (node)   ; node or nodeset
      (cond
        ((null? node)  ; empty nodeset - nothing to do
         '())
        ((and (pair? node) (not (symbol? (car node))))  ; non-empty nodeset
         (foll (car node)))
        (else  ; a single node
         (foll node))))))

; Following-sibling axis for the special case
; We need only to care of duplicates removal, and ordering would be
; achieved automatically
(define (ddo:following-sibling-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))    
                 (res '()))
        (if
         (null? src)  ; everyone processed
         (reverse res)
         (let ((curr (car src)))           
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (cond
              ((memq (sxml:context->node-u curr)
                     (cdr  ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u curr))))
               => (lambda (foll-siblings)
                    (let rpt ((foll-siblings (cdr foll-siblings))
                              (src (cdr src))
                              (res res))
                      (cond
                        ((null? foll-siblings)
                         (loop src res))
                        ((null? src)  ; no more source nodes in document order
                         (append
                          (reverse res)
                          (draft:siblings->context-set
                           ((sxml:filter test-pred?) foll-siblings)
                           (draft:list-head
                            (sxml:context->ancestors-u curr) num-anc))))
                        (else                       
                         (rpt
                          (cdr foll-siblings)
                          (if (eq? (car foll-siblings)
                                   (sxml:context->node (car src)))
                              (cdr src)  ; remove the first source node
                              src)
                          (if (test-pred? (car foll-siblings))
                              (cons
                               (draft:smart-make-context
                                (car foll-siblings)
                                (sxml:context->ancestors-u curr)
                                num-anc)
                               res)
                              res)))))))
              (else  ; no following siblings
               (loop (cdr src) res)))
            (loop (cdr src) res)  ; no parent => no siblings
            )))))))

; Parent axis for the case when all nodes in the input node-set are located
; on the same level of hierarchy within a document
; In this case the parent axis can be computed with the O(n) complexity, n
; is the number of nodes in the document, compared to O(n^2) complexity for
; ddo:parent
(define (ddo:parent-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-parent #f)
                 (res '()))
        (if
         (null? src)
         (reverse res)
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (let ((curr-parent (car (sxml:context->ancestors-u curr))))
              (if
               ; this condition would never evaluate to #t when prev-parent=#f
               (eq? curr-parent prev-parent)  ; a duplicate node
               ; this node is already in the result
               (loop (cdr src) prev-parent res)
               (loop (cdr src) curr-parent
                     (if
                      (test-pred? curr-parent)
                      (cons
                       (draft:smart-make-context
                        curr-parent
                        (cdr (sxml:context->ancestors-u curr))
                        num-anc)
                       res)
                      res))))
            ; no parent
            (loop (cdr src) prev-parent res))))))))

; Preceding axis for the special case
; The implementation exploits the idea expressed in
; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
; that is, it is sufficient to calculate preceding for the last member of the
; input nodeset
(define (ddo:preceding-single-level test-pred? . num-ancestors)
  (let ((prec (apply draft:preceding
                     (cons test-pred? num-ancestors))))
    (lambda (node)   ; node or nodeset
      (cond
        ((null? node)  ; empty nodeset - nothing to do
         '())
        ((and (pair? node) (not (symbol? (car node))))  ; non-empty nodeset
         (reverse  ; restore document order
          (prec (ddo:list-last node))))
        (else  ; a single node
         (reverse (prec node)))))))

; Preceding-sibling axis for the special case
(define (ddo:preceding-sibling-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)   ; node or nodeset
      (let loop ((src (reverse (as-nodeset node)))
                 (res '()))
        (if
         (null? src)  ; everyone processed
         res
         (let ((curr (car src)))           
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (cond
              ((memq (sxml:context->node-u curr)
                     (reverse
                      (cdr  ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u curr)))))
               => (lambda (prec-siblings)
                    (let rpt ((prec-siblings (cdr prec-siblings))
                              (src (cdr src))
                              (res res))
                      (cond
                        ((null? prec-siblings)
                         (loop src res))
                        ((null? src)  ; no more source nodes
                         (append
                          (reverse
                           (draft:siblings->context-set
                            ((sxml:filter test-pred?) prec-siblings)
                            (draft:list-head
                             (sxml:context->ancestors-u curr) num-anc)))
                          res))
                        (else                       
                         (rpt
                          (cdr prec-siblings)
                          (if (eq? (car prec-siblings)
                                   (sxml:context->node (car src)))
                              (cdr src)  ; remove the first source node
                              src)
                          (if (test-pred? (car prec-siblings))
                              (cons
                               (draft:smart-make-context
                                (car prec-siblings)
                                (sxml:context->ancestors-u curr)
                                num-anc)
                               res)
                              res)))))))
              (else  ; no preceding siblings
               (loop (cdr src) res)))
            (loop (cdr src) res)  ; no parent => no siblings
            )))))))


;=========================================================================
; XPath axes for location steps probably involving position-based predicates
; Result is represented in the form of
;  pos-result ::= (listof pos-nodeset)
;  pos-nodeset ::= (listof (cons node order-num))
; Each pos-nodeset is a result of applying the axis to a single node in the
; input nodeset. Pos-result can be informally considered as
;  (map axis-pos input-nodeset)
; Each node in the pos-nodeset comes with its order number. An order-num is
; an integer, possibly a negative one. A node precedes another node in
; document order if the order-num of the former node is less than the order-num
; of the latter node. Equal order-nums (in different pos-nodesets) correspond
; to equal nodes.
; Each pos-nodeset is sorted in accordance with the position() of each of its
; members. Consequently, order-nums increase within pos-nodeset for forward
; XPath axes and decrease for reverse XPath axes.

; Ancestor axis, for position-based filtering
(define (ddo:ancestor-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        (; A hybrid of assq and memq
         (assmemq
          (lambda (key lst)
            (cond
              ((null? lst) #f)
              ((eq? key (caar lst)) lst)
              (else (assmemq key (cdr lst)))))))
      (lambda (node)   ; node or nodeset
        (let loop ((src (as-nodeset node))
                   (prev-ancestors '())
                   (ancs-alist '())
                   (pos-res '())
                   (vacant-num 1))
          ; ancs-alist ::= (listof (cons node pos-nodeset))                
          (if
           (null? src)  ; everyone processed
           pos-res
           (let ((curr (car src)))
             (cond
               ((or (not (sxml:context? curr))
                    (null? (sxml:context->ancestors-u curr)))
                ; no ancestors for this node
                (loop (cdr src) prev-ancestors ancs-alist pos-res vacant-num))
               ((and (not (null? prev-ancestors))
                     (eq? (car (sxml:context->ancestors-u curr))
                          (car prev-ancestors)))
                ; The common case of removing (some) duplicate result node-sets
                ; from consideration.
                (loop (cdr src) prev-ancestors ancs-alist pos-res vacant-num))
               (else
                (let rpt ((curr-ancs (sxml:context->ancestors-u curr))
                          (new-content '()))
                  ; new content - that didn't repeat with the previous
                  ; ancestors
                  (cond
                    ((or (null? curr-ancs)  ; all ancestors are new
                         ; the first repeated found
                         (memq (car curr-ancs) prev-ancestors))
                     => (lambda (prev-tail)
                          (call-with-values
                           (lambda()
                             (if
                              (pair? prev-tail)
                              (let ((t
                                     (assmemq (car prev-tail) ancs-alist)))
                                (values prev-tail t (cdar t)))
                              (values '() '() '())))
                           (lambda (prev-ancestors ancs-alist this-nset)
                             (let creat ((prev-ancestors prev-ancestors)
                                       (ancs-alist ancs-alist)
                                       (vacant-num vacant-num)
                                       (this-nset this-nset)
                                       (new-content new-content))
                             (if
                              (null? new-content)  ; everyone processed
                              (loop (cdr src)
                                    prev-ancestors
                                    ancs-alist
                                    (cons this-nset pos-res)
                                    vacant-num)
                              (let ((new-this-nset
                                     (if
                                      (test-pred? (caar new-content))
                                      ; add to the result
                                      (cons
                                       (cons
                                        (draft:smart-make-context
                                         (caar new-content)
                                         (cdar new-content)
                                         num-anc)
                                        vacant-num)
                                       this-nset)
                                      this-nset)))
                                (creat (car new-content)
                                       (cons
                                        (cons
                                         (caar new-content)
                                         new-this-nset)
                                        ancs-alist)
                                       (+ vacant-num 1)
                                       new-this-nset
                                       (cdr new-content)))))))))
                    (else
                     (rpt (cdr curr-ancs)
                          (cons curr-ancs new-content))))))))))))))

; Ancestor-or-self axis, for position-based filtering
(define (ddo:ancestor-or-self-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        (; A hybrid of assq and memq
         (assmemq
          (lambda (key lst)
            (cond
              ((null? lst) #f)
              ((eq? key (caar lst)) lst)
              (else (assmemq key (cdr lst)))))))
      (lambda (node)   ; node or nodeset
        (let loop ((src (as-nodeset node))
                   (prev-ancestors '())
                   (ancs-alist '())
                   (pos-res '())
                   (vacant-num 1))
          ; ancs-alist ::= (listof (cons node pos-nodeset))                
          (if
           (null? src)  ; everyone processed
           pos-res
           (let rpt ((curr-ancs (sxml:context->content (car src)))
                     (new-content '()))
             ; new content - that didn't repeat with the previous
             ; ancestors
             (cond
               ((or (null? curr-ancs)  ; all ancestors are new
                    ; or the first repeated found
                    (memq (car curr-ancs) prev-ancestors))
                => (lambda (prev-tail)                     
                     (call-with-values
                      (lambda ()
                        (if
                         (pair? prev-tail)
                         (let ((t (assmemq (car prev-tail) ancs-alist)))
                           (values prev-tail t (cdar t)))
                         (values '() '() '())))
                      (lambda (prev-ancestors ancs-alist this-nset)
                        (let creat ((prev-ancestors prev-ancestors)
                                    (ancs-alist ancs-alist)
                                    (vacant-num vacant-num)
                                    (this-nset this-nset)
                                    (new-content new-content))
                          (if
                           (null? new-content)  ; everyone processed
                           (loop (cdr src)
                                 prev-ancestors
                                 ancs-alist
                                 (cons this-nset pos-res)
                                 vacant-num)
                           (let ((new-this-nset
                                  (if
                                   (test-pred? (caar new-content))
                                   ; add to the result
                                   (cons
                                    (cons
                                     (draft:smart-make-context
                                      (caar new-content)
                                      (cdar new-content)
                                      num-anc)
                                     vacant-num)
                                    this-nset)
                                   this-nset)))
                             (creat (car new-content)
                                    (cons
                                     (cons
                                      (caar new-content)
                                      new-this-nset)
                                     ancs-alist)
                                    (+ vacant-num 1)
                                    new-this-nset
                                    (cdr new-content)))))))))
               (else
                (rpt (cdr curr-ancs)
                     (cons curr-ancs new-content)))))))))))

; Child axis, for position-based filtering
(define (ddo:child-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        (; Creates a pos-nodeset
         (create-pos-nset
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                '()
                (cons
                 (cons (if (null? ancestors)
                           (car nset)
                           (draft:make-context (car nset) ancestors))
                       vacant-num)
                 (create-pos-nset (cdr nset) ancestors (+ vacant-num 1))))))
         (src-walk
          ; curr-node - current input node (probably context)
          ; src - the remaining nodes in the input nodeset
          ; order-num - the order number of the current result node
          ; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let ((curr-children
                   (child (sxml:context->node curr-node))))
              (if
               (null? curr-children)  ; nothing to do for this curr-node
               (values '() src order-num)
               (let ((curr-ancestors (draft:list-head
                                      (sxml:context->content curr-node)
                                      num-anc)))
                 (if
                  (null? src)  ; no searching for descendants required
                  (values (list (create-pos-nset
                                 ((sxml:filter test-pred?) curr-children)
                                 curr-ancestors order-num))
                          src  ; always null
                          #f  ; nobody cares of order-num anymore
                          )
                  (let loop ((src src)
                             (next-node (sxml:context->node (car src)))
                             (curr-children (cdr curr-children))
                             (desc-to-scan (list (car curr-children)))
                             (this-res
                              (if
                               (test-pred? (car curr-children))
                               (list
                                (cons
                                 (if (null? curr-ancestors)
                                     (car curr-children)
                                     (draft:make-context
                                      (car curr-children) curr-ancestors))
                                 order-num))
                               '()))
                             (pos-result '())
                             (order-num (+ order-num 1)))
                    (cond
                      ((null? desc-to-scan)
                       ; we can proceed to next curr-children
                       (if
                        (null? curr-children)
                        (values (cons (reverse this-res) pos-result)
                                src
                                order-num)
                        (loop src next-node
                              (cdr curr-children)
                              (list (car curr-children))
                              (if
                               (test-pred? (car curr-children))
                               (cons
                                (cons
                                 (if (null? curr-ancestors)
                                     (car curr-children)
                                     (draft:make-context
                                      (car curr-children) curr-ancestors))
                                 order-num)
                                this-res)
                               this-res)
                              pos-result
                              (+ order-num 1))))
                      ; There are descendants to be scanned
                      ((eq? (car desc-to-scan) next-node)
                       (call-with-values
                        (lambda ()
                          (src-walk (car src)
                                    (cdr src)
                                    order-num))
                        (lambda (new-pos-res new-src new-order-num)
                          (if
                           (null? new-src)  ; no more nodes in src nodeset
                           (values
                            (cons
                             (append
                              (reverse this-res)
                              (create-pos-nset
                               ((sxml:filter test-pred?) curr-children)
                               curr-ancestors order-num))
                             (append pos-result new-pos-res))
                            new-src  ; always null
                            #f  ; nobody cares of this number anymore
                            )
                           (loop new-src
                                 (sxml:context->node (car new-src))
                                 curr-children
                                 (cdr desc-to-scan)  ; descendants processed
                                 this-res
                                 (append pos-result new-pos-res)
                                 new-order-num)))))
                      (else  ; proceed to the next descendant
                       (loop src next-node curr-children
                             (append  ; content-to-scan
                              (ddo:attr-child (car desc-to-scan))
                              (cdr desc-to-scan))
                             this-res
                             pos-result
                             order-num)))))))))))
      (lambda (node)   ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)  ; all processed
           (filter  ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
            (lambda () (src-walk (car src) (cdr src) order-num))
            (lambda (new-pos-res new-src new-order-num)
              (rpt new-src
                   (append pos-result new-pos-res)
                   new-order-num)))))))))

; Descendant axis, for position-based filtering
; When no node in the input node-set is a descendant of another node in the
; input node-set, the ordinary draft:descendant function can be used
(define (ddo:descendant-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc (draft:descendant test-pred? num-anc)))
    (letrec                 
        ((src-walk
          ; curr-node - current input node (probably context)
          ; src - the remaining nodes in the input nodeset
          ; order-num - the order number of the current result node
          ; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let loop ((src src)
                       (next-node (if (null? src)
                                      #f
                                      (sxml:context->node (car src))))
                       (content-to-scan
                        (let ((cntnt (sxml:context->content curr-node)))
                          (map
                           (lambda (c) (cons c cntnt))
                           (child (sxml:context->node curr-node)))))                      
                       (this-res '())
                       (pos-result '())
                       (order-num order-num))
              (if
               (null? content-to-scan)
               (values (cons (reverse this-res) pos-result)
                       src
                       (+ order-num 1))
               (let ((curr-cntnt (car content-to-scan)))
                 (if
                  (eq? (car curr-cntnt) next-node)
                  (call-with-values
                   (lambda () (src-walk (car src)
                                        (cdr src)
                                        (+ order-num 1)))
                   (lambda (new-pos-res new-src new-order-num)
                     (loop new-src
                           (if (null? new-src)
                               #f
                               (sxml:context->node (car new-src)))
                           (cdr content-to-scan)  ; descendants processed
                           (append
                            (reverse (car new-pos-res))
                            (if  ; this res
                             (test-pred? (car curr-cntnt))
                             (cons
                              (cons
                               (draft:smart-make-context
                                (car curr-cntnt) (cdr curr-cntnt) num-anc)
                               order-num)
                              this-res)
                             this-res))
                           (append pos-result new-pos-res)
                           new-order-num)))
                  (loop src
                        next-node
                        (append  ; content-to-scan
                         (map
                          (lambda (c) (cons c curr-cntnt))
                          (child (car curr-cntnt)))
                         (cdr content-to-scan))
                        (if  ; this res
                         (test-pred? (car curr-cntnt))  ; satisfies the node test
                         (cons
                          (cons
                           (draft:smart-make-context
                            (car curr-cntnt) (cdr curr-cntnt) num-anc)
                           order-num)
                          this-res)
                         this-res)
                        pos-result
                        (+ order-num 1)))))))))
      (lambda (node)   ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)  ; all processed
           (filter  ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
            (lambda () (src-walk (car src) (cdr src) order-num))
            (lambda (new-pos-res new-src new-order-num)
              (rpt new-src
                   (append pos-result new-pos-res)
                   new-order-num)))))))))

; Descendant-or-selt axis, for position-based filtering
; When no node in the input node-set is a descendant of another node in the
; input node-set, the ordinary draft:descendant function can be used
(define (ddo:descendant-or-self-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?)))
    (letrec                 
        ((src-walk
          ; curr-node - current input node (probably context)
          ; src - the remaining nodes in the input nodeset
          ; order-num - the order number of the current result node
          ; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let loop ((src src)
                       (next-node (if (null? src)
                                      #f
                                      (sxml:context->node (car src))))
                       (content-to-scan
                        (list (sxml:context->content curr-node)))
                       (this-res '())
                       (pos-result '())
                       (order-num order-num))
              (if
               (null? content-to-scan)
               (values (cons (reverse this-res) pos-result)
                       src
                       (+ order-num 1))
               (let ((curr-cntnt (car content-to-scan)))
                 (if
                  (eq? (car curr-cntnt) next-node)
                  (call-with-values
                   (lambda () (src-walk (car src) (cdr src) order-num))
                   (lambda (new-pos-res new-src new-order-num)
                     (loop new-src
                           (if (null? new-src)
                               #f
                               (sxml:context->node (car new-src)))
                           (cdr content-to-scan)  ; descendants processed
                           (append
                            (reverse (car new-pos-res))
                            this-res)
                           (append pos-result new-pos-res)
                           new-order-num)))
                  (loop src
                        next-node
                        (append  ; content-to-scan
                         (map
                          (lambda (c) (cons c curr-cntnt))
                          (child (car curr-cntnt)))
                         (cdr content-to-scan))
                        (if  ; this res
                         (test-pred? (car curr-cntnt))  ; satisfies the node test
                         (cons
                          (cons
                           (draft:smart-make-context
                            (car curr-cntnt) (cdr curr-cntnt) num-anc)
                           order-num)
                          this-res)
                         this-res)
                        pos-result
                        (+ order-num 1)))))))))
      (lambda (node)   ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)  ; all processed
           (filter  ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
            (lambda () (src-walk (car src) (cdr src) order-num))
            (lambda (new-pos-res new-src new-order-num)
              (rpt new-src
                   (append pos-result new-pos-res)
                   new-order-num)))))))))

; Following-sibling axis, for position-based filtering
(define (ddo:following-sibling-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        ((associate-num
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                nset
                (cons
                 (cons
                  (if (null? ancestors)
                      (car nset)
                      (draft:make-context (car nset) ancestors))
                  vacant-num)
                 (associate-num (cdr nset) ancestors (+ vacant-num 1))))))         
         ; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  (values pos-result new-src new-vacant-num)
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
             ; Siblings cannot be identified
             (values '() src vacant-num)
             (cond
               ((memq (sxml:context->node-u curr)
                      (cdr  ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u curr))))
                =>
                (lambda (foll-siblings)
                  (let ((ancestors
                         (draft:list-head
                          (sxml:context->ancestors-u curr) num-anc)))
                    ; Scanning descendants of the context node
                    (let loop ((foll-siblings (cdr foll-siblings))
                               (descs (child (car foll-siblings)))
                               (src (ddo:discard-attributes
                                     (car foll-siblings) src))
                               (vacant-num vacant-num)
                               (res '())
                               (pos-res '()))
                      (cond
                        ((null? src)
                         (values
                          (cons
                           (append 
                            (reverse res)
                            (associate-num
                             foll-siblings ancestors vacant-num))
                           pos-res)
                          src  ; always null
                          #f  ; nobody cares of this number anymore
                          ))                        
                        ((null? descs)  ; descendants of current foll-sibling
                         (if
                          (null? foll-siblings)  ; that stuff is over
                          (values (cons (reverse res) pos-res)
                                  src
                                  vacant-num)                           
                          (let ((new-res
                                 (if (test-pred? (car foll-siblings))
                                     (cons
                                      (cons
                                       (if (null? ancestors)
                                           (car foll-siblings)
                                           (draft:make-context
                                            (car foll-siblings) ancestors))
                                       vacant-num)
                                      res)
                                     res)))
                            (if                               
                             (eq? (car foll-siblings)
                                  (sxml:context->node (car src)))
                             (call-with-values
                              (lambda ()
                                (process-single
                                 (car src) (cdr src) (+ vacant-num 1)))
                              (lambda (new-pos-res new-src new-vacant)
                                (values (cons
                                         (append
                                          (reverse new-res)
                                          (if (null? new-pos-res)
                                              '() (car new-pos-res)))
                                         (append pos-res new-pos-res))
                                        new-src
                                        new-vacant)))
                             (loop (cdr foll-siblings)
                                   (ddo:attr-child (car foll-siblings))
                                   (ddo:discard-attributes
                                    (car foll-siblings) src)
                                   (+ vacant-num 1)
                                   new-res
                                   pos-res)))))
                        ((eq? (car descs) (sxml:context->node (car src)))
                         ; His siblings are on the way
                         (call-with-values
                          (lambda ()
                            (process-single
                             (car src) (cdr src) vacant-num))
                          (lambda (new-pos-res new-src new-vacant)
                            (loop foll-siblings
                                  (cdr descs)  ; descendants processed
                                  new-src
                                  new-vacant
                                  res
                                  (cons pos-res new-pos-res)))))
                        (else
                         (loop foll-siblings
                               (append (child (car descs)) (cdr descs))
                               (ddo:discard-attributes (car descs) src)
                               vacant-num
                               res
                               pos-res)))))))
               (else
                (values '() src vacant-num)))))))
      (lambda (node)   ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((src node)
                    (pos-res '())
                    (vacant-num 1))
           (if
            (null? src)
            (filter  ; removing empty result nodesets
             (lambda (x) (not (null? x)))
             pos-res)
            (call-with-values
             (lambda () (process-single (car src) (cdr src) vacant-num))
             (lambda (new-pos-res new-src new-vacant)
               (iter new-src
                     (append pos-res new-pos-res)
                     new-vacant))))))))))

; Parent axis, for position-based filtering
; We won't reinvent the wheel here. We'll use ddo:parent and apply the fact
; that for every single input node the parent axis produces no more than a
; single result node
(define (ddo:parent-pos test-pred? . num-ancestors)
  (let ((parent (apply ddo:parent (cons test-pred? num-ancestors))))
    (letrec
        ((add-order-num
          (lambda (num nset)
            (if (null? nset)
                nset
                (cons (list (cons (car nset) num))
                      (add-order-num (+ num 1) (cdr nset)))))))
      (lambda (node)   ; node or nodeset
        (add-order-num 1 (parent node))))))

; Preceding-sibling axis, for position-based filtering
(define (ddo:preceding-sibling-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (reverse-desc  ; reverse descendants of node
         (lambda (node)
           (let scan ((more (ddo:attr-child node))
                      (res '()))
             (if (null? more)  ; result formed
                 res
                 (scan (append (ddo:attr-child (car more))
                               (cdr more))
                       (cons (car more) res)))))))
    (letrec
        ((associate-num
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                nset
                (cons
                 (cons
                  (if (null? ancestors)
                      (car nset)
                      (draft:make-context (car nset) ancestors))
                  vacant-num)
                 (associate-num (cdr nset) ancestors (- vacant-num 1))))))         
         ; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  (values pos-result new-src new-vacant-num)
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
             ; Siblings cannot be identified
             (values '() src vacant-num)
             (cond               
               ((memq (sxml:context->node-u curr)
                      (reverse
                       (cdr  ; parent is an element => cdr gives its children
                        (car (sxml:context->ancestors-u curr)))))
                =>
                (lambda (prec-siblings)  ; prec-siblings + self
                  (if
                   (null? (cdr prec-siblings))  ; no preceding siblings
                   (values '() src vacant-num)
                   (let ((ancestors
                         (draft:list-head
                          (sxml:context->ancestors-u curr) num-anc)))
                    ; Scanning descendants of the context node
                    (let loop ((prec-siblings (cdr prec-siblings))
                               (descs (reverse-desc (cadr prec-siblings)))
                               (src src)
                               (vacant-num vacant-num)
                               (res '())
                               (pos-res '()))
                      (cond
                        ((null? src)
                         (values
                          (cons
                           (append
                            (reverse res)
                            (associate-num
                             ; DL: was: (if nonself? prec-siblings (cdr prec-siblings))
                             prec-siblings
                             ancestors vacant-num))
                           pos-res)
                          src  ; always null
                          #f  ; nobody cares of this number anymore
                          ))
                        ((null? descs)  ; descendants of current prec-sibling
                         (let ((new-res
                                 (if (test-pred? (car prec-siblings))
                                     (cons
                                      (cons
                                       (if (null? ancestors)
                                           (car prec-siblings)
                                           (draft:make-context
                                            (car prec-siblings) ancestors))
                                       vacant-num)
                                      res)
                                     res)))
                           (cond                             
                             ((eq? (car prec-siblings)  ; to be now added
                                   (sxml:context->node (car src)))
                              (call-with-values
                               (lambda ()
                                 (process-single
                                  (car src) (cdr src) (- vacant-num 1)))
                               (lambda (new-pos-res new-src new-vacant)
                                 (values (cons
                                          (append
                                           (reverse new-res)
                                           (if (null? new-pos-res)
                                               '() (car new-pos-res)))
                                          (append pos-res new-pos-res))
                                         new-src
                                         new-vacant))))
                             ((null? (cdr prec-siblings))  ; that stuff is over
                              (values (cons (reverse new-res) pos-res)
                                      src
                                      vacant-num))
                             (else
                              (loop (cdr prec-siblings)
                                    (reverse-desc (cadr prec-siblings))
                                    src
                                    (- vacant-num 1)
                                    new-res
                                    pos-res)))))
                        ((eq? (car descs) (sxml:context->node (car src)))
                         ; His siblings are on the way
                         (call-with-values
                          (lambda ()
                            (process-single
                             (car src) (cdr src) vacant-num))
                          (lambda (new-pos-res new-src new-vacant)
                            (loop prec-siblings
                                (cdr descs)  ; descendants processed
                                new-src
                                new-vacant
                                res
                                (append pos-res new-pos-res)))))
                        (else
                         (loop prec-siblings                               
                               (cdr descs)
                               src
                               vacant-num
                               res
                               pos-res))))))))
               (else
                (values '() src vacant-num)))))))
      (lambda (node)   ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((src (reverse node))
                    (pos-res '())
                    (vacant-num -1))
           (if
            (null? src)
            (filter  ; removing empty result nodesets
             (lambda (x) (not (null? x)))
             pos-res)
            (call-with-values
             (lambda () (process-single (car src) (cdr src) vacant-num))
             (lambda (new-pos-res new-src new-vacant)
               (iter new-src
                     (append new-pos-res pos-res)
                     new-vacant))))))))))

;-------------------------------------------------
; Particular case: all nodes in the input node-set are on the same level of
; hierarchy within a document
; In this case, some axes can be implemented more effectively

; Following axis for the special case
(define (ddo:following-single-level-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (follow (draft:following test-pred? num-anc)))
    (letrec
        (; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  pos-result
         (process-single
          (lambda (curr src vacant-num)
            (cond
              ((null? src)  ; no more nodes
               (let add-labels ((to-scan (follow curr))
                                (res '())
                                (vacant-num vacant-num))
                 (if (null? to-scan)
                     (list (reverse res))
                     (add-labels (cdr to-scan)
                                 (cons (cons (car to-scan) vacant-num) res)
                                 (+ vacant-num 1)))))
              ((not (sxml:context? curr))  ; can't find following nodes
               (cons '() (process-single (car src) (cdr src) vacant-num)))
              (else
               (let ((next (sxml:context->node (car src))))
                 (let loop ((this-level (sxml:context->node-u curr))
                            (ancs-to-view (sxml:context->ancestors-u curr))
                            (content-set '())
                            (pos-nset '())
                            (vacant-num vacant-num))
                   (cond
                     ((null? content-set)  ; no one to scan at this level
                      (cond
                        ((null? ancs-to-view)
                         (cons
                          (reverse pos-nset)
                          (process-single (car src) (cdr src) vacant-num)))
                        ((memq next (sxml:attr-list (car ancs-to-view)))
                         ; next is an attribute
                         (let ((pos-result
                                (process-single (car src) (cdr src) vacant-num)))
                           (cons
                            (append (reverse pos-nset) (car pos-result))
                            pos-result)))
                        (else  ; go to the next level                        
                         (loop
                          (car ancs-to-view)
                          (cdr ancs-to-view)                                    
                          (map
                           (lambda (n) (cons n (cdr ancs-to-view)))
                           (cond
                             ((memq this-level
                                    (cdr  ; parent is an element => children
                                     (car ancs-to-view)))
                              => cdr)
                             (else  ; curr-node is an attribute node
                              ((select-kids sxml:node?) (car ancs-to-view)))))
                          pos-nset
                          vacant-num))))
                     ((memq next (sxml:attr-list (caar ancs-to-view)))
                      ; next node is an attribute of currently scanned node
                      (let ((pos-result
                             (process-single (car src) (cdr src) vacant-num)))
                        (cons
                         (append (reverse pos-nset) (car pos-result))
                         pos-result)))                     
                     ((eq? (caar content-set) next)
                      ; current node is eq to the next one in src
                      (let add-desc ((to-scan
                                      (descend
                                       (draft:smart-make-context
                                        (caar content-set)
                                        (cdar content-set)
                                        num-anc)))
                                     (pos-nset pos-nset)
                                     (vacant-num vacant-num))
                        (if
                         (null? to-scan)
                         (let ((pos-result
                                (process-single
                                 (car src) (cdr src) vacant-num)))
                           (cons
                            (append (reverse pos-nset) (car pos-result))
                            pos-result))
                         (add-desc (cdr to-scan)
                                   (cons (cons (car to-scan) vacant-num)
                                         pos-nset)                                   
                                   (+ vacant-num 1)))))
                     (else  ; go on to scan the next node
                      (loop
                       this-level
                       ancs-to-view
                       (append
                        (map
                         (lambda (n) (cons n (car content-set)))
                         ((sxml:child sxml:node?) (caar content-set)))
                        (cdr content-set))
                       (if
                        (test-pred? (caar content-set))
                        (cons (cons (draft:smart-make-context
                                     (caar content-set) (cdar content-set)
                                     num-anc)
                                    vacant-num)
                              pos-nset)
                        pos-nset)                           
                       (+ vacant-num 1)))))))))))
      (lambda (node)  ; node or nodeset
        (let ((nset (as-nodeset node)))
          (if (null? nset)  ; nothing to be done
              nset
              (filter  ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) 1))))))))

; Following-sibling axis for the special case
(define (ddo:following-sibling-single-level-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        (; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  pos-result
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
             ; Siblings cannot be identified
             (if (null? src)  ; recursion is over
                 '()
                 (process-single (car src) (cdr src) vacant-num))             
             (cond
               ((memq (sxml:context->node-u curr)
                      (cdr  ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u curr))))
                => (lambda (foll-siblings)
                     (let ((ancestors
                            (draft:list-head
                             (sxml:context->ancestors-u curr) num-anc)))
                       (if
                        (null? src)  ; no more nodes
                        (let no-more ((foll-siblings (cdr foll-siblings))
                                      (res '())
                                      (vacant-num vacant-num))
                          (if
                           (null? foll-siblings)  ; everyone processed
                           (list (reverse res))
                           (no-more
                            (cdr foll-siblings)
                            (if (test-pred? (car foll-siblings))
                                (cons
                                 (cons
                                  (if (null? ancestors)
                                      (car foll-siblings)
                                      (draft:make-context
                                       (car foll-siblings) ancestors))
                                  vacant-num)
                                 res)
                                res)
                            (+ vacant-num 1))))
                        ; else there are more nodes in src
                        (let ((next (sxml:context->node (car src))))
                          (let more ((foll-siblings (cdr foll-siblings))
                                     (res '())
                                     (vacant-num vacant-num))
                            (if
                             (null? foll-siblings)  ; everyone processed
                             (cons
                              (reverse res)
                              (process-single (car src) (cdr src) vacant-num))
                             (let ((new-res
                                    (if (test-pred? (car foll-siblings))
                                        (cons
                                         (cons
                                          (if (null? ancestors)
                                              (car foll-siblings)
                                              (draft:make-context
                                               (car foll-siblings) ancestors))
                                          vacant-num)
                                         res)
                                        res)))
                               (if
                                (eq? (car foll-siblings) next)  ; recursion
                                (let ((pos-res
                                       (process-single
                                        (car src)
                                        (cdr src)
                                        (+ vacant-num 1))))
                                  (if
                                   (null? pos-res)  ; this shouldn't occur
                                   (list (reverse new-res))
                                   (cons (append
                                          (reverse new-res) (car pos-res))
                                         pos-res)))
                                (more (cdr foll-siblings)
                                      new-res
                                      (+ vacant-num 1)))))))))))
               (else  ; no following siblings
                (if (null? src)  ; recursion is over
                    '()
                    (process-single (car src) (cdr src) vacant-num))))))))
      (lambda (node)  ; node or nodeset
        (let ((nset (as-nodeset node)))
          (if (null? nset)  ; nothing to be done
              nset
              (filter  ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) 1))))))))

; Parent axis, for position-based filtering
; This function has very much the same with ddo:parent-pos. In future, we
; should use a meta-function for paremetrization of these two
(define (ddo:parent-single-level-pos test-pred? . num-ancestors)
  (let ((parent
         (apply ddo:parent-single-level (cons test-pred? num-ancestors))))
    (letrec
        ((add-order-num
          (lambda (num nset)
            (if (null? nset)
                nset
                (cons (list (cons (car nset) num))
                      (add-order-num (+ num 1) (cdr nset)))))))
      (lambda (node)   ; node or nodeset
        (add-order-num 1 (parent node))))))

; Preceding axis for the special case
(define (ddo:preceding-single-level-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (precede (draft:preceding test-pred? num-anc)))
    (letrec
        (; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  pos-result
         (process-single
          (lambda (curr src vacant-num)
            (cond
              ((null? src)  ; no more nodes
               (let add-labels ((to-scan (precede curr))
                                (res '())
                                (vacant-num vacant-num))
                 (if (null? to-scan)
                     (list (reverse res))
                     (add-labels (cdr to-scan)
                                 (cons (cons (car to-scan) vacant-num) res)
                                 (- vacant-num 1)))))
              ((not (sxml:context? curr))  ; can't find following nodes
               (cons '() (process-single (car src) (cdr src) vacant-num)))
              (else
               (let ((next (sxml:context->node (car src))))
                 (let loop ((this-level (sxml:context->node-u curr))
                            (ancs-to-view (sxml:context->ancestors-u curr))
                            (content-set '())
                            (pos-nset '())
                            (vacant-num vacant-num))
                   (cond
                     ((null? content-set)  ; no one to scan at this level
                      (if
                       (null? ancs-to-view)
                       (cons
                        (reverse pos-nset)
                        (process-single (car src) (cdr src) vacant-num))
                       (loop
                        (car ancs-to-view)
                        (cdr ancs-to-view)                          
                        (reverse
                         (map
                          sxml:context->content
                          (descend
                           (map
                            (lambda (n)
                              (draft:smart-make-context
                               n (cdr ancs-to-view) num-anc))
                            (cond
                              ((memq this-level
                                     (reverse
                                      ((select-kids sxml:node?)
                                       (car ancs-to-view))))                             
                               => (lambda (nset) (reverse (cdr nset))))
                              (else  ; curr-node is an attribute node
                               '()))))))
                          pos-nset
                          vacant-num)))                     
                     ((eq? (caar content-set) next)
                      ; current node is eq to the next one in src
                      (let ((pos-result
                             (process-single
                              (car src)
                              (cdr src)
                              (- vacant-num 1))))
                        (cons
                         (append
                          (reverse
                           (if
                            (test-pred? (caar content-set))
                            (cons (cons (draft:smart-make-context
                                         (caar content-set) (cdar content-set)
                                         num-anc)
                                        vacant-num)
                                  pos-nset)
                            pos-nset))
                          (car pos-result))
                         pos-result)))
                     (else  ; go on to scan the next node
                      (loop
                       this-level
                       ancs-to-view                       
                       (cdr content-set)
                       (if
                        (test-pred? (caar content-set))
                        (cons (cons (draft:smart-make-context
                                     (caar content-set) (cdar content-set)
                                     num-anc)
                                    vacant-num)
                              pos-nset)
                        pos-nset)
                       (- vacant-num 1)))))))))))
      (lambda (node)  ; node or nodeset
        (let ((nset (reverse (as-nodeset node))))
          (if (null? nset)  ; nothing to be done
              nset
              (filter  ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) -1))))))))

; Preceding-sibling axis for the special case
(define (ddo:preceding-sibling-single-level-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        (; curr - current context to be processed
         ; src - remaining source contexts
         ; vacant-num - order number for the result node
         ; Returns:  pos-result
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
             ; Siblings cannot be identified
             (if (null? src)  ; recursion is over
                 '()
                 (process-single (car src) (cdr src) vacant-num))             
             (cond
               ((memq (sxml:context->node-u curr)
                      (reverse
                       (cdr  ; parent is an element => cdr gives its children
                        (car (sxml:context->ancestors-u curr)))))
                => (lambda (prec-siblings)
                     (let ((ancestors
                            (draft:list-head
                             (sxml:context->ancestors-u curr) num-anc)))
                       (if
                        (null? src)  ; no more nodes
                        (let no-more ((prec-siblings (cdr prec-siblings))
                                      (res '())
                                      (vacant-num vacant-num))
                          (if
                           (null? prec-siblings)  ; everyone processed
                           (list (reverse res))
                           (no-more
                            (cdr prec-siblings)
                            (if (test-pred? (car prec-siblings))
                                (cons
                                 (cons
                                  (if (null? ancestors)
                                      (car prec-siblings)
                                      (draft:make-context
                                       (car prec-siblings) ancestors))
                                  vacant-num)
                                 res)
                                res)
                            (- vacant-num 1))))
                        ; else there are more nodes in src
                        (let ((next (sxml:context->node (car src))))
                          (let more ((prec-siblings (cdr prec-siblings))
                                     (res '())
                                     (vacant-num vacant-num))
                            (if
                             (null? prec-siblings)  ; everyone processed
                             (cons
                              (reverse res)
                              (process-single (car src) (cdr src) vacant-num))
                             (let ((new-res
                                    (if (test-pred? (car prec-siblings))
                                        (cons
                                         (cons
                                          (if (null? ancestors)
                                              (car prec-siblings)
                                              (draft:make-context
                                               (car prec-siblings) ancestors))
                                          vacant-num)
                                         res)
                                        res)))
                               (if
                                (eq? (car prec-siblings) next)  ; recursion
                                (let ((pos-res
                                       (process-single
                                        (car src)
                                        (cdr src)
                                        (- vacant-num 1))))
                                  (if
                                   (null? pos-res)  ; this shouldn't occur
                                   (list (reverse new-res))
                                   (cons (append
                                          (reverse new-res) (car pos-res))
                                         pos-res)))
                                (more (cdr prec-siblings)
                                      new-res
                                      (- vacant-num 1)))))))))))
               (else  ; no preceding siblings
                (if (null? src)  ; recursion is over
                    '()
                    (process-single (car src) (cdr src) vacant-num))))))))
      (lambda (node)  ; node or nodeset
        (let ((nset (reverse (as-nodeset node))))
          (if (null? nset)  ; nothing to be done
              nset
              (reverse
               (filter  ; removing empty result nodesets
                (lambda (x) (not (null? x)))
                (process-single (car nset) (cdr nset) -1)))))))))

(provide (all-defined)))
