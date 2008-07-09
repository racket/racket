
(module browse-deriv mzscheme
  (require mzlib/class
           mzlib/plt-match
           mzlib/unit
           mred
           framework
           mrlib/hierlist)
  (require "../model/deriv.ss")
  (provide (all-defined))

  (define-signature browser^ (make-browser))
  
  (define-signature node^
    (;; type Node
     
     ;; node-children : Node -> (list-of Node)
     node-children
     
     ;; node-summary-string : Node -> string
     node-summary-string
     
     ;; node-display : Node text% -> void
     node-display
     ))

  (define deriv@
    (unit
      (import)
      (export node^)
      
      ;; Node = (union Derivation Transformation)

      ;; node-children
      (define (node-children node)
        (match node
          [(AnyQ mrule (e1 e2 tx next))
           (list tx next)]
          [(AnyQ transformation (e1 e2 rs me1 me2 locals))
           null]
          [(AnyQ p:define-syntaxes (e1 e2 rs rhs))
           (list rhs)]
          [(AnyQ p:define-values (e1 e2 rs rhs))
           (list rhs)]
          [(AnyQ p:if (e1 e2 rs full? test then else))
           (if full?
               (list test then else)
               (list test then))]
          [(AnyQ p:wcm (e1 e2 rs key value body))
           (list key value body)]
          [(AnyQ p:set! (e1 e2 rs id-rs rhs))
           (list rhs)]
          [(AnyQ p:set!-macro (e1 e2 rs deriv))
           (list deriv)]
          [(AnyQ p:begin (e1 e2 rs (AnyQ lderiv (es1 es2 derivs))))
           derivs]
          [(AnyQ p:begin0 (e1 e2 rs first (AnyQ lderiv (es1 es2 derivs))))
           (cons first derivs)]))

      
      ;; node-summary-string
      (define (node-summary-string node)
        (match node
          [($ pderiv e1 e2 prule)
           "PDeriv"]
          [($ mderiv e1 e2 mrule next)
           "MDeriv"]
          
          [($ mrule e1 e2 rs me1 me2 locals)
           "MRule"]
          [($ prule e1 e2 rs)
           "PRule"]))
      
      ;; node-display
      (define (node-display node text)
        '...)
      ))
  
  (define browser@
    (unit
      (import node^)
      (export browser^)
      
      (define callback-hierlist%
        (class hierarchical-list%
          (init-field callback)
          (define/override (on-select i)
            (callback i))
          (super-new)))
      
      (define browser%
        (class object%
          (init-field node)
          (super-new)

          (define frame (new frame% (label "Derivation browser") (min-width 400) (min-height 400)))
          (define hpanel (new panel:horizontal-dragable% (parent frame)))
          (define treeview (new callback-hierlist% (parent hpanel) 
                                (callback (lambda (i) (on-item-select i)))))
          (define details (new text%))
          (define details-view (new editor-canvas% (parent hpanel) (editor details)))

          (define current-node #f)
          (define node=>item (make-hash-table))
          (define item=>node (make-hash-table))

          (define/private (init-tree)
            (let loop ([node node] [parent treeview])
              (let ([children (node-children node)])
                (let ([item (if (pair? children)
                                (send parent new-list)
                                (send parent new-item))])
                  (hash-table-put! node=>item node item)
                  (hash-table-put! item=>node item node)
                  (send (send item get-editor)
                        insert (node-summary-string node))
                  (for-each (lambda (c) (loop c item)) children)
                  (when (pair? children) (send item open))))))
          
          (define/private (on-item-select item)
            (let ([node (hash-table-get item=>node item (lambda () #f))])
              (unless (eq? node current-node)
                (send details erase))
              (when node
                (node-display node details))))
          
          (init-tree)
          (send frame show #t)
          (void)))
      
      (define (make-browser node)
        (new browser% (node node)))))
  
  
  (define app@
    (compound-unit
      (import)
      (link [((NODE node^)) deriv@]
            [((BROWSER browser^)) browser@ NODE])
      (export BROWSER)))
  
  (define-values/invoke-unit app@ (export browser^))
  )
