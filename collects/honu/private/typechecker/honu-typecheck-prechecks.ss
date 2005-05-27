(module honu-typecheck-prechecks mzscheme
  
  (require (lib "list.ss" "srfi" "1"))
  (require "../../ast.ss")
  (require "../../read-error-with-stx.ss")
  
  (provide check-uses-of-this)
  (define (check-uses-of-this ast)
    (cond
      [(honu-program? ast)
       (for-each check-uses-of-this (honu-program-defns ast))]
      
      [(honu-type-defn? ast) (void)]
      [(honu-class? ast)
       (for-each check-uses-of-this (honu-class-defns ast))]
      [(honu-mixin? ast)
       (for-each check-uses-of-this (honu-mixin-defns-before ast))
       (check-uses-of-this (honu-mixin-super-new ast))
       (for-each check-uses-of-this (honu-mixin-defns-after ast))]
      [(honu-subclass? ast) (void)]
   
      [(honu-init-field? ast)
       (if (honu-init-field-value ast)
           (check-uses-of-this (honu-init-field-value ast)))]
      [(honu-field? ast)
       (check-uses-of-this (honu-field-value ast))]
      [(honu-method? ast)
       (check-uses-of-this (honu-method-body ast))]
      
      [(honu-super-new? ast)
       (for-each check-uses-of-this (honu-super-new-arg-vals ast))]

      [(honu-null? ast) (void)]
      [(honu-int? ast) (void)]
      [(honu-float? ast) (void)]
      [(honu-bool? ast) (void)]
      [(honu-char? ast) (void)]
      [(honu-str? ast) (void)]
      [(honu-prim? ast)
       (check-uses-of-this (honu-prim-left ast))
       (check-uses-of-this (honu-prim-right ast))]
      [(honu-facc? ast)
       (if (and (not (eqv? (honu-facc-obj ast) 'my))
                (not (honu-this? (honu-facc-obj ast))))  ; this on the left side of a dot is okay
           (check-uses-of-this (honu-facc-obj ast)))]
      [(honu-fassn? ast)
       (if (and (not (eqv? (honu-fassn-obj ast) 'my))
                (not (honu-this? (honu-fassn-obj ast)))) ; this on the left side of a dot is okay
           (check-uses-of-this (honu-fassn-obj ast)))
       (check-uses-of-this (honu-fassn-rhs ast))]
      [(honu-mcall? ast)
       (if (and (not (eqv? (honu-mcall-obj ast) 'my))
                (not (honu-this? (honu-mcall-obj ast)))) ; this on the left side of a dot is okay
           (check-uses-of-this (honu-mcall-obj ast)))
       (for-each check-uses-of-this (honu-mcall-args ast))]
      [(honu-var? ast) (void)]
      [(honu-assn? ast)
       (check-uses-of-this (honu-assn-rhs ast))]
      [(honu-call? ast)
       (for-each check-uses-of-this (honu-call-args ast))]
      [(honu-this? ast)
       ;; we reached a case where this is not okay, so we throw an exception
       (raise-read-error-with-stx
        "Unprotected use of this in possible client context."
        (honu-ast-src-stx ast))]
      [(honu-cast? ast)
       (if (not (honu-this? (honu-cast-obj ast))) ; this in a cast is okay.
           (check-uses-of-this (honu-cast-obj ast)))]
      [(honu-isa? ast)
       (if (not (honu-this? (honu-isa-obj ast))) ; this in an isa is okay.
           (check-uses-of-this (honu-isa-obj ast)))]
      [(honu-if? ast)
       (check-uses-of-this (honu-if-cond  ast))
       (check-uses-of-this (honu-if-true  ast))
       (check-uses-of-this (honu-if-false ast))]
      [(honu-new? ast)
       (for-each check-uses-of-this (honu-new-arg-vals ast))]
      [(honu-block? ast)
       (for-each check-uses-of-this (honu-block-binds ast))
       (for-each check-uses-of-this (honu-block-exps ast))]
      [(honu-return? ast)
       (check-uses-of-this (honu-return-body ast))]
      
      [(honu-binding? ast)
       (check-uses-of-this (honu-binding-rhs ast))]))

      
  )
