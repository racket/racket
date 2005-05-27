(module honu-typecheck-postchecks mzscheme
  
  (require "../../ast.ss")
  (require "honu-type-utils.ss")
  (require "../../read-error-with-stx.ss")
  
  (provide check-type-elaborations)
  (define (check-type-elaborations ast)
    (cond
      [(honu-program? ast)
       (for-each check-type-elaborations (honu-program-defns ast))]
      
      [(honu-type-defn? ast) (void)]
      [(honu-class? ast)
       (for-each check-type-elaborations (honu-class-defns ast))]
      [(honu-mixin? ast)
       (for-each check-type-elaborations (honu-mixin-defns-before ast))
       (check-type-elaborations (honu-mixin-super-new ast))
       (for-each check-type-elaborations (honu-mixin-defns-after ast))]
      [(honu-subclass? ast) (void)]
   
      [(honu-init-field? ast)
       (if (honu-init-field-value ast)
           (check-type-elaborations (honu-init-field-value ast)))]
      [(honu-field? ast)
       (check-type-elaborations (honu-field-value ast))]
      [(honu-method? ast)
       (check-type-elaborations (honu-method-body ast))]
      
      [(honu-super-new? ast)
       (for-each check-type-elaborations (honu-super-new-arg-vals ast))]

      [(honu-null? ast) (void)]
      [(honu-int? ast) (void)]
      [(honu-float? ast) (void)]
      [(honu-bool? ast) (void)]
      [(honu-char? ast) (void)]
      [(honu-str? ast) (void)]
      [(honu-prim? ast)
       (check-type-elaborations (honu-prim-left ast))
       (check-type-elaborations (honu-prim-right ast))]
      [(honu-facc? ast)
       (if (not (eqv? (honu-facc-obj ast) 'my))
           (begin
             (check-type-elaborations (honu-facc-obj ast))
             (if (not (honu-iface-type? (honu-facc-elab ast)))
                 (raise-read-error-with-stx
                  "Found a non-static field access without a type elaboration."
                  (honu-ast-src-stx ast)))))]
      [(honu-fassn? ast)
       (if (not (eqv? (honu-fassn-obj ast) 'my))
           (begin
             (check-type-elaborations (honu-fassn-obj ast))
             (if (not (honu-iface-type? (honu-fassn-elab ast)))
                 (raise-read-error-with-stx
                  "Found a non-static field assignment without a type elaboration."
                  (honu-ast-src-stx ast)))))
       (check-type-elaborations (honu-fassn-rhs ast))]
      [(honu-mcall? ast)
       (if (not (eqv? (honu-mcall-obj ast) 'my))
           (begin
             (check-type-elaborations (honu-mcall-obj ast))
             (if (not (honu-iface-type? (honu-mcall-elab ast)))
                 (raise-read-error-with-stx
                  "Found non-static method call without a type elaboration."
                  (honu-ast-src-stx ast)))))
       (for-each check-type-elaborations (honu-mcall-args ast))]
      [(honu-var? ast) (void)]
      [(honu-assn? ast)
       (check-type-elaborations (honu-assn-rhs ast))]
      [(honu-call? ast)
       (for-each check-type-elaborations (honu-call-args ast))]
      [(honu-this? ast) (void)]
      [(honu-cast? ast)
       (check-type-elaborations (honu-cast-obj ast))]
      [(honu-isa? ast)
       (check-type-elaborations (honu-isa-obj ast))]
      [(honu-if? ast)
       (check-type-elaborations (honu-if-cond  ast))
       (check-type-elaborations (honu-if-true  ast))
       (check-type-elaborations (honu-if-false ast))]
      [(honu-new? ast)
       (for-each check-type-elaborations (honu-new-arg-vals ast))]
      [(honu-block? ast)
       (for-each check-type-elaborations (honu-block-binds ast))
       (for-each check-type-elaborations (honu-block-exps ast))]
      [(honu-return? ast)
       (check-type-elaborations (honu-return-body ast))]
      
      [(honu-binding? ast)
       (check-type-elaborations (honu-binding-rhs ast))]))

      )
