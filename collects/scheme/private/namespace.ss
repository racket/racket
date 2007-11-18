(module namespace "pre-base.ss"
  (require (for-syntax '#%kernel "define.ss"
                       "stx.ss" "stxcase-scheme.ss" "small-scheme.ss" 
                       "stxloc.ss"))

  (provide make-base-empty-namespace
           make-base-namespace
           define-namespace-anchor
           namespace-anchor?
           namespace-anchor->empty-namespace
           namespace-anchor->namespace)
  
  ;; ----------------------------------------

  (define orig-varref (#%variable-reference orig-varref))

  (define (make-base-empty-namespace)
    (let ([ns (make-empty-namespace)])
      (namespace-attach-module (variable-reference->empty-namespace orig-varref)
                               'scheme/base 
                               ns)
      ns))

  (define (make-base-namespace)
    (let ([ns (make-base-empty-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-require 'scheme/base))
      ns))

  ;; ----------------------------------------
  
  (define-syntax (define-namespace-anchor stx)
    (unless (memq (syntax-local-context) '(top-level module))
      (raise-syntax-error #f
                          "allowed only in a top-level or module context"
                          stx))
    (syntax-case stx ()
      [(_ id)
       (let ([id-stx #'id])
         (unless (identifier? id-stx)
           (raise-syntax-error #f
                               "expected an identifier"
                               stx
                               id-stx))
         (syntax/loc stx
           (define id (make-namespace-anchor (#%variable-reference id)))))]))

  (define-struct namespace-anchor (var))
  
  (define (namespace-anchor->empty-namespace ra)
    (unless (namespace-anchor? ra)
      (raise-type-error 'anchor->empty-namespace
                        "namespace anchor"
                        ra))
    (variable-reference->empty-namespace (namespace-anchor-var ra)))

  (define (namespace-anchor->namespace ra)
    (unless (namespace-anchor? ra)
      (raise-type-error 'anchor->namespace
                        "namespace anchor"
                        ra))
    (let ([mp (variable-reference->resolved-module-path
               (namespace-anchor-var ra))])
      (if mp
          (let ([ns (namespace-anchor->empty-namespace ra)])
            (parameterize ([current-namespace ns])
              (module->namespace (let ([name (resolved-module-path-name mp)])
                                   (if (path? name)
                                     name
                                     (list 'quote name))))))
          (variable-reference->top-level-namespace
           (namespace-anchor-var ra))))))
