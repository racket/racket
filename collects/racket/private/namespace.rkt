(module namespace "pre-base.rkt"
  (require (for-syntax '#%kernel "define.rkt"
                       "member.rkt"
                       "stx.rkt" "stxcase-scheme.rkt" "small-scheme.rkt" 
                       "stxloc.rkt"))

  (provide make-base-empty-namespace
           make-base-namespace
           define-namespace-anchor
           namespace-anchor?
           namespace-anchor->empty-namespace
           namespace-anchor->namespace)
  
  ;; ----------------------------------------

  (define orig-varref (#%variable-reference orig-varref))

  (define (make-base-empty-namespace)
    (let* ([this-ns (variable-reference->empty-namespace orig-varref)]
           [ns (parameterize ([current-namespace this-ns]) ; ensures correct phase
                 (make-empty-namespace))])
      (namespace-attach-module this-ns
                               'racket/base 
                               ns)
      ns))

  (define (make-base-namespace)
    (let ([ns (make-base-empty-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-require 'racket/base))
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
           ;; two-step definition allows this to work in for-syntax contexts:
           (begin
             (define tmp #f)
             (define id (make-namespace-anchor (#%variable-reference tmp))))))]))

  (define-struct namespace-anchor (var))
  
  (define (namespace-anchor->empty-namespace ra)
    (unless (namespace-anchor? ra)
      (raise-argument-error 'anchor->empty-namespace
                            "namespace-anchor?"
                            ra))
    (variable-reference->empty-namespace (namespace-anchor-var ra)))

  (define (namespace-anchor->namespace ra)
    (unless (namespace-anchor? ra)
      (raise-argument-error 'anchor->namespace
                            "namespace-anchor?"
                            ra))
    (variable-reference->namespace (namespace-anchor-var ra))))
