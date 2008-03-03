(module internal-forms mzscheme
  
  (define-syntax internal-forms
    (syntax-rules ()
      [(_ nms ...)
       (begin
         (provide nms ...)
         (define-syntax (nms stx) (raise-syntax-error 'typecheck "Internal typechecker form used out of context" stx)) ...)]))
  
  (internal-forms require/typed-internal define-type-alias-internal
                  define-typed-struct-internal
                  define-typed-struct/exec-internal
                  assert-predicate-internal
                  :-internal)
  )