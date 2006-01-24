(module typecheck-utils mzscheme

  (require (lib "contract.ss")
           "../../ast.ss"
           "../tools/general.ss"
           "../../readerr.ss"
           "type-utils.ss"
           )

  (provide/contract
   [check-valid-type! (string? ast:type? . -> . void?)]
   [check-valid-types! (string? (listof ast:type?) . -> . void?)]
   )

  ;; check-valid-type! : Name Type -> Void
  ;; Raises an error if named type is not valid.
  (define (check-valid-type! name type)
    (if (not (type-valid? type))
        (raise-read-error-with-stx
         (format "~s is undefined" name)
         (ast-syntax type))))

  ;; check-valid-types! : Name [Listof Type] -> Void
  ;; Raises an error if any of the named types are not valid.
  (define (check-valid-types! name types)
    (for-each (curry check-valid-type! name) types))

  )
