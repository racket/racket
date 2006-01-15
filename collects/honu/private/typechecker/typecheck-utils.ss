(module typecheck-utils mzscheme

  (require (lib "contract.ss")
           "../../ast.ss"
           "../../utils.ss"
           "../../readerr.ss"
           "type-utils.ss"
           )

  (provide/contract
   [check-valid-type! (string? honu:type? . -> . void?)]
   )

  ;; check-valid-type! : Name Type -> Void
  ;; Raises an error if named type is not valid.
  (define (check-valid-type! name type)
    (if (not (type-valid? type))
        (raise-read-error-with-stx
         (format "~s is undefined" name)
         (honu:ast-stx type))))

  )
