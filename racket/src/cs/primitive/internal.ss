
;; Exports that are not exposed to Racket, but
;; can be used in a linklet:

(define-primitive-table internal-table
  [extract-procedure (known-constant)]
  [set-ctl-c-handler! (known-constant)]
  [register-linklet-instantiate-continuation! (known-constant)]
  [impersonator-val (known-constant)]
  [impersonate-ref (known-constant)]
  [impersonate-set! (known-constant)]
  [struct-type-install-properties! (known-constant)]
  [structure-type-lookup-prefab-uid (known-constant)]
  [struct-type-constructor-add-guards (known-constant)]
  [register-struct-constructor! (known-constant)]
  [register-struct-predicate! (known-constant)]
  [register-struct-field-accessor! (known-constant)]
  [register-struct-field-mutator! (known-constant)]
  [struct-property-set! (known-constant)]
  [|#%call-with-values| (known-constant)]
  [unbox/check-undefined (known-constant)]
  [set-box!/check-undefined (known-constant)]

  [make-record-type-descriptor (known-constant)]
  [make-record-constructor-descriptor (known-constant)]
  [record-constructor (known-constant)]
  [record-predicate (known-constant)]
  [record-accessor (known-constant)]
  [record-mutator (known-constant)]
  [unsafe-struct? (known-constant)]

  [call-with-module-prompt (known-procedure 2)]

  [fork-place (known-procedure 1)]
  [start-place (known-procedure 32)]
  [make-pthread-parameter (known-procedure 2)]
  [break-enabled-key (known-constant)]

  [force-unfasl (known-procedure 2)])
