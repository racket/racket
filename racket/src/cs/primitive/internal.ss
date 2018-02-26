
;; Exports that are not exposed to Racket, but
;; can be used in a linklet:

(define-primitive-table internal-table
  [call/cm (known-constant)]
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

  [make-pthread-parameter (known-procedure 2)])
