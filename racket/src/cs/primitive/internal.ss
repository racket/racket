
;; Exports that are not exposed to Racket, but
;; can be used in a linklet. These need to be
;; listed in "built-in-symbol.rkt" in the expander,
;; too.

(define-primitive-table internal-table
  [impersonator-val (known-constant)]
  [impersonate-ref (known-constant)]
  [impersonate-set! (known-constant)]
  [struct-type-install-properties! (known-constant)]
  [structure-type-lookup-prefab-uid (known-constant)]
  [struct-type-constructor-add-guards (known-constant)]
  [|#%call-with-values| (known-constant)]
  [unbox/check-undefined (known-constant)]
  [set-box!/check-undefined (known-constant)]

  [|#%struct-constructor| (known-constant)]
  [|#%struct-predicate| (known-constant)]
  [|#%struct-field-accessor| (known-constant)]
  [|#%struct-field-mutator| (known-constant)]
  [|#%nongenerative-uid| (known-constant)]
  [make-record-type-descriptor (known-constant)]
  [make-record-type-descriptor* (known-constant)]
  [make-record-constructor-descriptor (known-constant)]
  [record-constructor (known-constant)]
  [record-predicate (known-constant)]
  [record-accessor (known-constant)]
  [record-mutator (known-constant)]
  [unsafe-struct? (known-constant)]
  [unsafe-struct (known-constant)]

  [call-with-module-prompt (known-procedure 2)]
  [raise-binding-result-arity-error (known-procedure 4)]
  [raise-definition-result-arity-error (known-procedure 4)]

  [make-pthread-parameter (known-procedure 2)]
  [engine-block (known-procedure 1)]

  [ptr-ref/int8 (known-procedure 8)]
  [ptr-ref/uint8 (known-procedure 8)]
  [ptr-ref/int16 (known-procedure 8)]
  [ptr-ref/uint16 (known-procedure 8)]
  [ptr-ref/int32 (known-procedure 8)]
  [ptr-ref/uint32 (known-procedure 8)]
  [ptr-ref/int64 (known-procedure 8)]
  [ptr-ref/uint64 (known-procedure 8)]
  [ptr-ref/double (known-procedure 8)]
  [ptr-ref/float (known-procedure 8)]

  [ptr-set!/int8 (known-procedure 16)]
  [ptr-set!/uint8 (known-procedure 16)]
  [ptr-set!/int16 (known-procedure 16)]
  [ptr-set!/uint16 (known-procedure 16)]
  [ptr-set!/int32 (known-procedure 16)]
  [ptr-set!/uint32 (known-procedure 16)]
  [ptr-set!/int64 (known-procedure 16)]
  [ptr-set!/uint64 (known-procedure 16)]
  [ptr-set!/double (known-procedure 16)]
  [ptr-set!/float (known-procedure 16)])
