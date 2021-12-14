#lang racket/base

;; A built-in symbol is one that the compiler must avoid using for a
;; binding. Built-in symbols include the names of run-time primitives
;; and identifiers reserved by the compiler itself (see
;; "reserved-symbol.rkt")

(provide register-built-in-symbol!
         built-in-symbol?
         make-built-in-symbol!)

(define built-in-symbols (make-hasheq))

(define (register-built-in-symbol! s)
  (hash-set! built-in-symbols s #t))

(define (built-in-symbol? s)
  (hash-ref built-in-symbols s #f))

(define (make-built-in-symbol! s)
  ;; Make a symbol that is a little more obscure than just `s`
  (define built-in-s (string->symbol (format ".~s" s)))
  (register-built-in-symbol! built-in-s)
  built-in-s)

;; ----------------------------------------

(void
 (begin
   ;; Primitive expression forms
   (for-each register-built-in-symbol!
             '(lambda case-lambda
                if begin begin0
                let-values letrec-values
                set! quote
                with-continuation-mark
                #%variable-reference))

   ;; Source-mode linklet glue
   (for-each register-built-in-symbol!
             '(check-not-undefined 
               instance-variable-box
               variable-reference
               variable-reference?
               variable-reference->instance
               variable-reference-constant?
               variable-reference-from-unsafe?))

   ;; Linklet compilation on Chez Scheme; anything
   ;; introduced by schemify needs to be here to
   ;; make sure the introduced name isn't shadowed
   ;; by a definition in expanded code
   (for-each register-built-in-symbol!
             '(or
               and
               let
               letrec*
               define
               $value
               with-continuation-mark*
               pariah
               begin-unsafe
               variable-set!
               variable-ref
               variable-ref/no-check
               variable-set!/check-undefined
               variable-set!/define
               make-instance-variable-reference
               instance-variable-reference
               unbox/check-undefined
               set-box!/check-undefined
               annotation?
               annotation-expression
               #%app
               #%call-with-values
               #%app/no-return
               #%app/value
               call-with-module-prompt
               make-pthread-parameter
               engine-block
               make-record-type-descriptor
               make-record-type-descriptor*
               make-record-constructor-descriptor
               record-constructor
               record-accessor
               record-mutator
               record-predicate
               make-struct-type-install-properties
               #%struct-constructor
               #%struct-predicate
               #%struct-field-accessor
               #%struct-field-mutator
               #%nongenerative-uid
               #%struct-ref-error
               #%struct-set!-error
               unsafe-struct?
               unsafe-sealed-struct?
               unsafe-struct
               raise-binding-result-arity-error
               raise-definition-result-arity-error
               structure-type-lookup-prefab-uid
               struct-type-constructor-add-guards
               impersonator-val
               impersonator-ref
               impersonate-set!
               ptr-ref/int8 ptr-set!/int8
               ptr-ref/uint8 ptr-set!/uint8
               ptr-ref/int16 ptr-set!/int16
               ptr-ref/uint16 ptr-set!/uint16
               ptr-ref/int32 ptr-set!/int32
               ptr-ref/uint32 ptr-set!/uint32
               ptr-ref/int64 ptr-set!/int64
               ptr-ref/uint64 ptr-set!/uint64
               ptr-ref/double ptr-set!/double
               ptr-ref/float ptr-set!/float))))
