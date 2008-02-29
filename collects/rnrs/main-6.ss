#lang scheme/base

(define-syntax re-export
  (syntax-rules ()
    [(_) (re-export rnrs/base-6
                    rnrs/exceptions-6
                    rnrs/programs-6
                    rnrs/files-6
                    rnrs/bytevectors-6
                    rnrs/hashtables-6
                    rnrs/sorting-6
                    rnrs/syntax-case-6
                    rnrs/conditions-6
                    rnrs/unicode-6
                    rnrs/control-6
                    rnrs/lists-6
                    rnrs/enums-6
                    rnrs/arithmetic/bitwise-6
                    rnrs/arithmetic/fixnums-6
                    rnrs/arithmetic/flonums-6
                    rnrs/io/ports-6
                    rnrs/io/simple-6
                    rnrs/records/inspection-6
                    rnrs/records/syntactic-6
                    rnrs/records/procedural-6)]
    [(_ id) (begin
              (require id)
              (provide (all-from-out id)))]
    [(_ id ...)
     (begin (re-export id) ...)]))

(re-export)

