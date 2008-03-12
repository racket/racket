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
              (require id
                       ;; Shift any run time exports to for-syntax:
                       (for-syntax (only-meta-in 0 id))
                       ;; Shift any for-syntax exports for run time:
                       (for-template (only-meta-in 1 id)))
              (provide (all-from-out id)
                       (for-template (all-from-out id))
                       (for-syntax (all-from-out id))))]
    [(_ id ...)
     (begin (re-export id) ...)]))

(re-export)

;; Also need to export prelims for syntax, since there will
;;  not be a for-syntax import when this module is imported:
(require (for-syntax r6rs/private/prelims))
(provide (for-syntax (all-from-out r6rs/private/prelims)))
