#lang racket/base

;; Provides generic interfaces that correspond to struct properties
;; that live in racket/base

(require racket/private/generics)

(provide gen:equal+hash)

(define-values (prop:gen:equal+hash equal+hash? gen:equal+hash-acc)
  (make-struct-type-property
   'prop:gen:equal+hash
   (lambda (v si)
     (unless (and (vector? v)
                  (= 3 (vector-length v))
                  (procedure? (vector-ref v 0))
                  (procedure-arity-includes? (vector-ref v 0) 3)
                  (procedure? (vector-ref v 1))
                  (procedure-arity-includes? (vector-ref v 1) 2)
                  (procedure? (vector-ref v 2))
                  (procedure-arity-includes? (vector-ref v 2) 2))
       (raise-type-error 'guard-for-prop:gen:equal+hash
                         "vector of three procedures (arities 3, 2, 2)"
                         v))
     v)
   (list (cons prop:equal+hash vector->list))))

(define-generics (equal+hash gen:equal+hash prop:gen:equal+hash equal+hash?
                             #:defined-table dummy
                             #:prop-defined-already? gen:equal+hash-acc)
  (equal-proc equal+hash rhs equal?/recur)
  (hash-proc  equal+hash equal-hash-code/recur)
  (hash2-proc equal+hash equal-secondary-hash-code/recur))
