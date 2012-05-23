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
                             #:defined-table equal+hash-def-table
                             #:prop-defined-already? gen:equal+hash-acc)
  (equal-proc equal+hash rhs equal?/recur)
  (hash-proc  equal+hash equal-hash-code/recur)
  (hash2-proc equal+hash equal-secondary-hash-code/recur))


(provide gen:custom-write)

(define-values (prop:gen:custom-write gen:custom-write? gen:custom-write-acc)
  (make-struct-type-property
   'prop:gen:custom-write
   (lambda (v si)
     (unless (and (vector? v)
                  (= 1 (vector-length v))
                  (procedure? (vector-ref v 0))
                  (procedure-arity-includes? (vector-ref v 0) 3))
       (raise-type-error 'guard-for-prop:gen:custom-write
                         "vector of one procedure (arity 3)"
                         v))
     v)
   (list (cons prop:custom-write (lambda (v) (vector-ref v 0))))))

(define-generics (custom-write gen:custom-write prop:gen:custom-write
                               gen:custom-write?
                               #:defined-table custom-write-def-table
                               #:prop-defined-already? gen:custom-write-acc)
  (write-proc custom-write port mode))
