#lang racket/base

(provide (struct-out known-defined)
         (struct-out known-defined/delay)
         (struct-out known-property)
         (struct-out known-function)
         (struct-out known-function-of-satisfying)
         (struct-out known-predicate)
         (struct-out known-satisfies)
         (struct-out known-struct-op)
         lookup-defn)

;; Known locals and defined variables map to one of he following:

(struct known-defined () #:prefab)
;; all we know is that it's defined and can be referenced now

(struct known-defined/delay (thunk) #:prefab)
;; force the thunk and try again

(struct known-property () #:prefab)
;; defined as a struct property with no guard

(struct known-function (arity pure?) #:prefab)
;; function of known arity and maybe known pure, where
;; pure must return 1 value

(struct known-function-of-satisfying (arg-predicate-keys) #:prefab)
;; function that is known to be pure as long as its arguments
;; are known to satisfy certain predicates

(struct known-predicate (key) #:prefab)
;; a predicate that is pure and categorizes an argument

(struct known-satisfies (predicate-key) #:prefab)
;; a value that is known to satisfy a specific predicate

(struct known-struct-op (type field-count) #:prefab)
;; struct operation for a type with n fields
;;  where type is one of: 'struct-type, 'constructor
;;                        'predicate, 'accessor, 'mutator 
;;                        'general-accessor,
;;                        or 'general-mutator  (needs field index)
;; and the 'constructor mode can be used for things that
;; construct built-in datatypes; for 'general-accessor or
;; 'general-mutator, the field count doesn't include inherited

;; Supports `known-defined/delay`:
(define (lookup-defn defns sym)
  (define d (hash-ref defns sym #f))
  (cond
   [(known-defined/delay? d)
    ((known-defined/delay-thunk d))
    (lookup-defn defns sym)]
   [else d]))
