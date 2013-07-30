#;
(exn-pred #rx"cannot be applied at a different type")
#lang typed/racket

;; Example from Wikipedia
;; http://en.wikipedia.org/w/index.php?title=Polymorphic_recursion&oldid=543854337

;; Note that the following code does not work, but some
;; of it could work if polymorphic recursion were implemented
;; in the future.
;;
;; Right now the type definition of Nested should throw a static error

(struct: (A B) :<: ([left : A] [right : B])
         #:transparent)
(struct: ε ())

;; Note that this type uses polymorphic recursion
(define-type (Nested A)
  (U (:<: A (Nested (Listof A))) ε))

(: nested (Nested Integer))
(define nested
  (:<: 1 (:<: (list 2 3 4) (:<: (list (list 4 5) (list 7) (list 8 9)) (ε)))))

;; inference loops forever for this one...
(: nested-length (All (A) ((Nested A) -> Integer)))
(define (nested-length n)
  (if (ε? n)
      0
      ;; explicit instantiation needed
      (add1 ((inst nested-length (Listof A)) (:<:-right n)))))

;; Test subtyping with polymorphic recursion
(: nested2 (Nested Number))
(define nested2 nested)

(provide nested nested-length :<: Nested ε)

