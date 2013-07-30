#;
(exn-pred #rx"cannot be applied at a different type")
#lang typed/racket

;; Polymorphic recursion should fail through mutual recursion

(define-type (Foo A) (Listof (Bar (Listof A))))
(define-type (Bar B) (Listof (Foo (Listof B))))

(define-type (Foo2 A) (Listof (Bar2 (Listof A))))
(define-type (Bar2 B) (Listof (Foo2 B)))

