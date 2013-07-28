#lang typed-scheme
(define-typed-struct rectangle ([width : Number] [height : Number]))
(define-typed-struct circle ([radius : Number]))

(define-type-alias shape (U rectangle circle))

(define: (area [sh : shape]) : Number
  (cond [(circle? sh)
         (* (ann 3.1416 : Number) (circle-radius sh) (circle-radius sh))]
        [else
         (* (rectangle-width sh) (rectangle-height sh))]))
