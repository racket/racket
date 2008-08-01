#!r6rs

;; from Derick Eddington

(library (tests r6rs contrib helper1)
  (export x s)
  (import (rnrs))
  
  (define-syntax x (lambda (_) #f))
  
  (define-syntax s
    (syntax-rules (x)  ;; This x refers only to the one in scope above.
      [(_ x)  ;; This pattern matches only if the 2nd subform is an
              ;; identifier that is free-identifier=? to the x in the literals list.
       'ok])))
