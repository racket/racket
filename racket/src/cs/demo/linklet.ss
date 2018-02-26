(import (rumble)
        (linklet))

(define l1 (compile-linklet
            '(linklet 
              () ; imports
              (f x) ; exports
              (define-values (f) (lambda (y) (add1 y)))
              (define-values (x) 5)
              'done)
            'l1))

(define l2 (compile-linklet
            '(linklet 
              ((f x)) ; imports
              () ; exports
              (display (f x))
              (newline))))

(instantiate-linklet l2 (list (instantiate-linklet l1 '())))
