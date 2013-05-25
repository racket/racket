;;;
;;; COMPREHENSIONS
;;;

(module comprehension-struct mzscheme
  (provide comprehension comprehension? comprehension-loop make-comprehension
           comprehension-payload)
  
  (define-struct comprehension
    (name loop payload)))
