#lang typed-scheme

(: fold-left (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-left f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply fold-left f
             (apply f c (car as) (map car bss))
             (cdr as) (map cdr bss))))

(: fold-right (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-right f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply f
             (apply fold-right f c (cdr as) (map cdr bss))
             (car as) (map car bss))))

;; Matthias -- tell me why this returns 4.
((plambda: (x ...) [xs : x ... x]
    (apply fold-left
           (lambda: ([a : Integer] [b : Integer] . [xs : x ... x])
             (+ a b))
           3
           (list 1 2 3)
           (map list xs)))
 3 4 5)

((plambda: (x ...) [xs : x ... x]
    (apply fold-right
           (lambda: ([a : Integer] [b : Integer] . [xs : x ... x])
             (+ a b))
           3
           (list 1 2 3)
           (map list xs)))
 3 4 5)

(fold-left  (lambda: ([a : (Listof Integer)] [c : Integer]) (cons c a)) null (list 3 4 5 6))
(fold-right (lambda: ([a : (Listof Integer)] [c : Integer]) (cons c a)) null (list 3 4 5 6))
