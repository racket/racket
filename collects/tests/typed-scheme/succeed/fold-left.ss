#lang typed-scheme

(: fold-left (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-left f c as . bss)
  (if (or (null? a)
          (ormap null? bss))
      c
      (apply fold-left f 
             (apply f c (car as) (map car bss))
             (cdr as) (map cdr bs))))

(: fold-right (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-right f c as . bss)
  (if (or (null? a)
          (ormap null? bss))
      c
      (apply f
             (apply fold-left f c (cdr as) (map cdr bs))
             (car as) (map car bss))))