#lang typed-scheme

(: fold-left (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-left f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply (inst fold-left c a b ... b) f
             (apply f c (car as) (map car bss))
             (cdr as) (map cdr bss))))

(: fold-right (All (c a b ...) ((c a b ... b -> c) c (Listof a) (Listof b) ... b -> c)))
(define (fold-right f c as . bss)
  (if (or (null? as)
          (ormap null? bss))
      c
      (apply f
             (apply (inst fold-left c a b ... b) f c (cdr as) (map cdr bss))
             (car as) (map car bss))))
