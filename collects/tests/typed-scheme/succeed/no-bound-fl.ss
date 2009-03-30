#lang typed-scheme

(: fold-left (All (a b ...) ((a b ... -> a) a (Listof b) ... -> a)))
(define (fold-left f a . bss)
  (if (ormap null? bss)
      a
      (apply fold-left
             f
             (apply f a (map car bss))
             (map cdr bss))))

