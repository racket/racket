#lang typed/scheme

(define: x : (Listof (Listof Integer))
  '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5)))

(ann (caar x)   Integer)
(ann (cdar x)   (Listof Integer))
(ann (cadr x)   (Listof Integer))
(ann (cddr x)   (Listof (Listof Integer)))
(ann (caadr x)  Integer)
(ann (cdadr x)  (Listof Integer))
(ann (cadar x)  Integer)
(ann (cddar x)  (Listof Integer))
(ann (caddr x)  (Listof Integer))
(ann (cdddr x)  (Listof (Listof Integer)))
(ann (caddar x) Integer)
(ann (cdddar x) (Listof Integer))
(ann (cadadr x) Integer)
(ann (cddadr x) (Listof Integer))
(ann (caaddr x) Integer)
(ann (cdaddr x) (Listof Integer))
(ann (cadddr x) (Listof Integer))
(ann (cddddr x) (Listof (Listof Integer)))


(define: y : (Listof (Listof (Listof Integer)))
  '(((11 11) (12 12) (13 13))
    ((21 21) (22 22) (23 23))
    ((31 31) (32 32) (33 33))))

(ann (caaar y)  Integer)
(ann (cdaar y)  (Listof Integer))
(ann (cadaar y) Integer)
(ann (cddaar y) (Listof Integer))
(ann (caadar y) Integer)
(ann (cdadar y) (Listof Integer))
(ann (caaadr y) Integer)
(ann (cdaadr y) (Listof Integer))


(define: z : (Listof (Listof (Listof (Listof Integer))))
  '((((111 111) (112 112))
     ((121 121) (122 122)))
    (((211 211) (212 212))
     ((221 221) (222 222)))))

(ann (caaaar z) Integer)
(ann (cdaaar z) (Listof Integer))
