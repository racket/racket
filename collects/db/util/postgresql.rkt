#lang racket/base
(require racket/contract
         "geometry.rkt")

#|
inet, cidr = family:byte bits:byte is_cidr:byte addrlen:byte addr:be-integer
  is_cidr is ignored

box = x1 y1 x2 y2 (all float8)
circle = x y rad (all float8)
line = not yet implemented
lseg = x1 y1 x2 y2 (all float8)
path = closed?:byte #points:int4 (x y : float8)*
point = x y (all float8)
polygon = #points:int4 (x y : float8)*
|#

(struct pg-box (ne sw)
        #:transparent
        #:guard (lambda (ne sw _n)
                  (let ([x1 (point-x ne)]
                        [x2 (point-x sw)]
                        [y1 (point-y ne)]
                        [y2 (point-y sw)])
                    (values (point (max x1 x2) (max y1 y2))
                            (point (min x1 x2) (min y1 y2))))))

(struct pg-circle (center radius)
        #:transparent
        #:guard (lambda (center radius _n)
                  (values center (exact->inexact radius))))

(struct pg-path (closed? points)
        #:transparent
        #:guard (lambda (closed? points _n)
                  (values (and closed? #t)
                          points)))

(provide/contract
 [struct pg-box ([ne point?] [sw point?])]
 [struct pg-circle ([center point?] [radius (and/c real? (not/c negative?))])]
 [struct pg-path ([closed? any/c] [points (listof point?)])])
