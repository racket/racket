#lang typed/scheme

(define key (gensym))

(: eph-one (Ephemeronof Integer))
(define eph-one (make-ephemeron key 1))

(ephemeron? eph-one)

(ephemeron-value eph-one)

(: get-number ((Ephemeronof Number) -> Number))
(define (get-number e)
 (or (ephemeron-value e) 0))

(get-number eph-one)
