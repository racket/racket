#lang typed/racket

(: p Place)
(: p2 Place)
(: p3 Place)

(define p (dynamic-place 'tests/typed-racket/succeed/places-helper 'double-place))
(place-channel-put/get p 10)
(place-wait p)


(define p2 (dynamic-place 'tests/typed-racket/succeed/places-helper 'double-place))
(place-channel-put/get p2 -2+4i)
(place-wait p2)


(define p3 (dynamic-place 'tests/typed-racket/succeed/places-helper 'echo-place))
(place-channel-put/get p3 'echo-this)
(place-wait p3)
