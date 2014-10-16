#lang typed/racket

;; Test racket/format. Should be a unit test, but lifts don't
;; work well with TR's top-level currently.

(require racket/format)

(~a "foo" 'a 3)
(~a "foo" 'a 3 #:separator ", " #:width 20)
(~a "foo" 'a 3 #:max-width 20 #:min-width 10)
(~s "foo" 'a 3)
(~s "foo" 'a 3 #:separator ", " #:width 20)
(~s "foo" 'a 3 #:max-width 20 #:min-width 10)
(~v "foo" 'a 3)
(~v "foo" 'a 3 #:separator ", " #:width 20)
(~v "foo" 'a 3 #:max-width 20 #:min-width 10)
(~.a "foo" 'a 3)
(~.a "foo" 'a 3 #:separator ", " #:width 20)
(~.a "foo" 'a 3 #:max-width 20 #:min-width 10)
(~.s "foo" 'a 3)
(~.s "foo" 'a 3 #:separator ", " #:width 20)
(~.s "foo" 'a 3 #:max-width 20 #:min-width 10)
(~.v "foo" 'a 3)
(~.v "foo" 'a 3 #:separator ", " #:width 20)
(~.v "foo" 'a 3 #:max-width 20 #:min-width 10)
(~r 234234)
(~r 3.5 #:sign '+ #:base 3 #:precision 3
    #:notation 'positional #:format-exponent "fooo"
    #:min-width 10 #:pad-string ",")
(~r 3.5 #:sign (list "x" "y" (list "z" "z"))
    #:notation (lambda (_) 'positional))
