#;#;
#<<END
TR info: pr14380.rkt 11:15 foo -- struct constructor
END
#<<END
#<procedure:unbox>
#<procedure:magnitude>
#<procedure:list-ref>
#<procedure:+>
#<procedure:car>
#<procedure:string-length>
#<procedure:foo-x>

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

;; Test for PR14380. None of these should get optimized.

(begin0 unbox (box 3))
(begin0 magnitude 3.0+5.0i)
(begin0 list-ref '(a b c) 0)
(begin0 + 3)
(begin0 car '(a b))
(begin0 string-length "foo")
(struct foo ([x : String]))
(begin0 foo-x (foo "foo"))
