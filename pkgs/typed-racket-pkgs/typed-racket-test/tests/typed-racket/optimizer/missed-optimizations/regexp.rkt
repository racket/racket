#;#;
#<<END
TR info: regexp.rkt 2:14 "foo" -- non-regexp pattern
TR info: regexp.rkt 3:14 #"foo" -- non-regexp pattern
END
#<<END
'("foo")
'(#"foo")
'("foo")
'(#"foo")

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(regexp-match "foo" "foo")
(regexp-match #"foo" #"foo")
(regexp-match (regexp "foo") "foo") ; ok
(regexp-match (byte-regexp #"foo") #"foo") ; ok
