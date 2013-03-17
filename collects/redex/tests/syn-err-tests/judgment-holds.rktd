(#rx"expected a judgment form name"
 ([not-judgment-form junk])
 (judgment-holds (not-judgment-form z (s z))))

(#rx"a[?]: mode specifies a 1-ary relation but use supplied 2 terms"
 ([bad-judgment (a? a q)])
 ([name a?])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name I)
     [(name a)])
   (judgment-holds bad-judgment)))
