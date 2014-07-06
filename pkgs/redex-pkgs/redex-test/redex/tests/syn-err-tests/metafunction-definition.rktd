(#rx"define-metafunction:.*expression context"
 ([illegal-def (define-metafunction syn-err-lang [(f) ()])])
 (values illegal-def))
(#rx"define-metafunction/extension:.*expression context"
 ([illegal-def (define-metafunction/extension f syn-err-lang [(g) ()])])
 (let ()
   (define-metafunction syn-err-lang [(f) ()])
   (values illegal-def)))

(#rx"expected a pattern and a right-hand side"
 ([clause [(f x)]])
 (define-metafunction syn-err-lang
   clause))

(#rx"expected an identifier"
 ([not-id (junk)])
 (define-metafunction not-id also-junk))
(#rx"expected an identifier"
 ([not-id junk])
 (define-metafunction not-id also-junk))

(#rx"before underscore"
 ([not-non-term z!_1])
 (define-metafunction syn-err-lang
  [(func M_1 E_2)
   (M_1 (E_2 not-non-term) M_1)]))

(#rx"metafunction must be in an application"
 ([z2 z])
 (let ()
   (define-metafunction syn-err-lang
     [(i any) any])
   (define-metafunction syn-err-lang
     [(z any) (0)])
   (let ()
     (term (i z2 4)))))
