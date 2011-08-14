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