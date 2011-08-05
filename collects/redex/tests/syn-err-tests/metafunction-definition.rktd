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