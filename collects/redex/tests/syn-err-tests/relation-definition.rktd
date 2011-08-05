(#rx"expected the name of the relation"
 ([bad-def (define-relation syn-err-lang R)])
 bad-def)

(#rx"expected a sequence of patterns separated by"
 ([subset ⊂])
 (define-relation syn-err-lang R subset))

(#rx"expected clause definitions"
 ([bad-def (define-relation syn-err-lang foo ⊆ c)])
 bad-def)

(#rx"expected a pattern"
 ([cross ×])
 (define-relation syn-err-lang foo ⊆ c cross))