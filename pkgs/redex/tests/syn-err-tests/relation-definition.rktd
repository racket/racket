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

(#rx"found a 'where' clause not at the end"
 ([first-where (where any_c any_a)]
  [first-post-where (R () ())])
 (define-relation syn-err-lang
   [(R () ())]
   [(R (any_a) (any_b)) 
    (R any_c any_d) 
    first-where
    (where any_d any_b)
    first-post-where]))

(#rx"expected an identifier in the language position"
 ([not-lang [(R a)]])
 (define-relation not-lang))
