(#rx"expected a mode"
 ([bad-def (define-judgment-form syn-err-lang)])
 bad-def)
(#rx"expected a mode"
 ([mode-kw mode])
 (define-judgment-form syn-err-lang mode-kw))
(#rx"expected a clause"
 ([junk 1])
 (define-judgment-form syn-err-lang
   mode : junk
   [(q 1)]))
(#rx"expected at least one clause"
 ([bad-def (define-judgment-form syn-err-lang mode :)])
 bad-def)
(#rx"expected a pattern to follow"
 ([cross ×])
 (define-judgment-form syn-err-lang
   mode : I
   J ⊆ number cross))
(#rx"use the same name"
 ([name1 J] [name2 K])
 (define-judgment-form syn-err-lang
   mode : I
   name1 ⊆ number
   [(name2 number)]))
(#rx"malformed premise"
 ([bad-prem (q)])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I
     [(J number)
      bad-prem])
   (void)))
(#rx"different numbers of positions"
 ([bad-def (define-judgment-form syn-err-lang
             mode : I
             J ⊆ number × number
             [(J number)])])
 bad-def)

(#rx"unbound pattern variable"
 ([unbound number_2])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I O
     [(J number_1 unbound)
      (J number_1 number_1)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound number_2])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I O
     [(J number_1 number_2)
      (J unbound number_1)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound number_3])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I O
     [(J number_1 number_2)
      (where number_2 unbound)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound q])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I O
     [(J number_1 number_2)
      (where number_2 unbound)
      (where (name q number) number_1)])
   (void)))
(#rx"arity"
 ([bad-conc (J)])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I
     [bad-conc])
   (void)))
(#rx"arity"
 ([bad-prem (J)]) ([name J])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I
     [(name number)
      bad-prem])
   (void)))
(#rx"unquote unsupported"
 ([unq ,(+ 1)])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I
     [(uses-unquote n)
      (where n unq)])
   (void)))
(#rx"unquote unsupported"
 ([unq ,'z])
 (let ()
   (define-judgment-form syn-err-lang
     mode : I O
     [(uses-unquote n unq)])
   (void)))