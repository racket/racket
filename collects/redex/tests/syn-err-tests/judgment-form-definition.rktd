(#rx"define-judgment-form:.*expression context"
 ([illegal-def (define-judgment-form L #:mode (J) [(J)])])
 (values illegal-def))

(#rx"expected an identifier defined by define-language"
 ([not-lang q])
 (define-judgment-form not-lang))

(#rx"expected.*I.*or.*O"
 ([bad-mode q])
 (define-judgment-form syn-err-lang
   #:mode (sum I bad-mode O)))

(#rx"expected mode specification"
 ([bad-spec q])
 (define-judgment-form syn-err-lang
   #:mode bad-spec))

(#rx"expected contract specification"
 ([bad-spec q])
 (define-judgment-form syn-err-lang
   #:mode (sum I I O)
   #:contract bad-spec))

(#rx"expected definition to include a mode specification"
 ([bad-def (define-judgment-form syn-err-lang
             #:contract (J)
             [(J)])])
 bad-def)

(#rx"expected at most one contract specification"
 ([dup-spec (J)])
 (define-judgment-form syn-err-lang
  #:mode (J)
  #:contract (J)
  #:contract dup-spec))

(#rx"expected at most one contract specification"
 ([dup-spec1 (J)] [dup-spec2 (J)])
 (define-judgment-form syn-err-lang
   #:contract (J)
   #:mode (J)
   #:contract dup-spec1
   #:contract dup-spec2))

(#rx"expected the same name"
 ([name1 J] [name2 K]) ([name3 J])
 (define-judgment-form syn-err-lang
   #:mode (name1)
   #:contract (name2)
   [(name3)]))

(#rx"expected the same name"
 ([name1 J] [name2 K]) ([name3 J])
 (define-judgment-form syn-err-lang
   #:mode (name1)
   #:contract (name3)
   [(name2)]))

(#rx"expected at least one rule"
 ([bad-def (define-judgment-form syn-err-lang
             #:mode (J I)
             #:contract (J n))])
 bad-def)
(#rx"malformed premise"
 ([bad-prem q])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I)
     [(J number)
      bad-prem])
   (void)))
(#rx"expected judgment form name"
 ([bad-judgment-form q])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I)
     [(J number)
      (bad-judgment-form)])
   (void)))
(#rx"different numbers of positions"
 ([bad-def (define-judgment-form syn-err-lang
             #:mode (J I)
             #:contract (J n n)
             [(J number)])])
 bad-def)

(#rx"unbound pattern variable"
 ([unbound number_2])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I O)
     [(J number_1 unbound)
      (J number_1 number_1)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound number_2])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I O)
     [(J number_1 number_2)
      (J unbound number_1)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound number_3])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I O)
     [(J number_1 number_2)
      (where number_2 unbound)])
   (void)))
(#rx"unbound pattern variable"
 ([unbound q])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (J I O)
     [(J number_1 number_2)
      (where number_2 unbound)
      (where (name q number) number_1)])
   (void)))
(#rx"J: mode specifies a 1-ary relation but use supplied 0 terms"
 ([bad-conc (J)]) ([name J])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name I)
     [bad-conc])
   (void)))
(#rx"J: mode specifies a 1-ary relation but use supplied 0 terms"
 ([bad-prem (J)]) ([name J])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name I)
     [(name number)
      bad-prem])
   (void)))
(#rx"J: mode specifies a 0-ary relation but use supplied 1 term"
 ([bad-prem (J a)]) ([name J])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name)
     [(name)
      bad-prem])
   (void)))
(#rx"unquote unsupported"
 ([unq ,(+ 1)])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (uses-unquote I)
     [(uses-unquote n)
      (where n unq)])
   (void)))
(#rx"unquote unsupported"
 ([unq ,'z])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (uses-unquote I O)
     [(uses-unquote n unq)])
   (void)))

(#rx"missing ellipses"
 ([use any_0]) ([ellipsis ...] [def any_0])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (tmpl-depth I O)
     [(tmpl-depth (def ellipsis) any)
      (tmpl-depth use any)])
   (void)))
(#rx"same binder.*different depths"
 ([binder1 any_0] [binder2 any_0])
 ([ellipsis ...])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (pat-depth I O)
     [(pat-depth (binder2 ellipsis) ())
      (pat-depth () binder1)])
   (void)))
(#rx"too many ellipses"
 ([premise (no-ellipsis any)])
 ([binder any] [name no-ellipsis] [ellipsis ...])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name I)
     [(name binder)
      premise ellipsis])
   (void)))