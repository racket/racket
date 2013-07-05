(#rx"no rules"
 ([unused ==>])
 (reduction-relation 
  syn-err-lang
  (~~> (number_1 number_2)
       ,(* (term number_1) (term number_2)))
  with
  [(--> (M a) (M b)) (~~> a b)]
  [(~~> (M a) (M b)) (unused a b)]))

(#rx"no rules use -->"
 ([bad-def (reduction-relation syn-err-lang)])
 bad-def)

(#rx"~~> relation is not defined"
 ([undef ~~>])
 (reduction-relation 
  syn-err-lang
  (undef (number_1 number_2)
         ,(* (term number_1) (term number_2)))))

(#rx"same name on multiple rules"
 ([name1 mult] [name2 mult])
 (reduction-relation 
  syn-err-lang
  (--> (number_1 number_2) 
       ,(* (term number_1) (term number_2))
       name2)
  (--> (number_1 number_2) 
       ,(* (term number_1) (term number_2))
       name1)))


(#rx"different depths"
 ([binder1 number_1] [binder2 number_1]) ([ellipsis ...])
 (reduction-relation
  syn-err-lang
  (--> binder1
       ()
       (where (binder2 ellipsis) '()))))

(#rx"different depths"
 ([binder1 x] [binder2 x]) ([ellipsis ...])
 (redex-match
  syn-err-lang
  ((name binder1 any) (name binder2 any_2) ellipsis)))

(#rx"different depths"
 ([binder1 x] [binder2 x]) ([ellipsis ...])
 (let ()
   (define-language bad-lang5
     (e ((name binder1 any) (name binder2 any_2) ellipsis)))
   (void)))

(#rx"==> relation is not defined"
 ([undef ==>])
 (reduction-relation 
  syn-err-lang
  (--> 1 2)
  (undef 3 4)))

(#rx"~> relation is not defined"
 ([undef ~>])
 (reduction-relation 
  syn-err-lang
  (--> 1 2)
  (==> 3 4)
  with
  [(undef a b) (==> a b)]))

(#rx"expected identifier"
 ([not-id (+ 3 b)])
 (reduction-relation
  syn-err-lang
  (==> 1 2)
  with
  [(--> a b)
   (==> a not-id)]))

(#rx"expected identifier"
 ([not-id (+ 3 a)])
 (reduction-relation
  syn-err-lang
  (==> 1 2)
  with
  [(--> a b)
   (==> not-id b)]))

(#rx"name expected to have arguments" 
 ([name-kw name])
 (let () (define-language bad-lang1 (e name-kw)) (void)))
(#rx"name expected to have 2 arguments" 
 ([bad-pat (name x)])
 (let () (define-language bad-lang2 (e bad-pat)) (void)))
(#rx"cannot use _" ([bad-underscore x_y]) (define-language bad-lang3 (bad-underscore x)))
(#rx"at least one production" ([nt b]) (define-language bad-lang4 (a 1 2) (nt)))
(#rx"at least one production"
 ([nt a])
 (let ()
   (define-language good-lang (nt 1 2))
   (define-extended-language bad-lang5 good-lang (nt) (b 2))
   (void)))
(#rx"same non-terminal"
 ([nt2 x] [nt1 x])
 (define-language bad-lang5 (nt1 1) (nt2 2)))
(#rx"same non-terminal"
 ([nt2 x] [nt1 x])
 (define-language bad-lang6 ((nt1 nt2) 1)))
(#rx"same non-terminal"
 ([nt2 x] [nt1 x])
 (let ()
   (define-language good-lang)
   (define-extended-language bad-lang7 good-lang ((nt1 nt2) 1))))

(#rx"before underscore" 
 ([bad-underscore m_1])
 (redex-match syn-err-lang bad-underscore))
(#rx"expected an identifier"
 ([not-id 2])
 (redex-match syn-err-lang (variable-except a not-id c)))
(#rx"expected an identifier"
 ([not-id 7])
 (redex-match syn-err-lang (variable-prefix not-id)))
(#rx"expected an identifier"
 ([not-id 7])
 (redex-match syn-err-lang (cross not-id)))

(#rx"a?: mode specifies a 1-ary relation but use supplied 2 terms"
 ([bad-judgment (a? a q)])
 ([name a?])
 (let ()
   (define-judgment-form syn-err-lang
     #:mode (name I)
     [(name a)])
   (reduction-relation
    syn-err-lang
    (--> 1 1
         (judgment-holds bad-judgment)))))

(#rx"before underscore"
 ([not-non-term Z_1])
 (reduction-relation 
  syn-err-lang
  (--> (in-hole E (Q_1 M_1))
       (in-hole E (M_1 not-non-term)))))
