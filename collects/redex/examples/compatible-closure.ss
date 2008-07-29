(module compatible-closure mzscheme
  (require (planet robby/redex:5/reduction-semantics)
           (planet robby/redex:5/gui))
  
  (define-language grammar
    (B t
       f
       (B * B)))

  (define r
    (reduction-relation
     grammar
     (--> (f * B_1) B_1 false) ; [a]
     (--> (t * B_1) t true))) ; [b]

  (define ->r (compatible-closure r grammar B))
  
  (traces grammar ->r '((f * f) * (t * f))))
