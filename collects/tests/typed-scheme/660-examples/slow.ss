(module slow "../../typed-scheme.ss"
  (require "../../CSU660/datatype.ss")

(define-type BINTREE
  [Node (l BINTREE) (r BINTREE)]
  [Leaf (n number)]
  [Q ]
  [Q1 ]
  [Q2 ]
  [Q3 ]
  [Q4 ]
  [Q5 ]

  )

(cases (Leaf 1)
  [(Node (Node (Node (Node (Node zz z) x) a) e) b) a]
  [(Node a b) a]
  [(Q) 1]
  [(Q1) 1]
  [(Q2) 1]
  [(Q3) 1]
  [(Q4) 1]
  [(Q5) 1]
  [(Leaf l) l]))
