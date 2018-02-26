#hash((__x
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (138 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (7 . #s(stx-boundary (s0 (s1 . s2))))
        (2 . #s(stx-boundary (s0 (s1 . s2))))))
      ((#%stratified-body
        (define (first z) z)
        (define (ok x) (second x))
        (define (second y) 8)
        (ok (first 5))
        (define more 'oops))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5))
             (s1 s8 (s9 s10)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5))
             (s1 s8 (s9 s10)))))
        (155 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s0 (s5 s6) 8)
             (s3 (s1 5))
             (s0 s7 (s8 s9)))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s0 (s5 s6) 8)
             (s3 (s1 5))
             (s0 s7 (s8 s9))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s0 (s5 s6) 8)
             (s3 (s1 5))
             (s0 s7 (s8 s9)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) 8)))
        (0 . #s(stx-boundary (s0 (s1 s2) 8)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) 8)))
        (21 . #s(stx-boundary (s0 (s1 s2) 8)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 5))))
        (127 . #s(stx-boundary (s0 (s1 5))))
        (14
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary ((s5) (s1 (s6) 8))))
         .
         #s(stx-boundary ((s7 (s3 (s0 5)) (s8 s9 (s10 s11))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) 8)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (10 . #s(stx-boundary (8)))
        (24 #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (3 . #f)
        (126 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (12 . #s(stx-boundary (8)))
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 (s1 (s2 5)) (s3 s4 (s5 s6))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (155 . #f)
        (10 . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (24
         #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5))))
         .
         #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 5))))
        (127 . #s(stx-boundary (s0 (s1 5))))
        (12 . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (4 . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 5))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 5))))
        (8 . #s(stx-boundary (s0 s1 (s2 5))))
        (21 . #s(stx-boundary (s0 s1 (s2 5))))
        (22 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s0 s1 (s2 5))))
        (9 . #s(stx-boundary (s0 s1 (s2 5))))
        (0 . #s(stx-boundary (s0 s1 (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 5))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 5))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))))
      ((quote-syntax (stx-quoted))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 (s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1))))
        (118 . #f)
        (7 . #s(stx-boundary (s0 (s1))))
        (2 . #s(stx-boundary (s0 (s1))))
        (7 . #s(stx-boundary (s0 (s1 (s2)))))
        (2 . #s(stx-boundary (s0 (s1 (s2)))))))
      ((module m racket/base
         (define-syntax (ok stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (ok))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (148 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (126 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (21 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9)))
         .
         #s(stx-boundary (s15 (s8 (s9 s10) (s11 (s3 s12) (s13 s14))) (s9))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (21
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11))))
         .
         #s(stx-boundary
            (s17
             (s3 s4 (s5 s6) (s7 s8) (s9 #f))
             (s10 (s11 s12) (s13 (s5 s14) (s15 s16)))
             (s11))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (127
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (102 . #f)
        (148
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (130 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (132 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (133 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (131 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (148 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (11
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 (s2 s13) (s14 s15)))))
         #s(stx-boundary (s7 s8 (s10))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (158 . #f)
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (102 . #f)
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 #f)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (148 . #s(stx-boundary (s0 #f)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f)))
        (6 . #s(stx-boundary (s0 s1 #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 #f))))
        (7 . #s(stx-boundary (s0 s1 (s2 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 #f))))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (130 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (132 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (133 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (131 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (22
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9))))))
         .
         #s(stx-boundary (s10 s11 (s12 (s2 s4) (s5 (s6 s7) (s8 s9))))))
        (9 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (0 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (2 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (148 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (11
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         #s(stx-boundary (s9 s10 (s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (148 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (103 . #f)
        (157 . #f)
        (20 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 s3) (s4 s5)))))
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 s4))))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (12 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (4 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))))
      ((module m racket/base (define (proc x) x) (provide proc))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (148 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (126 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (21 . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (22
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9)))
         .
         #s(stx-boundary (s12 (s8 (s9 s10) s10) (s11 s9))))
        (9
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (0
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (21
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11))))
         .
         #s(stx-boundary
            (s14
             (s3 s4 (s5 s6) (s7 s8) (s9 #f))
             (s10 (s11 s12) s12)
             (s13 s11))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (127
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (102 . #f)
        (148
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (130 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (132 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (133 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (131 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (148 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (11
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) s11)))
         #s(stx-boundary (s7 s8 (s12 s10))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (158 . #f)
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (102 . #f)
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 #f)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (148 . #s(stx-boundary (s0 #f)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f)))
        (6 . #s(stx-boundary (s0 s1 #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 #f))))
        (7 . #s(stx-boundary (s0 s1 (s2 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 #f))))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (130 . #s(stx-boundary (s0 (s1 s2) s2)))
        (132 . #s(stx-boundary (s0 (s1 s2) s2)))
        (141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (133 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (131 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4))))
         .
         #s(stx-boundary (s5 s6 (s7 (s2 s4) s4))))
        (9 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (0 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (2 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (148 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (11
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         #s(stx-boundary (s4 s5 (s6 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (148 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))
        (130 . #s(stx-boundary (s0 s1)))
        (132 . #s(stx-boundary (s0 s1)))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 (s1 (s2 s3)))) . #s(stx-boundary (s4 s3)))
        (9 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (133 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (131 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (22
         #s(stx-boundary (s0 (s1 (s2 (s3 s4)))))
         .
         #s(stx-boundary (s5 s6 (s7 s4))))
        (9 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (0 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (2 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (148 . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (11 #s(stx-boundary (s0 (s1 (s2 s3)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (148 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (3 . #f)
        (135)
        (13 . #f)
        (6 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (122 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (9 . #s(stx-boundary (s0 s1)))
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (7 . #f)
        (3 . #f)
        (7
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s9 (s10) (s11 (s12) s12))
             (s13 s10))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s9 (s10) (s11 (s12) s12))
             (s13 s10))))
        (148
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))
        (7
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))))
      ((module m racket/base 'done)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (101 . #f)
        (157 . #f)
        (148 . #s(stx-boundary (s0 s1)))
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (142 . #s(stx-boundary (s0 (s1 s2))))
        (126 . #s(stx-boundary (s0 (s1 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8)))
         .
         #s(stx-boundary (s9 (s3 s8))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (21 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (22
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10))))
         .
         #s(stx-boundary (s11 (s3 s4 (s5 s6) (s7 s8) (s9 #f)) (s5 s10))))
        (9
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (127
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (0
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (102 . #f)
        (148
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (130 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (132 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (133 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (131 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (148 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (11
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s2 s9))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (158 . #f)
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (102 . #f)
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 #f)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (148 . #s(stx-boundary (s0 #f)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f)))
        (6 . #s(stx-boundary (s0 s1 #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 #f))))
        (7 . #s(stx-boundary (s0 s1 (s2 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 #f))))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))
        (130 . #s(stx-boundary (s0 s1)))
        (132 . #s(stx-boundary (s0 s1)))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (133 . #s(stx-boundary (s0 s1)))
        (131 . #s(stx-boundary (s0 s1)))
        (22
         #s(stx-boundary (s0 (s1 (s2 s3))))
         .
         #s(stx-boundary (s4 s1 (s2 s3))))
        (9 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (148 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (11 #s(stx-boundary (s0 (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5))
         .
         #s(stx-boundary (s6 (s3 s4))))
        (9 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (0 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (2 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (148 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 () (s2 s3)) s4)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () (s1 s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 s2))))
        (110 . #f)
        (17 #s(stx-boundary ()) . #s(stx-boundary ((s0 s1))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (5 . #s(stx-boundary ((s0 s1))))
        (7 . #s(stx-boundary (s0 () (s1 s2))))
        (2 . #s(stx-boundary (s0 () (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 (s1 () (s2 s3)) s4)))
        (7 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (2 . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (135)
        (13 . #f)
        (3 . #f)
        (7
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s7 s9 (s10 () (s3 s11)) s12))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s7 s9 (s10 () (s3 s11)) s12))))
        (148
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))
        (7
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))))
      ((let () (define-syntax (ok stx) (quote-syntax 8)) (ok 5))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 8))))
        (21 . #s(stx-boundary (s0 (s1) (s2 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 8))))
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 5)))
        (21 . #s(stx-boundary (s0 5)))
        (22 #s(stx-boundary 8) . #s(stx-boundary (s0 5)))
        (9 . #s(stx-boundary 8))
        (2 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (14 #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (0 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         ()
         .
         #s(stx-boundary (8)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (142 . #s(stx-boundary (s0 () (s1 8))))
        (7 . #s(stx-boundary (s0 () (s1 8))))
        (2 . #s(stx-boundary (s0 () (s1 8))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((with-continuation-mark __x __y __z)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 s1 s2 s3)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 s3)))
        (106 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (7 . #s(stx-boundary (s0 (s1 . s2) (s1 . s3) (s1 . s4))))
        (2 . #s(stx-boundary (s0 (s1 . s2) (s1 . s3) (s1 . s4))))
        (7 . #s(stx-boundary (s0 (s1 (s2 . s3) (s2 . s4) (s2 . s5)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 . s3) (s2 . s4) (s2 . s5)))))))
      ((#%top . __x)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 . s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 . s2))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 . s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (7 . #s(stx-boundary (s0 (s1 . s2))))
        (2 . #s(stx-boundary (s0 (s1 . s2))))))
      ((let () (define-syntax-rule (ok x) x) (ok 5))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5)))
         .
         #s(stx-boundary (s4 () (s1 (s2 s3) s3) (s2 5))))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) s2) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8))))))))
         .
         #s(stx-boundary (s5 (s1 s8) s8)))
        (9
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (0
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (21
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (22
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8))))))))
         .
         #s(stx-boundary
            (s13
             s1
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (127
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3)
              (s4
               s5
               #t
               s3
               ()
               s6
               #f
               ((s7 s8) (s9 (s10 s3 s8)))
               (s7 (s11 s3 (s12 (s8)))))))))
        (103 . #f)
        (148
         .
         #s(stx-boundary
            ((s0)
             (s1
              (s2)
              (s3
               s4
               #t
               s2
               ()
               s5
               #f
               ((s6 s7) (s8 (s9 s2 s7)))
               (s6 (s10 s2 (s11 (s7)))))))))
        (157 . #f)
        (144 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              s3
              #t
              s1
              ()
              s4
              #f
              ((s5 s6) (s7 (s8 s1 s6)))
              (s5 (s9 s1 (s10 (s6))))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              s3
              #t
              s1
              ()
              s4
              #f
              ((s5 s6) (s7 (s8 s1 s6)))
              (s5 (s9 s1 (s10 (s6))))))))
        (110 . #f)
        (17
         #s(stx-boundary (s0))
         .
         #s(stx-boundary
            ((s1
              s2
              #t
              s0
              ()
              s3
              #f
              ((s4 s5) (s6 (s7 s0 s5)))
              (s4 (s8 s0 (s9 (s5))))))))
        (10
         .
         #s(stx-boundary
            ((s0
              s1
              #t
              s2
              ()
              s3
              #f
              ((s4 s5) (s6 (s7 s2 s5)))
              (s4 (s8 s2 (s9 (s5))))))))
        (24
         #s(stx-boundary
            ((s0
              s1
              #t
              s2
              ()
              s3
              #f
              ((s4 s5) (s6 (s7 s2 s5)))
              (s4 (s8 s2 (s9 (s5)))))))
         .
         #s(stx-boundary
            ((s0
              s1
              #t
              s2
              ()
              s3
              #f
              ((s4 s5) (s6 (s7 s2 s5)))
              (s4 (s8 s2 (s9 (s5))))))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0
             s1
             #t
             s2
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s2 s5)))
             (s4 (s8 s2 (s9 (s5)))))))
        (0
         .
         #s(stx-boundary
            (s0
             s1
             #t
             s2
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s2 s5)))
             (s4 (s8 s2 (s9 (s5)))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             s1
             #t
             s2
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s2 s5)))
             (s4 (s8 s2 (s9 (s5)))))))
        (21
         .
         #s(stx-boundary
            (s0
             s1
             #t
             s2
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s2 s5)))
             (s4 (s8 s2 (s9 (s5)))))))
        (22
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1)))))))
         .
         #s(stx-boundary
            (s24
             s25
             #t
             s2
             ()
             s26
             #f
             ((s27 s16) (s19 (s20 s2 s16)))
             (s27 (s21 s2 (s22 (s16)))))))
        (9
         .
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1))))))))
        (2
         .
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1))))))))
        (0
         .
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1))))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1))))))))
        (21
         .
         #s(stx-boundary
            (s0
             ((s1 s2))
             (s0
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s0 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s1)))
              (s6
               s3
               (s0
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s2 s16))))
               (s0
                ((s3 ((s4 (s5) s8) s1)))
                (s6
                 s3
                 (s0 () (s15 () () (s21 s2 (s22 (s16)))))
                 (s23 #f #:opaque s1))))))))
        (22
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1)))))))
         .
         #s(stx-boundary
            (s3
             ((s1 s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (9
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (127
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (12
         .
         #s(stx-boundary
            ((s0
              (((s1) s2))
              (s3
               ((s4
                 ((s5
                   (s6)
                   (s7
                    (s8 s6)
                    (s7
                     ((s5 (s6) s9) (s10 s6))
                     ((s5
                       (s6)
                       (s7
                        (s8 s6)
                        (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                        #f))
                      (s14 s6))
                     #f)
                    #f))
                  s1)))
               (s7
                s4
                (s3
                 ((s15 s4))
                 (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
                (s3
                 ((s4 ((s5 (s6) s9) s1)))
                 (s7
                  s4
                  (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                  (s24 #f #:opaque s1)))))))))
        (4
         .
         #s(stx-boundary
            ((s0
              (((s1) s2))
              (s3
               ((s4
                 ((s5
                   (s6)
                   (s7
                    (s8 s6)
                    (s7
                     ((s5 (s6) s9) (s10 s6))
                     ((s5
                       (s6)
                       (s7
                        (s8 s6)
                        (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                        #f))
                      (s14 s6))
                     #f)
                    #f))
                  s1)))
               (s7
                s4
                (s3
                 ((s15 s4))
                 (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
                (s3
                 ((s4 ((s5 (s6) s9) s1)))
                 (s7
                  s4
                  (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                  (s24 #f #:opaque s1)))))))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s3
              ((s4
                ((s5
                  (s6)
                  (s7
                   (s8 s6)
                   (s7
                    ((s5 (s6) s9) (s10 s6))
                    ((s5
                      (s6)
                      (s7
                       (s8 s6)
                       (s3 ((s11 (s10 s6))) (s12 s11 (s13 (s14 s6)) s11))
                       #f))
                     (s14 s6))
                    #f)
                   #f))
                 s1)))
              (s7
               s4
               (s3
                ((s15 s4))
                (s16 (((s17) (s18 0 (s19 s15)))) () (s20 (s21 s2 s17))))
               (s3
                ((s4 ((s5 (s6) s9) s1)))
                (s7
                 s4
                 (s3 () (s16 () () (s22 s2 (s23 (s17)))))
                 (s24 #f #:opaque s1))))))))
        (112 . #f)
        (16
         (#s(stx-boundary ((s0) s1)))
         .
         #s(stx-boundary
            ((s2
              ((s3
                ((s4
                  (s5)
                  (s6
                   (s7 s5)
                   (s6
                    ((s4 (s5) s8) (s9 s5))
                    ((s4
                      (s5)
                      (s6
                       (s7 s5)
                       (s2 ((s10 (s9 s5))) (s11 s10 (s12 (s13 s5)) s10))
                       #f))
                     (s13 s5))
                    #f)
                   #f))
                 s0)))
              (s6
               s3
               (s2
                ((s14 s3))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s1 s16))))
               (s2
                ((s3 ((s4 (s5) s8) s0)))
                (s6
                 s3
                 (s2 () (s15 () () (s21 s1 (s22 (s16)))))
                 (s23 #f #:opaque s0))))))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0
              ((s1
                ((s2
                  (s3)
                  (s4
                   (s5 s3)
                   (s4
                    ((s2 (s3) s6) (s7 s3))
                    ((s2
                      (s3)
                      (s4
                       (s5 s3)
                       (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                       #f))
                     (s11 s3))
                    #f)
                   #f))
                 s12)))
              (s4
               s1
               (s0
                ((s13 s1))
                (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
               (s0
                ((s1 ((s2 (s3) s6) s12)))
                (s4
                 s1
                 (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                 (s23 #f #:opaque s12))))))))
        (24
         #s(stx-boundary
            ((s0
              ((s1
                ((s2
                  (s3)
                  (s4
                   (s5 s3)
                   (s4
                    ((s2 (s3) s6) (s7 s3))
                    ((s2
                      (s3)
                      (s4
                       (s5 s3)
                       (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                       #f))
                     (s11 s3))
                    #f)
                   #f))
                 s12)))
              (s4
               s1
               (s0
                ((s13 s1))
                (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
               (s0
                ((s1 ((s2 (s3) s6) s12)))
                (s4
                 s1
                 (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                 (s23 #f #:opaque s12)))))))
         .
         #s(stx-boundary
            ((s0
              ((s1
                ((s2
                  (s3)
                  (s4
                   (s5 s3)
                   (s4
                    ((s2 (s3) s6) (s7 s3))
                    ((s2
                      (s3)
                      (s4
                       (s5 s3)
                       (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                       #f))
                     (s11 s3))
                    #f)
                   #f))
                 s12)))
              (s4
               s1
               (s0
                ((s13 s1))
                (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
               (s0
                ((s1 ((s2 (s3) s6) s12)))
                (s4
                 s1
                 (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                 (s23 #f #:opaque s12))))))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0
             ((s1
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                      #f))
                    (s11 s3))
                   #f)
                  #f))
                s12)))
             (s4
              s1
              (s0
               ((s13 s1))
               (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
              (s0
               ((s1 ((s2 (s3) s6) s12)))
               (s4
                s1
                (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                (s23 #f #:opaque s12)))))))
        (0
         .
         #s(stx-boundary
            (s0
             ((s1
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                      #f))
                    (s11 s3))
                   #f)
                  #f))
                s12)))
             (s4
              s1
              (s0
               ((s13 s1))
               (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
              (s0
               ((s1 ((s2 (s3) s6) s12)))
               (s4
                s1
                (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                (s23 #f #:opaque s12)))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ((s1
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                      #f))
                    (s11 s3))
                   #f)
                  #f))
                s12)))
             (s4
              s1
              (s0
               ((s13 s1))
               (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
              (s0
               ((s1 ((s2 (s3) s6) s12)))
               (s4
                s1
                (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                (s23 #f #:opaque s12)))))))
        (21
         .
         #s(stx-boundary
            (s0
             ((s1
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s0 ((s8 (s7 s3))) (s9 s8 (s10 (s11 s3)) s8))
                      #f))
                    (s11 s3))
                   #f)
                  #f))
                s12)))
             (s4
              s1
              (s0
               ((s13 s1))
               (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
              (s0
               ((s1 ((s2 (s3) s6) s12)))
               (s4
                s1
                (s0 () (s14 () () (s21 s20 (s22 (s15)))))
                (s23 #f #:opaque s12)))))))
        (22
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13))))))
         .
         #s(stx-boundary
            (s8
             ((s1
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (9
         .
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (127
         .
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (12
         .
         #s(stx-boundary
            ((s0
              (((s1)
                ((s2
                  (s3)
                  (s4
                   (s5 s3)
                   (s4
                    ((s2 (s3) s6) (s7 s3))
                    ((s2
                      (s3)
                      (s4
                       (s5 s3)
                       (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                       #f))
                     (s12 s3))
                    #f)
                   #f))
                 s13)))
              (s4
               s1
               (s8
                ((s14 s1))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
               (s8
                ((s1 ((s2 (s3) s6) s13)))
                (s4
                 s1
                 (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                 (s24 #f #:opaque s13))))))))
        (4
         .
         #s(stx-boundary
            ((s0
              (((s1)
                ((s2
                  (s3)
                  (s4
                   (s5 s3)
                   (s4
                    ((s2 (s3) s6) (s7 s3))
                    ((s2
                      (s3)
                      (s4
                       (s5 s3)
                       (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                       #f))
                     (s12 s3))
                    #f)
                   #f))
                 s13)))
              (s4
               s1
               (s8
                ((s14 s1))
                (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
               (s8
                ((s1 ((s2 (s3) s6) s13)))
                (s4
                 s1
                 (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                 (s24 #f #:opaque s13))))))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1)
               ((s2
                 (s3)
                 (s4
                  (s5 s3)
                  (s4
                   ((s2 (s3) s6) (s7 s3))
                   ((s2
                     (s3)
                     (s4
                      (s5 s3)
                      (s8 ((s9 (s7 s3))) (s10 s9 (s11 (s12 s3)) s9))
                      #f))
                    (s12 s3))
                   #f)
                  #f))
                s13)))
             (s4
              s1
              (s8
               ((s14 s1))
               (s15 (((s16) (s17 0 (s18 s14)))) () (s19 (s20 s21 s16))))
              (s8
               ((s1 ((s2 (s3) s6) s13)))
               (s4
                s1
                (s8 () (s15 () () (s22 s21 (s23 (s16)))))
                (s24 #f #:opaque s13)))))))
        (112 . #f)
        (16
         (#s(stx-boundary
             ((s0)
              ((s1
                (s2)
                (s3
                 (s4 s2)
                 (s3
                  ((s1 (s2) s5) (s6 s2))
                  ((s1
                    (s2)
                    (s3
                     (s4 s2)
                     (s7 ((s8 (s6 s2))) (s9 s8 (s10 (s11 s2)) s8))
                     #f))
                   (s11 s2))
                  #f)
                 #f))
               s12))))
         .
         #s(stx-boundary
            ((s3
              s0
              (s7
               ((s13 s0))
               (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
              (s7
               ((s0 ((s1 (s2) s5) s12)))
               (s3
                s0
                (s7 () (s14 () () (s21 s20 (s22 (s15)))))
                (s23 #f #:opaque s12)))))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2
               (s3 s1)
               (s2
                ((s0 (s1) s4) (s5 s1))
                ((s0
                  (s1)
                  (s2 (s3 s1) (s6 ((s7 (s5 s1))) (s8 s7 (s9 (s10 s1)) s7)) #f))
                 (s10 s1))
                #f)
               #f))
             s11)))
        (1 . #s(stx-boundary s0))
        (142
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s4 s2)
               (s3
                ((s1 (s2) s5) (s6 s2))
                ((s1
                  (s2)
                  (s3
                   (s4 s2)
                   (s7 ((s8 (s6 s2))) (s9 s8 (s10 (s11 s2)) s8))
                   #f))
                 (s11 s2))
                #f)
               #f))
             s12)))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s4 s2)
               (s3
                ((s1 (s2) s5) (s6 s2))
                ((s1
                  (s2)
                  (s3
                   (s4 s2)
                   (s7 ((s8 (s6 s2))) (s9 s8 (s10 (s11 s2)) s8))
                   #f))
                 (s11 s2))
                #f)
               #f))
             s12)))
        (109 . #f)
        (4
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2
               (s3 s1)
               (s2
                ((s0 (s1) s4) (s5 s1))
                ((s0
                  (s1)
                  (s2 (s3 s1) (s6 ((s7 (s5 s1))) (s8 s7 (s9 (s10 s1)) s7)) #f))
                 (s10 s1))
                #f)
               #f))
             s11)))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s1)
              (s2
               ((s0 (s1) s4) (s5 s1))
               ((s0
                 (s1)
                 (s2 (s3 s1) (s6 ((s7 (s5 s1))) (s8 s7 (s9 (s10 s1)) s7)) #f))
                (s10 s1))
               #f)
              #f))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s1)
              (s2
               ((s0 (s1) s4) (s5 s1))
               ((s0
                 (s1)
                 (s2 (s3 s1) (s6 ((s7 (s5 s1))) (s8 s7 (s9 (s10 s1)) s7)) #f))
                (s10 s1))
               #f)
              #f))))
        (110 . #f)
        (17
         #s(stx-boundary (s0))
         .
         #s(stx-boundary
            ((s1
              (s2 s0)
              (s1
               ((s3 (s0) s4) (s5 s0))
               ((s3
                 (s0)
                 (s1 (s2 s0) (s6 ((s7 (s5 s0))) (s8 s7 (s9 (s10 s0)) s7)) #f))
                (s10 s0))
               #f)
              #f))))
        (10
         .
         #s(stx-boundary
            ((s0
              (s1 s2)
              (s0
               ((s3 (s2) s4) (s5 s2))
               ((s3
                 (s2)
                 (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
                (s10 s2))
               #f)
              #f))))
        (24
         #s(stx-boundary
            ((s0
              (s1 s2)
              (s0
               ((s3 (s2) s4) (s5 s2))
               ((s3
                 (s2)
                 (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
                (s10 s2))
               #f)
              #f)))
         .
         #s(stx-boundary
            ((s0
              (s1 s2)
              (s0
               ((s3 (s2) s4) (s5 s2))
               ((s3
                 (s2)
                 (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
                (s10 s2))
               #f)
              #f))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0
             (s1 s2)
             (s0
              ((s3 (s2) s4) (s5 s2))
              ((s3
                (s2)
                (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
               (s10 s2))
              #f)
             #f)))
        (127
         .
         #s(stx-boundary
            (s0
             (s1 s2)
             (s0
              ((s3 (s2) s4) (s5 s2))
              ((s3
                (s2)
                (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
               (s10 s2))
              #f)
             #f)))
        (12
         .
         #s(stx-boundary
            ((s0
              (s1 s2)
              (s0
               ((s3 (s2) s4) (s5 s2))
               ((s3
                 (s2)
                 (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
                (s10 s2))
               #f)
              #f))))
        (4
         .
         #s(stx-boundary
            ((s0
              (s1 s2)
              (s0
               ((s3 (s2) s4) (s5 s2))
               ((s3
                 (s2)
                 (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
                (s10 s2))
               #f)
              #f))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2)
             (s0
              ((s3 (s2) s4) (s5 s2))
              ((s3
                (s2)
                (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
               (s10 s2))
              #f)
             #f)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 s2)
             (s0
              ((s3 (s2) s4) (s5 s2))
              ((s3
                (s2)
                (s0 (s1 s2) (s6 ((s7 (s5 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
               (s10 s2))
              #f)
             #f)))
        (105 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             ((s1 (s2) s3) (s4 s2))
             ((s1
               (s2)
               (s0 (s5 s2) (s6 ((s7 (s4 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
              (s10 s2))
             #f)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             ((s1 (s2) s3) (s4 s2))
             ((s1
               (s2)
               (s0 (s5 s2) (s6 ((s7 (s4 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
              (s10 s2))
             #f)))
        (105 . #f)
        (0 . #s(stx-boundary ((s0 (s1) s2) (s3 s1))))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (6 . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (109 . #f)
        (4 . #s(stx-boundary ((s0 (s1) s2) (s3 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s2)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s1)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s2)))
        (2 . #s(stx-boundary (s0 (s1) s2)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 (s1) s2) (s3 s4 s1))))
        (7 . #s(stx-boundary (s0 (s1 (s2) s3) (s0 s4 s2))))
        (2 . #s(stx-boundary (s0 (s1 (s2) s3) (s0 s4 s2))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))
             (s9 s1))))
        (1 . #s(stx-boundary s0))
        (142
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2))))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2))))
        (109 . #f)
        (4
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))
             (s9 s1))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))))
        (110 . #f)
        (17
         #s(stx-boundary (s0))
         .
         #s(stx-boundary
            ((s1 (s2 s0) (s3 ((s4 (s5 s0))) (s6 s4 (s7 (s8 s0)) s4)) #f))))
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (127
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (12
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (4
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (105 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (21 . #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (22
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1)))
         .
         #s(stx-boundary (s7 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (9 . #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (0 . #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (112 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 s2))))
         .
         #s(stx-boundary ((s3 s0 (s4 (s5 s2)) s0))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1))))
        (24
         #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1)))
         .
         #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f))
         .
         #s(stx-boundary (s2 s1 (s3 (s4 s5)) s1)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (127 . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (12 . #s(stx-boundary ((s0 s1 (s2 (s3 (s4 s5)) s1) #f))))
        (4 . #s(stx-boundary ((s0 s1 (s2 (s3 (s4 s5)) s1) #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (105 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (21 . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (22
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f))
         .
         #s(stx-boundary (s4 (s1 (s2 s3)) s5)))
        (9 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (105 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 s3))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary (s0 (s1 s2 s3))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (9 . #s(stx-boundary (s0 s1)))
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (138 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (142 . #s(stx-boundary s0))
        (7 . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (7 . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) s5 (s6 #f))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) s5 (s6 #f))))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (7
         .
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (2
         .
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (5
         .
         #s(stx-boundary ((s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f)))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 s3 s4)))
             (s5 s1 (s5 (s2 s6 (s2 s7 s4)) s1 (s8 #f)) (s8 #f)))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 s3 s4)))
             (s5 s1 (s5 (s2 s6 (s2 s7 s4)) s1 (s8 #f)) (s8 #f)))))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (7
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (5
         .
         #s(stx-boundary
            ((s0
              (s1 s2 s3)
              (s4
               (((s5) (s1 s6 s3)))
               (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
              (s9 #f)))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s4 s1)
              (s5
               (((s6) (s3 s7 s1)))
               (s2 s6 (s2 (s3 s8 (s3 s9 s1)) s6 (s10 #f)) (s10 #f)))
              (s10 #f)))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s4 s1)
              (s5
               (((s6) (s3 s7 s1)))
               (s2 s6 (s2 (s3 s8 (s3 s9 s1)) s6 (s10 #f)) (s10 #f)))
              (s10 #f)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2
               (s3 s4 s1)
               (s5
                (((s6) (s3 s7 s1)))
                (s2 s6 (s2 (s3 s8 (s3 s9 s1)) s6 (s10 #f)) (s10 #f)))
               (s10 #f)))
             (s3 s9 s1))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s0 s4 s2)
               (s5
                (((s6) (s0 s7 s2)))
                (s3 s6 (s3 (s0 s8 (s0 s9 s2)) s6 (s10 #f)) (s10 #f)))
               (s10 #f)))
             (s0 s9 s2))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s0 s4 s2)
               (s5
                (((s6) (s0 s7 s2)))
                (s3 s6 (s3 (s0 s8 (s0 s9 s2)) s6 (s10 #f)) (s10 #f)))
               (s10 #f)))
             (s0 s9 s2))))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (7
         .
         #s(stx-boundary
            (s0
             (s1 (s2 (s3) s4) (s1 s5 s3))
             (s1
              (s2
               (s3)
               (s0
                (s1 s6 s3)
                (s7
                 (((s8) (s1 s5 s3)))
                 (s0 s8 (s0 (s1 s9 (s1 s10 s3)) s8 (s11 #f)) (s11 #f)))
                (s11 #f)))
              (s1 s10 s3))
             (s11 #f))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 (s2 (s3) s4) (s1 s5 s3))
             (s1
              (s2
               (s3)
               (s0
                (s1 s6 s3)
                (s7
                 (((s8) (s1 s5 s3)))
                 (s0 s8 (s0 (s1 s9 (s1 s10 s3)) s8 (s11 #f)) (s11 #f)))
                (s11 #f)))
              (s1 s10 s3))
             (s11 #f))))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (7
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s0
              (s1 (s4 (s3) s5) (s1 s6 s3))
              (s1
               (s4
                (s3)
                (s0
                 (s1 s2 s3)
                 (s7
                  (((s8) (s1 s6 s3)))
                  (s0 s8 (s0 (s1 s9 (s1 s10 s3)) s8 (s11 #f)) (s11 #f)))
                 (s11 #f)))
               (s1 s10 s3))
              (s11 #f))
             (s11 #f))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s0
              (s1 (s4 (s3) s5) (s1 s6 s3))
              (s1
               (s4
                (s3)
                (s0
                 (s1 s2 s3)
                 (s7
                  (((s8) (s1 s6 s3)))
                  (s0 s8 (s0 (s1 s9 (s1 s10 s3)) s8 (s11 #f)) (s11 #f)))
                 (s11 #f)))
               (s1 s10 s3))
              (s11 #f))
             (s11 #f))))
        (5
         .
         #s(stx-boundary
            ((s0
              (s1 s2 s3)
              (s0
               (s1 (s4 (s3) s5) (s1 s6 s3))
               (s1
                (s4
                 (s3)
                 (s0
                  (s1 s2 s3)
                  (s7
                   (((s8) (s1 s6 s3)))
                   (s0 s8 (s0 (s1 s9 (s1 s10 s3)) s8 (s11 #f)) (s11 #f)))
                  (s11 #f)))
                (s1 s10 s3))
               (s11 #f))
              (s11 #f)))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s4 s1)
              (s2
               (s3 (s0 (s1) s5) (s3 s6 s1))
               (s3
                (s0
                 (s1)
                 (s2
                  (s3 s4 s1)
                  (s7
                   (((s8) (s3 s6 s1)))
                   (s2 s8 (s2 (s3 s9 (s3 s10 s1)) s8 (s11 #f)) (s11 #f)))
                  (s11 #f)))
                (s3 s10 s1))
               (s11 #f))
              (s11 #f)))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (s3 s4 s1)
              (s2
               (s3 (s0 (s1) s5) (s3 s6 s1))
               (s3
                (s0
                 (s1)
                 (s2
                  (s3 s4 s1)
                  (s7
                   (((s8) (s3 s6 s1)))
                   (s2 s8 (s2 (s3 s9 (s3 s10 s1)) s8 (s11 #f)) (s11 #f)))
                  (s11 #f)))
                (s3 s10 s1))
               (s11 #f))
              (s11 #f)))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2
               (s3 s4 s1)
               (s2
                (s3 (s0 (s1) s5) (s3 s6 s1))
                (s3
                 (s0
                  (s1)
                  (s2
                   (s3 s4 s1)
                   (s7
                    (((s8) (s3 s6 s1)))
                    (s2 s8 (s2 (s3 s9 (s3 s10 s1)) s8 (s11 #f)) (s11 #f)))
                   (s11 #f)))
                 (s3 s10 s1))
                (s11 #f))
               (s11 #f)))
             s12)))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s0 s4 s2)
               (s3
                (s0 (s1 (s2) s5) (s0 s6 s2))
                (s0
                 (s1
                  (s2)
                  (s3
                   (s0 s4 s2)
                   (s7
                    (((s8) (s0 s6 s2)))
                    (s3 s8 (s3 (s0 s9 (s0 s10 s2)) s8 (s11 #f)) (s11 #f)))
                   (s11 #f)))
                 (s0 s10 s2))
                (s11 #f))
               (s11 #f)))
             s12)))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3
               (s0 s4 s2)
               (s3
                (s0 (s1 (s2) s5) (s0 s6 s2))
                (s0
                 (s1
                  (s2)
                  (s3
                   (s0 s4 s2)
                   (s7
                    (((s8) (s0 s6 s2)))
                    (s3 s8 (s3 (s0 s9 (s0 s10 s2)) s8 (s11 #f)) (s11 #f)))
                   (s11 #f)))
                 (s0 s10 s2))
                (s11 #f))
               (s11 #f)))
             s12)))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14)))))))
        (24
         #s(stx-boundary
            ((s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14))))))
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14)))))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0
             s1
             (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
             (s2
              ((s1 ((s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s4 () () (s15 s10 (s16 (s5)))))
               (s17 #f #:opaque s14))))))
        (127
         .
         #s(stx-boundary
            (s0
             s1
             (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
             (s2
              ((s1 ((s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s4 () () (s15 s10 (s16 (s5)))))
               (s17 #f #:opaque s14))))))
        (12
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14)))))))
        (4
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14)))))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             s1
             (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
             (s2
              ((s1 ((s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s4 () () (s15 s10 (s16 (s5)))))
               (s17 #f #:opaque s14))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             s1
             (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
             (s2
              ((s1 ((s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s4 () () (s15 s10 (s16 (s5)))))
               (s17 #f #:opaque s14))))))
        (105 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (21
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (22
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4)))))
         .
         #s(stx-boundary
            (s10 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (9
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (112 . #f)
        (16
         (#s(stx-boundary ((s0) s1)))
         .
         #s(stx-boundary ((s2 (((s3) (s4 0 (s5 s0)))) () (s6 (s7 s8 s3))))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (13 . #f)
        (10
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (24
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (3 . #f)
        (126
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (127
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (12
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (4
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 0 (s2 s3)))))
         ()
         .
         #s(stx-boundary ((s4 (s5 s6 s0)))))
        (157 . #f)
        (3 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (6 . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 0 (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 0)))
        (6 . #s(stx-boundary (s0 . 0)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 0)))
        (2 . #s(stx-boundary (s0 0)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (5 . #s(stx-boundary (s0 (s1 0) (s2 s3))))
        (7 . #s(stx-boundary (s0 s1 (s2 0) (s3 s4))))
        (2 . #s(stx-boundary (s0 s1 (s2 0) (s3 s4))))
        (3 . #f)
        (145 . #f)
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (24
         #s(stx-boundary ((s0 (s1 s2 s3))))
         .
         #s(stx-boundary ((s0 (s1 s2 s3)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2 s3))))
        (127 . #s(stx-boundary (s0 (s1 s2 s3))))
        (12 . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (4 . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2 s3))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 s2 s3))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (153 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (154 . #t)
        (22 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s3 s1)))
        (9 . #s(stx-boundary (s0 s1)))
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (153 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (154 . #t)
        (22 #s(stx-boundary s0) . #s(stx-boundary (s1 s2)))
        (9 . #s(stx-boundary s0))
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (142 . #s(stx-boundary (s0 () (s1 s2 s3))))
        (7 . #s(stx-boundary (s0 () (s1 s2 s3))))
        (2 . #s(stx-boundary (s0 () (s1 s2 s3))))
        (5 . #s(stx-boundary ((s0 () (s1 s2 s3)))))
        (7 . #s(stx-boundary (s0 (((s1) s2)) (s0 () (s3 s4 s1)))))
        (2 . #s(stx-boundary (s0 (((s1) s2)) (s0 () (s3 s4 s1)))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (21
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (22
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5))))
         .
         #s(stx-boundary
            (s7
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (9
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (112 . #f)
        (16
         (#s(stx-boundary ((s0) ((s1 (s2) s3) s4))))
         .
         #s(stx-boundary
            ((s5
              s0
              (s6 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s4)))))
        (3 . #f)
        (0 . #s(stx-boundary ((s0 (s1) s2) s3)))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (6 . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (109 . #f)
        (4 . #s(stx-boundary ((s0 (s1) s2) s3)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s2)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s1)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s2)))
        (2 . #s(stx-boundary (s0 (s1) s2)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary ((s0 (s1) s2) s3)))
        (7 . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (2 . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (24
         #s(stx-boundary
            ((s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (3 . #f)
        (126
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (127
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (12
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (4
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (3 . #f)
        (0
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (105 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (21 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (22
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5))))))
         .
         #s(stx-boundary (s6 () (s1 () () (s2 s3 (s4 (s5)))))))
        (9 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (0 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (24
         #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4))))))
         .
         #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (127 . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (12 . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (4 . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (114 . #f)
        (19 () () . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (157 . #f)
        (13 . #f)
        (10 . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (24
         #s(stx-boundary ((s0 s1 (s2 (s3)))))
         .
         #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (127 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (12 . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (4 . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (6 . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1))))
        (117 . #f)
        (7 . #s(stx-boundary (s0 (s1))))
        (2 . #s(stx-boundary (s0 (s1))))
        (5 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (7 . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (2 . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (5 . #s(stx-boundary ((s0 s1 s2 (s3 (s4))))))
        (142 . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (7 . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (2 . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (5 . #s(stx-boundary ((s0 () (s1 s2 s3 (s4 (s5)))))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 s2 s3 (s4 (s5)))))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 s2 s3 (s4 (s5)))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f #:opaque s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (6 . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f #:opaque s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary #:opaque))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #:opaque)))
        (6 . #s(stx-boundary (s0 . #:opaque)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #:opaque)))
        (2 . #s(stx-boundary (s0 #:opaque)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 (s1 #f) (s1 #:opaque) s2)))
        (7 . #s(stx-boundary (s0 s1 (s2 #f) (s2 #:opaque) s3)))
        (2 . #s(stx-boundary (s0 s1 (s2 #f) (s2 #:opaque) s3)))
        (7
         .
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (5
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
              (s3 s8 (s6 #f) (s6 #:opaque) s9)))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3 (s4) s5) s6)))
             (s7
              s1
              (s0 () (s0 () (s2 s8 s9 (s10 (s11)))))
              (s2 s12 (s10 #f) (s10 #:opaque) s6)))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3 (s4) s5) s6)))
             (s7
              s1
              (s0 () (s0 () (s2 s8 s9 (s10 (s11)))))
              (s2 s12 (s10 #f) (s10 #:opaque) s6)))))
        (7
         .
         #s(stx-boundary
            (s0
             s1
             (s2 (((s3) s1)) (s2 () (s4 s5 s3)))
             (s2
              (((s1) (s4 (s6 (s7) s8) s9)))
              (s0
               s1
               (s2 () (s2 () (s4 s10 s11 (s12 (s13)))))
               (s4 s14 (s12 #f) (s12 #:opaque) s9))))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             (s2 (((s3) s1)) (s2 () (s4 s5 s3)))
             (s2
              (((s1) (s4 (s6 (s7) s8) s9)))
              (s0
               s1
               (s2 () (s2 () (s4 s10 s11 (s12 (s13)))))
               (s4 s14 (s12 #f) (s12 #:opaque) s9))))))
        (5
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 (((s3) s1)) (s2 () (s4 s5 s3)))
              (s2
               (((s1) (s4 (s6 (s7) s8) s9)))
               (s0
                s1
                (s2 () (s2 () (s4 s10 s11 (s12 (s13)))))
                (s4 s14 (s12 #f) (s12 #:opaque) s9)))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1)
               (s2
                (s3
                 (s4)
                 (s5
                  (s2 s6 s4)
                  (s5
                   (s2 (s3 (s4) s7) (s2 s8 s4))
                   (s2
                    (s3
                     (s4)
                     (s5
                      (s2 s6 s4)
                      (s0
                       (((s9) (s2 s8 s4)))
                       (s5 s9 (s5 (s2 s10 (s2 s11 s4)) s9 (s12 #f)) (s12 #f)))
                      (s12 #f)))
                    (s2 s11 s4))
                   (s12 #f))
                  (s12 #f)))
                s13)))
             (s5
              s1
              (s0 (((s14) s1)) (s0 () (s2 s15 s14)))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s16 s17 (s12 (s18)))))
                (s2 s19 (s12 #f) (s12 #:opaque) s13)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1)
               (s2
                (s3
                 (s4)
                 (s5
                  (s2 s6 s4)
                  (s5
                   (s2 (s3 (s4) s7) (s2 s8 s4))
                   (s2
                    (s3
                     (s4)
                     (s5
                      (s2 s6 s4)
                      (s0
                       (((s9) (s2 s8 s4)))
                       (s5 s9 (s5 (s2 s10 (s2 s11 s4)) s9 (s12 #f)) (s12 #f)))
                      (s12 #f)))
                    (s2 s11 s4))
                   (s12 #f))
                  (s12 #f)))
                s13)))
             (s5
              s1
              (s0 (((s14) s1)) (s0 () (s2 s15 s14)))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s16 s17 (s12 (s18)))))
                (s2 s19 (s12 #f) (s12 #:opaque) s13)))))))
        (5
         .
         #s(stx-boundary
            ((s0
              (((s1)
                (s2
                 (s3
                  (s4)
                  (s5
                   (s2 s6 s4)
                   (s5
                    (s2 (s3 (s4) s7) (s2 s8 s4))
                    (s2
                     (s3
                      (s4)
                      (s5
                       (s2 s6 s4)
                       (s0
                        (((s9) (s2 s8 s4)))
                        (s5 s9 (s5 (s2 s10 (s2 s11 s4)) s9 (s12 #f)) (s12 #f)))
                       (s12 #f)))
                     (s2 s11 s4))
                    (s12 #f))
                   (s12 #f)))
                 s13)))
              (s5
               s1
               (s0 (((s14) s1)) (s0 () (s2 s15 s14)))
               (s0
                (((s1) (s2 (s3 (s4) s7) s13)))
                (s5
                 s1
                 (s0 () (s0 () (s2 s16 s17 (s12 (s18)))))
                 (s2 s19 (s12 #f) (s12 #:opaque) s13))))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s0
              (((s3)
                (s4
                 (s5
                  (s6)
                  (s7
                   (s4 s8 s6)
                   (s7
                    (s4 (s5 (s6) s9) (s4 s10 s6))
                    (s4
                     (s5
                      (s6)
                      (s7
                       (s4 s8 s6)
                       (s0
                        (((s11) (s4 s10 s6)))
                        (s7
                         s11
                         (s7 (s4 s12 (s4 s13 s6)) s11 (s14 #f))
                         (s14 #f)))
                       (s14 #f)))
                     (s4 s13 s6))
                    (s14 #f))
                   (s14 #f)))
                 s1)))
              (s7
               s3
               (s0 (((s15) s3)) (s0 () (s4 s16 s15)))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s17 s2 (s14 (s18)))))
                 (s4 s19 (s14 #f) (s14 #:opaque) s1))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s0
              (((s3)
                (s4
                 (s5
                  (s6)
                  (s7
                   (s4 s8 s6)
                   (s7
                    (s4 (s5 (s6) s9) (s4 s10 s6))
                    (s4
                     (s5
                      (s6)
                      (s7
                       (s4 s8 s6)
                       (s0
                        (((s11) (s4 s10 s6)))
                        (s7
                         s11
                         (s7 (s4 s12 (s4 s13 s6)) s11 (s14 #f))
                         (s14 #f)))
                       (s14 #f)))
                     (s4 s13 s6))
                    (s14 #f))
                   (s14 #f)))
                 s1)))
              (s7
               s3
               (s0 (((s15) s3)) (s0 () (s4 s16 s15)))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s17 s2 (s14 (s18)))))
                 (s4 s19 (s14 #f) (s14 #:opaque) s1))))))))
        (5
         .
         #s(stx-boundary
            ((s0
              (((s1) s2))
              (s0
               (((s3)
                 (s4
                  (s5
                   (s6)
                   (s7
                    (s4 s8 s6)
                    (s7
                     (s4 (s5 (s6) s9) (s4 s10 s6))
                     (s4
                      (s5
                       (s6)
                       (s7
                        (s4 s8 s6)
                        (s0
                         (((s11) (s4 s10 s6)))
                         (s7
                          s11
                          (s7 (s4 s12 (s4 s13 s6)) s11 (s14 #f))
                          (s14 #f)))
                        (s14 #f)))
                      (s4 s13 s6))
                     (s14 #f))
                    (s14 #f)))
                  s1)))
               (s7
                s3
                (s0 (((s15) s3)) (s0 () (s4 s16 s15)))
                (s0
                 (((s3) (s4 (s5 (s6) s9) s1)))
                 (s7
                  s3
                  (s0 () (s0 () (s4 s17 s2 (s14 (s18)))))
                  (s4 s19 (s14 #f) (s14 #:opaque) s1)))))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (((s3) s1))
              (s2
               (((s4)
                 (s5
                  (s0
                   (s6)
                   (s7
                    (s5 s8 s6)
                    (s7
                     (s5 (s0 (s6) s9) (s5 s10 s6))
                     (s5
                      (s0
                       (s6)
                       (s7
                        (s5 s8 s6)
                        (s2
                         (((s11) (s5 s10 s6)))
                         (s7
                          s11
                          (s7 (s5 s12 (s5 s13 s6)) s11 (s14 #f))
                          (s14 #f)))
                        (s14 #f)))
                      (s5 s13 s6))
                     (s14 #f))
                    (s14 #f)))
                  s3)))
               (s7
                s4
                (s2 (((s15) s4)) (s2 () (s5 s16 s15)))
                (s2
                 (((s4) (s5 (s0 (s6) s9) s3)))
                 (s7
                  s4
                  (s2 () (s2 () (s5 s17 s1 (s14 (s18)))))
                  (s5 s19 (s14 #f) (s14 #:opaque) s3)))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2
              (((s3) s1))
              (s2
               (((s4)
                 (s5
                  (s0
                   (s6)
                   (s7
                    (s5 s8 s6)
                    (s7
                     (s5 (s0 (s6) s9) (s5 s10 s6))
                     (s5
                      (s0
                       (s6)
                       (s7
                        (s5 s8 s6)
                        (s2
                         (((s11) (s5 s10 s6)))
                         (s7
                          s11
                          (s7 (s5 s12 (s5 s13 s6)) s11 (s14 #f))
                          (s14 #f)))
                        (s14 #f)))
                      (s5 s13 s6))
                     (s14 #f))
                    (s14 #f)))
                  s3)))
               (s7
                s4
                (s2 (((s15) s4)) (s2 () (s5 s16 s15)))
                (s2
                 (((s4) (s5 (s0 (s6) s9) s3)))
                 (s7
                  s4
                  (s2 () (s2 () (s5 s17 s1 (s14 (s18)))))
                  (s5 s19 (s14 #f) (s14 #:opaque) s3)))))))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 5)))
        (21 . #s(stx-boundary (s0 5)))
        (22 #s(stx-boundary 5) . #s(stx-boundary (s0 5)))
        (9 . #s(stx-boundary 5))
        (2 . #s(stx-boundary 5))
        (127 . #s(stx-boundary 5))
        (14
         #s(stx-boundary
            (s0
             (((s1)
               (s2
                (s3)
                (s4
                 s5
                 #t
                 s3
                 ()
                 s6
                 #f
                 ((s7 s8) (s9 (s10 s3 s8)))
                 (s7 (s11 s3 (s12 (s8))))))))
             ()
             5)))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1)
               (s2
                (s3)
                (s4
                 s5
                 #t
                 s3
                 ()
                 s6
                 #f
                 ((s7 s8) (s9 (s10 s3 s8)))
                 (s7 (s11 s3 (s12 (s8))))))))
             ()
             5)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1)
               (s2
                (s3)
                (s4
                 s5
                 #t
                 s3
                 ()
                 s6
                 #f
                 ((s7 s8) (s9 (s10 s3 s8)))
                 (s7 (s11 s3 (s12 (s8))))))))
             ()
             5)))
        (114 . #f)
        (19
         (#s(stx-boundary
             ((s0)
              (s1
               (s2)
               (s3
                s4
                #t
                s2
                ()
                s5
                #f
                ((s6 s7) (s8 (s9 s2 s7)))
                (s6 (s10 s2 (s11 (s7)))))))))
         ()
         .
         #s(stx-boundary (5)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (5)))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary ((s0 5))))
        (142 . #s(stx-boundary (s0 () (s1 5))))
        (7 . #s(stx-boundary (s0 () (s1 5))))
        (2 . #s(stx-boundary (s0 () (s1 5))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 5)))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 5)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () (s2 5))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () (s2 5))))))))
      ((let () (define (ok x) (second x)) (define (second y) 8) (ok 5))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (138 . #f)
        (0
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (21
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5)))
         .
         #s(stx-boundary (s6 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (9
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (0
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) 8)))
        (0 . #s(stx-boundary (s0 (s1 s2) 8)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) 8)))
        (21 . #s(stx-boundary (s0 (s1 s2) 8)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (127 . #s(stx-boundary (s0 5)))
        (14
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) (s3 s2))))
          #s(stx-boundary ((s3) (s1 (s4) 8))))
         .
         #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) 8)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (10 . #s(stx-boundary (8)))
        (24 #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (3 . #f)
        (126 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (12 . #s(stx-boundary (8)))
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary ((s0 s1 (s2 5)))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 s5 s3))) ((s5) (s2 (s6) (s7 8))))
             (s4 s1 (s7 5)))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 s5 s3))) ((s5) (s2 (s6) (s7 8))))
             (s4 s1 (s7 5)))))
        (7
         .
         #s(stx-boundary
            (s0
             ()
             (s1
              (((s2) (s3 (s4) (s5 s6 s4))) ((s6) (s3 (s7) (s8 8))))
              (s5 s2 (s8 5))))))
        (2
         .
         #s(stx-boundary
            (s0
             ()
             (s1
              (((s2) (s3 (s4) (s5 s6 s4))) ((s6) (s3 (s7) (s8 8))))
              (s5 s2 (s8 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2
               (((s3) (s4 (s5) (s6 s7 s5))) ((s7) (s4 (s8) (s9 8))))
               (s6 s3 (s9 5)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2
               (((s3) (s4 (s5) (s6 s7 s5))) ((s7) (s4 (s8) (s9 8))))
               (s6 s3 (s9 5)))))))))
      ((module m racket/base (require racket/list) foldl)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) s3)))
        (148 . #s(stx-boundary (s0 (s1 s2) s3)))
        (126 . #s(stx-boundary (s0 (s1 s2) s3)))
        (0 . #s(stx-boundary (s0 (s1 s2) s3)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s3)))
        (21 . #s(stx-boundary (s0 (s1 s2) s3)))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10))
         .
         #s(stx-boundary (s11 (s8 s9) s10)))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (21
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12)))
         .
         #s(stx-boundary (s13 (s3 s4 (s5 s6) (s7 s8) (s9 #f)) (s10 s11) s12)))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (127
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (102 . #f)
        (148
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (130 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (132 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (133 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (131 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (148 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (11
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 s10)))
         #s(stx-boundary (s7 s8 s11)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (158 . #f)
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (102 . #f)
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 #f)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (148 . #s(stx-boundary (s0 #f)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f)))
        (6 . #s(stx-boundary (s0 s1 #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 #f))))
        (7 . #s(stx-boundary (s0 s1 (s2 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 #f))))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))
        (130 . #s(stx-boundary (s0 s1)))
        (132 . #s(stx-boundary (s0 s1)))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (9 . #s(stx-boundary (s0 s1)))
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (133 . #s(stx-boundary (s0 s1)))
        (131 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 (s1 s2))) . #s(stx-boundary (s3 s4 (s5 s2))))
        (9 . #s(stx-boundary (s0 (s1 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2))))
        (2 . #s(stx-boundary (s0 (s1 s2))))
        (148 . #s(stx-boundary (s0 (s1 s2))))
        (11 #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 s3 s4)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (130 . #s(stx-boundary s0))
        (132 . #s(stx-boundary s0))
        (141 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (133 . #s(stx-boundary s0))
        (131 . #s(stx-boundary s0))
        (22 #s(stx-boundary (s0 (s1 s2))) . #s(stx-boundary (s3 s1 s2)))
        (9 . #s(stx-boundary (s0 (s1 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2))))
        (2 . #s(stx-boundary (s0 (s1 s2))))
        (148 . #s(stx-boundary (s0 (s1 s2))))
        (11 #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (22 #s(stx-boundary (s0 s1 (s2 () s3) s4)) . #s(stx-boundary (s5 s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (0 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (2 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (148 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 () s2) s3)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () s1)))
        (110 . #f)
        (17 #s(stx-boundary ()) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 () s1)))
        (2 . #s(stx-boundary (s0 () s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 (s1 () s2) s3)))
        (7 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (2 . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (135)
        (13 . #f)
        (3 . #f)
        (7
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s5 s9)
             (s7 s10 (s11 () s12) s13))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s5 s9)
             (s7 s10 (s11 () s12) s13))))
        (148
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))
        (7
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))
        (2
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))))
      ('quoted
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 (s1 s2))))
        (2 . #s(stx-boundary (s0 (s1 s2))))))
      ((module m '#%kernel 5)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (101 . #f)
        (157 . #f)
        (148 . #s(stx-boundary 5))
        (126 . #s(stx-boundary 5))
        (127 . #s(stx-boundary 5))
        (142 . #s(stx-boundary (s0 5)))
        (126 . #s(stx-boundary (s0 5)))
        (127 . #s(stx-boundary (s0 5)))
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 5)))
        (102 . #f)
        (148 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (148 . #s(stx-boundary (s0 5)))
        (135)
        (13 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 5)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 5))))
        (2 . #s(stx-boundary (s0 (s1 5))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))))
      ((let-values (((x) __y) ((y z) __w)) __x)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (112 . #f)
        (16
         (#s(stx-boundary ((s0) s1)) #s(stx-boundary ((s2 s3) s4)))
         .
         #s(stx-boundary (s5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (13 . #f)
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (5 . #s(stx-boundary ((s0 . s1))))
        (7
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (2
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (7
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
      ((module m racket/base
         (define-syntax (ok stx) (quote-syntax 8))
         (ok)
         (list (ok) (ok)))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (148 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (126 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (0 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (21 . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9))))
         .
         #s(stx-boundary (s13 (s8 (s9 s10) (s11 8)) (s9) (s12 (s9) (s9)))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (21
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (22
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11)))))
         .
         #s(stx-boundary
            (s15
             (s3 s4 (s5 s6) (s7 s8) (s9 #f))
             (s10 (s11 s12) (s13 8))
             (s11)
             (s14 (s11) (s11)))))
        (9
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (127
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (0
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (102 . #f)
        (148
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (130 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (132 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (133 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (131 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (22
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (9 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (0 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (148 . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (11
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 8))))
         #s(stx-boundary (s7 s8 (s10)))
         #s(stx-boundary (s7 s8 (s13 (s10) (s10)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (158 . #f)
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (101 . #f)
        (157 . #f)
        (142 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (102 . #f)
        (148 . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (148 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 #f)))
        (100 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (148 . #s(stx-boundary (s0 #f)))
        (135)
        (13 . #f)
        (3 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #f)))
        (6 . #s(stx-boundary (s0 s1 #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 #f))))
        (7 . #s(stx-boundary (s0 s1 (s2 #f))))
        (2 . #s(stx-boundary (s0 s1 (s2 #f))))
        (135)
        (13 . #f)
        (3 . #f)
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (148 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (130 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (132 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (133 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (131 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (22
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8)))))
         .
         #s(stx-boundary (s6 s7 (s8 (s2 s4) (s5 8)))))
        (9 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (0 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (2 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (148 . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (11
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         #s(stx-boundary (s5 s6 (s1)))
         #s(stx-boundary (s5 s6 (s7 (s1) (s1)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (100 . #f)
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (148 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (103 . #f)
        (157 . #f)
        (20 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 8))))
        (21 . #s(stx-boundary (s0 (s1) (s2 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 8))))
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define (ident x) x)
         9)
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9))
         .
         #s(stx-boundary (s8 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 8))))
        (21 . #s(stx-boundary (s0 (s1) (s2 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 8))))
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary 9))
        (127 . #s(stx-boundary 9))
        (14
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         (#s(stx-boundary ((s4) (s5 (s6) s6))))
         .
         #s(stx-boundary (9)))
        (157 . #f)
        (13 . #f)
        (113 . #f)
        (16 (#s(stx-boundary ((s0) (s1 (s2) s2)))) . #s(stx-boundary (9)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (13 . #f)
        (4 . #s(stx-boundary (9)))
        (3 . #f)
        (0 . #s(stx-boundary 9))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 9)))
        (6 . #s(stx-boundary (s0 . 9)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 9)))
        (2 . #s(stx-boundary (s0 9)))
        (5 . #s(stx-boundary ((s0 9))))
        (142 . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (7 . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (2 . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (7 . #s(stx-boundary (s0 () (s0 (((s1) (s2 (s3) s3))) (s4 9)))))
        (2 . #s(stx-boundary (s0 () (s0 (((s1) (s2 (s3) s3))) (s4 9)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 (((s2) (s3 (s4) s4))) (s5 9))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 (((s2) (s3 (s4) s4))) (s5 9))))))))
      ((set! __x 99)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2 99))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2 99))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 s1 99)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 99)))
        (123 . #f)
        (1 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 99))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 99)))
        (6 . #s(stx-boundary (s0 . 99)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 99)))
        (2 . #s(stx-boundary (s0 99)))
        (7 . #s(stx-boundary (s0 s1 (s2 99))))
        (2 . #s(stx-boundary (s0 s1 (s2 99))))
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 99)))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 99)))))))
      ((let ()
         (define-syntax (lift stx) (syntax-local-lift-expression #'(+ 1 2)))
         (lift))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2)))
         .
         #s(stx-boundary (s7 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2))))))
         .
         #s(stx-boundary (s7 (s1 s3) (s4 (s5 (s6 1 2))))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 1 2)))))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2)))))
         .
         #s(stx-boundary (s5 (s1) (s2 (s3 (s4 1 2))))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 (s3 1 2))))))
        (10 . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (24
         #s(stx-boundary ((s0 (s1 (s2 1 2)))))
         .
         #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (127 . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (12 . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (4 . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 1 2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 1 2))))
        (21 . #s(stx-boundary (s0 (s1 1 2))))
        (153 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (154 . #f)
        (22 #s(stx-boundary (s0 (s1 1 2))) . #s(stx-boundary (s2 (s1 1 2))))
        (9 . #s(stx-boundary (s0 (s1 1 2))))
        (0 . #s(stx-boundary (s0 (s1 1 2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 1 2))))
        (118 . #f)
        (7 . #s(stx-boundary (s0 (s1 1 2))))
        (2 . #s(stx-boundary (s0 (s1 1 2))))
        (5 . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (7 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (5 . #s(stx-boundary ((s0 s1 (s2 (s3 1 2))))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 (s5 1 2))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 (s5 1 2))))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0)))
        (0 . #s(stx-boundary (s0)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0)))
        (21 . #s(stx-boundary (s0)))
        (129 (#s(stx-boundary s0)) . #s(stx-boundary (s1 1 2)))
        (22 #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (9 . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (14 #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (0 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 1 2)))))))
         ()
         .
         #s(stx-boundary (s6)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (142 . #s(stx-boundary (s0 () s1)))
        (7 . #s(stx-boundary (s0 () s1)))
        (2 . #s(stx-boundary (s0 () s1)))
        (7 . #s(stx-boundary (s0 () (s0 () s1))))
        (2 . #s(stx-boundary (s0 () (s0 () s1))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (128
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (0 . #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (107 . #f)
        (4 . #s(stx-boundary ((s0 (s1) (s2 1 2)) (s3 (s4 () (s4 () s1))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (104 . #f)
        (0 . #s(stx-boundary (s0 1 2)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 1 2)))
        (8 . #s(stx-boundary (s0 s1 1 2)))
        (21 . #s(stx-boundary (s0 s1 1 2)))
        (22 #s(stx-boundary (s0 s1 1 2)) . #s(stx-boundary (s0 s1 1 2)))
        (9 . #s(stx-boundary (s0 s1 1 2)))
        (0 . #s(stx-boundary (s0 s1 1 2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 1 2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 1 2)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 1))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 1)))
        (6 . #s(stx-boundary (s0 . 1)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 1)))
        (2 . #s(stx-boundary (s0 1)))
        (3 . #f)
        (0 . #s(stx-boundary 2))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 2)))
        (6 . #s(stx-boundary (s0 . 2)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 2)))
        (2 . #s(stx-boundary (s0 2)))
        (5 . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (7 . #s(stx-boundary (s0 s1 (s2 1) (s2 2))))
        (2 . #s(stx-boundary (s0 s1 (s2 1) (s2 2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 1) (s4 2)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 1) (s4 2)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s0 () s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s0 () s1))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 () s1))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 () s1))))
        (24 #s(stx-boundary ((s0 () s1))) . #s(stx-boundary ((s0 () s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 () s1)))
        (127 . #s(stx-boundary (s0 () s1)))
        (12 . #s(stx-boundary ((s0 () s1))))
        (4 . #s(stx-boundary ((s0 () s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () s1)))
        (112 . #f)
        (16 () . #s(stx-boundary (s0)))
        (13 . #f)
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 () s1)))
        (2 . #s(stx-boundary (s0 () s1)))
        (5 . #s(stx-boundary ((s0 () s1))))
        (7 . #s(stx-boundary (s0 () (s0 () s1))))
        (2 . #s(stx-boundary (s0 () (s0 () s1))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (5
         .
         #s(stx-boundary
            ((s0 (s1) (s2 s3 (s4 1) (s4 2))) (s5 (s6 () (s6 () s1))))))
        (7
         .
         #s(stx-boundary
            (s0 (s1 (s2) (s3 s4 (s5 1) (s5 2))) (s6 (s7 () (s7 () s2))))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 (s2) (s3 s4 (s5 1) (s5 2))) (s6 (s7 () (s7 () s2))))))))
      ((let () (define (ok x) '8) (ok 5))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (127 . #s(stx-boundary (s0 5)))
        (14 #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (0 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         .
         #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary ((s0 s1 (s2 5)))))
        (7 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5)))))
        (2 . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5)))))
        (7
         .
         #s(stx-boundary
            (s0 () (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5))))))
        (2
         .
         #s(stx-boundary
            (s0 () (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5))))))
        (7
         .
         #s(stx-boundary
            (s0 (s1 () (s1 (((s2) (s3 (s4) (s5 8)))) (s6 s2 (s5 5)))))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 () (s1 (((s2) (s3 (s4) (s5 8)))) (s6 s2 (s5 5)))))))))
      ((begin0 '3 '5)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (108 . #f)
        (3 . #f)
        (0 . #s(stx-boundary (s0 3)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 3)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 3)))
        (2 . #s(stx-boundary (s0 3)))
        (3 . #f)
        (4 . #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 5)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary ((s0 5))))
        (7 . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (2 . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (7 . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))))
      ((case-lambda ((x) x) ((x y) (+ x y)))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (111 . #f)
        (3 . #f)
        (18 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (3 . #f)
        (18 #s(stx-boundary (s0 s1)) . #s(stx-boundary ((s2 s0 s1))))
        (10 . #s(stx-boundary ((s0 s1 s2))))
        (24 #s(stx-boundary ((s0 s1 s2))) . #s(stx-boundary ((s0 s1 s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1 s2)))
        (127 . #s(stx-boundary (s0 s1 s2)))
        (12 . #s(stx-boundary ((s0 s1 s2))))
        (4 . #s(stx-boundary ((s0 s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2 s3)))
        (8 . #s(stx-boundary (s0 s1 s2 s3)))
        (21 . #s(stx-boundary (s0 s1 s2 s3)))
        (22 #s(stx-boundary (s0 s1 s2 s3)) . #s(stx-boundary (s0 s1 s2 s3)))
        (9 . #s(stx-boundary (s0 s1 s2 s3)))
        (0 . #s(stx-boundary (s0 s1 s2 s3)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 s3)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1 s2)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1 s2)))
        (7 . #s(stx-boundary (s0 s1 s2 s3)))
        (2 . #s(stx-boundary (s0 s1 s2 s3)))
        (5 . #s(stx-boundary ((s0 s1 s2 s3))))
        (7 . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s4 s1 s2)))))
        (2 . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s4 s1 s2)))))
        (7 . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s5 s2 s3))))))
        (2 . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s5 s2 s3))))))))
      ((let ()
         (define-syntax (ok stx)
           (define-values
            (exp opaque)
            (syntax-local-expand-expression (cadr (syntax-e stx))))
           opaque)
         (#%expression (ok 9)))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (21
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (22
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9))))
         .
         #s(stx-boundary
            (s11
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (9
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
         .
         #s(stx-boundary (s10 (s1 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
        (9
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (2
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (127
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (103 . #f)
        (148
         .
         #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4))
         .
         #s(stx-boundary (s8 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (110 . #f)
        (17
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 s3) (s4 (s5 (s6 s0)))) s3)))
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2)))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2)))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0 s1) (s2 (s3 (s4 s5))))))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (14 #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (0 . #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0 s1) (s2 (s3 (s4 s5))))))
         .
         #s(stx-boundary (s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 s3))))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))
        (22
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary (s0 (s1 s2 s3))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (5 . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)))))
        (13 . #f)
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (((s1 s2) (s3 s4 (s3 s5 (s3 s6 s7))))) s2)))
        (2 . #s(stx-boundary (s0 (((s1 s2) (s3 s4 (s3 s5 (s3 s6 s7))))) s2)))
        (7
         .
         #s(stx-boundary
            (s0 (s1) (s2 (((s3 s4) (s5 s6 (s5 s7 (s5 s8 s1))))) s4))))
        (2
         .
         #s(stx-boundary
            (s0 (s1) (s2 (((s3 s4) (s5 s6 (s5 s7 (s5 s8 s1))))) s4))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 9))))
        (127 . #s(stx-boundary (s0 (s1 9))))
        (14
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))))
         ()
         .
         #s(stx-boundary ((s9 (s0 9)))))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary ((s0 (s1 9)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 9))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 9))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 9)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 9)))
        (21 . #s(stx-boundary (s0 9)))
        (130 . #s(stx-boundary 9))
        (132 . #s(stx-boundary 9))
        (141 . #f)
        (0 . #s(stx-boundary 9))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 9)))
        (6 . #s(stx-boundary (s0 . 9)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 9)))
        (2 . #s(stx-boundary (s0 9)))
        (133 . #s(stx-boundary (s0 9)))
        (146 . #s(stx-boundary #:opaque))
        (131 . #s(stx-boundary (s0 9)))
        (22 #s(stx-boundary #:opaque) . #s(stx-boundary (s0 9)))
        (9 . #s(stx-boundary #:opaque))
        (0 . #s(stx-boundary #:opaque))
        (146 . #s(stx-boundary (s0 9)))
        (142 . #s(stx-boundary (s0 9)))
        (7 . #s(stx-boundary (s0 9)))
        (2 . #s(stx-boundary (s0 9)))
        (5 . #s(stx-boundary ((s0 9))))
        (142 . #s(stx-boundary (s0 () (s1 9))))
        (7 . #s(stx-boundary (s0 () (s1 9))))
        (2 . #s(stx-boundary (s0 () (s1 9))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((let ()
         (define (first z) z)
         (define (ok x) (second x))
         (printf "extra expression\n")
         (define (second y) 8)
         (ok (first 5)))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s8 #:opaque)
              (s2 (s7 s9) 8)
              (s5 (s3 5))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s8 #:opaque)
              (s2 (s7 s9) 8)
              (s5 (s3 5))))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (21
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (22
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5))))
         .
         #s(stx-boundary
            (s9
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (9
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5)))))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5)))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 #:opaque)))
        (127 . #s(stx-boundary (s0 #:opaque)))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) 8)))
        (0 . #s(stx-boundary (s0 (s1 s2) 8)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) 8)))
        (21 . #s(stx-boundary (s0 (s1 s2) 8)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 5))))
        (127 . #s(stx-boundary (s0 (s1 5))))
        (14
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary (() (s6 (s7 #:opaque) (s8))))
          #s(stx-boundary ((s5) (s1 (s9) 8))))
         .
         #s(stx-boundary ((s3 (s0 5)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (107 . #f)
        (4 . #s(stx-boundary ((s0 #:opaque) (s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 #:opaque)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 #:opaque)))
        (8 . #s(stx-boundary (s0 s1 #:opaque)))
        (21 . #s(stx-boundary (s0 s1 #:opaque)))
        (22
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s0 s1 #:opaque)))
        (9 . #s(stx-boundary (s0 s1 #:opaque)))
        (0 . #s(stx-boundary (s0 s1 #:opaque)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 #:opaque)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 #:opaque)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary #:opaque))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #:opaque)))
        (6 . #s(stx-boundary (s0 . #:opaque)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #:opaque)))
        (2 . #s(stx-boundary (s0 #:opaque)))
        (5 . #s(stx-boundary (s0 (s1 #:opaque))))
        (7 . #s(stx-boundary (s0 s1 (s2 #:opaque))))
        (2 . #s(stx-boundary (s0 s1 (s2 #:opaque))))
        (3 . #f)
        (0 . #s(stx-boundary (s0)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1)))
        (6 . #s(stx-boundary (s0 s1)))
        (109 . #f)
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (5 . #s(stx-boundary ((s0 s1 (s2 #:opaque)) (s0 s3))))
        (7 . #s(stx-boundary (s0 (s1 s2 (s3 #:opaque)) (s1 s4))))
        (2 . #s(stx-boundary (s0 (s1 s2 (s3 #:opaque)) (s1 s4))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) 8)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (10 . #s(stx-boundary (8)))
        (24 #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (3 . #f)
        (126 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (12 . #s(stx-boundary (8)))
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 (s1 5)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 5))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 5))))
        (8 . #s(stx-boundary (s0 s1 (s2 5))))
        (21 . #s(stx-boundary (s0 s1 (s2 5))))
        (22 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s0 s1 (s2 5))))
        (9 . #s(stx-boundary (s0 s1 (s2 5))))
        (0 . #s(stx-boundary (s0 s1 (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 5))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 5))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (5 . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s4
              (((s5) (s2 (s6) (s7 s8 s6)))
               (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
               ((s8) (s2 (s13) (s11 8))))
              (s7 s5 (s7 s1 (s11 5)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s4
              (((s5) (s2 (s6) (s7 s8 s6)))
               (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
               ((s8) (s2 (s13) (s11 8))))
              (s7 s5 (s7 s1 (s11 5)))))))
        (7
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s4
               (((s5) (s2 (s6) (s7 s8 s6)))
                (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
                ((s8) (s2 (s13) (s11 8))))
               (s7 s5 (s7 s1 (s11 5))))))))
        (2
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s4
               (((s5) (s2 (s6) (s7 s8 s6)))
                (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
                ((s8) (s2 (s13) (s11 8))))
               (s7 s5 (s7 s1 (s11 5))))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s5
                (((s6) (s3 (s7) (s8 s9 s7)))
                 (() (s10 (s8 s11 (s12 #:opaque)) (s8 s13)))
                 ((s9) (s3 (s14) (s12 8))))
                (s8 s6 (s8 s2 (s12 5)))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s5
                (((s6) (s3 (s7) (s8 s9 s7)))
                 (() (s10 (s8 s11 (s12 #:opaque)) (s8 s13)))
                 ((s9) (s3 (s14) (s12 8))))
                (s8 s6 (s8 s2 (s12 5)))))))))))
      ((#%variable-reference __z)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (149 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 (s1 s2))))
        (2 . #s(stx-boundary (s0 (s1 s2))))))
      ((let ()
         (define-syntax (lift stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (lift))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (21 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (22
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2)))
         .
         #s(stx-boundary (s9 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (9 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (0 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 s7))))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 s3) (s4 s5)))))
        (10 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 s4))))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (127 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (12 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (4 . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (8 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (22
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (9 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (0 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1)))
        (21 . #s(stx-boundary (s0 s1)))
        (153 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (154 . #f)
        (22 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (9 . #s(stx-boundary (s0 s1)))
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (5 . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (7 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (5 . #s(stx-boundary ((s0 s1 (s2 s3) (s4 s5)))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s5) (s6 s7)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s5) (s6 s7)))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0)))
        (0 . #s(stx-boundary (s0)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0)))
        (21 . #s(stx-boundary (s0)))
        (150
         #s(stx-boundary (s0 s1))
         #s(stx-boundary s2)
         .
         #s(stx-boundary s2))
        (22 #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (9 . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (14
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (0
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 s7))))))
         ()
         .
         #s(stx-boundary (s7)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (142 . #s(stx-boundary (s0 () s1)))
        (7 . #s(stx-boundary (s0 () s1)))
        (2 . #s(stx-boundary (s0 () s1)))
        (7 . #s(stx-boundary (s0 () (s0 () s1))))
        (2 . #s(stx-boundary (s0 () (s0 () s1))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (128 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (107 . #f)
        (4 . #s(stx-boundary ((s0 s1) (s2 (s3 () (s3 () s4))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (119 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 () (s0 () s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () (s0 () s1))))
        (112 . #f)
        (16 () . #s(stx-boundary ((s0 () s1))))
        (13 . #f)
        (10 . #s(stx-boundary ((s0 () s1))))
        (24 #s(stx-boundary ((s0 () s1))) . #s(stx-boundary ((s0 () s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 () s1)))
        (127 . #s(stx-boundary (s0 () s1)))
        (12 . #s(stx-boundary ((s0 () s1))))
        (4 . #s(stx-boundary ((s0 () s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 () s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 () s1)))
        (112 . #f)
        (16 () . #s(stx-boundary (s0)))
        (13 . #f)
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 () s1)))
        (2 . #s(stx-boundary (s0 () s1)))
        (5 . #s(stx-boundary ((s0 () s1))))
        (7 . #s(stx-boundary (s0 () (s0 () s1))))
        (2 . #s(stx-boundary (s0 () (s0 () s1))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (5 . #s(stx-boundary ((s0 s1) (s2 (s3 () (s3 () s4))))))
        (7 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (2 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))))
      ((letrec-values (((x) __y) ((y z) __w)) __x)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) s1)) #s(stx-boundary ((s2 s3) s4)))
         .
         #s(stx-boundary (s5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (13 . #f)
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (5 . #s(stx-boundary ((s0 . s1))))
        (7
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (2
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (7
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))
        (2
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
      ((let ()
         (define-syntax (ok stx)
           (local-expand (cadr (syntax-e stx)) 'expression #f))
         (ok 9))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (21
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (22
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9)))
         .
         #s(stx-boundary
            (s9 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (9
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (13 . #f)
        (10
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 (s6 s3)) (s7 s8) #f))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (127
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f)))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (110 . #f)
        (17
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 (s3 s0)) (s4 s5) #f))))
        (10 . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (24
         #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f)))
         .
         #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (127 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (12 . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (4 . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 s2))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 s3))))
        (8 . #s(stx-boundary (s0 s1 (s2 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3))))
        (22
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 s3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 s3))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 s2))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary (s0 (s1 s2 s3))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 s1)))
        (2 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary #f))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . #f)))
        (6 . #s(stx-boundary (s0 . #f)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 #f)))
        (2 . #s(stx-boundary (s0 #f)))
        (5 . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) (s5 s6) (s5 #f))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (5 . #s(stx-boundary ((s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f)))))
        (7
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s2 s4 (s2 s5 s1)) (s6 s7) (s6 #f)))))
        (2
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s2 s4 (s2 s5 s1)) (s6 s7) (s6 #f)))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 9)))
        (0 . #s(stx-boundary (s0 9)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 9)))
        (21 . #s(stx-boundary (s0 9)))
        (130 . #s(stx-boundary 9))
        (132 . #s(stx-boundary 9))
        (126 . #s(stx-boundary 9))
        (127 . #s(stx-boundary 9))
        (133 . #s(stx-boundary 9))
        (131 . #s(stx-boundary 9))
        (22 #s(stx-boundary 9) . #s(stx-boundary (s0 9)))
        (9 . #s(stx-boundary 9))
        (2 . #s(stx-boundary 9))
        (127 . #s(stx-boundary 9))
        (14
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))))
         ()
         .
         #s(stx-boundary (9)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (9)))
        (3 . #f)
        (0 . #s(stx-boundary 9))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 9)))
        (6 . #s(stx-boundary (s0 . 9)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 9)))
        (2 . #s(stx-boundary (s0 9)))
        (5 . #s(stx-boundary ((s0 9))))
        (142 . #s(stx-boundary (s0 () (s1 9))))
        (7 . #s(stx-boundary (s0 () (s1 9))))
        (2 . #s(stx-boundary (s0 () (s1 9))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((lambda (x) (define y (+ x x)) y)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (21 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (22
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (9 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (0 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s2 (s3 s0 s0)) s2)))
        (10 . #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1)))
        (24
         #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1))
         .
         #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1)))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (22
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 s3 s3)))
         .
         #s(stx-boundary (s4 s1 (s2 s3 s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 s2 s2))))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (14 #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (0 . #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (113 . #f)
        (16 (#s(stx-boundary ((s0) (s1 s2 s2)))) . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2 s2)))
        (8 . #s(stx-boundary (s0 s1 s2 s2)))
        (21 . #s(stx-boundary (s0 s1 s2 s2)))
        (22 #s(stx-boundary (s0 s1 s2 s2)) . #s(stx-boundary (s0 s1 s2 s2)))
        (9 . #s(stx-boundary (s0 s1 s2 s2)))
        (0 . #s(stx-boundary (s0 s1 s2 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1 s1)))
        (7 . #s(stx-boundary (s0 s1 s2 s2)))
        (2 . #s(stx-boundary (s0 s1 s2 s2)))
        (13 . #f)
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (((s1) (s2 s3 s4 s4))) s1)))
        (2 . #s(stx-boundary (s0 (((s1) (s2 s3 s4 s4))) s1)))
        (7 . #s(stx-boundary (s0 (s1) (s2 (((s3) (s4 s5 s1 s1))) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (((s3) (s4 s5 s1 s1))) s3))))
        (7 . #s(stx-boundary (s0 (s1 (s2) (s3 (((s4) (s5 s6 s2 s2))) s4)))))
        (2 . #s(stx-boundary (s0 (s1 (s2) (s3 (((s4) (s5 s6 s2 s2))) s4)))))))
      ((if 1 2 3)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 1 2 3))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 1 2 3))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 1 2 3)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 1 2 3)))
        (105 . #f)
        (0 . #s(stx-boundary 1))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 1)))
        (6 . #s(stx-boundary (s0 . 1)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 1)))
        (2 . #s(stx-boundary (s0 1)))
        (3 . #f)
        (0 . #s(stx-boundary 2))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 2)))
        (6 . #s(stx-boundary (s0 . 2)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 2)))
        (2 . #s(stx-boundary (s0 2)))
        (3 . #f)
        (0 . #s(stx-boundary 3))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 3)))
        (6 . #s(stx-boundary (s0 . 3)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 3)))
        (2 . #s(stx-boundary (s0 3)))
        (7 . #s(stx-boundary (s0 (s1 1) (s1 2) (s1 3))))
        (2 . #s(stx-boundary (s0 (s1 1) (s1 2) (s1 3))))
        (7 . #s(stx-boundary (s0 (s1 (s2 1) (s2 2) (s2 3)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 1) (s2 2) (s2 3)))))))
      ((begin 1 __x (+ 3 4))
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (107 . #f)
        (4 . #s(stx-boundary (1 s0 (s1 3 4))))
        (3 . #f)
        (0 . #s(stx-boundary 1))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 1)))
        (6 . #s(stx-boundary (s0 . 1)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 1)))
        (2 . #s(stx-boundary (s0 1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . s1)))
        (6 . #s(stx-boundary (s0 . s1)))
        (116 . #f)
        (7 . #s(stx-boundary (s0 . s1)))
        (2 . #s(stx-boundary (s0 . s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 3 4)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 3 4)))
        (8 . #s(stx-boundary (s0 s1 3 4)))
        (21 . #s(stx-boundary (s0 s1 3 4)))
        (22 #s(stx-boundary (s0 s1 3 4)) . #s(stx-boundary (s0 s1 3 4)))
        (9 . #s(stx-boundary (s0 s1 3 4)))
        (0 . #s(stx-boundary (s0 s1 3 4)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 3 4)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 3 4)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 3))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 3)))
        (6 . #s(stx-boundary (s0 . 3)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 3)))
        (2 . #s(stx-boundary (s0 3)))
        (3 . #f)
        (0 . #s(stx-boundary 4))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 4)))
        (6 . #s(stx-boundary (s0 . 4)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 4)))
        (2 . #s(stx-boundary (s0 4)))
        (5 . #s(stx-boundary (s0 (s1 3) (s1 4))))
        (7 . #s(stx-boundary (s0 s1 (s2 3) (s2 4))))
        (2 . #s(stx-boundary (s0 s1 (s2 3) (s2 4))))
        (5 . #s(stx-boundary ((s0 1) (s1 . s2) (s3 s4 (s0 3) (s0 4)))))
        (7 . #s(stx-boundary (s0 (s1 1) (s2 . s3) (s4 s5 (s1 3) (s1 4)))))
        (2 . #s(stx-boundary (s0 (s1 1) (s2 . s3) (s4 s5 (s1 3) (s1 4)))))
        (7 . #s(stx-boundary (s0 (s1 (s2 1) (s3 . s4) (s5 s6 (s2 3) (s2 4))))))
        (2
         .
         #s(stx-boundary (s0 (s1 (s2 1) (s3 . s4) (s5 s6 (s2 3) (s2 4))))))))
      ((#%stratified-body
        (define (first z) z)
        (define (ok x) (second x))
        (define (second y) 8)
        (ok (first 5)))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5)))))
        (155 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5)))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) 8)))
        (0 . #s(stx-boundary (s0 (s1 s2) 8)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) 8)))
        (21 . #s(stx-boundary (s0 (s1 s2) 8)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 5))))
        (127 . #s(stx-boundary (s0 (s1 5))))
        (14
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary ((s5) (s1 (s6) 8))))
         .
         #s(stx-boundary ((s7 (s3 (s0 5))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) 8)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (10 . #s(stx-boundary (8)))
        (24 #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (3 . #f)
        (126 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (12 . #s(stx-boundary (8)))
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 (s1 (s2 5))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2 5)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 (s2 5)))))
        (155 . #f)
        (10 . #s(stx-boundary ((s0 (s1 5)))))
        (24 #s(stx-boundary ((s0 (s1 5)))) . #s(stx-boundary ((s0 (s1 5)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 5))))
        (127 . #s(stx-boundary (s0 (s1 5))))
        (12 . #s(stx-boundary ((s0 (s1 5)))))
        (4 . #s(stx-boundary ((s0 (s1 5)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 5))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 5))))
        (8 . #s(stx-boundary (s0 s1 (s2 5))))
        (21 . #s(stx-boundary (s0 s1 (s2 5))))
        (22 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s0 s1 (s2 5))))
        (9 . #s(stx-boundary (s0 s1 (s2 5))))
        (0 . #s(stx-boundary (s0 s1 (s2 5))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 5))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 5))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (5 . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (5 . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              (((s2) (s3 (s4) s4))
               ((s5) (s3 (s6) (s7 s8 s6)))
               ((s8) (s3 (s9) (s10 8))))
              (s7 s5 (s7 s2 (s10 5)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              (((s2) (s3 (s4) s4))
               ((s5) (s3 (s6) (s7 s8 s6)))
               ((s8) (s3 (s9) (s10 8))))
              (s7 s5 (s7 s2 (s10 5)))))))))
      ((let () (define (ok x) '8) (define (second y) (ok y)) (second 5))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (21
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (22
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5)))
         .
         #s(stx-boundary
            (s7 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (9
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (13 . #f)
        (10
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (127 . #s(stx-boundary (s0 5)))
        (14
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s5) (s0 s5)))))
         .
         #s(stx-boundary ((s4 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (117 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 s1))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (10 . #s(stx-boundary ((s0 s1))))
        (24 #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 s1)))
        (127 . #s(stx-boundary (s0 s1)))
        (12 . #s(stx-boundary ((s0 s1))))
        (4 . #s(stx-boundary ((s0 s1))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 s1)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 s2)))
        (8 . #s(stx-boundary (s0 s1 s2)))
        (21 . #s(stx-boundary (s0 s1 s2)))
        (22 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (9 . #s(stx-boundary (s0 s1 s2)))
        (0 . #s(stx-boundary (s0 s1 s2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 s2)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 s1)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0 s1)))
        (7 . #s(stx-boundary (s0 s1 s2)))
        (2 . #s(stx-boundary (s0 s1 s2)))
        (5 . #s(stx-boundary ((s0 s1 s2))))
        (7 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (2 . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 5))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 5)))
        (8 . #s(stx-boundary (s0 s1 5)))
        (21 . #s(stx-boundary (s0 s1 5)))
        (22 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (9 . #s(stx-boundary (s0 s1 5)))
        (0 . #s(stx-boundary (s0 s1 5)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 5)))
        (109 . #f)
        (4 . #s(stx-boundary (s0 5)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary 5))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 5)))
        (6 . #s(stx-boundary (s0 . 5)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 5)))
        (2 . #s(stx-boundary (s0 5)))
        (5 . #s(stx-boundary (s0 (s1 5))))
        (7 . #s(stx-boundary (s0 s1 (s2 5))))
        (2 . #s(stx-boundary (s0 s1 (s2 5))))
        (5 . #s(stx-boundary ((s0 s1 (s2 5)))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))))
             (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))))
             (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5))))))
        (7
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) (s4 8))))
              (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5)))))))
        (2
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) (s4 8))))
              (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5)))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) (s5 8))))
               (s1 (((s6) (s3 (s7) (s8 s2 s7)))) (s8 s6 (s5 5))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) (s5 8))))
               (s1 (((s6) (s3 (s7) (s8 s2 s7)))) (s8 s6 (s5 5))))))))))
      ((#%plain-app 1 2)
       .
       ((141 . #f)
        (0 . #s(stx-boundary (s0 (s1 1 2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 1 2))))
        (138 . #f)
        (0 . #s(stx-boundary (s0 1 2)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 1 2)))
        (109 . #f)
        (4 . #s(stx-boundary (1 2)))
        (3 . #f)
        (0 . #s(stx-boundary 1))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 1)))
        (6 . #s(stx-boundary (s0 . 1)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 1)))
        (2 . #s(stx-boundary (s0 1)))
        (3 . #f)
        (0 . #s(stx-boundary 2))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 2)))
        (6 . #s(stx-boundary (s0 . 2)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 2)))
        (2 . #s(stx-boundary (s0 2)))
        (5 . #s(stx-boundary ((s0 1) (s0 2))))
        (7 . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (2 . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (7 . #s(stx-boundary (s0 (s1 (s2 1) (s2 2)))))
        (2 . #s(stx-boundary (s0 (s1 (s2 1) (s2 2)))))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define-syntax (second stx) (quote-syntax (ok 6)))
         (second 5))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (21
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (22
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5)))
         .
         #s(stx-boundary
            (s6 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (9
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (0
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (24
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5)))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 8))))
        (21 . #s(stx-boundary (s0 (s1) (s2 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 8))))
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 6))))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 6)))))
        (10 . #s(stx-boundary ((s0 (s1 6)))))
        (24 #s(stx-boundary ((s0 (s1 6)))) . #s(stx-boundary ((s0 (s1 6)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 6))))
        (127 . #s(stx-boundary (s0 (s1 6))))
        (12 . #s(stx-boundary ((s0 (s1 6)))))
        (4 . #s(stx-boundary ((s0 (s1 6)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 6))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 6))))
        (118 . #f)
        (7 . #s(stx-boundary (s0 (s1 6))))
        (2 . #s(stx-boundary (s0 (s1 6))))
        (5 . #s(stx-boundary ((s0 (s1 6)))))
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 5)))
        (0 . #s(stx-boundary (s0 5)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 5)))
        (21 . #s(stx-boundary (s0 5)))
        (22 #s(stx-boundary (s0 6)) . #s(stx-boundary (s1 5)))
        (9 . #s(stx-boundary (s0 6)))
        (2 . #s(stx-boundary (s0 6)))
        (0 . #s(stx-boundary (s0 6)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 6)))
        (21 . #s(stx-boundary (s0 6)))
        (22 #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (9 . #s(stx-boundary 8))
        (2 . #s(stx-boundary 8))
        (127 . #s(stx-boundary 8))
        (14
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (0
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s2) (s3 (s0 6))))))
         ()
         .
         #s(stx-boundary (8)))
        (157 . #f)
        (13 . #f)
        (4 . #s(stx-boundary (8)))
        (3 . #f)
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (142 . #s(stx-boundary (s0 () (s1 8))))
        (7 . #s(stx-boundary (s0 () (s1 8))))
        (2 . #s(stx-boundary (s0 () (s1 8))))
        (7 . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (2 . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (7 . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))
        (2 . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define-syntax (second stx) (quote-syntax (ok 6)))
         (define (ident x) x)
         (define (second-ident y) y)
         (ident (second-ident (second))))
       .
       ((141 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 8))
              (s2 (s6 s4) (s5 (s3 6)))
              (s7 (s8 s9) s9)
              (s7 (s10 s11) s11)
              (s8 (s10 (s6)))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 8))
              (s2 (s6 s4) (s5 (s3 6)))
              (s7 (s8 s9) s9)
              (s7 (s10 s11) s11)
              (s8 (s10 (s6)))))))
        (138 . #f)
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (1 . #s(stx-boundary s0))
        (8
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (21
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (22
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5)))))
         .
         #s(stx-boundary
            (s11
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (9
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (0
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (112 . #f)
        (16
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4))))))
        (13 . #f)
        (10
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4))))))
        (24
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4)))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4))))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 8))))
        (21 . #s(stx-boundary (s0 (s1) (s2 8))))
        (22
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (9 . #s(stx-boundary (s0 (s1) (s2 8))))
        (0 . #s(stx-boundary (s0 (s1) (s2 8))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 8))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (10 . #s(stx-boundary ((s0 8))))
        (24 #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 8)))
        (127 . #s(stx-boundary (s0 8)))
        (12 . #s(stx-boundary ((s0 8))))
        (4 . #s(stx-boundary ((s0 8))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 8)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 8)))
        (118 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary ((s0 8))))
        (7 . #s(stx-boundary (s0 (s1) (s2 8))))
        (2 . #s(stx-boundary (s0 (s1) (s2 8))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (0 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (21 . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (103 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 6))))))
        (157 . #f)
        (144 . #f)
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (21 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (0 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 6)))))
        (10 . #s(stx-boundary ((s0 (s1 6)))))
        (24 #s(stx-boundary ((s0 (s1 6)))) . #s(stx-boundary ((s0 (s1 6)))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 6))))
        (127 . #s(stx-boundary (s0 (s1 6))))
        (12 . #s(stx-boundary ((s0 (s1 6)))))
        (4 . #s(stx-boundary ((s0 (s1 6)))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 6))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1 6))))
        (118 . #f)
        (7 . #s(stx-boundary (s0 (s1 6))))
        (2 . #s(stx-boundary (s0 (s1 6))))
        (5 . #s(stx-boundary ((s0 (s1 6)))))
        (7 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (3 . #f)
        (145 . #f)
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 s2) s2)))
        (0 . #s(stx-boundary (s0 (s1 s2) s2)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 (s1 s2) s2)))
        (21 . #s(stx-boundary (s0 (s1 s2) s2)))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (22
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (9 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (2 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (127 . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (104 . #f)
        (148 . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (3 . #f)
        (126 . #s(stx-boundary (s0 (s1 (s2)))))
        (127 . #s(stx-boundary (s0 (s1 (s2)))))
        (14
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (0
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (1 . #s(stx-boundary s0))
        (6
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (114 . #f)
        (19
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s2) (s3 (s0 6))))))
         (#s(stx-boundary ((s5) (s6 (s7) s7)))
          #s(stx-boundary ((s8) (s6 (s9) s9))))
         .
         #s(stx-boundary ((s5 (s8 (s4))))))
        (157 . #f)
        (13 . #f)
        (113 . #f)
        (16
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) s4))))
         .
         #s(stx-boundary ((s0 (s3 (s5))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1) s1)))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 (s1) s1)))
        (110 . #f)
        (17 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (10 . #s(stx-boundary (s0)))
        (24 #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (3 . #f)
        (126 . #s(stx-boundary s0))
        (127 . #s(stx-boundary s0))
        (12 . #s(stx-boundary (s0)))
        (4 . #s(stx-boundary (s0)))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (5 . #s(stx-boundary (s0)))
        (7 . #s(stx-boundary (s0 (s1) s1)))
        (2 . #s(stx-boundary (s0 (s1) s1)))
        (13 . #f)
        (4 . #s(stx-boundary ((s0 (s1 (s2))))))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1 (s2)))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (8 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (21 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (22
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (9 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (0 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1 (s2)))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0 (s1))))
        (1 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 s1 (s2))))
        (8 . #s(stx-boundary (s0 s1 (s2))))
        (21 . #s(stx-boundary (s0 s1 (s2))))
        (22 #s(stx-boundary (s0 s1 (s2))) . #s(stx-boundary (s0 s1 (s2))))
        (9 . #s(stx-boundary (s0 s1 (s2))))
        (0 . #s(stx-boundary (s0 s1 (s2))))
        (1 . #s(stx-boundary s0))
        (6 . #s(stx-boundary (s0 s1 (s2))))
        (109 . #f)
        (4 . #s(stx-boundary (s0 (s1))))
        (3 . #f)
        (0 . #s(stx-boundary s0))
        (1 . #s(stx-boundary s0))
        (125 #s(stx-boundary s0) . #s(stx-boundary s0))
        (2 . #s(stx-boundary s0))
        (3 . #f)
        (0 . #s(stx-boundary (s0)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0)))
        (21 . #s(stx-boundary (s0)))
        (22 #s(stx-boundary (s0 6)) . #s(stx-boundary (s1)))
        (9 . #s(stx-boundary (s0 6)))
        (0 . #s(stx-boundary (s0 6)))
        (1 . #s(stx-boundary s0))
        (8 . #s(stx-boundary (s0 6)))
        (21 . #s(stx-boundary (s0 6)))
        (22 #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (9 . #s(stx-boundary 8))
        (0 . #s(stx-boundary 8))
        (1 . #s(stx-boundary s0))
        (142 . #s(stx-boundary (s0 . 8)))
        (6 . #s(stx-boundary (s0 . 8)))
        (115 . #f)
        (7 . #s(stx-boundary (s0 8)))
        (2 . #s(stx-boundary (s0 8)))
        (5 . #s(stx-boundary (s0 (s1 8))))
        (7 . #s(stx-boundary (s0 s1 (s2 8))))
        (2 . #s(stx-boundary (s0 s1 (s2 8))))
        (5 . #s(stx-boundary (s0 (s1 s2 (s3 8)))))
        (7 . #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (2 . #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (5 . #s(stx-boundary ((s0 s1 (s0 s2 (s3 8))))))
        (142
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (7
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (2
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (7
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8))))))))
        (2
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8))))))))
        (7
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s1 (((s5) (s3 (s6) s6))) (s7 s2 (s7 s5 (s8 8)))))))))
        (2
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s1 (((s5) (s3 (s6) s6))) (s7 s2 (s7 s5 (s8 8))))))))))))
