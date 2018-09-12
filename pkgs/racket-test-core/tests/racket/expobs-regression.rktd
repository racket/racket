#hash((__x
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (exit-prim . #s(stx-boundary (s0 (s1 . s2))))
        (return . #s(stx-boundary (s0 (s1 . s2))))))
      ((#%stratified-body
        (define (first z) z)
        (define (ok x) (second x))
        (define (second y) 8)
        (ok (first 5))
        (define more 'oops))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (enter-check
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))
              (s2 s9 (s10 s11))))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5))
             (s1 s8 (s9 s10)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5))
             (s1 s8 (s9 s10)))))
        (prim-#%stratified . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s0 (s5 s6) 8)
             (s3 (s1 5))
             (s0 s7 (s8 s9)))))
        (block-renames
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
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) 8)))
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 5))))
        (exit-check . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5)) (s9 s10 (s11 s12))))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary ((s5) (s1 (s6) 8))))
         .
         #s(stx-boundary ((s7 (s3 (s0 5)) (s8 s9 (s10 s11))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 s1 s2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (enter-block . #s(stx-boundary (8)))
        (block-renames #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (next . #f)
        (enter-check . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->list . #s(stx-boundary (8)))
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 (s1 (s2 5)) (s3 s4 (s5 s6))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (prim-#%stratified . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5))))
         .
         #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 5))))
        (exit-check . #s(stx-boundary (s0 (s1 5))))
        (block->list . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (enter-list . #s(stx-boundary ((s0 (s1 5)) (s2 s3 (s4 s5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 5))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))))
      ((quote-syntax (stx-quoted))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (enter-check . #s(stx-boundary (s0 (s1 (s2)))))
        (exit-check . #s(stx-boundary (s0 (s1 (s2)))))
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 (s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1))))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 (s1))))
        (return . #s(stx-boundary (s0 (s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2)))))
        (return . #s(stx-boundary (s0 (s1 (s2)))))))
      ((module m racket/base
         (define-syntax (ok stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (ok))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (enter-check
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (exit-check
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (rename-one
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (macro-post-x
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9)))
         .
         #s(stx-boundary (s15 (s8 (s9 s10) (s11 (s3 s12) (s13 s14))) (s9))))
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (prim-module-begin . #f)
        (rename-one
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (enter-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-pre . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (rename-one . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 (s2 s13) (s14 s15)))))
         #s(stx-boundary (s7 s8 (s10))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (rename-one . #s(stx-boundary (s0 #f)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 #f))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 #f))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (enter-local . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (local-pre . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (start . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (local-post
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (exit-local
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9))))))
         .
         #s(stx-boundary (s10 s11 (s12 (s2 s4) (s5 (s6 s7) (s8 s9))))))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (visit
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (prim-stop . #f)
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (return
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (rename-one
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         #s(stx-boundary (s9 s10 (s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (rename-one
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-define-syntaxes . #f)
        (prepare-env . #f)
        (phase-up . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 s3) (s4 s5)))))
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 s4))))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block->list . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (enter-list . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))))
      ((module m racket/base (define (proc x) x) (provide proc))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (enter-check . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (exit-check . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (enter-check . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (macro-post-x
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9)))
         .
         #s(stx-boundary (s12 (s8 (s9 s10) s10) (s11 s9))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (prim-module-begin . #f)
        (rename-one
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (enter-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-pre . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (rename-one . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) s11)))
         #s(stx-boundary (s7 s8 (s12 s10))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (rename-one . #s(stx-boundary (s0 #f)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 #f))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 #f))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (enter-local . #s(stx-boundary (s0 (s1 s2) s2)))
        (local-pre . #s(stx-boundary (s0 (s1 s2) s2)))
        (start . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (local-post . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-local . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4))))
         .
         #s(stx-boundary (s5 s6 (s7 (s2 s4) s4))))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (return . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         #s(stx-boundary (s4 s5 (s6 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3))))
         .
         #s(stx-boundary (s4 s3)))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (return . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (local-post . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (exit-local . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 (s3 s4)))))
         .
         #s(stx-boundary (s5 s6 (s7 s4))))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (visit . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (return . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2 (s3 s4))))))
        (splice #s(stx-boundary (s0 (s1 (s2 s3)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (return . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (next . #f)
        (module-lift-end-loop)
        (next-group . #f)
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (prim-provide . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s9 (s10) (s11 (s12) s12))
             (s13 s10))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s9 (s10) (s11 (s12) s12))
             (s13 s10))))
        (rename-one
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (enter-check . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (exit-check . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (prim-module . #f)
        (prepare-env . #f)
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (tag . #s(stx-boundary (s0 (s1 s2))))
        (enter-check . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8)))
         .
         #s(stx-boundary (s9 (s3 s8))))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (return
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (macro-post-x
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10))))
         .
         #s(stx-boundary (s11 (s3 s4 (s5 s6) (s7 s8) (s9 #f)) (s5 s10))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (prim-module-begin . #f)
        (rename-one
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (enter-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-pre . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (rename-one . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s2 s9))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (rename-one . #s(stx-boundary (s0 #f)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 #f))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 #f))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (local-post . #s(stx-boundary (s0 s1)))
        (exit-local . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3))))
         .
         #s(stx-boundary (s4 s1 (s2 s3))))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (return . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (splice #s(stx-boundary (s0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5))
         .
         #s(stx-boundary (s6 (s3 s4))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (visit . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (return . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (rename-one . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 () (s2 s3)) s4)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 s2))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary ()) . #s(stx-boundary ((s0 s1))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-list . #s(stx-boundary ((s0 s1))))
        (exit-prim . #s(stx-boundary (s0 () (s1 s2))))
        (return . #s(stx-boundary (s0 () (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 (s1 () (s2 s3)) s4)))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (return . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s7 s9 (s10 () (s3 s11)) s12))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s7 s9 (s10 () (s3 s11)) s12))))
        (rename-one
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (exit-check . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (exit-macro . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 5)))
        (exit-macro . #s(stx-boundary 8))
        (return . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->letrec #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (visit . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) () 8)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         ()
         .
         #s(stx-boundary (8)))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (tag . #s(stx-boundary (s0 () (s1 8))))
        (exit-prim . #s(stx-boundary (s0 () (s1 8))))
        (return . #s(stx-boundary (s0 () (s1 8))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((with-continuation-mark __x __y __z)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (enter-check . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (exit-check . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (visit . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 s1 s2 s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s3)))
        (prim-with-continuation-mark . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (exit-prim . #s(stx-boundary (s0 (s1 . s2) (s1 . s3) (s1 . s4))))
        (return . #s(stx-boundary (s0 (s1 . s2) (s1 . s3) (s1 . s4))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 . s3) (s2 . s4) (s2 . s5)))))
        (return . #s(stx-boundary (s0 (s1 (s2 . s3) (s2 . s4) (s2 . s5)))))))
      ((#%top . __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 . s2))))
        (enter-check . #s(stx-boundary (s0 (s1 . s2))))
        (exit-check . #s(stx-boundary (s0 (s1 . s2))))
        (visit . #s(stx-boundary (s0 (s1 . s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 . s2))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 . s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (exit-prim . #s(stx-boundary (s0 (s1 . s2))))
        (return . #s(stx-boundary (s0 (s1 . s2))))))
      ((let () (define-syntax-rule (ok x) x) (ok 5))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (enter-check . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (exit-check . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5)))
         .
         #s(stx-boundary (s4 () (s1 (s2 s3) s3) (s2 5))))
        (exit-macro . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) s2) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) s2) (s1 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
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
        (exit-macro
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
        (return
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (macro-pre-x
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
        (macro-post-x
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
        (exit-macro
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
        (return
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
        (exit-check
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
        (prim-define-syntaxes . #f)
        (rename-one
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
        (prepare-env . #f)
        (enter-bind . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-lambda . #f)
        (lambda-renames
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
        (enter-block
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
        (block-renames
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
        (next . #f)
        (enter-check
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (macro-pre-x
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
        (macro-post-x
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
        (exit-macro
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
        (return
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (macro-pre-x
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
        (macro-post-x
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
        (exit-macro
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
        (return
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
        (exit-check
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
        (block->list
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
        (enter-list
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-let-values . #f)
        (let-renames
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
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next-group . #f)
        (enter-block
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
        (block-renames
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
        (next . #f)
        (enter-check
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (macro-pre-x
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
        (macro-post-x
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
        (exit-macro
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
        (return
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
        (exit-check
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
        (block->list
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
        (enter-list
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-let-values . #f)
        (let-renames
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (tag
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
        (enter-prim
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
        (prim-#%app . #f)
        (enter-list
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-lambda . #f)
        (lambda-renames
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
        (enter-block
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
        (block-renames
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
        (next . #f)
        (enter-check
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
        (exit-check
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
        (block->list
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
        (enter-list
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-if . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             ((s1 (s2) s3) (s4 s2))
             ((s1
               (s2)
               (s0 (s5 s2) (s6 ((s7 (s4 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
              (s10 s2))
             #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             ((s1 (s2) s3) (s4 s2))
             ((s1
               (s2)
               (s0 (s5 s2) (s6 ((s7 (s4 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
              (s10 s2))
             #f)))
        (prim-if . #f)
        (visit . #s(stx-boundary ((s0 (s1) s2) (s3 s1))))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary ((s0 (s1) s2) (s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s2)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s1)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s2)))
        (return . #s(stx-boundary (s0 (s1) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 (s1) s2) (s3 s4 s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2) s3) (s0 s4 s2))))
        (return . #s(stx-boundary (s0 (s1 (s2) s3) (s0 s4 s2))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))
             (s9 s1))))
        (resolve . #s(stx-boundary s0))
        (tag
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2))))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2))))
        (prim-#%app . #f)
        (enter-list
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))
             (s9 s1))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary
            ((s1 (s2 s0) (s3 ((s4 (s5 s0))) (s6 s4 (s7 (s8 s0)) s4)) #f))))
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (block-renames
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (next . #f)
        (enter-check
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (block->list
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (enter-list
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (prim-if . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (macro-post-x
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1)))
         .
         #s(stx-boundary (s7 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (exit-macro
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (prim-let-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 s2))))
         .
         #s(stx-boundary ((s3 s0 (s4 (s5 s2)) s0))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1))))
        (block-renames
         #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1)))
         .
         #s(stx-boundary ((s0 s1 (s2 (s3 s4)) s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f))
         .
         #s(stx-boundary (s2 s1 (s3 (s4 s5)) s1)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (return . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (exit-check . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (block->list . #s(stx-boundary ((s0 s1 (s2 (s3 (s4 s5)) s1) #f))))
        (enter-list . #s(stx-boundary ((s0 s1 (s2 (s3 (s4 s5)) s1) #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (prim-if . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f))
         .
         #s(stx-boundary (s4 (s1 (s2 s3)) s5)))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (prim-if . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary (s0 (s1 s2 s3))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (tag . #s(stx-boundary s0))
        (exit-prim . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) s5 (s6 #f))))
        (return . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) s5 (s6 #f))))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (return
         .
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (exit-list
         .
         #s(stx-boundary ((s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 s3 s4)))
             (s5 s1 (s5 (s2 s6 (s2 s7 s4)) s1 (s8 #f)) (s8 #f)))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 s3 s4)))
             (s5 s1 (s5 (s2 s6 (s2 s7 s4)) s1 (s8 #f)) (s8 #f)))))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (exit-list
         .
         #s(stx-boundary
            ((s0
              (s1 s2 s3)
              (s4
               (((s5) (s1 s6 s3)))
               (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
              (s9 #f)))))
        (exit-prim
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
        (return
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
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list
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
        (exit-prim
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
        (return
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
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-prim
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
        (return
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
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-prim
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
        (return
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
        (exit-list
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
        (exit-prim
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
        (return
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
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list
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
        (exit-prim
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
        (return
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
        (next-group . #f)
        (enter-block
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
        (block-renames
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
        (next . #f)
        (enter-check
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
        (exit-check
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
        (block->list
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
        (enter-list
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
        (next . #f)
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-if . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (macro-post-x
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4)))))
         .
         #s(stx-boundary
            (s10 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (prim-let-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) s1)))
         .
         #s(stx-boundary ((s2 (((s3) (s4 0 (s5 s0)))) () (s6 (s7 s8 s3))))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (block-renames
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (next . #f)
        (enter-check
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (exit-check
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (block->list
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (enter-list
         .
         #s(stx-boundary ((s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1))))))
        (next . #f)
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 0 (s2 s3)))))
         ()
         .
         #s(stx-boundary ((s4 (s5 s6 s0)))))
        (prepare-env . #f)
        (next . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (enter-prim . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 0)))
        (enter-prim . #s(stx-boundary (s0 . 0)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 0)))
        (return . #s(stx-boundary (s0 0)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-list . #s(stx-boundary (s0 (s1 0) (s2 s3))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 0) (s3 s4))))
        (return . #s(stx-boundary (s0 s1 (s2 0) (s3 s4))))
        (next . #f)
        (exit-bind . #f)
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2 s3))))
         .
         #s(stx-boundary ((s0 (s1 s2 s3)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2 s3))))
        (exit-check . #s(stx-boundary (s0 (s1 s2 s3))))
        (block->list . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (enter-list . #s(stx-boundary ((s0 (s1 s2 s3)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 s3))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #t)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #t)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (macro-post-x
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s0 () (s6 s7))))
         .
         #s(stx-boundary (s4 s5 s8)))
        (exit-macro
         .
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s0 () (s6 s7)))))
        (visit . #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s0 () (s6 s7)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s0 () (s6 s7)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s0 () (s6 s7)))))
        (macro-post-x
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 () (s7 s8))))
         .
         #s(stx-boundary (s6 ((s1 (s2 (s3 s4) s5))) (s6 () (s7 s8)))))
        (exit-macro
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 () (s7 s8)))))
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 () (s7 s8)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 () (s7 s8)))))
        (prim-let-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2 s3) s4))))
         .
         #s(stx-boundary ((s5 () (s6 s7)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s3)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2) s3)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 (s1 s2) s3)))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (return . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 () (s1 s2)))))
        (block-renames
         #s(stx-boundary ((s0 () (s1 s2))))
         .
         #s(stx-boundary ((s0 () (s1 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 () (s1 s2))))
        (visit . #s(stx-boundary (s0 () (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 () (s1 s2))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 s2))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 s2)))
         .
         #s(stx-boundary (s3 () (s1 s2))))
        (exit-macro . #s(stx-boundary (s0 () (s1 s2))))
        (return . #s(stx-boundary (s0 () (s1 s2))))
        (exit-check . #s(stx-boundary (s0 () (s1 s2))))
        (block->list . #s(stx-boundary ((s0 () (s1 s2)))))
        (enter-list . #s(stx-boundary ((s0 () (s1 s2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 s2))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 s1))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (tag . #s(stx-boundary s0))
        (exit-prim . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (exit-list . #s(stx-boundary ((s0 () s1))))
        (exit-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 s3 (s4 s5) s6))) (s0 () s7))))
        (return . #s(stx-boundary (s0 (((s1) (s2 s3 (s4 s5) s6))) (s0 () s7))))
        (exit-list
         .
         #s(stx-boundary (s0 (s1 (((s2) (s3 s4 (s5 s6) s7))) (s1 () s8)))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 (((s3) (s0 s4 (s5 s6) s7))) (s2 () s8)))))
        (return
         .
         #s(stx-boundary (s0 s1 (s2 (((s3) (s0 s4 (s5 s6) s7))) (s2 () s8)))))
        (exit-list
         .
         #s(stx-boundary
            ((s0 s1 (s2 (((s3) (s0 s4 (s5 s6) s7))) (s2 () s8))))))
        (tag
         .
         #s(stx-boundary
            (s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) (s0 () s8))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) (s0 () s8))))))
        (return
         .
         #s(stx-boundary
            (s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) (s0 () s8))))))
        (exit-list
         .
         #s(stx-boundary
            ((s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) (s0 () s8)))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s0 () (s3 s4 (s0 (((s5) (s3 s6 (s7 s8) s9))) (s0 () s1)))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s0 () (s3 s4 (s0 (((s5) (s3 s6 (s7 s8) s9))) (s0 () s1)))))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5)))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (prim-let-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) ((s1 (s2) s3) s4))))
         .
         #s(stx-boundary
            ((s5
              s0
              (s6 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s4)))))
        (next . #f)
        (visit . #s(stx-boundary ((s0 (s1) s2) s3)))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary ((s0 (s1) s2) s3)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s2)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s1)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s2)))
        (return . #s(stx-boundary (s0 (s1) s2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary ((s0 (s1) s2) s3)))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (return . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (block-renames
         #s(stx-boundary
            ((s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (next . #f)
        (enter-check
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (exit-check
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (block->list
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (enter-list
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s3 () () (s4 s5 (s6 (s7)))))
              (s8 #f #:opaque s9)))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (prim-if . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5))))))
         .
         #s(stx-boundary (s6 () (s1 () () (s2 s3 (s4 (s5)))))))
        (exit-macro . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (visit . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (block-renames
         #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4))))))
         .
         #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (exit-check . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (block->list . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (enter-list . #s(stx-boundary ((s0 () () (s1 s2 (s3 (s4)))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames () () . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (prepare-env . #f)
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (block-renames
         #s(stx-boundary ((s0 s1 (s2 (s3)))))
         .
         #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (exit-check . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (block->list . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (enter-list . #s(stx-boundary ((s0 s1 (s2 (s3))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1))))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 (s1))))
        (return . #s(stx-boundary (s0 (s1))))
        (exit-list . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (exit-prim . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (return . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (exit-list . #s(stx-boundary ((s0 s1 s2 (s3 (s4))))))
        (tag . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (exit-prim . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (return . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (exit-list . #s(stx-boundary ((s0 () (s1 s2 s3 (s4 (s5)))))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 s2 s3 (s4 (s5)))))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 s2 s3 (s4 (s5)))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f #:opaque s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (enter-prim . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f #:opaque s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary #:opaque))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #:opaque)))
        (enter-prim . #s(stx-boundary (s0 . #:opaque)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #:opaque)))
        (return . #s(stx-boundary (s0 #:opaque)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 (s1 #f) (s1 #:opaque) s2)))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f) (s2 #:opaque) s3)))
        (return . #s(stx-boundary (s0 s1 (s2 #f) (s2 #:opaque) s3)))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (return
         .
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (exit-list
         .
         #s(stx-boundary
            ((s0
              s1
              (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
              (s3 s8 (s6 #f) (s6 #:opaque) s9)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3 (s4) s5) s6)))
             (s7
              s1
              (s0 () (s0 () (s2 s8 s9 (s10 (s11)))))
              (s2 s12 (s10 #f) (s10 #:opaque) s6)))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3 (s4) s5) s6)))
             (s7
              s1
              (s0 () (s0 () (s2 s8 s9 (s10 (s11)))))
              (s2 s12 (s10 #f) (s10 #:opaque) s6)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (((s3) s1))
              (s2 () (s4 s5 (s2 (((s6) (s4 s7 (s8 s9) s10))) (s2 () s3)))))
             (s2
              (((s1) (s4 (s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s2 () (s4 s15 s10 (s8 (s16)))))
               (s4 s17 (s8 #f) (s8 #:opaque) s14))))))
        (return
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (((s3) s1))
              (s2 () (s4 s5 (s2 (((s6) (s4 s7 (s8 s9) s10))) (s2 () s3)))))
             (s2
              (((s1) (s4 (s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s2 () (s4 s15 s10 (s8 (s16)))))
               (s4 s17 (s8 #f) (s8 #:opaque) s14))))))
        (exit-list
         .
         #s(stx-boundary
            ((s0
              s1
              (s2
               (((s3) s1))
               (s2 () (s4 s5 (s2 (((s6) (s4 s7 (s8 s9) s10))) (s2 () s3)))))
              (s2
               (((s1) (s4 (s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s2 () (s4 s15 s10 (s8 (s16)))))
                (s4 s17 (s8 #f) (s8 #:opaque) s14)))))))
        (exit-prim
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
              (s0
               (((s14) s1))
               (s0
                ()
                (s2 s15 (s0 (((s16) (s2 s17 (s12 s18) s19))) (s0 () s14)))))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s20 s19 (s12 (s21)))))
                (s2 s22 (s12 #f) (s12 #:opaque) s13)))))))
        (return
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
              (s0
               (((s14) s1))
               (s0
                ()
                (s2 s15 (s0 (((s16) (s2 s17 (s12 s18) s19))) (s0 () s14)))))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s20 s19 (s12 (s21)))))
                (s2 s22 (s12 #f) (s12 #:opaque) s13)))))))
        (exit-list
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
               (s0
                (((s14) s1))
                (s0
                 ()
                 (s2 s15 (s0 (((s16) (s2 s17 (s12 s18) s19))) (s0 () s14)))))
               (s0
                (((s1) (s2 (s3 (s4) s7) s13)))
                (s5
                 s1
                 (s0 () (s0 () (s2 s20 s19 (s12 (s21)))))
                 (s2 s22 (s12 #f) (s12 #:opaque) s13))))))))
        (exit-prim
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
               (s0
                (((s15) s3))
                (s0
                 ()
                 (s4 s16 (s0 (((s17) (s4 s18 (s14 s19) s2))) (s0 () s15)))))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s20 s2 (s14 (s21)))))
                 (s4 s22 (s14 #f) (s14 #:opaque) s1))))))))
        (return
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
               (s0
                (((s15) s3))
                (s0
                 ()
                 (s4 s16 (s0 (((s17) (s4 s18 (s14 s19) s2))) (s0 () s15)))))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s20 s2 (s14 (s21)))))
                 (s4 s22 (s14 #f) (s14 #:opaque) s1))))))))
        (exit-list
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
                (s0
                 (((s15) s3))
                 (s0
                  ()
                  (s4 s16 (s0 (((s17) (s4 s18 (s14 s19) s2))) (s0 () s15)))))
                (s0
                 (((s3) (s4 (s5 (s6) s9) s1)))
                 (s7
                  s3
                  (s0 () (s0 () (s4 s20 s2 (s14 (s21)))))
                  (s4 s22 (s14 #f) (s14 #:opaque) s1)))))))))
        (exit-prim
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
                (s2
                 (((s15) s4))
                 (s2
                  ()
                  (s5 s16 (s2 (((s17) (s5 s18 (s14 s19) s1))) (s2 () s15)))))
                (s2
                 (((s4) (s5 (s0 (s6) s9) s3)))
                 (s7
                  s4
                  (s2 () (s2 () (s5 s20 s1 (s14 (s21)))))
                  (s5 s22 (s14 #f) (s14 #:opaque) s3)))))))))
        (return
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
                (s2
                 (((s15) s4))
                 (s2
                  ()
                  (s5 s16 (s2 (((s17) (s5 s18 (s14 s19) s1))) (s2 () s15)))))
                (s2
                 (((s4) (s5 (s0 (s6) s9) s3)))
                 (s7
                  s4
                  (s2 () (s2 () (s5 s20 s1 (s14 (s21)))))
                  (s5 s22 (s14 #f) (s14 #:opaque) s3)))))))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary 5) . #s(stx-boundary (s0 5)))
        (exit-macro . #s(stx-boundary 5))
        (return . #s(stx-boundary 5))
        (exit-check . #s(stx-boundary 5))
        (block->letrec
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
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
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (5)))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary ((s0 5))))
        (tag . #s(stx-boundary (s0 () (s1 5))))
        (exit-prim . #s(stx-boundary (s0 () (s1 5))))
        (return . #s(stx-boundary (s0 () (s1 5))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 5)))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 5)))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () (s2 5))))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () (s2 5))))))))
      ((let () (define (ok x) (second x)) (define (second y) 8) (ok 5))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (enter-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5)))
         .
         #s(stx-boundary (s6 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (exit-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s2)) (s0 (s3 s4) 8) (s1 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) 8)))
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (exit-check . #s(stx-boundary (s0 5)))
        (block->letrec
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 s3))) ((s4) (s2 (s5) 8))) (s1 5))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 s2))))
          #s(stx-boundary ((s3) (s1 (s4) 8))))
         .
         #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 s1 s2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (enter-block . #s(stx-boundary (8)))
        (block-renames #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (next . #f)
        (enter-check . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->list . #s(stx-boundary (8)))
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 5)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 s5 s3))) ((s5) (s2 (s6) (s7 8))))
             (s4 s1 (s7 5)))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 s5 s3))) ((s5) (s2 (s6) (s7 8))))
             (s4 s1 (s7 5)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s1
              (((s2) (s3 (s4) (s5 s6 s4))) ((s6) (s3 (s7) (s8 8))))
              (s5 s2 (s8 5))))))
        (return
         .
         #s(stx-boundary
            (s0
             ()
             (s1
              (((s2) (s3 (s4) (s5 s6 s4))) ((s6) (s3 (s7) (s8 8))))
              (s5 s2 (s8 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2
               (((s3) (s4 (s5) (s6 s7 s5))) ((s7) (s4 (s8) (s9 8))))
               (s6 s3 (s9 5)))))))
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (enter-check . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (exit-check . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) s3)))
        (rename-one . #s(stx-boundary (s0 (s1 s2) s3)))
        (enter-check . #s(stx-boundary (s0 (s1 s2) s3)))
        (visit . #s(stx-boundary (s0 (s1 s2) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s3)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s3)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10))
         .
         #s(stx-boundary (s11 (s8 s9) s10)))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (return
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (visit
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (macro-post-x
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12)))
         .
         #s(stx-boundary (s13 (s3 s4 (s5 s6) (s7 s8) (s9 #f)) (s10 s11) s12)))
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (prim-module-begin . #f)
        (rename-one
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (enter-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-pre . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (rename-one . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 s10)))
         #s(stx-boundary (s7 s8 s11)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (rename-one . #s(stx-boundary (s0 #f)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 #f))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 #f))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (local-post . #s(stx-boundary (s0 s1)))
        (exit-local . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s3 s4 (s5 s2))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2))))
        (return . #s(stx-boundary (s0 (s1 s2))))
        (rename-one . #s(stx-boundary (s0 (s1 s2))))
        (splice #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 s3 s4)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (enter-local . #s(stx-boundary s0))
        (local-pre . #s(stx-boundary s0))
        (start . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (local-post . #s(stx-boundary s0))
        (exit-local . #s(stx-boundary s0))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s3 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2))))
        (return . #s(stx-boundary (s0 (s1 s2))))
        (rename-one . #s(stx-boundary (s0 (s1 s2))))
        (splice #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 () s3) s4))
         .
         #s(stx-boundary (s5 s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (visit . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (return . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (rename-one . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 () s2) s3)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary ()) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 (s1 () s2) s3)))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (return . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s5 s9)
             (s7 s10 (s11 () s12) s13))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s0 (s5 s6) (s7 s8 (s3 #f))))
             (s5 s9)
             (s7 s10 (s11 () s12) s13))))
        (rename-one
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (enter-check . #s(stx-boundary (s0 (s1 s2))))
        (exit-check . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 (s1 s2))))
        (return . #s(stx-boundary (s0 (s1 s2))))))
      ((let ()
         (define-syntax (lift stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (lift))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2)))
         .
         #s(stx-boundary (s9 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (exit-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 s5) (s6 s7))) (s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 s7))))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 s3) (s4 s5)))))
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 s4))))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block->list . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (enter-list . #s(stx-boundary ((s0 (s1 s2) (s3 s4)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-list . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 s3) (s4 s5)))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s5) (s6 s7)))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s5) (s6 s7)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0)))
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (lift-require
         #s(stx-boundary (s0 s1))
         #s(stx-boundary s2)
         .
         #s(stx-boundary s2))
        (macro-post-x #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (exit-macro . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->letrec
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))) () s8)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 s7))))))
         ()
         .
         #s(stx-boundary (s7)))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (tag . #s(stx-boundary (s0 () s1)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (exit-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (lift-loop . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (prim-begin . #f)
        (enter-list . #s(stx-boundary ((s0 s1) (s2 (s3 () (s3 () s4))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s0 () s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 () s1))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 () s1))))
        (block-renames
         #s(stx-boundary ((s0 () s1)))
         .
         #s(stx-boundary ((s0 () s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 () s1)))
        (exit-check . #s(stx-boundary (s0 () s1)))
        (block->list . #s(stx-boundary ((s0 () s1))))
        (enter-list . #s(stx-boundary ((s0 () s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary (s0)))
        (next-group . #f)
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (exit-list . #s(stx-boundary ((s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (exit-list . #s(stx-boundary ((s0 s1) (s2 (s3 () (s3 () s4))))))
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))))
      ((module m '#%kernel 5)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (enter-check . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (exit-check . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (visit . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (prim-module . #f)
        (prepare-env . #f)
        (rename-one . #s(stx-boundary 5))
        (enter-check . #s(stx-boundary 5))
        (exit-check . #s(stx-boundary 5))
        (tag . #s(stx-boundary (s0 5)))
        (enter-check . #s(stx-boundary (s0 5)))
        (exit-check . #s(stx-boundary (s0 5)))
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (rename-one . #s(stx-boundary (s0 5)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 5))))
        (return . #s(stx-boundary (s0 (s1 5))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))))
      ((let-values (((x) __y) ((y z) __w)) __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (enter-check . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (exit-check . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (prim-let-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) s1)) #s(stx-boundary ((s2 s3) s4)))
         .
         #s(stx-boundary (s5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next-group . #f)
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (exit-list . #s(stx-boundary ((s0 . s1))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (return
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
      ((module m racket/base
         (define-syntax (ok stx) (quote-syntax 8))
         (ok)
         (list (ok) (ok)))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (enter-check
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (exit-check
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (rename-one
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (macro-post-x
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9))))
         .
         #s(stx-boundary (s13 (s8 (s9 s10) (s11 8)) (s9) (s12 (s9) (s9)))))
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9)))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (prim-module-begin . #f)
        (rename-one
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f)))))
        (enter-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-pre . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (rename-one . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 8))))
         #s(stx-boundary (s7 s8 (s10)))
         #s(stx-boundary (s7 s8 (s13 (s10) (s10)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #f)
        (prepare-env . #f)
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (rename-one . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (rename-one . #s(stx-boundary (s0 #f)))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 #f))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #f))))
        (return . #s(stx-boundary (s0 s1 (s2 #f))))
        (module-lift-end-loop)
        (next-group . #f)
        (next . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (enter-local . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (local-pre . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (start . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (local-post . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-local . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8)))))
         .
         #s(stx-boundary (s6 s7 (s8 (s2 s4) (s5 8)))))
        (exit-macro . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (return . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (rename-one . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         #s(stx-boundary (s5 s6 (s1)))
         #s(stx-boundary (s5 s6 (s7 (s1) (s1)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-stop . #f)
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #f)
        (prepare-env . #f)
        (phase-up . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define (ident x) x)
         9)
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (macro-pre-x
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9))
         .
         #s(stx-boundary (s8 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (exit-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s4 (s5 s6) s6) 9)))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary 9))
        (exit-check . #s(stx-boundary 9))
        (block->letrec
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8)))) (((s5) (s6 (s7) s7))) 9)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         (#s(stx-boundary ((s4) (s5 (s6) s6))))
         .
         #s(stx-boundary (9)))
        (prepare-env . #f)
        (next-group . #f)
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) s2))))
         .
         #s(stx-boundary (9)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next-group . #f)
        (enter-list . #s(stx-boundary (9)))
        (next . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 9)))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 9)))
        (return . #s(stx-boundary (s0 9)))
        (exit-list . #s(stx-boundary ((s0 9))))
        (tag . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (exit-prim . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (return . #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (exit-prim
         .
         #s(stx-boundary (s0 () (s0 (((s1) (s2 (s3) s3))) (s4 9)))))
        (return . #s(stx-boundary (s0 () (s0 (((s1) (s2 (s3) s3))) (s4 9)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 () (s1 (((s2) (s3 (s4) s4))) (s5 9))))))
        (return
         .
         #s(stx-boundary (s0 (s1 () (s1 (((s2) (s3 (s4) s4))) (s5 9))))))))
      ((set! __x 99)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 99))))
        (enter-check . #s(stx-boundary (s0 (s1 s2 99))))
        (exit-check . #s(stx-boundary (s0 (s1 s2 99))))
        (visit . #s(stx-boundary (s0 (s1 s2 99))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 99))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 s1 99)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 99)))
        (prim-set! . #f)
        (resolve . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 99))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 99)))
        (enter-prim . #s(stx-boundary (s0 . 99)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 99)))
        (return . #s(stx-boundary (s0 99)))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 99))))
        (return . #s(stx-boundary (s0 s1 (s2 99))))
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 99)))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 99)))))))
      ((let ()
         (define-syntax (lift stx) (syntax-local-lift-expression #'(+ 1 2)))
         (lift))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2)))
         .
         #s(stx-boundary (s7 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (exit-macro
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 1 2)))) (s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2))))))
         .
         #s(stx-boundary (s7 (s1 s3) (s4 (s5 (s6 1 2))))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 1 2)))))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2)))))
         .
         #s(stx-boundary (s5 (s1) (s2 (s3 (s4 1 2))))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 (s3 1 2))))))
        (enter-block . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (block-renames
         #s(stx-boundary ((s0 (s1 (s2 1 2)))))
         .
         #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (exit-check . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (block->list . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (enter-list . #s(stx-boundary ((s0 (s1 (s2 1 2))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 1 2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 1 2))))
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (macro-post-x
         #s(stx-boundary (s0 (s1 1 2)))
         .
         #s(stx-boundary (s2 (s1 1 2))))
        (exit-macro . #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2))))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 1 2))))
        (return . #s(stx-boundary (s0 (s1 1 2))))
        (exit-list . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 (s3 1 2))))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 (s4 (s5 1 2))))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 (s5 1 2))))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0)))
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (lift-expr (#s(stx-boundary s0)) . #s(stx-boundary (s1 1 2)))
        (macro-post-x #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (exit-macro . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->letrec
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 (s5 (s6 1 2)))))) () s7)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 1 2)))))))
         ()
         .
         #s(stx-boundary (s6)))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (tag . #s(stx-boundary (s0 () s1)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (exit-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (lift-loop
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (visit
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (prim-begin . #f)
        (enter-list
         .
         #s(stx-boundary ((s0 (s1) (s2 1 2)) (s3 (s4 () (s4 () s1))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (prim-define-values . #f)
        (visit . #s(stx-boundary (s0 1 2)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 1 2)))
        (enter-macro . #s(stx-boundary (s0 s1 1 2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 1 2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 1 2))
         .
         #s(stx-boundary (s0 s1 1 2)))
        (exit-macro . #s(stx-boundary (s0 s1 1 2)))
        (visit . #s(stx-boundary (s0 s1 1 2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 1 2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 1 2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 1)))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 1)))
        (return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 2)))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 2)))
        (return . #s(stx-boundary (s0 2)))
        (exit-list . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 1) (s2 2))))
        (return . #s(stx-boundary (s0 s1 (s2 1) (s2 2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 (s4 1) (s4 2)))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 1) (s4 2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s0 () s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 () s1))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 () s1))))
        (block-renames
         #s(stx-boundary ((s0 () s1)))
         .
         #s(stx-boundary ((s0 () s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 () s1)))
        (exit-check . #s(stx-boundary (s0 () s1)))
        (block->list . #s(stx-boundary ((s0 () s1))))
        (enter-list . #s(stx-boundary ((s0 () s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary (s0)))
        (next-group . #f)
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 () s1)))
        (return . #s(stx-boundary (s0 () s1)))
        (exit-list . #s(stx-boundary ((s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (exit-list
         .
         #s(stx-boundary
            ((s0 (s1) (s2 s3 (s4 1) (s4 2))) (s5 (s6 () (s6 () s1))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (s1 (s2) (s3 s4 (s5 1) (s5 2))) (s6 (s7 () (s7 () s2))))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 (s2) (s3 s4 (s5 1) (s5 2))) (s6 (s7 () (s7 () s2))))))))
      ((let () (define (ok x) '8) (ok 5))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (exit-check . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (exit-macro . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (prim-let-values . #f)
        (let-renames () . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (next-group . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s1 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (exit-check . #s(stx-boundary (s0 5)))
        (block->letrec #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (visit . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s1 5))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
         .
         #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 5)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5)))))
        (return
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 () (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5))))))
        (return
         .
         #s(stx-boundary
            (s0 () (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (s1 () (s1 (((s2) (s3 (s4) (s5 8)))) (s6 s2 (s5 5)))))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 () (s1 (((s2) (s3 (s4) (s5 8)))) (s6 s2 (s5 5)))))))))
      ((begin0 '3 '5)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (enter-check . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (exit-check . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (prim-begin0 . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 3)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 3)))
        (return . #s(stx-boundary (s0 3)))
        (next . #f)
        (enter-list . #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary ((s0 5))))
        (exit-prim . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (return . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (return . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))))
      ((case-lambda ((x) x) ((x y) (+ x y)))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (enter-check
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (visit . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (prim-case-lambda . #f)
        (next . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (next . #f)
        (lambda-renames
         #s(stx-boundary (s0 s1))
         .
         #s(stx-boundary ((s2 s0 s1))))
        (enter-block . #s(stx-boundary ((s0 s1 s2))))
        (block-renames
         #s(stx-boundary ((s0 s1 s2)))
         .
         #s(stx-boundary ((s0 s1 s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1 s2)))
        (exit-check . #s(stx-boundary (s0 s1 s2)))
        (block->list . #s(stx-boundary ((s0 s1 s2))))
        (enter-list . #s(stx-boundary ((s0 s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2 s3)))
        (enter-macro . #s(stx-boundary (s0 s1 s2 s3)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2 s3)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2 s3))
         .
         #s(stx-boundary (s0 s1 s2 s3)))
        (exit-macro . #s(stx-boundary (s0 s1 s2 s3)))
        (visit . #s(stx-boundary (s0 s1 s2 s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s3)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1 s2)))
        (exit-prim . #s(stx-boundary (s0 s1 s2 s3)))
        (return . #s(stx-boundary (s0 s1 s2 s3)))
        (exit-list . #s(stx-boundary ((s0 s1 s2 s3))))
        (exit-prim . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s4 s1 s2)))))
        (return . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s4 s1 s2)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s5 s2 s3))))))
        (return
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s5 s2 s3))))))))
      ((let ()
         (define-syntax (ok stx)
           (local-expand (cadr (syntax-e stx)) 'expression #f))
         (ok 9))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (enter-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (macro-post-x
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9)))
         .
         #s(stx-boundary
            (s9 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)) (s1 9))))
        (next . #f)
        (enter-check
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 (s6 s3)) (s7 s8) #f))))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (return
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (exit-check
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (prim-define-syntaxes . #f)
        (rename-one
         .
         #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f)))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 (s3 s0)) (s4 s5) #f))))
        (enter-block . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (block-renames
         #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f)))
         .
         #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (exit-check . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (block->list . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (enter-list . #s(stx-boundary ((s0 (s1 (s2 s3)) (s4 s5) #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary (s0 (s1 s2 s3))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #f)))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #f)))
        (return . #s(stx-boundary (s0 #f)))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) (s5 s6) (s5 #f))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (exit-list
         .
         #s(stx-boundary ((s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s2 s4 (s2 s5 s1)) (s6 s7) (s6 #f)))))
        (return
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s2 s4 (s2 s5 s1)) (s6 s7) (s6 #f)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 9)))
        (visit . #s(stx-boundary (s0 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 9)))
        (macro-pre-x . #s(stx-boundary (s0 9)))
        (enter-local . #s(stx-boundary 9))
        (local-pre . #s(stx-boundary 9))
        (enter-check . #s(stx-boundary 9))
        (exit-check . #s(stx-boundary 9))
        (local-post . #s(stx-boundary 9))
        (exit-local . #s(stx-boundary 9))
        (macro-post-x #s(stx-boundary 9) . #s(stx-boundary (s0 9)))
        (exit-macro . #s(stx-boundary 9))
        (return . #s(stx-boundary 9))
        (exit-check . #s(stx-boundary 9))
        (block->letrec
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))) () 9)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))))
         ()
         .
         #s(stx-boundary (9)))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (9)))
        (next . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 9)))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 9)))
        (return . #s(stx-boundary (s0 9)))
        (exit-list . #s(stx-boundary ((s0 9))))
        (tag . #s(stx-boundary (s0 () (s1 9))))
        (exit-prim . #s(stx-boundary (s0 () (s1 9))))
        (return . #s(stx-boundary (s0 () (s1 9))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((let ()
         (define (first z) z)
         (define (ok x) (second x))
         (printf "extra expression\n")
         (define (second y) 8)
         (ok (first 5)))
       .
       ((start-top . #f)
        (visit
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
        (enter-check
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
        (exit-check
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5)))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2)
             (s0 (s3 s4) (s5 s4))
             (s6 #:opaque)
             (s0 (s5 s7) 8)
             (s3 (s1 5)))))
        (block-renames
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
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 #:opaque)))
        (exit-check . #s(stx-boundary (s0 #:opaque)))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) 8)))
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 5))))
        (exit-check . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s5)))
              (() (s7 (s8 #:opaque) (s9)))
              ((s6) (s2 (s10) 8)))
             (s4 (s1 5)))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary (() (s6 (s7 #:opaque) (s8))))
          #s(stx-boundary ((s5) (s1 (s9) 8))))
         .
         #s(stx-boundary ((s3 (s0 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 s1 s2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (prim-begin . #f)
        (enter-list . #s(stx-boundary ((s0 #:opaque) (s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 #:opaque)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 #:opaque)))
        (enter-macro . #s(stx-boundary (s0 s1 #:opaque)))
        (macro-pre-x . #s(stx-boundary (s0 s1 #:opaque)))
        (macro-post-x
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s0 s1 #:opaque)))
        (exit-macro . #s(stx-boundary (s0 s1 #:opaque)))
        (visit . #s(stx-boundary (s0 s1 #:opaque)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 #:opaque)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 #:opaque)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #:opaque))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . #:opaque)))
        (enter-prim . #s(stx-boundary (s0 . #:opaque)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 #:opaque)))
        (return . #s(stx-boundary (s0 #:opaque)))
        (exit-list . #s(stx-boundary (s0 (s1 #:opaque))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 #:opaque))))
        (return . #s(stx-boundary (s0 s1 (s2 #:opaque))))
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 #:opaque)) (s0 s3))))
        (exit-prim . #s(stx-boundary (s0 (s1 s2 (s3 #:opaque)) (s1 s4))))
        (return . #s(stx-boundary (s0 (s1 s2 (s3 #:opaque)) (s1 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (enter-block . #s(stx-boundary (8)))
        (block-renames #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (next . #f)
        (enter-check . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->list . #s(stx-boundary (8)))
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 (s1 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 5))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s4
              (((s5) (s2 (s6) (s7 s8 s6)))
               (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
               ((s8) (s2 (s13) (s11 8))))
              (s7 s5 (s7 s1 (s11 5)))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s4
              (((s5) (s2 (s6) (s7 s8 s6)))
               (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
               ((s8) (s2 (s13) (s11 8))))
              (s7 s5 (s7 s1 (s11 5)))))))
        (exit-prim
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
        (return
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
        (exit-prim
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
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (enter-check . #s(stx-boundary (s0 (s1 s2))))
        (exit-check . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%variable-reference . #f)
        (exit-prim . #s(stx-boundary (s0 s1)))
        (return . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 (s1 s2))))
        (return . #s(stx-boundary (s0 (s1 s2))))))
      ((let ()
         (define-syntax (ok stx)
           (define-values
            (exp opaque)
            (syntax-local-expand-expression (cadr (syntax-e stx))))
           opaque)
         (#%expression (ok 9)))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (enter-check
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (block-renames
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5) (s9 (s1 9)))))
        (next . #f)
        (enter-check
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (visit
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
         .
         #s(stx-boundary (s10 (s1 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
        (exit-macro
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (return
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (prim-define-syntaxes . #f)
        (rename-one
         .
         #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (macro-pre-x
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4))
         .
         #s(stx-boundary (s8 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (exit-macro
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 (s2 s3) (s4 (s5 (s6 s0)))) s3)))
        (enter-block . #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2)))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 (s4 (s5 s6)))) s2)))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (exit-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0 s1) (s2 (s3 (s4 s5))))))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->letrec #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (visit . #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1 s2) (s3 (s4 (s5 s6))))) s2)))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0 s1) (s2 (s3 (s4 s5))))))
         .
         #s(stx-boundary (s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary (s0 (s1 s2 s3))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)))))
        (next-group . #f)
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim
         .
         #s(stx-boundary (s0 (((s1 s2) (s3 s4 (s3 s5 (s3 s6 s7))))) s2)))
        (return
         .
         #s(stx-boundary (s0 (((s1 s2) (s3 s4 (s3 s5 (s3 s6 s7))))) s2)))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (s1) (s2 (((s3 s4) (s5 s6 (s5 s7 (s5 s8 s1))))) s4))))
        (return
         .
         #s(stx-boundary
            (s0 (s1) (s2 (((s3 s4) (s5 s6 (s5 s7 (s5 s8 s1))))) s4))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 9))))
        (exit-check . #s(stx-boundary (s0 (s1 9))))
        (block->letrec
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
             ()
             (s10 (s1 9)))))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))))
         ()
         .
         #s(stx-boundary ((s9 (s0 9)))))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 (s1 9)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 9))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 9)))
        (macro-pre-x . #s(stx-boundary (s0 9)))
        (enter-local . #s(stx-boundary 9))
        (local-pre . #s(stx-boundary 9))
        (start . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 9)))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 9)))
        (return . #s(stx-boundary (s0 9)))
        (local-post . #s(stx-boundary (s0 9)))
        (opaque-expr . #s(stx-boundary #:opaque))
        (exit-local . #s(stx-boundary (s0 9)))
        (macro-post-x #s(stx-boundary #:opaque) . #s(stx-boundary (s0 9)))
        (exit-macro . #s(stx-boundary #:opaque))
        (visit . #s(stx-boundary #:opaque))
        (opaque-expr . #s(stx-boundary (s0 9)))
        (tag . #s(stx-boundary (s0 9)))
        (exit-prim . #s(stx-boundary (s0 9)))
        (return . #s(stx-boundary (s0 9)))
        (exit-list . #s(stx-boundary ((s0 9))))
        (tag . #s(stx-boundary (s0 () (s1 9))))
        (exit-prim . #s(stx-boundary (s0 () (s1 9))))
        (return . #s(stx-boundary (s0 () (s1 9))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((letrec-values (((x) __y) ((y z) __w)) __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (enter-check . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (exit-check . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) s1)) #s(stx-boundary ((s2 s3) s4)))
         .
         #s(stx-boundary (s5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next-group . #f)
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (exit-list . #s(stx-boundary ((s0 . s1))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (return
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (exit-prim
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))
        (return
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
      ((lambda (x) (define y (+ x x)) y)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (enter-check . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (exit-check . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (visit . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (prim-lambda . #f)
        (lambda-renames
         #s(stx-boundary (s0))
         .
         #s(stx-boundary ((s1 s2 (s3 s0 s0)) s2)))
        (enter-block . #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1)))
        (block-renames
         #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1))
         .
         #s(stx-boundary ((s0 s1 (s2 s3 s3)) s1)))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (return . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 s3 s3)))
         .
         #s(stx-boundary (s4 s1 (s2 s3 s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 s2 s2))))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->letrec #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (visit . #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) (s2 s3 s3))) s1)))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 s2 s2))))
         .
         #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2 s2))
         .
         #s(stx-boundary (s0 s1 s2 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2 s2)))
        (visit . #s(stx-boundary (s0 s1 s2 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2 s2)))
        (return . #s(stx-boundary (s0 s1 s2 s2)))
        (next-group . #f)
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (((s1) (s2 s3 s4 s4))) s1)))
        (return . #s(stx-boundary (s0 (((s1) (s2 s3 s4 s4))) s1)))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (((s3) (s4 s5 s1 s1))) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (((s3) (s4 s5 s1 s1))) s3))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (((s4) (s5 s6 s2 s2))) s4)))))
        (return
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (((s4) (s5 s6 s2 s2))) s4)))))))
      ((if 1 2 3)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2 3))))
        (enter-check . #s(stx-boundary (s0 (s1 1 2 3))))
        (exit-check . #s(stx-boundary (s0 (s1 1 2 3))))
        (visit . #s(stx-boundary (s0 (s1 1 2 3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2 3))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 1 2 3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 2 3)))
        (prim-if . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 1)))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 1)))
        (return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 2)))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 2)))
        (return . #s(stx-boundary (s0 2)))
        (next . #f)
        (visit . #s(stx-boundary 3))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 3)))
        (enter-prim . #s(stx-boundary (s0 . 3)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 3)))
        (return . #s(stx-boundary (s0 3)))
        (exit-prim . #s(stx-boundary (s0 (s1 1) (s1 2) (s1 3))))
        (return . #s(stx-boundary (s0 (s1 1) (s1 2) (s1 3))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 1) (s2 2) (s2 3)))))
        (return . #s(stx-boundary (s0 (s1 (s2 1) (s2 2) (s2 3)))))))
      ((begin 1 __x (+ 3 4))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (enter-check . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (exit-check . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (visit . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (prim-begin . #f)
        (enter-list . #s(stx-boundary (1 s0 (s1 3 4))))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 1)))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 1)))
        (return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . s1)))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #f)
        (exit-prim . #s(stx-boundary (s0 . s1)))
        (return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 3 4)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 3 4)))
        (enter-macro . #s(stx-boundary (s0 s1 3 4)))
        (macro-pre-x . #s(stx-boundary (s0 s1 3 4)))
        (macro-post-x
         #s(stx-boundary (s0 s1 3 4))
         .
         #s(stx-boundary (s0 s1 3 4)))
        (exit-macro . #s(stx-boundary (s0 s1 3 4)))
        (visit . #s(stx-boundary (s0 s1 3 4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 3 4)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 3 4)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 3))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 3)))
        (enter-prim . #s(stx-boundary (s0 . 3)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 3)))
        (return . #s(stx-boundary (s0 3)))
        (next . #f)
        (visit . #s(stx-boundary 4))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 4)))
        (enter-prim . #s(stx-boundary (s0 . 4)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 4)))
        (return . #s(stx-boundary (s0 4)))
        (exit-list . #s(stx-boundary (s0 (s1 3) (s1 4))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 3) (s2 4))))
        (return . #s(stx-boundary (s0 s1 (s2 3) (s2 4))))
        (exit-list . #s(stx-boundary ((s0 1) (s1 . s2) (s3 s4 (s0 3) (s0 4)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 1) (s2 . s3) (s4 s5 (s1 3) (s1 4)))))
        (return . #s(stx-boundary (s0 (s1 1) (s2 . s3) (s4 s5 (s1 3) (s1 4)))))
        (exit-prim
         .
         #s(stx-boundary (s0 (s1 (s2 1) (s3 . s4) (s5 s6 (s2 3) (s2 4))))))
        (return
         .
         #s(stx-boundary (s0 (s1 (s2 1) (s3 . s4) (s5 s6 (s2 3) (s2 4))))))))
      ((#%stratified-body
        (define (first z) z)
        (define (ok x) (second x))
        (define (second y) 8)
        (ok (first 5)))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (enter-check
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (exit-check
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1
              (s2 (s3 s4) s4)
              (s2 (s5 s6) (s7 s6))
              (s2 (s7 s8) 8)
              (s5 (s3 5))))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5)))))
        (prim-#%stratified . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5)))))
        (block-renames
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5))))
         .
         #s(stx-boundary
            ((s0 (s1 s2) s2) (s0 (s3 s4) (s5 s4)) (s0 (s5 s6) 8) (s3 (s1 5)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) 8)))
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 5))))
        (exit-check . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)) ((s4) (s2 (s5) (s6 s5))) ((s6) (s2 (s7) 8)))
             (s8 (s4 (s1 5))))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) (s5 s4))))
          #s(stx-boundary ((s5) (s1 (s6) 8))))
         .
         #s(stx-boundary ((s7 (s3 (s0 5))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 s1 s2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (8)))
        (enter-block . #s(stx-boundary (8)))
        (block-renames #s(stx-boundary (8)) . #s(stx-boundary (8)))
        (next . #f)
        (enter-check . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->list . #s(stx-boundary (8)))
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 (s1 (s2 5))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 5)))))
        (prim-#%stratified . #f)
        (enter-block . #s(stx-boundary ((s0 (s1 5)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 5))))
         .
         #s(stx-boundary ((s0 (s1 5)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 5))))
        (exit-check . #s(stx-boundary (s0 (s1 5))))
        (block->list . #s(stx-boundary ((s0 (s1 5)))))
        (enter-list . #s(stx-boundary ((s0 (s1 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 5))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s3 5)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list . #s(stx-boundary ((s0 s1 (s0 s2 (s3 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1
              (((s2) (s3 (s4) s4))
               ((s5) (s3 (s6) (s7 s8 s6)))
               ((s8) (s3 (s9) (s10 8))))
              (s7 s5 (s7 s2 (s10 5)))))))
        (return
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
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (enter-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (macro-post-x
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5)))
         .
         #s(stx-boundary
            (s7 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5)))
         .
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s5) (s1 s5)) (s4 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 s2)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (exit-check . #s(stx-boundary (s0 5)))
        (block->letrec
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s6) (s1 s6)))) (s5 5))))
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s5) (s0 s5)))))
         .
         #s(stx-boundary ((s4 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 s0))))
        (enter-block . #s(stx-boundary ((s0 s1))))
        (block-renames #s(stx-boundary ((s0 s1))) . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 s1)))
        (exit-check . #s(stx-boundary (s0 s1)))
        (block->list . #s(stx-boundary ((s0 s1))))
        (enter-list . #s(stx-boundary ((s0 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 s2)))
        (enter-macro . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1 s2)))
        (return . #s(stx-boundary (s0 s1 s2)))
        (exit-list . #s(stx-boundary ((s0 s1 s2))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 5)))
        (enter-macro . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 5)))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 5)))
        (return . #s(stx-boundary (s0 5)))
        (exit-list . #s(stx-boundary (s0 (s1 5))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list . #s(stx-boundary ((s0 s1 (s2 5)))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))))
             (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))))
             (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) (s4 8))))
              (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5)))))))
        (return
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) (s4 8))))
              (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5)))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) (s5 8))))
               (s1 (((s6) (s3 (s7) (s8 s2 s7)))) (s8 s6 (s5 5))))))))
        (return
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
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (enter-check . #s(stx-boundary (s0 (s1 1 2))))
        (exit-check . #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2))))
        (prim-#%expression . #f)
        (visit . #s(stx-boundary (s0 1 2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 2)))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (1 2)))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 1)))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 1)))
        (return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 2)))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 2)))
        (return . #s(stx-boundary (s0 2)))
        (exit-list . #s(stx-boundary ((s0 1) (s0 2))))
        (exit-prim . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (return . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (exit-prim . #s(stx-boundary (s0 (s1 (s2 1) (s2 2)))))
        (return . #s(stx-boundary (s0 (s1 (s2 1) (s2 2)))))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define-syntax (second stx) (quote-syntax (ok 6)))
         (second 5))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (enter-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (exit-check
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (macro-post-x
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5)))
         .
         #s(stx-boundary
            (s6 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (exit-macro
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (block-renames
         #s(stx-boundary ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5)))
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8)) (s0 (s4 s2) (s3 (s1 6))) (s4 5))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 6))))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 6)))))
        (enter-block . #s(stx-boundary ((s0 (s1 6)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 6))))
         .
         #s(stx-boundary ((s0 (s1 6)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 6))))
        (exit-check . #s(stx-boundary (s0 (s1 6))))
        (block->list . #s(stx-boundary ((s0 (s1 6)))))
        (enter-list . #s(stx-boundary ((s0 (s1 6)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 6))))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 6))))
        (return . #s(stx-boundary (s0 (s1 6))))
        (exit-list . #s(stx-boundary ((s0 (s1 6)))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 5)))
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary (s0 6)) . #s(stx-boundary (s1 5)))
        (exit-macro . #s(stx-boundary (s0 6)))
        (return . #s(stx-boundary (s0 6)))
        (visit . #s(stx-boundary (s0 6)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 6)))
        (macro-pre-x . #s(stx-boundary (s0 6)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (exit-macro . #s(stx-boundary 8))
        (return . #s(stx-boundary 8))
        (exit-check . #s(stx-boundary 8))
        (block->letrec
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (visit
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0 (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6))))) () 8)))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s2) (s3 (s0 6))))))
         ()
         .
         #s(stx-boundary (8)))
        (prepare-env . #f)
        (next-group . #f)
        (enter-list . #s(stx-boundary (8)))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (tag . #s(stx-boundary (s0 () (s1 8))))
        (exit-prim . #s(stx-boundary (s0 () (s1 8))))
        (return . #s(stx-boundary (s0 () (s1 8))))
        (exit-prim . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (return . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (exit-prim . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))
        (return . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define-syntax (second stx) (quote-syntax (ok 6)))
         (define (ident x) x)
         (define (second-ident y) y)
         (ident (second-ident (second))))
       .
       ((start-top . #f)
        (visit
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
        (enter-check
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
        (exit-check
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (enter-prim
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
        (prim-#%expression . #f)
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (macro-pre-x
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (macro-post-x
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
        (exit-macro
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (visit
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (prim-let-values . #f)
        (let-renames
         ()
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4))))))
        (next-group . #f)
        (enter-block
         .
         #s(stx-boundary
            ((s0 (s1 s2) (s3 8))
             (s0 (s4 s2) (s3 (s1 6)))
             (s5 (s6 s7) s7)
             (s5 (s8 s9) s9)
             (s6 (s8 (s4))))))
        (block-renames
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
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 8)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 8))))
        (enter-block . #s(stx-boundary ((s0 8))))
        (block-renames #s(stx-boundary ((s0 8))) . #s(stx-boundary ((s0 8))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 8)))
        (exit-check . #s(stx-boundary (s0 8)))
        (block->list . #s(stx-boundary ((s0 8))))
        (enter-list . #s(stx-boundary ((s0 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary ((s0 8))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (prim-define-syntaxes . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) (s3 (s4 6))))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary ((s1 (s2 6)))))
        (enter-block . #s(stx-boundary ((s0 (s1 6)))))
        (block-renames
         #s(stx-boundary ((s0 (s1 6))))
         .
         #s(stx-boundary ((s0 (s1 6)))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 6))))
        (exit-check . #s(stx-boundary (s0 (s1 6))))
        (block->list . #s(stx-boundary ((s0 (s1 6)))))
        (enter-list . #s(stx-boundary ((s0 (s1 6)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 6))))
        (prim-quote-syntax . #f)
        (exit-prim . #s(stx-boundary (s0 (s1 6))))
        (return . #s(stx-boundary (s0 (s1 6))))
        (exit-list . #s(stx-boundary ((s0 (s1 6)))))
        (exit-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 s2) s2)))
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-check . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #f)
        (rename-one . #s(stx-boundary ((s0) (s1 (s2) s2))))
        (next . #f)
        (enter-check . #s(stx-boundary (s0 (s1 (s2)))))
        (exit-check . #s(stx-boundary (s0 (s1 (s2)))))
        (block->letrec
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (visit
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))) ((s5) (s2 (s3) (s4 (s1 6)))))
             (((s6) (s7 (s8) s8)) ((s9) (s7 (s10) s10)))
             (s6 (s9 (s5))))))
        (prim-letrec-syntaxes+values . #f)
        (letrec-syntaxes-renames
         (#s(stx-boundary ((s0) (s1 (s2) (s3 8))))
          #s(stx-boundary ((s4) (s1 (s2) (s3 (s0 6))))))
         (#s(stx-boundary ((s5) (s6 (s7) s7)))
          #s(stx-boundary ((s8) (s6 (s9) s9))))
         .
         #s(stx-boundary ((s5 (s8 (s4))))))
        (prepare-env . #f)
        (next-group . #f)
        (prim-letrec-values . #f)
        (let-renames
         (#s(stx-boundary ((s0) (s1 (s2) s2)))
          #s(stx-boundary ((s3) (s1 (s4) s4))))
         .
         #s(stx-boundary ((s0 (s3 (s5))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #f)
        (lambda-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (enter-block . #s(stx-boundary (s0)))
        (block-renames #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (next . #f)
        (enter-check . #s(stx-boundary s0))
        (exit-check . #s(stx-boundary s0))
        (block->list . #s(stx-boundary (s0)))
        (enter-list . #s(stx-boundary (s0)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list . #s(stx-boundary (s0)))
        (exit-prim . #s(stx-boundary (s0 (s1) s1)))
        (return . #s(stx-boundary (s0 (s1) s1)))
        (next-group . #f)
        (enter-list . #s(stx-boundary ((s0 (s1 (s2))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1 (s2)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 s1 (s2))))
        (enter-macro . #s(stx-boundary (s0 s1 (s2))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2)))
         .
         #s(stx-boundary (s0 s1 (s2))))
        (exit-macro . #s(stx-boundary (s0 s1 (s2))))
        (visit . #s(stx-boundary (s0 s1 (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2))))
        (prim-#%app . #f)
        (enter-list . #s(stx-boundary (s0 (s1))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (macro-post-x #s(stx-boundary (s0 6)) . #s(stx-boundary (s1)))
        (exit-macro . #s(stx-boundary (s0 6)))
        (visit . #s(stx-boundary (s0 6)))
        (resolve . #s(stx-boundary s0))
        (enter-macro . #s(stx-boundary (s0 6)))
        (macro-pre-x . #s(stx-boundary (s0 6)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (exit-macro . #s(stx-boundary 8))
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag . #s(stx-boundary (s0 . 8)))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #f)
        (exit-prim . #s(stx-boundary (s0 8)))
        (return . #s(stx-boundary (s0 8)))
        (exit-list . #s(stx-boundary (s0 (s1 8))))
        (exit-prim . #s(stx-boundary (s0 s1 (s2 8))))
        (return . #s(stx-boundary (s0 s1 (s2 8))))
        (exit-list . #s(stx-boundary (s0 (s1 s2 (s3 8)))))
        (exit-prim . #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (return . #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (exit-list . #s(stx-boundary ((s0 s1 (s0 s2 (s3 8))))))
        (tag
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8))))))))
        (return
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8))))))))
        (exit-prim
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s1 (((s5) (s3 (s6) s6))) (s7 s2 (s7 s5 (s8 8)))))))))
        (return
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s1 (((s5) (s3 (s6) s6))) (s7 s2 (s7 s5 (s8 8))))))))))))
