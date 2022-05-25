#hash(((let-values (((x) __y) ((y z) __w)) __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (prim-let-values . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)) (#s(stx-boundary s1) #s(stx-boundary s2)))
         (#s(stx-boundary s3) #s(stx-boundary s4))
         #s(stx-boundary s5))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (exit-list #s(stx-boundary (s0 . s1)))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
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
        (stop/return
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
        (prim-#%expression
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
        (prim-#%stratified
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5)))))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s0 (s5 s6) 8))
         #s(stx-boundary (s3 (s1 5))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) s2))
          #s(stx-boundary (s0 (s3 s4) (s5 s4)))
          #s(stx-boundary (s0 (s5 s6) 8))
          #s(stx-boundary (s3 (s1 5))))
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s0 (s5 s6) 8))
         #s(stx-boundary (s3 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 s2)))
         .
         #s(stx-boundary (s1 (s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) 8))
         .
         #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) 8))
         .
         #s(stx-boundary (s1 (s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)) (#s(stx-boundary s2)))
         (#s(stx-boundary (s3 (s4) s4))
          #s(stx-boundary (s3 (s5) (s2 s5)))
          #s(stx-boundary (s3 (s6) 8)))
         #s(stx-boundary (s7 (s1 (s0 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s1))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 s0)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-list #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #s(stx-boundary (s0 (s1) 8)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary 8))
        (enter-block #s(stx-boundary 8))
        (block-renames (#s(stx-boundary 8)) #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->list . #f)
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (enter-list #s(stx-boundary (s0 (s1 (s2 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 5)))))
        (prim-#%stratified . #s(stx-boundary (s0 (s1 (s2 5)))))
        (enter-block #s(stx-boundary (s0 (s1 5))))
        (block-renames
         (#s(stx-boundary (s0 (s1 5))))
         #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 5))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s1 (s2 5))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (finish-block
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3))
              ((s4) (s2 (s5) (s6 s7 s5)))
              ((s7) (s2 (s8) (s9 8))))
             (s6 s4 (s6 s1 (s9 5))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (s1
              (((s2) (s3 (s4) s4))
               ((s5) (s3 (s6) (s7 s8 s6)))
               ((s8) (s3 (s9) (s10 8))))
              (s7 s5 (s7 s2 (s10 5)))))))))
      ((module m racket/base 'done)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (prim-module . #s(stx-boundary (s0 s1 s2 (s3 s4))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)))
        (track-syntax s0 #s(stx-boundary (s1 s2)) . #s(stx-boundary (s1 s2)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (tag . #s(stx-boundary (s0 (s1 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3)))
         .
         #s(stx-boundary (s1 (s2 s3))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s0 (s1 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8)))
         .
         #s(stx-boundary (s9 (s3 s8))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8)))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s3 s8)))
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
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10))))
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f))) (s1 s2 (s5 s10)))))
        (track-syntax
         s0
         #s(stx-boundary
            (s1 (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f))) (s2 s3 (s6 s11))))
         .
         #s(stx-boundary
            (s1 (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f))) (s2 s3 (s6 s11)))))
        (next . #f)
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
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f))))
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
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s2 s9))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 #f)))
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #f)))
        (module-pass1-case . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 #f)) . #s(stx-boundary (s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f))))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (local-post . #s(stx-boundary (s0 s1)))
        (exit-local . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3))))
         .
         #s(stx-boundary (s4 s1 (s2 s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2 s3))))
         .
         #s(stx-boundary (s0 (s1 (s2 s3)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (prim-begin . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (splice #s(stx-boundary (s0 (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s0 (s1 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5))
         .
         #s(stx-boundary (s6 (s3 s4))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5))
         .
         #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (visit . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 s2))))
        (prim-lambda . #s(stx-boundary (s0 () (s1 s2))))
        (lambda-renames #s(stx-boundary ()) #s(stx-boundary (s0 s1)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-list #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 () (s1 s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 () (s3 s4)) s5)))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return
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
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s9 s11 (s12 () (s5 s13)) s14)))))))
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
        (stop/return
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
        (prim-#%expression
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
        (prim-#%stratified
         .
         #s(stx-boundary
            (s0
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s1 (s6 s7) 8)
             (s4 (s2 5))
             (s1 s8 (s9 s10)))))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s0 (s5 s6) 8))
         #s(stx-boundary (s3 (s1 5)))
         #s(stx-boundary (s0 s7 (s8 s9))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) s2))
          #s(stx-boundary (s0 (s3 s4) (s5 s4)))
          #s(stx-boundary (s0 (s5 s6) 8))
          #s(stx-boundary (s3 (s1 5)))
          #s(stx-boundary (s0 s7 (s8 s9))))
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s0 (s5 s6) 8))
         #s(stx-boundary (s3 (s1 5)))
         #s(stx-boundary (s0 s7 (s8 s9))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 s2)))
         .
         #s(stx-boundary (s1 (s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) 8))
         .
         #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) 8))
         .
         #s(stx-boundary (s1 (s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)) (#s(stx-boundary s2)))
         (#s(stx-boundary (s3 (s4) s4))
          #s(stx-boundary (s3 (s5) (s2 s5)))
          #s(stx-boundary (s3 (s6) 8)))
         #s(stx-boundary (s7 (s1 (s0 5)) (s8 s9 (s10 s11)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s1))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 s0)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-list #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #s(stx-boundary (s0 (s1) 8)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary 8))
        (enter-block #s(stx-boundary 8))
        (block-renames (#s(stx-boundary 8)) #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->list . #f)
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (enter-list #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (prim-#%stratified . #s(stx-boundary (s0 (s1 (s2 5)) (s3 s4 (s5 s6)))))
        (enter-block
         #s(stx-boundary (s0 (s1 5)))
         #s(stx-boundary (s2 s3 (s4 s5))))
        (block-renames
         (#s(stx-boundary (s0 (s1 5))) #s(stx-boundary (s2 s3 (s4 s5))))
         #s(stx-boundary (s0 (s1 5)))
         #s(stx-boundary (s2 s3 (s4 s5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 5))))
        (block->list . #f)
        (enter-list
         #s(stx-boundary (s0 (s1 5)))
         #s(stx-boundary (s2 s3 (s4 s5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s1 (s2 5))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))))
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
        (stop/return
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
        (prim-#%expression
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
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) s3)
             (s1 (s4 s5) (s6 s5))
             (s7 #:opaque)
             (s1 (s6 s8) 8)
             (s4 (s2 5)))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s6 #:opaque))
         #s(stx-boundary (s0 (s5 s7) 8))
         #s(stx-boundary (s3 (s1 5))))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s6 #:opaque))
         #s(stx-boundary (s0 (s5 s7) 8))
         #s(stx-boundary (s3 (s1 5))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) s2))
          #s(stx-boundary (s0 (s3 s4) (s5 s4)))
          #s(stx-boundary (s6 #:opaque))
          #s(stx-boundary (s0 (s5 s7) 8))
          #s(stx-boundary (s3 (s1 5))))
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s0 (s3 s4) (s5 s4)))
         #s(stx-boundary (s6 #:opaque))
         #s(stx-boundary (s0 (s5 s7) 8))
         #s(stx-boundary (s3 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 s2)))
         .
         #s(stx-boundary (s1 (s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 #:opaque)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #:opaque)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) 8))
         .
         #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) 8))
         .
         #s(stx-boundary (s1 (s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 5))))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)) () (#s(stx-boundary s2)))
         (#s(stx-boundary (s3 (s4) s4))
          #s(stx-boundary (s3 (s5) (s2 s5)))
          #s(stx-boundary (s6 (s7 #:opaque) (s8)))
          #s(stx-boundary (s3 (s9) 8)))
         #s(stx-boundary (s1 (s0 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s1))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 s0)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-list #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (prim-begin . #s(stx-boundary (s0 (s1 #:opaque) (s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 #:opaque)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s1 #:opaque)))
        (enter-macro
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s0 s1 #:opaque)))
        (macro-pre-x . #s(stx-boundary (s0 s1 #:opaque)))
        (macro-post-x
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s0 s1 #:opaque)))
        (exit-macro
         #s(stx-boundary (s0 s1 #:opaque))
         .
         #s(stx-boundary (s0 s1 #:opaque)))
        (visit . #s(stx-boundary (s0 s1 #:opaque)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 #:opaque)))
        (prim-#%app . #s(stx-boundary (s0 s1 #:opaque)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #:opaque))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #:opaque)) . #s(stx-boundary #:opaque))
        (enter-prim . #s(stx-boundary (s0 . #:opaque)))
        (prim-#%datum . #s(stx-boundary (s0 . #:opaque)))
        (exit-prim/return . #s(stx-boundary (s0 #:opaque)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #:opaque))))
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1)) . #s(stx-boundary (s1)))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%app . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 s2 (s3 #:opaque)) (s1 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #s(stx-boundary (s0 (s1) 8)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary 8))
        (enter-block #s(stx-boundary 8))
        (block-renames (#s(stx-boundary 8)) #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->list . #f)
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (enter-list #s(stx-boundary (s0 (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 5))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 5))) . #s(stx-boundary (s1 (s2 5))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 5)))
         .
         #s(stx-boundary (s0 s1 (s2 5))))
        (visit . #s(stx-boundary (s0 s1 (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 5))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 5))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (exit-list #s(stx-boundary (s0 s1 (s0 s2 (s3 5)))))
        (finish-block
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s4
              (((s5) (s2 (s6) (s7 s8 s6)))
               (() (s9 (s7 s10 (s11 #:opaque)) (s7 s12)))
               ((s8) (s2 (s13) (s11 8))))
              (s7 s5 (s7 s1 (s11 5)))))))
        (exit-prim/return
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
        (exit-prim/return
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
      ((module m racket/base
         (define-syntax (ok stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (ok))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (prim-module
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 (s7 s8) (s9 s10))) (s4))))
        (prepare-env . #f)
        (rename-one
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         #s(stx-boundary (s1)))
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))
         .
         #s(stx-boundary (s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2)))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9)))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 (s3 s12) (s13 s14)))
             (s9)))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11))))
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
        (stop/return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 (s5 s14) (s15 s16))))
             (s1 s2 (s11)))))
        (track-syntax
         s0
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) (s14 (s6 s15) (s16 s17))))
             (s2 s3 (s12))))
         .
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) (s14 (s6 s15) (s16 s17))))
             (s2 s3 (s12)))))
        (next . #f)
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
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f))))
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
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 (s2 s13) (s14 s15)))))
         #s(stx-boundary (s7 s8 (s10))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 #f)))
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #f)))
        (module-pass1-case . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 #f)) . #s(stx-boundary (s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f))))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 (s6 s7) (s8 s9)))))
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
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
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
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9))))))
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (visit
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (prim-begin
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 (s6 s7) (s8 s9)))))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         #s(stx-boundary (s9 s10 (s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prepare-env . #f)
        (phase-up . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 (s2 s3) (s4 s5))))
        (enter-block #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 s4))))
         #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))))
      ((case-lambda ((x) x) ((x y) (+ x y)))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (visit . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (visit . #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s2 s3))))))
        (visit . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (prim-case-lambda
         .
         #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s1 s2)))))
        (next . #f)
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (next . #f)
        (lambda-renames #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 s0 s1)))
        (enter-block #s(stx-boundary (s0 s1 s2)))
        (block-renames
         (#s(stx-boundary (s0 s1 s2)))
         #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 s2)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2 s3)) . #s(stx-boundary (s1 s2 s3)))
        (enter-macro
         #s(stx-boundary (s0 s1 s2 s3))
         .
         #s(stx-boundary (s0 s1 s2 s3)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2 s3)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2 s3))
         .
         #s(stx-boundary (s0 s1 s2 s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 s2 s3))
         .
         #s(stx-boundary (s0 s1 s2 s3)))
        (visit . #s(stx-boundary (s0 s1 s2 s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s3)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2 s3)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2 s3)))
        (exit-list #s(stx-boundary (s0 s1 s2 s3)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 ((s1) s1) ((s1 s2) (s3 s4 s1 s2)))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 ((s2) s2) ((s2 s3) (s4 s5 s2 s3))))))))
      ((module m racket/base (require racket/list) foldl)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (prim-module . #s(stx-boundary (s0 s1 s2 (s3 s4) s5)))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary s2))
        (tag . #s(stx-boundary (s0 (s1 s2) s3)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) s4))
         .
         #s(stx-boundary (s1 (s2 s3) s4)))
        (visit . #s(stx-boundary (s0 (s1 s2) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s3))
         .
         #s(stx-boundary (s0 (s1 s2) s3)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s3)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10))
         .
         #s(stx-boundary (s11 (s8 s9) s10)))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (visit
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 s9) s10))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12)))
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
        (stop/return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 s11))
             (s1 s2 s12))))
        (track-syntax
         s0
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 s12))
             (s2 s3 s13)))
         .
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 s12))
             (s2 s3 s13))))
        (next . #f)
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
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f))))
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
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 s10)))
         #s(stx-boundary (s7 s8 s11)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 #f)))
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #f)))
        (module-pass1-case . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 #f)) . #s(stx-boundary (s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f))))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (local-post . #s(stx-boundary (s0 s1)))
        (exit-local . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s3 s4 (s5 s2))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 s2))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2))))
        (splice #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 s3 s4)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
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
        (exit-macro
         #s(stx-boundary (s0 (s1 s2)))
         .
         #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 s2))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2))))
        (splice #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 () s3) s4))
         .
         #s(stx-boundary (s5 s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 () s3) s4))
         .
         #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (visit . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-lambda . #s(stx-boundary (s0 () s1)))
        (lambda-renames #s(stx-boundary ()) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 () s3) s4)))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return
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
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s7 s11)
              (s9 s12 (s13 () s14) s15)))))))
      ((#%plain-app 1 2)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 1 2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 2)))
        (prim-#%app . #s(stx-boundary (s0 1 2)))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 1)) . #s(stx-boundary 1))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #s(stx-boundary (s0 . 1)))
        (exit-prim/return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 2)) . #s(stx-boundary 2))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #s(stx-boundary (s0 . 2)))
        (exit-prim/return . #s(stx-boundary (s0 2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 1) (s1 2))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2 1) (s2 2)))))))
      ('quoted
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2))))))
      ((let () (define-syntax (ok stx) (quote-syntax 8)) (ok 5))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (prim-let-values . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8))) #s(stx-boundary (s1 5)))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 5)) . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 5)))
        (exit-macro #s(stx-boundary 8) . #s(stx-boundary 8))
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->letrec () () #s(stx-boundary 8))
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (finish-block #s(stx-boundary (s0 () (s1 8))))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((let () (define (ok x) '8) (ok 5))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s3 5)))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s5 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (prim-let-values . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s2 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8))) #s(stx-boundary (s1 5)))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 8)))
         .
         #s(stx-boundary (s1 (s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 5)))
        (block->letrec
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 (s2) (s3 8))))
         #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (enter-list #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list #s(stx-boundary (s0 s1 (s2 5))))
        (finish-block
         #s(stx-boundary (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5)))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 () (s0 (((s1) (s2 (s3) (s4 8)))) (s5 s1 (s4 5))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (s1 () (s1 (((s2) (s3 (s4) (s5 8)))) (s6 s2 (s5 5)))))))))
      ((begin0 '3 '5)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))
        (visit . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (prim-begin0 . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (next . #f)
        (visit . #s(stx-boundary (s0 3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 3)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 3)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 3) (s1 5))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2 3) (s2 5)))))))
      ((letrec-values (((x) __y) ((y z) __w)) __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 (((s2) s3) ((s4 s5) s6)) s7))))
        (visit . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (prim-letrec-values
         .
         #s(stx-boundary (s0 (((s1) s2) ((s3 s4) s5)) s6)))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)) (#s(stx-boundary s1) #s(stx-boundary s2)))
         (#s(stx-boundary s3) #s(stx-boundary s4))
         #s(stx-boundary s5))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (exit-list #s(stx-boundary (s0 . s1)))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (((s1) (s2 . s3)) ((s4 s5) (s2 . s6))) (s2 . s7))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (s1 (((s2) (s3 . s4)) ((s5 s6) (s3 . s7))) (s3 . s8)))))))
      ((let () (define (ok x) (second x)) (define (second y) 8) (ok 5))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-#%expression
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 s4)) (s2 (s5 s6) 8) (s3 5)))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5)))
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
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (prim-let-values
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 s3)) (s1 (s4 s5) 8) (s2 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         #s(stx-boundary (s0 (s3 s4) 8))
         #s(stx-boundary (s1 5)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         #s(stx-boundary (s0 (s3 s4) 8))
         #s(stx-boundary (s1 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 s2)))
          #s(stx-boundary (s0 (s3 s4) 8))
          #s(stx-boundary (s1 5)))
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         #s(stx-boundary (s0 (s3 s4) 8))
         #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 s2)))
         .
         #s(stx-boundary (s1 (s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) 8))
         .
         #s(stx-boundary (s0 (s1 s2) 8)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) 8)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) 8))
         .
         #s(stx-boundary (s1 (s2) 8)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1 s3) 8)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) 8))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 5)))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)))
         (#s(stx-boundary (s2 (s3) (s1 s3))) #s(stx-boundary (s2 (s4) 8)))
         #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s1))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 s0)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-list #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) 8)))
        (prim-lambda . #s(stx-boundary (s0 (s1) 8)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary 8))
        (enter-block #s(stx-boundary 8))
        (block-renames (#s(stx-boundary 8)) #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->list . #f)
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (enter-list #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list #s(stx-boundary (s0 s1 (s2 5))))
        (finish-block
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 s5 s3))) ((s5) (s2 (s6) (s7 8))))
             (s4 s1 (s7 5)))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             ()
             (s1
              (((s2) (s3 (s4) (s5 s6 s4))) ((s6) (s3 (s7) (s8 8))))
              (s5 s2 (s8 5))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2
               (((s3) (s4 (s5) (s6 s7 s5))) ((s7) (s4 (s8) (s9 8))))
               (s6 s3 (s9 5)))))))))
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
        (visit
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s2 (s3 s4) (s5 (s6 s7) (s8 (s9 (s10 s4)))) s7)
              (s0 (s3 9))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-#%expression
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
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9))))
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
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9))))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)
             (s10 (s2 9)))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))
         #s(stx-boundary (s9 (s1 9))))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))
         #s(stx-boundary (s9 (s1 9))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))
          #s(stx-boundary (s9 (s1 9))))
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))
         #s(stx-boundary (s9 (s1 9))))
        (next . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5))
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
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6)))
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (prim-define-syntaxes
         .
         #s(stx-boundary
            (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 (s8 (s9 s3)))) s6))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 s5) (s6 (s7 (s8 s2)))) s5)))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4))
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
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (prim-lambda
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 (s6 (s7 s1)))) s4)))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 (s2 s3) (s4 (s5 (s6 s0)))))
         #s(stx-boundary s3))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6)))))
         #s(stx-boundary s2))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))) #s(stx-boundary s2))
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6)))))
         #s(stx-boundary s2))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (prim-define-values . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s6))))))
        (rename-one
         (#s(stx-boundary s0) #s(stx-boundary s1))
         #s(stx-boundary (s2 (s3 (s4 s5)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->letrec
         ((#s(stx-boundary s0) #s(stx-boundary s1)))
         (#s(stx-boundary (s2 (s3 (s4 s5)))))
         #s(stx-boundary s1))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s1 (s2 (s3 s4)))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 (s3 s4)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 s3))) . #s(stx-boundary (s1 (s2 s3))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)))))
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (finish-block
         #s(stx-boundary (s0 (((s1 s2) (s3 s4 (s3 s5 (s3 s6 s7))))) s2)))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (s1) (s2 (((s3 s4) (s5 s6 (s5 s7 (s5 s8 s1))))) s4))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 9))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 9))))
        (block->letrec () () #s(stx-boundary (s0 (s1 9))))
        (enter-list #s(stx-boundary (s0 (s1 9))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 9))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 9))))
        (visit . #s(stx-boundary (s0 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 9)) . #s(stx-boundary (s0 9)))
        (macro-pre-x . #s(stx-boundary (s0 9)))
        (enter-local . #s(stx-boundary 9))
        (local-pre . #s(stx-boundary 9))
        (start . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 9)) . #s(stx-boundary 9))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #s(stx-boundary (s0 . 9)))
        (exit-prim/return . #s(stx-boundary (s0 9)))
        (local-post . #s(stx-boundary (s0 9)))
        (opaque-expr . #s(stx-boundary #:opaque))
        (exit-local . #s(stx-boundary (s0 9)))
        (macro-post-x #s(stx-boundary #:opaque) . #s(stx-boundary (s0 9)))
        (exit-macro #s(stx-boundary #:opaque) . #s(stx-boundary #:opaque))
        (visit . #s(stx-boundary #:opaque))
        (opaque-expr . #s(stx-boundary (s0 9)))
        (tag . #s(stx-boundary (s0 9)))
        (exit-prim/return . #s(stx-boundary (s0 9)))
        (exit-list #s(stx-boundary (s0 9)))
        (finish-block #s(stx-boundary (s0 () (s1 9))))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((set! __x 99)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 99))))
        (visit . #s(stx-boundary (s0 (s1 s2 99))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 99))))
        (visit . #s(stx-boundary (s0 (s1 s2 99))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 99))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 s2 99))))
        (visit . #s(stx-boundary (s0 s1 99)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 99)))
        (prim-set! . #s(stx-boundary (s0 s1 99)))
        (resolve . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 99))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 99)) . #s(stx-boundary 99))
        (enter-prim . #s(stx-boundary (s0 . 99)))
        (prim-#%datum . #s(stx-boundary (s0 . 99)))
        (exit-prim/return . #s(stx-boundary (s0 99)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 99))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2 (s3 99)))))))
      ((module m racket/base
         (define-syntax (ok stx) (quote-syntax 8))
         (ok)
         (list (ok) (ok)))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (visit
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (prim-module
         .
         #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) (s6 8)) (s4) (s7 (s4) (s4)))))
        (prepare-env . #f)
        (rename-one
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s1))
         #s(stx-boundary (s4 (s1) (s1))))
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 (s3 s4) (s5 8)) (s3) (s6 (s3) (s3))))
         .
         #s(stx-boundary (s1 (s2 (s3 s4) (s5 8)) (s3) (s6 (s3) (s3)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 8)) (s2) (s5 (s2) (s2))))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9))))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4) (s5 s6) (s7 #f))
             (s8 (s9 s10) (s11 8))
             (s9)
             (s12 (s9) (s9))))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11)))))
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
        (stop/return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) (s13 8)))
             (s1 s2 (s11))
             (s1 s2 (s14 (s11) (s11))))))
        (track-syntax
         s0
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) (s14 8)))
             (s2 s3 (s12))
             (s2 s3 (s15 (s12) (s12)))))
         .
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) (s14 8)))
             (s2 s3 (s12))
             (s2 s3 (s15 (s12) (s12))))))
        (next . #f)
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
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f))))
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
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) (s12 8))))
         #s(stx-boundary (s7 s8 (s10)))
         #s(stx-boundary (s7 s8 (s13 (s10) (s10)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 #f)))
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #f)))
        (module-pass1-case . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 #f)) . #s(stx-boundary (s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f))))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4) (s5 8)))))
        (enter-local . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (local-pre . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (start . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (local-post . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (exit-local . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8)))))
         .
         #s(stx-boundary (s6 s7 (s8 (s2 s4) (s5 8)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8)))))
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (prim-begin . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) (s5 8))))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         #s(stx-boundary (s5 s6 (s1)))
         #s(stx-boundary (s5 s6 (s7 (s1) (s1)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (module-pass1-case . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prepare-env . #f)
        (phase-up . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))))
      (__x
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 . s2))))))
      ((module m '#%kernel 5)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (visit . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (visit . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) 5)))
        (prepare-env . #f)
        (rename-one #s(stx-boundary 5))
        (track-syntax s0 #s(stx-boundary 5) . #s(stx-boundary 5))
        (visit . #s(stx-boundary 5))
        (stop/return . #s(stx-boundary 5))
        (tag . #s(stx-boundary (s0 5)))
        (track-syntax s0 #s(stx-boundary (s1 5)) . #s(stx-boundary (s1 5)))
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 5)))
        (track-syntax s0 #s(stx-boundary (s1 5)) . #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (module-pass1-case . #s(stx-boundary (s0 5)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 5)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 5))))
        (rename-one . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 (s2 5)))))))
      ((#%top . __x)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 . s2))))
        (visit . #s(stx-boundary (s0 (s1 . s2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 . s2))))
        (visit . #s(stx-boundary (s0 (s1 . s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 . s2))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 . s2))))
        (visit . #s(stx-boundary (s0 . s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 . s2))))))
      ((module m racket/base (define (proc x) x) (provide proc))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (prim-module . #s(stx-boundary (s0 s1 s2 (s3 (s4 s5) s5) (s6 s4))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 (s1 s2) s2)) #s(stx-boundary (s3 s1)))
        (tag . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 (s3 s4) s4) (s5 s3)))
         .
         #s(stx-boundary (s1 (s2 (s3 s4) s4) (s5 s3))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2)))
         .
         #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 (s2 s3) s3) (s4 s2))))
        (macro-post-x
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9)))
         .
         #s(stx-boundary (s12 (s8 (s9 s10) s10) (s11 s9))))
        (exit-macro
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9)))
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary
            (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)) (s8 (s9 s10) s10) (s11 s9)))
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
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11))))
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
        (stop/return
         .
         #s(stx-boundary
            (s0
             (s1 s2 (s3 s4 (s5 s6) (s7 s8) (s9 #f)))
             (s1 s2 (s10 (s11 s12) s12))
             (s1 s2 (s13 s11)))))
        (track-syntax
         s0
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) s13))
             (s2 s3 (s14 s12))))
         .
         #s(stx-boundary
            (s1
             (s2 s3 (s4 s5 (s6 s7) (s8 s9) (s10 #f)))
             (s2 s3 (s11 (s12 s13) s13))
             (s2 s3 (s14 s12)))))
        (next . #f)
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
         #s(stx-boundary (s0 s1 (s2 s3 (s4 s5) (s6 s7) (s8 #f))))
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
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (local-post . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (exit-local . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s8 s9 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (exit-macro
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f))))
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (visit . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (module-pass1-case
         .
         #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2 (s3 s4) (s5 s6) (s7 #f)))))
        (splice
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f)))
         #s(stx-boundary (s7 s8 (s9 (s10 s11) s11)))
         #s(stx-boundary (s7 s8 (s12 s10))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (module-pass1-case . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-submodule . #f)
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prim-module . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5) (s6 #f))))
        (prepare-env . #f)
        (rename-one #s(stx-boundary (s0 s1)) #s(stx-boundary (s2 #f)))
        (tag . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2 s3) (s4 #f)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (prim-module-begin . #f)
        (rename-one . #s(stx-boundary (s0 (s1 s2) (s3 #f))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (module-pass1-case . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-case . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 #f)))
        (module-pass1-case . #s(stx-boundary (s0 #f)))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 #f)) . #s(stx-boundary (s1 #f)))
        (enter-prim . #s(stx-boundary (s0 s1 #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f))))
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2) (s3 s4 (s5 #f)))))
        (rename-one
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (exit-prim
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 (s5 s6) (s7 s8 (s2 #f))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4) s4)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4) s4))))
        (enter-local . #s(stx-boundary (s0 (s1 s2) s2)))
        (local-pre . #s(stx-boundary (s0 (s1 s2) s2)))
        (start . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (local-post . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-local . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4))))
         .
         #s(stx-boundary (s5 s6 (s7 (s2 s4) s4))))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4))))
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (prim-begin . #s(stx-boundary (s0 (s1 (s2) (s3 (s4) s4)))))
        (splice
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         #s(stx-boundary (s4 s5 (s6 s1))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (module-pass1-case . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (exit-case
         #s(stx-boundary s0)
         (#s(stx-boundary s1))
         #s(stx-boundary (s2 (s3) s3)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (enter-local . #s(stx-boundary (s0 s1)))
        (local-pre . #s(stx-boundary (s0 s1)))
        (start . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3))))
         .
         #s(stx-boundary (s4 s3)))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (local-post . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (exit-local . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4)))))
         .
         #s(stx-boundary (s6 s7 (s5 s4))))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4)))))
         .
         #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4))))))
        (visit . #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4))))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4))))))
        (prim-begin . #s(stx-boundary (s0 (s1 (s2 (s3 s4) (s5 s4))))))
        (splice #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (module-pass1-case . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (prim-stop . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (visit . #f)
        (enter-prim . #f)
        (prim-define-values . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (exit-prim/return . #f)
        (next . #f)
        (next-group . #f)
        (next-group . #f)
        (next . #f)
        (next . #f)
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (prim-provide . #s(stx-boundary (s0 (s1 (s2 s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (exit-prim . #s(stx-boundary (s0 s1)))
        (next-group . #f)
        (next . #f)
        (next . #f)
        (next . #f)
        (exit-prim/return
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
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             s1
             s2
             (s3
              (s0 s4 (s5 s6) (s3 (s7 s8) (s9 s10 (s5 #f))))
              (s11 (s12) (s13 (s14) s14))
              (s15 s12)))))))
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
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-#%expression
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s4) (s5 (s3 6))) (s6 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5)))
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
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5)))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s3) (s4 (s2 6))) (s5 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s4 5)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s4 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8)))
          #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
          #s(stx-boundary (s4 5)))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s4 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 6))))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 6)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 (s2 6))))
        (enter-block #s(stx-boundary (s0 (s1 6))))
        (block-renames
         (#s(stx-boundary (s0 (s1 6))))
         #s(stx-boundary (s0 (s1 6))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 6))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 6))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 6))))
        (prim-quote-syntax . #s(stx-boundary (s0 (s1 6))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 6))))
        (exit-list #s(stx-boundary (s0 (s1 6))))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 5)) . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary (s0 6)) . #s(stx-boundary (s1 5)))
        (exit-macro #s(stx-boundary (s0 6)) . #s(stx-boundary (s0 6)))
        (visit . #s(stx-boundary (s0 6)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 6)) . #s(stx-boundary (s0 6)))
        (macro-pre-x . #s(stx-boundary (s0 6)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (exit-macro #s(stx-boundary 8) . #s(stx-boundary 8))
        (visit . #s(stx-boundary 8))
        (stop/return . #s(stx-boundary 8))
        (block->letrec () () #s(stx-boundary 8))
        (enter-list #s(stx-boundary 8))
        (next . #f)
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (finish-block #s(stx-boundary (s0 () (s1 8))))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () (s1 8)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () (s2 8))))))))
      ((with-continuation-mark __x __y __z)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (visit . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (visit . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 s2 s3 s4))))
        (visit . #s(stx-boundary (s0 s1 s2 s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s3)))
        (prim-with-continuation-mark . #s(stx-boundary (s0 s1 s2 s3)))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 . s2) (s1 . s3) (s1 . s4))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 (s2 . s3) (s2 . s4) (s2 . s5)))))))
      ((#%variable-reference __z)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 s2))))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%variable-reference . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 s2))))))
      ((lambda (x) (define y (+ x x)) y)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (visit . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 s4 (s5 s2 s2)) s4))))
        (visit . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (visit . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s1 s1)) s3)))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 s2 (s3 s0 s0)))
         #s(stx-boundary s2))
        (enter-block #s(stx-boundary (s0 s1 (s2 s3 s3))) #s(stx-boundary s1))
        (block-renames
         (#s(stx-boundary (s0 s1 (s2 s3 s3))) #s(stx-boundary s1))
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         #s(stx-boundary s1))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3 s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 s3 s3)))
         .
         #s(stx-boundary (s4 s1 (s2 s3 s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 s3 s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 s3 s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 s2 s2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->letrec
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 s2 s2)))
         #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2 s2)) . #s(stx-boundary (s1 s2 s2)))
        (enter-macro
         #s(stx-boundary (s0 s1 s2 s2))
         .
         #s(stx-boundary (s0 s1 s2 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2 s2))
         .
         #s(stx-boundary (s0 s1 s2 s2)))
        (exit-macro
         #s(stx-boundary (s0 s1 s2 s2))
         .
         #s(stx-boundary (s0 s1 s2 s2)))
        (visit . #s(stx-boundary (s0 s1 s2 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2 s2)))
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (finish-block #s(stx-boundary (s0 (((s1) (s2 s3 s4 s4))) s1)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1) (s2 (((s3) (s4 s5 s1 s1))) s3))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 (((s4) (s5 s6 s2 s2))) s4)))))))
      ((let () (define (ok x) '8) (define (second y) (ok y)) (second 5))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-#%expression
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 8)) (s2 (s6 s7) (s3 s7)) (s6 5)))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5)))
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
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5)))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 8)) (s1 (s5 s6) (s2 s6)) (s5 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s5) (s1 s5)))
         #s(stx-boundary (s4 5)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s5) (s1 s5)))
         #s(stx-boundary (s4 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8)))
          #s(stx-boundary (s0 (s4 s5) (s1 s5)))
          #s(stx-boundary (s4 5)))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s5) (s1 s5)))
         #s(stx-boundary (s4 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 8)))
         .
         #s(stx-boundary (s1 (s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 8)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 8)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 s2)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 s2))))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) (s3 s2)))
         .
         #s(stx-boundary (s1 (s2) (s3 s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1 s3) (s4 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) (s4 s3)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s5 s1 (s2 (s3) (s4 s3)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 s3)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 5)))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)))
         (#s(stx-boundary (s2 (s3) (s4 8))) #s(stx-boundary (s2 (s5) (s0 s5))))
         #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 s1))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 s1))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 s0)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-list #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 s1))))
        (enter-list #s(stx-boundary (s0 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s1 5)))
        (enter-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (macro-pre-x . #s(stx-boundary (s0 s1 5)))
        (macro-post-x #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (exit-macro #s(stx-boundary (s0 s1 5)) . #s(stx-boundary (s0 s1 5)))
        (visit . #s(stx-boundary (s0 s1 5)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 5)))
        (prim-#%app . #s(stx-boundary (s0 s1 5)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 5))))
        (exit-list #s(stx-boundary (s0 s1 (s2 5))))
        (finish-block
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) (s4 8))))
             (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) (s4 8))))
              (s0 (((s5) (s2 (s6) (s7 s1 s6)))) (s7 s5 (s4 5)))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) (s5 8))))
               (s1 (((s6) (s3 (s7) (s8 s2 s7)))) (s8 s6 (s5 5))))))))))
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
        (stop/return
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
        (prim-#%expression
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
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0
             ()
             (s1 (s2 s3) (s4 8))
             (s1 (s5 s3) (s4 (s2 6)))
             (s6 (s7 s8) s8)
             (s6 (s9 s10) s10)
             (s7 (s9 (s5))))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s5 (s6 s7) s7))
         #s(stx-boundary (s5 (s8 s9) s9))
         #s(stx-boundary (s6 (s8 (s4)))))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s5 (s6 s7) s7))
         #s(stx-boundary (s5 (s8 s9) s9))
         #s(stx-boundary (s6 (s8 (s4)))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8)))
          #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
          #s(stx-boundary (s5 (s6 s7) s7))
          #s(stx-boundary (s5 (s8 s9) s9))
          #s(stx-boundary (s6 (s8 (s4)))))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s0 (s4 s2) (s3 (s1 6))))
         #s(stx-boundary (s5 (s6 s7) s7))
         #s(stx-boundary (s5 (s8 s9) s9))
         #s(stx-boundary (s6 (s8 (s4)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 6))))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s6 (s1 s3) (s4 (s5 6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 6))))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 6)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s4 (s1) (s2 (s3 6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 (s2 6))))
        (enter-block #s(stx-boundary (s0 (s1 6))))
        (block-renames
         (#s(stx-boundary (s0 (s1 6))))
         #s(stx-boundary (s0 (s1 6))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 6))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 6))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 6))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 6))))
        (prim-quote-syntax . #s(stx-boundary (s0 (s1 6))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 6))))
        (exit-list #s(stx-boundary (s0 (s1 6))))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 (s3 6)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2)))))
        (block->letrec
         ((#s(stx-boundary s0)) (#s(stx-boundary s1)))
         (#s(stx-boundary (s2 (s3) s3)) #s(stx-boundary (s2 (s4) s4)))
         #s(stx-boundary (s0 (s1 (s5)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (enter-list #s(stx-boundary (s0 (s1 (s2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s1 (s2 (s3)))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2))) . #s(stx-boundary (s1 (s2))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2)))
         .
         #s(stx-boundary (s0 s1 (s2))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2)))
         .
         #s(stx-boundary (s0 s1 (s2))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2)))
         .
         #s(stx-boundary (s0 s1 (s2))))
        (visit . #s(stx-boundary (s0 s1 (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (macro-post-x #s(stx-boundary (s0 6)) . #s(stx-boundary (s1)))
        (exit-macro #s(stx-boundary (s0 6)) . #s(stx-boundary (s0 6)))
        (visit . #s(stx-boundary (s0 6)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 6)) . #s(stx-boundary (s0 6)))
        (macro-pre-x . #s(stx-boundary (s0 6)))
        (macro-post-x #s(stx-boundary 8) . #s(stx-boundary (s0 6)))
        (exit-macro #s(stx-boundary 8) . #s(stx-boundary 8))
        (visit . #s(stx-boundary 8))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 8)) . #s(stx-boundary 8))
        (enter-prim . #s(stx-boundary (s0 . 8)))
        (prim-#%datum . #s(stx-boundary (s0 . 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 8))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (exit-list #s(stx-boundary (s0 s1 (s0 s2 (s3 8)))))
        (finish-block
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3) s3)))
             (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8)))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             ()
             (s0
              (((s1) (s2 (s3) s3)))
              (s0 (((s4) (s2 (s5) s5))) (s6 s1 (s6 s4 (s7 8))))))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (s1
              ()
              (s1
               (((s2) (s3 (s4) s4)))
               (s1 (((s5) (s3 (s6) s6))) (s7 s2 (s7 s5 (s8 8)))))))))))
      ((let ()
         (define-syntax (ok stx) (quote-syntax 8))
         (define (ident x) x)
         9)
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 8)) (s6 (s7 s8) s8) 9))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9))
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
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (prim-let-values
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 8)) (s5 (s6 s7) s7) 9)))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s4 (s5 s6) s6))
         #s(stx-boundary 9))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s4 (s5 s6) s6))
         #s(stx-boundary 9))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 8)))
          #s(stx-boundary (s4 (s5 s6) s6))
          #s(stx-boundary 9))
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         #s(stx-boundary (s4 (s5 s6) s6))
         #s(stx-boundary 9))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 8)))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s5 (s1 s3) (s4 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (prim-define-syntaxes . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 8)))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) (s3 8))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 8))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s3 (s1) (s2 8))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 8)))
         .
         #s(stx-boundary (s0 (s1) (s2 8))))
        (visit . #s(stx-boundary (s0 (s1) (s2 8))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 8))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 8))))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary (s1 8)))
        (enter-block #s(stx-boundary (s0 8)))
        (block-renames (#s(stx-boundary (s0 8))) #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 8)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 8)))
        (next . #f)
        (visit . #s(stx-boundary (s0 8)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 8)))
        (prim-quote-syntax . #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 8)))
        (exit-list #s(stx-boundary (s0 8)))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 8))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) s2)))
        (track-syntax
         s0
         #s(stx-boundary (s1 (s2) s2))
         .
         #s(stx-boundary (s1 (s2) s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1 s3) s3)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3) s3))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s4 s1 (s2 (s3) s3))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) s3)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 (s3) s3))))
        (rename-one (#s(stx-boundary s0)) #s(stx-boundary (s1 (s2) s2)))
        (next . #f)
        (visit . #s(stx-boundary 9))
        (stop/return . #s(stx-boundary 9))
        (block->letrec
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 (s2) s2)))
         #s(stx-boundary 9))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s1)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s1)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s1)))
        (enter-list #s(stx-boundary 9))
        (next . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 9)) . #s(stx-boundary 9))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #s(stx-boundary (s0 . 9)))
        (exit-prim/return . #s(stx-boundary (s0 9)))
        (exit-list #s(stx-boundary (s0 9)))
        (finish-block #s(stx-boundary (s0 (((s1) (s2 (s3) s3))) (s4 9))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 () (s0 (((s1) (s2 (s3) s3))) (s4 9)))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 () (s1 (((s2) (s3 (s4) s4))) (s5 9))))))))
      ((let () (define-syntax-rule (ok x) x) (ok 5))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (visit . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) s4) (s3 5)))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5)))
         .
         #s(stx-boundary (s4 () (s1 (s2 s3) s3) (s2 5))))
        (exit-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (prim-let-values . #s(stx-boundary (s0 () (s1 (s2 s3) s3) (s2 5))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s1 5)))
        (enter-block #s(stx-boundary (s0 (s1 s2) s2)) #s(stx-boundary (s1 5)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) s2)) #s(stx-boundary (s1 5)))
         #s(stx-boundary (s0 (s1 s2) s2))
         #s(stx-boundary (s1 5)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) s2))
         .
         #s(stx-boundary (s0 (s1 s2) s2)))
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
        (visit
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
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-define-syntaxes
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
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary
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
              (s6 (s10 s2 (s11 (s7))))))))
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
        (prim-lambda
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
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary
            (s1
             s2
             #t
             s0
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s0 s5)))
             (s4 (s8 s0 (s9 (s5)))))))
        (enter-block
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
        (block-renames
         (#s(stx-boundary
             (s0
              s1
              #t
              s2
              ()
              s3
              #f
              ((s4 s5) (s6 (s7 s2 s5)))
              (s4 (s8 s2 (s9 (s5)))))))
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
        (next . #f)
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
         #s(stx-boundary
            (s0
             s1
             #t
             s2
             ()
             s3
             #f
             ((s4 s5) (s6 (s7 s2 s5)))
             (s4 (s8 s2 (s9 (s5))))))
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
        (track-syntax s0 #s(stx-boundary s1) . #s(stx-boundary s1))
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
        (stop/return
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
        (block->list . #f)
        (enter-list
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
        (prim-let-values
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
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary s1))
         #s(stx-boundary
            (s2
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
                (s23 #f #:opaque s0)))))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (enter-block
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
        (block-renames
         (#s(stx-boundary
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
        (next . #f)
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
                (s23 #f #:opaque s12))))))
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
        (stop/return
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
        (block->list . #f)
        (enter-list
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
        (prim-let-values
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
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary
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
              s12)))
         #s(stx-boundary
            (s3
             s0
             (s7
              ((s13 s0))
              (s14 (((s15) (s16 0 (s17 s13)))) () (s18 (s19 s20 s15))))
             (s7
              ((s0 ((s1 (s2) s5) s12)))
              (s3
               s0
               (s7 () (s14 () () (s21 s20 (s22 (s15)))))
               (s23 #f #:opaque s12))))))
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
        (tag2
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
             s12))
         .
         #s(stx-boundary
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
        (prim-#%app
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
        (prim-lambda
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
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary
            (s1
             (s2 s0)
             (s1
              ((s3 (s0) s4) (s5 s0))
              ((s3
                (s0)
                (s1 (s2 s0) (s6 ((s7 (s5 s0))) (s8 s7 (s9 (s10 s0)) s7)) #f))
               (s10 s0))
              #f)
             #f)))
        (enter-block
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
        (block-renames
         (#s(stx-boundary
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
        (stop/return
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
        (block->list . #f)
        (enter-list
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
        (prim-if
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
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
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
        (prim-if
         .
         #s(stx-boundary
            (s0
             ((s1 (s2) s3) (s4 s2))
             ((s1
               (s2)
               (s0 (s5 s2) (s6 ((s7 (s4 s2))) (s8 s7 (s9 (s10 s2)) s7)) #f))
              (s10 s2))
             #f)))
        (visit . #s(stx-boundary ((s0 (s1) s2) (s3 s1))))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2)))
         .
         #s(stx-boundary ((s1 (s2) s3) (s4 s2))))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (prim-#%app . #s(stx-boundary (s0 (s1 (s2) s3) (s4 s2))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s2)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s2)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s1))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2) s3) (s0 s4 s2))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            ((s0
              (s1)
              (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))
             (s9 s1))))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2)))
         .
         #s(stx-boundary
            ((s1
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
        (prim-#%app
         .
         #s(stx-boundary
            (s0
             (s1
              (s2)
              (s3 (s4 s2) (s5 ((s6 (s7 s2))) (s8 s6 (s9 (s10 s2)) s6)) #f))
             (s10 s2))))
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
        (prim-lambda
         .
         #s(stx-boundary
            (s0
             (s1)
             (s2 (s3 s1) (s4 ((s5 (s6 s1))) (s7 s5 (s8 (s9 s1)) s5)) #f))))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary
            (s1 (s2 s0) (s3 ((s4 (s5 s0))) (s6 s4 (s7 (s8 s0)) s4)) #f)))
        (enter-block
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (block-renames
         (#s(stx-boundary
             (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (block->list . #f)
        (enter-list
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
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
        (prim-if
         .
         #s(stx-boundary
            (s0 (s1 s2) (s3 ((s4 (s5 s2))) (s6 s4 (s7 (s8 s2)) s4)) #f)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (next . #f)
        (visit . #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 ((s1 (s2 s3))) (s4 s1 (s5 (s6 s3)) s1)))
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
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1)))
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (prim-let-values
         .
         #s(stx-boundary (s0 (((s1) (s2 s3))) (s4 s1 (s5 (s6 s3)) s1))))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 s2)))
         #s(stx-boundary (s3 s0 (s4 (s5 s2)) s0)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (enter-block #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (block-renames
         (#s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)) s1)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f))
         .
         #s(stx-boundary (s2 s1 (s3 (s4 s5)) s1)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (prim-if . #s(stx-boundary (s0 s1 (s2 (s3 (s4 s5)) s1) #f)))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 (s2 s3)) s4))
         .
         #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (macro-pre-x . #s(stx-boundary (s0 (s1 (s2 s3)) s4)))
        (macro-post-x
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f))
         .
         #s(stx-boundary (s4 (s1 (s2 s3)) s5)))
        (exit-macro
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f))
         .
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (prim-if . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 s3))) . #s(stx-boundary (s1 (s2 s3))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (tag . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 s2 (s1 s3 s4)) s5 (s6 #f))))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (exit-list
         #s(stx-boundary (s0 s1 (s0 (s2 s3 (s2 s4 s5)) s1 (s6 #f)) (s6 #f))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 s3 s4)))
             (s5 s1 (s5 (s2 s6 (s2 s7 s4)) s1 (s8 #f)) (s8 #f)))))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (exit-list
         #s(stx-boundary
            (s0
             (s1 s2 s3)
             (s4
              (((s5) (s1 s6 s3)))
              (s0 s5 (s0 (s1 s7 (s1 s8 s3)) s5 (s9 #f)) (s9 #f)))
             (s9 #f))))
        (exit-prim/return
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
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return
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
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
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
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
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
        (exit-prim/return
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
        (exit-prim/return
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
        (enter-block
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
        (block-renames
         (#s(stx-boundary
             (s0
              s1
              (s2 ((s3 s1)) (s4 (((s5) (s6 0 (s7 s3)))) () (s8 (s9 s10 s5))))
              (s2
               ((s1 ((s11 (s12) s13) s14)))
               (s0
                s1
                (s2 () (s4 () () (s15 s10 (s16 (s5)))))
                (s17 #f #:opaque s14))))))
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
        (stop/return
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
        (block->list . #f)
        (enter-list
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
        (prim-if
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
         #s(stx-boundary
            (s0 ((s1 s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4)))))
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
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4)))))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0 (((s1) s2)) (s3 (((s4) (s5 0 (s6 s1)))) () (s7 (s8 s9 s4))))))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary s1))
         #s(stx-boundary (s2 (((s3) (s4 0 (s5 s0)))) () (s6 (s7 s8 s3)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (enter-block
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (block-renames
         (#s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (next . #f)
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (block->list . #f)
        (enter-list
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (next . #f)
        (visit
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (prim-letrec-syntaxes+values
         .
         #s(stx-boundary (s0 (((s1) (s2 0 (s3 s4)))) () (s5 (s6 s7 s1)))))
        (letX-renames
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 0 (s2 s3))))
         ()
         ()
         #s(stx-boundary (s4 (s5 s6 s0))))
        (prepare-env . #f)
        (next . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 0 (s2 s3)))
         .
         #s(stx-boundary (s1 0 (s2 s3))))
        (enter-prim . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (prim-#%app . #s(stx-boundary (s0 s1 0 (s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 0)) . #s(stx-boundary 0))
        (enter-prim . #s(stx-boundary (s0 . 0)))
        (prim-#%datum . #s(stx-boundary (s0 . 0)))
        (exit-prim/return . #s(stx-boundary (s0 0)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote-syntax . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 0) (s3 s4))))
        (next . #f)
        (exit-bind . #f)
        (next-group . #f)
        (enter-block #s(stx-boundary (s0 (s1 s2 s3))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2 s3))))
         #s(stx-boundary (s0 (s1 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 s3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2 s3))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2 s3))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 s3 s4)))
         .
         #s(stx-boundary (s1 (s2 s3 s4))))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
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
        (track-syntax s0 #s(stx-boundary s1) . #s(stx-boundary s1))
        (macro-post-x
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7)))
         .
         #s(stx-boundary (s4 s5 s8)))
        (exit-macro
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7)))
         .
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7))))
        (visit . #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7)))
         .
         #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7))))
        (macro-pre-x . #s(stx-boundary (s0 ((s1 (s2 (s3 s4) s5))) (s6 s7))))
        (macro-post-x
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7)))
         .
         #s(stx-boundary (s8 ((s1 (s2 (s3 s4) s5))) (s6 s7))))
        (exit-macro
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7)))
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7))))
        (visit . #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7))))
        (prim-let-values
         .
         #s(stx-boundary (s0 (((s1) (s2 (s3 s4) s5))) (s6 s7))))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary (s1 (s2 s3) s4)))
         #s(stx-boundary (s5 s6)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) s3)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 s3) s4))
         .
         #s(stx-boundary (s1 (s2 s3) s4)))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3) s4)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 s3) s4)))
        (enter-block #s(stx-boundary (s0 s1)))
        (block-renames (#s(stx-boundary (s0 s1))) #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-#%expression . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (tag . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (((s1) (s2 s3 (s4 s5) s6))) s7)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 s1 (s2 (((s3) (s0 s4 (s5 s6) s7))) s8))))
        (exit-list
         #s(stx-boundary (s0 s1 (s2 (((s3) (s0 s4 (s5 s6) s7))) s8))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) s8)))))
        (exit-list
         #s(stx-boundary (s0 () (s1 s2 (s0 (((s3) (s1 s4 (s5 s6) s7))) s8)))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (((s1) s2))
             (s0 () (s3 s4 (s0 (((s5) (s3 s6 (s7 s8) s9))) s1))))))
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
         #s(stx-boundary
            (s0
             ((s1 ((s2 (s3) s4) s5)))
             (s6
              s1
              (s0 () (s7 () () (s8 s9 (s10 (s11)))))
              (s12 #f #:opaque s5))))
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
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5))))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0
             (((s1) ((s2 (s3) s4) s5)))
             (s6
              s1
              (s7 () (s8 () () (s9 s10 (s11 (s12)))))
              (s13 #f #:opaque s5)))))
        (letX-renames
         ()
         ()
         ((#s(stx-boundary s0)))
         (#s(stx-boundary ((s1 (s2) s3) s4)))
         #s(stx-boundary
            (s5
             s0
             (s6 () (s7 () () (s8 s9 (s10 (s11)))))
             (s12 #f #:opaque s4))))
        (next . #f)
        (visit . #s(stx-boundary ((s0 (s1) s2) s3)))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 (s1 (s2) s3) s4))
         .
         #s(stx-boundary ((s1 (s2) s3) s4)))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (prim-#%app . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) s2)))
        (prim-lambda . #s(stx-boundary (s0 (s1) s2)))
        (lambda-renames #s(stx-boundary (s0)) #s(stx-boundary s1))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1) s2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2) s3) s4)))
        (enter-block
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (block-renames
         (#s(stx-boundary
             (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (next . #f)
        (visit
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (block->list . #f)
        (enter-list
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
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
        (prim-if
         .
         #s(stx-boundary
            (s0 s1 (s2 () (s3 () () (s4 s5 (s6 (s7))))) (s8 #f #:opaque s9))))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5))))))
         .
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (macro-pre-x . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (macro-post-x
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5))))))
         .
         #s(stx-boundary (s6 () (s1 () () (s2 s3 (s4 (s5)))))))
        (exit-macro
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5))))))
         .
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (visit . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (prim-let-values
         .
         #s(stx-boundary (s0 () (s1 () () (s2 s3 (s4 (s5)))))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (enter-block #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (block-renames
         (#s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
         #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (prim-letrec-syntaxes+values
         .
         #s(stx-boundary (s0 () () (s1 s2 (s3 (s4))))))
        (letX-renames () () () () #s(stx-boundary (s0 s1 (s2 (s3)))))
        (prepare-env . #f)
        (next-group . #f)
        (enter-block #s(stx-boundary (s0 s1 (s2 (s3)))))
        (block-renames
         (#s(stx-boundary (s0 s1 (s2 (s3)))))
         #s(stx-boundary (s0 s1 (s2 (s3)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 s1 (s2 (s3)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1 (s2 (s3)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 s2 (s3 (s4))))
         .
         #s(stx-boundary (s1 s2 (s3 (s4)))))
        (enter-prim . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (prim-#%app . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
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
        (exit-prim/return . #s(stx-boundary (s0 (s1))))
        (exit-prim/return . #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (exit-list #s(stx-boundary (s0 s1 s2 (s3 (s4)))))
        (exit-prim/return . #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (exit-list #s(stx-boundary (s0 () (s1 s2 s3 (s4 (s5))))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 () (s0 () (s1 s2 s3 (s4 (s5)))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 #f #:opaque s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 #f #:opaque s2))
         .
         #s(stx-boundary (s1 #f #:opaque s2)))
        (enter-prim . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 #f #:opaque s2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (next . #f)
        (visit . #s(stx-boundary #:opaque))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #:opaque)) . #s(stx-boundary #:opaque))
        (enter-prim . #s(stx-boundary (s0 . #:opaque)))
        (prim-#%datum . #s(stx-boundary (s0 . #:opaque)))
        (exit-prim/return . #s(stx-boundary (s0 #:opaque)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 #f) (s2 #:opaque) s3)))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (exit-list
         #s(stx-boundary
            (s0
             s1
             (s2 () (s2 () (s3 s4 s5 (s6 (s7)))))
             (s3 s8 (s6 #f) (s6 #:opaque) s9))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             (((s1) (s2 (s3 (s4) s5) s6)))
             (s7
              s1
              (s0 () (s0 () (s2 s8 s9 (s10 (s11)))))
              (s2 s12 (s10 #f) (s10 #:opaque) s6)))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0
             s1
             (s2
              (((s3) s1))
              (s2 () (s4 s5 (s2 (((s6) (s4 s7 (s8 s9) s10))) s3))))
             (s2
              (((s1) (s4 (s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s2 () (s4 s15 s10 (s8 (s16)))))
               (s4 s17 (s8 #f) (s8 #:opaque) s14))))))
        (exit-list
         #s(stx-boundary
            (s0
             s1
             (s2
              (((s3) s1))
              (s2 () (s4 s5 (s2 (((s6) (s4 s7 (s8 s9) s10))) s3))))
             (s2
              (((s1) (s4 (s11 (s12) s13) s14)))
              (s0
               s1
               (s2 () (s2 () (s4 s15 s10 (s8 (s16)))))
               (s4 s17 (s8 #f) (s8 #:opaque) s14))))))
        (exit-prim/return
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
               (s0 () (s2 s15 (s0 (((s16) (s2 s17 (s12 s18) s19))) s14))))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s20 s19 (s12 (s21)))))
                (s2 s22 (s12 #f) (s12 #:opaque) s13)))))))
        (exit-list
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
               (s0 () (s2 s15 (s0 (((s16) (s2 s17 (s12 s18) s19))) s14))))
              (s0
               (((s1) (s2 (s3 (s4) s7) s13)))
               (s5
                s1
                (s0 () (s0 () (s2 s20 s19 (s12 (s21)))))
                (s2 s22 (s12 #f) (s12 #:opaque) s13)))))))
        (exit-prim/return
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
                (s0 () (s4 s16 (s0 (((s17) (s4 s18 (s14 s19) s2))) s15))))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s20 s2 (s14 (s21)))))
                 (s4 s22 (s14 #f) (s14 #:opaque) s1))))))))
        (exit-list
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
                (s0 () (s4 s16 (s0 (((s17) (s4 s18 (s14 s19) s2))) s15))))
               (s0
                (((s3) (s4 (s5 (s6) s9) s1)))
                (s7
                 s3
                 (s0 () (s0 () (s4 s20 s2 (s14 (s21)))))
                 (s4 s22 (s14 #f) (s14 #:opaque) s1))))))))
        (exit-prim/return
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
                 (s2 () (s5 s16 (s2 (((s17) (s5 s18 (s14 s19) s1))) s15))))
                (s2
                 (((s4) (s5 (s0 (s6) s9) s3)))
                 (s7
                  s4
                  (s2 () (s2 () (s5 s20 s1 (s14 (s21)))))
                  (s5 s22 (s14 #f) (s14 #:opaque) s3)))))))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 5)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 5)) . #s(stx-boundary (s0 5)))
        (macro-pre-x . #s(stx-boundary (s0 5)))
        (macro-post-x #s(stx-boundary 5) . #s(stx-boundary (s0 5)))
        (exit-macro #s(stx-boundary 5) . #s(stx-boundary 5))
        (visit . #s(stx-boundary 5))
        (stop/return . #s(stx-boundary 5))
        (block->letrec () () #s(stx-boundary 5))
        (enter-list #s(stx-boundary 5))
        (next . #f)
        (visit . #s(stx-boundary 5))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 5)) . #s(stx-boundary 5))
        (enter-prim . #s(stx-boundary (s0 . 5)))
        (prim-#%datum . #s(stx-boundary (s0 . 5)))
        (exit-prim/return . #s(stx-boundary (s0 5)))
        (exit-list #s(stx-boundary (s0 5)))
        (finish-block #s(stx-boundary (s0 () (s1 5))))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () (s1 5)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () (s2 5))))))))
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
        (visit
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
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
        (prim-#%expression
         .
         #s(stx-boundary
            (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 s4)) (s8 s9) #f)) (s3 9)))))
        (visit
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9)))
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
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9)))
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
        (prim-let-values
         .
         #s(stx-boundary
            (s0 () (s1 (s2 s3) (s4 (s5 (s6 s3)) (s7 s8) #f)) (s2 9))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))
         #s(stx-boundary (s1 9)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))
         #s(stx-boundary (s1 9)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))
          #s(stx-boundary (s1 9)))
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))
         #s(stx-boundary (s1 9)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 s2)) (s6 s7) #f)))
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
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (visit
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 s3)) (s7 s8) #f)))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 (s5 s2)) (s6 s7) #f))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f)))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f)))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 s1)) (s5 s6) #f))))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 (s2 (s3 s0)) (s4 s5) #f)))
        (enter-block #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (block-renames
         (#s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
         #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 s3)) (s4 s5) #f)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s1 (s2 (s3 s4)) (s5 s6) #f)))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 (s3 s4)) (s5 s6) #f)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 (s2 s3))) . #s(stx-boundary (s1 (s2 s3))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 s3)))
         .
         #s(stx-boundary (s0 s1 (s2 s3))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s1 s2)))
        (enter-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 s2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 s2))
         .
         #s(stx-boundary (s0 s1 s2)))
        (exit-macro #s(stx-boundary (s0 s1 s2)) . #s(stx-boundary (s0 s1 s2)))
        (visit . #s(stx-boundary (s0 s1 s2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 s2)))
        (prim-#%app . #s(stx-boundary (s0 s1 s2)))
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
        (exit-prim/return . #s(stx-boundary (s0 s1 s2)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s0 s2 s3))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote . #f)
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary #f))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . #f)) . #s(stx-boundary #f))
        (enter-prim . #s(stx-boundary (s0 . #f)))
        (prim-#%datum . #s(stx-boundary (s0 . #f)))
        (exit-prim/return . #s(stx-boundary (s0 #f)))
        (exit-prim/return
         .
         #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (exit-list #s(stx-boundary (s0 s1 (s0 s2 (s0 s3 s4)) (s5 s6) (s5 #f))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1) (s2 s3 (s2 s4 (s2 s5 s1)) (s6 s7) (s6 #f)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0 9)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 9)) . #s(stx-boundary (s0 9)))
        (macro-pre-x . #s(stx-boundary (s0 9)))
        (enter-local . #s(stx-boundary 9))
        (local-pre . #s(stx-boundary 9))
        (visit . #s(stx-boundary 9))
        (stop/return . #s(stx-boundary 9))
        (local-post . #s(stx-boundary 9))
        (exit-local . #s(stx-boundary 9))
        (macro-post-x #s(stx-boundary 9) . #s(stx-boundary (s0 9)))
        (exit-macro #s(stx-boundary 9) . #s(stx-boundary 9))
        (visit . #s(stx-boundary 9))
        (stop/return . #s(stx-boundary 9))
        (block->letrec () () #s(stx-boundary 9))
        (enter-list #s(stx-boundary 9))
        (next . #f)
        (visit . #s(stx-boundary 9))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 9)) . #s(stx-boundary 9))
        (enter-prim . #s(stx-boundary (s0 . 9)))
        (prim-#%datum . #s(stx-boundary (s0 . 9)))
        (exit-prim/return . #s(stx-boundary (s0 9)))
        (exit-list #s(stx-boundary (s0 9)))
        (finish-block #s(stx-boundary (s0 () (s1 9))))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () (s1 9)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () (s2 9))))))))
      ((quote-syntax (stx-quoted))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2)))))
        (visit . #s(stx-boundary (s0 (s1 (s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 (s2)))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 (s2)))))
        (visit . #s(stx-boundary (s0 (s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1))))
        (prim-quote-syntax . #s(stx-boundary (s0 (s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2)))))))
      ((let ()
         (define-syntax (lift stx)
           (syntax-local-lift-require 'racket/list #'foldl))
         (lift))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 s7) (s8 s9))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2)))
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
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (visit
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (prim-let-values
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 s6) (s7 s8))) (s2))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         #s(stx-boundary (s1)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         #s(stx-boundary (s1)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
          #s(stx-boundary (s1)))
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         #s(stx-boundary (s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7))))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 s5) (s6 s7)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s9 (s1 s3) (s4 (s5 s6) (s7 s8)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 s6) (s7 s8))))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 s5) (s6 s7)))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s7 (s1) (s2 (s3 s4) (s5 s6)))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 s4) (s5 s6)))))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 (s2 s3) (s4 s5))))
        (enter-block #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 s4))))
         #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 s4))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s1 (s2 s3) (s4 s5))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5)))
         .
         #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (visit . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
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
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (macro-pre-x . #s(stx-boundary (s0 s1)))
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (local-value . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (local-value-result . #f)
        (macro-post-x #s(stx-boundary (s0 s1)) . #s(stx-boundary (s2 s1)))
        (exit-macro #s(stx-boundary (s0 s1)) . #s(stx-boundary (s0 s1)))
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-quote-syntax . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (exit-list #s(stx-boundary (s0 s1 (s2 s3) (s4 s5))))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 s5) (s6 s7)))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (lift-require
         #s(stx-boundary (s0 s1))
         #s(stx-boundary s2)
         .
         #s(stx-boundary s2))
        (macro-post-x #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (exit-macro #s(stx-boundary s0) . #s(stx-boundary s0))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->letrec () () #s(stx-boundary s0))
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (finish-block #s(stx-boundary (s0 () s1)))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (lift-loop . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (prim-begin . #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1)))
        (prim-require . #s(stx-boundary (s0 s1)))
        (exit-prim/return . #s(stx-boundary (s0 s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (visit . #s(stx-boundary (s0 () (s0 () s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (prim-let-values . #s(stx-boundary (s0 () (s0 () s1))))
        (letX-renames () () () () #s(stx-boundary (s0 () s1)))
        (enter-block #s(stx-boundary (s0 () s1)))
        (block-renames
         (#s(stx-boundary (s0 () s1)))
         #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 () s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-let-values . #s(stx-boundary (s0 () s1)))
        (letX-renames () () () () #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 () s1)))
        (exit-list #s(stx-boundary (s0 () s1)))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 () (s4 () s5))))))))
      ((begin 1 __x (+ 3 4))
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (visit . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (visit . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 1 s2 (s3 3 4)))))
        (visit . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (prim-begin . #s(stx-boundary (s0 1 s1 (s2 3 4))))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 1)) . #s(stx-boundary 1))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #s(stx-boundary (s0 . 1)))
        (exit-prim/return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . s1)) . #s(stx-boundary s1))
        (enter-prim . #s(stx-boundary (s0 . s1)))
        (prim-#%top . #s(stx-boundary (s0 . s1)))
        (exit-prim/return . #s(stx-boundary (s0 . s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 3 4)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 3 4)) . #s(stx-boundary (s1 3 4)))
        (enter-macro
         #s(stx-boundary (s0 s1 3 4))
         .
         #s(stx-boundary (s0 s1 3 4)))
        (macro-pre-x . #s(stx-boundary (s0 s1 3 4)))
        (macro-post-x
         #s(stx-boundary (s0 s1 3 4))
         .
         #s(stx-boundary (s0 s1 3 4)))
        (exit-macro
         #s(stx-boundary (s0 s1 3 4))
         .
         #s(stx-boundary (s0 s1 3 4)))
        (visit . #s(stx-boundary (s0 s1 3 4)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 3 4)))
        (prim-#%app . #s(stx-boundary (s0 s1 3 4)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 3))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 3)) . #s(stx-boundary 3))
        (enter-prim . #s(stx-boundary (s0 . 3)))
        (prim-#%datum . #s(stx-boundary (s0 . 3)))
        (exit-prim/return . #s(stx-boundary (s0 3)))
        (next . #f)
        (visit . #s(stx-boundary 4))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 4)) . #s(stx-boundary 4))
        (enter-prim . #s(stx-boundary (s0 . 4)))
        (prim-#%datum . #s(stx-boundary (s0 . 4)))
        (exit-prim/return . #s(stx-boundary (s0 4)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 3) (s2 4))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 1) (s2 . s3) (s4 s5 (s1 3) (s1 4)))))
        (exit-prim/return
         .
         #s(stx-boundary (s0 (s1 (s2 1) (s3 . s4) (s5 s6 (s2 3) (s2 4))))))))
      ((let ()
         (define-syntax (lift stx) (syntax-local-lift-expression #'(+ 1 2)))
         (lift))
       .
       ((start-top . #f)
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (stop/return
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (visit
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (prim-#%expression
         .
         #s(stx-boundary (s0 (s1 () (s2 (s3 s4) (s5 (s6 (s7 1 2)))) (s3)))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2)))
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
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2)))
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (visit . #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (prim-let-values
         .
         #s(stx-boundary (s0 () (s1 (s2 s3) (s4 (s5 (s6 1 2)))) (s2))))
        (letX-renames
         ()
         ()
         ()
         ()
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2)))))
         #s(stx-boundary (s1)))
        (enter-block
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2)))))
         #s(stx-boundary (s1)))
        (block-renames
         (#s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2)))))
          #s(stx-boundary (s1)))
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2)))))
         #s(stx-boundary (s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2)))))
         .
         #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (macro-pre-x . #s(stx-boundary (s0 (s1 s2) (s3 (s4 (s5 1 2))))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2))))))
         .
         #s(stx-boundary (s7 (s1 s3) (s4 (s5 (s6 1 2))))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2))))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (prim-define-syntaxes
         .
         #s(stx-boundary (s0 (s1) (s2 (s3) (s4 (s5 (s6 1 2)))))))
        (rename-one
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 (s2) (s3 (s4 (s5 1 2))))))
        (prepare-env . #f)
        (enter-bind . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (macro-pre-x . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (macro-post-x
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2)))))
         .
         #s(stx-boundary (s5 (s1) (s2 (s3 (s4 1 2))))))
        (exit-macro
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2)))))
         .
         #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (visit . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (prim-lambda . #s(stx-boundary (s0 (s1) (s2 (s3 (s4 1 2))))))
        (lambda-renames
         #s(stx-boundary (s0))
         #s(stx-boundary (s1 (s2 (s3 1 2)))))
        (enter-block #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (block-renames
         (#s(stx-boundary (s0 (s1 (s2 1 2)))))
         #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 (s2 1 2)))))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s1 (s2 (s3 1 2)))))
        (enter-macro
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (macro-pre-x . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (macro-post-x
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (exit-macro
         #s(stx-boundary (s0 s1 (s2 (s3 1 2))))
         .
         #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (visit . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (prim-#%app . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-macro
         #s(stx-boundary (s0 (s1 1 2)))
         .
         #s(stx-boundary (s0 (s1 1 2))))
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
        (exit-macro
         #s(stx-boundary (s0 (s1 1 2)))
         .
         #s(stx-boundary (s0 (s1 1 2))))
        (visit . #s(stx-boundary (s0 (s1 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2))))
        (prim-quote-syntax . #s(stx-boundary (s0 (s1 1 2))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 1 2))))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (exit-list #s(stx-boundary (s0 s1 (s2 (s3 1 2)))))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 (s5 1 2))))))
        (next . #f)
        (exit-bind . #f)
        (next . #f)
        (visit . #s(stx-boundary (s0)))
        (resolve . #s(stx-boundary s0))
        (enter-macro #s(stx-boundary (s0)) . #s(stx-boundary (s0)))
        (macro-pre-x . #s(stx-boundary (s0)))
        (lift-expr
         (#s(stx-boundary s0))
         #s(stx-boundary (s1 1 2))
         .
         #s(stx-boundary (s1 1 2)))
        (macro-post-x #s(stx-boundary s0) . #s(stx-boundary (s1)))
        (exit-macro #s(stx-boundary s0) . #s(stx-boundary s0))
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->letrec () () #s(stx-boundary s0))
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (finish-block #s(stx-boundary (s0 () s1)))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
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
        (prim-begin
         .
         #s(stx-boundary (s0 (s1 (s2) (s3 1 2)) (s4 (s5 () (s5 () s2))))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (prim-define-values . #s(stx-boundary (s0 (s1) (s2 1 2))))
        (visit . #s(stx-boundary (s0 1 2)))
        (resolve . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 s1 1 2)) . #s(stx-boundary (s1 1 2)))
        (enter-macro
         #s(stx-boundary (s0 s1 1 2))
         .
         #s(stx-boundary (s0 s1 1 2)))
        (macro-pre-x . #s(stx-boundary (s0 s1 1 2)))
        (macro-post-x
         #s(stx-boundary (s0 s1 1 2))
         .
         #s(stx-boundary (s0 s1 1 2)))
        (exit-macro
         #s(stx-boundary (s0 s1 1 2))
         .
         #s(stx-boundary (s0 s1 1 2)))
        (visit . #s(stx-boundary (s0 s1 1 2)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 s1 1 2)))
        (prim-#%app . #s(stx-boundary (s0 s1 1 2)))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 1)) . #s(stx-boundary 1))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #s(stx-boundary (s0 . 1)))
        (exit-prim/return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 2)) . #s(stx-boundary 2))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #s(stx-boundary (s0 . 2)))
        (exit-prim/return . #s(stx-boundary (s0 2)))
        (exit-prim/return . #s(stx-boundary (s0 s1 (s2 1) (s2 2))))
        (exit-prim/return . #s(stx-boundary (s0 (s1) (s2 s3 (s4 1) (s4 2)))))
        (next . #f)
        (visit . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (visit . #s(stx-boundary (s0 () (s0 () s1))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () (s0 () s1))))
        (prim-let-values . #s(stx-boundary (s0 () (s0 () s1))))
        (letX-renames () () () () #s(stx-boundary (s0 () s1)))
        (enter-block #s(stx-boundary (s0 () s1)))
        (block-renames
         (#s(stx-boundary (s0 () s1)))
         #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 () s1)))
        (block->list . #f)
        (enter-list #s(stx-boundary (s0 () s1)))
        (next . #f)
        (visit . #s(stx-boundary (s0 () s1)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 () s1)))
        (prim-let-values . #s(stx-boundary (s0 () s1)))
        (letX-renames () () () () #s(stx-boundary s0))
        (enter-block #s(stx-boundary s0))
        (block-renames (#s(stx-boundary s0)) #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary s0))
        (block->list . #f)
        (enter-list #s(stx-boundary s0))
        (next . #f)
        (visit . #s(stx-boundary s0))
        (resolve . #s(stx-boundary s0))
        (variable #s(stx-boundary s0) . #s(stx-boundary s0))
        (return . #s(stx-boundary s0))
        (exit-list #s(stx-boundary s0))
        (exit-prim/return . #s(stx-boundary (s0 () s1)))
        (exit-list #s(stx-boundary (s0 () s1)))
        (exit-prim/return . #s(stx-boundary (s0 () (s0 () s1))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 () (s1 () s2)))))
        (exit-prim/return
         .
         #s(stx-boundary
            (s0 (s1 (s2) (s3 s4 (s5 1) (s5 2))) (s6 (s7 () (s7 () s2))))))))
      ((if 1 2 3)
       .
       ((start-top . #f)
        (visit . #s(stx-boundary (s0 (s1 1 2 3))))
        (visit . #s(stx-boundary (s0 (s1 1 2 3))))
        (resolve . #s(stx-boundary s0))
        (stop/return . #s(stx-boundary (s0 (s1 1 2 3))))
        (visit . #s(stx-boundary (s0 (s1 1 2 3))))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 (s1 1 2 3))))
        (prim-#%expression . #s(stx-boundary (s0 (s1 1 2 3))))
        (visit . #s(stx-boundary (s0 1 2 3)))
        (resolve . #s(stx-boundary s0))
        (enter-prim . #s(stx-boundary (s0 1 2 3)))
        (prim-if . #s(stx-boundary (s0 1 2 3)))
        (visit . #s(stx-boundary 1))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 1)) . #s(stx-boundary 1))
        (enter-prim . #s(stx-boundary (s0 . 1)))
        (prim-#%datum . #s(stx-boundary (s0 . 1)))
        (exit-prim/return . #s(stx-boundary (s0 1)))
        (next . #f)
        (visit . #s(stx-boundary 2))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 2)) . #s(stx-boundary 2))
        (enter-prim . #s(stx-boundary (s0 . 2)))
        (prim-#%datum . #s(stx-boundary (s0 . 2)))
        (exit-prim/return . #s(stx-boundary (s0 2)))
        (next . #f)
        (visit . #s(stx-boundary 3))
        (resolve . #s(stx-boundary s0))
        (tag2 #s(stx-boundary (s0 . 3)) . #s(stx-boundary 3))
        (enter-prim . #s(stx-boundary (s0 . 3)))
        (prim-#%datum . #s(stx-boundary (s0 . 3)))
        (exit-prim/return . #s(stx-boundary (s0 3)))
        (exit-prim/return . #s(stx-boundary (s0 (s1 1) (s1 2) (s1 3))))
        (exit-prim/return . #s(stx-boundary (s0 (s1 (s2 1) (s2 2) (s2 3))))))))
