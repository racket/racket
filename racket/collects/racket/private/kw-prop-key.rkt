(module kw-prop-key '#%kernel
  (#%provide (protect alias-of
                      kw-converted-arguments-variant-of))

  (#%declare #:cross-phase-persistent)
  
  (define-values (kw-converted-arguments-variant-of) (gensym "converted-arguments-variant-of"))
  (define-values (alias-of) (gensym "alias-of")))

