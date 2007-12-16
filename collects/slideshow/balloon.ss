(module balloon scheme/base
  (require texpict/balloon)
  (provide (except-out (all-from-out texpict/balloon)
                       place-balloon)))
