(module common scheme/base
  (require scribble/manual
           scribble/basic
           scheme/class
           scheme/contract)
  (provide (all-from-out scribble/manual)
           (all-from-out scribble/basic)
           (all-from-out scheme/class)
           (all-from-out scheme/contract))

  (require (for-label scheme/gui/base
                      scheme/class
                      scheme/contract
                      scheme/base
                      framework))
  (provide (for-label (all-from-out scheme/gui/base)
                      (all-from-out scheme/class)
                      (all-from-out scheme/contract)
                      (all-from-out scheme/base)
                      (all-from-out framework))))

