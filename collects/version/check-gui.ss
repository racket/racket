(module check-gui mzscheme
  (require "private/gui-defs.ss" "private/go-check.ss" (lib "etc.ss"))
  (provide check-version)
  (define check-version
    (opt-lambda ([parent-frame #f] [sync? #f])
      (go-check parent-frame sync? gui-defs@))))
