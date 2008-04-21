#lang scheme/base

(require scribble/manual
         (for-label embedded-gui
                    scheme/base
                    scheme/contract
                    scheme/class
                    scheme/gui/base))
(provide (all-from-out scribble/manual)
         (for-label (all-from-out embedded-gui
                                  scheme/base
                                  scheme/contract
                                  scheme/class
                                  scheme/gui/base)))

