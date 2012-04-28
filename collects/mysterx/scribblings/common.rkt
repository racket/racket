#lang at-exp scheme/base

(require scribble/manual
         scribble/core
         (for-label mysterx
                    scheme/class
                    scheme/base
                    scheme/contract))

(provide deprecated yellow
         (all-from-out scribble/manual)
         (for-label (all-from-out mysterx
                                  scheme/class
                                  scheme/base
                                  scheme/contract)))

(define (yellow . content)
  (make-element (make-style
                 #f
                 (list (make-background-color-property "yellow")))
                content))

(define (deprecated2)
  @para{@yellow{@bold{WARNING:}}
         All bindings in this section are scheduled for removal after version 5.2.1.})


