#lang scheme/base

(require (for-syntax scheme/base)
         scribble/manual
         (for-label mzscheme))

(provide mzlib
         (all-from-out scribble/manual)
         (for-label (all-from-out mzscheme)))

(define-syntax (mzlib stx)
  (syntax-case stx ()
    [(_ #:mode section name)
     (with-syntax ([lib (string->symbol
                         (format "mzlib/~a" (syntax-e #'name)))])
       #'(begin
           (section #:style 'hidden (scheme lib))
           (defmodule lib)))]
    [(_ name) #'(mzlib #:mode section name)]))
