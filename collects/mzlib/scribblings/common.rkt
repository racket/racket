#lang scheme/base

(require (for-syntax scheme/base)
         scribble/manual
         (for-label mzscheme
		    (only-in scheme/base exn:fail exn:fail:unsupported exn:fail:contract)))

(provide mzlib
         (all-from-out scribble/manual)
         (for-label (all-from-out mzscheme)
		    (all-from-out scheme/base)))

(define-syntax (mzlib stx)
  (syntax-case stx ()
    [(_ #:mode section name #:use-sources (src ...))
     (with-syntax ([lib (string->symbol
                         (format "mzlib/~a" (syntax-e #'name)))])
       #'(begin
           (section #:style 'hidden (racket lib))
           (defmodule lib #:use-sources (src ...))))]
    [(_ #:mode section name)
     #'(mzlib #:mode section name #:use-sources ())]
    [(_ name) 
     #'(mzlib #:mode section name #:use-sources ())]))
