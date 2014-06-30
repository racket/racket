#lang racket/base
  (require mzlib/unit
           racket/contract
           racket/class
           racket/draw)

  (require racket/draw/draw-sig
           racket/draw/draw-unit
           "private/mrpict-sig.rkt"
           "private/common-sig.rkt"
           "mrpict-sig.rkt"
           "mrpict-unit.rkt")

  (define-compound-unit/infer mrpict+mred@
    (import)
    (export texpict-common^ mrpict-extra^)
    (link draw@ mrpict@))
  
  (define-values/invoke-unit/infer mrpict+mred@)

  (provide-signature-elements texpict-common^)
  (provide
   dc-for-text-size
   convert-bounds-padding
   show-pict
   caps-text current-expected-text-scale
   dc
   linewidth
   linestyle
   
   draw-pict
   make-pict-drawer)
  
  (define family/c
    (or/c 'base 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))

  (define text-style/c
    (flat-rec-contract
     text-style/c
     (or/c null?
           (is-a?/c font%)
           family/c
           string? ;; could be more specific, I guess.
           (cons/c string? family/c)
           (cons/c (or/c 'bold 'italic 'superscript 'subscript 'combine 'no-combine 'caps
                         'outline 'aligned 'unaligned
                         (is-a?/c color%))
                   text-style/c))))
  
  (provide/contract
   [text (->* (string?)
              (text-style/c 
               (and/c (between/c 1 1024) integer?)
               number?)
              pict?)])
  
  (provide text-style/c)
