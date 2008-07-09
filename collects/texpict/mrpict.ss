
(module mrpict mzscheme
  (require mzlib/unit
           mzlib/contract
           mzlib/class
           mred)

  (require mred/mred-sig
	   mred/mred-unit)
  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss")
  (require "mrpict-sig.ss"
	   "mrpict-unit.ss")

  (define-compound-unit/infer mrpict+mred@
    (import)
    (export texpict-common^ mrpict-extra^)
    (link standard-mred@ mrpict@))
  
  (define-values/invoke-unit/infer mrpict+mred@)

  (provide-signature-elements texpict-common^)
  (provide
   dc-for-text-size
   show-pict
   caps-text current-expected-text-scale
   dc
   linewidth
   
   draw-pict
   make-pict-drawer)
  
  (define family/c
    (symbols 'base 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))

  (define text-style/c
    (flat-rec-contract
     text-style/c
     (or/c null?
           (is-a?/c font%)
           family/c
           string? ;; could be more specific, I guess.
           (cons/c string? family/c)
           (cons/c (symbols 'bold 'italic 'superscript 'subscript 'combine 'no-combine 'caps)
                   text-style/c))))
  
  (provide/contract
   [text (opt-> (string?)
                (text-style/c 
                 (and/c (between/c 1 255) integer?)
                 number?)
                pict?)])
  
  (provide text-style/c))
