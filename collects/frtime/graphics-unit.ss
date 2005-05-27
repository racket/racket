(module graphics-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-posn-less-unit.ss")
  (provide graphics@)

  (define graphics@
    (compound-unit/sig
      (import [mred : mred^])
      (link [p : graphics:posn^ 
	       ((unit/sig graphics:posn^ (import) (define-struct posn (x y))))]
	    [g : graphics:posn-less^ (graphics-posn-less@ mred p)])
      (export
       (open p)
       (open g)))))