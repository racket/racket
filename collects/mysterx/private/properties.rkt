;; properties.rkt

(module properties mzscheme
  (require mzlib/list)	

  (provide
   *css-units*
   *background-attachments*
   *background-repeats*
   *text-transforms*
   *text-aligns*
   *border-widths*
   *border-styles*
   *displays*
   *font-styles*
   *font-variants*
   *font-sizes* 
   *style-floats*
   *clears*
   *horizontals*
   *verticals* 
   *decorations*
   *visibilities*
   *list-style-types*
   *list-style-positions*
   *positions*
   *overflows*
   *page-breaks*
   *cursors*
   *html-colors*
   *filters-and-official-names*
   *official-names-and-filters*
   *filters*
   *filter-official-names* 
   *filter-re*
   *alpha-filter-styles*
   *trans-filter-statuses*
   *filter-directions*
   *reveal-transitions*
   *vertical-aligns* 
   decoration?
   horizontal?
   vertical?
   font-size?
   style-float?
   clear?
   display?
   visibility?
   list-style-type?
   list-style-position? 
   position?
   overflow?
   pagebreak?
   cursor?
   filter?
   alpha-filter-style?
   trans-filter-status? 
   filter-direction?
   reveal-transition? 
   vertical-align?
   css-unit?
   border-style?)

  (define *css-units* '(em ex cm mm in pt pc px))
  (define *background-attachments* '(fixed scroll))
  (define *background-repeats* '(no-repeat repeat repeat-x repeat-y))
  (define *text-transforms* '(none capitalize uppercase lowercase))
  (define *text-aligns* '(left right center justify))
  (define *border-widths* '(medium thin thick))
  (define *border-styles* '(none dotted dashed solid double groove 
				 ridge inset outset))
  (define *displays* '(block none inline list-item 
			     table-header-group table-footer-group)) 
  (define *generic-font-families* '(serif sans-serif cursive 
					  fantasy monospace))
  (define *font-styles* '(normal italic oblique))
  (define *font-variants* '(normal small-caps))
  (define *font-sizes* '(xx-small x-small small medium large 
				  x-large xx-large
				  larger smaller))
  (define *style-floats* '(none left right))
  (define *clears* '(none left right both)) 
  (define *horizontals* '(left center right))
  (define *verticals* '(top center bottom))
  (define *decorations* '(none underline overline line-through blink))
  (define *visibilities* '(inherit visible hidden))
  (define *list-style-types* '(disc circle square decimal lower-roman 
				    upper-roman lower-alpha upper-alpha none)) 
  (define *list-style-positions* '(outside inside))
  (define *positions* '(static absolute relative))
  (define *overflows* '(visible scroll hidden auto))
  (define *page-breaks* '(always auto none))
  (define *cursors* '(auto crosshair default hand move 
			   n-resize ne-resize nw-resize s-resize 
			   se-resize sw-resize e-resize w-resize 
			   text wait help)) 
  (define *html-colors*
    '(aliceblue
      antiquewhite
      aqua
      aquamarine
      azure
      beige
      bisque
      black
      blanchedalmond
      blue
      blueviolet
      brown
      burlywood
      cadetblue
      chartreuse
      chocolate
      coral
      cornflower
      cornsilk
      crimson
      cyan
      darkblue
      darkcyan
      darkgoldenrod
      darkgray
      darkgreen
      darkkhaki
      darkmagenta
      darkolivegreen
      darkorange
      darkorchid
      darkred
      darksalmon
      darkseagreen
      darkslateblue
      darkslategray
      darkturquoise
      darkviolet
      deeppink
      deepskyblue
      dimgray
      dodgerblue
      firebrick
      floralwhite
      forestgreen
      fuchsia
      gainsboro
      ghostwhite
      gold
      goldenrod
      gray
      green
      greenyellow
      honeydew
      hotpink
      indianred
      indigo
      ivory
      khaki
      lavender
      lavenderblush
      lawngreen
      lemonchiffon
      lightblue
      lightcoral
      lightcyan
      lightgoldenrodyellow
      lightgreen
      lightgray
      lightpink
      lightsalmon
      lightseagreen
      lightskyblue
      lightslategray
      lightsteelblue
      lightyellow
      lime
      limegreen
      linen
      magenta
      maroon
      mediumaquamarine
      mediumblue
      mediumorchid
      mediumpurple
      mediumseagreen
      mediumslateblue
      mediumspringgreen
      mediumturquoise
      mediumvioletred
      midnightblue
      mintcream
      mistyrose
      moccasin
      navajowhite
      navy
      oldlace
      olive
      olivedrab
      orange
      orangered
      orchid
      palegoldenrod
      palegreen
      paleturquoise
      palevioletred
      papayawhip
      peachpuff
      peru
      pink
      plum
      powderblue
      purple
      red
      rosybrown
      royalblue
      saddlebrown
      salmon
      sandybrown
      seagreen
      seashell
      sienna
      silver
      skyblue
      slateblue
      slategray
      snow
      springgreen
      steelblue
      tan
      teal
      thistle
      tomato
      turquoise
      violet
      wheat
      white
      whitesmoke
      yellow
      yellowgreen))
  (define *filters-and-official-names*
    '((alpha alpha)
      (blend-trans blendTrans)
      (blur blur)
      (chroma chroma)
      (drop-shadow dropShadow)
      (flip-horizontal flipH)
      (flip-vertical flipV)
      (glow glow)
      (gray gray)
      (invert invert)
      (light light)
      (mask mask)
      (redirect redirect)
      (reveal-trans revealTrans)
      (shadow shadow)
      (wave wave)
      (x-ray xray)))
  (define *official-names-and-filters*
    (map (lambda (flt) (list (cadr flt)
			     (car flt)))
	 *filters-and-official-names*))
  (define *filters* (map car *filters-and-official-names*))
  (define *filter-official-names* (map cadr *filters-and-official-names*))
  (define *filter-re*
    (regexp (foldr (lambda (s t) 
		     (if (> (string-length t) 0) 
			 (string-append s "|" t) 
			 s)) "" 
			 (map symbol->string *filter-official-names*))))
  (define *alpha-filter-styles*
    '(uniform linear radial rectangular))
  (define *trans-filter-statuses*
    '(stopped applied playing))
  (define *filter-directions*
    '(0 45 90 135 180 225 270 315))
  (define *reveal-transitions*
    '(box-in box-out circle-in circle-out
      wipe-up wipe-down wipe-right wipe-left
      vertical-blinds horizontal-blinds checkerboard-across
      checkerboard-down random-dissolve split-vertical-in 
      split-vertical-out split-horizontal-in split-horizontal-out
      strips-left-down strips-left-up strips-right-down
      strips-right-up random-bars-horizontal random-bars-vertical
      random))
  (define *vertical-aligns* 
    '(baseline sub super top middle bottom text-top text-bottom ))

  ; predicates based on membership in symbol lists

  (define (make-sym-pred lst)
    (lambda (s)
      (memq s lst)))

  (define decoration? (make-sym-pred *decorations*))
  (define horizontal? (make-sym-pred *horizontals*))
  (define vertical? (make-sym-pred *verticals*))
  (define generic-font-family? (make-sym-pred *generic-font-families*))
  (define font-size? (make-sym-pred *font-sizes*))
  (define style-float? (make-sym-pred *style-floats*))
  (define clear? (make-sym-pred *clears*))
  (define display? (make-sym-pred *displays*))
  (define visibility? (make-sym-pred *visibilities*))
  (define list-style-type? (make-sym-pred *list-style-types*))
  (define list-style-position? (make-sym-pred *list-style-positions*))
  (define position? (make-sym-pred *positions*))
  (define overflow? (make-sym-pred *overflows*))
  (define pagebreak? (make-sym-pred *page-breaks*))
  (define cursor? (make-sym-pred *cursors*))
  (define filter? (make-sym-pred *filters*))
  (define alpha-filter-style? (make-sym-pred *alpha-filter-styles*))
  (define trans-filter-status? 
    (make-sym-pred *trans-filter-statuses*))
  (define filter-direction? (make-sym-pred *filter-directions*))
  (define reveal-transition? (make-sym-pred *reveal-transitions*))
  (define vertical-align? (make-sym-pred *vertical-aligns*))
  (define css-unit? (make-sym-pred *css-units*))
  (define border-style? (make-sym-pred *border-styles*)))




