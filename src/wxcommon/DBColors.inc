
/*

; Generating source:

(require (lib "list.ss") (lib "string.ss"))

;; The colors:

(define l
  '(("AliceBlue" 240 248 255)
   ("AntiqueWhite" 250 235 215)
   ("Aqua" 0 255 255)
   ("Azure" 240 255 255)
   ("Beige" 245 245 220)
   ("Bisque" 255 228 196)
   ("BlanchedAlmond" 255 235 205)
   ("BlueViolet" 138 43 226)
   ("Burlywood" 222 184 135)
   ("CadetBlue" 95 158 160)
   ("Chartreuse" 127 255 0)
   ("Chocolate" 210 105 30)
   ("CornflowerBlue" 100 149 237)
   ("Cornsilk" 255 248 220)
   ("Crimson" 220 20 60)
   ("DarkBlue" 0 0 139)
   ("DarkCyan" 0 139 139)
   ("DarkGoldenrod" 184 134 11)
   ("DarkGray" 169 169 169)
   ("DarkGreen" 0 100 0)
   ("DarkKhaki" 189 183 107)
   ("DarkMagenta" 139 0 139)
   ("DarkOliveGreen" 85 107 47)
   ("DarkOrange" 255 140 0)
   ("DarkOrchid" 153 50 204)
   ("DarkRed" 139 0 0)
   ("DarkSalmon" 233 150 122)
   ("DarkSeaGreen" 143 188 139)
   ("DarkSlateBlue" 72 61 139)
   ("DarkSlateGray" 47 79 79)
   ("DarkTurquoise" 0 206 209)
   ("DarkViolet" 148 0 211)
   ("DeepPink" 255 20 147)
   ("DeepSkyBlue" 0 191 255)
   ("DimGray" 105 105 105)
   ("DodgerBlue" 30 144 255)
   ("FloralWhite" 255 250 240)
   ("ForestGreen" 34 139 34)
   ("Fuchsia" 255 0 255)
   ("Gainsboro" 220 220 220)
   ("GhostWhite" 248 248 255)
   ("GreenYellow" 173 255 47)
   ("Honeydew" 240 255 240)
   ("HotPink" 255 105 180)
   ("IndianRed" 205 92 92)
   ("Indigo" 75 0 130)
   ("Ivory" 255 255 240)
   ("Lavender" 230 230 250)
   ("LavenderBlush" 255 240 245)
   ("LawnGreen" 124 252 0)
   ("LemonChiffon" 255 250 205)
   ("LightBlue" 173 216 230)
   ("LightCoral" 240 128 128)
   ("LightCyan" 224 255 255)
   ("LightGoldenrodYellow" 250 250 210)
   ("LightGreen" 144 238 144)
   ("LightGray" 211 211 211)
   ("LightPink" 255 182 193)
   ("LightSalmon" 255 160 122)
   ("LightSeaGreen" 32 178 170)
   ("LightSkyBlue" 135 206 250)
   ("LightSlateGray" 119 136 153)
   ("LightSteelBlue" 176 196 222)
   ("LightYellow" 255 255 224)
   ("Lime" 0 255 0)
   ("LimeGreen" 50 205 50)
   ("Linen" 250 240 230)
   ("MediumAquamarine" 102 205 170)
   ("MediumBlue" 0 0 205)
   ("MediumOrchid" 186 85 211)
   ("MediumPurple" 147 112 219)
   ("MediumSeaGreen" 60 179 113)
   ("MediumSlateBlue" 123 104 238)
   ("MediumSpringGreen" 0 250 154)
   ("MediumTurquoise" 72 209 204)
   ("MediumVioletRed" 199 21 133)
   ("MidnightBlue" 25 25 112)
   ("MintCream" 245 255 250)
   ("MistyRose" 255 228 225)
   ("Moccasin" 255 228 181)
   ("NavajoWhite" 255 222 173)
   ("OldLace" 253 245 230)
   ("Olive" 128 128 0)
   ("OliveDrab" 107 142 35)
   ("OrangeRed" 255 69 0)
   ("PaleGoldenrod" 238 232 170)
   ("PaleGreen" 152 251 152)
   ("PaleTurquoise" 175 238 238)
   ("PaleVioletRed" 219 112 147)
   ("PapayaWhip" 255 239 213)
   ("PeachPuff" 255 218 185)
   ("Peru" 205 133 63)
   ("PowderBlue" 176 224 230)
   ("RosyBrown" 188 143 143)
   ("RoyalBlue" 65 105 225)
   ("SaddleBrown" 139 69 19)
   ("SandyBrown" 244 164 96)
   ("SeaGreen" 46 139 87)
   ("SeaShell" 255 245 238)
   ("Silver" 192 192 192)
   ("SkyBlue" 135 206 235)
   ("SlateBlue" 106 90 205)
   ("SlateGray" 112 128 144)
   ("Snow" 255 250 250)
   ("SpringGreen" 0 255 127)
   ("SteelBlue" 70 130 180)
   ("Teal" 0 128 128)
   ("Tomato" 255 99 71)
   ("WhiteSmoke" 245 245 245)
   ("YellowGreen" 154 205 50)
 
   ("Aquamarine" 112  216  144)
   ("Black" 0  0  0)
   ("Blue" 0  0  255)
   ("Blue Violet" 138  43  226)
   ("Brown" 132  60  36)
   ("Cadet Blue" 96  160  160)
   ("Coral" 255  127  80)
   ("Cornflower Blue" 68  64  108)
   ("Cyan" 0  255  255)
   ("Dark Gray" 169  169  169)
   ("Dark Green" 0  100  0)
   ("Dark Olive Green" 85  107  47)
   ("Dark Orchid" 153  50  204)
   ("Dark Slate Blue" 72  61  139)
   ("Dark Slate Gray" 47  79  79)
   ("Dark Turquoise" 0  206  209)
   ("Dim Gray" 105  105  105)
   ("Firebrick" 178  34  34)
   ("Forest Green" 34  139  34)
   ("Gold" 255  215  0)
   ("Goldenrod" 218  165  32)
   ("Gray" 190  190  190)
   ("Green" 0  255  0)
   ("Green Yellow" 173  255  47)
   ("Indian Red" 205  92  92)
   ("Khaki" 240  230  140)
   ("Light Blue" 173  216  230)
   ("Light Gray" 211  211  211)
   ("Light Steel Blue" 176  196  222)
   ("Lime Green" 50  205  50)
   ("Magenta" 255  0  255)
   ("Maroon" 176  48  96)
   ("Medium Aquamarine" 102  205  170)
   ("Medium Blue" 0  0  205)
   ("Medium Forest Green" 107  142  35)
   ("MediumForestGreen" 107  142  35)
   ("Medium Goldenrod" 234  234  173)
   ("MediumGoldenrod" 234  234  173)
   ("Medium Orchid" 186  85  211)
   ("Medium Sea Green" 60  179  113)
   ("Medium Slate Blue" 123  104  238)
   ("Medium Spring Green" 0  250  154)
   ("Medium Turquoise" 72  209  204)
   ("Medium Violet Red" 199  21  133)
   ("Midnight Blue" 25  25  112)
   ("Navy" 36  36  140)
   ("Orange" 255  165  0)
   ("Orange Red" 255  69  0)
   ("Orchid" 218  112  214)
   ("Pale Green" 152  251  152)
   ("Pink" 255  192  203)
   ("Plum" 221  160  221)
   ("Purple" 160  32  240)
   ("Red" 255  0  0)
   ("Salmon" 250  128  114)
   ("Sea Green" 46  139  87)
   ("Sienna" 160  82  45)
   ("Sky Blue" 135  206  235)
   ("Slate Blue" 106  90  205)
   ("Spring Green" 0  255  127)
   ("Steel Blue" 70  130  180)
   ("Tan" 210  180  140)
   ("Thistle" 216  191  216)
   ("Turquoise" 64  224  208)
   ("Violet" 238  130  238)
   ("Violet Red" 208  32  144)
   ("VioletRed" 208  32  144)
   ("Wheat" 245  222  179)
   ("White" 255  255  255)
   ("Yellow" 255  255  0)
   ("Yellow Green" 154  205  50)))

(define red cadr)
(define green caddr)
(define blue cadddr)

(define (brightness l)
  (sqrt (+ (expt (red l) 2)
	   (expt (green l) 2)
	   (expt (cadddr l) 2))))

(define (redness l) (/ (red l) (add1 (brightness l))))
(define (greeness l) (/ (green l) (add1 (brightness l))))
(define (blueness l) (/ (blue l) (add1 (brightness l))))


;; Sort as:
;;         bluish
;;  Red
;;         greenish
;; -----
;;         reddish
;;  Green
;;         bluish
;; -----
;;         greenish
;;  Blue
;;         reddish
(define l2 (quicksort l (lambda (a b)
			  (let ([ar (redness a)]
				[ag (greeness a)]
				[ab (blueness a)]
				[br (redness b)]
				[bg (greeness b)]
				[bb (blueness b)])
			    (cond
			     [(and (>= ar ag)
				   (> ar ab))
			      ;; a is a shade of red
			      (cond
			       [(and (>= br bg)
				     (> br bb))
				;; b is a shade of red
				(cond
				 [(> ag ab)
				  ;; a is greener than blue
				  (cond
				   [(> bg bb)
				    ;; b is greener than blue
				    (> ar br)]
				   [else #t])]
				 [else 
				  ;; a is bluer than green
				  (cond
				   [(> bg bb)
				    ;; b is greener than blue
				    #t]
				   [else
				    (> ar br)])])]
			       [else #t])]
			     [else
			      (cond
			       [(and (>= br bg)
				     (> br bb))
				;; b is a shade of red
				#f]
			       [(and (> ag ar)
				     (> ag ab))
				;; a is a shade of green
				(cond
				 [(and (> bg br)
				       (> bg bb))
				  ;; b is a shade of green
				  (cond
				   [(> ar ab)
				    ;; a is redder than blue
				    (cond
				     [(> br bb)
				      ;; b is redder than blue
				      (> ag bg)]
				     [else #t])]
				   [else
				    (cond
				     [(> br bb)
				      #f]
				     [else
				      (> ag bg)])])]
				 [else #t])]
			       [else
				;; a is a shade of blue
				(cond
				 [(and (> bg br)
				       (> bg bb))
				  ;; b is a shade of green
				  #f]
				 [else 
				  ;; b is a shade of blue
				  (cond
				   [(> ag ar)
				    ;; a is greener than red
				    (cond
				     [(> bg br)
				      ;; b is greener than red
				      (> ab bb)]
				     [else #t])]
				   [else
				    (cond
				     [(> bg br)
				      #f]
				     [else
				      (> ab bb)])])])])])))))

;; pairwise swap to put names with spaces first:
(define l3 (let loop ([l l2])
	     (cond
	      [(null? (cdr l)) l]
	      [else (let ([l (cons (car l) (loop (cdr l)))])
		      (cond
		       [(and (> (string-length (caadr l))
				(string-length (caar l)))
			     (string=? (caar l)
				       (regexp-replace* " " (caadr l) "")))
			(list* (cadr l) (car l) (cddr l))]
		       [else l]))])))

(define (2hex n)
  (let ([s (format "00~x" n)])
    (substring s (- (string-length s) 2) (string-length s))))

(for-each (lambda (l)
	    (let ([s (string-copy (car l))]
		  [c (regexp-replace* " " (car l) "")])
	      (string-uppercase! s)
	      (cond
	       ;; Debugging: simple HTML output
	       [#f (printf "<tr><td bgcolor=\"#~a~a~a\">&nbsp;&nbsp;&nbsp;&nbsp;</td><td>~a</td></tr>~n"
			   (2hex (red l)) (2hex (green l)) (2hex (blue l)) (car l))]
	       ;; LaTex output:
	       [#t (printf "\\colorName{~a}{~a}{~a}{~a}{~a} \\\\~n"
			   (car l)
			   c
			   (red l)
			   (green l)
			   (blue l))]
	       ;; C output:
	       [#t (printf "C_APPEND(~s, new wxColour(~a, ~a, ~a));~n"
			   s
			   (red l)
			   (green l)
			   (blue l))])))
	  l3)
*/

APPEND_C("ALICEBLUE", new wxColour(240, 248, 255));
APPEND_C("ANTIQUEWHITE", new wxColour(250, 235, 215));
APPEND_C("AQUA", new wxColour(0, 255, 255));
APPEND_C("AQUAMARINE", new wxColour(112, 216, 144));
APPEND_C("AZURE", new wxColour(240, 255, 255));
APPEND_C("BEIGE", new wxColour(245, 245, 220));
APPEND_C("BISQUE", new wxColour(255, 228, 196));
APPEND_C("BLACK", new wxColour(0, 0, 0));
APPEND_C("BLANCHEDALMOND", new wxColour(255, 235, 205));
APPEND_C("BLUE", new wxColour(0, 0, 255));
APPEND_C("BLUE VIOLET", new wxColour(138, 43, 226));
APPEND_C("BLUEVIOLET", new wxColour(138, 43, 226));
APPEND_C("BROWN", new wxColour(132, 60, 36));
APPEND_C("BURLYWOOD", new wxColour(222, 184, 135));
APPEND_C("CADET BLUE", new wxColour(96, 160, 160));
APPEND_C("CADETBLUE", new wxColour(95, 158, 160));
APPEND_C("CHARTREUSE", new wxColour(127, 255, 0));
APPEND_C("CHOCOLATE", new wxColour(210, 105, 30));
APPEND_C("CORAL", new wxColour(255, 127, 80));
APPEND_C("CORNFLOWER BLUE", new wxColour(68, 64, 108));
APPEND_C("CORNFLOWERBLUE", new wxColour(100, 149, 237));
APPEND_C("CORNSILK", new wxColour(255, 248, 220));
APPEND_C("CRIMSON", new wxColour(220, 20, 60));
APPEND_C("CYAN", new wxColour(0, 255, 255));
APPEND_C("DARK GRAY", new wxColour(169, 169, 169));
APPEND_C("DARK GREEN", new wxColour(0, 100, 0));
APPEND_C("DARK OLIVE GREEN", new wxColour(85, 107, 47));
APPEND_C("DARK ORCHID", new wxColour(153, 50, 204));
APPEND_C("DARK SLATE BLUE", new wxColour(72, 61, 139));
APPEND_C("DARK SLATE GRAY", new wxColour(47, 79, 79));
APPEND_C("DARK TURQUOISE", new wxColour(0, 206, 209));
APPEND_C("DARKBLUE", new wxColour(0, 0, 139));
APPEND_C("DARKCYAN", new wxColour(0, 139, 139));
APPEND_C("DARKGOLDENROD", new wxColour(184, 134, 11));
APPEND_C("DARKGRAY", new wxColour(169, 169, 169));
APPEND_C("DARKGREEN", new wxColour(0, 100, 0));
APPEND_C("DARKKHAKI", new wxColour(189, 183, 107));
APPEND_C("DARKMAGENTA", new wxColour(139, 0, 139));
APPEND_C("DARKOLIVEGREEN", new wxColour(85, 107, 47));
APPEND_C("DARKORANGE", new wxColour(255, 140, 0));
APPEND_C("DARKORCHID", new wxColour(153, 50, 204));
APPEND_C("DARKRED", new wxColour(139, 0, 0));
APPEND_C("DARKSALMON", new wxColour(233, 150, 122));
APPEND_C("DARKSEAGREEN", new wxColour(143, 188, 139));
APPEND_C("DARKSLATEBLUE", new wxColour(72, 61, 139));
APPEND_C("DARKSLATEGRAY", new wxColour(47, 79, 79));
APPEND_C("DARKTURQUOISE", new wxColour(0, 206, 209));
APPEND_C("DARKVIOLET", new wxColour(148, 0, 211));
APPEND_C("DEEPPINK", new wxColour(255, 20, 147));
APPEND_C("DEEPSKYBLUE", new wxColour(0, 191, 255));
APPEND_C("DIM GRAY", new wxColour(105, 105, 105));
APPEND_C("DIMGRAY", new wxColour(105, 105, 105));
APPEND_C("DODGERBLUE", new wxColour(30, 144, 255));
APPEND_C("FIREBRICK", new wxColour(178, 34, 34));
APPEND_C("FLORALWHITE", new wxColour(255, 250, 240));
APPEND_C("FOREST GREEN", new wxColour(34, 139, 34));
APPEND_C("FORESTGREEN", new wxColour(34, 139, 34));
APPEND_C("FUCHSIA", new wxColour(255, 0, 255));
APPEND_C("GAINSBORO", new wxColour(220, 220, 220));
APPEND_C("GHOSTWHITE", new wxColour(248, 248, 255));
APPEND_C("GOLD", new wxColour(255, 215, 0));
APPEND_C("GOLDENROD", new wxColour(218, 165, 32));
APPEND_C("GRAY", new wxColour(190, 190, 190));
APPEND_C("GREEN", new wxColour(0, 255, 0));
APPEND_C("GREEN YELLOW", new wxColour(173, 255, 47));
APPEND_C("GREENYELLOW", new wxColour(173, 255, 47));
APPEND_C("HONEYDEW", new wxColour(240, 255, 240));
APPEND_C("HOTPINK", new wxColour(255, 105, 180));
APPEND_C("INDIAN RED", new wxColour(205, 92, 92));
APPEND_C("INDIANRED", new wxColour(205, 92, 92));
APPEND_C("INDIGO", new wxColour(75, 0, 130));
APPEND_C("IVORY", new wxColour(255, 255, 240));
APPEND_C("KHAKI", new wxColour(240, 230, 140));
APPEND_C("LAVENDER", new wxColour(230, 230, 250));
APPEND_C("LAVENDERBLUSH", new wxColour(255, 240, 245));
APPEND_C("LAWNGREEN", new wxColour(124, 252, 0));
APPEND_C("LEMONCHIFFON", new wxColour(255, 250, 205));
APPEND_C("LIGHT BLUE", new wxColour(173, 216, 230));
APPEND_C("LIGHT GRAY", new wxColour(211, 211, 211));
APPEND_C("LIGHT STEEL BLUE", new wxColour(176, 196, 222));
APPEND_C("LIGHTBLUE", new wxColour(173, 216, 230));
APPEND_C("LIGHTCORAL", new wxColour(240, 128, 128));
APPEND_C("LIGHTCYAN", new wxColour(224, 255, 255));
APPEND_C("LIGHTGOLDENRODYELLOW", new wxColour(250, 250, 210));
APPEND_C("LIGHTGRAY", new wxColour(211, 211, 211));
APPEND_C("LIGHTGREEN", new wxColour(144, 238, 144));
APPEND_C("LIGHTPINK", new wxColour(255, 182, 193));
APPEND_C("LIGHTSALMON", new wxColour(255, 160, 122));
APPEND_C("LIGHTSEAGREEN", new wxColour(32, 178, 170));
APPEND_C("LIGHTSKYBLUE", new wxColour(135, 206, 250));
APPEND_C("LIGHTSLATEGRAY", new wxColour(119, 136, 153));
APPEND_C("LIGHTSTEELBLUE", new wxColour(176, 196, 222));
APPEND_C("LIGHTYELLOW", new wxColour(255, 255, 224));
APPEND_C("LIME", new wxColour(0, 255, 0));
APPEND_C("LIME GREEN", new wxColour(50, 205, 50));
APPEND_C("LIMEGREEN", new wxColour(50, 205, 50));
APPEND_C("LINEN", new wxColour(250, 240, 230));
APPEND_C("MAGENTA", new wxColour(255, 0, 255));
APPEND_C("MAROON", new wxColour(176, 48, 96));
APPEND_C("MEDIUM AQUAMARINE", new wxColour(102, 205, 170));
APPEND_C("MEDIUM BLUE", new wxColour(0, 0, 205));
APPEND_C("MEDIUM FOREST GREEN", new wxColour(107, 142, 35));
APPEND_C("MEDIUM GOLDENROD", new wxColour(234, 234, 173));
APPEND_C("MEDIUM ORCHID", new wxColour(186, 85, 211));
APPEND_C("MEDIUM SEA GREEN", new wxColour(60, 179, 113));
APPEND_C("MEDIUM SLATE BLUE", new wxColour(123, 104, 238));
APPEND_C("MEDIUM SPRING GREEN", new wxColour(0, 250, 154));
APPEND_C("MEDIUM TURQUOISE", new wxColour(72, 209, 204));
APPEND_C("MEDIUM VIOLET RED", new wxColour(199, 21, 133));
APPEND_C("MEDIUMAQUAMARINE", new wxColour(102, 205, 170));
APPEND_C("MEDIUMBLUE", new wxColour(0, 0, 205));
APPEND_C("MEDIUMFORESTGREEN", new wxColour(107, 142, 35));
APPEND_C("MEDIUMGOLDENROD", new wxColour(234, 234, 173));
APPEND_C("MEDIUMORCHID", new wxColour(186, 85, 211));
APPEND_C("MEDIUMPURPLE", new wxColour(147, 112, 219));
APPEND_C("MEDIUMSEAGREEN", new wxColour(60, 179, 113));
APPEND_C("MEDIUMSLATEBLUE", new wxColour(123, 104, 238));
APPEND_C("MEDIUMSPRINGGREEN", new wxColour(0, 250, 154));
APPEND_C("MEDIUMTURQUOISE", new wxColour(72, 209, 204));
APPEND_C("MEDIUMVIOLETRED", new wxColour(199, 21, 133));
APPEND_C("MIDNIGHT BLUE", new wxColour(25, 25, 112));
APPEND_C("MIDNIGHTBLUE", new wxColour(25, 25, 112));
APPEND_C("MINTCREAM", new wxColour(245, 255, 250));
APPEND_C("MISTYROSE", new wxColour(255, 228, 225));
APPEND_C("MOCCASIN", new wxColour(255, 228, 181));
APPEND_C("NAVAJOWHITE", new wxColour(255, 222, 173));
APPEND_C("NAVY", new wxColour(36, 36, 140));
APPEND_C("OLDLACE", new wxColour(253, 245, 230));
APPEND_C("OLIVE", new wxColour(128, 128, 0));
APPEND_C("OLIVEDRAB", new wxColour(107, 142, 35));
APPEND_C("ORANGE", new wxColour(255, 165, 0));
APPEND_C("ORANGE RED", new wxColour(255, 69, 0));
APPEND_C("ORANGERED", new wxColour(255, 69, 0));
APPEND_C("ORCHID", new wxColour(218, 112, 214));
APPEND_C("PALE GREEN", new wxColour(152, 251, 152));
APPEND_C("PALEGOLDENROD", new wxColour(238, 232, 170));
APPEND_C("PALEGREEN", new wxColour(152, 251, 152));
APPEND_C("PALETURQUOISE", new wxColour(175, 238, 238));
APPEND_C("PALEVIOLETRED", new wxColour(219, 112, 147));
APPEND_C("PAPAYAWHIP", new wxColour(255, 239, 213));
APPEND_C("PEACHPUFF", new wxColour(255, 218, 185));
APPEND_C("PERU", new wxColour(205, 133, 63));
APPEND_C("PINK", new wxColour(255, 192, 203));
APPEND_C("PLUM", new wxColour(221, 160, 221));
APPEND_C("POWDERBLUE", new wxColour(176, 224, 230));
APPEND_C("PURPLE", new wxColour(160, 32, 240));
APPEND_C("RED", new wxColour(255, 0, 0));
APPEND_C("ROSYBROWN", new wxColour(188, 143, 143));
APPEND_C("ROYALBLUE", new wxColour(65, 105, 225));
APPEND_C("SADDLEBROWN", new wxColour(139, 69, 19));
APPEND_C("SALMON", new wxColour(250, 128, 114));
APPEND_C("SANDYBROWN", new wxColour(244, 164, 96));
APPEND_C("SEA GREEN", new wxColour(46, 139, 87));
APPEND_C("SEAGREEN", new wxColour(46, 139, 87));
APPEND_C("SEASHELL", new wxColour(255, 245, 238));
APPEND_C("SIENNA", new wxColour(160, 82, 45));
APPEND_C("SILVER", new wxColour(192, 192, 192));
APPEND_C("SKY BLUE", new wxColour(135, 206, 235));
APPEND_C("SKYBLUE", new wxColour(135, 206, 235));
APPEND_C("SLATE BLUE", new wxColour(106, 90, 205));
APPEND_C("SLATEBLUE", new wxColour(106, 90, 205));
APPEND_C("SLATEGRAY", new wxColour(112, 128, 144));
APPEND_C("SNOW", new wxColour(255, 250, 250));
APPEND_C("SPRING GREEN", new wxColour(0, 255, 127));
APPEND_C("SPRINGGREEN", new wxColour(0, 255, 127));
APPEND_C("STEEL BLUE", new wxColour(70, 130, 180));
APPEND_C("STEELBLUE", new wxColour(70, 130, 180));
APPEND_C("TAN", new wxColour(210, 180, 140));
APPEND_C("TEAL", new wxColour(0, 128, 128));
APPEND_C("THISTLE", new wxColour(216, 191, 216));
APPEND_C("TOMATO", new wxColour(255, 99, 71));
APPEND_C("TURQUOISE", new wxColour(64, 224, 208));
APPEND_C("VIOLET", new wxColour(238, 130, 238));
APPEND_C("VIOLET RED", new wxColour(208, 32, 144));
APPEND_C("VIOLETRED", new wxColour(208, 32, 144));
APPEND_C("WHEAT", new wxColour(245, 222, 179));
APPEND_C("WHITE", new wxColour(255, 255, 255));
APPEND_C("WHITESMOKE", new wxColour(245, 245, 245));
APPEND_C("YELLOW", new wxColour(255, 255, 0));
APPEND_C("YELLOW GREEN", new wxColour(154, 205, 50));
APPEND_C("YELLOWGREEN", new wxColour(154, 205, 50));
