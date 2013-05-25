#lang racket/base

;; (X)HTML elements etc.

(require "xml.rkt" scribble/text)

;; ----------------------------------------------------------------------------
;; Xhtml toplevel

;; creation of xhtml files requires some extra stuff
(define xhtml-prefix
  (literal
   (string-append
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
    " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")))
(provide xhtml)
(define (xhtml . body)
  (list xhtml-prefix
        (apply html 'xmlns: "http://www.w3.org/1999/xhtml" body)
        "\n"))

;; ----------------------------------------------------------------------------
;; Elements

;; For complete reference: http://www.w3.org/TR/html/dtds.html
;;  (See also http://www.w3schools.com/tags/)

;; The dtds, in increasing size:
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd

;; These are all entities, taked from the DTDs.  The ones marked with "[*]" are
;; defined later, since they need a different definition.
(define/provide-elements/not-empty
  ;; ========== Document Structure
  html
  ;; ========== Document Head
  head
  ;; The title element is not considered part of the flow of text.
  ;; It should be displayed, for example as the page header or
  ;; window title. Exactly one title is required per document.
  title
  ;; base ; document base URI, can be empty [*]
  ;; meta ; generic metainformation, can be empty [*]
  ;; link ; relationship values, can be empty [*]
  style ; style info, which may include CDATA sections
  script ; script statements, which may include CDATA sections
  noscript ; alternate content container for non script-based rendering
  ;; ========== Frames
  frameset ; only one noframes element permitted per document
  frame ; tiled window within frameset
  iframe ; inline subwindow
  noframes ; alternate content container for non frame-based rendering
  ;; ========== Document Body
  body
  div ; generic language/style container
  ;; ========== Paragraphs
  p
  ;; ========== Headings
  h1
  h2
  h3
  h4
  h5
  h6
  ;; ========== Lists
  ul ; Unordered list
  ol ; Ordered (numbered) list
  menu ; single column list (DEPRECATED)
  dir ; multiple column list (DEPRECATED)
  li ; list item
  dl ; definition lists - dt for term, dd for its definition
  dt
  dd
  ;; ========== Address
  address ; information on author
  ;; ========== Horizontal Rule
  ;; hr ; horizontal rule can be empty [*]
  ;; ========== Preformatted Text
  pre
  ;; ========== Block-like Quotes
  blockquote
  ;; ========== Text alignment
  center ; center content
  ;; ========== Inserted/Deleted Text
  ins
  del
  ;; ========== The Anchor Element
  a ; content is inline; except that anchors shouldn't be nested
  ;; ========== Inline Elements
  span ; generic language/style container
  bdo ; I18N BiDi over-ride
  ;; br ; forced line break, can be empty [*]
  em ; emphasis
  strong ; strong emphasis
  dfn ; definitional
  code ; program code
  samp ; sample
  kbd ; something user would type
  var ; variable
  cite ; citation
  abbr ; abbreviation
  acronym ; acronym
  q ; inlined quote
  sub ; subscript
  sup ; superscript
  tt ; fixed pitch font
  i ; italic font
  b ; bold font
  big ; bigger font
  small ; smaller font
  u ; underline
  s ; strike-through
  strike ; strike-through
  ;; basefont ; base font size, can be empty [*]
  font ; local change to font
  ;; ========== Object
  object ; embeded objects
  ;; param ; parameters for objects, can also specify as attrs, can be empty [*]
  applet ; Java applet
  ;; ========== Images
  ;; To avoid accessibility problems for people who aren't
  ;; able to see the image, you should provide a text
  ;; description using the alt and longdesc attributes.
  ;; In addition, avoid the use of server-side image maps.
  ;; img ; can be empty [*]
  ;; ========== Client-side image maps
  ;; map ; collides with scheme, but not really useful
  ;; area ; can be empty [*]
  ;; ========== Forms
  form ; forms shouldn't be nested
  label ; text that belongs to a form control
  ;; input ; form control, can be empty [*]
  select ; option selector
  optgroup ; option group
  option ; selectable choice
  textarea ; multi-line text field
  fieldset ; group form fields
  legend ; fieldset label (one per fieldset)
  button ; push button
  ;; isindex ; single-line text input control (DEPRECATED), can be empty [*]
  ;; ========== Tables
  table ; holds caption?, (col*|colgroup*), thead?, tfoot?, (tbody+|tr+)
  caption ; caption text
  thead ; header part, holds tr
  tfoot ; footer part, holds tr
  tbody ; body part, holds tr
  colgroup ; column group, olds col
  ;; col ; column info, has only attributes, can be empty [*]
  tr ; holds th or td
  th ; header cell
  td ; table cell
  )

;; [*] empty elements, these are listed with an `EMPTY' content in
;; http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd
(define/provide-elements/empty
  base meta link hr br basefont param img area input isindex col)

;; [*] elements with a cdata/comment body
(provide script/inline)
(define (script/inline . args)
  (define-values [attrs body] (attributes+body args))
  (make-element
   'script attrs
   `("\n" ,(set-prefix 0 (apply cdata #:line-prefix "//" body)) "\n")))
(provide style/inline)
(define (style/inline . args)
  (define-values [attrs body] (attributes+body args))
  (make-element 'style attrs `("\n" ,body "\n")))

;; ----------------------------------------------------------------------------
;; Entities

;; The three dtds that define the set of entities are at:
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml-lat1.ent
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml-special.ent
;;   http://www.w3.org/TR/xhtml1/DTD/xhtml-symbol.ent

(define/provide-entities
 nbsp ndash mdash bull middot sdot lsquo rsquo sbquo ldquo rdquo bdquo
 lang rang dagger Dagger plusmn deg)

#; ; the complete list
(define/provide-entities
  ;; 24.2 Character entity references for ISO 8859-1 characters
  nbsp     ;00A0 no-break space = non-breaking space
  iexcl    ;00A1 inverted exclamation mark
  cent     ;00A2 cent sign
  pound    ;00A3 pound sign
  curren   ;00A4 currency sign
  yen      ;00A5 yen sign = yuan sign
  brvbar   ;00A6 broken bar = broken vertical bar
  sect     ;00A7 section sign
  uml      ;00A8 diaeresis = spacing diaeresis
  copy     ;00A9 copyright sign
  ordf     ;00AA feminine ordinal indicator
  laquo    ;00AB left-pointing double angle quotation mark = left pointing guillemet
  not      ;00AC not sign
  shy      ;00AD soft hyphen = discretionary hyphen
  reg      ;00AE registered sign = registered trade mark sign
  macr     ;00AF macron = spacing macron = overline = APL overbar
  deg      ;00B0 degree sign
  plusmn   ;00B1 plus-minus sign = plus-or-minus sign
  sup2     ;00B2 superscript two = superscript digit two = squared
  sup3     ;00B3 superscript three = superscript digit three = cubed
  acute    ;00B4 acute accent = spacing acute
  micro    ;00B5 micro sign
  para     ;00B6 pilcrow sign = paragraph sign
  middot   ;00B7 middle dot = Georgian comma = Greek middle dot
  cedil    ;00B8 cedilla = spacing cedilla
  sup1     ;00B9 superscript one = superscript digit one
  ordm     ;00BA masculine ordinal indicator
  raquo    ;00BB right-pointing double angle quotation mark = right pointing guillemet
  frac14   ;00BC vulgar fraction one quarter = fraction one quarter
  frac12   ;00BD vulgar fraction one half = fraction one half
  frac34   ;00BE vulgar fraction three quarters = fraction three quarters
  iquest   ;00BF inverted question mark = turned question mark
  Agrave   ;00C0 latin capital letter A with grave = latin capital letter A grave
  Aacute   ;00C1 latin capital letter A with acute
  Acirc    ;00C2 latin capital letter A with circumflex
  Atilde   ;00C3 latin capital letter A with tilde
  Auml     ;00C4 latin capital letter A with diaeresis
  Aring    ;00C5 latin capital letter A with ring above = latin capital letter A ring
  AElig    ;00C6 latin capital letter AE = latin capital ligature AE
  Ccedil   ;00C7 latin capital letter C with cedilla
  Egrave   ;00C8 latin capital letter E with grave
  Eacute   ;00C9 latin capital letter E with acute
  Ecirc    ;00CA latin capital letter E with circumflex
  Euml     ;00CB latin capital letter E with diaeresis
  Igrave   ;00CC latin capital letter I with grave
  Iacute   ;00CD latin capital letter I with acute
  Icirc    ;00CE latin capital letter I with circumflex
  Iuml     ;00CF latin capital letter I with diaeresis
  ETH      ;00D0 latin capital letter ETH
  Ntilde   ;00D1 latin capital letter N with tilde
  Ograve   ;00D2 latin capital letter O with grave
  Oacute   ;00D3 latin capital letter O with acute
  Ocirc    ;00D4 latin capital letter O with circumflex
  Otilde   ;00D5 latin capital letter O with tilde
  Ouml     ;00D6 latin capital letter O with diaeresis
  times    ;00D7 multiplication sign
  Oslash   ;00D8 latin capital letter O with stroke = latin capital letter O slash
  Ugrave   ;00D9 latin capital letter U with grave
  Uacute   ;00DA latin capital letter U with acute
  Ucirc    ;00DB latin capital letter U with circumflex
  Uuml     ;00DC latin capital letter U with diaeresis
  Yacute   ;00DD latin capital letter Y with acute
  THORN    ;00DE latin capital letter THORN
  szlig    ;00DF latin small letter sharp s = ess-zed
  agrave   ;00E0 latin small letter a with grave = latin small letter a grave
  aacute   ;00E1 latin small letter a with acute
  acirc    ;00E2 latin small letter a with circumflex
  atilde   ;00E3 latin small letter a with tilde
  auml     ;00E4 latin small letter a with diaeresis
  aring    ;00E5 latin small letter a with ring above = latin small letter a ring
  aelig    ;00E6 latin small letter ae = latin small ligature ae
  ccedil   ;00E7 latin small letter c with cedilla
  egrave   ;00E8 latin small letter e with grave
  eacute   ;00E9 latin small letter e with acute
  ecirc    ;00EA latin small letter e with circumflex
  euml     ;00EB latin small letter e with diaeresis
  igrave   ;00EC latin small letter i with grave
  iacute   ;00ED latin small letter i with acute
  icirc    ;00EE latin small letter i with circumflex
  iuml     ;00EF latin small letter i with diaeresis
  eth      ;00F0 latin small letter eth
  ntilde   ;00F1 latin small letter n with tilde
  ograve   ;00F2 latin small letter o with grave
  oacute   ;00F3 latin small letter o with acute
  ocirc    ;00F4 latin small letter o with circumflex
  otilde   ;00F5 latin small letter o with tilde
  ouml     ;00F6 latin small letter o with diaeresis
  divide   ;00F7 division sign
  oslash   ;00F8 latin small letter o with stroke, = latin small letter o slash
  ugrave   ;00F9 latin small letter u with grave
  uacute   ;00FA latin small letter u with acute
  ucirc    ;00FB latin small letter u with circumflex
  uuml     ;00FC latin small letter u with diaeresis
  yacute   ;00FD latin small letter y with acute
  thorn    ;00FE latin small letter thorn
  yuml     ;00FF latin small letter y with diaeresis

  ;; 24.3 Character entity references for symbols, mathematical symbols, and
  ;;      Greek letters
  ;; Latin Extended-B
  fnof     ;0192 latin small f with hook = function = florin
  ;; Greek
  Alpha    ;0391 greek capital letter alpha
  Beta     ;0392 greek capital letter beta
  Gamma    ;0393 greek capital letter gamma
  Delta    ;0394 greek capital letter delta
  Epsilon  ;0395 greek capital letter epsilon
  Zeta     ;0396 greek capital letter zeta
  Eta      ;0397 greek capital letter eta
  Theta    ;0398 greek capital letter theta
  Iota     ;0399 greek capital letter iota
  Kappa    ;039A greek capital letter kappa
  Lambda   ;039B greek capital letter lambda
  Mu       ;039C greek capital letter mu
  Nu       ;039D greek capital letter nu
  Xi       ;039E greek capital letter xi
  Omicron  ;039F greek capital letter omicron
  Pi       ;03A0 greek capital letter pi
  Rho      ;03A1 greek capital letter rho
  Sigma    ;03A3 greek capital letter sigma
  Tau      ;03A4 greek capital letter tau
  Upsilon  ;03A5 greek capital letter upsilon
  Phi      ;03A6 greek capital letter phi
  Chi      ;03A7 greek capital letter chi
  Psi      ;03A8 greek capital letter psi
  Omega    ;03A9 greek capital letter omega
  alpha    ;03B1 greek small letter alpha
  beta     ;03B2 greek small letter beta
  gamma    ;03B3 greek small letter gamma
  delta    ;03B4 greek small letter delta
  epsilon  ;03B5 greek small letter epsilon
  zeta     ;03B6 greek small letter zeta
  eta      ;03B7 greek small letter eta
  theta    ;03B8 greek small letter theta
  iota     ;03B9 greek small letter iota
  kappa    ;03BA greek small letter kappa
  lambda   ;03BB greek small letter lambda
  mu       ;03BC greek small letter mu
  nu       ;03BD greek small letter nu
  xi       ;03BE greek small letter xi
  omicron  ;03BF greek small letter omicron
  pi       ;03C0 greek small letter pi
  rho      ;03C1 greek small letter rho
  sigmaf   ;03C2 greek small letter final sigma
  sigma    ;03C3 greek small letter sigma
  tau      ;03C4 greek small letter tau
  upsilon  ;03C5 greek small letter upsilon
  phi      ;03C6 greek small letter phi
  chi      ;03C7 greek small letter chi
  psi      ;03C8 greek small letter psi
  omega    ;03C9 greek small letter omega
  thetasym ;03D1 greek small letter theta symbol
  upsih    ;03D2 greek upsilon with hook symbol
  piv      ;03D6 greek pi symbol
  ;; *** General Punctuation
  bull     ;2022 bullet = black small circle
  hellip   ;2026 horizontal ellipsis = three dot leader
  prime    ;2032 prime = minutes = feet
  Prime    ;2033 double prime = seconds = inches
  oline    ;203E overline = spacing overscore
  frasl    ;2044 fraction slash
  ;; *** Letterlike Symbols
  weierp   ;2118 script capital P = power set = Weierstrass p
  image    ;2111 blackletter capital I = imaginary part
  real     ;211C blackletter capital R = real part symbol
  trade    ;2122 trade mark sign
  alefsym  ;2135 alef symbol = first transfinite cardinal
  ;; *** Arrows
  larr     ;2190 leftwards arrow
  uarr     ;2191 upwards arrow
  rarr     ;2192 rightwards arrow
  darr     ;2193 downwards arrow
  harr     ;2194 left right arrow
  crarr    ;21B5 downwards arrow with corner leftwards = carriage return
  lArr     ;21D0 leftwards double arrow
  uArr     ;21D1 upwards double arrow
  rArr     ;21D2 rightwards double arrow
  dArr     ;21D3 downwards double arrow
  hArr     ;21D4 left right double arrow
  ;; Mathematical Operators
  forall   ;2200 for all
  part     ;2202 partial differential
  exist    ;2203 there exists
  empty    ;2205 empty set = null set = diameter
  nabla    ;2207 nabla = backward difference
  isin     ;2208 element of
  notin    ;2209 not an element of
  ni       ;220B contains as member
  prod     ;220F n-ary product = product sign
  sum      ;2211 n-ary sumation
  minus    ;2212 minus sign
  lowast   ;2217 asterisk operator
  radic    ;221A square root = radical sign
  prop     ;221D proportional to
  infin    ;221E infinity
  ang      ;2220 angle
  and      ;2227 logical and = wedge
  or       ;2228 logical or = vee
  cap      ;2229 intersection = cap
  cup      ;222A union = cup
  int      ;222B integral
  there4   ;2234 therefore
  sim      ;223C tilde operator = varies with = similar to
  cong     ;2245 approximately equal to
  asymp    ;2248 almost equal to = asymptotic to
  ne       ;2260 not equal to
  equiv    ;2261 identical to
  le       ;2264 less-than or equal to
  ge       ;2265 greater-than or equal to
  sub      ;2282 subset of
  sup      ;2283 superset of
  nsub     ;2284 not a subset of
  sube     ;2286 subset of or equal to
  supe     ;2287 superset of or equal to
  oplus    ;2295 circled plus = direct sum
  otimes   ;2297 circled times = vector product
  perp     ;22A5 up tack = orthogonal to = perpendicular
  sdot     ;22C5 dot operator
  ;; Miscellaneous Technical
  lceil    ;2308 left ceiling = apl upstile
  rceil    ;2309 right ceiling
  lfloor   ;230A left floor = apl downstile
  rfloor   ;230B right floor
  lang     ;2329 left-pointing angle bracket = bra
  rang     ;232A right-pointing angle bracket = ket
  ;; Geometric Shapes
  loz      ;25CA lozenge
  ;; Miscellaneous Symbols
  spades   ;2660 black spade suit
  clubs    ;2663 black club suit = shamrock
  hearts   ;2665 black heart suit = valentine
  diams    ;2666 black diamond suit

  ;; 24.4 Character entity references for markup-significant and
  ;;      internationalization characters
  ;; C0 Controls and Basic Latin
  quot     ;0022 quotation mark = APL quote
  amp      ;0026 ampersand
  lt       ;003C less-than sign
  gt       ;003E greater-than sign
  ;; Latin Extended-A
  OElig    ;0152 latin capital ligature OE
  oelig    ;0153 latin small ligature oe
  Scaron   ;0160 latin capital letter S with caron
  scaron   ;0161 latin small letter s with caron
  Yuml     ;0178 latin capital letter Y with diaeresis
  ;; Spacing Modifier Letters
  circ     ;02C6 modifier letter circumflex accent
  tilde    ;02DC small tilde
  ;; General Punctuation
  ensp     ;2002 en space
  emsp     ;2003 em space
  thinsp   ;2009 thin space
  zwnj     ;200C zero width non-joiner
  zwj      ;200D zero width joiner
  lrm      ;200E left-to-right mark
  rlm      ;200F right-to-left mark
  ndash    ;2013 en dash
  mdash    ;2014 em dash
  lsquo    ;2018 left single quotation mark
  rsquo    ;2019 right single quotation mark
  sbquo    ;201A single low-9 quotation mark
  ldquo    ;201C left double quotation mark
  rdquo    ;201D right double quotation mark
  bdquo    ;201E double low-9 quotation mark
  dagger   ;2020 dagger
  Dagger   ;2021 double dagger
  permil   ;2030 per mille sign
  lsaquo   ;2039 single left-pointing angle quotation mark
  rsaquo   ;203A single right-pointing angle quotation mark
  euro     ;20AC euro sign
  )
