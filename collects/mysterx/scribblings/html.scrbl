#lang scribble/doc
@(require "common.ss")

@; ----------------------------------------------------------------------

@title[#:tag "html" #:style 'toc]{HTML and Dynamic HTML}

  The @scheme[mx-element%] class encapsulates HTML elements.  By
  calling the methods of the class, you can change the appearance of
  elements, and place new HTML before or after the element.  While the
  methods are described here, a good DHTML reference, such as
  Goodman's @italic{Dynamic HTML} will have more complete information.

  Many of the @scheme[mx-element%] methods have two variants, a
  version that takes or returns Racket data, and another
  @schemeidfont{-native} version that takes or returns a string.  For
  methods that return values of element properties, we assume two
  characteristics, which we do not mention in the methods'
  documentation: 1) Native methods return the empty string for
  properties that have not been set, and 2) non-native methods raise
  an error for properties that have not been set.

@; ----------------------------------------

@include-section["html-element.scrbl"]

@; ----------------------------------------

@section{Generating ActiveX HTML}

@deftogether[(
@defproc[(coclass->html [name string?]
                        [width exact-integer?]
                        [height exact-integer?]
                        [size (one-of/c 'pixels 'percent) 'pixels])
         string?]
@defproc[(progid->html [name string?]
                        [width exact-integer?]
                        [height exact-integer?]
                        [size (one-of/c 'pixels 'percent) 'pixels])
         string?]
)]{

  Returns a string containing HTML which when inserted into a document
  loads the COM object with the COM class or ProgID given by
  @scheme[name].  This procedure is suitable for placing ActiveX
  controls within complex HTML.  The optional @scheme[size] argument
  gives an interpretation for the @scheme[width] and @scheme[height]
  arguments; by default, @scheme[size] is @scheme['pixels], but may
  also be @scheme['percent], indicating that the width and height are
  a fixed percentage of the document window size.}

@; ----------------------------------------

@section{CSS}

  In the @scheme[mx-element%] method descriptions, ``CSS'' refers to
  the Cascading Style Sheets specification.  A CSS length is string
  consisting of a decimal integer number followed by one of the units
  @litchar{px} (pixels), @litchar{em} (font height), @litchar{ex}
  (height of an ``x''), @litchar{in} (inches), @litchar{cm}
  (centimeters), @litchar{mm} (millimeters), @litchar{pc} (picas), or
  @litchar{pt} (points).  A CSS percentage is a string consisting of a
  decimal real number followed by @litchar{%}.  When using
  @schemeidfont{-native} methods, CSS lengths and percentages are
  given as strings.  For use by non-native methods, the
  @scheme[css-percentage] and @scheme[css-length] structures have been
  defined.

@deftogether[(
@defstruct[css-percentage ([num real?])]
@defstruct[css-length ([num real?][units (symbols em ex cm mm in pt pc px)])]
)]

@; ----------------------------------------

@section{Colors}

 Many element properties represent colors.  In HTML, colors may be
 represented by an RGB string, which contains 7 characters.  The first
 character is @litchar{#}, the rest are hexadecimal digits
 (@litchar{0}-@litchar{9} and @litchar{a}-@litchar{f} or
 @litchar{A}-@litchar{F}); the first two digits are for the red
 component of the color, the middle two for the green component, and
 the last two for the blue component.  For example, @scheme["#FFFFFF"]
 is white, @scheme["#000000"] is black, and @scheme["#00FF00"] is
 green.

 There are also predefined color names.  The @schemeidfont{-native}
 methods use these names in strings, while their nonnative counterpart
 methods use the names as symbols.  

 The predefined color names are:

@verbatim[#:indent 2]{
  aliceblue antiquewhite aqua aquamarine azure
  beige bisque black blanchedalmond blue
  blueviolet brown burlywood cadetblue chartreuse
  chocolate coral cornflower cornsilk crimson cyan
  darkblue darkcyan darkgoldenrod darkgray
  darkgreen darkkhaki darkmagenta darkolivegreen
  darkorange darkorchid darkred darksalmon
  darkseagreen darkslateblue darkslategray
  darkturquoise darkviolet deeppink deepskyblue
  dimgray dodgerblue firebrick floralwhite
  forestgreen fuchsia gainsboro ghostwhite gold
  goldenrod gray green greenyellow honeydew
  hotpink indianred indigo ivory khaki lavender
  lavenderblush lawngreen lemonchiffon lightblue
  lightcoral lightcyan lightgoldenrodyellow
  lightgreen lightgray lightpink lightsalmon
  lightseagreen lightskyblue lightslategray
  lightsteelblue lightyellow lime limegreen linen
  magenta maroon mediumaquamarine mediumblue
  mediumorchid mediumpurple mediumseagreen
  mediumslateblue mediumspringgreen
  mediumturquoise mediumvioletred midnightblue
  mintcream mistyrose moccasin navajowhite navy
  oldlace olive olivedrab orange orangered orchid
  palegoldenrod palegreen paleturquoise
  palevioletred papayawhip peachpuff peru pink
  plum powderblue purple red rosybrown royalblue
  saddlebrown salmon sandybrown seagreen seashell
  sienna silver skyblue slateblue slategray snow
  springgreen steelblue tan teal thistle tomato
  turquoise violet wheat white whitesmoke yellow
  yellowgreen
}

