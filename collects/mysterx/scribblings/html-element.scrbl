#lang scribble/doc
@(require "common.rkt" scribble/bnf scribble/decode scribble/struct
          (for-syntax scheme/base))

@(define-syntax-rule (filter-table (name (option0 contract0) 
                                         (option contract) ...) ...)
   (let ([spacer (hspace 1)]
         [to-flow (lambda (e)
                   (make-flow (list (make-paragraph (list e)))))])
     (make-table
      #f
      (append
       (list (list (to-flow spacer)
                   (to-flow @emph{filter})
                   (to-flow spacer)
                   (to-flow @emph{option})
                   (to-flow spacer)
                   (to-flow @emph{value})))
       (list
        (list (to-flow spacer)
              (to-flow (racket 'name))
              (to-flow spacer)
              (to-flow (racket 'option0))
              (to-flow spacer)
              (make-flow (list (racketblock0 contract0))))
        (list (to-flow spacer)
              (to-flow spacer)
              (to-flow spacer)
              (to-flow (racket 'option))
              (to-flow spacer)
              (make-flow (list (racketblock0 contract))))
        ...)
       ...))))

@(define-syntax-rule (argmatch id)
   @elem{The argument must be a possible result from @method[mx-element% id].})

@(define-syntax-rule (resultmatch id)
   @elem{Possible results are the same as for @method[mx-element% id].})

@(define-syntax-rule (valmatch id)
   @elem{The non-string repersentation is the same as for @method[mx-element% id].})

@(define-syntax (defmethods stx)
  (syntax-case stx ()
   [(_ id contract . more)
    (with-syntax ([id-native
                   (datum->syntax #'id
                                  (string->symbol
                                   (format "~s-native" (syntax-e #'id))))]
                  [set-id!
                   (datum->syntax #'id
                                  (string->symbol
                                   (format "set-~s!" (syntax-e #'id))))]
                  [set-id-native!
                   (datum->syntax #'id
                                  (string->symbol
                                   (format "set-~s-native!" (syntax-e #'id))))]
                  [css-string (format "~a" (syntax-e #'id))])
     #'(make-splice
        (list
         @defmethod*[([(id) contract]
                      [(id-native) string?]
                      [(set-id! [v contract]) void?]      
                      [(set-id-native! [str string?]) void?])]{
          Retrieves or sets a value describing the CSS @tt[css-string] for
          the element.

          @(make-splice (list . more))})))]))

@(define-syntax-rule (defdef defXmethods ret-contract arg-contract)
  (define-syntax (defXmethods stx)
    (syntax-case stx ()
     [(_ id)
      (with-syntax ([set-id!
                     (datum->syntax #'id
                                    (string->symbol
                                     (format "set-~s!" (syntax-e #'id))))]
                    [css-string (format "~a" (syntax-e #'id))])
        #'@defmethod*[([(id) ret-contract]
                       [(set-id! [v arg-contract]) void?])]{
            Retrieves or sets the CSS @tt[css-string] for the element.})])))

@(defdef defboolmethods boolean? any/c)
@(defdef defintmethods exact-integer? exact-integer?)
@(defdef defrealmethods real? real?)

@; ----------------------------------------------------------------------

@title[#:tag "html-element"]{HTML Elements}

@(deprecated)

@defclass[mx-element% object% ()]{

@defmethod[(get-html)
           string?]{

  Returns a string containing all the HTML between the pair of 
  tags represented by the element.}

@defmethod[(get-text)
           string?]{

  Returns a string containing just the text between the pair of 
  tags represented by the element.  Any nested HTML tags 
  are not contained in the returned string.}

@defmethod[(insert-html [html string?])
           void?]{

  Places the HTML given by the string @racket[html] before the element.}

@defmethod[(append-html [html string?])
           void?]{

  Places the HTML given by the string @racket[html] after the element.}

@defmethod[(replace-html [html string?])
           void?]{

  Replaces the HTML in the element with the string @racket[html].  You
  must use the @method[mx-document<%> find-element] or
  @method[mx-document<%> find-element-by-id-or-name] methods of
  @racket[mx-document<%>] to retrieve the updated element.}
  
@defmethod[(insert-text [txt string?])
           void?]{

  Places the text given by the string @racket[txt] before the HTML element.}
  
@defmethod[(append-text [txt string?])
           void?]{

  Places the text given by the string @racket[txt] after the HTML element.}
  
@defmethod[(insert-object-from-coclass [coclass string?]
                          [width exact-integer?]
                          [height exact-integer?]
                          [size (one-of/c 'pixels 'percent) 'pixels])
           void?]{

  Composes @racket[coclass->html] with @method[mx-element% insert-html].}

@defmethod[(insert-object-from-progid [coclass string?]
                          [width exact-integer?]
                          [height exact-integer?]
                          [size (one-of/c 'pixels 'percent) 'pixels])
           void?]{

  Composes @racket[progid->html] with @method[mx-element% insert-html].}

@defmethod[(append-object-from-coclass [coclass string?]
                          [width exact-integer?]
                          [height exact-integer?]
                          [size (one-of/c 'pixels 'percent) 'pixels])
           void?]{

  Composes @racket[coclass->html] with @method[mx-element% append-html].}

@defmethod[(append-object-from-progid [coclass string?]
                          [width exact-integer?]
                          [height exact-integer?]
                          [size (one-of/c 'pixels 'percent) 'pixels])
           void?]{

  Composes @racket[progid->html] with @method[mx-element% append-html].}

@defmethod[(focus)
           void?]{

  Sets the focus to the element.  This method works only with
  Internet Explorer 5 and later.}

@defmethod[(selection)
           string?]{

  If the element has the @racket["select"] tag, returns a string
  indicating the value of the current selection.  Otherwise, an
  exception s raised.  The value of the selection may be different
  from the string visible in the dropdown list.}

@defmethod[(set-selection! [val string?])
           void?]{

  If the element has the @racket["select"] tag, sets the selection to
  the entry with the value @racket[val], a string.  Otherwise, an
  exception is raised.  The value of the selection may be different
  from the string visible in the dropdown list.}

@defmethod[(attribute [attr string?])
           (or/c string? real? boolean?)]{

  Retrieves the attribute named by the string @racket[attr].  The return
  value has a type that depends on the attribute.}

@defmethod[(set-attribute! [attr string?]
                           [val (or/c string? real? boolean?)])
           void?]{

  Sets the attribute named by the string @racket[attr].  The new
  value @racket[val] has a type that depends on the attribute.}
  
@defmethod[(click)
           void?]{

  Simulates a mouse click on the element. }

@defmethod[(tag)
           string?]{

  Retrieves the element's HTML tag.}

@defmethods[font-family
            (listof string?)]

@defmethods[font-style
            (one-of/c 'normal 'italic 'oblique)]

@defmethods[font-variant
            (one-of/c 'normal 'small-caps)]
@defmethods[font-weight
            (one-of/c 'normal 'bold 'bolder 'lighter
                      100 200 300 400 500 600 700 800 900)]

@defmethod*[([(font-native) string?]
             [(set-size-native! [fs string?]) void?])]{

  Retrieves or sets a string that encodes the CSS @tt{font-style},
  @tt{font-variant}, @tt{font-weight}, @tt{font-size},
  @tt{line-height}, and @tt{font-family} using the format

  @BNF-seq[@optional[@BNF-alt/close[@nonterm{font-style} @nonterm{font-variant} @nonterm{font-weight}]]
           @nonterm{font-size}
           @optional[@nonterm{line-height}]
           @nonterm{font-family}]}

@defmethods[font-size
            (or/c
             (one-of/c
              'xx-small 'x-small 'small 'medium 'large 'x-large 'xx-large
              'larger 'smaller)
             css-length?
             css-percentage?)]

@defmethod*[([(background-native) string?]
             [(set-background-native! [b string]) void?])]{

  Gets or sets the element's CSS @tt{background-color}, @tt{background-image},
  @tt{background-repeat}, @tt{background-attachment}, and @tt{background-position}
  using the string @racket[b].}


@defmethods[background-image
            (or/c (one-of/c 'none) string?)]

@defmethods[background-repeat
            (one-of/c 'no-repeat 'repeat 'repeat-x 'repeat-y)]

@defmethods[background-position
            (or/c
             css-length?
             css-percentage?
             (one-of/c 'left 'center 'right)
             (list/c
              (or/c css-length? css-percentage?
                    (one-of/c 'left 'center 'right))
              (or/c css-length? css-percentage?
                    (one-of/c 'left 'center 'right))))]

@defmethods[text-decoration
            (listof (one-of/c 'none 'underline 'overline 'line-through 'blink))]

@defmethods[text-transform
            (one-of/c 'none 'capitalize 'uppercase 'lowercase)]

@defmethods[text-align
            (one-of/c 'left 'right 'center 'justify)]
@defmethods[margin
            (listof (or/c (one-of 'auto)
                          css-length?
                          css-percentage?))]{

  A list representation contains one to four elements. A single
  element applies to all sides; two elements are top--bottom and
  left--right, respectively; four elements are top, left, bottom, and
  right, respectively.}


@defmethods[padding
            (listof (or/c css-length? css-percentage?))]{

  The list contains one to four elements, which apply to sides as for
  @method[mx-element% margin].}

@defmethods[border
            (listof (or/c (or/c (one-of/c 'medium 'thin 'thick) css-length?)
                          (one-of/c 'none 'dotted 'dashed 'solid 'double
                                    'groove 'ridge 'inset 'outset)
                          (or/c symbol? string?)))]{

  Each element of the list describes a width, style, or color. A color
  is a symbol indicating a color or an RGB string.}

@defmethods[border-top ....]{@valmatch[border]}
@defmethods[border-bottom ....]{@valmatch[border]}
@defmethods[border-left ....]{@valmatch[border]}
@defmethods[border-right ....]{@valmatch[border]}

@defmethods[border-color (listof (or/c symbol? string?))]{

  The list contains one to four elements, with side assignments
  as for @method[mx-element% margin].}

@defmethods[border-width
            (listof (or/c css-length?
                          (one-of/c 'medium 'thin 'thick)))]{

  The list contains one to four elements, with side assignments
  as for @method[mx-element% margin].}

@defmethods[border-style
            (one-of/c 'none 'dotted 'dashed 'solid 'double
                      'groove 'ridge 'inset 'outset)]

@defmethods[border-top-style ....]{@valmatch[border-style]}
@defmethods[border-bottom-style ....]{@valmatch[border-style]}
@defmethods[border-left-style ....]{@valmatch[border-style]}
@defmethods[border-right-style ....]{@valmatch[border-style]}

@defmethods[style-float
            (one-of/c 'none 'left 'right)]

@defmethods[clear
            (one-of/c 'none 'left 'right 'both)]

@defmethods[display
            (one-of/c 'block 'none 'inline 'list-item
                      'table-header-group 'table-footer-group)]

@defmethods[visibility
            (one-of/c 'inherit 'visible 'hidden)]

@defmethods[list-style-type
            (one-of/c 'disc 'circle 'square 'decimal 
                      'lower-roman 'upper-roman
                       'lower-alpha 'upper-alpha 'none)]

@defmethods[list-style-position
            (one-of/c 'outside 'inside)]

@defmethods[list-style-image (lambda (s) 
                               (and string?
                                    (regexp-match? #rx"^(none|url[(].*[)])$" s)))]

@defmethods[list-style list?]{

  A list representation contains one to three elements,
  which have the same representations as for
  @method[mx-element% list-style-type],
  @method[mx-element% list-style-position], 
  and @method[mx-element% list-style-image]. The
  values may appear in any order.}

@defmethods[position
            (one-of/c 'absolute 'relative 'static)]

@defmethods[overflow
            (one-of/c 'visible 'scroll 'hidden 'auto)]

@defmethods[pagebreak-before
            (one-of/c 'always 'auto 'none)]
@defmethods[pagebreak-after
            (one-of/c 'always 'auto 'none)]

@defmethod*[([(css-text-native) string?]
             [(set-css-text-native! [txt string?]) void?])]{

  Retrieves or sets a string describing the CSS @tt{text} for
  the element.}

@defmethods[cursor
            (one-of/c 'auto 'crosshair 'default
                      'hand 'move 'n-resize 'ne-resize 'nw-resize 's-resize
                      'se-resize 'sw-resize 'e-resize 'w-resize 'text 'wait
                      'help)]

@defmethods[clip
            (or/c (one-of/c 'auto)
                  (list/c (or/c (one-of/c 'auto)
                                css-length?)
                          (or/c (one-of/c 'auto)
                                css-length?)
                          (or/c (one-of/c 'auto)
                                css-length?)
                          (or/c (one-of/c 'auto)
                                css-length?)))]

@defmethods[filter
            (cons/c symbol? (listof (list/c symbol? any/c)))]{

  For a filter value that combines a symbol with a list, the symbol is
  a filter name, and the list maps symbol option names to values. The
  table below shows the possible options and value types for each
  possible filter name.

@filter-table[
( alpha           (enabled             boolean?)
                  (finish-opacity      (integer-in 0 100))
                  (opacity             (integer-in 0 100))
                  (start-x             exact-integer?)
                  (start-y             exact-integer?)
                  (finish-x            exact-integer?)
                  (finish-y            exact-integer?)
                  (style               (one-of/c 'uniform 
                                                 'linear
                                                 'radial 
                                                 'rectangular)))
( blend-trans     (enable              boolean?)
                  (duration            real?)
                  (status              (one-of/c 'stopped 
                                                 'applied 
                                                 'playing)))
( blur            (add                 boolean?)
                  (enabled             boolean?)
                  (direction           (one-of/c 0 45 90 
                                                 135 180 
                                                 225 270 
                                                 315))
                  (strength            (integer-in 1 100)))
( chroma          (enabled             boolean?)
                  (color               string?))
( drop-shadow     (enabled             boolean?)
                  (off-x               exact-integer?)
                  (off-y               exact-integer?))
( flip-horizontal (enabled             boolean?))
( flip-vertical   (enabled             boolean?))
( glow            (enabled             boolean?)
                  (color               string?)
                  (strength            (integer-in 1 100)))
( gray            (enabled             boolean?))
( invert          (enabled             boolean?))
( light           (enabled             boolean?))
( mask            (enabled             boolean?)
                  (color               string?))
( redirect        (enabled             boolean?))
( reveal-trans    (enabled             boolean?)
                  (duration            real?)
                  (status              (one-of/c 'stopped 
                                                 'applied 
                                                 'playing)))
( shadow          (enabled             boolean?)
                  (color               string?)
                  (direction           (one-of/c 0 45 90 
                                                 135 180 
                                                 225 270 
                                                 315)))
( wave            (enabled             boolean?)
                  (freq                (and/c real? 
                                              (not/c negative?)))
                  (light-strength      (integer-in 1 100)))
( x-ray           (enabled             boolean?))
]}

@defmethod[(style-string)
           string?]{

  Retrieves a string describing the complete CSS
  description for the element.}

@defboolmethods[text-decoration-none]
@defboolmethods[text-decoration-underline]
@defboolmethods[text-decoration-overline]
@defboolmethods[text-decoration-linethrough]
@defboolmethods[text-decoration-blink]

@defintmethods[pixel-top]
@defintmethods[pixel-left]
@defintmethods[pixel-width]
@defintmethods[pixel-height]

@defrealmethods[pos-top]
@defrealmethods[pos-left]
@defrealmethods[pos-width]
@defrealmethods[pos-height]

@defmethods[color
            (or/c symbol? string?)]
@defmethods[background-color
            (or/c symbol? string?)]

@defmethods[background-position-x
            (or/c css-length? css-percentage? 
                  (one-of/c 'left 'center 'right))]
@defmethods[background-position-y
            (or/c css-length? css-percentage? 
                  (one-of/c 'left 'center 'right))]

@defmethods[letter-spacing
            (or/c css-length? (one-of/c 'normal))]

@defmethods[vertical-align
            (one-of/c 'baseline 'sub 'super 'top 'middle 
                      'bottom 'text-top 'text-bottom)]
@defmethods[text-indent
            (or/c css-length? css-percentage?)]

@defmethods[line-height
            (or/c css-length? css-percentage?
                  (one-of/c 'normal))]

@defmethods[margin-top
            (or/c css-length? css-percentage?
                 (one-of/c 'auto))]
@defmethods[margin-bottom
            (or/c css-length? css-percentage?
                 (one-of/c 'auto))]
@defmethods[margin-left
            (or/c css-length? css-percentage?
                 (one-of/c 'auto))]
@defmethods[margin-right
            (or/c css-length? css-percentage?
                 (one-of/c 'auto))]

@defmethods[padding-top
            (or/c css-length? css-percentage?)]
@defmethods[padding-bottom
            (or/c css-length? css-percentage?)]
@defmethods[padding-left
            (or/c css-length? css-percentage?)]
@defmethods[padding-right
            (or/c css-length? css-percentage?)]

@defmethods[border-top-color
            (or/c symbol? string?)]
@defmethods[border-bottom-color
            (or/c symbol? string?)]
@defmethods[border-left-color
            (or/c symbol? string?)]
@defmethods[border-right-color
            (or/c symbol? string?)]

@defmethods[border-top-width
            (or/c css-length? 
                  (one-of/c 'medium 'thin 'thick))]
@defmethods[border-bottom-width
            (or/c css-length? 
                  (one-of/c 'medium 'thin 'thick))]
@defmethods[border-left-width
            (or/c css-length? 
                  (one-of/c 'medium 'thin 'thick))]
@defmethods[border-right-width
            (or/c css-length? 
                  (one-of/c 'medium 'thin 'thick))]

@defmethods[width
            (or/c css-length? css-percentage?
                  (one-of/c 'auto))]
@defmethods[height
            (or/c css-length? css-percentage?
                  (one-of/c 'auto))]

@defmethods[top
            (or/c css-length? css-percentage?
                  (one-of/c 'auto))]
@defmethods[left
            (or/c css-length? css-percentage?
                  (one-of/c 'auto))]

@defmethods[z-index
            (or/c exact-integer? (one-of/c 'auto))]

}
