#lang scribble/manual

@(require scribble/eval
          (for-label racket
                     images/flomap
                     racket/draw
                     racket/flonum
                     slideshow)
          images/flomap
          slideshow/pict)

@(define flomap-eval (make-base-eval))
@interaction-eval[#:eval flomap-eval (require racket racket/flonum images/flomap)]

@title[#:tag "flomap:title" #:style 'toc]{Floating-Point Bitmaps}
@author{@(author+email "Neil Toronto" "neil.toronto@gmail.com")}

@defmodule[images/flomap]

The @racketmodname[images/flomap] module provides the struct type @racket[flomap], whose instances represent floating-point bitmaps with any number of color components.
It also provides purely functional operations on flomaps for compositing, pointwise floating-point math, blur, gradient calculation, arbitrary spatial transformations (such as rotation), and conversion to and from @racket[bitmap%] instances.

@bold{This is a Typed Racket module.}
Its exports can generally be used from untyped code with negligible performance loss over typed code.
Exceptions are documented @bold{in bold text}.
Most exceptions are macros used to inline floating-point operations.

The following flomap @racket[fm] is used in various examples:
@interaction[#:eval flomap-eval
                    (define fm
                      (draw-flomap
                       (λ (bm-dc)
                         (send bm-dc set-alpha 0)
                         (send bm-dc set-background "black")
                         (send bm-dc clear)
                         (send bm-dc set-alpha 1/3)
                         (send bm-dc translate 2 2)
                         (send bm-dc set-pen "black" 4 'long-dash)
                         (send bm-dc set-brush "red" 'solid)
                         (send bm-dc draw-ellipse 0 0 192 192)
                         (send bm-dc set-brush "green" 'solid)
                         (send bm-dc draw-ellipse 64 0 192 192)
                         (send bm-dc set-brush "blue" 'solid)
                         (send bm-dc draw-ellipse 32 44 192 192))
                       260 240))
                    (flomap->bitmap fm)]
It is typical to use @racket[flomap->bitmap] to visualize a flomap at the REPL.

Contents:
@local-table-of-contents[]


@; ===================================================================================================


@section{Overview}

@subsection{Motivation}

There are three main reasons to use flomaps:
@(itemlist
  @item{@bold{Precision.}
         A point in a typical bitmap is represented by a few bytes, each of which can have one of 256 distinct values.
         In contrast, a point in a flomap is represented by double-precision floating-point numbers, typically between @racket[0.0] and @racket[1.0] inclusive.
         This range contains about 4.6 @italic{quintillion} (or 4.6×10@superscript{18}) distinct values.
         While bytes are fine for many applications, their low precision becomes a problem when images are repeatedly operated on, or when their values are built by adding many small amounts---which are often rounded to zero.
         }
  @item{@bold{Range.}
         A floating-point value can also represent about 4.6 quintillion distinct intensities above saturation (@racket[1.0]).
         If distinguishing oversaturated values is important, flomaps have the range for it.
         Further, floating-point images are (approximately) closed under pointwise arithmetic.
         }
  @item{@bold{Speed.}
         The @racketmodname[images/flomap] module benefits greatly from Typed Racket's type-directed optimizations.
         Even getting individual color values---interpolated between points, if desired---is fast.
         }
  )
For these reasons, other parts of the @racket[images] library use flomaps internally, to represent and operate on
ARGB and RGB images, light maps, shadow maps, height maps, and normal maps.

@subsection[#:tag "flomap:conceptual"]{Conceptual Model}

A flomap is conceptually infinite in its width and height, but has nonzero values in a finite rectangle starting at coordinate @racket[0] @racket[0]
and extending to its width and height (exclusive).
A flomap is @bold{not} conceptually infinite in its components because there is no natural linear order on component coordinates, as the meaning of components depends on programmer intent.

The following example creates a 10×10 bitmap with RGB components, and indexes its top-left red value and two values outside the finite, nonzero rectangle.
It also attempts to index component @racket[3], which doesn't exist.
Note that @racket[flomap-ref] accepts its coordinate arguments in a standard order: @racket[k] @racket[x] @racket[y] (with @racket[k] for @bold{k}omponent).
@interaction[#:eval flomap-eval
                    (define magenta-fm (make-flomap* 10 10 (flvector 1.0 0.0 1.0)))
                    (flomap->bitmap magenta-fm)
                    (flomap-ref magenta-fm 0 0 0)
                    (flomap-ref magenta-fm 0 -1 0)
                    (flomap-ref magenta-fm 0 0 1000)
                    (flomap-ref magenta-fm 3 0 0)]

Many flomap functions, such as @racket[flomap-bilinear-ref], treat their arguments as if every @italic{real} @racket[x] @racket[y] coordinate has values.
In all such cases, known nonzero values are at half-integer coordinates and others are interpolated.

@examples[#:eval flomap-eval
                 (flomap-bilinear-ref magenta-fm 0 0.5 0.5)
                 (flomap-bilinear-ref magenta-fm 0 0.25 0.25)
                 (flomap-bilinear-ref magenta-fm 0 0.0 0.0)]

This conceptual model allows us to treat flomaps as if they were multi-valued functions on @racket[Real]×@racket[Real].
For example, we might plot the red component of an icon:
@interaction[#:eval flomap-eval
                    (require images/icons/misc plot)
                    (define icon-fm (bomb-flomap "azure" "orange" 48))
                    (flomap->bitmap icon-fm)
                    (define-values (icon-width icon-height) (flomap-size icon-fm))
                    (plot3d-bitmap (contour-intervals3d
                                    (λ (x y) (flomap-bilinear-ref icon-fm 1 x y))
                                    0 icon-width 0 icon-height))]
Notice that the plot's maximum height is above saturation (@racket[1.0]).
The tallest peak corresponds to the specular highlight (the shiny part) on the bomb.
Specular highlights are one case where it is important to operate on oversaturated values without truncating them---until it is time to display the image.

@subsection[#:tag "flomap:opacity"]{Opacity (Alpha Components)}

A partially transparent flomap is simply a flomap in which component @racket[0] is assumed to be an alpha (opacity) component.
The other components should be multiplied by their corresponding alpha value;
i.e. an RGB triple @racket[1.0] @racket[0.5] @racket[0.25] with opacity @racket[0.5] should be represented
by @racket[0.5] @racket[0.5] @racket[0.25] @racket[0.125].

@margin-note*{This representation generally goes by the unfortunate misnomer ``premultiplied alpha,'' which makes it seem as if the @italic{alpha} component is multiplied by something.}
We will refer to this representation as @italic{alpha-multiplied} because the color components are multiplied by the alpha component.
All alpha-aware functions consume alpha-multiplied flomaps and produce alpha-multiplied flomaps.

There are many good reasons to use alpha-multiplied flomaps instead of non-alpha-multiplied flomaps.
Some are:
@(itemlist
  @item{Compositing requires fewer operations per point.}
  @item{Compositing is associative; i.e. @racket[(flomap-lt-superimpose fm1 (flomap-lt-superimpose fm2 fm3))]
        is the same as @racket[(flomap-lt-superimpose (flomap-lt-superimpose fm1 fm2) fm3)], up to floating-point error.}
  @item{There is only one transparent point: all zeros.
        We could not conceptualize partially transparent flomaps as being infinite in size without a unique transparent point.}
  @item{Many functions can operate on flomaps without treating the alpha component specially and still be correct.}
  )

As an example of the last point, consider blur.
The following example creates an alpha-multiplied flomap using @racket[draw-flomap].
It blurs the flomap using a general-purpose (i.e. non-alpha-aware) blur function, then converts the flomap to non-alpha-multiplied and does the same.
@interaction[#:eval flomap-eval
                    (define circle-fm (draw-flomap (λ (dc)
                                                     (send dc set-pen "black" 1 'transparent)
                                                     (send dc set-brush "green" 'solid)
                                                     (send dc draw-ellipse 10 10 30 30))
                                                   50 50))
                    (flomap->bitmap circle-fm)
                    (flomap->bitmap (flomap-blur circle-fm 4 4))
                    (let* ([fm  (flomap-divide-alpha circle-fm)]
                           [fm  (flomap-blur fm 4 4)]
                           [fm  (flomap-multiply-alpha fm)])
                      (flomap->bitmap fm))]
Notice the dark band around the second blurred circle.

Of course, this could be fixed by making @racket[flomap-blur] operate differently on flomaps with an alpha component.
But the implementation would amount to converting them to alpha-multiplied flomaps anyway.

The only valid reason to not multiply color components by alpha is loss of precision, which is not an issue with flomaps.

@subsection[#:tag "flomap:layout"]{Data Layout}

For most applications, there should be enough flomap functions available that you should not need to access their fields directly.
However, there will always be use cases for direct manipulation, so the fields are public.

@(define (color-square n col [size 30])
   (lt-superimpose (colorize (filled-rectangle size size) col)
                   (colorize (rectangle size size) "black")
                   (colorize (inset (text (number->string n) '(bold)) 2 1) "white")))

@(define (rgb-squares n)
   (panorama
    (pin-over (color-square n "red")
              10 10 (pin-over (color-square (+ n 1) "green")
                              10 10 (color-square (+ n 2) "blue")))))

@(define (rgb-square n)
   (cc-superimpose (colorize (filled-rectangle 60 60) "white")
                   (colorize (rectangle 60 60) "black")
                   (rgb-squares n)))

@(define (row n)
   (cc-superimpose (colorize (filled-rectangle 135 70) "lightgray")
                   (colorize (rectangle 135 70) "black")
                   (ht-append (rgb-square n) (blank 5) (rgb-square (+ n 3)))))

@(define (rgb-rect n)
   (ht-append (color-square n "red")
              (color-square (+ n 1) "green")
              (color-square (+ n 2) "blue")))

The color values in a flomap are stored flattened in a single @racket[FlVector], in row-major order with adjacent color components.
For example, a 2×2 RGB flomap can be visualized as

@(vl-append (row 0) (blank 5) (row 6))

In a flomap, it would be stored as

@(ht-append (rgb-rect 0) (rgb-rect 3) (rgb-rect 6) (rgb-rect 9))

Mathematically, for a @racket[c]-component, @racket[w]-width flomap, the @racket[k]th color component at position @racket[x] @racket[y] is at index
@racketblock[(+ k (* c (+ x (* y w))))]
The @racket[coords->index] function carries out this calculation quickly using only fixnum arithmetic.

If @racket[i] is a calculated index for the value at @racket[k] @racket[x] @racket[y],
then the @racket[(+ k 1)]th value is at index @racket[(+ i 1)],
the @racket[(+ x 1)]th value is at index @racket[(+ i c)],
and the @racket[(+ y 1)]th value is at index @racket[(+ i (* c w))].


@; ===================================================================================================


@section{Struct Type and Accessors}

@defstruct*[flomap ([values FlVector] [components Integer] [width Integer] [height Integer])]{
Represents a @racket[width]×@racket[height] floating-point bitmap with @racket[components] color components.
The @racketid[values] vector contains the flattened image data (see @secref{flomap:layout}).

A guard ensures that the @racketid[values] field has length @racket[(* components width height)],
and that each size field is a nonnegative fixnum.

@examples[#:eval flomap-eval
                 (require racket/flonum)
                 (flomap (flvector 0.0 0.0 0.0 0.0) 4 1 1)
                 (flomap (flvector) 0 0 0)
                 (flomap (flvector 0.0) 2 1 1)]

The default @racket[flomap] constructor is perhaps the hardest to use.
Instead, to construct a @racket[flomap] from scratch, you should generally use @racket[make-flomap], @racket[make-flomap*], @racket[build-flomap] or @racket[draw-flomap].
}

@defproc[(flomap-size [fm flomap]) (values Nonnegative-Fixnum Nonnegative-Fixnum)]{
Returns the width and height of @racket[fm] as nonnegative fixnums.
}

@defproc[(flomap-ref [fm flomap] [k Integer] [x Integer] [y Integer]) Float]{
Returns @racket[fm]'s value at @racket[k] @racket[x] @racket[y].
        
If @racket[x] or @racket[y] is out of bounds, this function returns @racket[0.0].
If @racket[k] is out of bounds, it raises an error.
See @secref{flomap:conceptual} to read about why.
}

@defproc[(flomap-bilinear-ref [fm flomap] [k Integer] [x Real] [y Real]) Float]{
Returns an estimated value at any given @racket[k] @racket[x] @racket[y] coordinate, calculated from known values in @racket[fm].

Like all other @racket[flomap] functions that operate on real-valued coordinates, @racket[flomap-bilinear-ref] regards known values as being at half-integer coordinates.
Mathematically, if @racket[x] = @racket[(+ i 0.5)] and @racket[y] = @racket[(+ j 0.5)] for any integers @racket[i] and @racket[j],
then @racket[(flomap-bilinear-ref fm k x y)] = @racket[(flomap-ref fm k i j)].

If @racket[x] or @racket[y] is out of bounds, this function returns @racket[0.0].
If @racket[k] is out of bounds, it raises an error.
See @secref{flomap:conceptual} to read about why.
}

@defproc[(flomap-min-value [fm flomap]) Float]
@defproc[(flomap-max-value [fm flomap]) Float]{
These return the minimum and maximum values in @racket[fm].
}

@defproc[(flomap-extreme-values [fm flomap]) (values Float Float)]{
Equivalent to @racket[(values (flomap-min-value fm) (flomap-max-value fm))], but faster.
}

@defproc[(flomap-nonzero-rect [fm flomap]) (values Nonnegative-Fixnum Nonnegative-Fixnum
                                                   Nonnegative-Fixnum Nonnegative-Fixnum)]{
Returns the smallest rectangle containing every nonzero value (in any component) in @racket[fm].
The values returned are @italic{x} minimum, @italic{y} minimum, @italic{x} maximum + 1, and @italic{y} maximum + 1.

The values returned by @racket[flomap-nonzero-rect] can be sent to @racket[subflomap] to trim away zero values.
But see @racket[flomap-trim], which is faster for alpha-multiplied flomaps.
}

@defproc[(coords->index [c Integer] [w Integer] [k Integer] [x Integer] [y Integer]) Fixnum]{
Returns the index of the value at coordinates @racket[k] @racket[x] @racket[y] of a flomap with @racket[c] color components and width @racket[w].
This function does not check any coordinates against their bounds.
}

@defproc[(unsafe-flomap-ref [vs FlVector]
                            [c Integer] [w Integer] [h Integer]
                            [k Integer] [x Integer] [y Integer]) Float]{
If @racket[fm] = @racket[(flomap vs c w h)], returns @racket[fm]'s value at @racket[k] @racket[x] @racket[y].
If @racket[x] or @racket[y] is out of bounds, this returns @racket[0.0].
It is unsafe because @racket[k] is unchecked, as well as indexing into @racket[vs].

This function is used by some library functions, such as @racket[flomap-bilinear-ref], to index into already-destructured flomaps.
From untyped code, applying this function is likely no faster than applying @racket[flomap-ref], because of extra contract checks.
}


@; ===================================================================================================

                                                                                          
@section{Conversion and Construction}

@defproc[(flomap->bitmap [fm flomap]) Any]{
Converts a flomap to a @racket[bitmap%].

The return type is imprecise because Typed Racket does not support the object system well yet.
As a typed function, this is most useful in DrRacket's REPL to visualize flomaps; any other typed use is difficult.

Flomaps are interpreted differently depending on the number of components:
@(itemlist
  @item{@bold{Zero components.} Raises an error.}
  @item{@bold{One component.} Interpreted as intensity (grayscale).}
  @item{@bold{Two components.} Interpreted as AL, or alpha+intensity, with intensity multiplied by alpha.}
  @item{@bold{Three components.} Interpreted as RGB.}
  @item{@bold{Four components.} Interpreted as ARGB with color components multiplied by alpha.}
  @item{@bold{More components.} Raises an error.}
  )
See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.

A zero-size @racket[fm] is padded by one point in any zero direction before conversion.
For example, if @racket[fm] is size 0×1, the result of @racket[(flomap->bitmap fm)] is size 1×1.
}

@defproc[(bitmap->flomap [bm Any]) flomap]{
Given a @racket[bitmap%] instance, returns an ARGB flomap with alpha-multiplied color components.
See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.

The argument type is imprecise because Typed Racket does not support the object system well yet.
}

@defproc[(make-flomap [c Integer] [w Integer] [h Integer] [v Real 0.0]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[c] components, with every value initialized to @racket[v].

To create flomaps filled with a solid color, use @racket[make-flomap*].
}

@defproc[(make-flomap* [w Integer] [h Integer] [vs FlVector]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[(flvector-length vs)] color components, with each known point initialized using the values in @racket[vs].

The following two examples create magenta bitmaps with an alpha channel:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (make-flomap* 100 100 (flvector 1.0 1.0 0.0 1.0)))
                    (flomap->bitmap (make-flomap* 100 100 (flvector 0.5 0.5 0.0 0.5)))]
See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.
}

@defproc[(build-flomap [c Integer] [w Integer] [h Integer]
                       [f (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                                              Nonnegative-Fixnum -> Real)]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[c] color components, with values defined by @racket[f].

The function @racket[f] receives four arguments @racket[k] @racket[x] @racket[y] @racket[i]: the color component, two positional coordinates, and a precalculated index into the flomap's @racketid[values] vector.
@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (build-flomap 1 100 100
                                (λ (k x y i) (/ (+ x y) 200))))
                 (define sine-fm
                   (build-flomap
                    1 100 100
                    (λ (k x y i)
                      (* 1/2 (+ 1 (sin (sqrt (+ (sqr (- x 50))
                                                (sqr (- y 50))))))))))
                 (flomap->bitmap sine-fm)]
}

@defform[(inline-build-flomap c w h f)]{
A macro version of @racket[build-flomap].
The function or macro @racket[f] must return a @racket[Float], not a @racket[Real] as the @racket[f] argument to @racket[build-flomap] can.

Using @racket[inline-build-flomap] instead of @racket[build-flomap] often ensures that @racket[f] is inlined, and therefore floats remain unboxed.
Many library functions use @racket[inline-build-flomap] internally for speed, notably @racket[fm+] and the other pointwise arithmetic operators.

@bold{This is not available in untyped Racket.}
}

@defproc[(draw-flomap [draw (Any -> Any)] [w Integer] [h Integer]) flomap]{
Returns a @racket[w]×@racket[h] bitmap drawn by @racket[draw].
Think of it as the flomap version of @racketmodname[slideshow]'s @racket[dc].

The @racket[draw] function should accept a @racket[dc<%>] instance and use its drawing methods to draw on an underlying bitmap.
The bitmap is converted to a flomap using @racket[bitmap->flomap] and returned.
See @secref{flomap:title} for an example.

This function is very difficult to use in Typed Racket, requiring occurrence checks for, and use of, experimental types.
However, as Typed Racket grows to handle Racket's object system, the types will be made more precise.
}

@defproc[(flomap-multiply-alpha [fm flomap]) flomap]
@defproc[(flomap-divide-alpha [fm flomap]) flomap]{
Multiplies/divides each nonzero-component value with the corresponding zero-component value.
Dividing by @racket[0.0] produces @racket[0.0].

In other words, @racket[flomap-multiply-alpha] converts non-alpha-multiplied flomaps into alpha-multiplied flomaps,
and @racket[flomap-divide-alpha] converts them back.

You should not generally have to use these functions, because @racket[bitmap->flomap] returns an alpha-multiplied flomap and every alpha-aware flomap function assumes its arguments are alpha-multiplied and produces alpha-multiplied flomaps.

See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.
}


@; ===================================================================================================


@section{Component Operations}

@defproc[(flomap-ref-component [fm flomap] [k Integer]) flomap]{
Extracts one component of a flomap and returns it as a new flomap.
Raises an error if @racket[k] is out of bounds.

Use this, for example, to extract the A and R components from an ARGB flomap:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (flomap-ref-component fm 0))
                    (flomap->bitmap (flomap-ref-component fm 1))]
}

@defproc[(flomap-take-components [fm flomap] [k Integer]) flomap]{
Extracts the first @racket[k] components and returns them as a new flomap.
Raises an error if @racket[k] is out of bounds.
@examples[#:eval flomap-eval (flomap->bitmap (flomap-take-components fm 2))]
}

@defproc[(flomap-drop-components [fm flomap] [k Integer]) flomap]{
Extracts all but the first @racket[k] components and returns them as a new flomap.
Raises an error if @racket[k] is out of bounds.

Use this, for example, to operate on only the RGB channels of an ARGB flomap:
@interaction[#:eval flomap-eval
                    (flomap->bitmap
                     (flomap-append-components (flomap-take-components fm 1)
                                               (fm* 0.25 (flomap-drop-components fm 1))))]
}

@defproc[(flomap-append-components [fm0 flomap] [fm flomap] ...) flomap]{
Appends the components of the given flomaps pointwise.
Raises an error if not all flomaps are the same width and height.

@examples[#:eval flomap-eval
                 (equal? fm (flomap-append-components (flomap-take-components fm 2)
                                                      (flomap-drop-components fm 2)))
                 (flomap-append-components (make-flomap 1 10 10)
                                           (make-flomap 3 20 20))]

This function could behave according to the @secref{flomap:conceptual}---that is, expand the smaller ones to the largest size before appending.
However, appending the components of two different-size flomaps almost always indicates a logic or design error.
If it really is intended, use @racket[flomap-inset] or @racket[subflomap] to expand the smaller flomaps manually, with more control over the expansion.
}


@; ===================================================================================================


@section{Pointwise Operations}

@defproc[(fmsqrt [fm flomap]) flomap]
@defproc[(fmsqr [fm flomap]) flomap]{
Unary functions, lifted pointwise to operate on flomaps.
Defined as @racket[(inline-flomap-lift flsqrt)] and so on.

For example, to estimate the local gradient magnitude at each point in a flomap:
@interaction[#:eval flomap-eval
                    (define-values (dx-fm dy-fm)
                      (flomap-gradient (flomap-drop-components fm 1)))
                    (flomap->bitmap
                     (fmsqrt (fm+ (fmsqr dx-fm) (fmsqr dy-fm))))]
}

@defproc[(flomap-lift [f (Float -> Real)]) (flomap -> flomap)]{
Lifts a unary floating-point function to operate pointwise on flomaps.
}

@defform[(inline-flomap-lift f)]{
A macro version of @racket[flomap-lift].
The function or macro @racket[f] must return a @racket[Float], not a @racket[Real] as the @racket[f] argument to @racket[flomap-lift] can.

Using @racket[inline-flomap-lift] instead of @racket[flomap-lift] often ensures that @racket[f] is inlined, and therefore floats remain unboxed.

@bold{This is not available in untyped Racket.}
}

@defproc[(flomap-normalize [fm flomap]) flomap]{
Returns a flomap like @racket[fm], but with values linearly rescaled to be between @racket[0.0] and @racket[1.0] inclusive.
@examples[#:eval flomap-eval
                 (define gray-fm
                   (build-flomap 1 100 100 (λ (k x y i) (+ 0.375 (/ (+ x y) 800)))))
                 (flomap->bitmap gray-fm)
                 (flomap->bitmap (flomap-normalize gray-fm))]
Besides increasing contrast, you could use this function to debug a rendering pipeline that produces overbright intermediate flomaps.
}

@defproc[(fm+ [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]
@defproc[(fm- [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]
@defproc[(fm* [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]
@defproc[(fm/ [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]
@defproc[(fmmin [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]
@defproc[(fmmax [fm1 (U Real flomap)] [fm2 (U Real flomap)]) flomap]{
Arithmetic, @racket[flmin] and @racket[flmax] lifted to operate pointwise on flomaps.
Defined as @racket[(inline-flomap-lift2 +)] and so on.

Binary operations accept the following argument combinations, in either order:
@(itemlist
  @item{@bold{Two @racket[flomap]s.} Both flomaps must have the same number of components, or one of them must have one component.
         If one flomap has one component, it is (conceptually) self-appended (see @racket[flomap-append-components]) as much as needed before the operation.
         In either case, both flomaps must have the same width and height.}
  @item{@bold{One @racket[flomap], one @racket[Real].} In this case, the real value is (conceptually) made into a uniform flomap (see @racket[make-flomap]) before applying the operation.}
  )
Any other argument combination will raise a type error.

@examples[#:eval flomap-eval
                 (define fm1 (build-flomap 1 260 240 (λ (k x y i) (/ (+ x y) 500))))
                 (define fm2 (fm- 1.0 fm1))
                 (flomap->bitmap fm1)
                 (flomap->bitmap fm2)
                 (flomap->bitmap (fmmax fm1 fm2))
                 (flomap->bitmap (fm* fm1 fm))
                 (fm/ (make-flomap 1 10 10 0.5)
                      (make-flomap 1 30 30 0.25))]
}

Binary pointwise operators could behave according to the @secref{flomap:conceptual}---that is, expand the smaller one to the larger size by filling it with @racket[0.0].
However, operating on the components of two different-size flomaps almost always indicates a logic or design error.
If it really is intended, use @racket[flomap-inset] or @racket[subflomap] to expand the smaller flomap manually, with more control over the expansion.

Because @racket[fm] is an alpha-multiplied flomap (see @secref{flomap:opacity}), multiplying each component by a scalar less than @racket[1.0] results in a more transparent flomap:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (fm* fm 0.2))]

@defproc[(flomap-lift2 [f (Flonum Flonum -> Real)]) ((U Real flomap) (U Real flomap) -> flomap)]{
Lifts a binary floating-point function to operate pointwise on flomaps, allowing the same argument combinations as @racket[fm+] and others.
}

@defform[(inline-flomap-lift2 f)]{
A macro version of @racket[flomap-lift2].
The function or macro @racket[f] must return a @racket[Float], not a @racket[Real] as the @racket[f] argument to @racket[flomap-lift2] can.

Using @racket[inline-flomap-lift2] instead of @racket[flomap-lift2] often ensures that @racket[f] is inlined, and therefore floats remain unboxed.

@bold{This is not available in untyped Racket.}
}


@; ===================================================================================================


@section{Gradients and Normals}

@defproc[(flomap-gradient-x [fm flomap]) flomap]
@defproc[(flomap-gradient-y [fm flomap]) flomap]{
These return, per-component, estimates of the local @italic{x}- and @italic{y}-directional derivatives using a 3×3 @link["http://en.wikipedia.org/wiki/Sobel_operator#Alternative_operators"]{Scharr operator}.
}

@defproc[(flomap-gradient [fm flomap]) (values flomap flomap)]{
Equivalent to @racket[(values (flomap-gradient-x fm) (flomap-gradient-y fm))].

@examples[#:eval flomap-eval
                 (let-values ([(dx-fm dy-fm)  (flomap-gradient
                                               (flomap-drop-components fm 1))])
                   (values (flomap->bitmap (fm* 0.5 (fm+ 1.0 dx-fm)))
                           (flomap->bitmap (fm* 0.5 (fm+ 1.0 dy-fm)))))]
}

@defproc[(flomap-gradient-normal [fm flomap]) flomap]{
Given a one-component flomap, returns a @racket[3]-component flomap containing estimated normals.
In other words, @racket[flomap-normal] converts height maps to normal maps.
@examples[#:eval flomap-eval
                 (flomap->bitmap sine-fm)
                 (flomap->bitmap (flomap-gradient-normal sine-fm))]
}


@; ===================================================================================================


@section{Blur}

@defproc[(flomap-gaussian-blur [fm flomap] [xσ Real] [yσ Real xσ]) flomap]{
Returns @racket[fm] convolved, per-component, with an axis-aligned Gaussian kernel with standard deviations @racket[xσ] and @racket[yσ].

If perfect Gaussian blur is not important, use @racket[flomap-blur] instead, which approximates Gaussian blur closely and is faster.

@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-gaussian-blur (flomap-inset fm 12) 4))
                 (flomap->bitmap (flomap-gaussian-blur (flomap-inset fm 12 3) 4 1))]
}

@defproc[(flomap-gaussian-blur-x [fm flomap] [σ Real]) flomap]{
Returns @racket[fm] convolved, per-component and per-row, with a Gaussian kernel with standard deviation @racket[σ].

If perfect Gaussian blur is not important, use @racket[flomap-blur-x] instead, which approximates Gaussian blur closely and is usually much faster.

@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-gaussian-blur-x (flomap-inset fm 12 0) 4))]
}

@defproc[(flomap-gaussian-blur-y [fm flomap] [σ Real]) flomap]{
Like @racket[flomap-gaussian-blur-x], but per-column instead of per-row.
}

@defproc[(flomap-box-blur [fm flomap] [x-radius Real] [y-radius Real x-radius]) flomap]{
Returns @racket[fm] convolved, per-component, with a box kernel with radii @racket[x-radius] and @racket[y-radius].
The radii are of the largest ellipse that would fit in the box.
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-box-blur (flomap-inset fm 4) 4))
                 (flomap->bitmap (flomap-box-blur (flomap-inset fm 4 1) 4 1))]
}

@defproc[(flomap-box-blur-x [fm flomap] [radius Real]) flomap]{
Returns @racket[fm] convolved, per-component and per-row, with a box kernel with radius @racket[radius].
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-box-blur-x (flomap-inset fm 4 0) 4))]
}

@defproc[(flomap-box-blur-y [fm flomap] [radius Real]) flomap]{
Like @racket[flomap-box-blur-x], but per-column instead of per-row.
}

@defproc[(flomap-blur [fm flomap] [xσ Real] [yσ Real xσ]) flomap]{
Returns approximately the result of @racket[(flomap-gaussian-blur fm xσ yσ)].

Gaussian blur, as it is implemented by @racket[flomap-gaussian-blur], is O(@racket[xσ] + @racket[yσ]) for any fixed flomap size.
On the other hand, @racket[flomap-blur] is O(1) for the same size.
@examples[#:eval flomap-eval
                 (define gauss-blur-fm (time (flomap-gaussian-blur fm 12)))
                 (define blur-fm (time (flomap-blur fm 12)))
                 (flomap-extreme-values
                  (fmsqr (fm- gauss-blur-fm blur-fm)))]
}

@defproc[(flomap-blur-x [fm flomap] [xσ Real]) flomap]{
Like @racket[flomap-blur], but blurs per-row only.
}

@defproc[(flomap-blur-y [fm flomap] [yσ Real]) flomap]{
Like @racket[flomap-blur], but blurs per-column only.
}


@; ===================================================================================================


@section{Resizing}

@defproc[(flomap-copy [fm flomap] [x-start Integer] [y-start Integer] [x-end Integer] [y-end Integer]) flomap]{
Returns the part of @racket[fm] for which the @racket[x] coordinate is @racket[x-start] ≤ @racket[x] < @racket[x-end] and the @racket[y] coordinate is @racket[y-start] ≤ @racket[y] < @racket[y-end].
If @racket[x-start] ≥ @racket[x-end], the result is width @racket[0], and if @racket[y-start] ≥ @racket[y-end], the result is height @racket[0].

The interval arguments may identify a rectangle with points outside the bounds of @racket[fm].
In this case, the points' values in the returned flomap are @racket[0.0], as per the @secref{flomap:conceptual}.

This function is guaranteed to return a copy.
}

@defproc[(subflomap [fm flomap] [x-start Integer] [y-start Integer] [x-end Integer] [y-end Integer]) flomap]{
Like @racket[flomap-copy], but returns @racket[fm] when @racket[x-start] and @racket[y-start] are @racket[0], and @racket[x-end] and @racket[y-end] are respectively the width and height of @racket[fm].

Use @racket[subflomap] instead of @racket[flomap-copy] when programming functionally.
Every library function that returns parts of a flomap (such as @racket[flomap-trim] and @racket[flomap-inset]) is defined using @racket[subflomap].
}

@defproc[(flomap-trim [fm flomap] [alpha? Boolean #t]) flomap]{
Shrinks @racket[fm] to its largest nonzero rectangle.
If @racket[alpha?] is @racket[#t], it uses only component 0 to determine the largest nonzero rectangle; otherwise, it uses every component.

This function cannot return a larger flomap.

@examples[#:eval flomap-eval
                 (define small-circle-fm
                   (draw-flomap (λ (dc)
                                  (send dc draw-ellipse 20 20 10 10))
                                100 100))
                 (flomap->bitmap small-circle-fm)
                 (flomap->bitmap (flomap-trim small-circle-fm))]
See @racket[flomap-nonzero-rect].
}

@defproc*[([(flomap-inset [fm flomap] [amt Integer]) flomap]
           [(flomap-inset [fm flomap] [h-amt Integer] [v-amt Integer]) flomap]
           [(flomap-inset [fm flomap] [l-amt Integer] [t-amt Integer] [r-amt Integer] [b-amt Integer]) flomap])]{
Extends @racket[fm] by some amount on each side, filling any new values with @racket[0.0].
Positive inset amounts grow the flomap; negative insets shrink it.
Large negative insets may shrink it to 0×0, which is a valid flomap size.
@examples[#:eval flomap-eval (flomap->bitmap (flomap-inset fm -10 20 -30 -40))]
}

@defproc[(flomap-crop [fm flomap] [w Integer] [h Integer] [left-frac Real] [top-frac Real]) flomap]{
Shrinks or grows @racket[fm] to be size @racket[w]×@racket[h].
The proportion of points removed/added to the left and top are given by @racket[left-frac] and @racket[top-frac];
e.g. @racket[left-frac] = @racket[1/2] causes the same number to be removed/added to the left and right sides.

You will most likely want to use one of the following cropping functions instead, which are defined using @racket[flomap-crop].
}

@defproc[(flomap-lt-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-lc-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-lb-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-ct-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-cc-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-cb-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-rt-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-rc-crop [fm flomap] [w Integer] [h Integer]) flomap]
@defproc[(flomap-rb-crop [fm flomap] [w Integer] [h Integer]) flomap]{
These shrink or grow @racket[fm] to be size @racket[w]×@racket[h].
The two-letter abbreviation determines which area of the flomap is preserved.
For example, @racket[flomap-lt-crop] (``flomap left-top crop'') preserves the left-top corner:
@interaction[#:eval flomap-eval (flomap->bitmap (flomap-lt-crop fm 150 150))]
}

@defproc[(flomap-scale [fm flomap] [x-scale Real] [y-scale Real x-scale]) flomap]{
Scales @racket[fm] to a proportion of its size.
Uses bilinear interpolation to sample between integer coordinates, and reduces resolution (blurs) correctly before downsampling so that shrunk images are still sharp but not aliased (pixelated-looking).
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-scale fm 1/8))
                 (flomap->bitmap (flomap-scale sine-fm 4))
                 (flomap-scale fm 0)]
}

@defproc[(flomap-resize [fm flomap] [w (Option Integer)] [h (Option Integer)]) flomap]{
Like @racket[flomap-scale], but accepts a width @racket[w] and height @racket[h] instead of scaling proportions.
If either size is @racket[#f], the flomap is scaled in that direction to maintain its aspect ratio.
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-resize fm 50 #f))
                 (flomap->bitmap (flomap-resize fm #f 50))
                 (flomap->bitmap (flomap-resize fm 20 50))
                 (flomap-resize fm 0 0)]
}


@; ===================================================================================================


@section{Compositing}

Unless stated otherwise, compositing functions assume every flomap argument has an alpha component.

@defproc*[([(flomap-pin [fm1 flomap] [x1 Integer] [y1 Integer] [fm2 flomap]) flomap]
           [(flomap-pin [fm1 flomap] [x1 Integer] [y1 Integer]
                        [fm2 flomap] [x2 Integer] [y2 Integer]) flomap])]{
Superimposes @racket[fm2] over @racket[fm1] so that point @racket[x2] @racket[y2] on flomap @racket[f2] is directly over point @racket[x1] @racket[y1] on flomap @racket[f1].
If @racket[x2] and @racket[y2] are not provided, they are assumed to be @racket[0].
The result is expanded as necessary.

@racket[fm1] and @racket[fm2] must have the same number of components.

@examples[#:eval flomap-eval
                 (flomap-pin fm -10 -10 sine-fm)
                 (define circle-fm
                   (draw-flomap (λ (the-dc)
                                  (send the-dc set-pen "black" 4 'short-dash)
                                  (send the-dc set-brush "yellow" 'solid)
                                  (send the-dc set-alpha 1/2)
                                  (send the-dc draw-ellipse 2 2 124 124))
                                128 128))
                 (flomap->bitmap (flomap-pin fm 0 0 circle-fm 64 64))
                 (flomap->bitmap (flomap-pin sine-fm 50 0 sine-fm))]

The other compositing functions are defined in terms of @racket[flomap-pin].
}

@defproc[(flomap-pin* [x1-frac Real] [y1-frac Real]
                      [x2-frac Real] [y2-frac Real]
                      [fm0 flomap] [fm flomap] ...) flomap]{
For each adjacent pair @racket[fm1] @racket[fm2] in the argument list, pins @racket[fm2] over @racket[fm1].

The pin-over points are calculated from the four real arguments as follows.
If @racket[fm1] is size @racket[w1]×@racket[h1], then @racket[x1] = @racket[(* w1 x1-frac)] and @racket[y1] = @racket[(* h1 y1-frac)], and similarly for @racket[x2] and @racket[y2].

The following example pins the upper-left corner of each @racket[fm2] over a point near the upper-left corner of each @racket[fm1]:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (flomap-pin* 1/8 1/8 0 0
                                                 circle-fm circle-fm circle-fm))]

All the flomap superimpose and append functions are defined using @racket[flomap-pin*] with different pin-over point fractions.
For example, @racket[(flomap-lt-superimpose fm0 fm ...)] = @racket[(flomap-pin* 0 0 0 0 fm0 fm ...)],
and @racket[(flomap-vc-append fm0 fm ...)] = @racket[(flomap-pin* 1/2 1 1/2 0 fm0 fm ...)].
}

@defproc[(flomap-lt-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-lc-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-lb-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-ct-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-cc-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-cb-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-rt-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-rc-superimpose [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-rb-superimpose [fm0 flomap] [fm flomap] ...) flomap]{
These create a new flomap by superimposing the flomaps in the argument list.
The two-letter abbreviation determines the pin-over points.
For example, @racket[flomap-lt-superimpose] (``flomap left-top superimpose'') pins points @racket[0] @racket[0] together on each adjacent pair of flomaps:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (flomap-lt-superimpose fm circle-fm))]
See @racket[flomap-pin] and @racket[flomap-pin*] for implementation details.
}

@defproc[(flomap-vl-append [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-vc-append [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-vr-append [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-ht-append [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-hc-append [fm0 flomap] [fm flomap] ...) flomap]
@defproc[(flomap-hb-append [fm0 flomap] [fm flomap] ...) flomap]{
These create a new flomap by spatially appending the flomaps in the argument list.
The two-letter abbreviation determines direction (@racket[v] or @racket[h]) and alignment (@racket[l], @racket[c], @racket[r], or @racket[t], @racket[c], @racket[b]).
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-ht-append circle-fm fm circle-fm))]
See @racket[flomap-pin] and @racket[flomap-pin*] for implementation details.
}


@; ===================================================================================================


@;@section{Transformations}


@; ===================================================================================================


@;@section{Effects}


@close-eval[flomap-eval]
