#lang scribble/manual

@(require scribble/eval
          (for-label racket
                     images/flomap
                     racket/draw
                     racket/flonum
                     slideshow)
          images/flomap
          slideshow/pict)

@(require (for-label (only-in typed/racket
                              Integer Float Nonnegative-Fixnum Real Boolean
                              FlVector Vectorof
                              U Any Option)))

@(define flomap-eval (make-base-eval))
@interaction-eval[#:eval flomap-eval (require racket racket/flonum images/flomap)]

@title[#:tag "flomap:title" #:style 'toc]{Floating-Point Bitmaps}
@author{@(author+email "Neil Toronto" "neil.toronto@gmail.com")}

@defmodule[images/flomap]

The @racketmodname[images/flomap] module provides the struct type @racket[flomap], whose instances represent floating-point bitmaps with any number of color components.
It also provides purely functional operations on flomaps for compositing, pointwise floating-point math, blur, gradient calculation, arbitrary spatial transforms (such as rotation), and conversion to and from @racket[bitmap%] instances.

@bold{This is a Typed Racket module.}
Its exports can generally be used from untyped code with negligible performance loss over typed code.
Exceptions are documented @bold{in bold text}.
Most exceptions are macros used to inline floating-point operations.

The following flomap @racket[fm] is used in various examples:
@interaction[#:eval flomap-eval
                    (define fm
                      (draw-flomap
                       (λ (fm-dc)
                         (send fm-dc set-alpha 0)
                         (send fm-dc set-background "black")
                         (send fm-dc clear)
                         (send fm-dc set-alpha 1/3)
                         (send fm-dc translate 2 2)
                         (send fm-dc set-pen "black" 4 'long-dash)
                         (send fm-dc set-brush "red" 'solid)
                         (send fm-dc draw-ellipse 0 0 192 192)
                         (send fm-dc set-brush "green" 'solid)
                         (send fm-dc draw-ellipse 64 0 192 192)
                         (send fm-dc set-brush "blue" 'solid)
                         (send fm-dc draw-ellipse 32 44 192 192))
                       260 240))
                    (flomap->bitmap fm)]
It is typical to use @racket[flomap->bitmap] to visualize a flomap at the REPL.

Contents:
@local-table-of-contents[]


@; ===================================================================================================


@section{Overview}

Contents:
@local-table-of-contents[]

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
         Further, floating-point images are closed under pointwise arithmetic (up to floating-point error).
         }
  @item{@bold{Speed.}
         The @racketmodname[images/flomap] module benefits greatly from Typed Racket's type-directed optimizations.
         Even getting individual color values---interpolated between points, if desired---is fast.
         }
  )
For these reasons, other parts of the @racket[images] library use flomaps internally, to represent and operate on
RGB and ARGB images, light maps, shadow maps, height maps, and normal maps.

@subsection[#:tag "flomap:conceptual"]{Conceptual Model}

A flomap is conceptually infinite in its width and height, but has nonzero values in a finite rectangle starting at coordinate @racket[0] @racket[0]
and extending to its width and height (exclusive).
A flomap is @bold{not} conceptually infinite in its components because there is no natural linear order on component coordinates, as the meaning of components depends on programmer intent.

The following example creates a 10×10 bitmap with RGB components, and indexes its top-left red value and two values outside the finite, nonzero rectangle.
It also attempts to index component @racket[3], which doesn't exist.
Note that @racket[flomap-ref] accepts its coordinate arguments in a standard order: @racket[k] @racket[x] @racket[y] (with @racket[k] for @bold{k}omponent).
@interaction[#:eval flomap-eval
                    (define magenta-fm (make-flomap* 10 10 #(0.5 0.0 1.0)))
                    (flomap->bitmap magenta-fm)
                    (flomap-ref* magenta-fm 0 0)
                    (flomap-ref* magenta-fm -1 0)
                    (flomap-ref* magenta-fm 0 1000)
                    (flomap-ref magenta-fm 3 0 0)]

Many flomap functions, such as @racket[flomap-bilinear-ref] and @racket[flomap-rotate], treat their arguments as if every @italic{real} @racket[x] @racket[y] coordinate has values.
In all such cases, known values are at half-integer coordinates and others are interpolated.

@examples[#:eval flomap-eval
                 (flomap-bilinear-ref* magenta-fm 0.5 0.5)
                 (flomap-bilinear-ref* magenta-fm 0.25 0.25)
                 (flomap-bilinear-ref* magenta-fm 0.0 0.0)
                 (flomap-bilinear-ref* magenta-fm -0.25 -0.25)]

This conceptual model allows us to treat flomaps as if they were multi-valued functions on @racket[Real]×@racket[Real].
For example, we might plot the red component of an ARGB icon:
@interaction[#:eval flomap-eval
                    (require images/icons/misc plot)
                    (define icon-fm (bomb-flomap #:bomb-color "orange" #:height 48))
                    (flomap->bitmap icon-fm)
                    (define-values (icon-width icon-height) (flomap-size icon-fm))
                    (plot3d-bitmap (contour-intervals3d
                                    (λ (x y) (flomap-bilinear-ref icon-fm 1 x y))
                                    -0.5 (+ 0.5 icon-width) -0.5 (+ 0.5 icon-height)))]
Notice that the plot's maximum height is above saturation (@racket[1.0]).
The tallest peak corresponds to the specular highlight (the shiny part) on the bomb.
Specular highlights are one case where it is important to operate on oversaturated values without truncating them---until it is time to display the image.

If we have a @racket[w]×@racket[h] flomap and consider its known values as being at half-integer coordinates, the exact center of the flomap is at @racket[(* 1/2 w)] @racket[(* 1/2 h)].
When unknown values are estimated using bilinear interpolation, the finite rectangle containing all the known @italic{and estimated} nonzero values is from @racket[-1/2] @racket[-1/2] to @racket[(+ w 1/2)] @racket[(+ h 1/2)].

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
                    (define circle-fm (draw-flomap (λ (fm-dc)
                                                     (send fm-dc set-pen "black" 1 'transparent)
                                                     (send fm-dc set-brush "green" 'solid)
                                                     (send fm-dc draw-ellipse 10 10 30 30))
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
The @secref{flomap:conceptual} section explains why @racket[k] is treated differently.
}

@defproc[(flomap-ref* [fm flomap] [x Integer] [y Integer]) FlVector]{
Returns @racket[fm]'s component values at @racket[x] @racket[y] as an flvector.

If @racket[x] or @racket[y] is out of bounds, this function returns an flvector filled with @racket[0.0].
It always returns an flvector of length @racket[(flomap-components fm)].
}

@defproc[(flomap-bilinear-ref [fm flomap] [k Integer] [x Real] [y Real]) Float]{
Returns an estimated value at any given @racket[k] @racket[x] @racket[y] coordinate, calculated from known values in @racket[fm].

Like all other @racket[flomap] functions that operate on real-valued coordinates, @racket[flomap-bilinear-ref] regards known values as being at half-integer coordinates.
Mathematically, if @racket[x] = @racket[(+ i 0.5)] and @racket[y] = @racket[(+ j 0.5)] for any integers @racket[i] and @racket[j],
then @racket[(flomap-bilinear-ref fm k x y)] = @racket[(flomap-ref fm k i j)].

Suppose @racket[fm] is size @racket[w]×@racket[h].
If @racket[x] ≤ @racket[-0.5] or @racket[x] ≥ @racket[(+ w 0.5)], this function returns @racket[0.0]; similarly for @racket[y] and @racket[h].
If @racket[k] is out of bounds, it raises an error.
The @secref{flomap:conceptual} section explains why @racket[k] is treated differently.
}

@defproc[(flomap-bilinear-ref* [fm flomap] [x Real] [y Real]) FlVector]{
Like @racket[flomap-bilinear-ref], but returns an flvector containing estimates of all the components at @racket[x] @racket[y].
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

@defproc[(unsafe-flomap-ref* [vs FlVector]
                             [c Integer] [w Integer] [h Integer]
                             [x Integer] [y Integer]) FlVector]{
Like @racket[unsafe-flomap-ref], but returns an flvector containing all the component values at @racket[x] @racket[y].
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

Values are clamped to between @racket[0.0] and @racket[1.0] before conversion.
}

@defproc[(bitmap->flomap [bm Any]) flomap]{
Given a @racket[bitmap%] instance, returns an ARGB flomap with alpha-multiplied color components.
See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.

The argument type is imprecise because Typed Racket does not support the object system well yet.
}

@defproc[(make-flomap [c Integer] [w Integer] [h Integer] [v Real 0.0]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[c] components, with every value initialized to @racket[v].
Analogous to @racket[make-vector].

To create flomaps filled with a solid color, use @racket[make-flomap*].
}

@defproc[(make-flomap* [w Integer] [h Integer] [vs (U (Vectorof Real) FlVector)]) flomap]{
Returns a @racket[w]×@racket[h] flomap with each point's components initialized using the values in @racket[vs].
Analogous to @racket[make-vector].

The following two examples create an RGB and an ARGB flomap:
@interaction[#:eval flomap-eval
                    (flomap->bitmap (make-flomap* 100 100 #(0.5 0.0 1.0)))
                    (flomap->bitmap (make-flomap* 100 100 #(0.5 0.25 0.0 0.5)))]
See @secref{flomap:opacity} for a discussion of opacity (alpha) representation.
}

@defproc[(build-flomap [c Integer] [w Integer] [h Integer]
                       [f (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum -> Real)]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[c] color components, with values defined by @racket[f].
Analogous to @racket[build-vector].

The function @racket[f] receives three arguments @racket[k] @racket[x] @racket[y]: the color component and two positional coordinates.

@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (build-flomap 1 100 100
                                (λ (k x y) (/ (+ x y) 200))))
                 (define sine-fm
                   (build-flomap
                    1 100 100
                    (λ (k x y)
                      (* 1/2 (+ 1 (sin (sqrt (+ (sqr (- x 50))
                                                (sqr (- y 50))))))))))
                 (flomap->bitmap sine-fm)]

To build a flomap using a function that returns vectors, see @racket[build-flomap*].
}

@defproc[(build-flomap* [c Integer] [w Integer] [h Integer]
                        [f (Nonnegative-Fixnum Nonnegative-Fixnum
                            -> (U (Vectorof Real) FlVector))]) flomap]{
Returns a @racket[w]×@racket[h] flomap with @racket[c] color components.
Its values are defined by @racket[f], which returns vectors of point components.
The vectors returned by @racket[f] must be length @racket[c].

Analogous to @racket[build-vector].

@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (build-flomap* 4 100 100
                                 (λ (x y)
                                   (vector (/ (+ x y) 200)
                                           (/ (+ (- 100 x) y) 200)
                                           (/ (+ (- 100 x) (- 100 y)) 200)
                                           (/ (+ x (- 100 y)) 200)))))
                 
                 (build-flomap* 4 100 100
                                (λ (x y) (vector (/ (+ x y) 200))))]
}

@defproc[(draw-flomap [draw (Any -> Any)] [w Integer] [h Integer]) flomap]{
Returns a @racket[w]×@racket[h] bitmap drawn by @racket[draw].
Analogous to @racketmodname[slideshow]'s @racket[dc].

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

@defform[(inline-build-flomap c w h f)
         #:contracts ([c Integer]
                      [w Integer]
                      [h Integer]
                      [f (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                          Nonnegative-Fixnum -> Float)])]{
A macro version of @racket[build-flomap].

There are three differences between the function @racket[f] passed to @racket[build-flomap] and the @racket[f] passed to @racket[inline-build-flomap].
First, the @racket[f] passed to @racket[inline-build-flomap] can be a macro.
Second, it receives arguments @racket[k] @racket[x] @racket[y] @racket[i], where @racket[i] is a precalculated index into the result's @racketid[values].
Third, it must return a @racket[Float].

Using @racket[inline-build-flomap] instead of @racket[build-flomap] may ensure that @racket[f] is inlined, and therefore floats remain unboxed.
Many library functions use @racket[inline-build-flomap] internally for speed, notably @racket[fm+] and the other pointwise arithmetic operators.

@bold{This is not available in untyped Racket.}
}

@defform[(inline-build-flomap* c w h f)
         #:contracts ([c Integer]
                      [w Integer]
                      [h Integer]
                      [f (Nonnegative-Fixnum Nonnegative-Fixnum
                          Nonnegative-Fixnum -> FlVector)])]{
A macro version of @racket[build-flomap*].

There are three differences between the function @racket[f] passed to @racket[build-flomap*] and the @racket[f] passed to @racket[inline-build-flomap*].
First, the @racket[f] passed to @racket[inline-build-flomap*] can be a macro.
Second, it receives arguments @racket[x] @racket[y] @racket[i], where @racket[i] is a precalculated index into the result's @racketid[values].
Third, it must return a @racket[FlVector].

@bold{This is not available in untyped Racket.}
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

@defproc[(flomap-normalize [fm flomap]) flomap]{
Returns a flomap like @racket[fm], but with values linearly rescaled to be between @racket[0.0] and @racket[1.0] inclusive.
@examples[#:eval flomap-eval
                 (define gray-fm
                   (build-flomap 1 100 100 (λ (k x y) (+ 0.375 (/ (+ x y) 800)))))
                 (flomap->bitmap gray-fm)
                 (flomap->bitmap (flomap-normalize gray-fm))]
Besides increasing contrast, you could use this function to visualize oversaturated flomaps, or visualize flomaps that don't correspond directly to displayed images, such as height maps and normal maps.
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
                 (define fm1 (build-flomap 1 260 240 (λ (k x y) (/ (+ x y) 500))))
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

@defproc[(flomap-lift2 [f (Float Float -> Real)]) ((U Real flomap) (U Real flomap) -> flomap)]{
Lifts a binary floating-point function to operate pointwise on flomaps, allowing the same argument combinations as @racket[fm+] and others.
}

@defform[(inline-flomap-lift f) #:contracts ([f (Float -> Float)])]{
A macro version of @racket[flomap-lift].
The function or macro @racket[f] must return a @racket[Float], not a @racket[Real] as the @racket[f] argument to @racket[flomap-lift] can.

Using @racket[inline-flomap-lift] instead of @racket[flomap-lift] may ensure that @racket[f] is inlined, and therefore floats remain unboxed.

@bold{This is not available in untyped Racket.}
}

@defform[(inline-flomap-lift2 f) #:contracts ([f (Float Float -> Float)])]{
A macro version of @racket[flomap-lift2].
The function or macro @racket[f] must return a @racket[Float], not a @racket[Real] as the @racket[f] argument to @racket[flomap-lift2] can.

Using @racket[inline-flomap-lift2] instead of @racket[flomap-lift2] may ensure that @racket[f] is inlined, and therefore floats remain unboxed.

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
                 (define-values (dx-fm dy-fm)
                   (flomap-gradient (flomap-drop-components fm 1)))
                 (values (flomap->bitmap (fm* 0.5 (fm+ 1.0 dx-fm)))
                         (flomap->bitmap (fm* 0.5 (fm+ 1.0 dy-fm))))]
}

@defproc[(flomap-gradient-normal [fm flomap]) flomap]{
Given a one-component flomap, returns a @racket[3]-component flomap containing estimated normals.
In other words, @racket[flomap-normal] converts height maps to normal maps.
@examples[#:eval flomap-eval
                 (flomap->bitmap sine-fm)
                 (flomap->bitmap (flomap-gradient-normal sine-fm))
                 (flomap-gradient-normal fm)]
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
The radii are of the largest axis-aligned ellipse that would fit in the box.
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
                   (draw-flomap (λ (fm-dc)
                                  (send fm-dc draw-ellipse 20 20 10 10))
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
                   (draw-flomap (λ (fm-dc)
                                  (send fm-dc set-pen "black" 4 'short-dash)
                                  (send fm-dc set-brush "yellow" 'solid)
                                  (send fm-dc set-alpha 1/2)
                                  (send fm-dc draw-ellipse 2 2 124 124))
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
The two-letter abbreviation determines direction (@racketid[v] or @racketid[h]) and alignment (@racketid[l], @racketid[c], @racketid[r], or @racketid[t], @racketid[c], @racketid[b]).
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-ht-append circle-fm fm
                                                   (flomap-scale circle-fm 1/2)))]
See @racket[flomap-pin] and @racket[flomap-pin*] for implementation details.
}


@; ===================================================================================================


@section{Spatial Transformations}

This section gives the API for applying spatial transforms to a flomap, such as rotations, warps, morphs, and lens distortion effects.

To use the provided transforms, apply a function like @racket[flomap-flip-horizontal] directly,
or apply something like a @racket[flomap-rotate-transform] to a flomap using @racket[flomap-transform].

To make your own transforms, compose existing ones with @racket[flomap-transform-compose], or construct a value of type @racket[Flomap-Transform] directly:
@racketblock[(: my-awesome-transform Flomap-Transform)
             (define (my-awesome-transform w h)
               (make-flomap-2d-mapping fun inv))]
Here, @racket[fun] is a mapping from input coordinates to output coordinates and @racket[inv] is its inverse.

Contents:
@local-table-of-contents[]

@subsection{Provided Transformations}

@defproc[(flomap-flip-horizontal [fm flomap]) flomap]
@defproc[(flomap-flip-vertical [fm flomap]) flomap]
@defproc[(flomap-transpose [fm flomap]) flomap]
@defproc[(flomap-cw-rotate [fm flomap]) flomap]
@defproc[(flomap-ccw-rotate [fm flomap]) flomap]{
Some standard image transforms.
These are lossless, in that repeated applications do not degrade (blur or alias) the image.
@examples[#:eval flomap-eval
                 (require slideshow/pict)
                 (define text-fm
                   (flomap-trim
                    (bitmap->flomap
                     (pict->bitmap (vc-append (text "We CLAIM the" '(bold) 25)
                                              (text "PRIVILEGE" '(bold) 25))))))
                 (flomap->bitmap text-fm)
                 (flomap->bitmap (flomap-flip-horizontal text-fm))
                 (flomap->bitmap (flomap-flip-vertical text-fm))
                 (flomap->bitmap (flomap-transpose text-fm))
                 (flomap->bitmap (flomap-cw-rotate text-fm))
                 (flomap->bitmap (flomap-ccw-rotate text-fm))
                 (equal? (flomap-cw-rotate fm)
                         (flomap-flip-vertical (flomap-transpose fm)))
                 (equal? (flomap-ccw-rotate fm)
                         (flomap-flip-horizontal (flomap-transpose fm)))]
}

@defproc[(flomap-rotate [fm flomap] [θ Real]) flomap]{
Returns a flomap rotated by @racket[θ] radians counterclockwise.
Equivalent to @racket[(flomap-transform fm (flomap-rotate-transform θ))].
@examples[#:eval flomap-eval
                 (flomap->bitmap (flomap-rotate text-fm (* 1/4 pi)))]
}

@defproc[(flomap-rotate-transform [θ Real]) Flomap-Transform]{
Returns a flomap transform that rotates a flomap @racket[θ] radians counterclockwise around its (@racket[Real]-valued) center.

Use @racket[flomap-rotate-transform] if you need to know the bounds of the rotated flomap or need to compose a rotation with another transform using @racket[flomap-transform-compose].

@examples[#:eval flomap-eval
                 (flomap-transform-bounds (flomap-rotate-transform (* 1/4 pi))
                                          100 100)
                 (flomap->bitmap
                  (flomap-transform text-fm (flomap-rotate-transform (* 1/4 pi))))]
}

@defproc[(flomap-whirl-transform [θ Real]) Flomap-Transform]{
Returns a flomap transform that ``whirls'' a flomap: rotates it counterclockwise @racket[θ] radians in the center, and rotates less with more distance from the center.

This transform does not alter the size of its input.

@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (flomap-transform text-fm (flomap-whirl-transform pi)))]
}

@defproc[(flomap-fisheye-transform [α Real]) Flomap-Transform]{
Returns a flomap transform that simulates ``fisheye'' lens distortion with an @racket[α] diagonal angle of view.
Equivalent to
@racketblock[(flomap-projection-transform (equal-area-projection α)
                                          (perspective-projection α)
                                          #f)]

@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (flomap-transform text-fm (flomap-fisheye-transform (* 2/3 pi))))]
}

@defproc[(flomap-scale-transform [x-scale Real] [y-scale Real x-scale]) Flomap-Transform]{
Returns a flomap transform that scales flomaps by @racket[x-scale] horizontally and @racket[y-scale] vertically.

You should generally prefer to use @racket[flomap-scale], which is faster and correctly reduces resolution before downsampling to avoid aliasing.
This is provided for composition with other transforms using @racket[flomap-transform-compose].
}

@defthing[flomap-id-transform Flomap-Transform]{
A flomap transform that does nothing.
See @racket[flomap-transform-compose] for an example of using @racket[flomap-id-transform] as the initial value for a fold.
}

@subsection{General Transformations}

@defproc*[([(flomap-transform [fm flomap] [t Flomap-Transform]) flomap]
           [(flomap-transform [fm flomap] [t Flomap-Transform]
                              [x-start Integer] [y-start Integer]
                              [x-end Integer] [y-end Integer])
            flomap])]{
Applies spatial transform @racket[t] to @racket[fm].

The rectangle @racket[x-start] @racket[y-start] @racket[x-end] @racket[y-end] is with respect to the @racket[fm]'s @italic{transformed} coordinates.
If given, points in @racket[fm] are transformed only if their transformed coordinates are within that rectangle.
If not given, @racket[flomap-transform] uses the rectangle returned by @racket[(flomap-transform-bounds t w h)], where @racket[w] and @racket[h] are the size of @racket[fm].

This transform doubles a flomap's size:
@interaction[#:eval flomap-eval
                    (define (double-transform w h)
                      (make-flomap-2d-mapping (λ (x y) (values (* x 2) (* y 2)))
                                              (λ (x y) (values (/ x 2) (/ y 2)))))
                    (flomap->bitmap
                     (flomap-transform text-fm double-transform))]
Transforms can use the width and height arguments @racket[w] @racket[h] however they wish; for example, @racket[double-transform] ignores them, and @racket[flomap-rotate-transform] uses them to calculate the center coordinate.

The @racket[flomap-rotate] function usually increases the size of a flomap to fit its corners in the result.
To rotate in a way that does not change the size---i.e. to do an @italic{in-place} rotation---use @racket[0] @racket[0] @racket[w] @racket[h] as the transformed rectangle:
@interaction[#:eval flomap-eval
                    (define (flomap-in-place-rotate fm θ)
                      (define-values (w h) (flomap-size fm))
                      (flomap-transform fm (flomap-rotate-transform θ) 0 0 w h))]
Using it on @racket[text-fm] with a purple background:
@interaction[#:eval flomap-eval
                    (define-values (text-fm-w text-fm-h) (flomap-size text-fm))
                    (define purple-text-fm
                      (flomap-lt-superimpose (make-flomap* text-fm-w text-fm-h #(1 1/2 0 1))
                                             text-fm))
                    (flomap->bitmap purple-text-fm)
                    (flomap->bitmap (flomap-in-place-rotate purple-text-fm (* 1/8 pi)))]
See @racket[flomap-projection-transform] for another example of using @racket[flomap-transform]'s rectangle arguments, to manually crop a lens projection.

Alternatively, we could define a new transform-producing function @racket[flomap-in-place-rotate-transform]
that never transforms points outside of the orginal flomap:
@interaction[#:eval flomap-eval
                    (define ((flomap-in-place-rotate-transform θ) w h)
                      (match-define (flomap-2d-mapping fun inv _)
                        ((flomap-rotate-transform θ) w h))
                      (make-flomap-2d-mapping (λ (x y)
                                                (let-values ([(x y)  (fun x y)])
                                                  (values (if (<= 0 x w) x +nan.0)
                                                          (if (<= 0 y h) y +nan.0))))
                                              inv))
                    (flomap->bitmap
                     (flomap-transform purple-text-fm
                                       (flomap-in-place-rotate-transform (* 1/8 pi))))]

To transform @racket[fm], @racket[flomap-transform] uses only the @racketid[inv] field of @racket[(t w h)].
Every point @racket[new-x] @racket[new-y] in the transformed bounds is given the components returned by
@racketblock[(let-values ([(old-x old-y)  (inv new-x new-y)])
               (flomap-bilinear-ref* fm old-x old-y))]
The forward mapping @racket[fun] is used by @racket[flomap-transform-bounds].
}

@defidform[Flomap-Transform]{
Defined as @racket[(Integer Integer -> flomap-2d-mapping)].

A value of type @racket[Flomap-Transform] receives the width and height of a flomap to operate on, and returns a @racket[flomap-2d-mapping] on the coordinates of flomaps of that size.
}

@defstruct*[flomap-2d-mapping ([fun (Float Float -> (values Float Float))]
                               [inv (Float Float -> (values Float Float))]
                               [bounded-by (U 'id 'corners 'edges 'all)])]{
Represents an invertible mapping from @racket[Real]×@racket[Real] to @racket[Real]×@racket[Real], or from real-valued flomap coordinates to real-valued flomap coordinates.
See @racket[flomap-transform] for examples.
See @secref{flomap:conceptual} for the meaning of real-valued flomap coordinates.

The forward mapping @racket[fun] is used to determine the bounds of a transformed flomap.
(See @racket[flomap-transform-bounds] for details.)
The inverse mapping @racket[inv] is used to actually transform the flomap.
(See @racket[flomap-transform] for details.)

The symbol @racket[bounded-by] tells @racket[flomap-transform-bounds] how to transform bounds.
In order of efficiency:
@(itemlist
  @item{@racket['id]: Do not transform bounds.
         Use this for in-place transforms such as @racket[flomap-whirl-transform].}
  @item{@racket['corners]: Return the smallest rectangle containing only the transformed corners.
         Use this for linear and affine transforms (such as @racket[flomap-rotate-transform] or a skew transform),
         transforms that do not produce extreme points, and others for which it can be proved (or at least empirically demonstrated)
         that the rectangle containing the transformed corners contains all the transformed points.}
  @item{@racket['edges]: Return the smallest rectangle containing only the transformed left, top, right, and bottom edges.
         Use this for transforms that are almost-everywhere continuous and invertible---which describes most interesting transforms.}
  @item{@racket['all]: Return the smallest rectangle containing all the transformed points.
         Use this for transforms that produce overlaps and other non-invertible results.}
  )

For good performance, define instances of @racket[flomap-2d-mapping] and functions that return them (e.g. instances of @racket[Flomap-Transform]), in Typed Racket.
Defining them in untyped Racket makes every application of @racket[fun] and @racket[inv] contract-checked when used in typed code, such as the implementation of @racket[flomap-transform].
(In the worst case, @racket[flomap-transform] applies @racket[fun] to every pair of coordinates in the input flomap.
It always applies @racket[inv] to every pair of coordinates in the output flomap.)
}

@defproc[(make-flomap-2d-mapping [fun (Float Float -> (values Real Real))]
                                 [inv (Float Float -> (values Real Real))]
                                 [bounded-by (U 'id 'corners 'edges 'all) 'edges]) flomap-2d-mapping]{
A more permissive, more convenient constructor for @racket[flomap-2d-mapping].
}

@defproc[(flomap-transform-compose [t2 Flomap-Transform] [t1 Flomap-Transform]) Flomap-Transform]{
Composes two flomap transforms.
Applying the result of @racket[(flomap-transform-compose t2 t1)] is the same as applying @racket[t1] and then @racket[t2], @bold{except}:
@(itemlist
  @item{The points are transformed only once, meaning their component values are estimated only once, so the result is less degraded (blurry or aliased).}
  @item{The bounds are generally tighter.}
  )

The following example ``whirls'' @racket[text-fm] clockwise 360 degrees and back.
This is first done by applying the two transforms separately, and secondly by applying a composition of them.
@interaction[#:eval flomap-eval
                    (let* ([text-fm  (flomap-transform
                                      text-fm (flomap-whirl-transform (* 2 pi)))]
                           [text-fm  (flomap-transform
                                      text-fm (flomap-whirl-transform (* -2 pi)))])
                      (flomap->bitmap text-fm))
                    (flomap->bitmap
                     (flomap-transform text-fm (flomap-transform-compose
                                                (flomap-whirl-transform (* -2 pi))
                                                (flomap-whirl-transform (* 2 pi)))))]
Notice the heavy aliasing (a ``Moiré pattern'') in the first result is not in the second.

In the next example, notice that rotating multiple times blurs the result and pads it with transparent points, but that applying composed rotation transforms doesn't:
@interaction[#:eval flomap-eval
                    (let* ([text-fm  (flomap-rotate text-fm (* 1/8 pi))]
                           [text-fm  (flomap-rotate text-fm (* 1/8 pi))]
                           [text-fm  (flomap-rotate text-fm (* 1/8 pi))]
                           [text-fm  (flomap-rotate text-fm (* 1/8 pi))])
                      (flomap->bitmap text-fm))
                    (define rotate-pi/2
                      (for/fold ([t flomap-id-transform]) ([_  (in-range 4)])
                        (flomap-transform-compose (flomap-rotate-transform (* 1/8 pi)) t)))
                    (flomap->bitmap (flomap-transform text-fm rotate-pi/2))]

How the bounds for the composed transform are calculated depends on how they would have been calculated for @racket[t1] and @racket[t2].
Suppose @racket[b1] is the bounds rule for @racket[(t1 w h)] and @racket[b2] is the bounds rule for @racket[(t2 w h)].
Then the bounds rule @racket[b] for @racket[(flomap-transform-compose t2 t1)] is determined by the following rules, applied in order:
@(itemlist
  @item{If either @racket[b1] = @racket['all] or @racket[b2] = @racket['all], then @racket[b] = @racket['all].}
  @item{If either @racket[b1] = @racket['edges] or @racket[b2] = @racket['edges], then @racket[b] = @racket['edges].}
  @item{If either @racket[b1] = @racket['corners] or @racket[b2] = @racket['corners], then @racket[b] = @racket['corners].}
  @item{Otherwise, @racket[b1] = @racket[b2] = @racket['id], so @racket[b] = @racket['id].}
  )
See @racket[flomap-2d-mapping] for details on how @racket[b] affects bounds calculation.
}

@defproc[(flomap-transform-bounds [t Flomap-Transform] [w Integer] [h Integer])
                                  (values Integer Integer Integer Integer)]{
Returns the rectangle that would contain a @racket[w]×@racket[h] flomap after transform by @racket[t].
                                                 
How the rectangle is determined depends on the @racket[bounded-by] field of @racket[(t w h)].
See @racket[flomap-2d-mapping] for details.

See @racket[flomap-rotate-transform] and @racket[flomap-projection-transform] for examples.
}

@subsection{Lens Projection and Correction}

The following API demonstrates a parameterized family of spatial transforms.
It also provides a physically grounded generalization of the flomap transforms returned by @racket[flomap-fisheye-transform].

@interaction-eval[#:eval flomap-eval
                         (begin (require racket/draw)
                                (define state-of-the-union-fm
                                  (bitmap->flomap (read-bitmap (collection-file-path "state-of-the-union.jpg"
                                                                                     (build-path "images" "scribblings" "images"))
                                                               'jpeg))))]

@defproc[(flomap-projection-transform [to-proj Projection] [from-proj Projection] [crop? Boolean]) Flomap-Transform]{
Returns a flomap transform that corrects for or simulates lens distortion.

To correct for lens distortion in a flomap:
@(itemlist
  @item{Find a projection @racket[from-proj] that models the actual lens.}
  @item{Find a projection @racket[to-proj] that models the desired (but fictional) lens.}
  @item{Apply @racket[(flomap-projection-transform to-proj from-proj)] to the flomap.}
  )

@margin-note*{This photo is in the public domain.}
In the following example, a photo of the State of the Union address was taken using an ``equal area'' (or ``equisolid angle'') fisheye lens with a 180-degree diagonal angle of view:
@interaction[#:eval flomap-eval
                    (flomap->bitmap state-of-the-union-fm)]
We would like it to have been taken with a perfect ``rectilinear'' (or ``perspective projection'') lens with a 120-degree diagonal angle of view.
Following the steps above, we apply a projection transform using @racket[(equal-area-projection (degrees->radians 180))] for @racket[from-proj] and @racket[(perspective-projection (degrees->radians 120))] for @racket[to-proj]:
@interaction[#:eval flomap-eval
                    (flomap->bitmap
                     (flomap-transform
                      state-of-the-union-fm
                      (flomap-projection-transform
                       (perspective-projection (degrees->radians 120))
                       (equal-area-projection (degrees->radians 180)))))]
Notice that the straight geometry in the House chamber (especially the trim around the ceiling) is represented by straight edges in the corrected photo.

When @racket[crop?] is @racket[#t], the output flomap is no larger than the input flomap.
When @racket[crop?] is @racket[#f], the output flomap is large enough to contain the entire transformed flomap.
An uncropped result can be quite large, especially with angles of view at or near @racket[180] degrees.
@interaction[#:eval flomap-eval
                    (define rectangle-fm
                      (draw-flomap (λ (fm-dc)
                                     (send fm-dc set-pen "black" 4 'dot)
                                     (send fm-dc set-brush "yellow" 'solid)
                                     (send fm-dc set-alpha 1/2)
                                     (send fm-dc draw-rectangle 0 0 32 32))
                                   32 32))
                    (flomap->bitmap rectangle-fm)
                    (flomap-transform-bounds
                     (flomap-projection-transform
                      (perspective-projection (degrees->radians 90))
                      (equal-area-projection (degrees->radians 180))
                      #f)
                     32 32)
                    (flomap->bitmap
                     (flomap-transform
                      rectangle-fm
                      (flomap-projection-transform
                       (perspective-projection (degrees->radians 90))
                       (orthographic-projection (degrees->radians 160))
                       #f)))]
To crop manually, apply @racket[flomap-transform] to explicit rectangle arguments:
@interaction[#:eval flomap-eval
                    (flomap->bitmap
                     (flomap-transform
                      rectangle-fm
                      (flomap-projection-transform
                       (perspective-projection (degrees->radians 90))
                       (orthographic-projection (degrees->radians 160))
                       #f)
                      -10 -10 42 42))]
}

@defproc[(perspective-projection [α Real]) Projection]
@defproc[(linear-projection [α Real]) Projection]
@defproc[(orthographic-projection [α Real]) Projection]
@defproc[(equal-area-projection [α Real]) Projection]
@defproc[(stereographic-projection [α Real]) Projection]{
Given a diagonal angle of view @racket[α], these all return a projection modeling some kind of camera lens.
See @link["http://en.wikipedia.org/wiki/Fisheye_lens"]{Fisheye Lens} for the defining formulas.
}

@defidform[Projection]{
Equivalent to @racket[(Float -> projection-mapping)].

A value of type @racket[Projection] receives the diagonal size of a flomap to operate on, and returns a @racket[projection-mapping] instance.
The provided projections (such as @racket[perspective-projection]) use a closed-over diagonal angle of view @racketid[α] and the diagonal size to calculate the focal length.
}

@defstruct*[projection-mapping ([fun (Float -> Float)]
                                [inv (Float -> Float)])]{
Represents an invertible function from a point's angle @racket[ρ] from the optical axis, to the distance @racket[r] to the center of a photo, in flomap coordinates.

For example, given a diagonal angle of view @racket[α] and the diagonal size @racket[d] of a flomap, the @racket[perspective-projection] function calculates the focal length @racket[f]:
@racketblock[(define f (/ d 2.0 (tan (* 0.5 α))))]
It then constructs the projection mapping as
@racketblock[(projection-mapping (λ (ρ) (* (tan ρ) f))
                                 (λ (r) (atan (/ r f))))]
See @link["http://en.wikipedia.org/wiki/Fisheye_lens"]{Fisheye Lens} for details.
}


@; ===================================================================================================


@section{Effects}

@defproc[(flomap-shadow [fm flomap] [σ Real] [color (Option (U (Vectorof Real) FlVector)) #f]) flomap]{
Returns the alpha (zeroth) component of @racket[fm], blurred with standard deviation @racket[σ] and colorized by @racket[color].
Assumes @racket[fm] and @racket[color] are alpha-multiplied; see @secref{flomap:opacity}.

If @racket[color] = @racket[#f], it is interpreted as @racket[(flvector 1.0 0.0 ...)], or opaque black.
@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (flomap-shadow (flomap-inset text-fm 12) 4 #(1/2 1/8 0 1/4)))
                 (flomap->bitmap
                  (flomap-cc-superimpose
                   (flomap-shadow (flomap-inset text-fm 12) 4 #(1/2 1/8 0 1/4))
                   text-fm))]
}

@defproc[(flomap-outline [fm flomap] [radius Real] [color (Option (U (Vectorof Real) FlVector)) #f]) flomap]{
Returns a flomap that outlines @racket[fm] with a @racket[radius]-thick line when @racket[fm] is superimposed over it.
Assumes @racket[fm] and @racket[color] are alpha-multiplied; see @secref{flomap:opacity}.

If @racket[color] = @racket[#f], it is interpreted as @racket[(flvector 1.0 0.0 ...)], or opaque black.
@examples[#:eval flomap-eval
                 (flomap->bitmap
                  (flomap-outline (flomap-inset text-fm 2) 2 #(1 0 1 1)))
                 (flomap->bitmap
                  (flomap-cc-superimpose
                   (flomap-outline (flomap-inset text-fm 2) 2 #(1 0 1 1))
                   text-fm))]

The greatest alpha value in the returned outline is the greatest alpha value in @racket[fm].
Because of this, @racket[flomap-outline] does fine with flomaps with fully opaque regions that are made semi-transparent:
@interaction[#:eval flomap-eval
                    (define trans-text-fm (fm* 0.5 text-fm))
                    (flomap->bitmap trans-text-fm)
                    (flomap->bitmap
                     (flomap-cc-superimpose
                      (flomap-outline (flomap-inset trans-text-fm 2) 2 #(1 0 1 1))
                      trans-text-fm))]
However, it does not do so well with flomaps that are partly opaque and partly semi-transparent:
@interaction[#:eval flomap-eval
                    (define mixed-text-fm
                      (flomap-vc-append text-fm (make-flomap 4 0 10) trans-text-fm))
                    (flomap->bitmap
                     (flomap-cc-superimpose
                      (flomap-outline (flomap-inset mixed-text-fm 2) 2 #(1 0 1 1))
                      mixed-text-fm))]
}


@close-eval[flomap-eval]
