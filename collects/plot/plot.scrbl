#lang scribble/doc
@(require scribble/manual
          (for-label scheme
                     scheme/gui/base
                     plot
                     plot/extend))

@title{@bold{PLoT}: Graph Plotting}

@author["Alexander Friedman" "Jamie Raymond"]

PLoT (a.k.a. PLTplot) provides a basic interface for producing common
types of plots such as line and vector field plots as well as an
advanced interface for producing customized plot types.  Additionally,
plots and plot-items are first-class values and can be generated in
and passed to other programs.

@table-of-contents[]

@section{Quick Start}

@subsection{Overview}

PLoT (aka PLTplot) provides a basic interface for producing common
types of plots such as line and vector field plots as well as an
advanced interface for producing customized plot types.  Additionally,
plots and plot-items are first-class values and can be generated in
and passed to other programs.

@subsection{Basic Plotting}

After loading the correct module using @scheme[(require plot)] try

@schemeblock[(plot (line (lambda (x) x)))]

Any other function using the contract @scheme[(real? . -> . real?)]
can be plotted using the same form.  To plot multiple items, use the
functions @scheme[mix] and @scheme[mix*] to combine the items to be
plotted.

@schemeblock[
  (plot (mix (line (lambda (x) (sin x)))
             (line (lambda (x) (cos x)))))
  ]

The display area and appearance of the plot can be changed by adding
brackets argument/value pairs after the first argument.

@schemeblock[
  (plot (line (lambda (x) (sin x)))
        #:x-min -1 #:x-max 1 #:title "Sin(x)")
  ]

The appearance of each individual plot item can be altered by adding
argument/value pairs after the data.

@schemeblock[
  (plot (line (lambda (x) x)
              #:color 'green #:width 3))
  ]

Besides plotting lines from functions in 2-D, the plotter can also
render a variety of other data in several ways:

@itemize[
         
 @item{Discrete data, such as
      
   @schemeblock[
     (define data (list (vector 1 1 2)
                        (vector 2 2 2)))
     ]
   
   can be interpreted in several ways:
   
   @itemize[
            
     @item{As points: @scheme[(plot (points _data))]}
          
     @item{As error data: @scheme[(plot (error-bars _data))]}
     
     ]
   }
   
  @item{A function of two variables, such as
           
    @schemeblock[
      (define 3dfun (lambda (x y) (* (sin x) (sin y))))
      ]
    
    can be plotted on a 2d graph
    
    @itemize[
             
      @item{Using contours to represent height (z)
                  
        @schemeblock[
          (plot (contour 3dfun))
          ]
        }
           
      @item{Using color shading
                  
        @schemeblock[
          (plot (shade 3dfun))
          ]
        }
      
      @item{Using a gradient field
                  
        @schemeblock[
          (plot (vector-field (gradient 3dfun)))
          ]
        }
      ]
    
    or in a 3d box
    
    @itemize[
             
      @item{Displaying only the top of the surface
                       
        @schemeblock[
          (plot3d (surface 3dfun))
          ]
        }
      ]
    }
   
   ]

@subsection[#:tag "ex-curve-fit"]{Curve Fitting}

The @schememodname[plot] library uses a non-linear, least-squares fit
algorithm to fit parameterized functions to given data.

To fit a particular function to a curve:

@itemize[
         
  @item{Set up the independent and dependent variable data.  The first
  item in each vector is the independent variable, the second is the
  result.  The last item is the weight of the error; we can leave it
  as @scheme[1] since all the items weigh the same.
            
    @schemeblock[
      (define data '(#(0 3 1)
                     #(1 5 1)
                     #(2 7 1)
                     #(3 9 1)
                     #(4 11 1)))
      ]
    }
       
  @item{Set up the function to be fitted using fit.  This particular
  function looks like a line.  The independent variables must come
  before the parameters.
            
    @schemeblock[
      (define fit-fun
        (lambda (x m b) (+ b (* m x))))
      ]
    }
  
  @item{If possible, come up with some guesses for the values of the
  parameters.  The guesses can be left as one, but each parameter must
  be named.}
  
  @item{Do the fit; the details of the function are described in
  @secref["curve-fit"].
           
    @schemeblock[
      (define fitted
        (fit fit-fun
             '((m 1) (b 1))
             data))
      ]
    }
  
  @item{View the resulting parameters; for example,
             
    @schemeblock[
      (fit-result-final-params fitted)
      ]
    
    will produce @schemeresultfont{(2.0 3.0)}.
    }
  
  @item{For some visual feedback of the fit result, plot the function
  with the new parameters. For convenience, the structure that is
  returned by the fit command has already the function.
            
    @schemeblock[
      (plot (mix (points data)
                 (line (fit-result-function fitted)))
            #:y-max 15)
      ]
    }
  ]

A more realistic example can be found in
@filepath{demos/fit-demo-2.ss} in the @filepath{plot} collection.

@subsection{Creating Custom Plots}

Defining custom plots is simple: a plot-item (that is passed to plot
or mix) is just a function that acts on a view.  Both the 2-D and 3-D
view snip have several drawing functions defined that the plot-item
can call in any order.  The full details of the view interface can be
found in @secref["extend"].

For example, if we wanted to create a constructor that creates
plot-items that draw dashed lines given a @scheme[(real? . -> . real?)]
function, we could do the following:

@schemeblock[
  (require plot/extend)
  
  (define (dashed-line fun
                       #:x-min [x-min -5]
                       #:x-max [x-max 5]
                       #:samples [samples 100]
                       #:segments [segments 20]
                       #:color [color 'red]
                       #:width [width 1])
    (let* ((dash-size (/ (- x-max x-min) segments))
           (x-lists (build-list
                     (/ segments 2)
                     (lambda (index)
                       (x-values
                        (/ samples segments)
                        (+ x-min (* 2 index dash-size))
                        (+ x-min (* (add1 (* 2 index)) 
                           dash-size)))))))
      (lambda (2dview)
        (send 2dview set-line-color color)
        (send 2dview set-line-width width)
        (for-each
         (lambda (dash)
           (send 2dview plot-line
                 (map (lambda (x) (vector x (fun x))) dash)))
         x-lists))))
  ]

Plot a test case using @scheme[dashed-line]:

@schemeblock[
  (plot (dashed-line (lambda (x) x) #:color 'blue))
]

@; ----------------------------------------

@section[#:tag "plot"]{Plotting}

@defmodule[plot]

The @schememodname[plot] library provides the ability to make basic
plots, fit curves to data, and some useful miscellaneous functions.

@subsection{Plotting}

The @scheme[plot] and @scheme[plot3d] functions generate plots that can be
viewed in the DrRacket interactions window.

@defproc[(plot [data ((is-a?/c 2d-view%) . -> . void?)]
               [#:width width real? 400]
               [#:height height real? 400]
               [#:x-min x-min real? -5]
               [#:x-max x-max real? 5]
               [#:y-min y-min real? -5]
               [#:y-max y-max real? 5]
               [#:x-label x-label string? "X axis"]
               [#:y-label y-label string? "Y axis"]
               [#:title title string? ""]
               [#:fgcolor fgcolor (list/c byte? byte? byte) '(0 0 0)]
               [#:bgcolor bgcolor (list/c byte? byte? byte) '(255 255 255)]
               [#:lncolor lncolor (list/c byte? byte? byte) '(255 0 0)])
          (is-a?/c snip%)]{

Plots @scheme[data] in 2-D, where @scheme[data] is generated by
functions like @scheme[points] or @scheme[lines].

A @scheme[data] value is represented as a procedure that takes a
@scheme[2d-view%] instance and adds plot information to it.}

@defproc[(plot3d [data ((is-a?/c 3d-view%) . -> . void?)]
                 [#:width width real? 400]
               [#:height height real? 400]
               [#:x-min x-min real? -5]
               [#:x-max x-max real? 5]
               [#:y-min y-min real? -5]
               [#:y-max y-max real? 5]
               [#:z-min z-min real? -5]
               [#:z-max z-max real? 5]
               [#:alt alt real? 30]
               [#:az az real? 45]
               [#:x-label x-label string? "X axis"]
               [#:y-label y-label string? "Y axis"]
               [#:z-label z-label string? "Z axis"]
               [#:title title string? ""]
               [#:fgcolor fgcolor (list/c byte? byte? byte) '(0 0 0)]
               [#:bgcolor bgcolor (list/c byte? byte? byte) '(255 255 255)]
               [#:lncolor lncolor (list/c byte? byte? byte) '(255 0 0)])
          (is-a?/c snip%)]{

Plots @scheme[data] in 3-D, where @scheme[data] is generated by a
function like @scheme[surface]. The arguments @scheme[alt] and
@scheme[az] set the viewing altitude (in degrees) and the azimuth
(also in degrees), respectively.

A 3-D @scheme[data] value is represented as a procedure that takes a
@scheme[3d-view%] instance and adds plot information to it.}


@defproc[(points [vecs (listof (vector/c real? real?))]
                 [#:sym sym (or/c character? integer? symbol?) 'fullsquare]
                 [#:color color plot-color? 'black])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data (to be provided to @scheme[plot]) given a list
of points specifying locations. The @scheme[sym] argument determines
the appearance of the points.  It can be a symbol, an ASCII character,
or a small integer (between -1 and 127).  The following symbols are
known: @scheme['pixel], @scheme['dot], @scheme['plus],
@scheme['asterisk], @scheme['circle], @scheme['times],
@scheme['square], @scheme['triangle], @scheme['oplus], @scheme['odot],
@scheme['diamond], @scheme['5star], @scheme['6star],
@scheme['fullsquare], @scheme['bullet], @scheme['full5star],
@scheme['circle1], @scheme['circle2], @scheme['circle3],
@scheme['circle4], @scheme['circle5], @scheme['circle6],
@scheme['circle7], @scheme['circle8], @scheme['leftarrow],
@scheme['rightarrow], @scheme['uparrow], @scheme['downarrow].  }



@defproc[(line [f (real? . -> . (or/c real? (vector real? real?)))]
               [#:samples samples exact-nonnegative-integer? 150]
               [#:width width exact-positive-integer? 1]
               [#:color color plot-color? 'red]
               [#:mode mode (one-of/c 'standard 'parametric) 'standard]
               [#:mapping mapping (or-of/c 'cartesian 'polar) 'cartesian]
               [#:t-min t-min real? -5]
               [#:t-max t-min real? 5])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data to draw a line.

The line is specified in either functional, i.e. @math{y = f(x)}, or
parametric, i.e. @math{x,y = f(t)}, mode.  If the function is
parametric, the @scheme[mode] argument must be set to
@scheme['parametric].  The @scheme[t-min] and @scheme[t-max] arguments
set the parameter when in parametric mode.}


@defproc[(error-bars [vecs (listof (vector/c real? real? real?))]
                     [#:color color plot-color? 'black])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data for error bars given a list of vectors.  Each
vector specifies the center of the error bar @math{(x,y)} as the first
two elements and its magnitude as the third.}


@defproc[(vector-field [f ((vector real? real?) . -> . (vector real? real?))]
                       [#:width width exact-positive-integer? 1]
                       [#:color color plot-color? 'red]
                       [#:style style (one-of/c 'scaled 'normalized 'read) 'scaled])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data to draw a vector-field from a vector-valued
function.}


@defproc[(contour [f (real? real? . -> . real?)]
                  [#:samples samples exact-nonnegative-integer? 50]
                  [#:width width exact-positive-integer? 1]
                  [#:color color plot-color? 'black]
                  [#:levels levels (or/c exact-nonnegative-integer?
                                         (listof real?))
                            10])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data to draw contour lines, rendering a 3-D function
a 2-D graph cotours (respectively) to represent the value of the
function at that position.}

@defproc[(shade [f (real? real? . -> . real?)]
                [#:samples samples exact-nonnegative-integer? 50]
                [#:levels levels (or/c exact-nonnegative-integer?
                                       (listof real?))
                          10])
         ((is-a?/c 2d-view%) . -> . void?)]{

Creates 2-D plot data to draw like @scheme[contour], except using
shading instead of contour lines.}


@defproc[(surface [f (real? real? . -> . real?)]
                  [#:samples samples exact-nonnegative-integer? 50]
                  [#:width width exact-positive-integer? 1]
                  [#:color color plot-color? 'black])
         ((is-a?/c 3d-view%) . -> . void?)]{

Creates 3-D plot data to draw a 3-D surface in a 2-D box, showing only
the @italic{top} of the surface.}


@defproc[(mix [data (any/c . -> . void?)] ...+)
         (any/c . -> . void?)]{

Creates a procedure that calls each @scheme[data] on its argument in
order. Thus, this function can composes multiple plot @scheme[data]s
into a single data.}


@defproc[(plot-color? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is one of the following symbols,
@scheme[#f] otherwise:

@schemeblock[
'white 'black 'yellow 'green 'aqua 'pink
'wheat 'grey 'blown 'blue 'violet 'cyan
'turquoise 'magenta 'salmon 'red
]}

@; ----------------------------------------

@subsection[#:tag "curve-fit"]{Curve Fitting}

PLoT uses the standard Non-Linear Least Squares fit algorithm for
curve fitting.  The code that implements the algorithm is public
domain, and is used by the @tt{gnuplot} package.

@defproc[(fit [f (real? ... . -> . real?)]
              [guess-list (list/c (list symbol? real?))]
              [data (or/c (list-of (vector/c real? real? real?))
                          (list-of (vector/c real? real? real? real?)))])
         fit-result?]{

Attempts to fit a @defterm{fittable function} to the data that is
given. The @scheme[guess-list] should be a set of arguments and
values. The more accurate your initial guesses are, the more likely
the fit is to succeed; if there are no good values for the guesses,
leave them as @scheme[1].}

@defstruct[fit-result ([rms real?]
                       [variance real?]
                       [names (listof symbol?)]
                       [final-params (listof real?)]
                       [std-error (listof real?)]
                       [std-error-percent (listof real?)]
                       [function (real? ... . -> . real?)])]{

The @scheme[params] field contains an associative list of the
parameters specified in @scheme[fit] and their values.  Note that the
values may not be correct if the fit failed to converge.  For a visual
test, use the @scheme[function] field to get the function with the
parameters in place and plot it along with the original data.}

@; ----------------------------------------

@subsection{Miscellaneous Functions}

@defproc[(derivative [f (real? . -> . real?)] [h real? .000001])
         (real? . -> . real?)]{

Creates a function that evaluates the numeric derivative of
@scheme[f].  The given @scheme[h] is the divisor used in the
calculation.}

@defproc[(gradient [f (real? real? . -> . real?)] [h real? .000001])
         ((vector/c real? real?) . -> . (vector/c real? real?))]{

Creates a vector-valued function that the numeric gradient of
@scheme[f].}

@defproc[(make-vec [fx (real? real? . -> . real?)] [fy (real? real? . -> . real?)])
         ((vector/c real? real?) . -> . (vector/c real? real?))]{

Creates a vector-values function from two parts.}

@; ----------------------------------------

@section[#:tag "extend"]{Customizing Plots}

@defmodule[plot/extend]

The @schememodname[plot/extend] module allows you to create your own
constructors, further customize the appearance of the plot windows,
and in general extend the package.

@defproc[(sample-size [sample-count exact-positive-integer?]
                      [x-min number]
                      [x-max number])
         real?]{
               
Given @scheme[sample-count], @scheme[x-min], and @scheme[x-max], returns the size of each sample.}


@defproc[(scale-vectors [vecs (listof vector?)] [x-sample-size real?] [y-sample-size real?])
         (listof vector?)]{
               
Scales vectors, causing them to fit in their boxes.}


@defproc[(x-values [sample-count exact-positive-integer?]
                      [x-min number]
                      [x-max number])
         (listof real?)]{
                 
Given @scheme[samples], @scheme[x-min], and @scheme[x-max], returns a
list of @scheme[x]s spread across the range.}

  
@defproc[(normalize-vector [vec vector?] [x-sample-size real?] [y-sample-size real?]) vector?]{
               
Normalizes @scheme[vec] based on @scheme[x-sample-size] and @scheme[y-sample-size].}


@defproc[(normalize-vectors [vecs (listof vector?)] [x-sample-size real?] [y-sample-size real?])
         (listof vector?)]{

Normalizes @scheme[vecs] based on @scheme[x-sample-size] and @scheme[y-sample-size].}


@defproc[(make-column [x real?] [ys (listof real?)]) (listof (vector/c real? real?))]{
               
Given an @scheme[x] and a list of @scheme[_y]s, produces a list of
points pairing the @scheme[x] with each of the @scheme[_y]s.}
  

@defproc[(xy-list [sample-count exact-positive-integer?]
                  [x-min real?]
                  [x-max real?]
                  [y-min real?]
                  [y-max real?])
          (listof (listof (vector/c real? real?)))]{

Makes a list of all the positions on the graph.}
  

@defproc[(zgrid [f (real? real? . -> . real?)]
                [xs (listof real?)]
                [ys (listof real?)])
         (listof (listof real?))]{

Given a function that consumes @scheme[_x] and @scheme[_y] to produce
@scheme[_z], a list of @scheme[_x]s, and a list of @scheme[_y]s,
produces a list of @scheme[_z] column values.}

@; ----------------------------------------

@defclass[plot-view% object% ()]{

@defmethod[(get-x-min) real?]{
               
Returns the minimum plottable @scheme[_x] coordinate.}
  
@defmethod[(get-y-min) real?]{
               
Returns the minimum plottable @scheme[_y] coordinate.}
  
@defmethod[(get-x-max) real?]{
               
Returns the maximum plottable @scheme[_x] coordinate.}

@defmethod[(get-y-max) real?]{
               
Returns the maximum plottable @scheme[_y] coordinate.}

@defmethod[(set-line-color [color plot-color?]) void?]{

Sets the drawing color.}

@defmethod[(set-line-width [width real?]) void?]{

Sets the drawing line width.}

}

@; ----------------------------------------

@defclass[2d-view% plot-view% ()]{

Provides an interface to drawing 2-D plots. An instance of
@scheme[2d-view%] is created by @scheme[plot], and the following
methods can be used to adjust it.

@defmethod[(set-labels [x-label string?]
                     [y-label string?]
                     [title string?])
         void?]{

Sets the axis labels and title.}


@defmethod[(plot-vector [head (vector/c real? real?)]
                      [tail (vector/c real? real?)])
         void?]{

Plots a single vector.}


@defmethod[(plot-vectors [vecs (listof (list/c (vector/c real? real?)
                                             (vector/c real? real?)))])
         void?]{

Plots a set of vectors.}


@defmethod[(plot-points [points (listof (vector/c real? real?))]
                        [sym (or/c character? integer? symbol?)])
         void?]{

Plots points using a specified symbol.  See @scheme[points] for
possible values for @scheme[sym]}


@defmethod[(plot-line [points (listof (vector/c real? real?))]) void?]{

Plots a line given a set of points.}


@defmethod[(plot-contours [grid (listof (listof real?))] 
                        [xs (listof real?)]
                        [ys (listof real?)]
                        [levels (listof real?)]) void?]{

Plots a grid representing a 3-D function using contours to distinguish levels.}

@defmethod[(plot-shades [grid (listof (listof real?))] 
                      [xs (listof real?)]
                      [ys (listof real?)]
                      [levels (listof real?)]) void?]{
               
Plots a grid representing a 3-D function using shades to show levels.}

}

@; ----------------------------------------

@defclass[3d-view% plot-view% ()]{

Provides an interface to drawing 3-D plots. An instance of
@scheme[3d-view%] is created by @scheme[plot3d], and the following
methods can be used to adjust it.


@defmethod[(plot-surface [xs (listof real?)]
                       [ys (listof real?)]
                       [zs (listof real?)]) void?]{

Plots a grid representing a 3d function in a 3d box, showing only the
top of the surface.}


@defmethod[(plot-line [xs (listof real?)]
                       [ys (listof real?)]
                       [zs (listof real?)]) void?]{

Plots a line in 3-D space.}


@defmethod[(get-z-min) real?]{
               
Returns the minimum plottable @scheme[_z] coordinate.}
  
@defmethod[(get-z-max) real?]{
               
Returns the maximum plottable @scheme[_z] coordinate.}

  
@defmethod[(get-alt) real?]{

Returns the altitude (in degrees) from which the 3-D box is viewed.}
  
@defmethod[(get-az) real?]{
               
Returns the azimuthal angle.}

}
