#lang scribble/doc

@title{Quick: An Introduction to Racket with Pictures}

@author["Matthew Flatt"]

@; ----------------------------------------------------------------------

@(require scribble/manual
          "mreval.rkt"
          "keep.rkt"
          scribble/urls
          scribble/struct
          racket/class

          (for-label racket/base
                     racket/gui/base
                     racket/class
                     slideshow
                     slideshow/code
                     slideshow/flash)

          (for-syntax racket/base))

@; ----------------------------------------------------------------------

This tutorial provides a brief introduction to the Racket
programming language by using one of its picture-drawing
libraries. Even if you don't intend to use Racket for your artistic
endeavours, the picture library supports interesting and enlightening
examples. After all, a picture is worth five hundred ``hello world''s.

Along the same lines, we assume that you will run the examples using
@link[url:download-drracket]{DrRacket}. Using DrRacket is the fastest
way to get a sense of what the language and system feels like, even if
you eventually use Racket with Emacs, vi, or some other editor.

@; ----------------------------------------------------------------------
@section{Ready...}

@link[url:download-drracket]{Download Racket}, install, and then
start DrRacket.

@; ----------------------------------------------------------------------
@section{Set...}

To draw pictures, we must first load some picture functions, which are
part of a library for creating slide presentations.  Copy the
following into the @defterm{definitions area}, which is the top text
area that you see in DrRacket:

@racketmod[slideshow]

Then click the @onscreen{Run} button. You'll see the text caret move
to the bottom text area, which is the @defterm{interactions area}.

If you've used DrRacket before, you might need to reset DrRacket to
use the language declared in the source via the @menuitem["Language"
"Choose Language..."] menu item before clicking @onscreen{Run}.

@; ----------------------------------------------------------------------
@section{Go!}

When you type an expression after the @onscreen{>} in the interactions
window and hit Enter, DrRacket evaluates the expression and prints its
result. An expression can be just a value, such as the number
@racket[5] or the string @racket["art gallery"]:

@ss-interaction[5 "art gallery"]

An expression can also be a function call. To call a function, put
an open parenthesis before the function name, then expressions for the
function arguments, and then a close parenthesis, like this:

@ss-interaction[(circle 10)]

A result from the @racket[circle] function is a picture value, which
prints as an expression result in much the same way that numbers or
strings print.  The argument to @racket[circle] determines the
circle's size in pixels.  As you might guess, there's a
@racket[rectangle] function that takes two arguments instead of one:

@ss-interaction[(rectangle 10 20)]

Try giving @racket[circle] the wrong number of arguments, just to see
what happens:

@ss-interaction[(circle 10 20)]

Note that DrRacket highlights in pink the expression that triggered
the error (but pink highlighting is not shown in this documentation).

In addition to basic picture constructors like @racket[circle] and
@racket[rectangle], there's a @racket[hc-append] function that
combines pictures. When you start composing function calls in Racket,
it looks like this:

@ss-interaction[(hc-append (circle 10) (rectangle 10 20))]

The hyphen in the name @racket[hc-append] is just a part of the
identifier; it's not @racketidfont{hc} minus
@racketidfont{append}. The function name starts with @racket[h]
because it combines pictures horizontally, and the next letter is
@racket[c] because the pictures are centered vertically.

If you wonder what other functions exist---perhaps a way to stack
pictures vertically and left-aligned?---move the text caret to the
name @racket[hc-append] and press the F1 key in DrRacket. A browser
window will open, and it will give you a link to the documentation for
@racket[hc-append]. Click the link, and you'll see lots of other
functions.

If you're reading this in HTML form, you can also just click on
@racket[hc-append] or any other imported identifier that is used in
this tutorial.

@; ----------------------------------------------------------------------
@section{Definitions}

To use a particular circle and rectangle picture many times, it's
simpler to give them names. Move back to the definitions area (the top
area) and add two definitions, so that the complete content of the
definitions area looks like this:

@ss-racketmod+eval[
slideshow
(define c (circle 10))
(define r (rectangle 10 20))
]

Then click @onscreen{Run} again. Now, you can just type @racket[c] or
@racket[r]:

@ss-interaction[r (hc-append c r) (hc-append 20 c r c)]

As you can see, the @racket[hc-append] function accepts an optional
number argument before the picture arguments, and it accepts any
number of picture arguments. When a number is provided, it specifies
the amount of space to add between pictures.

We could have evaluated the @racket[define] forms for @racket[c] and
@racket[r] in the interactions area instead of the definitions
area. In practice, though, the definitions area is where your program
lives---it's the file that you save---while the interaction area is
for transient explorations and debugging tasks.

Let's add a function definition to the program. A function definition
uses @racket[define], just like our shape definitions, but with an
open parenthesis before the function name, and names for the function
arguments before the matching close parenthesis:

@ss-racketblock+eval[
(define (square n)
  (code:comment @#,t{A semi-colon starts a line comment.})
  (code:comment @#,t{The expression below is the function body.})
  (filled-rectangle n n))
]

The syntax of the definition mirrors the syntax of a function
call:

@ss-interaction[(square 10)]

In the same way that definitions can be evaluated in the interactions
area, expressions can be included in the definitions area. When a
program is run, expression results from the definition area are shown
in the interaction area. From now on, we'll write our example
definitions and expressions together, and you can put them in
whichever area you prefer. The examples will build on each other,
however, so it's best to put at least the definitions in the
definition area.

@; ----------------------------------------------------------------------
@section{Local Binding}

The @racket[define] form can be used in some places to create local
bindings. For example, it can be used inside a function body:

@ss-def+int[
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))
]

More typically, Racketeers use the @racket[let] or @racket[let*] form
for local binding. An advantage of @racket[let] is that it can be used
in any expression position. Also, it binds many identifiers at once,
instead of requiring a separate @racket[define] for each identifier:

@ss-def+int[
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))
]

A @racket[let] form binds many identifiers at the same time, so the
bindings cannot refer to each other. The @racket[let*] form, in
contrast, allows later bindings to use earlier bindings:

@ss-def+int[
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))
]

@; ----------------------------------------------------------------------
@section{Functions are Values}

Instead of calling @racket[circle] as a function, try evaluating just
@racket[circle] as an expression:

@ss-interaction[circle]

That is, the identifier @racket[circle] is bound to a function
(a.k.a. ``procedure''), just like @racket[c] is bound to a
circle. Unlike a circle picture, there's not a simple way of
completely printing the function, so DrRacket just prints
@procedure{circle}.

This example shows that functions are values, just like numbers and
pictures (even if they don't print as nicely). Since functions are
values, you can define functions that expect other functions as
arguments:

@ss-def+int[
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)
(series square)
]

When calling a function that accepts a function argument, the
argument function often isn't needed anywhere else. Having to write
down the function via @racket[define] would be a hassle, because you
have to make up a name and find a place to put the function
definition. The alternative is to use @racket[lambda], which creates an
anonymous function:

@ss-interaction[(series (lambda (size) (checkerboard (square size))))]

The parenthesized names after a @racket[lambda] are the arguments to
the function, and the expression after the argument names is the
function body. Using the word ``lambda'' instead of ``function'' or
``procedure'' is part of Racket's history and culture.

A @racket[define] form for a function is really a shorthand for a
simple @racket[define] using @racket[lambda] as the value. For
example, the @racket[series] definition could be written as

@racketblock[
(define series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))
]

Most Racketeers prefer to use the shorthand function form with
@racket[define] instead of expanding to @racket[lambda].

@; ----------------------------------------------------------------------
@section{Lexical Scope}

Racket is a lexically scoped language, which means that whenever an
identifier is used as an expression, something in the textual
environment of the expression determines the identifier's
binding. This rule applies to identifiers in a @racket[lambda] body as
well as anywhere else.

In the following @racket[rgb-series] function, the uses
of @racket[mk] in each @racket[lambda] form refer to the argument of
@racket[rgb-series], since that's the binding that is textually in
scope:

@ss-def+int[
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
(rgb-series circle)
(rgb-series square)
]

Here's another example, where @racket[rgb-maker] takes a function and
returns a new one that remembers and uses the original function.

@ss-def+int[
(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
(series (rgb-maker circle))
(series (rgb-maker square))
]

Note how composing functions via @racket[rgb-maker] creates a
different alignment of objects within the picture compared to using
@racket[rgb-series].

@; ----------------------------------------------------------------------
@section{Lists}

Racket inherits much of its style from the language Lisp, whose name
originally stood for ``LISt Processor,'' and lists remain an important
part of Racket.

The @racket[list] function takes any number of arguments and returns
a list containing the given values:

@ss-interaction[(list "red" "green" "blue")
                (list (circle 10) (square 10))]

As you can see, a list prints as a single quote and then pair of parentheses wrapped around
the printed form of the list elements. There's room for confusion
here, because parentheses are used for both expressions, such as
@racket[(circle 10)], and printed results, such as
@racketresult['("red" "green" "blue")]. The quote is the key difference,
as @seclink[#:doc '(lib "scribblings/guide/guide.scrbl") "quoting-lists"]{discussed
elsewhere}. To help emphasize the difference, in the documentation and in DrRacket, 
result parentheses are printed in blue, unlike expression parentheses.

If you have a list, then you'll eventually want to do something with
each of the elements. The @racket[map] function takes a list and a
function to apply to each element of the list; it returns a new list
to combine the function's results:

@ss-def+int[
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
(rainbow (square 5))
]

Another function that works with lists is @racket[apply]. Like
@racket[map], it takes a function and a list, but a function given
to @racket[apply] should take all of the arguments at once, instead of
each one individually. The @racket[apply] function is especially
useful with functions that take any number of arguments, such as
@racket[vc-append]:

@ss-interaction[
(apply vc-append (rainbow (square 5)))
]

Note that @racket[(vc-append (rainbow (square 5)))] would not work,
because @racket[vc-append] does not want a list as an argument; it
wants a picture as an argument, and it is willing to accept any number
of them. The @racket[apply] function bridges the gap between a
function that wants many arguments and a list of those arguments as a
single value.

@; ----------------------------------------------------------------------
@section{Modules}

Since your program in the definitions window starts with

@racketmod[slideshow]

all of the code that you put in the definitions window is inside a
module. Furthermore, the module initially imports everything from the
module designated by @racketmodname[slideshow], which exports
picture-making functions as well as more commonly used functions
such as @racket[list] and @racket[map].

To import additional libraries, use the @racket[require] form. For
example, the library @racketmodname[slideshow/flash] provides a
@racket[filled-flash] function:

@ss-def+int[
(require slideshow/flash)
(filled-flash 40 30)
]

Modules are named and distributed in various ways:

@itemize[

 @item{Some modules are packaged in the Racket distribution or
       otherwise installed into a hierarchy of
       @defterm{collections}. For example, the module name
       @racketmodname[slideshow/flash] means ``the module implemented
       in the file @filepath{flash.rkt} that is located in the
       @filepath{slideshow} collection.'' When a module name includes
       no slash, then it refers to a @filepath{main.rkt} file.}

 @item{Some modules are distributed through the
       @link[url:planet]{@PLaneT} server, and they can be
       downloaded automatically on demand. For example, the first time
       that you evaluate the following fragment:

       @mr-def+int[
        (require (planet schematics/random:1:0/random))
        (random-gaussian)
       ]

       DrRacket automatically downloads version 1.0 of the
       @filepath{random.plt} library by @filepath{schematics} and then
       imports the @filepath{random.rkt} module.}

 @item{Some modules live relative to other modules, without
       necessarily belonging to any particular collection or package.
       For example, in DrRacket, if you save your definitions so far in a
       file @filepath{quick.rkt} and add the line

        @racketblock[(provide rainbow square)]

       then you can open a new tab or window in DrRacket, type the new
       program @filepath{use.rkt} in the same directory as
       @filepath{quick.rkt}:

        @racketmod[
         racket
         (require "quick.rkt")
         (rainbow (square 5))
        ]

        and when you run @filepath{use.rkt}, a rainbow list of squares
        is the output. Note that @filepath{use.rkt} is written using
        the initial import @racketmodname[racket], which does not
        supply any picture-making functions itself---but does provide
        @racket[require] and the function-calling syntax.}

]

Racketeers typically write new programs and libraries as modules that
import each other through relative paths, and that use existing
libraries from collections and @racket[planet]. When a program or
library developed this way seems useful to others, it can be uploaded
as a @PLaneT package or distributed in the more old-fashioned way as
an installable collection archive (in either case without modifying
the internal relative references among modules).

@; ----------------------------------------------------------------------
@section{Macros}

Here's another library to try:

@mr-def+int[
(require slideshow/code)
(code (circle 10))
]

Instead of a circle, the result is a picture of the code that, if it
were used as an expression, would produce a circle. In other words,
@racket[code] is not a function, but instead a new syntactic form for
creating pictures; the bit between the opening parenthesis with
@racket[code] is not an expression, but instead manipulated by the
@racket[code] syntactic form.

This helps explain what we meant in the previous section when we said
that @racketmodname[racket] provides @racket[require] and the
function-calling syntax. Libraries are not restricted to exporting
values, such as functions; they can also define new syntactic
forms. In this sense, Racket isn't exactly a language at all; it's
more of an idea for how to structure a language so that you can extend
it or create entirely new languages.

One way to introduce a new syntactic form is through
@racket[define-syntax] with @racket[syntax-rules]:

@mr-def+int[
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
(pict+code (circle 10))
]

This kind of definition is a macro. The @racket[(pict+code expr)] part
is a pattern for uses of the macro; instances of the pattern in a
program are replaced by instances of the corresponding template, which
is @racket[(hc-append 10 expr (code expr))].  In particular,
@racket[(pict+code (circle 10))] matches the pattern with
@racket[(circle 10)] as @racket[expr], so it is replaced with
@racket[(hc-append 10 (circle 10) (code (circle 10)))].

Of course, this sort of syntactic extension cuts both ways: inventing
a new language can make it easier to say what you want, but harder for
others to understand. As it happens, the developers of Racket are
constantly giving talks and writing papers that involve Racket code,
and it's worthwhile for everyone who works on those products to know
about @racket[code].

In fact, you might want to take a look at the @keep-file["quick.scrbl"]
@link["quick.scrbl"]{source of this document}. You'll see that it
starts with @racketfont{#lang}, but otherwise doesn't look a lot
like Racket; nevertheless, we build this document by running its
source as a Racket program. We have to use a lot more than
@racket[syntax-rules] to extend Racket's syntax enough for writing
documents, but Racket's syntactic extension can take you a long way.

@; ----------------------------------------------------------------------
@section{Objects}

An object system is another example of a sophisticated language
extension that is worth learning and using for Racket users. Objects
are sometimes better than functions, even when you have
@racket[lambda], and objects work especially well for graphical user
interfaces. The API for Racket's GUI and graphics system is expressed
in terms of objects and classes.

The class system itself is implemented by the
@racketmodname[racket/class] library, and the
@racketmodname[racket/gui/base] library provides the GUI and drawing
classes. By convention, the classes are given names that end with
@racket[%]:

@mr-defs+int[
[(require racket/class
          racket/gui/base)
 (define f (new frame% [label "My Art"]
                       [width 300]
                       [height 300]
                       [alignment '(center center)]))]
(send f show #t)
]

@(mr-interaction-eval (send f show #f))

The @racket[new] form creates an instance of a class, where
initialization arguments like @racket[label] and @racket[width] are
provided by name. The @racket[send] form calls a method of the object,
such as @racket[show], with arguments after the method name; the
argument @racket[#t] in this case is the boolean constant ``true.''

Pictures generated with @racketmodname[slideshow] encapsulate a
function that uses the graphics toolbox's drawing commands to render
the picture to a drawing context, such as a canvas in a frame. The
@racket[make-pict-drawer] function from @racketmodname[slideshow]
exposes a picture's drawing function. We can use
@racket[make-pict-drawer] in a canvas-painting callback to draw a
picture into a canvas:

@(mr-interaction-eval (require slideshow/flash))

@mr-def+int[
(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))
(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))
]

@centerline{
@(mr-interaction-eval-show (scale
                            (bitmap
                             (build-path
                              (collection-path "scribblings/quick") 
                              "art.png"))
                            0.5))}

Each canvas stretches to fill an equal portion of the frame, because
that's how a frame manages its children by default.

@; ----------------------------------------------------------------------
@section{Where to Go From Here}

This introduction to Racket purposely avoids many of the
traditional ways of introducing and distinguishing Lisp or Scheme:
prefix arithmetic notation, symbols, quoting and quasiquoting lists,
@racket[eval], first-class continuations, and the idea that all syntax
is really just a @racket[lambda] in disguise. While those are all part
of Racket, they are not the main ingredients of day-to-day programming
in Racket.

Instead, Racket programmers typically program with functions,
records, objects, exceptions, regular expressions, modules, and
threads. That is, instead of a ``minimalist'' language---which is the
way that Scheme is often described---Racket offers a rich language
with an extensive set of libraries and tools.

If you are new to programming or if you have the patience to work
through a textbook, we recommend reading
@italic{@link["http://www.htdp.org/"]{How to Design Programs}}. If you
have already read it, or if you want to see where the book will take
you, then see @other-manual['(lib
"web-server/scribblings/tutorial/continue.scrbl")].

For experienced programmers, to continue touring Racket from a
systems-oriented perspective instead of pictures, your next stop is
@other-manual['(lib "scribblings/more/more.scrbl")].

To instead start learning about the full Racket language and tools
in depth, move on to @other-manual['(lib "guide.scrbl"
"scribblings/guide")].
